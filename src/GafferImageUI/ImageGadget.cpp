//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2015, Image Engine Design Inc. All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//      * Redistributions of source code must retain the above
//        copyright notice, this list of conditions and the following
//        disclaimer.
//
//      * Redistributions in binary form must reproduce the above
//        copyright notice, this list of conditions and the following
//        disclaimer in the documentation and/or other materials provided with
//        the distribution.
//
//      * Neither the name of John Haddon nor the names of
//        any other contributors to this software may be used to endorse or
//        promote products derived from this software without specific prior
//        written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
//  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
//  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
//  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//////////////////////////////////////////////////////////////////////////

#include "GafferImageUI/ImageGadget.h"

#include "GafferImage/ImageAlgo.h"
#include "GafferImage/ImagePlug.h"
#include "GafferImage/OpenColorIOTransform.h"

#include "GafferUI/Style.h"
#include "GafferUI/ViewportGadget.h"

#include "Gaffer/BackgroundTask.h"
#include "Gaffer/Context.h"
#include "Gaffer/Node.h"
#include "Gaffer/ScriptNode.h"

#include "IECoreGL/GL.h"
#include "IECoreGL/IECoreGL.h"
#include "IECoreGL/LuminanceTexture.h"
#include "IECoreGL/Selector.h"
#include "IECoreGL/Shader.h"
#include "IECoreGL/ShaderLoader.h"

#include "OpenColorIO/OpenColorIO.h"

#include "boost/algorithm/string/predicate.hpp"
#include "boost/bind.hpp"
#include "boost/lexical_cast.hpp"


using namespace std;
using namespace boost;
using namespace Imath;
using namespace IECore;
using namespace IECoreGL;
using namespace Gaffer;
using namespace GafferUI;
using namespace GafferImage;
using namespace GafferImageUI;

//////////////////////////////////////////////////////////////////////////
// ImageGadget implementation
//////////////////////////////////////////////////////////////////////////

ImageGadget::ImageGadget()
	:	Gadget( defaultName<ImageGadget>() ),
		m_image( nullptr ),
		m_soloChannel( -1 ),
		m_clipping( false ),
		m_exposure( 0.0f ),
		m_gamma( 1.0f ),
		m_useGPU( true ),
		m_labelsVisible( true ),
		m_paused( false ),
		m_dirtyFlags( AllDirty ),
		m_renderRequestPending( false )
{
	m_rgbaChannels[0] = "R";
	m_rgbaChannels[1] = "G";
	m_rgbaChannels[2] = "B";
	m_rgbaChannels[3] = "A";

	setContext( new Context() );

	visibilityChangedSignal().connect( boost::bind( &ImageGadget::visibilityChanged, this ) );

	m_deepStateNode = new DeepState();
	m_deepStateNode->deepStatePlug()->setValue( int( DeepState::TargetState::Flat ) );

	m_clampNode = new Clamp();
	m_clampNode->inPlug()->setInput( m_deepStateNode->outPlug() );
	m_clampNode->enabledPlug()->setValue( false );
	m_clampNode->channelsPlug()->setValue( "*" );
	m_clampNode->minClampToEnabledPlug()->setValue( true );
	m_clampNode->maxClampToEnabledPlug()->setValue( true );
	m_clampNode->minClampToPlug()->setValue( Color4f( 1.0f, 1.0f, 1.0f, 0.0f ) );
	m_clampNode->maxClampToPlug()->setValue( Color4f( 0.0f, 0.0f, 0.0f, 1.0f ) );

	m_gradeNode = new Grade;
	m_gradeNode->inPlug()->setInput( m_clampNode->outPlug() );
	m_gradeNode->channelsPlug()->setValue( "*" );

	m_lut3dTextureID = 0;
}

ImageGadget::~ImageGadget()
{
	// Make sure background task completes before anything
	// it relies on is destroyed.
	m_tilesTask.reset();

	if( m_lut3dTextureID != 0 )
	{
		glDeleteTextures( 1, &m_lut3dTextureID );
	}
}

void ImageGadget::setImage( GafferImage::ImagePlugPtr image )
{
	if( image == m_image )
	{
		return;
	}

	m_image = image;

	// IMPORTANT : This DeepState node must be the first node in the processing chain.  Otherwise, we
	// would not be able to share hashes with the DeepState node at the beginning of the ImageSampler
	// also used by ImageView
	m_deepStateNode->inPlug()->setInput( m_image );

	if( Gaffer::Node *node = const_cast<Gaffer::Node *>( image->node() ) )
	{
		m_plugDirtiedConnection = node->plugDirtiedSignal().connect( boost::bind( &ImageGadget::plugDirtied, this, ::_1 ) );
	}
	else
	{
		m_plugDirtiedConnection.disconnect();
	}

	dirty( AllDirty );
}

const GafferImage::ImagePlug *ImageGadget::getImage() const
{
	return m_image.get();
}

void ImageGadget::setContext( Gaffer::ContextPtr context )
{
	if( context == m_context )
	{
		return;
	}

	m_context = context;
	m_contextChangedConnection = m_context->changedSignal().connect( boost::bind( &ImageGadget::contextChanged, this, ::_2 ) );

	dirty( AllDirty );
}

Gaffer::Context *ImageGadget::getContext()
{
	return m_context.get();
}

const Gaffer::Context *ImageGadget::getContext() const
{
	return m_context.get();
}

void ImageGadget::setChannels( const Channels &channels )
{
	if( channels == m_rgbaChannels )
	{
		return;
	}

	m_rgbaChannels = channels;
	channelsChangedSignal()( this );
	dirty( TilesDirty );
}

const ImageGadget::Channels &ImageGadget::getChannels() const
{
	return m_rgbaChannels;
}

ImageGadget::ImageGadgetSignal &ImageGadget::channelsChangedSignal()
{
	return m_channelsChangedSignal;
}

void ImageGadget::setSoloChannel( int index )
{
	if( index == m_soloChannel )
	{
		return;
	}
	if( index < -1 || index > 3 )
	{
		throw Exception( "Invalid index" );
	}

	m_soloChannel = index;
	if( m_soloChannel == -1 )
	{
		// Last time we called updateTiles(), we
		// only updated the solo channel, so now
		// we need to trigger a pass over all the
		// channels.
		dirty( TilesDirty );
	}
	requestRender();
}

int ImageGadget::getSoloChannel() const
{
	return m_soloChannel;
}

void ImageGadget::setClipping( bool clipping )
{
	m_clipping = clipping;

	if( !m_gpuOcioTransform )
	{
		dirty( TilesDirty );
	}
	else
	{
		requestRender();
	}
}

bool ImageGadget::getClipping() const
{
	return m_clipping;
}

void ImageGadget::setExposure( float exposure )
{
	m_exposure = exposure;
	if( !m_gpuOcioTransform )
	{
		dirty( TilesDirty );
	}
	else
	{
		requestRender();
	}
}

float ImageGadget::getExposure() const
{
	return m_exposure;
}

void ImageGadget::setGamma( float gamma )
{
	m_gamma = gamma;

	if( !m_gpuOcioTransform )
	{
		dirty( TilesDirty );
	}
	else
	{
		requestRender();
	}
}

float ImageGadget::getGamma() const
{
	return m_gamma;
}

void ImageGadget::setDisplayTransform( ImageProcessorPtr displayTransform )
{
	m_displayTransform = displayTransform;

	OpenColorIOTransformPtr ocioTransformNode = IECore::runTimeCast< OpenColorIOTransform >( m_displayTransform );
	if( m_useGPU && ocioTransformNode )
	{
		m_gpuOcioTransform = ocioTransformNode->transform();
	}
	else if( displayTransform )
	{
		m_displayTransform->inPlug()->setInput( m_gradeNode->outPlug() );
		m_gpuOcioTransform.reset();
	}
	else
	{
		m_gpuOcioTransform.reset();
	}

	dirty( TilesDirty );
	m_shaderDirty = true;
	requestRender();
}

ConstImageProcessorPtr ImageGadget::getDisplayTransform() const
{
	return m_displayTransform;
}

void ImageGadget::setUseGPU( bool useGPU )
{
	m_useGPU = useGPU;
	setDisplayTransform( m_displayTransform );
}

bool ImageGadget::getUseGPU() const
{
	return m_useGPU;
}

void ImageGadget::setLabelsVisible( bool visible )
{
	if( visible == m_labelsVisible )
	{
		return;
	}
	m_labelsVisible = visible;
	requestRender();
}

bool ImageGadget::getLabelsVisible() const
{
	return m_labelsVisible;
}

void ImageGadget::setPaused( bool paused )
{
	if( paused == m_paused )
	{
		return;
	}
	m_paused = paused;
	if( m_paused )
	{
		m_tilesTask.reset();
		stateChangedSignal()( this );
	}
	else if( m_dirtyFlags )
	{
		requestRender();
	}
}

bool ImageGadget::getPaused() const
{
	return m_paused;
}

ImageGadget::State ImageGadget::state() const
{
	if( m_paused )
	{
		return Paused;
	}
	return m_dirtyFlags ? Running : Complete;
}

ImageGadget::ImageGadgetSignal &ImageGadget::stateChangedSignal()
{
	return m_stateChangedSignal;
}

Imath::V2f ImageGadget::pixelAt( const IECore::LineSegment3f &lineInGadgetSpace ) const
{
	V3f i;
	if( !lineInGadgetSpace.intersect( Plane3f( V3f( 0, 0, 1 ), 0 ), i ) )
	{
		return V2f( 0 );
	}

	return V2f( i.x / format().getPixelAspect(), i.y );
}

Imath::Box3f ImageGadget::bound() const
{
	Format f;
	try
	{
		f = format();
	}
	catch( ... )
	{
		return Box3f();
	}

	const Box2i &w = f.getDisplayWindow();
	if( BufferAlgo::empty( w ) )
	{
		return Box3f();
	}

	const float a = f.getPixelAspect();
	return Box3f(
		V3f( (float)w.min.x * a, (float)w.min.y, 0 ),
		V3f( (float)w.max.x * a, (float)w.max.y, 0 )
	);
}

void ImageGadget::plugDirtied( const Gaffer::Plug *plug )
{
	if( plug == m_image->formatPlug() )
	{
		dirty( FormatDirty );
	}
	else if( plug == m_image->dataWindowPlug() )
	{
		dirty( DataWindowDirty | TilesDirty );
	}
	else if( plug == m_image->channelNamesPlug() )
	{
		dirty( ChannelNamesDirty | TilesDirty );
	}
	else if( plug == m_image->channelDataPlug() )
	{
		dirty( TilesDirty );
	}
}

void ImageGadget::contextChanged( const IECore::InternedString &name )
{
	if( !boost::starts_with( name.string(), "ui:" ) )
	{
		dirty( AllDirty );
	}
}

//////////////////////////////////////////////////////////////////////////
// Image property access.
//////////////////////////////////////////////////////////////////////////

void ImageGadget::dirty( unsigned flags )
{
	if( (flags & TilesDirty) && !(m_dirtyFlags & TilesDirty) )
	{
		m_tilesTask.reset();
	}

	m_dirtyFlags |= flags;
	requestRender();
}

const GafferImage::Format &ImageGadget::format() const
{
	if( m_dirtyFlags & FormatDirty )
	{
		if( !m_image )
		{
			m_format = Format();
		}
		else
		{
			Context::Scope scopedContext( m_context.get() );
			m_format = m_image->formatPlug()->getValue();
		}
		m_dirtyFlags &= ~FormatDirty;
	}

	return m_format;
}

const Imath::Box2i &ImageGadget::dataWindow() const
{
	if( m_dirtyFlags & DataWindowDirty )
	{
		if( !m_image )
		{
			m_dataWindow = Box2i();
		}
		else
		{
			Context::Scope scopedContext( m_context.get() );
			m_dataWindow = m_image->dataWindowPlug()->getValue();
		}
		m_dirtyFlags &= ~DataWindowDirty;
	}

	return m_dataWindow;
}

const std::vector<std::string> &ImageGadget::channelNames() const
{
	if( m_dirtyFlags & ChannelNamesDirty )
	{
		if( !m_image )
		{
			m_channelNames.clear();
		}
		else
		{
			Context::Scope scopedContext( m_context.get() );
			m_channelNames = m_image->channelNamesPlug()->getValue()->readable();
		}
		m_dirtyFlags &= ~ChannelNamesDirty;
	}
	return m_channelNames;
}

//////////////////////////////////////////////////////////////////////////
// Tile storage
//////////////////////////////////////////////////////////////////////////

namespace
{

IECoreGL::Texture *blackTexture()
{
	static IECoreGL::TexturePtr g_texture;
	if( !g_texture )
	{
		GLuint texture;
		glGenTextures( 1, &texture );
		g_texture = new Texture( texture );
		Texture::ScopedBinding binding( *g_texture );

		const float black = 0;
		glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
		glTexImage2D( GL_TEXTURE_2D, 0, GL_R16F, /* width = */ 1, /* height = */ 1, 0, GL_RED,
			GL_FLOAT, &black );
	}
	return g_texture.get();
}

} // namespace

ImageGadget::Tile::Tile( const Tile &other )
	:	m_channelDataHash( other.m_channelDataHash ),
		m_channelDataToConvert( other.m_channelDataToConvert ),
		m_texture( other.m_texture ),
		m_active( false )
{
}

ImageGadget::Tile::Update ImageGadget::Tile::computeUpdate( const GafferImage::ImagePlug *image )
{
	const IECore::MurmurHash h = image->channelDataPlug()->hash();
	Mutex::scoped_lock lock( m_mutex );
	if( m_channelDataHash != MurmurHash() && m_channelDataHash == h )
	{
		return Update{ nullptr, nullptr, MurmurHash() };
	}

	m_active = true;
	m_activeStartTime = std::chrono::steady_clock::now();
	lock.release(); // Release while doing expensive calculation so UI thread doesn't wait.
	ConstFloatVectorDataPtr channelData = image->channelDataPlug()->getValue( &h );
	return Update{ this, channelData, h };
}

void ImageGadget::Tile::applyUpdates( const std::vector<Update> &updates )
{
	for( const auto &u : updates )
	{
		if( u.tile )
		{
			u.tile->m_mutex.lock();
		}
	}

	for( const auto &u : updates )
	{
		if( u.tile )
		{
			u.tile->m_channelDataToConvert = u.channelData;
			u.tile->m_channelDataHash = u.channelDataHash;
			u.tile->m_active = false;
		}
	}

	for( const auto &u : updates )
	{
		if( u.tile )
		{
			u.tile->m_mutex.unlock();
		}
	}
}

const IECoreGL::Texture *ImageGadget::Tile::texture( bool &active )
{
	const auto now = std::chrono::steady_clock::now();
	Mutex::scoped_lock lock( m_mutex );
	if( m_active && ( now - m_activeStartTime ) > std::chrono::milliseconds( 20 ) )
	{
		// We don't draw a tile as active until after a short delay, to avoid
		// distractions when the image generation is really fast anyway.
		active = true;
	}

	ConstFloatVectorDataPtr channelDataToConvert = m_channelDataToConvert;
	m_channelDataToConvert = nullptr;
	lock.release(); // Don't hold lock while doing expensive conversion

	if( channelDataToConvert )
	{
		GLuint texture;
		glGenTextures( 1, &texture );
		m_texture = new Texture( texture ); // Lock not needed, because this is only touched on the UI thread.
		Texture::ScopedBinding binding( *m_texture );

		glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
		glTexImage2D(
			GL_TEXTURE_2D, 0, GL_R16F, ImagePlug::tileSize(), ImagePlug::tileSize(), 0, GL_RED,
			GL_FLOAT, channelDataToConvert->readable().data()
		);

		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
	}

	return m_texture ? m_texture.get() : blackTexture();
}

// Needed to allow TileIndex to be used as a key in concurrent_unordered_map.
inline size_t GafferImageUI::tbb_hasher( const ImageGadget::TileIndex &tileIndex )
{
	return
		tbb::tbb_hasher( tileIndex.tileOrigin.x ) ^
		tbb::tbb_hasher( tileIndex.tileOrigin.y ) ^
		tbb::tbb_hasher( tileIndex.channelName.c_str() );
}

void ImageGadget::updateTiles()
{
	if( !(m_dirtyFlags & TilesDirty) )
	{
		return;
	}

	if( m_paused )
	{
		return;
	}

	if( m_tilesTask )
	{
		const auto status = m_tilesTask->status();
		if( status == BackgroundTask::Pending || status == BackgroundTask::Running )
		{
			return;
		}
	}

	stateChangedSignal()( this );
	removeOutOfBoundsTiles();

	ImagePlug* tilesImage;
	if( m_gpuOcioTransform || !m_displayTransform )
	{
		tilesImage = m_deepStateNode->outPlug();
	}
	else
	{
		// The background task is only passed m_image as the subject, which means edits to the internal
		// network won't cancel it.  This means it is only safe to edit the internal network here,
		// where we have already checked above that m_tilesTask->status() is not running.
		m_clampNode->enabledPlug()->setValue( m_clipping );
		const float m = pow( 2.0f, m_exposure );
		m_gradeNode->multiplyPlug()->setValue( Color4f( m, m, m, 1.0f ) );
		m_gradeNode->gammaPlug()->setValue( Color4f( m_gamma, m_gamma, m_gamma, 1.0f ) );
		tilesImage = m_displayTransform->outPlug();
	}

	// Decide which channels to compute. This is the intersection
	// of the available channels (channelNames) and the channels
	// we want to display (m_rgbaChannels).
	const vector<string> &channelNames = this->channelNames();
	vector<string> channelsToCompute;
	for( vector<string>::const_iterator it = channelNames.begin(), eIt = channelNames.end(); it != eIt; ++it )
	{
		if( find( m_rgbaChannels.begin(), m_rgbaChannels.end(), *it ) != m_rgbaChannels.end() )
		{
			if( m_soloChannel == -1 || m_rgbaChannels[m_soloChannel] == *it )
			{
				channelsToCompute.push_back( *it );
			}
		}
	}

	const Box2i dataWindow = this->dataWindow();

	// Do the actual work of generating the tiles asynchronously,
	// in the background.

	auto tileFunctor = [this, channelsToCompute] ( const ImagePlug *image, const V2i &tileOrigin ) {

		vector<Tile::Update> updates;
		ImagePlug::ChannelDataScope channelScope( Context::current() );
		for( auto &channelName : channelsToCompute )
		{
			channelScope.setChannelName( channelName );
			Tile &tile = m_tiles[TileIndex(tileOrigin, channelName)];
			updates.push_back( tile.computeUpdate( image ) );
		}

		Tile::applyUpdates( updates );

		if( refCount() && !m_renderRequestPending.exchange( true ) )
		{
			// Must hold a reference to stop us dying before our UI thread call is scheduled.
			ImageGadgetPtr thisRef = this;
			ParallelAlgo::callOnUIThread(
				[thisRef] {
					thisRef->m_renderRequestPending = false;
					thisRef->requestRender();
				}
			);
		}
	};


	// callOnBackgroundThread requires a "subject" that will trigger task cancellation
	// when dirtied.  This subject usually needs to be in a script, but there's a special
	// case in BackgroundTask::scriptNode for nodes that are in a GafferUI::View.  We
	// can work with this by passing in m_image, which is passed to us by ImageView.
	// This means that any internal nodes of ImageGadget are not part of the automatic
	// task cancellation and we must ensure that we never modify internal nodes while
	// the background task is running.
	Context::Scope scopedContext( m_context.get() );
	m_tilesTask = ParallelAlgo::callOnBackgroundThread(
		// Subject
		m_image.get(),
		// OK to capture `this` via raw pointer, because ~ImageGadget waits for
		// the background process to complete.
		[this, channelsToCompute, dataWindow, tileFunctor, tilesImage] {
			ImageAlgo::parallelProcessTiles( tilesImage, tileFunctor, dataWindow );
			m_dirtyFlags &= ~TilesDirty;
			if( refCount() )
			{
				ImageGadgetPtr thisRef = this;
				ParallelAlgo::callOnUIThread(
					[thisRef] {
						thisRef->stateChangedSignal()( thisRef.get() );
					}
				);
			}
		}
	);

}

void ImageGadget::removeOutOfBoundsTiles() const
{
	// In theory, any given tile we hold could turn out to be valid
	// for some future image we want to display, particularly if the
	// user is switching back and forth between the same images. But
	// we don't want to accumulate unbounded numbers of tiles either,
	// so here we prune out any tiles that we know can't be useful for
	// the current image, because they either have an invalid channel
	// name or are outside the data window.
	const Box2i &dw = dataWindow();
	const vector<string> &ch = channelNames();
	for( Tiles::iterator it = m_tiles.begin(); it != m_tiles.end(); )
	{
		const Box2i tileBound( it->first.tileOrigin, it->first.tileOrigin + V2i( ImagePlug::tileSize() ) );
		if( !BufferAlgo::intersects( dw, tileBound ) || find( ch.begin(), ch.end(), it->first.channelName.string() ) == ch.end() )
		{
			it = m_tiles.unsafe_erase( it );
		}
		else
		{
			++it;
		}
	}
}

//////////////////////////////////////////////////////////////////////////
// Rendering
//////////////////////////////////////////////////////////////////////////

namespace
{

const char *vertexSource()
{
	static const char *g_vertexSource =
	"void main()"
	"{"
	"	gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;"
	"	gl_TexCoord[0] = gl_MultiTexCoord0;"
	"}";

	return g_vertexSource;
}

const std::string &fragmentSource()
{
	static std::string g_fragmentSource;
	if( g_fragmentSource.empty() )
	{
		g_fragmentSource =

		"uniform sampler2D redTexture;\n"
		"uniform sampler2D greenTexture;\n"
		"uniform sampler2D blueTexture;\n"
		"uniform sampler2D alphaTexture;\n"

		"uniform sampler3D lutTexture;\n"

		"uniform bool activeParam;\n"
		"uniform float multiply;\n"
		"uniform float power;\n"
		"uniform bool clipping;\n"

		"#if __VERSION__ >= 330\n"

		"layout( location=0 ) out vec4 outColor;\n"
		"#define OUTCOLOR outColor\n"

		"#else\n"

		"#define OUTCOLOR gl_FragColor\n"

		"#endif\n"

		"#define ACTIVE_CORNER_RADIUS 0.3\n"

		"void main()"
		"{"
		"	OUTCOLOR = vec4(\n"
		"		texture2D( redTexture, gl_TexCoord[0].xy ).r,\n"
		"		texture2D( greenTexture, gl_TexCoord[0].xy ).r,\n"
		"		texture2D( blueTexture, gl_TexCoord[0].xy ).r,\n"
		"		texture2D( alphaTexture, gl_TexCoord[0].xy ).r\n"
		"	);\n"
		"	if( clipping )\n"
		"	{\n"
		"		OUTCOLOR = vec4(\n"
		"			OUTCOLOR.r < 0.0 ? 1.0 : ( OUTCOLOR.r > 1.0 ? 0.0 : OUTCOLOR.r ),\n"
		"			OUTCOLOR.g < 0.0 ? 1.0 : ( OUTCOLOR.g > 1.0 ? 0.0 : OUTCOLOR.g ),\n"
		"			OUTCOLOR.b < 0.0 ? 1.0 : ( OUTCOLOR.b > 1.0 ? 0.0 : OUTCOLOR.b ),\n"
		"			OUTCOLOR.a\n"
		"		);\n"
		"	}\n"
		"	OUTCOLOR = vec4( pow( OUTCOLOR.rgb * multiply, vec3( power ) ), OUTCOLOR.a );\n"
		"	OUTCOLOR = OCIODisplay( OUTCOLOR, lutTexture );\n"
		"	if( activeParam )\n"
		"	{\n"
		"		vec2 pixelWidth = vec2( dFdx( gl_TexCoord[0].x ), dFdy( gl_TexCoord[0].y ) );\n"
		"		float aspect = pixelWidth.x / pixelWidth.y;\n"
		"		vec2 p = abs( gl_TexCoord[0].xy - vec2( 0.5 ) );\n"
		"		float eX = step( 0.5 - pixelWidth.x, p.x ) * step( 0.5 - ACTIVE_CORNER_RADIUS, p.y );\n"
		"		float eY = step( 0.5 - pixelWidth.y, p.y ) * step( 0.5 - ACTIVE_CORNER_RADIUS * aspect, p.x );\n"
		"		float e = eX + eY - eX * eY;\n"
		"		OUTCOLOR += vec4( 0.15 ) * e;\n"
		"	}\n"
		"}";
	}
	return g_fragmentSource;
}


} // namespace

void ImageGadget::visibilityChanged()
{
	if( !visible() )
	{
		m_tilesTask.reset();
	}
}

IECoreGL::Shader *ImageGadget::shader( bool dirty, const OpenColorIO::ConstTransformRcPtr& transform, GLuint &lut3dTextureID ) const
{
	const int LUT3D_EDGE_SIZE = 128;
	if( m_lut3dTextureID == 0)
	{
		glGenTextures( 1, &m_lut3dTextureID );
	}

	if( !m_shader || dirty )
	{
		std::string colorTransformCode;
		std::vector<float> lut3d;
		if( transform )
		{
			OpenColorIO::ConstConfigRcPtr config = OpenColorIO::GetCurrentConfig();

			OpenColorIO::ConstProcessorRcPtr processor = config->getProcessor( transform );

			OpenColorIO::GpuShaderDesc shaderDesc;
			shaderDesc.setLanguage( OpenColorIO::GPU_LANGUAGE_GLSL_1_3 );
			shaderDesc.setFunctionName( "OCIODisplay" );
			shaderDesc.setLut3DEdgeLen( LUT3D_EDGE_SIZE );

			int num3Dentries = 3*LUT3D_EDGE_SIZE*LUT3D_EDGE_SIZE*LUT3D_EDGE_SIZE;
			lut3d.resize( num3Dentries );
			processor->getGpuLut3D( &lut3d[0], shaderDesc );
			colorTransformCode =  processor->getGpuShaderText( shaderDesc );
		}
		else
		{
			colorTransformCode = "vec4 OCIODisplay(vec4 inPixel, sampler3D lut3d) { return inPixel; }\n";
		}

		std::string combinedFragmentCode;
		if( glslVersion() >= 330 )
		{
			// the __VERSION__ define is a workaround for the fact that cortex's source preprocessing doesn't
			// define it correctly in the same way as the OpenGL shader preprocessing would.
			combinedFragmentCode = "#version 330 compatibility\n #define __VERSION__ 330\n\n";
		}
		combinedFragmentCode += colorTransformCode + fragmentSource();

		m_shader = ShaderLoader::defaultShaderLoader()->create( vertexSource(), "", combinedFragmentCode );

		if( transform && m_shader->uniformParameter( "lutTexture" ) )
		{
			// Load the data into an OpenGL 3D Texture
			glActiveTexture( GL_TEXTURE0 + m_shader->uniformParameter( "lutTexture" )->textureUnit );
			glBindTexture( GL_TEXTURE_3D, m_lut3dTextureID );
			glTexImage3D(
				GL_TEXTURE_3D, 0, GL_RGB16F, LUT3D_EDGE_SIZE, LUT3D_EDGE_SIZE, LUT3D_EDGE_SIZE,
				0, GL_RGB, GL_FLOAT, &lut3d[0]
			);
		}
	}

	lut3dTextureID = m_lut3dTextureID;
	return m_shader.get();
}

void ImageGadget::renderTiles() const
{
	GLint previousProgram;
	glGetIntegerv( GL_CURRENT_PROGRAM, &previousProgram );



	PushAttrib pushAttrib( GL_COLOR_BUFFER_BIT );

	GLuint lutTextureID;
	Shader *shader = this->shader( m_shaderDirty, m_gpuOcioTransform, lutTextureID );
	m_shaderDirty = false;

	glUseProgram( shader->program() );

	glEnable( GL_TEXTURE_2D );

	glEnable( GL_BLEND );
	glBlendFunc( GL_ONE, GL_ONE_MINUS_SRC_ALPHA );

	std::vector<std::string> uniformNames;
	shader->uniformParameterNames( uniformNames );

	GLuint textureUnits[4];
	textureUnits[0] = shader->uniformParameter( "redTexture" )->textureUnit;
	textureUnits[1] = shader->uniformParameter( "greenTexture" )->textureUnit;
	textureUnits[2] = shader->uniformParameter( "blueTexture" )->textureUnit;
	textureUnits[3] = shader->uniformParameter( "alphaTexture" )->textureUnit;

	glUniform1i( shader->uniformParameter( "redTexture" )->location, textureUnits[0] );
	glUniform1i( shader->uniformParameter( "greenTexture" )->location, textureUnits[1] );
	glUniform1i( shader->uniformParameter( "blueTexture" )->location, textureUnits[2] );
	glUniform1i( shader->uniformParameter( "alphaTexture" )->location, textureUnits[3] );

	if( shader->uniformParameter( "lutTexture" ) )
	{
		GLuint lutTextureUnit = shader->uniformParameter( "lutTexture" )->textureUnit;
		glUniform1i( shader->uniformParameter( "lutTexture" )->location, lutTextureUnit );
		glActiveTexture( GL_TEXTURE0 + lutTextureUnit );
		glBindTexture( GL_TEXTURE_3D, lutTextureID );

		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
	}

	if( m_gpuOcioTransform || !m_displayTransform )
	{
		glUniform1f( shader->uniformParameter( "multiply" )->location, pow( 2.0f, m_exposure ) );
		glUniform1f( shader->uniformParameter( "power" )->location, m_gamma > 0.0 ? 1.0f / m_gamma : 1.0f );
		glUniform1f( shader->uniformParameter( "clipping" )->location, m_clipping );
	}
	else
	{
		glUniform1f( shader->uniformParameter( "multiply" )->location, 1.0f );
		glUniform1f( shader->uniformParameter( "power" )->location, 1.0f );
		glUniform1f( shader->uniformParameter( "clipping" )->location, false );
	}

	GLint activeParameterLocation = shader->uniformParameter( "activeParam" )->location;

	const Box2i dataWindow = this->dataWindow();
	const float pixelAspect = this->format().getPixelAspect();

	V2i tileOrigin = ImagePlug::tileOrigin( dataWindow.min );
	for( ; tileOrigin.y < dataWindow.max.y; tileOrigin.y += ImagePlug::tileSize() )
	{
		for( tileOrigin.x = ImagePlug::tileOrigin( dataWindow.min ).x; tileOrigin.x < dataWindow.max.x; tileOrigin.x += ImagePlug::tileSize() )
		{
			bool active = false;
			for( int i = 0; i < 4; ++i )
			{
				glActiveTexture( GL_TEXTURE0 + textureUnits[i] );
				const InternedString channelName = m_soloChannel == -1 ? m_rgbaChannels[i] : m_rgbaChannels[m_soloChannel];
				Tiles::const_iterator it = m_tiles.find( TileIndex( tileOrigin, channelName ) );
				if( it != m_tiles.end() )
				{
					it->second.texture( active )->bind();
				}
				else
				{
					blackTexture()->bind();
				}
			}

			glUniform1i( activeParameterLocation, active );

			const Box2i tileBound( tileOrigin, tileOrigin + V2i( ImagePlug::tileSize() ) );
			const Box2i validBound = BufferAlgo::intersection( tileBound, dataWindow );
			const Box2f uvBound(
				V2f(
					lerpfactor<float>( validBound.min.x, tileBound.min.x, tileBound.max.x ),
					lerpfactor<float>( validBound.min.y, tileBound.min.y, tileBound.max.y )
				),
				V2f(
					lerpfactor<float>( validBound.max.x, tileBound.min.x, tileBound.max.x ),
					lerpfactor<float>( validBound.max.y, tileBound.min.y, tileBound.max.y )
				)
			);

			glBegin( GL_QUADS );

				glTexCoord2f( uvBound.min.x, uvBound.min.y  );
				glVertex2f( validBound.min.x * pixelAspect, validBound.min.y );

				glTexCoord2f( uvBound.min.x, uvBound.max.y  );
				glVertex2f( validBound.min.x * pixelAspect, validBound.max.y );

				glTexCoord2f( uvBound.max.x, uvBound.max.y  );
				glVertex2f( validBound.max.x * pixelAspect, validBound.max.y );

				glTexCoord2f( uvBound.max.x, uvBound.min.y  );
				glVertex2f( validBound.max.x * pixelAspect, validBound.min.y );

			glEnd();

		}
	}

	glUseProgram( previousProgram );
}

void ImageGadget::renderText( const std::string &text, const Imath::V2f &position, const Imath::V2f &alignment, const GafferUI::Style *style ) const
{
	const float scale = 10.0f;
	const ViewportGadget *viewport = ancestor<ViewportGadget>();
	const V2f rasterPosition = viewport->gadgetToRasterSpace( V3f( position.x, position.y, 0.0f ), this );
	const Box3f bound = style->textBound( Style::LabelText, text );

	ViewportGadget::RasterScope rasterScope( viewport );
	glTranslate( V2f(
		rasterPosition.x - scale * lerp( bound.min.x, bound.max.x, alignment.x ),
		rasterPosition.y + scale * lerp( bound.min.y, bound.max.y, alignment.y )
	) );

	glScalef( scale, -scale, scale );
	style->renderText( Style::LabelText, text );
}

void ImageGadget::doRenderLayer( Layer layer, const GafferUI::Style *style ) const
{
	if( layer != Layer::Main )
	{
		return;
	}

	// Compute what we need, and abort rendering if
	// there are any computation errors.

	Format format;
	Box2i dataWindow;
	try
	{
		format = this->format();
		dataWindow = this->dataWindow();
		const_cast<ImageGadget *>( this )->updateTiles();
	}
	catch( ... )
	{
		return;
	}

	// Early out if the image has no size.

	const Box2i &displayWindow = format.getDisplayWindow();
	if( BufferAlgo::empty( displayWindow ) )
	{
		return;
	}

	// Render a black background the size of the image.
	// We need to account for the pixel aspect ratio here
	// and in all our drawing. Variables ending in F denote
	// windows corrected for pixel aspect.

	const Box2f displayWindowF(
		V2f( displayWindow.min ) * V2f( format.getPixelAspect(), 1.0f ),
		V2f( displayWindow.max ) * V2f( format.getPixelAspect(), 1.0f )
	);

	const Box2f dataWindowF(
		V2f( dataWindow.min ) * V2f( format.getPixelAspect(), 1.0f ),
		V2f( dataWindow.max ) * V2f( format.getPixelAspect(), 1.0f )
	);

	glColor3f( 0.0f, 0.0f, 0.0f );
	style->renderSolidRectangle( displayWindowF );
	if( !BufferAlgo::empty( dataWindow ) )
	{
		style->renderSolidRectangle( dataWindowF );
	}

	// Draw the image tiles over the top.

	if( IECoreGL::Selector::currentSelector() )
	{
		// The rectangle we drew above is sufficient for
		// selection rendering.
		return;
	}

	renderTiles();

	// And add overlays for the display and data windows.

	glColor3f( 0.1f, 0.1f, 0.1f );
	style->renderRectangle( displayWindowF );

	if( !BufferAlgo::empty( dataWindow ) && dataWindow != displayWindow )
	{
		glColor3f( 0.5f, 0.5f, 0.5f );
		style->renderRectangle( dataWindowF );
	}

	// Render labels for resolution and suchlike.

	if( m_labelsVisible )
	{
		string formatText = Format::name( format );
		const string dimensionsText = lexical_cast<string>( displayWindow.size().x ) + " x " +  lexical_cast<string>( displayWindow.size().y );
		if( formatText.empty() )
		{
			formatText = dimensionsText;
		}
		else
		{
			formatText += " ( " + dimensionsText + " )";
		}

		renderText( formatText, V2f( displayWindowF.center().x, displayWindowF.min.y ), V2f( 0.5, 1.5 ), style );

		if( displayWindow.min != V2i( 0 ) )
		{
			renderText( lexical_cast<string>( displayWindow.min ), displayWindowF.min, V2f( 1, 1.5 ), style );
			renderText( lexical_cast<string>( displayWindow.max ), displayWindowF.max, V2f( 0, -0.5 ), style );
		}

		if( !BufferAlgo::empty( dataWindow ) && dataWindow.min != displayWindow.min )
		{
			renderText( lexical_cast<string>( dataWindow.min ), dataWindowF.min, V2f( 1, 1.5 ), style );
		}

		if( !BufferAlgo::empty( dataWindow ) && dataWindow.max != displayWindow.max )
		{
			renderText( lexical_cast<string>( dataWindow.max ), dataWindowF.max, V2f( 0, -0.5 ), style );
		}
	}
}
