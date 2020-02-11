//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2020, Cinesite VFX Ltd. All rights reserved.
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
//      * Neither the name of Cinesite VFX Ltd. nor the names of
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

#include "GafferScene/AttributeTweaks.h"

#include "IECore/SimpleTypedData.h"
#include "IECore/StringAlgo.h"

#include "boost/algorithm/string/replace.hpp"

using namespace std;
using namespace IECore;
using namespace Gaffer;
using namespace GafferScene;

GAFFER_GRAPHCOMPONENT_DEFINE_TYPE( AttributeTweaks );

size_t AttributeTweaks::g_firstPlugIndex = 0;

AttributeTweaks::AttributeTweaks( const std::string &name )
	:	SceneElementProcessor( name, IECore::PathMatcher::NoMatch )
{
	storeIndexOfNextChild( g_firstPlugIndex );
	addChild( new StringPlug( "attributeName" ) );
	addChild( new StringPlug( "search" ) );
	addChild( new StringPlug( "replace" ) );

	// Fast pass-throughs for the things we don't alter.
	outPlug()->objectPlug()->setInput( inPlug()->objectPlug() );
	outPlug()->transformPlug()->setInput( inPlug()->transformPlug() );
	outPlug()->boundPlug()->setInput( inPlug()->boundPlug() );
}

AttributeTweaks::~AttributeTweaks()
{
}

Gaffer::StringPlug *AttributeTweaks::attributeNamePlug()
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex );
}

const Gaffer::StringPlug *AttributeTweaks::attributeNamePlug() const
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex );
}

Gaffer::StringPlug *AttributeTweaks::searchPlug()
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex + 1 );
}

const Gaffer::StringPlug *AttributeTweaks::searchPlug() const
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex + 1 );
}

Gaffer::StringPlug *AttributeTweaks::replacePlug()
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex + 2 );
}

const Gaffer::StringPlug *AttributeTweaks::replacePlug() const
{
	return getChild<Gaffer::StringPlug>( g_firstPlugIndex + 2 );
}

void AttributeTweaks::affects( const Gaffer::Plug *input, AffectedPlugsContainer &outputs ) const
{
	SceneElementProcessor::affects( input, outputs );

	if(
		input == attributeNamePlug() ||
		input == searchPlug() ||
		input == replacePlug()
	)
	{
		outputs.push_back( outPlug()->attributesPlug() );
	}
}

bool AttributeTweaks::processesAttributes() const
{
	return true;
}

void AttributeTweaks::hashProcessedAttributes( const ScenePath &path, const Gaffer::Context *context, IECore::MurmurHash &h ) const
{
	attributeNamePlug()->hash( h );
	searchPlug()->hash( h );
	replacePlug()->hash( h );
}

IECore::ConstCompoundObjectPtr AttributeTweaks::computeProcessedAttributes( const ScenePath &path, const Gaffer::Context *context, IECore::ConstCompoundObjectPtr inputAttributes ) const
{
	const std::string &name = attributeNamePlug()->getValue();
	const std::string &search = searchPlug()->getValue();
	const std::string &replace = replacePlug()->getValue();

	if( name.empty() || search.empty() )
	{
		return inputAttributes;
	}

	CompoundObjectPtr result = new CompoundObject;
	const CompoundObject::ObjectMap &in = inputAttributes->members();
	CompoundObject::ObjectMap &out = result->members();
	for( CompoundObject::ObjectMap::const_iterator it = in.begin(), eIt = in.end(); it != eIt; ++it )
	{
		if( !StringAlgo::matchMultiple( it->first, name ) )
		{
			out.insert( *it );
			continue;
		}

		const StringData *stringData = runTimeCast<const StringData>( it->second.get() );
		if( !stringData )
		{
			continue;
		}

		std::string value = stringData->readable();
		boost::replace_all( value, search, replace );
		out[it->first] = new IECore::StringData( value );
	}

	return result;
}
