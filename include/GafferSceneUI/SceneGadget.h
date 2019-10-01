//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2014, Image Engine Design Inc. All rights reserved.
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

#ifndef GAFFERSCENEUI_SCENEGADGET_H
#define GAFFERSCENEUI_SCENEGADGET_H

#include "GafferSceneUI/Export.h"
#include "GafferSceneUI/TypeIds.h"

#include "GafferScene/RenderController.h"
#include "GafferScene/ScenePlug.h"

#include "GafferUI/Gadget.h"

#include "Gaffer/Context.h"
#include "Gaffer/ParallelAlgo.h"

#include "IECoreGL/State.h"

namespace GafferSceneUI
{

IE_CORE_FORWARDDECLARE( SceneGadget );

class GAFFERSCENEUI_API SceneGadget : public GafferUI::Gadget
{

	public :

		SceneGadget();
		~SceneGadget() override;

		GAFFER_GRAPHCOMPONENT_DECLARE_TYPE( GafferSceneUI::SceneGadget, SceneGadgetTypeId, Gadget );

		/// Scene
		/// =====
		///
		/// These methods specify the scene and how it is drawn.

		void setScene( GafferScene::ConstScenePlugPtr scene );
		const GafferScene::ScenePlug *getScene() const;

		void setContext( Gaffer::ConstContextPtr context );
		const Gaffer::Context *getContext() const;

		/// Limits the expanded parts of the scene to those in the specified paths.
		void setExpandedPaths( const IECore::PathMatcher &expandedPaths );
		const IECore::PathMatcher &getExpandedPaths() const;

		void setMinimumExpansionDepth( size_t depth );
		size_t getMinimumExpansionDepth() const;

		/// Returns the selection.
		const IECore::PathMatcher &getSelection() const;
		/// Sets the selection.
		void setSelection( const IECore::PathMatcher &selection );

		/// Specifies options to control the OpenGL renderer. These are used
		/// to specify wireframe/point drawing and colours etc. A copy of
		/// `options` is taken.
		void setOpenGLOptions( const IECore::CompoundObject *options );
		const IECore::CompoundObject *getOpenGLOptions() const;

		/// Update process
		/// ==============
		///
		/// The SceneGadget updates progressively by performing
		/// all computations on background threads, displaying
		/// results as they become available. These methods control
		/// that process.

		void setPaused( bool paused );
		bool getPaused() const;

		/// Specifies a set of paths that block drawing until they are
		/// up to date. Use sparingly.
		void setBlockingPaths( const IECore::PathMatcher &blockingPaths );
		const IECore::PathMatcher &getBlockingPaths() const;

		/// Specifies a set of paths that are given priorty when performing
		/// asynchronous updates.
		void setPriorityPaths( const IECore::PathMatcher &priorityPaths );
		const IECore::PathMatcher &getPriorityPaths() const;

		enum State
		{
			Paused,
			Running,
			Complete
		};

		State state() const;

		typedef boost::signal<void (SceneGadget *)> SceneGadgetSignal;
		SceneGadgetSignal &stateChangedSignal();

		/// Blocks until the update is completed. This is primarily of
		/// use for the unit tests.
		void waitForCompletion();

		/// Scene queries
		/// =============
		///
		/// These queries are performed against the current state of the scene,
		/// which might still be being updated asynchronously. Call `waitForCompletion()`
		/// first if you need a final answer and are willing to block the UI
		/// waiting for it.

		Imath::Box3f bound() const override;

		/// Specifies which object types are selectable via `objectAt()` and `objectsAt()`.
		/// May be null, which means all object types are selectable. A copy of `typeNames`
		/// is taken.
		void setSelectionMask( const IECore::StringVectorData *typeNames );
		const IECore::StringVectorData *getSelectionMask() const;

		/// Finds the path of the frontmost object intersecting the specified line
		/// through gadget space. Returns true on success and false if there is no
		/// such object.
		bool objectAt( const IECore::LineSegment3f &lineInGadgetSpace, GafferScene::ScenePlug::ScenePath &path ) const;
		bool objectAt( const IECore::LineSegment3f &lineInGadgetSpace, GafferScene::ScenePlug::ScenePath &path, float &hitDepth ) const;
		/// Fills paths with all objects intersected by a rectangle in screen space,
		/// defined by two corners in gadget space (as required for drag selection).
		size_t objectsAt(
			const Imath::V3f &corner0InGadgetSpace,
			const Imath::V3f &corner1InGadgetSpace,
			IECore::PathMatcher &paths
		) const;

		/// Returns the bounding box of all the selected objects.
		Imath::Box3f selectionBound() const;

		/// Implemented to return the name of the object under the mouse.
		std::string getToolTip( const IECore::LineSegment3f &line ) const override;

	protected :

		void doRenderLayer( Layer layer, const GafferUI::Style *style ) const override;

	private :

		void updateRenderer();
		void renderScene() const;
		IECore::PathMatcher convertSelection( IECore::UIntVectorDataPtr ids ) const;
		void visibilityChanged();

		bool m_paused;
		IECore::PathMatcher m_blockingPaths;
		IECore::PathMatcher m_priorityPaths;
		SceneGadgetSignal m_stateChangedSignal;

		IECoreScenePreview::RendererPtr m_renderer;
		mutable GafferScene::RenderController m_controller;
		mutable std::shared_ptr<Gaffer::BackgroundTask> m_updateTask;
		bool m_updateErrored;
		std::atomic_bool m_renderRequestPending;

		IECore::ConstCompoundObjectPtr m_openGLOptions;
		IECore::PathMatcher m_selection;

		IECore::StringVectorDataPtr m_selectionMask;

};

typedef Gaffer::FilteredChildIterator<Gaffer::TypePredicate<SceneGadget> > SceneGadgetIterator;
typedef Gaffer::FilteredRecursiveChildIterator<Gaffer::TypePredicate<SceneGadget> > RecursiveSceneGadgetIterator;

} // namespace GafferUI

#endif // GAFFERSCENEUI_SCENEGADGET_H
