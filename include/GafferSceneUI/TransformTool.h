//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2014-2016, John Haddon. All rights reserved.
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

#ifndef GAFFERSCENEUI_TRANSFORMTOOL_H
#define GAFFERSCENEUI_TRANSFORMTOOL_H

#include "GafferSceneUI/SelectionTool.h"
#include "GafferSceneUI/TypeIds.h"

#include "GafferScene/ScenePlug.h"

#include "GafferUI/KeyEvent.h"

#include "Gaffer/TransformPlug.h"

namespace GafferSceneUI
{

IE_CORE_FORWARDDECLARE( SceneView )

class GAFFERSCENEUI_API TransformTool : public GafferSceneUI::SelectionTool
{

	public :

		~TransformTool() override;

		GAFFER_GRAPHCOMPONENT_DECLARE_TYPE( GafferSceneUI::TransformTool, TransformToolTypeId, SelectionTool );

		enum Orientation
		{
			Local,
			Parent,
			World
		};

		Gaffer::FloatPlug *sizePlug();
		const Gaffer::FloatPlug *sizePlug() const;

		struct Selection
		{

			// Constructs an empty selection.
			Selection();

			// Constructs a selection for the specified
			// viewed scene.
			Selection(
				const GafferScene::ConstScenePlugPtr scene,
				const GafferScene::ScenePlug::ScenePath &path,
				const Gaffer::ConstContextPtr &context
			);

			/// Viewed scene
			/// ============
			///
			/// The scene being viewed.
			GafferScene::ConstScenePlugPtr scene;
			/// The location within the viewed scene that has been
			/// selected for editing.
			GafferScene::ScenePlug::ScenePath path;
			/// The context the scene is being viewed in.
			Gaffer::ConstContextPtr context;

			/// Upstream scene
			/// ==============
			///
			/// Often, the scene being viewed isn't actually the
			/// scene that is being edited. Instead, an upstream
			/// node is being edited, and the user is viewing a
			/// downstream node to see the edits in the context of later
			/// changes. The `upstreamScene` is the output from the node
			/// actually being edited.
			GafferScene::ConstScenePlugPtr upstreamScene;
			/// The hierarchies of the upstream and viewed scenes may
			/// differ. The upstreamPath is the equivalent of
			/// the viewed path but in the upstream scene.
			GafferScene::ScenePlug::ScenePath upstreamPath;
			/// The upstream context is the equivalent of the
			/// viewed context, but for the upstream scene.
			Gaffer::ConstContextPtr upstreamContext;

			/// Transform to edit
			/// =================
			///
			/// The plug to edit. This will be a child of
			/// the node generating the upstream scene.
			Gaffer::TransformPlugPtr transformPlug;
			/// The coordinate system within which the
			/// transform is applied by the upstream node.
			/// This is relative to the world space of the
			/// upstream scene.
			Imath::M44f transformSpace;

			/// Utilities
			/// =========
			///
			/// Returns a matrix which converts from world
			/// space in `scene` to `transformSpace`.
			Imath::M44f sceneToTransformSpace() const;
			/// Returns a matrix suitable for positioning
			/// transform handles in `scene's` world space.
			Imath::M44f orientedTransform( Orientation orientation ) const;

		};

		const std::vector<Selection> &selection() const;

		using SelectionChangedSignal = boost::signal<void (TransformTool &)>;
		SelectionChangedSignal &selectionChangedSignal();

		/// Returns the transform of the handles. Throws
		/// if the selection is invalid because then the
		/// transform would be meaningless. This is
		/// exposed primarily for the unit tests.
		Imath::M44f handlesTransform();

	protected :

		TransformTool( SceneView *view, const std::string &name );

		/// The scene being edited.
		GafferScene::ScenePlug *scenePlug();
		const GafferScene::ScenePlug *scenePlug() const;

		/// Gadget under which derived classes
		/// should parent their handles.
		GafferUI::Gadget *handles();
		const GafferUI::Gadget *handles() const;

		/// Must be implemented by derived classes to return true if
		/// the input plug is used in updateHandles(). Implementation
		/// must call the base class implementation first, returning true
		/// if it does.
		virtual bool affectsHandles( const Gaffer::Plug *input ) const = 0;
		/// Must be implemented by derived classes to update the
		/// handles appropriately. Typically this means setting their
		/// transform and matching their enabled state to the editability
		/// of the selection.
		virtual void updateHandles( float rasterScale ) = 0;

		/// Must be called by derived classes when they begin
		/// a drag.
		void dragBegin();
		/// Must be called by derived classes when they end
		/// a drag.
		void dragEnd();
		/// Should be used in UndoScopes created by
		/// derived classes.
		std::string undoMergeGroup() const;

		/// Utilities to help derived classes update plug values.
		static bool canSetValueOrAddKey( const Gaffer::FloatPlug *plug );
		static void setValueOrAddKey( Gaffer::FloatPlug *plug, float time, float value );

		bool inTargetedMode() const { return m_targetedMode; }

	private :

		void connectToViewContext();
		void contextChanged( const IECore::InternedString &name );
		void plugDirtied( const Gaffer::Plug *plug );
		void metadataChanged( IECore::InternedString key );
		void updateSelection() const;
		void preRender();
		bool keyPress( const GafferUI::KeyEvent &event );
		bool keyRelease( const GafferUI::KeyEvent &event );
		void viewportLeave( const GafferUI::ButtonEvent & event );

		boost::signals::scoped_connection m_contextChangedConnection;
		boost::signals::scoped_connection m_preRenderConnection;

		GafferUI::GadgetPtr m_handles;
		bool m_handlesDirty;

		mutable std::vector<Selection> m_selection;
		mutable bool m_selectionDirty;
		bool m_priorityPathsDirty;
		SelectionChangedSignal m_selectionChangedSignal;

		bool m_dragging;
		int m_mergeGroupId;

		bool m_targetedMode;

		static size_t g_firstPlugIndex;

};

IE_CORE_DECLAREPTR( TransformTool )

} // namespace GafferSceneUI

#endif // GAFFERSCENEUI_TRANSFORMTOOL_H
