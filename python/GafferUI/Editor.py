##########################################################################
#
#  Copyright (c) 2011-2012, John Haddon. All rights reserved.
#  Copyright (c) 2012-2013, Image Engine Design Inc. All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#
#      * Redistributions of source code must retain the above
#        copyright notice, this list of conditions and the following
#        disclaimer.
#
#      * Redistributions in binary form must reproduce the above
#        copyright notice, this list of conditions and the following
#        disclaimer in the documentation and/or other materials provided with
#        the distribution.
#
#      * Neither the name of John Haddon nor the names of
#        any other contributors to this software may be used to endorse or
#        promote products derived from this software without specific prior
#        written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
##########################################################################

import types

import IECore

import Gaffer
import GafferUI

from Qt import QtCore
from Qt import QtWidgets

import uuid

class _EditorMetaclass( Gaffer.Trackable.__class__ ) :

	def __call__( cls, *args, **kw ) :

		instance = type.__call__( cls, *args, **kw )
		while hasattr( cls, "instanceCreatedSignal" ) :
			cls.instanceCreatedSignal()( instance )
			cls = cls.__bases__[0]

		return instance

## Base class for UI components which display or manipulate a ScriptNode
# or its children. These make up the tabs in the UI layout.
class Editor( GafferUI.Widget ) :

	__metaclass__ = _EditorMetaclass

	def __init__( self, topLevelWidget, scriptNode, _restorationID=None, **kw ) :

		GafferUI.Widget.__init__( self, topLevelWidget, **kw )

		self.__restorationID = _restorationID if _restorationID else str(uuid.uuid4())

		self._qtWidget().setFocusPolicy( QtCore.Qt.ClickFocus )

		assert( isinstance( scriptNode, Gaffer.ScriptNode ) )

		self.__scriptNode = scriptNode
		self.__context = None

		self.__title = ""
		self.__titleChangedSignal = GafferUI.WidgetSignal()

		self.enterSignal().connect( Gaffer.WeakMethod( self.__enter ), scoped = False )
		self.leaveSignal().connect( Gaffer.WeakMethod( self.__leave ), scoped = False )

		self.__setContextInternal( scriptNode.context(), callUpdate=False )

	def scriptNode( self ) :

		return self.__scriptNode

	## May be called to explicitly set the title for this editor. The
	# editor itself is not responsible for displaying the title - this
	# is left to the enclosing ui.
	def setTitle( self, title ) :

		if title == self.__title :
			return

		self.__title = title
		self.titleChangedSignal()( self )

	## May be overridden to provide sensible default behaviour for
	# the title, but must return BaseClass.getTitle() if it is non-empty.
	def getTitle( self ) :

		if self.__title :
			return self.__title

		# if there's no explicit title and a derived class
		# has overridden getTitle() then we return the empty
		# string to signify that the derived class is free
		# to return what it wants
		c = self.__class__
		while c is not Editor :
			if "getTitle" in c.__dict__ :
				return ""
			c = c.__bases__[0]

		# otherwise we default to using the classname
		return IECore.CamelCase.toSpaced( self.__class__.__name__ )

	## A signal emitted whenever the title changes.
	def titleChangedSignal( self ) :

		return self.__titleChangedSignal

	## By default Editors operate in the main context held by the script node. This function
	# allows an alternative context to be provided, making it possible for an editor to
	# display itself at a custom frame (or with any other context modification).
	def setContext( self, context ) :

		self.__setContextInternal( context, callUpdate=True )

	def getContext( self ) :

		return self.__context

	def __setContextInternal( self, context, callUpdate ) :

		assert( isinstance( context, ( Gaffer.Context, types.NoneType ) ) )

		previousContext = self.__context
		self.__context = context
		if self.__context is not None :
			self.__contextChangedConnection = self.__context.changedSignal().connect( Gaffer.WeakMethod( self.__contextChanged ) )
		else :
			## \todo I'm not sure why this code allows a None context - surely we
			# should always have a valid one?
			self.__contextChangedConnection = None

		if callUpdate :
			modifiedItems = set()
			if previousContext is not None :
				modifiedItems |= set( previousContext.names() )
			if self.__context is not None :
				modifiedItems |= set( self.__context.names() )
			self._updateFromContext( modifiedItems )

	## May be implemented by derived classes to update state based on a change of context.
	# To temporarily suspend calls to this function, use Gaffer.BlockedConnection( self._contextChangedConnection() ).
	def _updateFromContext( self, modifiedItems ) :

		pass

	def _contextChangedConnection( self ) :

		return self.__contextChangedConnection

	## The restorationID is a persistent ID for the editor that can be used
	# to serialise references to a particular editor instance on disk.
	# Newly constructed editors are assigned a unique id, which is then persisted
	# though any repr/eval cycles.
	def _restorationID( self ) :

		return self.__restorationID

	## This must be implemented by all derived classes as it is used for serialisation of layouts.
	# Considerations:
	#  - It is not expected that the script being edited is also serialised as
	#    part of this operation - instead the new script will be provided later
	#    as a variable named scriptNode.
	#  - The serialisation must include the base class kwargs.
	# So a suitable serialisation will look like:
	#    "GafferUI.Editor( scriptNode, %s )" % self._reprStandardKwargs().
	def __repr__( self ) :

		raise NotImplementedError

	# Returns a string suitable for inclusion in a serialisation containing
	# all kwargs required for the correct initialisation of the base Editor class.
	def _reprStandardKwargs( self ) :

		return "_restorationID='%s'" % self.__restorationID

	def __contextChanged( self, context, key ) :

		assert( context.isSame( self.getContext() ) )

		self._updateFromContext( set( [ key ] ) )

	@classmethod
	def types( cls ) :

		return cls.__namesToCreators.keys()

	@classmethod
	def create( cls, name, scriptNode ) :

		return cls.__namesToCreators[name]( scriptNode = scriptNode )

	@classmethod
	def registerType( cls, name, creator ) :

		cls.__namesToCreators[name] = creator

	__namesToCreators = {}

	@classmethod
	def instanceCreatedSignal( cls ) :

		s = cls.__dict__.get( "__instanceCreatedSignal", None )
		if s is not None :
			return s

		s = Gaffer.Signal1()
		setattr( cls, "__instanceCreatedSignal", s )
		return s

	def __enter( self, widget ) :

		if not isinstance( QtWidgets.QApplication.focusWidget(), ( QtWidgets.QLineEdit, QtWidgets.QPlainTextEdit ) ) :
			self._qtWidget().setFocus()

	def __leave( self, widget ) :

		self._qtWidget().clearFocus()

