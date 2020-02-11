##########################################################################
#
#  Copyright (c) 2012, John Haddon. All rights reserved.
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

__import__( "GafferScene" )

try :

	# Make sure we import IECoreArnold and _GafferArnold
	# _without_ RTLD_GLOBAL. This prevents clashes between the
	# LLVM symbols in libai.so and the Mesa OpenGL driver.
	# Ideally we wouldn't use RTLD_GLOBAL anywhere - see
	# https://github.com/ImageEngine/cortex/pull/810.

	import sys
	import ctypes
	originalDLOpenFlags = sys.getdlopenflags()
	sys.setdlopenflags( originalDLOpenFlags & ~ctypes.RTLD_GLOBAL )

	__import__( "IECoreArnold" )
	from _GafferArnold import *

finally :

	sys.setdlopenflags( originalDLOpenFlags )
	del sys, ctypes, originalDLOpenFlags

from ArnoldShaderBall import ArnoldShaderBall
from ArnoldTextureBake import ArnoldTextureBake

def __createArnoldShader( shaderName, nodeType ) :
	s = nodeType()
	s.loadShader( shaderName )
	return s

import GafferScene
GafferScene.ShaderAlgo.RegisterShaderNodeCreator( "ai:shader", lambda s : __createArnoldShader( s, ArnoldShader ) )
GafferScene.ShaderAlgo.RegisterShaderNodeCreator( "ai:surface", lambda s : __createArnoldShader( s, ArnoldShader ) )
GafferScene.ShaderAlgo.RegisterShaderNodeCreator( "ai:light", lambda s : __createArnoldShader( s, ArnoldLight ) )
GafferScene.ShaderAlgo.RegisterShaderNodeCreator( "ai:lightFilter", lambda s : __createArnoldShader( s, ArnoldLightFilter ) )

__import__( "IECore" ).loadConfig( "GAFFER_STARTUP_PATHS", subdirectory = "GafferArnold" )
