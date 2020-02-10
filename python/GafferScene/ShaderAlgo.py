##########################################################################
#
#  Copyright (c) 2020, Cinesite VFX Ltd. All rights reserved.
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
#      * Neither the name of Cinesite VFX Ltd. nor the names of
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

__nodeCreators = {}

def CreateNodesForNetwork( network, parent ) :

	nodes = {}

	for shaderName in network.shaders() :

		shader = network.getShader( shaderName )

		node = None

		try :
			node = __nodeCreators[ shader.type ]( shader.name )
		except Exception as error :
			print(
				"Unable to create '%s' (%s shader '%s') : %s"
				% ( shaderName, shader.type, shader.name, error )
			)
			continue

		if not node :
			print(
				"Unable to create '%s' (%s shader '%s')."
				% ( shaderName, shader.type, shader.name )
			)
			continue

		node.setName( shaderName )

		nodes[shaderName] = node
		parent.addChild( node )

		for name, valueData in shader.parameters.items() :
			try :
				node["parameters"][name].setValue( valueData.value )
			except Exception as error :
				print(
					"Unable to set the parameter '%s' on '%s': %s"
					% ( name, shaderName, error )
				)

	for shaderName in network.shaders() :

		destNode = nodes.get( shaderName, None )
		if not destNode :
			print(
				"Unable to recreate connections to %s as the shader is unavailable."
				% shaderName
			)
			continue

		for connection in network.inputConnections( shaderName ) :

			srcNode = nodes.get( connection.source.shader, None )
			if not srcNode :
				print(
					"Unable to recreate the connection %s.%s -> %s.%s as the source shader is unavailable."
					% (
						connection.source.shader, connection.source.name,
						connection.destination.shader, connection.destination.name
					)
				)
				continue

			srcName = connection.source.name or "out"
			try :
				destNode["parameters"][ connection.destination.name ].setInput( srcNode[ srcName ] )
			except Exception as error :
				print(
					"Unable to recreate the connection %s.%s -> %s.%s: %s"
					% (
						connection.source.shader, connection.source.name,
						connection.destination.shader, connection.destination.name,
						error
					)
				)
				continue

	return nodes.get( network.getOutput().shader, None )

def CrateNodesForShader( scenePlug, location, shader, parent ) :

	attributes = scenePlug.attributes( location )
	if shader in attributes :
		return CreateNodesForNetwork( attributes[shader], parent )
	else :
		raise RuntimeError( "The shader '%s' does not exist on '%s'." % ( shader, location ) )

def RegisterShaderNodeCreator( namespace, nodeCreator ) :

	__nodeCreators[ namespace ] = nodeCreator
