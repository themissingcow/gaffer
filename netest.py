import Gaffer
import GafferArnold
import GafferUI

s = Gaffer.ScriptNode()

s["surface"] = GafferArnold.ArnoldShader()
s["surface"].loadShader( "standard_surface" )
s["node"] = Gaffer.Node()

ne = GafferUI.NodeEditor.acquire( s["surface"] )

a = Gaffer.StandardSet( [ s["surface"] ] )
b = Gaffer.StandardSet( [ s["node"] ] )

for i in range( 1000 ) :
	ne.setNodeSet( a )
	ne.setNodeSet( b )
