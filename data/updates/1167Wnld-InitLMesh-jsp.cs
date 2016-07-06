"Adds the light mesh to WonderlandConstants.  Must follow the file Wnld-LightMesh in the Updates."

WonderlandConstants class == Dictionary ifFalse: [self error: 'needs to be declared'].!

WonderlandConstants at: 'lightMesh' put: SmartRefStream scannedObject.!
	
SmartRefStream scannedObject: nil.!