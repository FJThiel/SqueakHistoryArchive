"Adds the light mesh to WonderlandConstants.  Must follow the file Wnld-LightTexture in the Updates."

WonderlandConstants class == Dictionary ifFalse: [self error: 'needs to be declared'].!

WonderlandConstants at: 'lightTexture' put: SmartRefStream scannedObject.!
	
SmartRefStream scannedObject: nil.!