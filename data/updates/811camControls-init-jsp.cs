"Add the camera controls image to WonderlandConstants.  Must follow the file camControlsBMP in the Updates."

WonderlandConstants class == Dictionary ifFalse: [self error: 'needs to be declared'].!

WonderlandConstants at: 'camControlsBMP' put: SmartRefStream scannedObject.!
	
SmartRefStream scannedObject: nil.!