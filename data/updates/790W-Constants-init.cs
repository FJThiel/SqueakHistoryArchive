"Set the dictionary to the last object brought in.  Must follow the file wonderlandConstants in the Updates."

WonderlandConstants class == Dictionary ifFalse: [self error: 'needs to be declared'].!

SmartRefStream scannedObject class == Dictionary ifFalse: [self error: 'need to read object file'].!

WonderlandConstants become: SmartRefStream scannedObject.!
	
SmartRefStream scannedObject: nil.!