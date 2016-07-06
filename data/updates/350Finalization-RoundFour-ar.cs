'From Squeak 2.2 of Sept 23, 1998 on 8 October 1998 at 11:09:59 am'!"Change Set:		Finalization-SockStreamDate:			7 October 1998Author:			Andreas RaabRound four of the Finalization updates installs weak registriesin class Socket and StandardFileStream, but does not activate them.(this has to do with the fileIn order of certain methods)"!Object subclass: #Socket	instanceVariableNames: 'semaphore socketHandle '	classVariableNames: 'Connected DeadServer InvalidSocket OtherEndClosed Registry ThisEndClosed Unconnected WaitingForConnection '	poolDictionaries: ''	category: 'System-Network'!FileStream subclass: #StandardFileStream	instanceVariableNames: 'name fileID buffer1 '	classVariableNames: 'Registry '	poolDictionaries: ''	category: 'System-Files'!!FileDirectory methodsFor: 'file operations' stamp: 'ar 3/21/98 18:08'!deleteFileNamed: localFileName ifAbsent: failBlock	"Delete the file of the given name if it exists, else evaluate failBlock.	If the first deletion attempt fails do a GC to force finalization of any lost references. ar 3/21/98 17:53"	(self 		retryWithGC:[self primDeleteFileNamed: (self fullNameFor: localFileName)]		until:[:result| result notNil]) == nil			ifTrue: [^failBlock value].! !!FileDirectory methodsFor: 'file operations' stamp: 'ar 3/21/98 18:10'!rename: oldFileName toBe: newFileName	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name."	"Modified for retry after GC ar 3/21/98 18:09"	(self retryWithGC:[self primRename: (self fullNameFor: oldFileName)						to: (self fullNameFor: newFileName)]		until:[:result| result notNil]) == nil ifTrue:[	self error:'Attempt to rename a non-existent file,or to use a name that is already in use'].! !!FileDirectory methodsFor: 'private' stamp: 'ar 3/21/98 18:04'!primRename: oldFileFullName to: newFileFullName 	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name.	Changed to return nil instead of failing ar 3/21/98 18:04"	<primitive: 159>	^nil! !!Socket methodsFor: 'primitives' stamp: 'ar 3/21/98 17:43'!primSocketDestroyGently: socketID	"Release the resources associated with this socket. If a connection is open, it is aborted.	Do not fail if the receiver is already closed."	<primitive: 210>! !!Socket methodsFor: 'registry' stamp: 'ar 3/21/98 17:40'!register	^self class register: self! !!Socket methodsFor: 'registry' stamp: 'ar 3/21/98 17:41'!unregister	^self class unregister: self! !!Socket methodsFor: 'finalization' stamp: 'ar 3/21/98 17:44'!finalize	self primSocketDestroyGently: socketHandle.	Smalltalk unregisterExternalObject: semaphore.! !!FTPSocket methodsFor: 'finalization' stamp: 'ar 3/21/98 18:19'!actAsExecutor	super actAsExecutor.	dataSocket := nil.! !!Socket class methodsFor: 'registry' stamp: 'ar 10/7/1998 14:40'!register: anObject	WeakArray isFinalizationSupported ifFalse:[^anObject].	self registry add: anObject! !!Socket class methodsFor: 'registry' stamp: 'ar 10/7/1998 14:40'!registry	WeakArray isFinalizationSupported ifFalse:[^nil].	^Registry isNil		ifTrue:[Registry := WeakRegistry new]		ifFalse:[Registry].! !!Socket class methodsFor: 'registry' stamp: 'ar 10/7/1998 15:22'!unregister: anObject	WeakArray isFinalizationSupported ifFalse:[^anObject].	self registry remove: anObject ifAbsent:[]! !!StandardFileStream methodsFor: 'registry' stamp: 'ar 3/21/98 17:23'!register	^self class register: self! !!StandardFileStream methodsFor: 'registry' stamp: 'ar 3/21/98 17:23'!unregister	^self class unregister: self! !!StandardFileStream methodsFor: 'finalization' stamp: 'ar 3/21/98 18:16'!actAsExecutor	super actAsExecutor.	name := nil.! !!StandardFileStream methodsFor: 'finalization' stamp: 'ar 10/7/1998 15:44'!finalize	self primCloseNoError: fileID.! !!StandardFileStream class methodsFor: 'registry' stamp: 'ar 10/7/1998 14:41'!register: anObject	WeakArray isFinalizationSupported ifFalse:[^anObject].	self registry add: anObject! !!StandardFileStream class methodsFor: 'registry' stamp: 'ar 10/7/1998 14:41'!registry	WeakArray isFinalizationSupported ifFalse:[^nil].	^Registry isNil		ifTrue:[Registry := WeakRegistry new]		ifFalse:[Registry].! !!StandardFileStream class methodsFor: 'registry' stamp: 'ar 10/7/1998 15:23'!unregister: anObject	WeakArray isFinalizationSupported ifFalse:[^anObject].	self registry remove: anObject ifAbsent:[]! !