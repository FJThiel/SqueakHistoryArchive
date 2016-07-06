'From Squeakland 3.8.5976 of 19 August 2004 [latest update: #243] on 21 August 2004 at 7:11:01 pm'!"Change Set:		SqueakMapCheckpointFix-nkDate:			21 August 2004Author:			Ned KonzIn the 243 devel image, you can't open a SqueakMap package loader.This fixes that problem. Again."!!SMSqueakMap methodsFor: 'checkpoints' stamp: 'nk 8/21/2004 19:10'!createCheckpointNumber: number	"Export me using an ImageSegment.	This is used for checkpointing the map on disk	in a form that can be brought into an independent image.	We do not overwrite older versions, since using ImageSegments	is an intermediate hack anyway we don't care about the disk waste!!	Sidenote: Some refactoring was needed to produce a .gz file directly so	I didn't bother."	| is fname stream oldMutex |	fname _ self filename, '.', number asString, '.s'.	(self directory fileExists: fname) ifTrue: [self error: 'Checkpoint already exists!!'].	stream _ StandardFileStream newFileNamed: (self directory fullNameFor: fname).	checkpointNumber _ number.	oldMutex _ mutex.	mutex _ nil. self clearCaches.	[is _ ImageSegment new.	is copyFromRoots: (Array with: self) sizeHint: 1000000 areUnique: true.	is writeForExportOn: stream.	self compressFile: (StandardFileStream oldFileNamed: (self directory fullNameFor: fname)).	isDirty _ false]		ensure: [mutex _ oldMutex].	^is! !