'From Squeak3.2alpha of 2 October 2001 [latest update: #4445] on 28 October 2001 at 1:00:27 pm'!"Change Set:		PastEndPutFixDate:			28 October 2001Author:			Dan IngallsAn optimization to WriteStream nextPutAll: had defeated the operation of LimitedWriteStream.  This changeSet suppresses the optimization only when the pastEndPut: logic should be triggered.  It also factors pastEndPut: in LimitedWriteStream so that it calls super.Thus besides restoring the function of LimitedWriteStreams, the change consolidates three independent methods for handling stream overflow."!!WriteStream methodsFor: 'accessing' stamp: 'di 10/28/2001 12:46'!nextPutAll: aCollection	| newEnd |	collection class == aCollection class ifFalse:		[^ super nextPutAll: aCollection ].	newEnd _ position + aCollection size.	newEnd > writeLimit ifTrue:		[^ super nextPutAll: aCollection "Trigger normal pastEndPut: logic"].	collection replaceFrom: position+1 to: newEnd  with: aCollection.	position _ newEnd.! !!LimitedWriteStream methodsFor: 'as yet unclassified' stamp: 'di 10/28/2001 12:49'!pastEndPut: anObject	collection size >= limit ifTrue: [limitBlock value].  "Exceptional return"	^ super pastEndPut: anObject! !