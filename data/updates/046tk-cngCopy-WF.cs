'From Squeak 2.0 BETA of May 8, 1998 on 19 May 1998 at 4:36:35 pm'!!ChangeSet methodsFor: 'fileIn/Out' stamp: 'tk 5/19/1998 16:35'!fileOutPSFor: class on: stream 	"Write out removals and initialization for this class."	(methodChanges at: class name ifAbsent: [^self]) associationsDo: 		[:mAssoc | 		mAssoc value = #remove ifTrue: [stream nextChunkPut:				class name, ' removeSelector: ', mAssoc key storeString; cr.				^ self].		mAssoc value = #addedThenRemoved ifTrue: [stream nextChunkPut:				class name, ' removeSelector: ', mAssoc key storeString; cr.				^ self].		(mAssoc key = #initialize and: [class isMeta])					ifTrue: [stream nextChunkPut: class soleInstance name, ' initialize'; cr]]! !!ChangeSorter methodsFor: 'access' stamp: 'tk 5/19/1998 16:23'!showChangeSet: chgSet	myChangeSet == chgSet ifFalse: [		myChangeSet _ chgSet.		currentClassName _ nil.		currentSelector _ nil].	self changed: #relabel.	self changed: #mainButtonName.	self changed: #classList.	self changed: #messageList.	self setContents.	self changed: #contents.! !