'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 15 February 2004 at 8:29 am'!"Change Set:		MessageListDragFix-nkDate:			15 February 2004Author:			Ned KonzFixes message dragging from MessageLists."!!Browser methodsFor: 'drag and drop' stamp: 'nk 2/14/2004 20:49'!dragPassengerFor: item inMorph: dragSource 	| transferType |	(dragSource isKindOf: PluggableListMorph)		ifFalse: [^item].	transferType _ self dragTransferTypeForMorph: dragSource.	transferType == #messageList		ifTrue: [^self selectedClassOrMetaClass-> self selectedMessageName ].	transferType == #classList		ifTrue: [^self selectedClass].	^item contents! !