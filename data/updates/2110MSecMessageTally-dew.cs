'From Squeak2.8alpha of 4 February 2000 [latest update: #2052] on 30 April 2000 at 3:25:10 pm'!"Change Set:		020MSecMessageTally-dewDate:			15 March 2000Author:			Doug WayAdds a millisecond count to the output of MessageTally>>spyOn: (and TimeProfileBrowser).  Also adds a percent sign to the percentage value."!!MessageTally methodsFor: 'printing' stamp: 'dew 3/15/2000 21:49'!fullPrintOn: aStream tallyExact: isExact orThreshold: perCent	| threshold |  	isExact ifFalse: [threshold _ (perCent asFloat / 100 * tally) rounded].	aStream nextPutAll: '**Tree**'; cr.	self treePrintOn: aStream		tabs: OrderedCollection new		thisTab: ''		total: tally		totalTime: time		tallyExact: isExact		orThreshold: threshold.	aStream nextPut: Character newPage; cr.	aStream nextPutAll: '**Leaves**'; cr.	self leavesPrintOn: aStream		tallyExact: isExact		orThreshold: threshold! !!MessageTally methodsFor: 'printing' stamp: 'dew 3/22/2000 02:28'!leavesPrintOn: aStream tallyExact: isExact orThreshold: threshold	| dict |	dict _ IdentityDictionary new: 100.	self leavesInto: dict fromSender: nil.	isExact ifTrue: 		[dict asSortedCollection			do: [:node |				node printOn: aStream total: tally totalTime: nil tallyExact: isExact.				node printSenderCountsOn: aStream]]		ifFalse:		[(dict asOrderedCollection				select: [:node | node tally > threshold])			asSortedCollection			do: [:node |				node printOn: aStream total: tally totalTime: time tallyExact: isExact]]! !!MessageTally methodsFor: 'printing' stamp: 'dew 3/15/2000 21:56'!printOn: aStream total: total totalTime: totalTime tallyExact: isExact	| aSelector className myTally aClass percentage |	isExact		ifTrue:			[myTally _ tally.			receivers == nil				ifFalse: [receivers do: [:r | myTally _ myTally - r tally]].			aStream print: myTally; space]		ifFalse:			[percentage _ tally asFloat / total * 100.0 roundTo: 0.1.			aStream				print: percentage;				nextPutAll: '% {';				print: (percentage * totalTime / 100) rounded;				nextPutAll: 'ms} '].	receivers == nil		ifTrue: [aStream nextPutAll: 'primitives'; cr]		ifFalse: 			[aSelector _ class selectorAtMethod: method setClass: [:c | aClass _c].			className _ aClass name contractTo: 30.			aStream nextPutAll: class name;				nextPutAll: (aClass = class ifTrue: ['>>']								ifFalse: ['(' , aClass name , ')>>']);				nextPutAll: (aSelector contractTo: 60-className size); cr]! !!MessageTally methodsFor: 'printing' stamp: 'dew 3/22/2000 02:28'!printSenderCountsOn: aStream	| mergedSenders mergedNode |	mergedSenders _ IdentityDictionary new.	senders do:		[:node |		mergedNode _ mergedSenders at: node method ifAbsent: [nil].		mergedNode == nil			ifTrue: [mergedSenders at: node method put: node]			ifFalse: [mergedNode bump: node tally]].	mergedSenders asSortedCollection do:		[:node | 		10 to: node tally printString size by: -1 do: [:i | aStream space].		node printOn: aStream total: tally totalTime: nil tallyExact: true]! !!MessageTally methodsFor: 'printing' stamp: 'dew 3/15/2000 21:51'!treePrintOn: aStream tabs: tabs thisTab: myTab	total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold	| sons sonTab |	tabs do: [:tab | aStream nextPutAll: tab].	tabs size > 0 ifTrue: [self printOn: aStream total: total totalTime: totalTime tallyExact: isExact].	sons _ isExact		ifTrue: [receivers]		ifFalse: [self sonsOver: threshold].	sons isEmpty ifFalse:		[tabs addLast: myTab.		sons _ sons asSortedCollection.		(1 to: sons size) do: 			[:i |			sonTab _ i < sons size ifTrue: ['  |'] ifFalse: ['  '].			(sons at: i) treePrintOn: aStream				tabs: (tabs size < 18					ifTrue: [tabs]					ifFalse: [(tabs select: [:x | x = '[']) copyWith: '['])				thisTab: sonTab total: total totalTime: totalTime				tallyExact: isExact orThreshold: threshold].		tabs removeLast]! !MessageTally removeSelector: #printOn:total:tallyExact:!MessageTally removeSelector: #treePrintOn:tabs:thisTab:total:tallyExact:orThreshold:!