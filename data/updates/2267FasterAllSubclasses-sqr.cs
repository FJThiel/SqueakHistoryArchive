'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 1 June 2000 at 1:06:04 pm'!"Change Set:		allSubclassesDate:			25 May 2000Author:			Andres ValloudImproved two methods in ClassDescription. Changed the implementation from recursive(slow) to iterative (fast)."!!ClassDescription methodsFor: 'accessing class hierarchy' stamp: 'sma 6/1/2000 12:22'!allSubclasses	"Answer a Set of the receiver's and the receiver's descendent's subclasses."	| scan scanTop |	scan _ OrderedCollection withAll: self subclasses.	scanTop _ 1.	[scanTop > scan size] whileFalse:		[scan addAll: (scan at: scanTop) subclasses.		scanTop _ scanTop + 1].	^ scan asSet! !!ClassDescription methodsFor: 'accessing class hierarchy' stamp: 'SqR 5/25/2000 16:54'!withAllSubclasses	"Answer a Set of the receiver, the receiver's descendent's, and the 	receiver's descendent's subclasses."	^ self allSubclasses add: self; yourself! !