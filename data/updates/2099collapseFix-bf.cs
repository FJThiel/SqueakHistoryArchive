'From Squeak2.8alpha of 18 January 2000 [latest update: #2098] on 11 May 2000 at 11:42:45 am'!"Change Set:		collapseFix-bfDate:			11 May 2000Author:			Bert FreudenbergThe CollapsedMorph would not be deleted when it was made unclosable (via window menu) and then expanded."!!CollapsedMorph methodsFor: 'as yet unclassified' stamp: 'bf 5/11/2000 11:41'!collapseOrExpand	isCollapsed		ifTrue: 			[uncollapsedMorph setProperty: #collapsedPosition toValue: self position.			mustNotClose _ false.	"We're not closing but expanding"			self delete.			self currentWorld addMorphFront: uncollapsedMorph]		ifFalse:			[super collapseOrExpand]! !