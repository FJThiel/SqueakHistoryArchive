Date:	97 Jul 17 3:36:12 pm
From:	Frank Zdybel <zdybel@interval.com>
To:		squeak@create.ucsb.edu
Subject:	bad patch for Inspector > replaceSelectionValue:

I sent a bad version of the patch; sorry.  Here
is a better one.

frank

'From Squeak 1.2 of June 29, 1997 on 17 July 1997 at 3:00:31 pm'!
!Inspector methodsFor: 'selecting' stamp: 'fz 7/17/97 14:58'!
replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| basicIndex |
	selectionIndex = 1 ifTrue: [^ object].
	(object class isVariable not or: [(selectionIndex - 2) <= object class
instSize])
		ifTrue: [^ object instVarAt: selectionIndex - 2 put: anObject].
	basicIndex _ selectionIndex - 2 - object class instSize.
	(object basicSize <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [^object basicAt: basicIndex put: anObject]
		ifFalse: [^object basicAt: object basicSize - (self i1 + self i2) +
basicIndex
					put: anObject]! !


