Date:	97 Jul 14 4:49:38 pm
From:	Frank Zdybel <zdybel@interval.com>
To:		squeak@create.ucsb.edu
Subject:	bug in Inspector > replaceSelectionValue:

Haven't checked the latest release, but in 1.19d this
method doesn't work right on instances of variable sized
classes with named instance variables.  With a named
instvar selected, basicAt:put: gets fed a bad index.

Here is a patched version:

'From Squeak 1.19d of April 13, 1997 on 14 July 1997 at 4:14:11 pm'!


!Inspector methodsFor: 'selecting'!
replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| basicIndex |
	selectionIndex < 3 ifTrue: [^ object].
	(object class isVariable not or: [(selectionIndex - 2) <= object class
instSize])
		ifTrue: [^ object instVarAt: 2 put: anObject].
	basicIndex _ selectionIndex - 2 - object class instSize.
	(object basicSize <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [^object basicAt: basicIndex put: anObject]
		ifFalse: [^object basicAt: object basicSize - (self i1 + self i2) +
basicIndex
					put: anObject]! !



