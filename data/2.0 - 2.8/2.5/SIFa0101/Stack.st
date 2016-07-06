"This Squeak format file contains the Squeak implementation of Stack.  This class provides a more traditional push/pop interface for Arrays.

=================================================
    Copyright (c) 1992 by Justin O. Graver.
    All rights reserved (with exceptions).
=================================================

 ******
 * $Name:  $
 * $Id: Stack.st 1.1 1999-10-11 07:05:48-05 Harmon Alpha $
 * $Log: Stack.st $
 * Revision 1.1  1999-10-11 07:05:48-05  Harmon
 * Initial revision
 *
 ******"!

'From Squeak 2.3 of January 14, 1999 on 25 July 1999 at 10:18:00 am'!
Array variableSubclass: #Stack
	instanceVariableNames: 'topPtr '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Stacked'!

!Stack commentStamp: '<historical>' prior: 0!
=================================================
    Copyright (c) 1992 by Justin O. Graver.
    All rights reserved (with exceptions)."
=================================================

This class provides a more traditional push/pop interface for Arrays.
!

!Stack methodsFor: 'private' stamp: 'RAH 07/08/1999 07:08:59'!
copyEmpty: aSize
	"Answer a copy of the receiver that contains no elements.
	This method should be redefined in subclasses that add instance variables, so that the state of those variables is preserved"
		#Stack. "Protocol: ."
	^(super copyEmpty: aSize) topPtr: self topPtr.! !

!Stack methodsFor: 'accessing' stamp: 'RAH 07/08/1999 07:08:59'!
pop
	"Answer the object on top of the stack."
	| n |
		#Stack. "Protocol: ."
	n := self at: topPtr.
	topPtr := topPtr - 1.
	^n! !

!Stack methodsFor: 'accessing' stamp: 'RAH 07/08/1999 07:08:59'!
pop: numElem 
	"Pop and discard top numElems and answer receiver"
		#Stack. "Protocol: ."
	topPtr := topPtr - numElem! !

!Stack methodsFor: 'accessing' stamp: 'RAH 07/08/1999 07:08:59'!
push: anObject 
	"Push anObject onto the top of the stack."
		#Stack. "Protocol: ."
	self isFull ifTrue: [self grow].
	topPtr := topPtr + 1.
	^self at: topPtr put: anObject! !

!Stack methodsFor: 'accessing' stamp: 'RAH 07/08/1999 07:08:59'!
size
	"Answer the number of objects on the stack."
		#Stack. "Protocol: ."
	^topPtr! !

!Stack methodsFor: 'accessing' stamp: 'RAH 07/08/1999 07:08:59'!
top
	"Answer (without removing) the object on top of the stack."
		#Stack. "Protocol: ."
	^self at: topPtr! !

!Stack methodsFor: 'testing' stamp: 'RAH 07/08/1999 07:08:59'!
isEmpty
		#Stack. "Protocol: ."
	^topPtr = 0! !

!Stack methodsFor: 'testing' stamp: 'RAH 07/08/1999 07:08:59'!
isFull
		#Stack. "Protocol: ."
	^ topPtr = self basicSize! !

!Stack methodsFor: 'state accessing' stamp: 'RAH 07/08/1999 07:08:59'!
topPtr
		#Stack. "Protocol: ."
	^topPtr! !

!Stack methodsFor: 'state accessing' stamp: 'RAH 07/08/1999 07:08:59'!
topPtr: arg
		#Stack. "Protocol: ."
	topPtr := arg! !

!Stack methodsFor: 'initialization' stamp: 'RAH 07/08/1999 07:08:59'!
init
		#Stack. "Protocol: ."
	self topPtr: 0! !

!Stack methodsFor: 'enumerating' stamp: 'RAH 07/08/1999 07:08:59'!
do: aBlock
	"Evaluate aBlock for each object on the stack, from top to bottom."
		#Stack. "Protocol: ."
	^super reverseDo: aBlock! !

!Stack methodsFor: 'enumerating' stamp: 'RAH 07/08/1999 07:08:59'!
reverseDo: aBlock
	"Evaluate aBlock for each object on the stack, from bottom to top."
		#Stack. "Protocol: ."
	^super do: aBlock! !


!Stack class methodsFor: 'instance creation' stamp: 'RAH 07/08/1999 07:08:59'!
new
		#Stack. "Protocol: ."
	^self new: 100! !

!Stack class methodsFor: 'instance creation' stamp: 'RAH 07/08/1999 07:08:59'!
new: arg
		#Stack. "Protocol: ."
	^( super new: arg ) init! !


