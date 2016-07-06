'From Squeak 1.3 of Jan 16, 1998 on 29 January 1998 at 12:25:34 am'!

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
differenceFromPoint: aPoint 
	^aPoint x - self @ (aPoint y - self)! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
productFromPoint: aPoint 
	^aPoint x * self @ (aPoint y * self)! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
quotientFromPoint: aPoint 
	^aPoint x / self @ (aPoint y / self)! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
sumFromPoint: aPoint 
	^aPoint x + self @ (aPoint y + self)! !


!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
negated
	"Answer a point whose x and y coordinates are the negatives of those of the receiver.  6/6/96 sw"

	^ x negated @ y negated! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
reciprocal
	^x reciprocal @ y reciprocal! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
exp
	^x exp @ y exp!
]style[(19)f1b! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
floorLog: aNumber
	^(x floorLog: aNumber) @ (y floorLog: aNumber)!
]style[(65)f1b! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
ln
	^x ln @ y ln!
]style[(16)f1b! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
log: aNumber
	^(x log: aNumber) @ (y log: aNumber)!
]style[(50)f1b! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
raisedTo: aNumber
	"Number compatibility"
	^(x raisedTo: aNumber) @ (y raisedTo: aNumber)!
]style[(41 48)f1b,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
raisedToInteger: aNumber
	"Number compatibility"
	^(x raisedToInteger: aNumber) @ (y raisedToInteger: aNumber)!
]style[(48 6 16 15 16 9)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
sqrt
	^x sqrt @ y sqrt!
]style[(22)f1b! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 1/29/98 0-24:'!
squared
	^x squared @ y squared!
]style[(31)f1b! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
ceiling
	"return the point where both x and y are the ceiling values of the receiver"
	^x ceiling @ y ceiling!
]style[(109)f1b! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
floor
	"return the point where both x and y are the ceiling values of the receiver"
	^x floor @ y floor!
]style[(103)f1b! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
roundTo: aNumber 
	"implemented like Number of upwards scalability"

	^(self / aNumber) rounded * aNumber! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
truncated
	"Answer a Point that is the receiver's x and y truncated by removing the fractional part."

	^x truncated @ y truncated! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
differenceFromFloat: aFloat
	^aFloat - x @ (aFloat - y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
differenceFromFraction: aFraction
	^aFraction - x @ (aFraction - y)!
]style[(33 3 9 8 9 5)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
differenceFromInteger: anInteger
	^anInteger - x @ (anInteger - y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
differenceFromPoint: aPoint
	^aPoint x - x @ (aPoint y - y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
productFromFloat: aFloat
	^aFloat * x @ (aFloat * y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
productFromFraction: aFraction
	^aFraction * x @ (aFraction * y)!
]style[(30 3 9 8 9 5)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
productFromInteger: anInteger
	^anInteger * x @ (anInteger * y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
productFromPoint: aPoint
	^aPoint x * x @ (aPoint y * y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
quotientFromFloat: aFloat
	^aFloat / x @ (aFloat / y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
quotientFromFraction: aFraction
	^aFraction / x @ (aFraction / y)!
]style[(31 3 9 8 9 5)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
quotientFromInteger: anInteger
	^anInteger / x @ (anInteger / y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
quotientFromPoint: aPoint
	^aPoint x / x @ (aPoint y / y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
sumFromFloat: aFloat
	^aFloat + x @ (aFloat + y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
sumFromFraction: aFraction
	^aFraction + x @ (aFraction + y)!
]style[(26 3 9 8 9 5)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
sumFromInteger: anInteger
	^anInteger + x @ (anInteger + y)! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 1/29/98 0-23:'!
sumFromPoint: aPoint
	^aPoint x + x @ (aPoint y + y)! !



