'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:25 pm'!

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:24'!
differenceFromPoint: aPoint 
	^ aPoint x - self @ (aPoint y - self)!
]style[(21 6 5 6 13 6 10)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:24'!
productFromPoint: aPoint 
	^ aPoint x * self @ (aPoint y * self)!
]style[(18 6 5 6 13 6 10)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:24'!
quotientFromPoint: aPoint 
	^ aPoint x / self @ (aPoint y / self)!
]style[(19 6 5 6 13 6 10)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:24'!
sumFromPoint: aPoint 
	^ aPoint x + self @ (aPoint y + self)!
]style[(14 6 5 6 13 6 10)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !


!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:30'!
abs
	"Answer a Point whose x and y are the absolute values of the receiver's 
	x  
	and y."
	^ x abs @ y abs!
]style[(3 2 86 17)f1b,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:30'!
negated
	"Answer a point whose x and y coordinates are the negatives of those of 
	the receiver."
	^ x negated @ y negated!
]style[(7 2 88 25)f1b,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:30'!
reciprocal
	^ x reciprocal @ y reciprocal! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:25'!
ceiling
	"return the point where both x and y are the ceiling values of the 
	receiver "
	^ x ceiling @ y ceiling!
]style[(7 2 79 25)f1b,f1,f1cblue;,f1! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:28'!
floor
	"return the point where both x and y are the ceiling values of the 
	receiver "
	^ x floor @ y floor!
]style[(5 2 79 21)f1b,f1,f1cblue;,f1! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:32'!
truncated
	"Answer a Point that is the receiver's x and y truncated by removing 
	the fractional part."
	^ x truncated @ y truncated!
]style[(9 2 92 29)f1b,f1,f1cblue;,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:28'!
exp
	^ x exp @ y exp! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:28'!
floorLog: aNumber 
	^ (x floorLog: aNumber)
		@ (y floorLog: aNumber)!
]style[(10 7 18 7 19 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:28'!
ln
	^ x ln @ y ln! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:29'!
log: aNumber 
	^ (x log: aNumber)
		@ (y log: aNumber)!
]style[(5 7 13 7 14 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:29'!
raisedTo: aNumber 
	"Number compatibility"
	^ (x raisedTo: aNumber)
		@ (y raisedTo: aNumber)!
]style[(10 7 3 22 17 7 19 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:29'!
raisedToInteger: aNumber 
	"Number compatibility"
	^ (x raisedToInteger: aNumber)
		@ (y raisedToInteger: aNumber)!
]style[(17 7 3 22 24 7 26 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:29'!
sqrt
	^ x sqrt @ y sqrt! !

!Point methodsFor: 'mathematical functions' stamp: 'TAG 10/13/1998 22:29'!
squared
	^ x squared @ y squared! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
differenceFromFloat: aFloat 
	^ aFloat - x @ (aFloat - y)!
]style[(21 6 5 6 8 6 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
differenceFromFraction: aFraction 
	^ aFraction - x @ (aFraction - y)!
]style[(24 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
differenceFromInteger: anInteger 
	^ anInteger - x @ (anInteger - y)!
]style[(23 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
differenceFromPoint: aPoint 
	^ aPoint x - x @ (aPoint y - y)!
]style[(21 6 5 6 10 6 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
productFromFloat: aFloat 
	^ aFloat * x @ (aFloat * y)!
]style[(18 6 5 6 8 6 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
productFromFraction: aFraction 
	^ aFraction * x @ (aFraction * y)!
]style[(21 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:26'!
productFromInteger: anInteger 
	^ anInteger * x @ (anInteger * y)!
]style[(20 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
productFromPoint: aPoint 
	^ aPoint x * x @ (aPoint y * y)!
]style[(18 6 5 6 10 6 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
quotientFromFloat: aFloat 
	^ aFloat / x @ (aFloat / y)!
]style[(19 6 5 6 8 6 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
quotientFromFraction: aFraction 
	^ aFraction / x @ (aFraction / y)!
]style[(22 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
quotientFromInteger: anInteger 
	^ anInteger / x @ (anInteger / y)!
]style[(21 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
quotientFromPoint: aPoint 
	^ aPoint x / x @ (aPoint y / y)!
]style[(19 6 5 6 10 6 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
sumFromFloat: aFloat 
	^ aFloat + x @ (aFloat + y)!
]style[(14 6 5 6 8 6 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
sumFromFraction: aFraction 
	^ aFraction + x @ (aFraction + y)!
]style[(17 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
sumFromInteger: anInteger 
	^ anInteger + x @ (anInteger + y)!
]style[(16 9 5 9 8 9 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:27'!
sumFromPoint: aPoint 
	^ aPoint x + x @ (aPoint y + y)!
]style[(14 6 5 6 10 6 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !



