'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:16 pm'!

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:38'!
zeroDivideError
	^ self error: 'Cannot divide by zero'! !


!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:34'!
differenceFromFloat: aFloat 
	^ self primitiveFailed!
]style[(21 6 25)f1b,f1cgreen;b,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
differenceFromFraction: aFraction 
	^ aFraction asFloat - self!
]style[(24 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
differenceFromInteger: anInteger 
	^ anInteger asFloat - self!
]style[(23 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
productFromFloat: aFloat 
	^ self primitiveFailed!
]style[(18 6 25)f1b,f1cgreen;b,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
productFromFraction: aFraction 
	^ aFraction asFloat * self!
]style[(21 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
productFromInteger: anInteger 
	^ anInteger asFloat * self!
]style[(20 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
quotientFromFloat: aFloat 
	^ self = 0.0
		ifTrue: [self zeroDivideError]
		ifFalse: [self primitiveFailed]!
]style[(19 6 82)f1b,f1cgreen;b,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
quotientFromFraction: aFraction 
	^ aFraction asFloat / self!
]style[(22 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
quotientFromInteger: anInteger 
	^ anInteger asFloat / self!
]style[(21 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
sumFromFloat: aFloat 
	^ self primitiveFailed!
]style[(14 6 25)f1b,f1cgreen;b,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
sumFromFraction: aFraction 
	^ aFraction asFloat + self!
]style[(17 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:35'!
sumFromInteger: anInteger 
	^ anInteger asFloat + self!
]style[(16 9 5 9 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !


!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
differenceFromFloat: aFloat 
	^ aFloat - self asFloat!
]style[(21 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
differenceFromFraction: aFraction 
	^ aFraction numerator * denominator - (numerator * aFraction denominator) / (aFraction denominator * denominator)!
]style[(24 9 5 9 40 9 17 9 27)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
differenceFromInteger: anInteger 
	^ anInteger * denominator - numerator / denominator!
]style[(23 9 5 9 40)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
productFromFloat: aFloat 
	^ aFloat * self asFloat!
]style[(18 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
productFromFraction: aFraction 
	^ aFraction numerator * numerator / (aFraction denominator * denominator)!
]style[(21 9 5 9 26 9 27)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
productFromInteger: anInteger 
	^ anInteger * numerator / denominator!
]style[(20 9 5 9 26)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
quotientFromFloat: aFloat 
	^ aFloat / self asFloat!
]style[(19 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
quotientFromFraction: aFraction 
	^ aFraction numerator * denominator / (aFraction denominator * numerator)!
]style[(22 9 5 9 28 9 25)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
quotientFromInteger: anInteger 
	^ anInteger * denominator / numerator!
]style[(21 9 5 9 26)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
sumFromFloat: aFloat 
	^ aFloat + self asFloat!
]style[(14 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
sumFromFraction: aFraction 
	^ aFraction numerator * denominator + (numerator * aFraction denominator) / (aFraction denominator * denominator)!
]style[(17 9 5 9 40 9 17 9 27)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:36'!
sumFromInteger: anInteger 
	^ anInteger * denominator + numerator / denominator!
]style[(16 9 5 9 40)f1b,f1cgreen;b,f1,f1cblue;,f1! !


!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
differenceFromFloat: aFloat 
	^ aFloat - self asFloat!
]style[(21 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
differenceFromFraction: aFraction 
	^ aFraction numerator - (self * aFraction denominator) / aFraction denominator!
]style[(24 9 5 9 21 9 16 9 12)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
differenceFromInteger: anInteger 
	^ self negative == anInteger negative
		ifTrue: [anInteger digitSubtract: self]
		ifFalse: [(anInteger digitAdd: self) normalize]!
]style[(23 9 22 9 21 9 35 9 27)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
productFromFloat: aFloat 
	^ aFloat * self asFloat!
]style[(18 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
productFromFraction: aFraction 
	^ aFraction numerator * self / aFraction denominator!
]style[(21 9 5 9 20 9 12)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
productFromInteger: anInteger 
	^ self digitMultiply: anInteger neg: self negative ~~ anInteger negative!
]style[(20 9 25 9 23 9 9)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
quotientFromFloat: aFloat 
	^ aFloat / self asFloat!
]style[(19 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
quotientFromFraction: aFraction 
	^ aFraction numerator / (aFraction denominator * self)!
]style[(22 9 5 9 14 9 20)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:37'!
quotientFromInteger: anInteger 
	| quoRem |
	quoRem _ anInteger abs digitDiv: self abs neg: self negative ~~ anInteger negative.
	^ (quoRem at: 2)
		= 0
		ifTrue: [(quoRem at: 1) normalize]
		ifFalse: [(Fraction numerator: anInteger denominator: self) reduced]!
]style[(21 9 5 7 3 6 3 9 46 9 15 6 26 6 52 9 28)f1b,f1cgreen;b,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:38'!
sumFromFloat: aFloat 
	^ aFloat + self asFloat!
]style[(14 6 5 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:38'!
sumFromFraction: aFraction 
	^ aFraction numerator + (self * aFraction denominator) / aFraction denominator!
]style[(17 9 5 9 21 9 16 9 12)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 10/13/1998 22:38'!
sumFromInteger: anInteger 
	^ self negative == anInteger negative
		ifTrue: [(self digitAdd: anInteger) normalize]
		ifFalse: [self digitSubtract: anInteger]!
]style[(16 9 22 9 37 9 45 9 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !



