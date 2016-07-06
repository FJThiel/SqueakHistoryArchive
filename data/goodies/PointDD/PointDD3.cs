'From Squeak 1.3 of Jan 16, 1998 on 30 January 1998 at 12:56:18 am'!

!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
rem: aNumericalThing
	"implemented just like Number, for upwards scalability"

	^self - ((self quo: aNumericalThing) * aNumericalThing)!
]style[(11 89 6 13 6 10)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 1/29/98 0-24:'!
\\ aNumericalThing
	"implemented just like Number, for upwards scalability"

	^self - (self // aNumericalThing * aNumericalThing)!
]style[(18 77 15 3 15 1)f1b,f1,f1b,f1,f1b,f1! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
roundTo: aNumericalThing 
	"implemented like Number for compatibility"

	^(self / aNumericalThing) rounded * aNumericalThing!
]style[(24 85 15)f1b,f1,f1b! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/30/98 0-24:'!
roundUpTo: aNumber 
	"same as Number, but for points"

	^(self/aNumber) ceiling * aNumber! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 1/29/98 0-24:'!
truncateTo: aNumericalThing 
	"implemented just like Number"

	^(self quo: aNumericalThing) * aNumericalThing!
]style[(27 48 15 4 15)f1b,f1,f1b,f1,f1b! !



