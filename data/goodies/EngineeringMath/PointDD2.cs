'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:31 pm'!

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:21'!
* aNumeric 
	"Use DD to allow points to be scaled by any other numeric type"
	^ aNumeric productFromPoint: self!
]style[(2 8 3 63 4 8 23)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:21'!
+ aNumeric 
	"Use DD to allow points to be added to any other numeric type"
	^ aNumeric sumFromPoint: self!
]style[(2 8 3 62 4 8 19)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:21'!
- aNumeric 
	"Use DD to allow points to be subtracted less any other numeric type"
	^ aNumeric differenceFromPoint: self!
]style[(2 8 3 69 4 8 26)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:22'!
/ aNumeric 
	"Use DD to allow points to be divided by any other numeric type"
	^ aNumeric quotientFromPoint: self!
]style[(2 8 3 64 4 8 24)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:21'!
// scale 
	"reimplemented for upwards scalability"
	^ (self / scale) floor!
]style[(3 5 3 39 12 5 7)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:21'!
\\ aNumericalThing 
	"implemented just like Number, for upwards scalability"
	^ self - (self // aNumericalThing * aNumericalThing)!
]style[(3 15 3 55 20 15 3 15 1)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:22'!
quo: scale 
	"implemented as such for upwards scalability"
	^ (self / scale) truncated!
]style[(5 5 3 45 12 5 11)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'arithmetic' stamp: 'TAG 10/13/1998 22:22'!
rem: aNumericalThing 
	"implemented just like Number, for upwards scalability"
	^ self - ((self quo: aNumericalThing)
			* aNumericalThing)!
]style[(5 15 3 55 23 15 7 15 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:23'!
roundTo: aNumericalThing 
	"implemented like Number for compatibility"
	^ (self / aNumericalThing) rounded * aNumericalThing!
]style[(9 15 3 43 12 15 12 15)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:23'!
roundUpTo: aNumber 
	"same as Number, but for points"
	^ (self / aNumber) ceiling * aNumber!
]style[(11 7 3 32 12 7 12 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!Point methodsFor: 'truncation and round off' stamp: 'TAG 10/13/1998 22:23'!
truncateTo: aNumericalThing 
	"implemented just like Number"
	^ (self quo: aNumericalThing)
		* aNumericalThing!
]style[(12 15 3 30 15 15 6 15)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !



