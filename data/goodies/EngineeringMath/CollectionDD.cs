'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:36 pm'!

!Collection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:44'!
anyOne
	"return a representative sample of an element, can be very helpful 
	when needing to preinfer the nature of the contents of 
	semi-homogeneous collections"
	self do: [:each | ^ each].
	^ self errorEmptyCollection!
]style[(6 2 155 22 4 31)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
* aNumeric 
	^ aNumeric productFromCollection: self!
]style[(2 8 5 8 28)f1,f1cgreen;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
+ aNumeric 
	^ aNumeric sumFromCollection: self!
]style[(2 8 5 8 24)f1,f1cgreen;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
- aNumeric 
	^ aNumeric differenceFromCollection: self!
]style[(2 8 5 8 31)f1,f1cgreen;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
/ aNumeric 
	^ aNumeric quotientFromCollection: self!
]style[(2 8 5 8 29)f1,f1cgreen;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
// aNumeric 
	"use the generic implementation so that we can mix with any Numeric"
	^ (self / aNumeric) floor!
]style[(3 8 3 68 12 8 7)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
\\ aNumeric 
	"Use the generic implementation so that we can mix with any numeric"
	^ self - (self // aNumeric * aNumeric)!
]style[(3 8 3 68 20 8 3 8 1)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
abs
	"Absolute value of all elements in the collection"
	^ self collect: [:a | a abs]!
]style[(3 2 50 24 1 5)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
ceiling
	^ self collect: [:a | a ceiling]!
]style[(7 24 1 9)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
decrement: aDelta 
	^ self collect: [:each | each - aDelta]!
]style[(11 6 28 4 3 6 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
decrementFrom: aDelta 
	^ self collect: [:each | aDelta - each]!
]style[(15 6 28 6 3 4 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
floor
	^ self collect: [:a | a floor]!
]style[(5 24 1 7)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
increment: aDelta 
	^ self collect: [:each | each + aDelta]!
]style[(11 6 28 4 3 6 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:50'!
log
	^ self collect: [:each | each log]!
]style[(3 27 4 5)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:42'!
negated
	"Negated value of all elements in the collection"
	^ self collect: [:a | a negated]!
]style[(7 2 49 24 1 9)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:43'!
quo: aNumeric 
	"User the generic implementation"
	^ (self / aNumeric) truncated!
]style[(5 8 3 33 12 8 11)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:43'!
raisedToInteger: aPower 
	^ self collect: [:each | each raisedToInteger: aPower]!
]style[(17 6 28 4 18 6 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:43'!
ratio: aScalar 
	^ self collect: [:each | each / aScalar]!
]style[(7 7 28 4 3 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:43'!
ratioFrom: aScalar 
	^ self collect: [:each | aScalar / each]!
]style[(11 7 28 7 3 4 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
reciprocal
	"Return the reciever full of reciprocated elements"
	^ self collect: [:a | a reciprocal]!
]style[(10 2 51 24 1 12)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
rem: aNumeric 
	"Use the generic implementation so we can mix with any numeric"
	^ self - ((self quo: aNumeric)
			* aNumeric)!
]style[(5 8 3 63 23 8 7 8 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
rounded
	^ self collect: [:a | a rounded]!
]style[(7 24 1 9)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
scale: aScalar 
	^ self collect: [:each | each * aScalar]!
]style[(7 7 28 4 3 7 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
sqrt
	^ self collect: [:each | each sqrt]!
]style[(4 27 4 6)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
squared
	^ self collect: [:each | each * each]!
]style[(7 27 4 3 4 1)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:44'!
truncated
	^ self collect: [:a | a truncated]!
]style[(9 24 1 11)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromCollection: aCollection 
	^ self error: 'Both collections must be sequenceable to subtract'!
]style[(26 11 68)f1b,f1cgreen;b,f1! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromFloat: aFloat 
	^ self decrementFrom: aFloat!
]style[(21 6 25 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromFraction: aFraction 
	^ self decrementFrom: aFraction!
]style[(24 9 25 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromInteger: anInteger 
	^ self decrementFrom: anInteger!
]style[(23 9 25 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromPoint: aPoint 
	^ self decrementFrom: aPoint!
]style[(21 6 25 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
differenceFromSequenceableCollection: aSequenceableCollection 
	^ self differenceFromCollection: aSequenceableCollection!
]style[(38 23 36 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
productFromCollection: aCollection 
	^ self error: 'Both collections must be sequenceable to multiply'!
]style[(23 11 68)f1b,f1cgreen;b,f1! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
productFromFloat: aFloat 
	^ self scale: aFloat!
]style[(18 6 17 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
productFromFraction: aFraction 
	^ self scale: aFraction!
]style[(21 9 17 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
productFromInteger: anInteger 
	^ self scale: anInteger!
]style[(20 9 17 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:40'!
productFromPoint: aPoint 
	^ self scale: aPoint!
]style[(18 6 17 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
productFromSequenceableCollection: aSequenceableCollection 
	^ self productFromCollection: aSequenceableCollection!
]style[(35 23 33 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromCollection: aCollection 
	^ self error: 'Both collections must be sequenceable to divide'!
]style[(24 11 66)f1b,f1cgreen;b,f1! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromFloat: aFloat 
	^ self ratioFrom: aFloat!
]style[(19 6 21 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromFraction: aFraction 
	^ self ratioFrom: aFraction!
]style[(22 9 21 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromInteger: anInteger 
	^ self ratioFrom: anInteger!
]style[(21 9 21 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromPoint: aPoint 
	^ self ratioFrom: aPoint!
]style[(19 6 21 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
quotientFromSequenceableCollection: aSequenceableCollection 
	^ self quotientFromCollection: aSequenceableCollection!
]style[(36 23 34 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromCollection: aCollection 
	^ self error: 'Both collections must be sequenceable to add'!
]style[(19 11 63)f1b,f1cgreen;b,f1! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromFloat: aFloat 
	^ self increment: aFloat!
]style[(14 6 21 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromFraction: aFraction 
	^ self increment: aFraction!
]style[(17 9 21 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromInteger: anInteger 
	^ self increment: anInteger!
]style[(16 9 21 9)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromPoint: aPoint 
	^ self increment: aPoint!
]style[(14 6 21 6)f1b,f1cgreen;b,f1,f1cblue;! !

!Collection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:41'!
sumFromSequenceableCollection: aSequenceableCollection 
	^ self sumFromCollection: aSequenceableCollection!
]style[(31 23 29 23)f1b,f1cgreen;b,f1,f1cblue;! !


!Bag methodsFor: 'adding' stamp: 'TAG 10/13/1998 22:39'!
add: newObject withOccurrences: anInteger 
	"Add the element newObject to the receiver. Do so as though the element  
	were added anInteger number of times. Answer newObject."
	contents at: newObject put: (contents at: newObject ifAbsent: [0])
			+ anInteger.
	^ newObject!
]style[(5 9 18 9 3 131 15 9 20 9 21 9 5 9)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !


!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
differenceFromCollection: aCollection 
	^ aCollection decrement: self!
]style[(26 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
differenceFromSequenceableCollection: aSequenceableCollection 
	^ self differenceFromCollection: aSequenceableCollection!
]style[(38 23 36 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
productFromCollection: aCollection 
	^ aCollection scale: self!
]style[(23 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
productFromSequenceableCollection: aSequenceableCollection 
	^ self productFromCollection: aSequenceableCollection!
]style[(35 23 33 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
quotientFromCollection: aCollection 
	^ aCollection ratio: self!
]style[(24 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
quotientFromSequenceableCollection: aSequenceableCollection 
	^ self quotientFromCollection: aSequenceableCollection!
]style[(36 23 34 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
sumFromCollection: aCollection 
	^ aCollection increment: self!
]style[(19 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Number methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:46'!
sumFromSequenceableCollection: aSequenceableCollection 
	^ self sumFromCollection: aSequenceableCollection!
]style[(31 23 29 23)f1b,f1cgreen;b,f1,f1cblue;! !


!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:47'!
differenceFromCollection: aCollection 
	^ aCollection decrement: self!
]style[(26 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:47'!
differenceFromSequenceableCollection: aSequenceableCollection 
	^ self differenceFromCollection: aSequenceableCollection!
]style[(38 23 36 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:47'!
productFromCollection: aCollection 
	^ aCollection scale: self!
]style[(23 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:48'!
productFromSequenceableCollection: aSequenceableCollection 
	^ self productFromCollection: aSequenceableCollection!
]style[(35 23 33 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:48'!
quotientFromCollection: aCollection 
	^ aCollection ratio: self!
]style[(24 11 5 11 12)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:48'!
quotientFromSequenceableCollection: aSequenceableCollection 
	^ self quotientFromCollection: aSequenceableCollection!
]style[(36 23 34 23)f1b,f1cgreen;b,f1,f1cblue;! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:48'!
sumFromCollection: aCollection 
	^ aCollection increment: self!
]style[(19 11 5 11 16)f1b,f1cgreen;b,f1,f1cblue;,f1! !

!Point methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:48'!
sumFromSequenceableCollection: aSequenceableCollection 
	^ self sumFromCollection: aSequenceableCollection!
]style[(31 23 29 23)f1b,f1cgreen;b,f1,f1cblue;! !


!SequenceableCollection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:49'!
differenceFromSequenceableCollection: aSequenceableCollection 
	^ aSequenceableCollection with: self collect: [:a :b | a - b]!
]style[(38 23 5 23 30 1 3 1 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:49'!
productFromSequenceableCollection: aSequenceableCollection 
	^ aSequenceableCollection with: self collect: [:a :b | a * b]!
]style[(35 23 5 23 30 1 3 1 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:49'!
quotientFromSequenceableCollection: aSequenceableCollection 
	^ aSequenceableCollection with: self collect: [:a :b | a / b]!
]style[(36 23 5 23 30 1 3 1 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'double dispatching' stamp: 'TAG 10/13/1998 22:49'!
sumFromSequenceableCollection: aSequenceableCollection 
	^ aSequenceableCollection with: self collect: [:a :b | a + b]!
]style[(31 23 5 23 30 1 3 1 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:48'!
* aNumeric 
	^ aNumeric productFromSequenceableCollection: self!
]style[(2 8 5 8 40)f1,f1cgreen;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:48'!
+ aNumeric 
	^ aNumeric sumFromSequenceableCollection: self!
]style[(2 8 5 8 36)f1,f1cgreen;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:48'!
- aNumeric 
	^ aNumeric differenceFromSequenceableCollection: self!
]style[(2 8 5 8 43)f1,f1cgreen;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:48'!
/ aNumeric 
	^ aNumeric quotientFromSequenceableCollection: self!
]style[(2 8 5 8 41)f1,f1cgreen;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:49'!
@ aCollection 
	^ self with: aCollection collect: [:a :b | a @ b]!
]style[(2 11 16 11 19 1 3 1 1)f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !


!Interval methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:45'!
decrement: aDelta 
	"reimplemented here, because this can be done imperitavely (rather 
	than iteratively) to an Interval,  
	returning another interval"
	^ start - aDelta to: stop - aDelta by: step!
]style[(11 6 3 133 12 6 12 6 9)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Interval methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:45'!
increment: aDelta 
	"reimplemented here, because this can be done imperitavely (rather 
	than iteratively) to an Interval,  
	returning another interval"
	^ start + aDelta to: stop + aDelta by: step!
]style[(11 6 3 133 12 6 12 6 9)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'TAG 10/13/1998 22:47'!
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	result _ self species new: self size.
	1 to: self size do: [:index | result addLast: (twoArgBlock value: (self at: index)
				value: (otherCollection at: index))].
	^ result!
]style[(6 15 10 11 3 129 4 7 3 6 63 6 11 11 18 5 14 15 5 5 8 6)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !



