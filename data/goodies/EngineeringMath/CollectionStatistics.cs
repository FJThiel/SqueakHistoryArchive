'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:46 pm'!

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:50'!
log
	^ self collect: [:each | each log]!
]style[(3 27 4 5)f1b,f1,f1cblue;,f1! !

!Collection methodsFor: 'numeric operations' stamp: 'TAG 10/13/1998 22:43'!
raisedToInteger: aPower 
	^ self collect: [:each | each raisedToInteger: aPower]!
]style[(17 6 28 4 18 6 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:49'!
average
	^ self sum / self size! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:49'!
max
	^ self inject: self anyOne into: [:max :each | max max: each]!
]style[(3 49 3 6 4 1)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:49'!
median
	^ self asSortedCollection median! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:49'!
min
	^ self inject: self anyOne into: [:min :each | min min: each]!
]style[(3 49 3 6 4 1)f1b,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
range
	^ self max - self min! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
sampleDeviation
	^ self sampleVariance sqrt! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
sampleVariance
	| ave nMinus1 |
	ave _ self average.
	nMinus1 _ self size - 1.
	^ (self collect: [:each | (each - ave) squared / nMinus1]) sum!
]style[(14 4 12 3 3 18 7 46 4 3 3 12 7 6)f1b,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
standardDeviation
	^ self standardVariance sqrt! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
standardVariance
	| ave n |
	ave _ self average.
	n _ self size.
	^ (self collect: [:each | (each - ave) squared / n]) sum!
]style[(16 4 6 3 3 18 1 42 4 3 3 12 1 6)f1b,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!Collection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:50'!
sum
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects. 
	Having read that, one has to imagine what my next project is :)"
	| sum sample |
	sample _ self anyOne.
	sum _ self inject: sample into: [:accum :each | accum + each].
	^ sum - sample!
]style[(3 2 422 4 11 3 6 17 3 16 6 23 5 3 4 6 3 3 6)f1b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !


!Dictionary methodsFor: 'enumerating' stamp: 'TAG 10/13/1998 22:51'!
keysAndValuesDo: aBlock 
	"Enumerate the keys and associated values of the receiver"
	self associationsDo: [:each | aBlock value: each key value: each value]!
]style[(17 6 3 58 32 6 8 4 12 4 7)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'TAG 10/13/1998 22:54'!
keysAndValuesDo: aBlock 
	"The reciever may be treated as a keyed collection, where the keys are 
	the interval 1 to size"
	1 to: self size do: [:i | aBlock value: i value: (self at: i)]!
]style[(17 6 3 96 28 6 8 1 18 1 2)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'converting' stamp: 'TAG 10/13/1998 22:53'!
readStream
	"return a write stream on the receiver"
	^ ReadStream on: self!
]style[(10 2 39 23)f1b,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'converting' stamp: 'TAG 10/13/1998 22:53'!
writeStream
	"return a write stream on the receiver"
	^ WriteStream on: self!
]style[(11 2 39 24)f1b,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'matrix operations' stamp: 'TAG 10/13/1998 22:53'!
determinant2
	"determine the determinant of the reciever based on the assumption that 
	it is a 2 x 2 matrix"
	^ ((self at: 1)
		at: 1)
		* ((self at: 2)
				at: 2) - (((self at: 2)
			at: 1)
			* ((self at: 1)
					at: 2))!
]style[(12 2 95 114)f1b,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'matrix operations' stamp: 'TAG 10/13/1998 22:53'!
guassianSolution
	"assumes this is a collection of collections where each sub collections size 
	is 1 + my size"
	"destroys the reciever"
	| wid ht masker result |
	wid _ (ht _ self size) + 1.
	1 to: ht do: 
		[:i | 
		self at: i put: ((self at: i)
				ratio: ((self at: i)
						at: i)).
		i + 1 to: ht do: [:n | self at: n put: (self at: i)
					* ((self at: n)
							at: i) negated + (self at: n)]].
	masker _ (1 to: wid)
				collect: [:ignored | 0].
	masker at: wid put: 1;
	 at: ht put: ((self at: ht)
			at: wid) negated.
	self size - 1
		to: 1
		by: -1
		do: [:i | masker at: i put: (masker * (self at: i)) sum negated].
	result _ Array new: ht.
	1 to: ht do: [:i | result at: i put: (masker at: i) negated].
	^ result!
]style[(16 2 119 4 21 3 3 4 2 26 2 26 1 17 1 24 1 12 1 6 1 9 2 20 1 16 1 20 1 13 1 22 1 6 6 10 3 32 6 5 3 15 2 17 2 9 3 55 6 5 1 7 6 13 1 18 6 14 2 9 2 11 6 5 1 7 6 5 1 15 6)f1b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:53'!
fitWithY: aSequenceableCollection power: anInteger 
	"treating the reciever as a collection of x coordinates and the argument 
	as a corresponding set of y values, do a least squares fit generating an 
	array of polynomial coeficients, where coefficient power is i - 1"
	| matSize matrix cacheA cacheB row |
	matSize _ anInteger + 1.
	matrix _ (1 to: matSize)
				collect: [:ignored | Array new: matSize + 1].
	(matrix at: 1)
		at: 1 put: self size.
	cacheA _ (1 to: anInteger)
				collect: [:i | self raisedToInteger: i].
	cacheB _ (cacheA collect: [:each | each sum])
				, ((anInteger + 1 to: anInteger * 2)
						collect: [:i | (self raisedToInteger: i) sum]).
	1 to: matSize do: 
		[:k | 
		row _ matrix at: k.
		1 to: matSize do: [:j | (j = 1 and: [k = 1])
				ifFalse: [row at: j put: (cacheB at: j + k - 2)]]].
	(matrix at: 1)
		at: matSize + 1 put: aSequenceableCollection sum.
	2 to: matSize do: [:i | (matrix at: i)
			at: matSize + 1 put: ((cacheA at: i - 1)
				* aSequenceableCollection) sum].
	^ matrix guassianSolution!
]style[(10 23 8 9 3 216 4 33 3 7 3 9 7 6 10 7 38 7 9 6 33 6 10 9 43 1 4 6 4 6 19 4 15 9 9 9 50 1 16 7 17 3 3 6 5 1 10 7 12 1 11 1 21 3 5 1 7 6 5 1 3 1 12 6 14 7 10 23 13 7 12 6 5 1 9 7 12 6 5 1 12 23 11 6 17)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:53'!
linearFitWithY: aCollection 
	"do a least squares fit of x against y values, return an array holding b 
	and m (where y = mx+ b)"
	| mySum productSum mySquareSum hisSum denom |
	mySum _ self sum.
	productSum _ (aCollection * self) sum.
	mySquareSum _ self squared sum.
	hisSum _ aCollection sum.
	denom _ self size * mySquareSum - mySum squared.
	^ Array with: hisSum * mySquareSum - (productSum * mySum) / denom with: self size * productSum - (hisSum * mySum) / denom!
]style[(16 11 3 99 4 42 3 5 14 10 4 11 15 11 22 6 3 11 7 5 15 11 3 5 25 6 3 11 4 10 3 5 4 5 19 10 4 6 3 5 4 5)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:52'!
medianFilter: anInteger 
	"return a copy of the reciever with a median filter of anInteger * 2 + 1 
	window size having been  
	applied"
	^ self medianFilter: anInteger into: (self species new: self size)!
]style[(14 9 3 110 23 9 36)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:52'!
medianFilter: anInteger into: result 
	"Place into result, the computation of applying a median filter of 
	anInteger * 2 + 1 window size. Narrow the window on the edges to get 
	as much filtering as possible."
	| sz ws |
	sz _ self size.
	sz < (anInteger * 2 + 1) ifTrue: [self error: 'filter too large for reciever'].
	sz = result size ifFalse: [self error: 'result not the same size as receiver'].
	ws _ result writeStream.
	1 to: anInteger do: [:i | ws nextPut: (self medianBetween: 1 and: i * 2 - 1)].
	anInteger + 1 to: sz - anInteger do: [:i | ws nextPut: (self medianBetween: i - anInteger and: i + anInteger)].
	sz - anInteger + 1 to: sz do: [:i | ws nextPut: (self medianBetween: 2 * i - sz and: sz)].
	^ result!
]style[(14 9 7 6 3 171 4 6 3 2 15 2 4 9 66 2 3 6 70 2 3 6 21 9 11 2 38 1 13 9 9 2 3 9 11 2 31 1 3 9 6 1 3 9 5 2 3 9 9 2 11 2 35 1 3 2 6 2 7 6)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:53'!
polynomialValue: anX 
	"Treat the reciever as a set of coefficients for a polynomial function, 
	evaluate the result of using anX as the variable input to that 
	polynomial. Order of the cooeficients is low at front and high at end"
	| sum |
	sum _ (self at: 1)
				+ ((self at: 2)
						* anX).
	3 to: self size do: [:i | sum _ sum + ((self at: i)
						* (anX raisedToInteger: i - 1))].
	^ sum!
]style[(17 3 3 209 4 4 3 3 44 3 30 3 3 3 14 1 11 3 18 1 12 3)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'numeric calculations' stamp: 'TAG 10/13/1998 22:53'!
steps
	"return the step sizes between successive elements"
	| ws rs prev next |
	ws _ WriteStream on: (self species new: self size - 1).
	rs _ ReadStream on: self.
	prev _ rs next.
	[rs atEnd]
		whileFalse: 
			[ws nextPut: (next _ rs next) - prev.
			prev _ next].
	^ ws contents!
]style[(5 2 51 4 16 3 2 55 2 25 4 3 2 9 2 27 2 11 4 3 2 9 4 5 4 3 4 6 2 9)f1b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:52'!
maxWithKey
	| max maxIndex |
	max _ self first.
	maxIndex _ 1.
	self
		keysAndValuesDo: [:index :magnitude | magnitude > max
				ifTrue: 
					[maxIndex _ index.
					max _ magnitude]].
	^ maxIndex -> max!
]style[(10 4 13 3 3 16 8 52 9 3 3 20 8 3 5 7 3 3 9 7 8 4 3)f1b,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:52'!
median
	^ self medianBetween: 1 and: self size! !

!SequenceableCollection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:52'!
medianBetween: aStartIndex and: aStopIndex 
	"compute the median found in the reciever between aStart and aStop 
	index "
	"this was optimized for a particular pattern we saw a lot, that where the 
	value at or near the median  
	index really represented the median. we do this by probing at the 
	center and then counting on either  
	side the number of values that are above and the number of values 
	that are below the current test -  
	when said count drops below the arm length (half of the range width) 
	we have our median. there's  
	also a check employed here to constantly tighten the scope of values 
	we're willing to check"
	"Algorithm employed:  Guess that the median is at the center index.  
	Count the number of values above and below our guess and if these 
	are both less than half the width, we have found the median.  If the 
	count of values above is too high, then our guess was too low and do 
	not making anymore guesses lower (lowLim := guess).  If the count of 
	values below is too high, then our guess was too high and do not 
	making anymore guesses higher (highLim := guess).  Since the median 
	tends to be in the physical middle of the sequence, the center index is 
	tried first, then one less than the center index, then one more than the 
	center index, then two less than the center index, etc."
	| arm above below midIndex checkIndex currentVal highLim lowLim temp |
	arm _ (above _ below _ aStopIndex - aStartIndex + 1) // 2.
	midIndex _ checkIndex _ aStartIndex + arm.
	currentVal _ self at: checkIndex.
	[above > arm or: [below > arm]]
		whileTrue: 
			[currentVal _ self at: checkIndex.
			((nil == lowLim or: [currentVal > lowLim])
				and: [nil == highLim or: [currentVal < highLim]])
				ifTrue: 
					[above _ below _ 0.
					aStartIndex to: checkIndex - 1 do: [:wi | "For smaller indicies, count the number values 
						above and below currentVal"
						(temp _ self at: wi) > currentVal
							ifTrue: [above _ above + 1]
							ifFalse: [temp < currentVal ifTrue: [below _ below + 1]]].
					checkIndex + 1 to: aStopIndex do: [:wi | "For larger indicies, count the number values above 
						and below currentVal"
						(temp _ self at: wi) < currentVal
							ifTrue: [below _ below + 1]
							ifFalse: [temp > currentVal ifTrue: [above _ above + 1]]].
					above > arm
						ifTrue: ["Found too many value above currentVal so 
							median must even higher"
							lowLim _ currentVal]
						ifFalse: [below > arm ifTrue: ["Found too many value below currentVal so 
								median must be even lower"
								highLim _ currentVal]]].
			checkIndex _ checkIndex < midIndex
						ifTrue: ["Alternate around the midIndex, going out from 
							center, to compute next index to check"
							midIndex + (midIndex - checkIndex)]
						ifFalse: [midIndex + (midIndex - checkIndex) - 1]].
	^ currentVal!
]style[(15 11 6 10 3 1288 4 67 3 3 4 5 3 5 3 10 3 11 13 8 3 10 3 11 3 3 3 10 12 10 4 5 3 3 6 5 3 3 21 10 12 10 14 6 6 10 3 6 20 7 6 10 3 7 23 5 3 5 11 11 5 10 16 81 8 4 12 2 4 10 17 5 3 5 23 4 3 10 10 5 3 5 14 10 9 10 12 80 8 4 12 2 4 10 17 5 3 5 23 4 3 10 10 5 3 5 14 5 3 3 16 74 8 6 3 10 18 5 3 3 10 77 9 7 3 10 8 10 3 10 3 8 16 94 8 8 4 8 3 10 19 8 4 8 3 10 12 10)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:52'!
minWithKey
	| min minIndex |
	min _ self first.
	minIndex _ 1.
	self
		keysAndValuesDo: [:index :magnitude | magnitude < min
				ifTrue: 
					[minIndex _ index.
					min _ magnitude]].
	^ minIndex -> min!
]style[(10 4 13 3 3 16 8 52 9 3 3 20 8 3 5 7 3 3 9 7 8 4 3)f1b,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'statistic operations' stamp: 'TAG 10/13/1998 22:52'!
sum
	"See the super implementations complement for an idea of why this is 
	implemented this way, it is slightly different from that implementation 
	for performance reasons"
	| sum |
	sum _ self at: 1.
	2 to: self size do: [:i | sum _ sum + (self at: i)].
	^ sum!
]style[(3 2 169 4 4 3 3 42 3 3 3 13 1 7 3)f1b,f1,f1cblue;,f1,f1cgreen;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'vector operations' stamp: 'TAG 10/13/1998 22:52'!
vectorMagnitude
	^ self squared sum sqrt! !



