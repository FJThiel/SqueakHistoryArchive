'From Squeak3.7alpha of 11 September 2003 [latest update: #5763] on 8 March 2004 at 4:23:52 pm'!"Change Set:		devideIsZeroDate:			7 March 2004Author:			Marcus DenkerJust a small fix from the Units package touse isZero in Float>>/ and SmallInteger>>/"!!Float methodsFor: 'arithmetic' stamp: 'hh 10/3/2000 11:46'!/ aNumber 	"Primitive. Answer the result of dividing receiver by aNumber.	Fail if the argument is not a Float. Essential. See Object documentation	whatIsAPrimitive."	<primitive: 50>	aNumber isZero ifTrue: [^(ZeroDivide dividend: self) signal].	^ aNumber adaptToFloat: self andSend: #/! !!SmallInteger methodsFor: 'arithmetic' stamp: 'hh 10/3/2000 11:47'!/ aNumber 	"Primitive. This primitive (for /) divides the receiver by the argument	and returns the result if the division is exact. Fail if the result is not a	whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional.	No Lookup. See Object documentation whatIsAPrimitive."	<primitive: 10>	aNumber isZero ifTrue: [^(ZeroDivide dividend: self) signal].	(aNumber isMemberOf: SmallInteger)		ifTrue: [^(Fraction numerator: self denominator: aNumber) reduced]		ifFalse: [^super / aNumber]! !