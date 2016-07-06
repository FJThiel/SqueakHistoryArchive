'From Squeak 1.3 of Jan 16, 1998 on 28 January 1998 at 12:15:46 am'!

!Number methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:51'!
zeroDivideError
	^self error: 'Cannot divide by zero'! !


!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:56'!
differenceFromFloat: aFloat
	^self primitiveFailed! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:56'!
differenceFromFraction: aFraction
	^aFraction asFloat - self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:56'!
differenceFromInteger: anInteger
	^anInteger asFloat - self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:06'!
productFromFloat: aFloat
	^self primitiveFailed! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:06'!
productFromFraction: aFraction
	^aFraction asFloat * self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:06'!
productFromInteger: anInteger
	^anInteger asFloat * self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:36'!
quotientFromFloat: aFloat 
	^self = 0.0
		ifTrue: [self zeroDivideError]
		ifFalse: [self primitiveFailed]! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:48'!
quotientFromFraction: aFraction
	^aFraction asFloat / self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/28/98 09:14'!
quotientFromInteger: anInteger
	^anInteger asFloat / self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:07'!
sumFromFloat: aFloat
	^self primitiveFailed! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:07'!
sumFromFraction: aFraction
	^aFraction asFloat + self! !

!Float methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:07'!
sumFromInteger: anInteger
	^anInteger asFloat + self! !


!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:43'!
differenceFromFloat: aFloat
	^aFloat - self asFloat! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:10'!
differenceFromFraction: aFraction
	^aFraction numerator * denominator - (numerator * aFraction denominator) / (aFraction denominator * denominator)! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:10'!
differenceFromInteger: anInteger
	^anInteger * denominator - numerator / denominator! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:49'!
productFromFloat: aFloat
	^aFloat * self asFloat! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:12'!
productFromFraction: aFraction
	^aFraction numerator * numerator / (aFraction denominator * denominator)! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 09:12'!
productFromInteger: anInteger
	^anInteger * numerator / denominator! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:36'!
quotientFromFloat: aFloat
	^aFloat / self asFloat! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:38'!
quotientFromFraction: aFraction
	^aFraction numerator * denominator / (aFraction denominator * numerator)! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:37'!
quotientFromInteger: anInteger 
	^anInteger * denominator / numerator! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:54'!
sumFromFloat: aFloat
	^aFloat + self asFloat! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:44'!
sumFromFraction: aFraction
	^aFraction numerator * denominator + (numerator * aFraction denominator) / (aFraction denominator * denominator)! !

!Fraction methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:54'!
sumFromInteger: anInteger
	^self primitiveFailed! !


!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:43'!
differenceFromFloat: aFloat
	^aFloat - self asFloat! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:52'!
differenceFromFraction: aFraction
	^aFraction numerator - (self * aFraction denominator) / aFraction denominator! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/28/98 08:33'!
differenceFromInteger: anInteger 
	^self negative == anInteger negative
		ifTrue: [self digitSubtract: anInteger]
		ifFalse: [(self digitAdd: anInteger) normalize]! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:49'!
productFromFloat: aFloat
	^aFloat * self asFloat! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:49'!
productFromFraction: aFraction
	^aFraction numerator * self / aFraction denominator! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/28/98 08:34'!
productFromInteger: anInteger 
	^self digitMultiply: anInteger neg: self negative ~~ anInteger negative! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:36'!
quotientFromFloat: aFloat
	^aFloat / self asFloat! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/27/98 23:38'!
quotientFromFraction: aFraction
	^aFraction numerator / (aFraction denominator * self)! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/28/98 09:12'!
quotientFromInteger: anInteger 
	| quoRem |
	^self = 0
		ifTrue: [self zeroDivideError]
		ifFalse: [
quoRem _ self digitDiv: anInteger 
								neg: self negative ~~ anInteger negative.
				(quoRem at: 2) = 0
					ifTrue: [(quoRem at: 1) normalize]
					ifFalse: [(Fraction numerator: anInteger denominator: self) reduced]]! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:54'!
sumFromFloat: aFloat
	^aFloat + self asFloat! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/23/98 07:54'!
sumFromFraction: aFraction
	^aFraction numerator + (self * aFraction numerator) / aFraction denominator! !

!Integer methodsFor: 'arithmetic-DD' stamp: 'TAG 1/28/98 08:32'!
sumFromInteger: anInteger 
	^self negative == anInteger negative
		ifTrue: [(self digitAdd: anInteger) normalize]
		ifFalse: [self digitSubtract: anInteger]! !



