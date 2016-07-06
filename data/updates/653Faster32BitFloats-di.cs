'From Squeak 2.3 of January 14, 1999 on 8 February 1999 at 1:13:56 pm'!"Change Set:		Faster32BitFloatsDate:			8 February 1999Author:			Dan IngallsSpeeds up conversion of Squeak Floats to and from 32-bit words.Uses bitOr: instead of + to avoid LargeInteger arithmetic.Example:Time millisecondsToRun:	[100000 timesRepeat: [Float fromIEEE32Bit: (Float pi asIEEE32BitWord)]]Before:	22939After:	3253"!!Float methodsFor: 'converting' stamp: 'di 2/8/1999 12:51'!asIEEE32BitWord
	"Convert the receiver into a 32 bit Integer value representing the same number in IEEE 32 bit format. Used for conversion in FloatArrays only."
	| word1 word2 sign mantissa exponent destWord |
	self = 0.0 ifTrue:[^0].
	word1 _ self basicAt: 1.
	word2 _ self basicAt: 2.
	mantissa _ (word2 bitShift: -29) + ((word1 bitAnd:  16rFFFFF) bitShift: 3).
	exponent _ ((word1 bitShift: -20) bitAnd: 16r7FF) - 1023 + 127.
	exponent < 0 ifTrue:[^0]. "Underflow"
	exponent > 254 ifTrue:["Overflow"
		exponent _ 255.
		mantissa _ 0].
	sign _ word1 bitAnd: 16r80000000.
	destWord _ (sign bitOr: (exponent bitShift: 23)) bitOr: mantissa.
	^ destWord! !!Float class methodsFor: 'instance creation' stamp: 'di 2/8/1999 12:58'!fromIEEE32Bit: word
	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from a 32bit IEEE floating point representation into an actual Squeak float object (being 64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
	| sign mantissa exponent newFloat |
	word negative ifTrue: [^ self error:'Cannot deal with negative numbers'].
	word = 0 ifTrue:[^ 0.0].
	mantissa _ word bitAnd:  16r7FFFFF.
	exponent _ ((word bitShift: -23) bitAnd: 16rFF) - 127.
	sign _ word bitAnd: 16r80000000.

	exponent = 128 ifTrue:["Either NAN or INF"
		mantissa = 0 ifFalse:[^ Float nan].
		sign = 0 
			ifTrue:[^ Float infinity]
			ifFalse:[^ Float infinity negated]].

	"Create new float"
	newFloat _ self new: 2.
	newFloat basicAt: 1 put: ((sign bitOr: (1023 + exponent bitShift: 20)) bitOr: (mantissa bitShift: -3)).
	newFloat basicAt: 2 put: ((mantissa bitAnd: 7) bitShift: 29).
	^newFloat! !