'From Squeak 2.3 of January 14, 1999 on 4 February 1999 at 8:28:46 pm'!"Change Set:		FloatArrayTweak-ar
Date:			4 February 1999
Author:			Andreas Raab

Removes duplicate code from classes that behave like FloatArrays (e.g., storing 32bit IEEE floating point numbers)"!!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 2/4/1999 17:04'!floatAt: index
	"For simulation only"
	<primitive: 'primitiveFloatArrayAt'>
	^Float fromIEEE32Bit: (self basicAt: index)! !!BalloonBuffer methodsFor: 'accessing' stamp: 'ar 2/4/1999 17:06'!floatAt: index put: value
	"For simulation only"
	<primitive: 'primitiveFloatArrayAtPut'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !!Float methodsFor: 'converting' stamp: 'ar 1/22/1999 19:58'!asIEEE32BitWord
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
	destWord _ sign + (exponent bitShift: 23) + mantissa.
	^destWord! !!Float class methodsFor: 'instance creation' stamp: 'ar 1/22/1999 19:51'!fromIEEE32Bit: word
	"Convert the given 32 bit word (which is supposed to be a positive 32bit value) from a 32bit IEEE floating point representation into an actual Squeak float object (being 64bit wide). Should only be used for conversion in FloatArrays or likewise objects."
	| sign mantissa exponent newFloat |
	word < 0 ifTrue:[^self error:'Cannot deal with negative numbers'].
	word = 0 ifTrue:[^0.0].
	mantissa _ word bitAnd:  16r7FFFFF.
	exponent _ ((word bitShift: -23) bitAnd: 16rFF) - 127.
	sign _ word bitAnd: 16r80000000.

	exponent = 128 ifTrue:["Either NAN or INF"
		mantissa = 0 ifFalse:[^Float nan].
		sign = 0 
			ifTrue:[^Float infinity]
			ifFalse:[^Float infinity negated]].

	"Create new float"
	newFloat _ self new: 2.
	newFloat basicAt: 1 put: sign + (1023 + exponent bitShift: 20) + (mantissa bitShift: -3).
	newFloat basicAt: 2 put: ((mantissa bitAnd: 7) bitShift: 29).
	^newFloat! !!FloatArray methodsFor: 'accessing' stamp: 'ar 1/22/1999 19:52'!at: index
	<primitive: 'primitiveFloatArrayAt'>
	^Float fromIEEE32Bit: (self basicAt: index)! !!FloatArray methodsFor: 'accessing' stamp: 'ar 2/4/1999 17:05'!at: index put: value
	<primitive: 'primitiveFloatArrayAtPut'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 2/4/1999 17:05'!at: index
	<primitive: 'primitiveFloatArrayAt'>
	^Float fromIEEE32Bit: (self basicAt: index)! !!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'ar 2/4/1999 17:05'!at: index put: value
	<primitive: 'primitiveFloatArrayAtPut'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !BalloonBuffer removeSelector: #basicFloatAt:put:!BalloonBuffer removeSelector: #basicFloatAt:!FloatArray removeSelector: #basicFloatAt:put:!FloatArray removeSelector: #basicFloatAt:!MatrixTransform2x3 removeSelector: #basicFloatAt:put:!MatrixTransform2x3 removeSelector: #basicFloatAt:!