"StreamIO.st
Code to support external I/O in floats, longs, 96-bit doubles, etc. Some of this is from the Kyma system thanks to Kurt Hebel.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    April 26, 1995"!


!PositionableStream methodsFor: 'nonhomogeneous accessing'!

nextFloat
	"Answer a c-format float value from the receiver, 
	or false in case of an error."
	
	| value float |
	float := 0.0.
	value 	:= self next. value == nil ifTrue: [^false].
	float basicAt: 1 put: value.
	value 	:= self next. value == nil ifTrue: [^false].
	float basicAt: 2 put: value.
	value 	:= self next. value == nil ifTrue: [^false].
	float basicAt: 3 put: value.
	value 	:= self next. value == nil ifTrue: [^false].
	float basicAt: 4 put: value.
	^float!

nextFloatPut: aFloat
	"Write a c-format float value to the receiver."
	
	| bytes |
	self nextPut: (aFloat basicAt: 1).
	self nextPut: (aFloat basicAt: 2).
	self nextPut: (aFloat basicAt: 3).
	self nextPut: (aFloat basicAt: 4)!

nextLongPut: a32BitW
	"Write out a 32-bit integer as 32 bits."

	self nextPut: ((a32BitW bitShift: -24) bitAnd: 16rFF).
	self nextPut: ((a32BitW bitAnd: 16rFF0000) bitShift: -16).
	self nextPut: ((a32BitW bitAnd: 16rFF00) bitShift: -8).
	self nextPut: (a32BitW bitAnd: 16rFF).
	^a32BitW!

nextWordPut: aWord 
	"Append to the receiver an Integer as the next two bytes."

	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	self nextPut: (aWord bitAnd: 255).
	^aWord!


next24Bit
	"Read a 24-bit quantity from the input stream."

	| m1 m2 m3 |
	m1 := self next. m1 == nil ifTrue: [^false].
	m2 := self next. m2 == nil ifTrue: [^false].
	m3 := self next. m3 == nil ifTrue: [^false].
	^m1 asInteger << 8 + m2 asInteger << 8 + m3 asInteger!

next24BitLE
	"Read a 24-bit quantity from the input stream."

	| m1 m2 m3 |
	m3 := self next. m3 == nil ifTrue: [^false].
	m2 := self next. m2 == nil ifTrue: [^false].
	m1 := self next. m1 == nil ifTrue: [^false].
	^m1 asInteger << 8 + m2 asInteger << 8 + m3 asInteger!

next24BitPut: a24BitW
	"Write out a 24-bit integer as 24 bits."

	| value |
	value := a24BitW bitAnd: 16r00FFFFFF.
	self nextPut: (value bitShift: -16).
	self nextPut: ((value bitAnd: 16rFF00) bitShift: -8).
	self nextPut: (value bitAnd: 16rFF).
	^a24BitW!

next32Bit
	"Read a 24-bit quantity from the input stream."

	| m1 m2 m3 m4 |
	m1 := self next. m1 == nil ifTrue: [^false].
	m2 := self next. m2 == nil ifTrue: [^false].
	m3 := self next. m3 == nil ifTrue: [^false].
	m4 := self next. m4 == nil ifTrue: [^false].
	^m1 asInteger << 8 + m2 asInteger << 8 + m3 asInteger << 8 + m4 asInteger!

next80BitFloat
	"Read a 80-bit extended float quantity from the input stream."

	| exp exponent sign mantissa1 mantissa2 |
	exp := self next * 256 + self next.
	exponent := (exp bitAnd: 16r7FFF) - 16383.
	sign := (exp bitAnd: 16r8000) == 0 ifTrue: [1.0] ifFalse: [-1.0].
	mantissa1 := self next * 256 + self next * 256 + self next.
	mantissa2 := self next.
	self skip: 4.
	^(mantissa1 == 0 and: [mantissa2 == 0])
		ifTrue: [0]
		ifFalse: [((mantissa2 >= 16r80
			ifTrue: [mantissa1 asFloat + 0.5]
			ifFalse: [mantissa1 asFloat]) timesTwoPower: exponent - 23) * sign]!

next96BitDouble
	"Read a 96-bit extended float quantity from the input stream."

	| exp exponent sign mantissa1 mantissa2 mantissa3 |
self error: 'fix me'.
	exp := self next * 256 + self next.
	exponent := (exp bitAnd: 16r7FFF) - 16383.
	sign := (exp bitAnd: 16r8000) == 0 ifTrue: [1.0] ifFalse: [-1.0].
	self skip: 2.
	mantissa1 := self next * 256 + self next * 256 + self next.
	mantissa2 := self next * 256 + self next * 256 + self next.
	mantissa3 := self next * 256 + self next.
	^(mantissa1 == 0 and: [mantissa2 == 0 and: [mantissa3 == 0]])
		ifTrue: [0]
		ifFalse: [((mantissa2 >= 16r800000
			ifTrue: [mantissa1 asFloat + 0.5]
			ifFalse: [mantissa1 asFloat]) timesTwoPower: exponent - 23) * sign]!

next96BitFloat
	"Read a 96-bit extended float quantity from the input stream."
	| exp exponent sign mantissa1 mantissa2 |

	exp := self next * 256 + self next.
	exponent := (exp bitAnd: 16r7FFF) - 16383.
	sign := (exp bitAnd: 16r8000) == 0 ifTrue: [1.0] ifFalse: [-1.0].
	self skip: 2.
	mantissa1 := self next * 256 + self next * 256 + self next.
	mantissa2 := self next.
	self skip: 4.
	^(mantissa1 == 0 and: [mantissa2 == 0])
		ifTrue: [0]
		ifFalse: [((mantissa2 >= 16r80
			ifTrue: [mantissa1 asFloat + 0.5]
			ifFalse: [mantissa1 asFloat]) timesTwoPower: exponent - 23) * sign]!

nextCString
	"Answer a new String read from the receiver. The string is 0-terminated 
 in the C style." 

	| s c |
	s := WriteStream on: (String new: 20).
	[(c := self next) == 0]
		whileFalse: [s nextPut: c asCharacter].
	^s contents!

nextCStringPut: aString 
	"Append the string, aString, to the receiver.
	The characters of the string are written, followed by a zero." 

	aString do: [:char | self nextPut: char asInteger].
	self nextPut: 0.
	^aString!

nextLongLE
	"Read a 32-bit quantity from the input stream, least significant byte first." 

	| m1 m2 m3 m4 |
	m4 := self next.	m4 == nil ifTrue: [^false].
	m3 := self next.	m3 == nil ifTrue: [^false].
	m2 := self next.	m2 == nil ifTrue: [^false].
	m1 := self next.	m1 == nil ifTrue: [^false].
	^(((m1 asInteger bitShift: 24) + (m2 asInteger bitShift: 16)) 
		+ (m3 asInteger bitShift: 8)) + m4 asInteger!

nextLongLEPut: a32BitW
	"Write out a 32-bit integer as 32 bits, least significant byte first."

	self nextPut: (a32BitW bitAnd: 16rFF).
	self nextPut: ((a32BitW bitAnd: 16rFF00) bitShift: -8).
	self nextPut: ((a32BitW bitAnd: 16rFF0000) bitShift: -16).
	self nextPut: ((a32BitW bitShift: -24) bitAnd: 16rFF).
	^a32BitW!

nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger." 

	| s |
	n <= 2
		ifTrue: 
			[s := 0.
			n timesRepeat: [s := s * 256 + self next].
			^s].
	s := Integer new: n neg: false.
	1 to: n do: [:i | s digitAt: n + 1 - i put: self next].
	"reverse order of significance"
	^s compressed!

nextNumber: n put: v 
	"Append to the receiver the argument, v, which is a positive SmallInteger or a 
	LargePositiveInteger, as the next n bytes. Possibly pad with leading zeros." 

	| vlen |
	vlen := v digitLength.
	n < vlen
		ifTrue: [self error: 'number too big']
		ifFalse: [n > vlen ifTrue: [self next: n - vlen put: 0]].
	"pad beginning with 0's"
	vlen to: 1 by: -1 do:
		[:i | self nextPut: (v digitAt: i)]!

nextSigned16Bit

	| unsigned |
	unsigned := self nextWord.
	unsigned onlyIfFalse: [^false].
	unsigned > 16r7FFF
		ifTrue: [^unsigned - 16r10000]
		ifFalse: [^unsigned]!

nextSigned16BitLE

	| unsigned |
	unsigned := self nextWordLE.
	unsigned onlyIfFalse: [^false].
	unsigned > 16r7FFF
		ifTrue: [^unsigned - 16r10000]
		ifFalse: [^unsigned]!

nextSigned24Bit

	| unsigned |
	unsigned := self next24Bit.
	unsigned onlyIfFalse: [^false].
	unsigned > 16r7FFFFF
		ifTrue: [^unsigned - 16r1000000]
		ifFalse: [^unsigned]!

nextSigned24BitLE

	| unsigned |
	unsigned := self next24BitLE.
	unsigned onlyIfFalse: [^false].
	unsigned > 16r7FFFFF
		ifTrue: [^unsigned - 16r1000000]
		ifFalse: [^unsigned]!

nextSigned8Bit

	| unsigned |
	unsigned := self next.
	unsigned onlyIfFalse: [^false].
	unsigned > 16r7F
		ifTrue: [^unsigned - 16r100]
		ifFalse: [^unsigned]!

nextSignedInteger
	"Answer the next two bytes from the receiver as a signed Integer."

	| returnValue |
	self atEnd ifTrue: [^false].
	returnValue := self next asInteger.
	self atEnd ifTrue: [^false].
	(returnValue := returnValue * 256 + self next asInteger) highBit = 16
		ifTrue:	[^returnValue - 65536	"16-bit negative 0"]
		ifFalse:	[^returnValue]!

nextString
	"Answer a new String read from the receiver. The first byte is the 
	length of the string, unless it is greater than 192, in which case the
	first two bytes encode the length. "

	| aString length |
	length := self next.	"first byte."
	length >= 192
		ifTrue: [length := length - 192 * 256 + self next].
	aString := String new: length.
	1 to: length do: [:i | aString at: i put: self next asCharacter].
	^aString!

nextStringPut: aString 
	"Append the string, aString, to the receiver."

	| length |
	(length := aString size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length // 256 + 192).
			self nextPut: (length \\ 256)].
	aString do: [:char | self nextPut: char asInteger].
	^aString!

nextWordLE
	"Answer the next two bytes from the receiver as an Integer, least
significant byte first."	

	| high low |
	low := self next.
	low == nil ifTrue: [^false].
	high := self next.
	high == nil ifTrue: [^false].
	^(high asInteger bitShift: 8) + low asInteger!

nextWordLEPut: aWord 
	"Append to the receiver an Integer as the next two bytes, 
	least significant byte first." 

	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord! !


!ByteArray methodsFor: 'backward compatibility'!

floatAt: i put: value 
	"Set the value of the double word (4 bytes) starting at byte index i."

	| w |
	w := value.
	self at: i put: (w at: 4).
	self at: i + 1 put: (w at: 3).
	self at: i + 2 put: (w at: 2).
	self at: i + 3 put: (w at: 1)!

longAt: i 
	"Answer the value of the double word (4 bytes) starting at byte index i."

	| b0 b1 b2 w |
	b0 := self at: i.  
	b1 := self at: i+1.  
	b2 := self at: i+2.  
	w := self at: i+3.
	"Following sequence minimizes LargeInteger arithmetic for small results."
	b2=0 ifFalse: [w := (b2 bitShift: 8) + w].
	b1=0 ifFalse: [w := (b1 bitShift: 16) + w].
	b0=0 ifFalse: [w := (b0 bitShift: 24) + w].
	^w!

longAt: i put: value 
	"Set the value of the double word (4 bytes) starting at byte index i."

	| w |
	w := value truncated.
	self at: i put: (w digitAt: 4).
	self at: i + 1 put: (w digitAt: 3).
	self at: i + 2 put: (w digitAt: 2).
	self at: i + 3 put: (w digitAt: 1)!

shortAt: i 
	"Answer the value of the word (2 bytes) starting at index i."

	| j |
	j := i + i.
	^((self at: j - 1) bitShift: 8) + (self at: j)!

shortAt: i put: v 
	"Set the value of the word (2 bytes) starting at index i."

	| j |
	j := i + i.
	self at: j - 1 put: ((v bitShift: -8) bitAnd: 8r377).
	self at: j put: (v bitAnd: 8r377)! !

