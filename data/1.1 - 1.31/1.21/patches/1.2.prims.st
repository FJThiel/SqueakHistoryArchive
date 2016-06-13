Date:	97 Aug 21 10:56:49 am
From:	Dan Ingalls <DanI@wdi.disney.com>
To:		Squeak@create.ucsb.edu
Subject:	Primitives:  You asked for it, you got it!

--============_-1339930736==_============
Content-Type: text/plain; charset="us-ascii"
Content-Transfer-Encoding: quoted-printable

=46olks -

The attached fileIn, once it has been incorporated in the VM, supports=
 primitive indices as high as 2047.  The current system uses indices 1-519,=
 so slots 520-2047 are, in principle, free.  [The actual parameters in this=
 file only provide a table for 699, because the C compiler I'm using barfs=
 on tables bigger than that, but the logic is all there and works at this si=
ze].

The extra two bits of primitive index are stored in the high bits of the=
 method header in order to be backward-compatible, and we'll clean this up=
 some day when we are bored and feel like putting out a new image format. =
 Computationally, though, this wastes no compute time, since the primitive=
 index is stored in the method cache, and thus the bit twiddling involved=
 takes place only on cache misses.

	- Dan

--============_-1339930736==_============
Content-Type: text/plain; name="ExtendedPrimitives-di.cs"; charset="us-ascii"
Content-Disposition: attachment; filename="ExtendedPrimitives-di.cs"

'From Squeak 1.21c of Aug 4, 1997 on 21 August 1997 at 10:53:23 am'!
"Change Set:		ExtPrimitives
Date:			21 August 1997
Author:			Dan Ingalls

This file includes changes to allow up to 2047 primitives in Squeak."

Interpreter addClassVarName: 'MaxPrimitiveIndex'.
!

!CompiledMethod methodsFor: 'accessing' stamp: 'di 8/20/97 15:17'!
primitive
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method.
	We currently allow 11 bits of primitive index, but they are in two places
	for  backward compatibility.  The time to unpack is negligible,
	since the reconstituted full index is stored in the method cache."
	| primBits |
	primBits _ self header bitAnd: 16r300001FF.
	primBits > 16r1FF
		ifTrue: [^ (primBits bitAnd: 16r1FF) + (primBits bitShift: -19)]
		ifFalse: [^ primBits]! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'di 8/21/97 10:36'!
newBytes: numberOfBytes nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	arguments. The remaining parts are not as yet determined."
	| largeBit primBits |
	largeBit _ (nTemps + stackSize) > SmallFrame ifTrue: [1] ifFalse: [0].
	primBits _ primitiveIndex <= 16r1FF
		ifTrue: [primitiveIndex]
		ifFalse: ["For now the high 2 bits of primitive no. are in high bits of header"
				(primitiveIndex bitAnd: 16r1FF) + ((primitiveIndex bitAnd: 16r600) bitShift: 19)].
	^ self newMethod: numberOfBytes + 4 	" +4 to store source code ptr" 
		header: (nArgs bitShift: 24) +
				(nTemps bitShift: 18) +
				(largeBit bitShift: 17) +
				(nLits bitShift: 9) +
				primBits! !


!Encoder methodsFor: 'temps' stamp: 'di 8/21/97 10:47'!
bindArg: name 
	"Declare an argument."
	| node |
	nTemps >= 15
		ifTrue: [^self notify: 'Too many arguments'].
	node _ self bindTemp: name.
	^ node nowHasDef nowHasRef! !


!Interpreter methodsFor: 'compiled methods' stamp: 'di 8/20/97 14:42'!
primitiveIndexOf: methodPointer
	"Note: We now have 11 bits of primitive index, but they are in two places
	for temporary backward compatibility.  The time to unpack is negligible,
	since the reconstituted full index is stored in the method cache."
	| primBits |
	primBits _ ((self headerOf: methodPointer) >> 1) bitAnd: 16r300001FF.
	primBits > 16r1FF
		ifTrue: [^ (primBits bitAnd: 16r1FF) + (primBits >> 19)]
		ifFalse: [^ primBits]! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 15:06'!
primitiveLoadInstVar
	| thisReceiver |
	thisReceiver _ self popStack.
	self push: (self fetchPointer: primitiveIndex-264 ofObject: thisReceiver)! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:56'!
primitivePushFalse
	self popStack.
	self push: falseObj! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:58'!
primitivePushMinusOne
	self popStack.
	self push: ConstMinusOne! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:56'!
primitivePushNil
	self popStack.
	self push: nilObj! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:58'!
primitivePushOne
	self popStack.
	self push: ConstOne! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:55'!
primitivePushSelf
"	no-op, really...
	thisReceiver _ self popStack.
	self push: thisReceiver
"! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:56'!
primitivePushTrue
	self popStack.
	self push: trueObj! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:58'!
primitivePushTwo
	self popStack.
	self push: ConstTwo! !

!Interpreter methodsFor: 'quick primitives' stamp: 'di 8/20/97 14:58'!
primitivePushZero
	self popStack.
	self push: ConstZero! !

!Interpreter methodsFor: 'primitive support' stamp: 'di 8/20/97 15:37'!
primitiveResponse
	primitiveIndex > MaxPrimitiveIndex ifTrue: [^ false].
	successFlag _ true.
	self dispatchOn: primitiveIndex in: PrimitiveTable.
	"check for possible timer interrupts after each primitive"
	(successFlag and:
	 [(nextWakeupTick ~= 0) and:
	 [self ioMSecs >= nextWakeupTick]]) ifTrue: [
		interruptCheckCounter _ 1000.
		self checkForInterrupts].
	^ successFlag! !


!Interpreter class methodsFor: 'initialization' stamp: 'di 8/21/97 10:53'!
initializePrimitiveTable
	"Interpreter initializePrimitiveTable"
	"This table generates a C switch statement."

	"NOTE: The real limit here is 2047, but our C compiler currently barfs over 700"
	MaxPrimitiveIndex _ 699.
	PrimitiveTable _ Array new: MaxPrimitiveIndex+1.
	self table: PrimitiveTable from: 
	#(
		"Integer Primitives (0-19)"
		(0 primitiveFail)
		(1 primitiveAdd)
		(2 primitiveSubtract)
		(3 primitiveLessThan)
		(4 primitiveGreaterThan)
		(5 primitiveLessOrEqual)
		(6 primitiveGreaterOrEqual)
		(7 primitiveEqual)
		(8 primitiveNotEqual)
		(9 primitiveMultiply)
		(10 primitiveDivide)
		(11 primitiveMod)
		(12 primitiveDiv)
		(13 primitiveQuo)
		(14 primitiveBitAnd)
		(15 primitiveBitOr)
		(16 primitiveBitXor)
		(17 primitiveBitShift)
		(18 primitiveMakePoint)
		(19 primitiveFail)

		"LargeInteger Primitives (20-39)"
		"32-bit logic is aliased to Integer prims above"
		(20 39 primitiveFail)

		"Float Primitives (40-59)"
		(40 primitiveAsFloat)
		(41 primitiveFloatAdd)
		(42 primitiveFloatSubtract)
		(43 primitiveFloatLessThan)
		(44 primitiveFloatGreaterThan)
		(45 primitiveFloatLessOrEqual)
		(46 primitiveFloatGreaterOrEqual)
		(47 primitiveFloatEqual)
		(48 primitiveFloatNotEqual)
		(49 primitiveFloatMultiply)
		(50 primitiveFloatDivide)
		(51 primitiveTruncated)
		(52 primitiveFractionalPart)
		(53 primitiveExponent)
		(54 primitiveTimesTwoPower)
		(55 primitiveSquareRoot)
		(56 primitiveSine)
		(57 primitiveArctan)
		(58 primitiveLogN)
		(59 primitiveExp)

		"Subscript and Stream Primitives (60-67)"
		(60 primitiveAt)
		(61 primitiveAtPut)
		(62 primitiveSize)
		(63 primitiveStringAt)
		(64 primitiveStringAtPut)
		(65 primitiveNext)
		(66 primitiveNextPut)
		(67 primitiveAtEnd)

		"StorageManagement Primitives (68-79)"
		(68 primitiveObjectAt)
		(69 primitiveObjectAtPut)
		(70 primitiveNew)
		(71 primitiveNewWithArg)
		(72 primitiveFail)					"Blue Book: primitiveBecome"
		(73 primitiveInstVarAt)
		(74 primitiveInstVarAtPut)
		(75 primitiveAsOop)
		(76 primitiveFail)					"Blue Book: primitiveAsObject"
		(77 primitiveSomeInstance)
		(78 primitiveNextInstance)
		(79 primitiveNewMethod)

		"Control Primitives (80-89)"
		(80 primitiveFail)   					"Blue Book:  primitiveBlockCopy"
		(81 primitiveValue)
		(82 primitiveValueWithArgs)
		(83 primitivePerform)
		(84 primitivePerformWithArgs)
		(85 primitiveSignal)
		(86 primitiveWait)
		(87 primitiveResume)
		(88 primitiveSuspend)
		(89 primitiveFlushCache)

		"Input/Output Primitives (90-109)"
		(90 primitiveMousePoint)
		(91 primitiveFail)					"Blue Book: primitiveCursorLocPut"
		(92 primitiveFail)					"Blue Book: primitiveCursorLink"
		(93 primitiveInputSemaphore)
		(94 primitiveFail)					"Blue Book: primitiveSampleInterval"
		(95 primitiveInputWord)
		(96 primitiveCopyBits)
		(97 primitiveSnapshot)
		(98 primitiveFail)					"Blue Book: primitiveTimeWordsInto"
		(99 primitiveFail)					"Blue Book: primitiveTickWordsInto"
		(100 primitiveFail)					"Blue Book: primitiveSignalAtTick"
		(101 primitiveBeCursor)
		(102 primitiveBeDisplay)
		(103 primitiveScanCharacters)
		(104 primitiveDrawLoop)
		(105 primitiveStringReplace)
		(106 primitiveScreenSize)
		(107 primitiveMouseButtons)
		(108 primitiveKbdNext)
		(109 primitiveKbdPeek)

		"System Primitives (110-119)"
		(110 primitiveEquivalent)
		(111 primitiveClass)
		(112 primitiveBytesLeft)
		(113 primitiveQuit)
		(114 primitiveExitToDebugger)
		(115 primitiveFail)					"Blue Book: primitiveOopsLeft"
		(116 primitiveFail)
		(117 primitiveFail)
		(118 primitiveFail)
		(119 primitiveFail)

		"Miscellaneous Primitives (120-127)"
		(120 primitiveFail)
		(121 primitiveImageName)
		(122 primitiveNoop)				"Blue Book: primitiveImageVolume"
		(123 primitiveFail)
		(124 primitiveLowSpaceSemaphore)
		(125 primitiveSignalAtBytesLeft)
		(126 primitiveFail)
		(127 primitiveFail)

		"Squeak Primitives Start Here"

		"Squeak Miscellaneous Primitives (128-149)"
		(128 primitiveArrayBecome)
		(129 primitiveSpecialObjectsOop)
		(130 primitiveFullGC)
		(131 primitiveIncrementalGC)
		(132 primitiveObjectPointsTo)
		(133 primitiveSetInterruptKey)
		(134 primitiveInterruptSemaphore)
		(135 primitiveMillisecondClock)
		(136 primitiveSignalAtMilliseconds)
		(137 primitiveSecondsClock)
		(138 primitiveSomeObject)
		(139 primitiveNextObject)
		(140 primitiveBeep)
		(141 primitiveClipboardText)
		(142 primitiveVMPath)
		(143 primitiveShortAt)
		(144 primitiveShortAtPut)
		(145 primitiveConstantFill)
		(146 primitiveReadJoystick)
		(147 primitiveWarpBits)
		(148 primitiveClone)
		(149 primitiveGetAttribute)

		"File Primitives (150-169)"
		(150 primitiveFileAtEnd)
		(151 primitiveFileClose)
		(152 primitiveFileGetPosition)
		(153 primitiveFileOpen)
		(154 primitiveFileRead)
		(155 primitiveFileSetPosition)
		(156 primitiveFileDelete)
		(157 primitiveFileSize)
		(158 primitiveFileWrite)
		(159 primitiveFileRename)
		(160 primitiveDirectoryCreate)
		(161 primitiveDirectoryDelimitor)
		(162 primitiveDirectoryLookup)
		(163 168 primitiveFail)
		(169 primitiveDirectorySetMacTypeAndCreator)

		"Sound Primitives (170-199)"
		(170 primitiveSoundStart)
		(171 primitiveFail)
		(172 primitiveSoundStop)
		(173 primitiveSoundAvailableSpace)
		(174 primitiveSoundPlaySamples)
		(175 primitiveSoundPlaySilence)
		(176 primWaveTableSoundmixSampleCountintostartingAtpan)
		(177 primFMSoundmixSampleCountintostartingAtpan)
		(178 primPluckedSoundmixSampleCountintostartingAtpan)
		(179 primSampledSoundmixSampleCountintostartingAtpan)
		(180 199 primitiveFail)

		"Networking Primitives (200-229)"
		(200 primitiveInitializeNetwork)
		(201 primitiveResolverStartNameLookup)
		(202 primitiveResolverNameLookupResult)
		(203 primitiveResolverStartAddressLookup)
		(204 primitiveResolverAddressLookupResult)
		(205 primitiveResolverAbortLookup)
		(206 primitiveResolverLocalAddress)
		(207 primitiveResolverStatus)
		(208 primitiveResolverError)
		(209 primitiveSocketCreate)
		(210 primitiveSocketDestroy)
		(211 primitiveSocketConnectionStatus)
		(212 primitiveSocketError)
		(213 primitiveSocketLocalAddress)
		(214 primitiveSocketLocalPort)
		(215 primitiveSocketRemoteAddress)
		(216 primitiveSocketRemotePort)
		(217 primitiveSocketConnectToPort)
		(218 primitiveSocketListenOnPort)
		(219 primitiveSocketCloseConnection)
		(220 primitiveSocketAbortConnection)
		(221 primitiveSocketReceiveDataBufCount)
		(222 primitiveSocketReceiveDataAvailable)
		(223 primitiveSocketSendDataBufCount)
		(224 primitiveSocketSendDone)
		(225 229 primitiveFail)

		"Unimplemented Primitives (230-249)"
		(230 249 primitiveFail)

		"VM Implementor Primitives (250-255)"
		(250 clearProfile)
		(251 dumpProfile)
		(252 startProfiling)
		(253 stopProfiling)
		(254 primitiveFail)
		(255 primitiveFail)

		"Quick Push Const Methods"
		(256 primitivePushSelf)
		(257 primitivePushTrue)
		(258 primitivePushFalse)
		(259 primitivePushNil)
		(260 primitivePushMinusOne)
		(261 primitivePushZero)
		(262 primitivePushOne)
		(263 primitivePushTwo)

		"Quick Push Const Methods"
		(264 519 primitiveLoadInstVar)

		"Unassigned Primitives"
		(520 primitiveBeep) "test of new primitive indices"
		(521 699 primitiveFail)
	).! !



--============_-1339930736==_============--


