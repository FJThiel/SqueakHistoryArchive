'From Squeak 2.2 of Sept 23, 1998 on 16 November 1998 at 11:31:31 pm'!"Change Set:		StringSearch-diDate:			15 November 1998Author:			Dan IngallsThis changeSet introduces a new primitive...<String> findSubstring: key in: body startingAt: start matchTable: matchTable	<primitive: 246>	Answer the index in the string body at which the substring key first occurs,	at or beyond start.  The match is determined using matchTable, which can be	used to effect, eg, case-insensitive matches.	If no match is found, zero will be returned.In addition, a number of other implementations of the same function are replaced, to wit:<String> findInsensitive: subStringLowercase allUpper:<String> findString: subString startingAt: start, and<String> includesSubstring: aString caseSensitive: caseSensitive.Also, endsWith:, beginsWith:, and match have been rewritten to take advantage of this considerably faster method."!!DynamicInterpreterState class methodsFor: 'class initialization' stamp: 'jm 11/16/1998 21:59'!initializePrimitiveTable	"This table generates a C switch statement for primitive dispatching."	"NOTE: The real limit here is 2047, but our C compiler currently barfs over 700"	MaxPrimitiveIndex _ 700.	PrimitiveTable _ Array new: MaxPrimitiveIndex + 1.	self table: PrimitiveTable from: 	#(	"Integer Primitives (0-19)"		(0 primitiveFail)		(1 primitiveAdd)		(2 primitiveSubtract)		(3 primitiveLessThan)		(4 primitiveGreaterThan)		(5 primitiveLessOrEqual)		(6 primitiveGreaterOrEqual)		(7 primitiveEqual)		(8 primitiveNotEqual)		(9 primitiveMultiply)		(10 primitiveDivide)		(11 primitiveMod)		(12 primitiveDiv)		(13 primitiveQuo)		(14 primitiveBitAnd)		(15 primitiveBitOr)		(16 primitiveBitXor)		(17 primitiveBitShift)		(18 primitiveMakePoint)		(19 primitiveFail)					"Guard primitive for simulation -- *must* fail"		"LargeInteger Primitives (20-39)"		"32-bit logic is aliased to Integer prims above"		(20 39 primitiveFail)		"Float Primitives (40-59)"		(40 primitiveAsFloat)		(41 primitiveFloatAdd)		(42 primitiveFloatSubtract)		(43 primitiveFloatLessThan)		(44 primitiveFloatGreaterThan)		(45 primitiveFloatLessOrEqual)		(46 primitiveFloatGreaterOrEqual)		(47 primitiveFloatEqual)		(48 primitiveFloatNotEqual)		(49 primitiveFloatMultiply)		(50 primitiveFloatDivide)		(51 primitiveTruncated)		(52 primitiveFractionalPart)		(53 primitiveExponent)		(54 primitiveTimesTwoPower)		(55 primitiveSquareRoot)		(56 primitiveSine)		(57 primitiveArctan)		(58 primitiveLogN)		(59 primitiveExp)		"Subscript and Stream Primitives (60-67)"		(60 primitiveAt)		(61 primitiveAtPut)		(62 primitiveSize)		(63 primitiveStringAt)		(64 primitiveStringAtPut)		(65 primitiveNext)		(66 primitiveNextPut)		(67 primitiveAtEnd)		"StorageManagement Primitives (68-79)"		(68 primitiveObjectAt)		(69 primitiveObjectAtPut)		(70 primitiveNew)		(71 primitiveNewWithArg)		(72 primitiveFail)					"Blue Book: primitiveBecome"		(73 primitiveInstVarAt)		(74 primitiveInstVarAtPut)		(75 primitiveAsOop)		(76 primitiveFail)					"Blue Book: primitiveAsObject"		(77 primitiveSomeInstance)		(78 primitiveNextInstance)		(79 primitiveNewMethod)		"Control Primitives (80-89)"		(80 primitiveFail)   					"Blue Book:  primitiveBlockCopy"		(81 primitiveValue)		(82 primitiveValueWithArgs)		(83 primitivePerform)		(84 primitivePerformWithArgs)		(85 primitiveSignal)		(86 primitiveWait)		(87 primitiveResume)		(88 primitiveSuspend)		(89 primitiveFlushCache)		"Input/Output Primitives (90-109)"		(90 primitiveMousePoint)		(91 primitiveFail)					"Blue Book: primitiveCursorLocPut"		(92 primitiveFail)					"Blue Book: primitiveCursorLink"		(93 primitiveInputSemaphore)		(94 primitiveFail)					"Blue Book: primitiveSampleInterval"		(95 primitiveInputWord)		(96 primitiveCopyBits)		(97 primitiveSnapshot)		(98 primitiveFail)					"Blue Book: primitiveTimeWordsInto"		(99 primitiveFail)					"Blue Book: primitiveTickWordsInto"		(100 primitiveFail)					"Blue Book: primitiveSignalAtTick"		(101 primitiveBeCursor)		(102 primitiveBeDisplay)		(103 primitiveScanCharacters)		(104 primitiveDrawLoop)		(105 primitiveStringReplace)		(106 primitiveScreenSize)		(107 primitiveMouseButtons)		(108 primitiveKbdNext)		(109 primitiveKbdPeek)		"System Primitives (110-119)"		(110 primitiveEquivalent)		(111 primitiveClass)		(112 primitiveBytesLeft)		(113 primitiveQuit)		(114 primitiveExitToDebugger)		(115 primitiveFail)					"Blue Book: primitiveOopsLeft"		(116 primitiveFail)		(117 primitiveExternalCall)		(118 primitiveDoPrimitiveWithArgs)		(119 primitiveFlushCacheSelective)		"Miscellaneous Primitives (120-127)"		(120 primitiveFail)		(121 primitiveImageName)		(122 primitiveNoop)					"Blue Book: primitiveImageVolume"		(123 primitiveFail)		(124 primitiveLowSpaceSemaphore)		(125 primitiveSignalAtBytesLeft)		"Squeak Primitives Start Here"		"Squeak Miscellaneous Primitives (128-149)"		(126 primitiveDeferDisplayUpdates)		(127 primitiveShowDisplayRect)		(128 primitiveArrayBecome)		(129 primitiveSpecialObjectsOop)		(130 primitiveFullGC)		(131 primitiveIncrementalGC)		(132 primitiveObjectPointsTo)		(133 primitiveSetInterruptKey)		(134 primitiveInterruptSemaphore)		(135 primitiveMillisecondClock)		(136 primitiveSignalAtMilliseconds)		(137 primitiveSecondsClock)		(138 primitiveSomeObject)		(139 primitiveNextObject)		(140 primitiveBeep)		(141 primitiveClipboardText)		(142 primitiveVMPath)		(143 primitiveShortAt)		(144 primitiveShortAtPut)		(145 primitiveConstantFill)		(146 primitiveReadJoystick)		(147 primitiveWarpBits)		(148 primitiveClone)		(149 primitiveGetAttribute)		"File Primitives (150-169)"		(150 primitiveFileAtEnd)		(151 primitiveFileClose)		(152 primitiveFileGetPosition)		(153 primitiveFileOpen)		(154 primitiveFileRead)		(155 primitiveFileSetPosition)		(156 primitiveFileDelete)		(157 primitiveFileSize)		(158 primitiveFileWrite)		(159 primitiveFileRename)		(160 primitiveDirectoryCreate)		(161 primitiveDirectoryDelimitor)		(162 primitiveDirectoryLookup)		(163 168 primitiveFail)		(169 primitiveDirectorySetMacTypeAndCreator)		"Sound Primitives (170-199)"		(170 primitiveSoundStart)		(171 primitiveSoundStartWithSemaphore)		(172 primitiveSoundStop)		(173 primitiveSoundAvailableSpace)		(174 primitiveSoundPlaySamples)		(175 primitiveSoundPlaySilence)		"obsolete; will be removed in the future"		(176 primWaveTableSoundmixSampleCountintostartingAtpan)		(177 primFMSoundmixSampleCountintostartingAtpan)		(178 primPluckedSoundmixSampleCountintostartingAtpan)		(179 primSampledSoundmixSampleCountintostartingAtpan)		(180 primFMSoundmixSampleCountintostartingAtleftVolrightVol)		(181 primPluckedSoundmixSampleCountintostartingAtleftVolrightVol)		(182 primSampledSoundmixSampleCountintostartingAtleftVolrightVol)		(183 primReverbSoundapplyReverbTostartingAtcount)		(184 primLoopedSampledSoundmixSampleCountintostartingAtleftVolrightVol)		(185 188 primitiveFail)		(189 primitiveSoundInsertSamples)		(190 primitiveSoundStartRecording)		(191 primitiveSoundStopRecording)		(192 primitiveSoundGetRecordingSampleRate)		(193 primitiveSoundRecordSamples)		(194 primitiveSoundSetRecordLevel)		(195 199 primitiveFail)		"Networking Primitives (200-229)"		(200 primitiveInitializeNetwork)		(201 primitiveResolverStartNameLookup)		(202 primitiveResolverNameLookupResult)		(203 primitiveResolverStartAddressLookup)		(204 primitiveResolverAddressLookupResult)		(205 primitiveResolverAbortLookup)		(206 primitiveResolverLocalAddress)		(207 primitiveResolverStatus)		(208 primitiveResolverError)		(209 primitiveSocketCreate)		(210 primitiveSocketDestroy)		(211 primitiveSocketConnectionStatus)		(212 primitiveSocketError)		(213 primitiveSocketLocalAddress)		(214 primitiveSocketLocalPort)		(215 primitiveSocketRemoteAddress)		(216 primitiveSocketRemotePort)		(217 primitiveSocketConnectToPort)		(218 primitiveSocketListenOnPort)		(219 primitiveSocketCloseConnection)		(220 primitiveSocketAbortConnection)		(221 primitiveSocketReceiveDataBufCount)		(222 primitiveSocketReceiveDataAvailable)		(223 primitiveSocketSendDataBufCount)		(224 primitiveSocketSendDone)		(225 229 primitiveFail)		"Other Primitives (230-249)"		(230 primitiveRelinquishProcessor)		(231 primitiveForceDisplayUpdate)		(232 primitiveFormPrint)		(233 primitiveSetFullScreen)		(234 primBitmapdecompressfromByteArrayat)		(235 primStringcomparewithcollated)		(236 primSampledSoundconvert8bitSignedFromto16Bit)		(237 primBitmapcompresstoByteArray)		(238 primitiveSerialPortOpen)		(239 primitiveSerialPortClose)		(240 primitiveSerialPortWrite)		(241 primitiveSerialPortRead)		(242 primitiveFail)		(243 primStringtranslatefromtotable)		(244 primStringfindFirstInStringinSetstartingAt)		(245 primStringindexOfAsciiinStringstartingAt)		(246 findSubstringinstartingAtmatchTable)		(247 249 primitiveFail)		"VM Implementor Primitives (250-255)"		(250 clearProfile)		(251 dumpProfile)		(252 startProfiling)		(253 stopProfiling)		(254 primitiveVMParameter)		(255 primitiveFail)		"Quick Push Const Methods"		(256 primitivePushSelf)		(257 primitivePushTrue)		(258 primitivePushFalse)		(259 primitivePushNil)		(260 primitivePushMinusOne)		(261 primitivePushZero)		(262 primitivePushOne)		(263 primitivePushTwo)		"Quick Push Const Methods"		(264 519 primitiveLoadInstVar)		"MIDI Primitives (520-539)"		(520 primitiveFail)		(521 primitiveMIDIClosePort)		(522 primitiveMIDIGetClock)		(523 primitiveMIDIGetPortCount)		(524 primitiveMIDIGetPortDirectionality)		(525 primitiveMIDIGetPortName)		(526 primitiveMIDIOpenPort)		(527 primitiveMIDIParameterGetOrSet)		(528 primitiveMIDIRead)		(529 primitiveMIDIWrite)		(530 539 primitiveFail)  "reserved for extended MIDI primitives"		"Experimental Asynchrous File Primitives"		(540 primitiveAsyncFileClose)		(541 primitiveAsyncFileOpen)		(542 primitiveAsyncFileReadResult)		(543 primitiveAsyncFileReadStart)		(544 primitiveAsyncFileWriteResult)		(545 primitiveAsyncFileWriteStart)		"Unassigned Primitives"		(546 700 primitiveFail)).! !!Interpreter class methodsFor: 'initialization' stamp: 'jm 11/16/1998 22:00'!initializePrimitiveTable	"This table generates a C switch statement for primitive dispatching."	"NOTE: The real limit here is 2047, but our C compiler currently barfs over 700"	MaxPrimitiveIndex _ 700.	PrimitiveTable _ Array new: MaxPrimitiveIndex + 1.	self table: PrimitiveTable from: 	#(	"Integer Primitives (0-19)"		(0 primitiveFail)		(1 primitiveAdd)		(2 primitiveSubtract)		(3 primitiveLessThan)		(4 primitiveGreaterThan)		(5 primitiveLessOrEqual)		(6 primitiveGreaterOrEqual)		(7 primitiveEqual)		(8 primitiveNotEqual)		(9 primitiveMultiply)		(10 primitiveDivide)		(11 primitiveMod)		(12 primitiveDiv)		(13 primitiveQuo)		(14 primitiveBitAnd)		(15 primitiveBitOr)		(16 primitiveBitXor)		(17 primitiveBitShift)		(18 primitiveMakePoint)		(19 primitiveFail)					"Guard primitive for simulation -- *must* fail"		"LargeInteger Primitives (20-39)"		"32-bit logic is aliased to Integer prims above"		(20 39 primitiveFail)		"Float Primitives (40-59)"		(40 primitiveAsFloat)		(41 primitiveFloatAdd)		(42 primitiveFloatSubtract)		(43 primitiveFloatLessThan)		(44 primitiveFloatGreaterThan)		(45 primitiveFloatLessOrEqual)		(46 primitiveFloatGreaterOrEqual)		(47 primitiveFloatEqual)		(48 primitiveFloatNotEqual)		(49 primitiveFloatMultiply)		(50 primitiveFloatDivide)		(51 primitiveTruncated)		(52 primitiveFractionalPart)		(53 primitiveExponent)		(54 primitiveTimesTwoPower)		(55 primitiveSquareRoot)		(56 primitiveSine)		(57 primitiveArctan)		(58 primitiveLogN)		(59 primitiveExp)		"Subscript and Stream Primitives (60-67)"		(60 primitiveAt)		(61 primitiveAtPut)		(62 primitiveSize)		(63 primitiveStringAt)		(64 primitiveStringAtPut)		(65 primitiveNext)		(66 primitiveNextPut)		(67 primitiveAtEnd)		"StorageManagement Primitives (68-79)"		(68 primitiveObjectAt)		(69 primitiveObjectAtPut)		(70 primitiveNew)		(71 primitiveNewWithArg)		(72 primitiveFail)					"Blue Book: primitiveBecome"		(73 primitiveInstVarAt)		(74 primitiveInstVarAtPut)		(75 primitiveAsOop)		(76 primitiveFail)					"Blue Book: primitiveAsObject"		(77 primitiveSomeInstance)		(78 primitiveNextInstance)		(79 primitiveNewMethod)		"Control Primitives (80-89)"		(80 primitiveFail)   					"Blue Book:  primitiveBlockCopy"		(81 primitiveValue)		(82 primitiveValueWithArgs)		(83 primitivePerform)		(84 primitivePerformWithArgs)		(85 primitiveSignal)		(86 primitiveWait)		(87 primitiveResume)		(88 primitiveSuspend)		(89 primitiveFlushCache)		"Input/Output Primitives (90-109)"		(90 primitiveMousePoint)		(91 primitiveFail)					"Blue Book: primitiveCursorLocPut"		(92 primitiveFail)					"Blue Book: primitiveCursorLink"		(93 primitiveInputSemaphore)		(94 primitiveFail)					"Blue Book: primitiveSampleInterval"		(95 primitiveInputWord)		(96 primitiveCopyBits)		(97 primitiveSnapshot)		(98 primitiveFail)					"Blue Book: primitiveTimeWordsInto"		(99 primitiveFail)					"Blue Book: primitiveTickWordsInto"		(100 primitiveFail)					"Blue Book: primitiveSignalAtTick"		(101 primitiveBeCursor)		(102 primitiveBeDisplay)		(103 primitiveScanCharacters)		(104 primitiveDrawLoop)		(105 primitiveStringReplace)		(106 primitiveScreenSize)		(107 primitiveMouseButtons)		(108 primitiveKbdNext)		(109 primitiveKbdPeek)		"System Primitives (110-119)"		(110 primitiveEquivalent)		(111 primitiveClass)		(112 primitiveBytesLeft)		(113 primitiveQuit)		(114 primitiveExitToDebugger)		(115 primitiveFail)					"Blue Book: primitiveOopsLeft"		(116 primitiveFail)		(117 primitiveExternalCall)		(118 primitiveDoPrimitiveWithArgs)		(119 primitiveFlushCacheSelective)		"Miscellaneous Primitives (120-127)"		(120 primitiveFail)		(121 primitiveImageName)		(122 primitiveNoop)					"Blue Book: primitiveImageVolume"		(123 primitiveFail)		(124 primitiveLowSpaceSemaphore)		(125 primitiveSignalAtBytesLeft)		"Squeak Primitives Start Here"		"Squeak Miscellaneous Primitives (128-149)"		(126 primitiveDeferDisplayUpdates)		(127 primitiveShowDisplayRect)		(128 primitiveArrayBecome)		(129 primitiveSpecialObjectsOop)		(130 primitiveFullGC)		(131 primitiveIncrementalGC)		(132 primitiveObjectPointsTo)		(133 primitiveSetInterruptKey)		(134 primitiveInterruptSemaphore)		(135 primitiveMillisecondClock)		(136 primitiveSignalAtMilliseconds)		(137 primitiveSecondsClock)		(138 primitiveSomeObject)		(139 primitiveNextObject)		(140 primitiveBeep)		(141 primitiveClipboardText)		(142 primitiveVMPath)		(143 primitiveShortAt)		(144 primitiveShortAtPut)		(145 primitiveConstantFill)		(146 primitiveReadJoystick)		(147 primitiveWarpBits)		(148 primitiveClone)		(149 primitiveGetAttribute)		"File Primitives (150-169)"		(150 primitiveFileAtEnd)		(151 primitiveFileClose)		(152 primitiveFileGetPosition)		(153 primitiveFileOpen)		(154 primitiveFileRead)		(155 primitiveFileSetPosition)		(156 primitiveFileDelete)		(157 primitiveFileSize)		(158 primitiveFileWrite)		(159 primitiveFileRename)		(160 primitiveDirectoryCreate)		(161 primitiveDirectoryDelimitor)		(162 primitiveDirectoryLookup)		(163 168 primitiveFail)		(169 primitiveDirectorySetMacTypeAndCreator)		"Sound Primitives (170-199)"		(170 primitiveSoundStart)		(171 primitiveSoundStartWithSemaphore)		(172 primitiveSoundStop)		(173 primitiveSoundAvailableSpace)		(174 primitiveSoundPlaySamples)		(175 primitiveSoundPlaySilence)		"obsolete; will be removed in the future"		(176 primWaveTableSoundmixSampleCountintostartingAtpan)		(177 primFMSoundmixSampleCountintostartingAtpan)		(178 primPluckedSoundmixSampleCountintostartingAtpan)		(179 primSampledSoundmixSampleCountintostartingAtpan)		(180 primFMSoundmixSampleCountintostartingAtleftVolrightVol)		(181 primPluckedSoundmixSampleCountintostartingAtleftVolrightVol)		(182 primSampledSoundmixSampleCountintostartingAtleftVolrightVol)		(183 primReverbSoundapplyReverbTostartingAtcount)		(184 primLoopedSampledSoundmixSampleCountintostartingAtleftVolrightVol)		(185 188 primitiveFail)		(189 primitiveSoundInsertSamples)		(190 primitiveSoundStartRecording)		(191 primitiveSoundStopRecording)		(192 primitiveSoundGetRecordingSampleRate)		(193 primitiveSoundRecordSamples)		(194 primitiveSoundSetRecordLevel)		(195 199 primitiveFail)		"Networking Primitives (200-229)"		(200 primitiveInitializeNetwork)		(201 primitiveResolverStartNameLookup)		(202 primitiveResolverNameLookupResult)		(203 primitiveResolverStartAddressLookup)		(204 primitiveResolverAddressLookupResult)		(205 primitiveResolverAbortLookup)		(206 primitiveResolverLocalAddress)		(207 primitiveResolverStatus)		(208 primitiveResolverError)		(209 primitiveSocketCreate)		(210 primitiveSocketDestroy)		(211 primitiveSocketConnectionStatus)		(212 primitiveSocketError)		(213 primitiveSocketLocalAddress)		(214 primitiveSocketLocalPort)		(215 primitiveSocketRemoteAddress)		(216 primitiveSocketRemotePort)		(217 primitiveSocketConnectToPort)		(218 primitiveSocketListenOnPort)		(219 primitiveSocketCloseConnection)		(220 primitiveSocketAbortConnection)		(221 primitiveSocketReceiveDataBufCount)		(222 primitiveSocketReceiveDataAvailable)		(223 primitiveSocketSendDataBufCount)		(224 primitiveSocketSendDone)		(225 229 primitiveFail)		"Other Primitives (230-249)"		(230 primitiveRelinquishProcessor)		(231 primitiveForceDisplayUpdate)		(232 primitiveFormPrint)		(233 primitiveSetFullScreen)		(234 primBitmapdecompressfromByteArrayat)		(235 primStringcomparewithcollated)		(236 primSampledSoundconvert8bitSignedFromto16Bit)		(237 primBitmapcompresstoByteArray)		(238 primitiveSerialPortOpen)		(239 primitiveSerialPortClose)		(240 primitiveSerialPortWrite)		(241 primitiveSerialPortRead)		(242 primitiveFail)		(243 primStringtranslatefromtotable)		(244 primStringfindFirstInStringinSetstartingAt)		(245 primStringindexOfAsciiinStringstartingAt)		(246 findSubstringinstartingAtmatchTable)		(247 249 primitiveFail)		"VM Implementor Primitives (250-255)"		(250 clearProfile)		(251 dumpProfile)		(252 startProfiling)		(253 stopProfiling)		(254 primitiveVMParameter)		(255 primitiveFail)		"Quick Push Const Methods"		(256 primitivePushSelf)		(257 primitivePushTrue)		(258 primitivePushFalse)		(259 primitivePushNil)		(260 primitivePushMinusOne)		(261 primitivePushZero)		(262 primitivePushOne)		(263 primitivePushTwo)		"Quick Push Const Methods"		(264 519 primitiveLoadInstVar)		"MIDI Primitives (520-539)"		(520 primitiveFail)		(521 primitiveMIDIClosePort)		(522 primitiveMIDIGetClock)		(523 primitiveMIDIGetPortCount)		(524 primitiveMIDIGetPortDirectionality)		(525 primitiveMIDIGetPortName)		(526 primitiveMIDIOpenPort)		(527 primitiveMIDIParameterGetOrSet)		(528 primitiveMIDIRead)		(529 primitiveMIDIWrite)		(530 539 primitiveFail)  "reserved for extended MIDI primitives"		"Experimental Asynchrous File Primitives"		(540 primitiveAsyncFileClose)		(541 primitiveAsyncFileOpen)		(542 primitiveAsyncFileReadResult)		(543 primitiveAsyncFileReadStart)		(544 primitiveAsyncFileWriteResult)		(545 primitiveAsyncFileWriteStart)		"Unassigned Primitives"		(546 700 primitiveFail)).! !!InterpreterSupportCode class methodsFor: 'source file exporting' stamp: 'di 11/15/1998 16:22'!cCodeForMiscPrimitives	"Return the contents of the miscellaneous primitives file, which is generated via automatic translation to C."	^ CCodeGenerator new codeStringForPrimitives: #(		(Bitmap compress:toByteArray:)		(Bitmap decompress:fromByteArray:at:)		(Bitmap encodeBytesOf:in:at:)		(Bitmap encodeInt:in:at:)		(String compare:with:collated:)		(String translate:from:to:table:)			(String findFirstInString:inSet:startingAt:)		(String indexOfAscii:inString:startingAt:)		(String findSubstring:in:startingAt:matchTable:)		(SampledSound convert8bitSignedFrom:to16Bit:))! !!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:43'!findString: subString startingAt: start 	"Answer the index of subString within the receiver, starting at start. If 	the receiver does not contain subString, answer 0."	^ self findSubstring: subString in: self startingAt: start matchTable: CaseSensitiveOrder! !!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:45'!findString: key startingAt: start caseSensitive: caseSensitive	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."	caseSensitive	ifTrue: [^ self findSubstring: key in: self startingAt: start matchTable: CaseSensitiveOrder]	ifFalse: [^ self findSubstring: key in: self startingAt: start matchTable: CaseInsensitiveOrder]! !!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:53'!includesSubstring: aString caseSensitive: caseSensitive		^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0! !!String methodsFor: 'accessing' stamp: 'di 11/16/1998 10:00'!indexOf: aCharacter	^ String indexOfAscii: aCharacter asciiValue inString: self startingAt: 1! !!String methodsFor: 'accessing' stamp: 'di 11/16/1998 10:00'!indexOf: aCharacter  startingAt: start	^ String indexOfAscii: aCharacter asciiValue inString: self startingAt: start! !!String methodsFor: 'accessing' stamp: 'di 11/15/1998 17:02'!indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock	| index |	index _ self findSubstring: sub in: self startingAt: start matchTable: CaseSensitiveOrder.	index = 0 ifTrue: [^ exceptionBlock value].	^ index! !!String methodsFor: 'comparing' stamp: 'di 11/15/1998 17:25'!beginsWith: prefix	"Answer whether the receiver begins with the given prefix string.	The comparison is case-sensitive."	self size < prefix size ifTrue: [^ false].	^ (self findSubstring: prefix in: self startingAt: 1			matchTable: CaseSensitiveOrder) = 1! !!String methodsFor: 'comparing' stamp: 'di 11/15/1998 17:25'!endsWith: suffix	"Answer whether the tail end of the receiver is the same as suffix.	The comparison is case-sensitive."	| extra |	(extra _ self size - suffix size) < 0 ifTrue: [^ false].	^ (self findSubstring: suffix in: self startingAt: extra + 1			matchTable: CaseSensitiveOrder) > 0"  'Elvis' endsWith: 'vis'"! !!String methodsFor: 'comparing' stamp: 'di 11/16/1998 12:23'!match: text 	"Answer whether text matches the pattern in this string.	Matching ignores upper/lower case differences.	Where this string contains *, text may contain any sequence of characters.	The most general match supported is 'foo*bar*zort'."	| i1 i2 ii |	self size = 0 ifTrue: [self error: 'invalid pattern'].	"Look for the first * ..."	i1 _ self indexOf: $*.	i1 = 0 ifTrue: [^ self sameAs: text "Simple match of 'foo' (case-insensitive)"].	i1 > 1 ifTrue:		["Pattern begins with 'foo*'"		(text findString: (self copyFrom: 1 to: i1-1)				startingAt: 1 caseSensitive: false) = 1 ifFalse:			[^ false "text does not begin with 'foo'"].		i1 = self size ifTrue:			[^ true "match of 'foo*' successful"]].	"At this point we know that there is a * at i1, and that,		if it was preceded by 'foo', then the text also begins with 'foo'."	"Look for the second * ..."	i1 = self size ifTrue:		[^ true "Trivial match of '*' always succeeds"].	i2 _ self indexOf: $* startingAt: i1+1.	i2 = 0 ifTrue:		["There is no second * -- match any trailing characters"		^ (text findString: (self copyFrom: i1+1 to: self size)				startingAt: text size - (self size-i1) + 1 caseSensitive: false) > 0].	i2 < self size ifTrue:		["If * not at end, then match any trailing characters, 'zort'."		(text findString: (self copyFrom: i2+1 to: self size)				startingAt: text size - (self size-i2) + 1 caseSensitive: false) > 0 ifFalse:			[^ false "Trailing characters did not match"]].	"At this point all leading and trailing character have been matched."	i2 > (i1+1) ifFalse:		[^ true "Case of degenerate 'baz'"].	"Now match '*baz*' -- just a findStr"	ii _ text findString: (self copyFrom: i1+1 to: i2-1)			startingAt: i1 caseSensitive: false.	"But do not count a hit that overlaps zort"	^ ii > 0 and: [ii <= (text size - (self size-i2) + 1 - (i2-i1-1))]" Examples: 	'*'		match: 'zort' true	'*baz'	match: 'mobaz' true	'*baz'	match: 'mobazo' false	'*baz*'	match: 'mobazo' true	'*baz*'	match: 'mozo' false	'foo*'		match: 'foozo' true	'foo*'		match: 'bozo' false	'foo*baz'	match: 'foo23baz' true	'foo*baz'	match: 'foobaz' true	'foo*baz'	match: 'foo23bazo' false	'foo'	match: 'Foo' true	'foo*baz*zort' match: 'foobazort' false	'foo*baz*zort' match: 'foobazzort' false"! !!String methodsFor: 'system primitives' stamp: 'di 11/15/1998 16:27'!findSubstring: key in: body startingAt: start matchTable: matchTable	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."	| index |	<primitive: 246>	self var: #key declareC: 'unsigned char *key'.	self var: #body declareC: 'unsigned char *body'.	self var: #matchTable declareC: 'unsigned char *matchTable'.	key size = 0 ifTrue: [^ 0].	start to: body size - key size + 1 do:		[:startIndex |		index _ 1.			[(matchTable at: (body at: startIndex+index-1) asciiValue + 1)				= (matchTable at: (key at: index) asciiValue + 1)]				whileTrue:				[index = key size ifTrue: [^ startIndex].				index _ index+1]].	^ 0"' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7"! !!Symbol class methodsFor: 'access' stamp: 'di 11/15/1998 16:46'!selectorsContaining: aString	"Answer a list of selectors that contain aString within them.  Case-insensitive."	| size table candidate selectorList selectorTable ascii |	selectorList _ OrderedCollection new.	(size _ aString size) = 0 ifTrue: [^ selectorList].	aString size = 1 ifTrue:		[ascii _ aString first asciiValue.		ascii < 128 ifTrue:			[selectorList add: (SingleCharSymbols at: ascii + 1)]].	aString first isLetter ifFalse: [^ selectorList].	(SelectorTables size to: 1 by: -1) do:		[:j | selectorTable _ SelectorTables at: j.		1 to: 26 do: [:index |		table _ selectorTable at: index.		1 to: table size do: 			[:t | 			((candidate _ table at: t) == nil) ifFalse:				[candidate size >= size ifTrue:					[((candidate findString: aString startingAt: 1 caseSensitive: false) > 0) ifTrue:							[selectorList add: candidate]]]]]].	^ selectorList"Symbol selectorsContaining: 'scon' "! !String removeSelector: #findInsensitive:allUpper:!