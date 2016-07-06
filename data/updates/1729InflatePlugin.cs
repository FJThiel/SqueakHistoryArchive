'From Squeak2.6 of 13 October 1999 [latest update: #1727] on 23 December 1999 at 4:14:11 pm'!"Change Set:		InflatePluginDate:			23 December 1999Author:			Andreas RaabAdds a tiny little plugin (four methods) for decompressing. Also enhances InflateStream to deal with source streams so that one doesn't have to read the entire source into memory at once. See FileList>>saveGZipContents for an example of use."!InterpreterPlugin subclass: #InflatePlugin	instanceVariableNames: 'gzCollection gzReadLimit gzState gzBitBuf gzBitPos gzSource gzSourcePos gzSourceLimit gzLitTable gzDistTable gzCollectionSize gzLitTableSize gzDistTableSize '	classVariableNames: 'MaxBits StateNoMoreData '	poolDictionaries: ''	category: 'Squeak-Plugins'!ReadStream subclass: #InflateStream	instanceVariableNames: 'state bitBuf bitPos source sourcePos sourceLimit litTable distTable sourceStream '	classVariableNames: 'BlockProceedBit BlockTypes FixedDistCodes FixedLitCodes MaxBits StateNewBlock StateNoMoreData '	poolDictionaries: ''	category: 'System-Compression'!!Object methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!isStream	"Return true if the receiver responds to the stream protocol"	^false! !!FileList methodsFor: 'file list menu' stamp: 'ar 12/23/1999 16:11'!saveGZipContents	"Save the contents of a gzipped file"	| zipped buffer unzipped |	unzipped _ directory newFileNamed: 		(fileName copyUpToLast: FileDirectory extensionDelimiter).	zipped _ GZipStream on: 		(directory readOnlyFileNamed: self fullName).	buffer _ String new: 50000.	'Extracting ', self fullName displayProgressAt: Sensor cursorPoint		from: 0 to: zipped sourceStream size		during:[:bar|			[zipped atEnd] whileFalse:[				bar value: zipped sourceStream position.				unzipped nextPutAll: (zipped nextInto: buffer)]].	zipped close.	unzipped close.	self updateFileList! !!InflatePlugin commentStamp: 'ar 12/23/1999 14:10' prior: 0!This plugin implements the one crucial function for efficiently decompressing streams.!!InflatePlugin methodsFor: 'primitives' stamp: 'ar 12/22/1999 00:04'!primitiveInflateDecompressBlock	"Primitive. Inflate a single block."	| oop rcvr |	self export: true.	interpreterProxy methodArgumentCount = 2 ifFalse:[^interpreterProxy primitiveFail].	"distance table"	oop _ interpreterProxy stackObjectValue: 0.	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy isWords: oop)		ifFalse:[^interpreterProxy primitiveFail].	gzDistTable _ interpreterProxy firstIndexableField: oop.	gzDistTableSize _ interpreterProxy slotSizeOf: oop.	"literal table"	oop _ interpreterProxy stackObjectValue: 1.	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy isWords: oop)		ifFalse:[^interpreterProxy primitiveFail].	gzLitTable _ interpreterProxy firstIndexableField: oop.	gzLitTableSize _ interpreterProxy slotSizeOf: oop.	"Receiver (InflateStream)"	rcvr _ interpreterProxy stackObjectValue: 2.	interpreterProxy failed ifTrue:[^nil].	(interpreterProxy isPointers: rcvr)		ifFalse:[^interpreterProxy primitiveFail].	(interpreterProxy slotSizeOf: rcvr) < 9		ifTrue:[^interpreterProxy primitiveFail].	"All the integer instvars"	gzReadLimit _ interpreterProxy fetchInteger: 2 ofObject: rcvr.	gzState _ interpreterProxy fetchInteger: 3 ofObject: rcvr.	gzBitBuf _ interpreterProxy fetchInteger: 4 ofObject: rcvr.	gzBitPos _ interpreterProxy fetchInteger: 5 ofObject: rcvr.	gzSourcePos _ interpreterProxy fetchInteger: 7 ofObject: rcvr.	gzSourceLimit _ interpreterProxy fetchInteger: 8 ofObject: rcvr.	interpreterProxy failed ifTrue:[^nil].	gzReadLimit _ gzReadLimit - 1.	gzSourcePos _ gzSourcePos - 1.	gzSourceLimit _ gzSourceLimit - 1.	"collection"	oop _ interpreterProxy fetchPointer: 0 ofObject: rcvr.	(interpreterProxy isIntegerObject: oop)		ifTrue:[^interpreterProxy primitiveFail].	(interpreterProxy isBytes: oop)		ifFalse:[^interpreterProxy primitiveFail].	gzCollection _ interpreterProxy firstIndexableField: oop.	gzCollectionSize _ interpreterProxy byteSizeOf: oop.	"source"	oop _ interpreterProxy fetchPointer: 6 ofObject: rcvr.	(interpreterProxy isIntegerObject: oop)		ifTrue:[^interpreterProxy primitiveFail].	(interpreterProxy isBytes: oop)		ifFalse:[^interpreterProxy primitiveFail].	gzSource _ interpreterProxy firstIndexableField: oop.	"do the primitive"	self gzDecompressBlock.	interpreterProxy failed ifFalse:[		"store modified values back"		interpreterProxy storeInteger: 2 ofObject: rcvr withValue: gzReadLimit + 1.		interpreterProxy storeInteger: 3 ofObject: rcvr withValue: gzState.		interpreterProxy storeInteger: 4 ofObject: rcvr withValue: gzBitBuf.		interpreterProxy storeInteger: 5 ofObject: rcvr withValue: gzBitPos.		interpreterProxy storeInteger: 7 ofObject: rcvr withValue: gzSourcePos + 1.		interpreterProxy pop: 2.	].! !!InflatePlugin methodsFor: 'inflating' stamp: 'ar 12/22/1999 15:14'!gzDecodeValueFrom: table size: tableSize	"Decode the next value in the receiver using the given huffman table."	| bits bitsNeeded tableIndex value index |	self var: #table declareC:'unsigned int *table'.	bitsNeeded _ (table at: 0) bitShift: -24.	"Initial bits needed"	bitsNeeded > MaxBits ifTrue:[interpreterProxy primitiveFail. ^0].	tableIndex _ 2.							"First real table"	[true] whileTrue:[		bits _ self gzNextBits: bitsNeeded.		"Get bits"		index _ tableIndex + bits - 1.		index >= tableSize ifTrue:[interpreterProxy primitiveFail. ^0].		value _ table at: index.					"Lookup entry in table"		(value bitAnd: 16r3F000000) = 0 ifTrue:[^value]. "Check if it is a leaf node"		"Fetch sub table"		tableIndex _ value bitAnd: 16rFFFF.	"Table offset in low 16 bit"		bitsNeeded _ (value bitShift: -24) bitAnd: 255. "Additional bits in high 8 bit"		bitsNeeded > MaxBits ifTrue:[interpreterProxy primitiveFail. ^0]].	^0! !!InflatePlugin methodsFor: 'inflating' stamp: 'ar 12/22/1999 00:04'!gzDecompressBlock	| value extra length distance oldPos oldBits oldBitPos dstPos srcPos max |	self inline: false.	max _ gzCollectionSize - 1.	[gzReadLimit < max and:[gzSourcePos <= gzSourceLimit]] whileTrue:[		"Back up stuff if we're running out of space"		oldBits _ gzBitBuf.		oldBitPos _ gzBitPos.		oldPos _ gzSourcePos.		value _ self gzDecodeValueFrom: gzLitTable size: gzLitTableSize.		value < 256 ifTrue:[ "A literal"			gzCollection at: (gzReadLimit _ gzReadLimit + 1) put: value.		] ifFalse:["length/distance or end of block"			value = 256 ifTrue:["End of block"				gzState _ gzState bitAnd: StateNoMoreData.				^0].			"Compute the actual length value (including possible extra bits)"			extra _ (value bitShift: -16) - 1.			length _ value bitAnd: 16rFFFF.			extra > 0 ifTrue:[length _ length + (self gzNextBits: extra)].			"Compute the distance value"			value _ self gzDecodeValueFrom: gzDistTable size: gzDistTableSize.			extra _ (value bitShift: -16).			distance _ value bitAnd: 16rFFFF.			extra > 0 ifTrue:[distance _ distance + (self gzNextBits: extra)].			(gzReadLimit + length >= max) ifTrue:[				gzBitBuf _ oldBits.				gzBitPos _ oldBitPos.				gzSourcePos _ oldPos.				^0].			dstPos _ gzReadLimit.			srcPos _ gzReadLimit - distance.			1 to: length do:[:i|				gzCollection at: dstPos+i put: (gzCollection at: srcPos+i)].			gzReadLimit _ gzReadLimit + length.		].	].! !!InflatePlugin methodsFor: 'inflating' stamp: 'ar 12/21/1999 23:06'!gzNextBits: n	| bits byte |	self inline: true.	[gzBitPos < n] whileTrue:[		byte _ gzSource at: (gzSourcePos _ gzSourcePos + 1).		gzBitBuf _ gzBitBuf + (byte << gzBitPos).		gzBitPos _ gzBitPos + 8].	bits _ gzBitBuf bitAnd: (1 << n)-1.	gzBitBuf _ gzBitBuf >> n.	gzBitPos _ gzBitPos - n.	^bits! !!InflatePlugin class methodsFor: 'class initialization' stamp: 'ar 12/21/1999 23:02'!initialize	"InflatePlugin initialize"	MaxBits _ 16.	StateNoMoreData _ 1.! !!InflatePlugin class methodsFor: 'translation' stamp: 'ar 12/22/1999 00:07'!declareCVarsIn: cg	cg var: 'gzCollection' declareC:'unsigned char *gzCollection'.	cg var: 'gzSource' declareC:'unsigned char *gzSource'.	cg var: 'gzLitTable' declareC:'unsigned int *gzLitTable'.	cg var: 'gzDistTable' declareC:'unsigned int *gzDistTable'.! !!Interpreter class methodsFor: 'translation' stamp: 'ar 12/23/1999 14:09'!translate: fileName doInlining: inlineFlag	"Time millisecondsToRun: [		Interpreter translate: 'interp.c' doInlining: true.		Smalltalk beep]"	"Interpreter patchInterp: 'Squeak VM PPC'"	| cg |	BitBltSimulation initialize.	Interpreter initialize.	ObjectMemory initialize.	cg _ CCodeGenerator new initialize.	cg addClass: BitBltSimulation.	cg addClass: Interpreter.	cg addClass: ObjectMemory.	BitBltSimulation declareCVarsIn: cg.	Interpreter declareCVarsIn: cg.	ObjectMemory declareCVarsIn: cg.	{FFTPlugin. FloatArrayPlugin. Matrix2x3Plugin. 	BalloonEngineBase. BalloonEnginePlugin.	InflatePlugin} do:[:plugin|		plugin initialize.		cg addClass:plugin.		plugin declareCVarsIn: cg].	cg storeCodeOnFile: fileName doInlining: inlineFlag.! !!Stream methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!isStream	"Return true if the receiver responds to the stream protocol"	^true! !!InflateStream methodsFor: 'initialize' stamp: 'ar 12/23/1999 15:45'!on: aCollectionOrStream	aCollectionOrStream isStream 		ifTrue:[	sourceStream _ aCollectionOrStream.				self getFirstBuffer]		ifFalse:[source _ aCollectionOrStream].	^self on: source from: 1 to: source size.! !!InflateStream methodsFor: 'initialize' stamp: 'ar 12/23/1999 15:35'!on: aCollection from: firstIndex to: lastIndex	bitBuf _ bitPos _ 0.	"The decompression buffer has a size of at 64k,	since we may have distances up to 32k back and	repetitions of at most 32k length forward"	collection _ aCollection species new: 1 << 16.	readLimit _ 0. "Not yet initialized"	position _ 0.	source _ aCollection.	sourceLimit _ lastIndex.	sourcePos _ firstIndex-1.	state _ StateNewBlock.! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 15:31'!close	sourceStream ifNotNil:[sourceStream close].! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/22/1999 01:29'!next	"Answer the next decompressed object in the Stream represented by the	receiver."	<primitive: 65>	position >= readLimit		ifTrue: [^self pastEndRead]		ifFalse: [^collection at: (position _ position + 1)]! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/3/1998 16:18'!next: anInteger 	"Answer the next anInteger elements of my collection.  overriden for simplicity"	| newArray |	newArray _ collection species new: anInteger.	1 to: anInteger do: [:index | newArray at: index put: self next].	^newArray! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 16:06'!next: n into: buffer startingAt: startIndex	"Read n objects into the given collection. 	Return aCollection or a partial copy if less than	n elements have been read."	| c numRead count |	numRead _ 0.	["Force decompression if necessary"	(c _ self next) == nil 		ifTrue:[^buffer copyFrom: 1 to: startIndex+numRead-1].	"Store the first value which provoked decompression"	buffer at: startIndex + numRead put: c.	numRead _ numRead + 1.	"After collection has been filled copy as many objects as possible"	count _ (readLimit - position) min: (n - numRead).	buffer 		replaceFrom: startIndex + numRead 		to: startIndex + numRead + count - 1 		with: collection 		startingAt: position+1.	position _ position + count.	numRead _ numRead + count.	numRead = n] whileFalse.	^buffer! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/21/1999 23:54'!sourceLimit	^sourceLimit! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/21/1999 23:52'!sourcePosition	^sourcePos! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 15:31'!sourceStream	^sourceStream! !!InflateStream methodsFor: 'accessing' stamp: 'ar 12/22/1999 02:04'!upToEnd	"Answer a subcollection from the current access position through the last element of the receiver."	| newStream buffer |	buffer _ collection species new: 1000.	newStream _ WriteStream on: (collection species new: 100).	[self atEnd] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].	^ newStream contents! !!InflateStream methodsFor: 'testing' stamp: 'ar 12/23/1999 15:50'!atEnd	^position >= readLimit and:[state = StateNoMoreData]! !!InflateStream methodsFor: 'private' stamp: 'ar 12/21/1999 22:57'!assert: aBlock	"aBlock value ifFalse:[^self error:'Assertion failed']"! !!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:15'!getFirstBuffer	"Get the first source buffer after initialization has been done"	sourceStream == nil ifTrue:[^self].	source _ sourceStream next: 1 << 16. "This is more than enough..."	sourceLimit _ source size.! !!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:17'!moveContentsToFront	"Move the decoded contents of the receiver to the front so that we have enough space for decoding more data."	| delta |	readLimit > 32768 ifTrue:[		delta _ readLimit - 32767.		collection 			replaceFrom: 1 			to: collection size - delta + 1 			with: collection 			startingAt: delta.		position _ position - delta + 1.		readLimit _ readLimit - delta + 1].! !!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:27'!moveSourceToFront	"Move the encoded contents of the receiver to the front so that we have enough space for decoding more data."	(sourceStream == nil or:[sourceStream atEnd]) ifTrue:[^self].	sourcePos > 10000 ifTrue:[		source 			replaceFrom: 1 			to: source size - sourcePos			with: source 			startingAt: sourcePos + 1.		source _ sourceStream 			next: sourcePos 			into: source 			startingAt: source size - sourcePos + 1.		sourcePos _ 0.		sourceLimit _ source size].! !!InflateStream methodsFor: 'private' stamp: 'ar 12/23/1999 15:18'!pastEndRead	"A client has attempted to read beyond the read limit.	Check in what state we currently are and perform	the appropriate action"	| blockType |	state = StateNoMoreData ifTrue:[^nil]. "Get out early if possible"	"Check if we can move decoded data to front"	self moveContentsToFront.	"Check if we can fetch more source data"	self moveSourceToFront.	state = StateNewBlock ifTrue:[state _ self getNextBlock].	state = StateNoMoreData ifTrue:[^nil].	blockType _ state bitShift: -1.	self perform: (BlockTypes at: blockType+1).	^collection at: (position _ position + 1)! !!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/21/1999 22:59'!computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits	"Assign numerical values to all codes.	Note: The values are stored according to the bit length"	| offsets values baseOffset codeLength |	offsets _ Array new: maxBits.	offsets atAllPut: 0.	baseOffset _ 1.	minBits to: maxBits do:[:bits|		offsets at: bits put: baseOffset.		baseOffset _ baseOffset + (counts at: bits+1)].	values _ WordArray new: aCollection size.	1 to: aCollection size do:[:i|		codeLength _ aCollection at: i.		codeLength > 0 ifTrue:[			baseOffset _ offsets at: codeLength.			values at: baseOffset put: i-1.			offsets at: codeLength put: baseOffset + 1]].	^values! !!InflateStream methodsFor: 'huffman trees' stamp: 'ar 12/21/1999 22:56'!createHuffmanTables: values counts: counts from: minBits to: maxBits	"Create the actual tables"	| table tableStart tableSize tableEnd 	valueIndex tableStack numValues deltaBits maxEntries	lastTable lastTableStart tableIndex lastTableIndex |	table _ WordArray new: ((4 bitShift: minBits) max: 16).	"Create the first entry - this is a dummy.	It gives us information about how many bits to fetch initially."	table at: 1 put: (minBits bitShift: 24) + 2. "First actual table starts at index 2"	"Create the first table from scratch."	tableStart _ 2. "See above"	tableSize _ 1 bitShift: minBits.	tableEnd _ tableStart + tableSize.	"Store the terminal symbols"	valueIndex _ (counts at: minBits+1).	tableIndex _ 0.	1 to: valueIndex do:[:i|		table at: tableStart + tableIndex put: (values at: i).		tableIndex _ self increment: tableIndex bits: minBits].	"Fill up remaining entries with invalid entries"	tableStack _ OrderedCollection new: 10. "Should be more than enough"	tableStack addLast: 		(Array 			with: minBits	"Number of bits (e.g., depth) for this table"			with: tableStart	"Start of table"			with: tableIndex "Next index in table"			with: minBits	"Number of delta bits encoded in table"			with: tableSize - valueIndex "Entries remaining in table").	"Go to next value index"	valueIndex _ valueIndex + 1.	"Walk over remaining bit lengths and create new subtables"	minBits+1 to: maxBits do:[:bits|		numValues _ counts at: bits+1.		[numValues > 0] whileTrue:["Create a new subtable"			lastTable _ tableStack last.			lastTableStart _ lastTable at: 2.			lastTableIndex _ lastTable at: 3.			deltaBits _ bits - (lastTable at: 1).			"Make up a table of deltaBits size"			tableSize _ 1 bitShift: deltaBits.			tableStart _ tableEnd.			tableEnd _ tableEnd + tableSize.			tableEnd > table size 				ifTrue:[table _ self growHuffmanTable: table].			"Connect to last table"			self assert:[(table at: lastTableStart + lastTableIndex) isNil]."Entry must be unused"			table at: lastTableStart + lastTableIndex put: (deltaBits bitShift: 24) + tableStart.			lastTable at: 3 put: (self increment: lastTableIndex bits: (lastTable at: 4)).			lastTable at: 5 put: (lastTable at: 5) - 1.			self assert:[(lastTable at: 5) >= 0]. "Don't exceed tableSize"			"Store terminal values"			maxEntries _ numValues min: tableSize.			tableIndex _ 0.			1 to: maxEntries do:[:i|				table at: tableStart + tableIndex put: (values at: valueIndex).				valueIndex _ valueIndex + 1.				numValues _ numValues - 1.				tableIndex _ self increment: tableIndex bits: deltaBits].			"Check if we have filled up the current table completely"			maxEntries = tableSize ifTrue:[				"Table has been filled. Back up to the last table with space left."				[tableStack isEmpty not and:[(tableStack last at: 5) = 0]]						whileTrue:[tableStack removeLast].			] ifFalse:[				"Table not yet filled. Put it back on the stack."				tableStack addLast:					(Array						with: bits		"Nr. of bits in this table"						with: tableStart	"Start of table"						with: tableIndex "Index in table"						with: deltaBits	"delta bits of table"						with: tableSize - maxEntries "Unused entries in table").			].		].	].	 ^table copyFrom: 1 to: tableEnd-1! !!FastInflateStream methodsFor: 'inflating' stamp: 'ar 12/22/1999 01:30'!decompressBlock: llTable with: dTable	"Process the compressed data in the block.	llTable is the huffman table for literal/length codes	and dTable is the huffman table for distance codes."	| value extra length distance oldPos oldBits oldBitPos |	<primitive:'primitiveInflateDecompressBlock'>	[readLimit < collection size and:[sourcePos <= sourceLimit]] whileTrue:[		"Back up stuff if we're running out of space"		oldBits _ bitBuf.		oldBitPos _ bitPos.		oldPos _ sourcePos.		value _ self decodeValueFrom: llTable.		value < 256 ifTrue:[ "A literal"			collection byteAt: (readLimit _ readLimit + 1) put: value.		] ifFalse:["length/distance or end of block"			value = 256 ifTrue:["End of block"				state _ state bitAnd: StateNoMoreData.				^self].			"Compute the actual length value (including possible extra bits)"			extra _ (value bitShift: -16) - 1.			length _ value bitAnd: 16rFFFF.			extra > 0 ifTrue:[length _ length + (self nextBits: extra)].			"Compute the distance value"			value _ self decodeValueFrom: dTable.			extra _ (value bitShift: -16).			distance _ value bitAnd: 16rFFFF.			extra > 0 ifTrue:[distance _ distance + (self nextBits: extra)].			(readLimit + length >= collection size) ifTrue:[				bitBuf _ oldBits.				bitPos _ oldBitPos.				sourcePos _ oldPos.				^self].			collection 					replaceFrom: readLimit+1 					to: readLimit + length + 1 					with: collection 					startingAt: readLimit - distance + 1.			readLimit _ readLimit + length.		].	].! !!FastInflateStream class methodsFor: 'class initialization' stamp: 'ar 12/21/1999 23:00'!initialize	"FastInflateStream initialize"	| low high |	"Init literal/length map"	low _ #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 ).	high _ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0).	LiteralLengthMap _ WordArray new: 256 + 32.	1 to: 257 do:[:i| LiteralLengthMap at: i put: i-1].	1 to: 29 do:[:i| LiteralLengthMap at: 257+i put: (low at:i) + ( (high at: i) + 1 << 16)].	"Init distance map"	high _ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).	low _ #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769			1025 1537 2049 3073 4097 6145 8193 12289 16385 24577).	DistanceMap _ WordArray new: 32.	1 to: 30 do:[:i| DistanceMap at: i put: (low at: i) + ( (high at: i) << 16)].	"Init fixed block huffman tables"	FixedLitTable _ self basicNew				huffmanTableFrom: FixedLitCodes				mappedBy: LiteralLengthMap.	FixedDistTable _ self basicNew				huffmanTableFrom: FixedDistCodes				mappedBy: DistanceMap.! !!StandardFileStream methodsFor: 'read, write, position' stamp: 'ar 12/22/1999 15:40'!upToEnd	"Answer a subcollection from the current access position through the last element of the receiver."	| newStream buffer |	buffer _ buffer1 species new: 1000.	newStream _ WriteStream on: (buffer1 species new: 100).	[self atEnd] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].	^ newStream contents! !InflatePlugin initialize!InflateStream removeSelector: #nextInto:!FastInflateStream initialize!