'From Squeak2.8alpha of 4 February 2000 [latest update: #2098] on 12 May 2000 at 6:16:37 pm'!"Change Set:		048AssertRefact-smaDate:			7 May 2000Author:			Stefan Matthias AustMoved various #assert: methods to Object and created a new Exception class AssertionFailure which will be used to signal a (resumable) assertion error.  One might also want to define a few utility methods like 	assertNil: aBlock		self assert: [aBlock isNil]	assertNotNil: aBlock		self assert: [aBlock notNil]	cannotHappen		""See 'The Pragmatic Programmer', page 122, for an example.""		self assert: [false]You may argue that one should be able to turn off assertion but I'd recommend to read 'The Pragmatic Programmer', page 123, which this isn't a good idea."!Halt subclass: #AssertionFailure	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'System-Exceptions Extensions'!!Object methodsFor: 'error handling' stamp: 'sma 5/6/2000 19:35'!assert: aBlock	"Throw an assertion error if aBlock does not evaluates to true."	aBlock value ifFalse: [AssertionFailure signal: 'Assertion failed']! !!InflateStream methodsFor: 'huffman trees' stamp: 'sma 5/12/2000 10:49'!createHuffmanTables: values counts: counts from: minBits to: maxBits	"Create the actual tables"	| table tableStart tableSize tableEnd 	valueIndex tableStack numValues deltaBits maxEntries	lastTable lastTableStart tableIndex lastTableIndex |	table _ WordArray new: ((4 bitShift: minBits) max: 16).	"Create the first entry - this is a dummy.	It gives us information about how many bits to fetch initially."	table at: 1 put: (minBits bitShift: 24) + 2. "First actual table starts at index 2"	"Create the first table from scratch."	tableStart _ 2. "See above"	tableSize _ 1 bitShift: minBits.	tableEnd _ tableStart + tableSize.	"Store the terminal symbols"	valueIndex _ (counts at: minBits+1).	tableIndex _ 0.	1 to: valueIndex do:[:i|		table at: tableStart + tableIndex put: (values at: i).		tableIndex _ self increment: tableIndex bits: minBits].	"Fill up remaining entries with invalid entries"	tableStack _ OrderedCollection new: 10. "Should be more than enough"	tableStack addLast: 		(Array 			with: minBits	"Number of bits (e.g., depth) for this table"			with: tableStart	"Start of table"			with: tableIndex "Next index in table"			with: minBits	"Number of delta bits encoded in table"			with: tableSize - valueIndex "Entries remaining in table").	"Go to next value index"	valueIndex _ valueIndex + 1.	"Walk over remaining bit lengths and create new subtables"	minBits+1 to: maxBits do:[:bits|		numValues _ counts at: bits+1.		[numValues > 0] whileTrue:["Create a new subtable"			lastTable _ tableStack last.			lastTableStart _ lastTable at: 2.			lastTableIndex _ lastTable at: 3.			deltaBits _ bits - (lastTable at: 1).			"Make up a table of deltaBits size"			tableSize _ 1 bitShift: deltaBits.			tableStart _ tableEnd.			tableEnd _ tableEnd + tableSize.			[tableEnd > table size ]				whileTrue:[table _ self growHuffmanTable: table].			"Connect to last table"			self assert:[(table at: lastTableStart + lastTableIndex) = 0]."Entry must be unused"			table at: lastTableStart + lastTableIndex put: (deltaBits bitShift: 24) + tableStart.			lastTable at: 3 put: (self increment: lastTableIndex bits: (lastTable at: 4)).			lastTable at: 5 put: (lastTable at: 5) - 1.			self assert:[(lastTable at: 5) >= 0]. "Don't exceed tableSize"			"Store terminal values"			maxEntries _ numValues min: tableSize.			tableIndex _ 0.			1 to: maxEntries do:[:i|				table at: tableStart + tableIndex put: (values at: valueIndex).				valueIndex _ valueIndex + 1.				numValues _ numValues - 1.				tableIndex _ self increment: tableIndex bits: deltaBits].			"Check if we have filled up the current table completely"			maxEntries = tableSize ifTrue:[				"Table has been filled. Back up to the last table with space left."				[tableStack isEmpty not and:[(tableStack last at: 5) = 0]]						whileTrue:[tableStack removeLast].			] ifFalse:[				"Table not yet filled. Put it back on the stack."				tableStack addLast:					(Array						with: bits		"Nr. of bits in this table"						with: tableStart	"Start of table"						with: tableIndex "Index in table"						with: deltaBits	"delta bits of table"						with: tableSize - maxEntries "Unused entries in table").			].		].	].	 ^table copyFrom: 1 to: tableEnd-1! !ZipEncoderTree removeSelector: #assert:!ZipEncoderNode removeSelector: #assert:!VRMLNodeSpec removeSelector: #assert:!TextDiffBuilder removeSelector: #assert:!InflateStream removeSelector: #assert:!DeflateStream removeSelector: #assert:!