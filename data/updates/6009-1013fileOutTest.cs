"<utf-8>"| me |ctx _ thisContext.[ctx == nil or:[ctx receiver isKindOf: FileStream]] whileFalse:[ctx _ ctx sender].ctx ifNotNil:[me _ ctx receiver].me ifNotNil: [me converter: UTF8TextConverter new.]!'From Squeak3.7-m17n of 30 June 2004 [latest update: #6] on 9 July 2004 at 6:52:26 am'!TestCase subclass: #FileOutFormatTest	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Multilingual-Test'!!FileOutFormatTest methodsFor: 'as yet unclassified' stamp: 'yo 7/8/2004 08:29'!setUp	FileOutFormatTest compileSilently: 'asciiMethod\\	"a method with all ASCII chars."\' withCRs classified: 'support'.	FileOutFormatTest compileSilently: 'latin1Method\\	"a method with some latin1 chars. -Â«<Â·>Â»+"\	^ ''-Â«<Â·>Â»+''' withCRs classified: 'support'.! !!FileOutFormatTest methodsFor: 'as yet unclassified' stamp: 'yo 7/8/2004 09:30'!test1	| stream |	FileDirectory default deleteFileNamed: 'FileOutFormatTest-asciiMethod.st'.	FileOutFormatTest fileOutMethod: #asciiMethod.	FileOutFormatTest removeSelector: #asciiMethod.	stream _ FileDirectory default readOnlyFileNamed: 'FileOutFormatTest-asciiMethod.st'.	stream converter: MacRomanTextConverter new.	self assert: (stream nextLine = '"<utf-8>"') not.	stream reset.	stream fileIn.	self assert: stream converter class = MacRomanTextConverter.! !!FileOutFormatTest methodsFor: 'as yet unclassified' stamp: 'yo 7/8/2004 09:30'!test2	| stream |	FileDirectory default deleteFileNamed: 'FileOutFormatTest-latin1Method.st'.	FileOutFormatTest fileOutMethod: #latin1Method.	FileOutFormatTest removeSelector: #latin1Method.	stream _ FileDirectory default readOnlyFileNamed: 'FileOutFormatTest-latin1Method.st'.	stream converter: MacRomanTextConverter new.	self assert: (stream nextLine = '"<utf-8>"').	stream reset.	stream fileIn.	self assert: stream converter class = UTF8TextConverter.! !FileOutFormatTest removeSelector: #test3!!FileOutFormatTest reorganize!('as yet unclassified' setUp test1 test2)('support' asciiMethod latin1Method)('ã¦ãã¨' nonASCIICategoryMethod)!]lang[(81 3 26)0,5,0!