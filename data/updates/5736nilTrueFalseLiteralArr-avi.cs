'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 16 February 2004 at 9:31:58 pm'!"Change Set:		nilTrueFalseInLiteralArray-aviDate:			16 February 2004Author:			Avi BryantModifies Scanner>>scanLitVec to be ANSI-compliant and treat #(nil true false) as a collection of an UndefinedObject and two Booleans, rather than as three Symbols.  Includes a test case (which also ensures that #(#nil #true #false) and #(#'nil' #'true' #'false') still contain symbols)."!TestCase subclass: #ArrayLiteralTest	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Tests-System-Compiler'!!ArrayLiteralTest methodsFor: 'as yet unclassified' stamp: 'avi 2/16/2004 21:09'!tearDown	self class removeSelector: #array! !!ArrayLiteralTest methodsFor: 'as yet unclassified' stamp: 'avi 2/16/2004 21:08'!testReservedIdentifiers	self class compile: 'array ^ #(nil true false)'.	self assert: self array = {nil. true. false}.! !!ArrayLiteralTest methodsFor: 'as yet unclassified' stamp: 'avi 2/16/2004 21:09'!testSymbols	self class compile: 'array ^ #(#nil #true #false #''nil'' #''true'' #''false'')'.	self assert: self array = {#nil. #true. #false. #nil. #true. #false}.! !!Scanner methodsFor: 'expression types' stamp: 'avi 2/16/2004 21:28'!scanLitVec	| s |	s _ WriteStream on: (Array new: 16).	[tokenType = #rightParenthesis or: [tokenType = #doIt]]		whileFalse: 			[tokenType = #leftParenthesis				ifTrue: 					[self scanToken; scanLitVec]				ifFalse: 					[tokenType = #word | (tokenType = #keyword) | (tokenType = #colon)						ifTrue: 							[self scanLitWord.							token = #true ifTrue: [token _ true].							token = #false ifTrue: [token _ false].							token = #nil ifTrue: [token _ nil]]						ifFalse:							[(token == #- 									and: [(typeTable at: hereChar asciiValue) = #xDigit])								ifTrue: 									[self scanToken.									token _ token negated]]].			s nextPut: token.			self scanToken].	token _ s contents! !ArrayLiteralTest removeSelector: #array!