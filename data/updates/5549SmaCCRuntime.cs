'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5545] on 14 November 2003 at 12:16:12 pm'!"Change Set:		SmaCCRuntimeDate:			14 November 2003Author:			Marcus DenkerThese classes are needed to run SmaCC-genrated compilers.SmaCC is now MIT-Licensed (thanks to John Brant and Don Roberts!!), thus this now can be part of Squeak Base. The next Compiler (Anthony's or something really close to that) will be build using SmaCC. Thus we need the SmaCC Runtime be part of the Image. I (md) vote for adding all the needed stuff step-by-step, thus makingit all simpler. This is the first part of the future Compiler, nextwill be Anthony's GeneralEnh-ajh.1.cs, then I guess I'l look into hisnice symbolic Squeak-Bytecode Assembler IRBuilder, after that the RB-AST(which is now MIT-Licensed, too), and so on.This fileout it identical to the SAR-Package on SqueakMap, portedand maintained by Markus Gaelli. I (md) just changed the Class-Categoryto be SmaCC-Runtime"!Stream subclass: #SmaCCLineNumberStream	instanceVariableNames: 'sourceStream previousWasCR eolPositions lastPosition '	classVariableNames: ''	poolDictionaries: ''	category: 'SmaCC-Runtime'!!SmaCCLineNumberStream commentStamp: 'jmb' prior: 0!SmaCCLineNumberStream is a wrapper for streams that calculates line numbers.

Instance Variables:
	eolPositions	<OrderedCollection of: Integer>	the positions of each end of line
	lastPosition	<Integer>	the position of the last character that we have calculated the end of line information for (we know the line number for all characters before this position and don't know anything about the characters after this position)
	previousWasCR	<Boolean>	was the previous character a CR. This is used for CR LF streams. A CR LF combination should only increment the line counter by 1
	sourceStream	<Stream>	the stream that we are wrapping

!Object subclass: #SmaCCParser	instanceVariableNames: 'scanner currentToken errorToken stateStack nodeStack '	classVariableNames: ''	poolDictionaries: ''	category: 'SmaCC-Runtime'!!SmaCCParser commentStamp: 'jmb' prior: 0!SmaCCParser is an abstract class that defines most of the parsing actions. Subclasses will define methods that specify their transitions and reduction actions. These are normally defined automatically when compiling the parser.

Subclasses must implement the following messages:
	accessing
		emptySymbolTokenId
		reduceTable
		transitionTable

Instance Variables:
	currentToken	<SmaCCToken>	the token last returned by the scanner that has not been shifted (reduce actions leave the current token alone)
	nodeStack	<OrderedCollection>	collection of items on stack. These items are specific to the parser and can be any object. 
	scanner	<SmaCCScanner>	our scanner
	stateStack	<OrderedCollection of: Integer>	the stack of states for our parser (standard LR state stack)

!Error subclass: #SmaCCParserError	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'SmaCC-Runtime'!!SmaCCParserError commentStamp: 'jmb' prior: 0!SmaCCParserException is the exception raised when a parsing error occurs. The description of the exception will be the error message and the parameter of the exception is the parser. With this information, you can insert a custom error message in your text view that you are parsing. For example, in VisualWorks, the following code will insert an error message into your text view:
	textController insertAndSelect: ex description , ' ->' at: ex parameter position!Object subclass: #SmaCCScanner	instanceVariableNames: 'stream start matchActions matchEnd currentCharacter outputStream lastOutputStreamMatchPosition lastMatchWasEmpty returnMatchBlock '	classVariableNames: ''	poolDictionaries: ''	category: 'SmaCC-Runtime'!!SmaCCScanner commentStamp: 'jmb' prior: 0!SmaCCScanner is an abstract class that represents a scanner for the parser. The scanner converts its string input into SmaCCToken objects that the parser then uses for its parsing.

Subclasses must implement the following messages:
	accessing
		scanForToken

Instance Variables:
	currentCharacter	<Character>	the current character we are scanning
	lastMatchWasEmpty	<Boolean>	was our last scanning match an empty string -- don't allow two empty matches in a row
	lastOutputStreamMatchPosition	<Integer>	the position in the outputStream of the last match
	matchActions	<Array | Symbol>	the actions for the last match (a symbol means that the action should be performed on the scanner)
	matchEnd	<Integer>	the position of the last match in the stream (our input stream)
	outputStream	<PositionableStream>	the matched characters go in this stream. After a match is made, we take this stream's contents and create a token object.
	returnMatchBlock	<BlockClosure>	when we match a token evaluate this block with the token (hack to return from multiple levels)
	start	<Integer>	the starting position of a match in the stream
	stream	<Stream>	our input

!SmaCCScanner class	instanceVariableNames: 'keywordMap '!Object subclass: #SmaCCToken	instanceVariableNames: 'start id value '	classVariableNames: ''	poolDictionaries: ''	category: 'SmaCC-Runtime'!!SmaCCToken commentStamp: 'jmb' prior: 0!SmaCCTokens are used as the interface objects between scanner and parser. They hold the string that was scanned and its position information. Also, included in the token is its id. The id specifies what type of token it is.

Instance Variables:
	id	<Array of: Integer>	the list of possible token types this represents. There can be overlapping tokens, so we list all of the id here. The default parser only looks at the first id, but we can redefine this behavior in a subclass to look at all possibilities until we find a valid token.
	start	<Integer>	the starting position of the token in the original input
	value	<Object>	the value of our token (normally a string, but could be anything)
!!SmaCCLineNumberStream methodsFor: 'testing'!atEnd
	^sourceStream atEnd! !!SmaCCLineNumberStream methodsFor: 'testing'!isReadable
	^sourceStream isReadable! !!SmaCCLineNumberStream methodsFor: 'testing'!isWritable
	^sourceStream isWritable! !!SmaCCLineNumberStream methodsFor: 'error handling'!doesNotUnderstand: aMessage 
	^sourceStream perform: aMessage selector withArguments: aMessage arguments! !!SmaCCLineNumberStream methodsFor: 'accessing'!contents
	^sourceStream contents! !!SmaCCLineNumberStream methodsFor: 'accessing'!flush
	^sourceStream flush! !!SmaCCLineNumberStream methodsFor: 'accessing'!lineNumber
	| index start stop pos |
	pos := sourceStream position.
	pos >= eolPositions last ifTrue: [^eolPositions size].
	start := 1.
	stop := eolPositions size.
	[start + 1 < stop] whileTrue: 
			[index := (start + stop) // 2.
			(eolPositions at: index) <= pos 
				ifTrue: [start := index]
				ifFalse: [stop := index]].
	^start! !!SmaCCLineNumberStream methodsFor: 'accessing'!next
	| character |
	character := sourceStream next.
	sourceStream position - 1 == lastPosition 
		ifTrue: 
			[lastPosition := lastPosition + 1.
			character == Character cr 
				ifTrue: 
					[eolPositions add: sourceStream position.
					previousWasCR := true]
				ifFalse: 
					[(previousWasCR not and: [character == Character lf]) 
						ifTrue: [eolPositions add: sourceStream position].
					previousWasCR := false]].
	^character! !!SmaCCLineNumberStream methodsFor: 'accessing'!nextPut: anObject 
	^sourceStream nextPut: anObject! !!SmaCCLineNumberStream methodsFor: 'accessing'!position
	^sourceStream position! !!SmaCCLineNumberStream methodsFor: 'accessing'!position: anInteger 
	anInteger > lastPosition 
		ifTrue: 
			[sourceStream position: lastPosition.
			[sourceStream position < anInteger and: [sourceStream atEnd not]] 
				whileTrue: [self next]]
		ifFalse: [sourceStream position: anInteger]! !!SmaCCLineNumberStream methodsFor: 'accessing'!skip: anInteger
	^self position: self position + anInteger! !!SmaCCLineNumberStream methodsFor: 'accessing' stamp: 'jmb 1/20/2003 21:45'!upTo: aCharacter 	| stream char |	stream := WriteStream on: String new.	[self atEnd or: [(char := self next) == aCharacter]] 		whileFalse: [stream nextPut: char].	^stream contents! !!SmaCCLineNumberStream methodsFor: 'initialize-release'!on: aReadStream 
	sourceStream := aReadStream.
	eolPositions := OrderedCollection with: aReadStream position.
	lastPosition := aReadStream position.
	previousWasCR := false! !!SmaCCLineNumberStream class methodsFor: 'instance creation'!on: aReadStream 
	^(self basicNew)
		on: aReadStream;
		yourself! !!SmaCCParser methodsFor: 'testing'!isEOFToken
	^currentToken id first = self emptySymbolTokenId! !!SmaCCParser methodsFor: 'accessing'!emptySymbolTokenId
	^scanner emptySymbolTokenId! !!SmaCCParser methodsFor: 'accessing'!errorTable
	^#()! !!SmaCCParser methodsFor: 'accessing'!errorTokenId
	^scanner errorTokenId! !!SmaCCParser methodsFor: 'accessing'!parse
	self setDefaultStartingStateIfNone.
	self performParsingLoop.
	^nodeStack last! !!SmaCCParser methodsFor: 'accessing'!position
	^currentToken isNil 
		ifTrue: [scanner position]
		ifFalse: [currentToken startPosition]! !!SmaCCParser methodsFor: 'accessing'!reduceTable
	^self subclassResponsibility! !!SmaCCParser methodsFor: 'accessing'!transitionTable
	^self subclassResponsibility! !!SmaCCParser methodsFor: 'initialize-release'!initialize
	nodeStack := OrderedCollection new! !!SmaCCParser methodsFor: 'initialize-release'!scanner: aScanner 
	scanner := aScanner! !!SmaCCParser methodsFor: 'initialize-release'!setStartingState: startingState 
	stateStack := OrderedCollection with: startingState! !!SmaCCParser methodsFor: 'private-error handling'!checkForErrors
	"If we have an error correction installed, we might have handled the errors. If we did, we don't 
	want to return the result, so we raise a final exception that can't be proceeded."

	errorToken isNil ifTrue: [^self].
	currentToken := errorToken.
	self reportErrorMessage: 'Token not expected'! !!SmaCCParser methodsFor: 'private-error handling'!dismissErrorToken
	currentToken := nil.
	self getNextToken! !!SmaCCParser methodsFor: 'private-error handling'!dismissStackTopForErrorRecovery
	stateStack removeLast.
	^nodeStack removeLast! !!SmaCCParser methodsFor: 'private-error handling'!errorHandlerStates
	^stateStack collect: 
			[:each | 
			| action |
			action := self actionForState: each and: self errorTokenId.
			(action bitAnd: self actionMask) = 1 
				ifTrue: [action bitShift: -2]
				ifFalse: [0]]! !!SmaCCParser methodsFor: 'private-error handling'!handleError: anInteger 
	errorToken isNil ifTrue: [errorToken := currentToken].
	(currentToken id first = self emptySymbolTokenId 
		or: [self hasErrorHandler not]) ifTrue: [self reportError: anInteger].
	self findErrorHandlerIfNoneUseErrorNumber: anInteger! !!SmaCCParser methodsFor: 'private-error handling'!hasErrorHandler
	^self errorHandlerStates anySatisfy: [:each | each ~~ 0]! !!SmaCCParser methodsFor: 'private-error handling'!reportError: anInteger 
	self reportErrorMessage: (anInteger = 0 
				ifTrue: ['Token not expected']
				ifFalse: [self errorTable at: anInteger])! !!SmaCCParser methodsFor: 'private-error handling' stamp: 'jmb 1/20/2003 21:52'!reportErrorMessage: aString 	(SmaCCParserError new)		tag: self;		signal: aString! !!SmaCCParser methodsFor: 'private-error handling'!willShift: potentialStateStack 
	| action compoundAction reduceEntry size |
	compoundAction := self actionForState: potentialStateStack last
				and: currentToken id first.
	action := compoundAction bitAnd: self actionMask.
	action == self shiftAction ifTrue: [^true].
	action == self reduceAction 
		ifTrue: 
			[reduceEntry := self reduceTable at: (compoundAction bitShift: -2).
			size := reduceEntry at: 2.
			size timesRepeat: [potentialStateStack removeLast].
			potentialStateStack 
				add: ((self actionForState: potentialStateStack last
						and: (reduceEntry at: 1)) bitShift: -2).
			^self willShift: potentialStateStack].
	^false! !!SmaCCParser methodsFor: 'private'!acceptAction
	^0! !!SmaCCParser methodsFor: 'private'!actionFor: aSymbolIndex 
	^self actionForState: self currentState and: aSymbolIndex! !!SmaCCParser methodsFor: 'private'!actionForCurrentToken
	^self actionFor: currentToken id first! !!SmaCCParser methodsFor: 'private'!actionForState: stateIndex and: aSymbolIndex 
	| index row |
	row := self transitionTable at: stateIndex.
	(row at: 1) == 2 
		ifTrue: 
			[index := self 
						binarySearchIn: row
						for: aSymbolIndex
						size: 1.
			index == 0 ifTrue: [^self errorAction] ifFalse: [^row at: 2]]
		ifFalse: 
			[index := self 
						binarySearchIn: row
						for: aSymbolIndex
						size: 2.
			index == 0 ifTrue: [^self errorAction] ifFalse: [^row at: index - 1]]! !!SmaCCParser methodsFor: 'private'!actionMask
	^2r11! !!SmaCCParser methodsFor: 'private'!binarySearchIn: aRow for: aSymbolIndex size: step 
	| start mid length midItem stop |
	start := 3.
	stop := aRow size.
	length := (stop - start) // step.
	[length > 4] whileTrue: 
			[length := length bitShift: -1.
			mid := length * step + start.
			midItem := aRow at: mid.
			midItem <= aSymbolIndex ifTrue: [start := mid] ifFalse: [stop := mid]].
	[start <= stop] whileTrue: 
			[(aRow at: start) == aSymbolIndex ifTrue: [^start].
			start := start + step].
	^0! !!SmaCCParser methodsFor: 'private'!currentState
	^stateStack last! !!SmaCCParser methodsFor: 'private'!errorAction
	^3! !!SmaCCParser methodsFor: 'private'!findErrorHandlerIfNoneUseErrorNumber: anInteger 
	| handlerStates index startingErrorToken newStack |
	handlerStates := self errorHandlerStates reverse.
	startingErrorToken := currentToken.
	
	[index := (1 to: handlerStates size) detect: 
					[:each | 
					| state |
					state := handlerStates at: each.
					state ~= 0 and: 
							[newStack := stateStack copyFrom: 1 to: handlerStates size - each + 1.
							newStack add: state.
							self willShift: newStack]]
				ifNone: [nil].
	index isNil] 
			whileTrue: 
				[self dismissErrorToken.
				currentToken id first = self emptySymbolTokenId 
					ifTrue: 
						[currentToken := startingErrorToken.
						self reportError: anInteger]].
	index - 1 timesRepeat: [self dismissStackTopForErrorRecovery].
	stateStack addLast: (handlerStates at: index).
	nodeStack addLast: startingErrorToken! !!SmaCCParser methodsFor: 'private'!getNextToken
	currentToken isNil ifTrue: [currentToken := scanner next]! !!SmaCCParser methodsFor: 'private'!liftFirstValue: aCollection 
	^aCollection first! !!SmaCCParser methodsFor: 'private'!liftLastValue: aCollection 
	^aCollection last! !!SmaCCParser methodsFor: 'private'!liftSecondValue: aCollection 
	^aCollection at: 2! !!SmaCCParser methodsFor: 'private'!performParsingLoop
	| action actionType |
	
	[self getNextToken.
	action := self actionForCurrentToken.
	action = self acceptAction] 
			whileFalse: 
				[actionType := action bitAnd: self actionMask.
				action := action bitShift: -2.
				actionType == self shiftAction 
					ifTrue: [self shift: action]
					ifFalse: 
						[actionType == self reduceAction 
							ifTrue: [self reduce: action]
							ifFalse: [self handleError: action]]].
	self checkForErrors! !!SmaCCParser methodsFor: 'private'!performReduceMethod: aSymbol with: items 
	^aSymbol last == $: 
		ifTrue: [self perform: aSymbol with: items]
		ifFalse: [self perform: aSymbol]! !!SmaCCParser methodsFor: 'private'!reduce: anInteger 
	| reduceEntry items size |
	reduceEntry := self reduceTable at: anInteger.
	items := OrderedCollection new: (size := reduceEntry at: 2).
	size timesRepeat: 
			[items addFirst: nodeStack removeLast.
			stateStack removeLast].
	nodeStack add: (self performReduceMethod: (reduceEntry at: 3) with: items).
	stateStack add: ((self actionFor: (reduceEntry at: 1)) bitShift: -2)! !!SmaCCParser methodsFor: 'private'!reduceAction
	^2r10! !!SmaCCParser methodsFor: 'private'!reduceFor: aCollection 
	| newCollection item |
	(aCollection allSatisfy: [:each | each class ~~ OrderedCollection]) 
		ifTrue: [^aCollection].
	aCollection first class == OrderedCollection 
		ifTrue: 
			[newCollection := aCollection first.
			2 to: aCollection size
				do: 
					[:i | 
					item := aCollection at: i.
					item class = OrderedCollection 
						ifTrue: [newCollection addAll: item]
						ifFalse: [newCollection add: item]].
			^newCollection].
	newCollection := OrderedCollection new.
	aCollection do: 
			[:each | 
			each class == OrderedCollection 
				ifTrue: [newCollection addAll: each]
				ifFalse: [newCollection add: each]].
	^newCollection! !!SmaCCParser methodsFor: 'private'!setDefaultStartingStateIfNone
	stateStack isNil 
		ifTrue: [self setStartingState: self class defaultStartingState]! !!SmaCCParser methodsFor: 'private'!shift: stateIndex 
	stateStack add: stateIndex.
	nodeStack add: currentToken.
	currentToken := nil! !!SmaCCParser methodsFor: 'private'!shiftAction
	^2r01! !!SmaCCParser class methodsFor: 'instance creation'!new
	^(super new)
		initialize;
		yourself! !!SmaCCParser class methodsFor: 'instance creation'!on: aStream 
	| parser scanner |
	scanner := self scannerClass on: aStream.
	parser := self new.
	parser scanner: scanner.
	^parser! !!SmaCCParser class methodsFor: 'accessing'!parse: aString 
	^self parse: aString startingAt: self defaultStartingState! !!SmaCCParser class methodsFor: 'accessing' stamp: 'jmb 1/20/2003 22:09'!parse: aString onError: aBlock 
	^[self parse: aString] on: SmaCCParserError
		do: [:ex | ex return: (aBlock value: ex description value: ex tag position)]! !!SmaCCParser class methodsFor: 'accessing'!parse: aString startingAt: anInteger 
	^self parseStream: (ReadStream on: aString) startingAt: anInteger! !!SmaCCParser class methodsFor: 'accessing' stamp: 'jmb 1/20/2003 22:04'!parse: aString startingAt: anInteger onError: aBlock 
	^[self parse: aString startingAt: anInteger] on: SmaCCParserError
		do: [:ex | ex return: (aBlock value: ex description value: ex tag position)]! !!SmaCCParser class methodsFor: 'accessing'!parseStream: aStream 
	^self parseStream: aStream startingAt: self defaultStartingState! !!SmaCCParser class methodsFor: 'accessing' stamp: 'jmb 1/20/2003 22:11'!parseStream: aStream onError: aBlock 
	^[self parseStream: aStream] on: SmaCCParserError
		do: [:ex | ex return: (aBlock value: ex description value: ex tag position)]! !!SmaCCParser class methodsFor: 'accessing'!parseStream: aStream startingAt: anInteger 
	| parser |
	parser := self on: aStream.
	parser setStartingState: anInteger.
	^parser parse! !!SmaCCParser class methodsFor: 'accessing'!parseStream: aStream startingAt: anInteger onError: aBlock 
	^[self parseStream: aStream startingAt: anInteger] 
		on: SmaCCParserError
		do: [:ex | ex return: (aBlock value: ex description value: ex parameter position)]! !!SmaCCParser class methodsFor: 'private'!defaultStartingState
	^1! !!SmaCCParser class methodsFor: 'private'!scannerClass
	^self subclassResponsibility! !!SmaCCScanner methodsFor: 'testing'!atEnd
	^stream atEnd! !!SmaCCScanner methodsFor: 'initialize-release'!initialize
	outputStream := WriteStream on: (String new: self initialBufferSize).
	lastMatchWasEmpty := true! !!SmaCCScanner methodsFor: 'initialize-release'!on: aStream 
	stream := aStream.
	start := stream position! !!SmaCCScanner methodsFor: 'default token handling'!comment
	"In case someone wants to record the comments"

	self whitespace! !!SmaCCScanner methodsFor: 'default token handling'!whitespace
	"By default, eat the whitespace"

	self resetScanner.
	self scanForToken! !!SmaCCScanner methodsFor: 'accessing'!contents
	| writeStream token |
	writeStream := WriteStream on: Array new.
	[self atEnd] whileFalse: 
			[token := self next.
			token notNil ifTrue: [writeStream nextPut: token]].
	^writeStream contents! !!SmaCCScanner methodsFor: 'accessing'!emptySymbolTokenId
	^self subclassResponsibility! !!SmaCCScanner methodsFor: 'accessing'!errorTokenId
	^self subclassResponsibility! !!SmaCCScanner methodsFor: 'accessing'!lineNumber
	"This requires the stream to be a line number stream (see the #needsLineNumbers class method)."

	^stream lineNumber! !!SmaCCScanner methodsFor: 'accessing'!next
	self resetScanner.
	returnMatchBlock := [:match | ^match].
	self scanForToken! !!SmaCCScanner methodsFor: 'accessing'!position
	^stream position! !!SmaCCScanner methodsFor: 'accessing'!position: anInteger
	^stream position: anInteger! !!SmaCCScanner methodsFor: 'accessing'!scanForToken
	^self subclassResponsibility! !!SmaCCScanner methodsFor: 'private'!checkForKeyword: aString 
	| stateMap action |
	action := matchActions isSymbol 
				ifTrue: [matchActions]
				ifFalse: [matchActions first].
	stateMap := self class keywordMap at: action ifAbsent: [nil].
	stateMap isNil ifTrue: [^self].
	matchActions := stateMap at: (self keywordFor: aString)
				ifAbsent: [matchActions].
	matchActions isInteger 
		ifTrue: [matchActions := Array with: matchActions with: action]! !!SmaCCScanner methodsFor: 'private'!checkForValidMatch
	matchActions isNil ifTrue: [self scannerError]! !!SmaCCScanner methodsFor: 'private'!createTokenFor: string 
	| token |
	token := SmaCCToken 
				value: string
				start: start
				id: matchActions.
	outputStream reset.
	matchActions := nil.
	returnMatchBlock value: token! !!SmaCCScanner methodsFor: 'private'!initialBufferSize
	^128! !!SmaCCScanner methodsFor: 'private'!recordAndReportMatch: aCollection 
	self
		recordMatch: aCollection;
		reportLastMatch! !!SmaCCScanner methodsFor: 'private'!recordMatch: aCollection 
	matchActions := aCollection.
	matchEnd := stream position.
	lastOutputStreamMatchPosition := outputStream position! !!SmaCCScanner methodsFor: 'private'!reportLastMatch
	"The scanner has found the end of a token and must report it"

	| string |
	self checkForValidMatch.
	self resetOutputToLastMatch.
	stream position: matchEnd.
	string := outputStream contents.
	self checkForKeyword: string.
	matchActions isSymbol 
		ifTrue: [self perform: matchActions]
		ifFalse: [self createTokenFor: string]! !!SmaCCScanner methodsFor: 'private'!resetOutputToLastMatch
	outputStream position: lastOutputStreamMatchPosition.
	lastOutputStreamMatchPosition == 0 
		ifTrue: 
			[lastMatchWasEmpty ifTrue: [self scannerError].
			lastMatchWasEmpty := true]
		ifFalse: [lastMatchWasEmpty := false]! !!SmaCCScanner methodsFor: 'private'!resetScanner
	start := stream position.
	outputStream reset.
	lastOutputStreamMatchPosition := 0! !!SmaCCScanner methodsFor: 'private'!scannerError
	(stream atEnd and: [start == stream position]) 
		ifTrue: 
			[returnMatchBlock value: (SmaCCToken 
						value: ''
						start: stream position
						id: (Array with: self emptySymbolTokenId))].
	stream position: start.
	returnMatchBlock value: (SmaCCToken 
				value: (String with: stream next)
				start: start
				id: #(0))! !!SmaCCScanner methodsFor: 'private'!step
	stream atEnd ifTrue: [^self reportLastMatch].
	currentCharacter := stream next.
	outputStream nextPut: currentCharacter! !!SmaCCScanner methodsFor: 'private-utility'!keywordFor: aString 
	"Subclasses can override this to ignore case"

	^aString! !!SmaCCScanner class methodsFor: 'instance creation'!new
	^(super new)
		initialize;
		yourself! !!SmaCCScanner class methodsFor: 'instance creation'!on: aStream 
	^(self new)
		on: (self needsLineNumbers 
					ifTrue: [SmaCCLineNumberStream on: aStream]
					ifFalse: [aStream]);
		yourself! !!SmaCCScanner class methodsFor: 'testing'!needsLineNumbers
	"Redefine to return true, if you need line number information"

	^false! !!SmaCCScanner class methodsFor: 'accessing'!keywordMap
	keywordMap isNil ifTrue: [self initializeKeywordMap].
	^keywordMap! !!SmaCCScanner class methodsFor: 'class initialization'!initialize
	self initializeKeywordMap! !!SmaCCScanner class methodsFor: 'class initialization'!initializeKeywordMap
	keywordMap := Dictionary new! !!SmaCCToken methodsFor: 'printing'!printOn: aStream 
	aStream
		nextPut: ${;
		nextPutAll: self value;
		nextPut: $(;
		nextPutAll: self startPosition printString;
		nextPut: $,;
		nextPutAll: self stopPosition printString;
		nextPut: $,;
		nextPutAll: self id printString;
		nextPutAll: ')}'! !!SmaCCToken methodsFor: 'accessing'!id
	^id! !!SmaCCToken methodsFor: 'accessing'!startPosition
	^start + 1! !!SmaCCToken methodsFor: 'accessing'!stopPosition
	^start + value size! !!SmaCCToken methodsFor: 'accessing'!value
	^value! !!SmaCCToken methodsFor: 'initialize-release'!value: anObject start: startPositionInteger id: anInteger 
	value := anObject.
	start := startPositionInteger.
	id := anInteger! !!SmaCCToken class methodsFor: 'instance creation'!value: aString start: anInteger id: anObject 
	^(self new)
		value: aString
			start: anInteger
			id: anObject;
		yourself! !SmaCCScanner initialize!