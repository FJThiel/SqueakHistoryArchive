'From Squeak 2.5 of August 6, 1999 [latest update: #1534] on 13 October 1999 at 10:06:25 am'!"Change Set:		selFinder-bfDate:			13 October 1999Author:			Bert Freudenberg* Makes the Selector Finder display code examples (data1 + data2) instead of just the selector in the message list* Fixes the bug where 'a'.'b'.'ab' didn't find anything* Fixes the bug where no binary selectors where found"!!ParagraphEditor methodsFor: 'menu messages' stamp: 'bf 10/13/1999 09:09'!selectedSelector	"Try to make a selector out of the current text selection"	^self selection string findSelector! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'bf 10/13/1999 08:32'!contents: aString notifying: aController	"Take what the user typed and find all selectors containing it"	| tokens |	contents _ aString.	classList _ #().  classListIndex _ 0.	selectorIndex _ 0.	tokens _ contents asString findTokens: ' .'.	selectorList _ Cursor wait showWhile: [		tokens size = 1 			ifTrue: [(Symbol selectorsContaining: contents asString) asSortedArray]			ifFalse: [self quickList]].	"find selectors from a single example of data"	self changed: #messageList.	self changed: #classList.	^ true! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'bf 10/13/1999 09:14'!messageListIndex: anInteger 	"Set the selected message selector to be the one indexed by anInteger.  Find all classes it is in."	selectorIndex _ anInteger.	selectorIndex = 0 ifFalse: [		classList _ Smalltalk allImplementorsOf: self selectedMessageName.		classListIndex _ 0.		self changed: #messageListIndex.		"update my selection"		self changed: #classList]! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'bf 10/13/1999 09:01'!quickList	"Compute the selectors for the single example of receiver and args, in the very top pane"	| data array ccc result resultArray |	ccc _ data _ contents asString.	"delete trailing period. This should be fixed in the Parser!!"	[data last isSeparator] whileTrue: [data _ data allButLast]. 	data last = $. ifTrue: [data _ data allButLast].	"Eval"	array _ Compiler evaluate: '{', data, '}'. "#( data1 data2 result )"	array size < 2 ifTrue: [self inform: 'If you are giving an example of receiver, \args, and result, please put periods between the parts.\Otherwise just type one selector fragment' withCRs. ^#()]. 	array _ Array with: array allButLast with: array last. "#( (data1 data2) result )" 	result _ MethodFinder methodFor: array.	resultArray _ (result findTokens: '(') collect:		[:s | s withBlanksTrimmed allButLast "remove $)"].	self selectorList: resultArray.	self contents: ccc.	"restore top pane, since MethodFinder clears it"	^ selectorList! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'bf 10/13/1999 10:02'!selectedMessageName	"Answer the name of the currently selected message."	| example |	selectorIndex = 0 ifTrue: [^nil].	example _ selectorList at: selectorIndex.	(example includes: $:) ifTrue: [^example findSelector].	Symbol hasInterned: (example findTokens: ' ') middle		ifTrue: [:aSymbol | ^ aSymbol].	self error: 'this should not happen'.	^nil! !!SequenceableCollection methodsFor: 'accessing' stamp: 'bf 10/13/1999 10:01'!middle	"Answer the middle element of the receiver."	self emptyCheck.	^ self at: self size // 2 + 1! !!String methodsFor: 'converting' stamp: 'bf 10/13/1999 09:26'!findSelector	"Dan's code for hunting down selectors with keyword parts; while this doesn't give a true parse, in most cases it does what we want, in where it doesn't, we're none the worse for it."	| sel possibleParens level n |	sel _ self withBlanksTrimmed.	(sel includes: $:) ifTrue:		[possibleParens _ sel findTokens: Character separators.		sel _ String streamContents:			[:s | level _ 0.			possibleParens do:				[:token |				(level = 0 and: [token endsWith: ':'])					ifTrue: [s nextPutAll: token]					ifFalse: [(n _ token occurrencesOf: $( ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $[ ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $] ) > 0 ifTrue: [level _ level - n].							(n _ token occurrencesOf: $) ) > 0 ifTrue: [level _ level - n]]]]].	sel isEmpty ifTrue: [^ nil].	Symbol hasInterned: sel ifTrue:		[:aSymbol | ^ aSymbol].	^ nil! !!Symbol class methodsFor: 'access' stamp: 'bf 10/13/1999 09:57'!selectorsContaining: aString	"Answer a list of selectors that contain aString within them.  Case-insensitive."	| size table candidate selectorList selectorTable ascii |	selectorList _ OrderedCollection new.	(size _ aString size) = 0 ifTrue: [^ selectorList].	aString size = 1 ifTrue:		[ascii _ aString first asciiValue.		ascii < 128 ifTrue:			[selectorList add: (SingleCharSymbols at: ascii + 1)]].	aString first isLetter ifFalse: [		aString size == 2 ifTrue: 			[Symbol hasInterned: aString ifTrue: [:s | selectorList add: s]].		^ selectorList].	(SelectorTables size to: 1 by: -1) do:		[:j | selectorTable _ SelectorTables at: j.		1 to: 26 do: [:index |		table _ selectorTable at: index.		1 to: table size do: 			[:t | 			((candidate _ table at: t) == nil) ifFalse:				[candidate size >= size ifTrue:					[((candidate findString: aString startingAt: 1 caseSensitive: false) > 0) ifTrue:							[selectorList add: candidate]]]]]].	^ selectorList"Symbol selectorsContaining: 'scon' "! !