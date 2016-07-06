'From Squeak2.9alpha of 13 June 2000 [latest update: #2915] on 12 November 2000 at 5:48:41 pm'!"Change Set:		243NewSymbolTable5-sqrDate:			24 August 2000Author:			Andres ValloudRemove code using old symbol tables.Removed otherThatStarts:skipping: because there were no senders.Added do:after: to WeakSet"!!Symbol class methodsFor: 'access' stamp: 'SqR 8/30/2000 13:48'!morePossibleSelectorsFor: misspelled	"Answer an ordered collection of possible corrections	for the misspelled selector in order of likelyhood.	Hunt over a larger amount of selectors"	| numArgs table lookupString list binary key |	lookupString _ misspelled asLowercase. "correct uppercase selectors to lowercase"	numArgs _ lookupString numArgs.	numArgs < 0 ifTrue: [^OrderedCollection new: 0].	key _ lookupString at: 1.	table _ OrderedCollection new.	SymbolTable do:		[:each |			each == #'' ifFalse: [(each at: 1) == key ifFalse:				[each numArgs = numArgs ifTrue: [table add: each]]]		].	list _ lookupString correctAgainst: table.	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [		binary _ misspelled, ':'.		"try for missing colon"		Symbol hasInterned: binary ifTrue: [:him | list addFirst: him]].	^ list! !!Symbol class methodsFor: 'access' stamp: 'SqR 8/30/2000 13:45'!possibleSelectorsFor: misspelled	"Answer an ordered collection of possible corrections	for the misspelled selector in order of likelyhood"	| numArgs table lookupString list binary key |	lookupString _ misspelled asLowercase. "correct uppercase selectors to lowercase"	numArgs _ lookupString numArgs.	numArgs < 0 ifTrue: [^OrderedCollection new: 0].	key _ lookupString at: 1.	table _ OrderedCollection new.	SymbolTable do:		[:each |			each == #'' ifFalse: [(each at: 1) == key ifTrue:				[each numArgs = numArgs ifTrue: [table add: each]]]		].	list _ lookupString correctAgainst: table.	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [		binary _ misspelled, ':'.		"try for missing colon"		Symbol hasInterned: binary ifTrue: [:him | list addFirst: him]].	^ list! !!Symbol class methodsFor: 'access' stamp: 'SqR 8/30/2000 13:14'!selectorsContaining: aString	"Answer a list of selectors that contain aString within them. Case-insensitive"	| size selectorList ascii |	selectorList _ OrderedCollection new.	(size _ aString size) = 0 ifTrue: [^selectorList].	aString size = 1 ifTrue:		[			ascii _ aString first asciiValue.			ascii < 128 ifTrue: [selectorList add: (SymbolTable like: aString)]		].	aString first isLetter ifFalse:		[			aString size == 2 ifTrue: 				[Symbol hasInterned: aString ifTrue:					[:s | selectorList add: s]].			^selectorList		].	selectorList _ selectorList copyFrom: 2 to: selectorList size.	SymbolTable do:		[:each |			each size >= size ifTrue:				[					((each at: 1) isLowercase and:						[((each findString: aString startingAt: 1 caseSensitive: false) > 0)])							ifTrue: [selectorList add: each]				]		].	^selectorList"Symbol selectorsContaining: 'scon'"! !!Symbol class methodsFor: 'access' stamp: 'SqR 8/30/2000 13:39'!thatDoesNotStart: leadingCharacters skipping: skipSym	"Use comments from thatStarts:skipping:.	Ignore first character and try to match the rest"	| size firstMatch key |	size _ leadingCharacters size.	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].	firstMatch _ leadingCharacters at: 1.	size > 1 ifTrue: [key _ leadingCharacters copyFrom: 2 to: size].	SymbolTable do:		[:each |			each size >= size ifTrue:				[					((each at: 1) ~~ firstMatch and:						[key == nil or:							[(each findString: key startingAt: 2 caseSensitive: false) = 2]])								ifTrue: [^each]				]		] after: skipSym.	^nil"Symbol thatStarts: 'sf' skipping: nil""Symbol thatStarts: 'sf' skipping: #sfpGetFile:with:with:with:with:with:with:with:with:""Symbol thatStarts: 'candidate' skipping: nil"! !!Symbol class methodsFor: 'access' stamp: 'SqR 8/30/2000 13:34'!thatStarts: leadingCharacters skipping: skipSym	"Answer a selector symbol that starts with leadingCharacters.	Symbols beginning with a lower-case letter handled directly here.	Ignore case after first char.	If skipSym is not nil, it is a previous answer; start searching after it.	If no symbols are found, answer nil.	Used by Alt-q (Command-q) routines"	| size firstMatch key |	size _ leadingCharacters size.	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].	firstMatch _ leadingCharacters at: 1.	size > 1 ifTrue: [key _ leadingCharacters copyFrom: 2 to: size].	SymbolTable do:		[:each |			each size >= size ifTrue:				[					((each at: 1) == firstMatch and:						[key == nil or:							[(each findString: key startingAt: 2 caseSensitive: false) = 2]])								ifTrue: [^each]				]		] after: skipSym.	^nil"Symbol thatStarts: 'sf' skipping: nil""Symbol thatStarts: 'sf' skipping: #sfpGetFile:with:with:with:with:with:with:with:with:""Symbol thatStarts: 'candidate' skipping: nil"! !!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:15'!add: newObject	"Include newObject as one of the receiver's elements, but only if	not already present. Answer newObject"	| index |	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].	index _ self findElementOrNil: newObject.	((array at: index) == flag or: [(array at: index) isNil])		ifTrue: [self atNewIndex: index put: newObject].	^newObject! !!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:13'!do: aBlock after: anElement	| each startIndex |	tally = 0 ifTrue: [^self].	startIndex _ anElement ifNil: [1] ifNotNil:		[self findElementOrNil: anElement].	startIndex + 1 to: array size do:		[:index |			((each _ array at: index) == nil or: [each == flag])				ifFalse: [aBlock value: each]		]! !!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:15'!includes: anObject 	^(array at: (self findElementOrNil: anObject)) ~~ flag! !Symbol class removeSelector: #otherThatStarts:skipping:!