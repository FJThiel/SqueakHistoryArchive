'From Squeak2.6 of 11 October 1999 [latest update: #1613] on 19 November 1999 at 12:14:02 pm'!"Change Set:		CleanerBracesDate:			18 November 1999Author:			Dan IngallsA general cleanup of Squeak's brace construct to eliminate messages sent to thisContext.The special treatment of	{x. y. z} as: CollectionTypehas been abandoned.  Things are simpler and faster without it.  Plus it was not used anywhere in the system except in BraceNode class example.The special array assignment feature exemplified by	x _ {1. {2. 3}. 4}.	{a. {b. c}. d. e} _ x, {5}.has also abandoned.  It was hard to understand and interacted poorly with tests for uninitialized variables.  Plus it was not used anywhere in the system except in BraceNode class example.Besides eliminating references to thisContext, the new brace package can compile brace constructs of indefinite length with only a two-cell overhead in stack space.  Also it is faster: 	Time millisecondsToRun: [1000 timesRepeat: [TetrisBoard basicNew blockInfo]]dropped from an original time of 2283 down to 737 after recompiling."!ParseNode subclass: #BraceNode	instanceVariableNames: 'elements sourceLocations collClassNode nElementsNode fromBraceStackNode toBraceStackNode withNode emitNode '	classVariableNames: ''	poolDictionaries: ''	category: 'System-Compiler'!!BraceNode commentStamp: 'di 11/19/1999 11:25' prior: 0!Used for compiling and decompiling brace constructs.These now compile into either a fast short form for 4 elements or less:	Array braceWith: a with: b ... or a long form of indefinfite length:	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.The erstwhile brace assignment form is no longer supported.!!BraceNode methodsFor: 'initialize-release' stamp: 'di 11/19/1999 11:06'!matchBraceStreamReceiver: receiver messages: messages	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])		ifFalse: [^ nil "no match"].	"Appears to be a long form brace construct"	self elements: (messages allButLast collect:		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)					ifFalse: [^ nil "not a brace element"].		msg arguments first])! !!BraceNode methodsFor: 'initialize-release' stamp: 'di 11/19/1999 11:19'!matchBraceWithReceiver: receiver selector: selector arguments: arguments	selector = (self selectorForShortForm: arguments size)		ifFalse: [^ nil "no match"].	"Appears to be a short form brace construct"	self elements: arguments! !!BraceNode methodsFor: 'code generation' stamp: 'di 11/19/1999 08:58'!emitForValue: stack on: aStream	^ emitNode emitForValue: stack on: aStream! !!BraceNode methodsFor: 'code generation' stamp: 'di 11/19/1999 11:12'!selectorForShortForm: nElements	^ #(braceWithNone braceWith: braceWith:with:			braceWith:with:with: braceWith:with:with:with:) at: nElements + 1! !!BraceNode methodsFor: 'code generation' stamp: 'di 11/19/1999 11:13'!sizeForValue: encoder	emitNode _ elements size <= 4		ifTrue: ["Short form: Array braceWith: a with: b ... "				MessageNode new					receiver: (encoder encodeVariable: #Array)					selector: (self selectorForShortForm: elements size)					arguments: elements precedence: 3 from: encoder]		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"				CascadeNode new					receiver: (MessageNode new								receiver: (encoder encodeVariable: #Array)								selector: #braceStream:								arguments: (Array with: (encoder encodeLiteral: elements size))								precedence: 3 from: encoder)					messages: ((elements collect: [:elt | MessageNode new receiver: nil														selector: #nextPut:														arguments: (Array with: elt)														precedence: 3 from: encoder])								copyWith: (MessageNode new receiver: nil														selector: #braceArray														arguments: (Array new)														precedence: 1 from: encoder))].	^ emitNode sizeForValue: encoder! !!BraceNode methodsFor: 'printing' stamp: 'di 11/19/1999 09:17'!printOn: aStream indent: level	aStream nextPut: ${.	1 to: elements size do: 		[:i | (elements at: i) printOn: aStream indent: level.		i < elements size ifTrue: [aStream nextPutAll: '. ']].	aStream nextPut: $}! !!BraceNode class methodsFor: 'examples' stamp: 'di 11/19/1999 09:05'!example	"Test the {a. b. c} syntax."	| x |	x _ {1. {2. 3}. 4}.	^ {x first. x second first. x second last. x last. 5} as: Set"BraceNode example Set (0 1 2 3 4 5 )"! !!Array class reorganize!('plugin generation' ccg:emitLoadFor:from:on: ccg:prolog:expr:index: ccgDeclareCForVar:)('brace support' braceStream: braceWith: braceWith:with: braceWith:with:with: braceWith:with:with:with: braceWithNone)!!Array class methodsFor: 'brace support' stamp: 'di 11/18/1999 22:53'!braceStream: nElements	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	^ WriteStream basicNew braceArray: (self new: nElements)! !!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:16'!braceWith: a	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	| array |	array _ self new: 1.	array at: 1 put: a.	^ array! !!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:15'!braceWith: a with: b 	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	| array |	array _ self new: 2.	array at: 1 put: a.	array at: 2 put: b.	^ array! !!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:17'!braceWith: a with: b with: c 	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	| array |	array _ self new: 3.	array at: 1 put: a.	array at: 2 put: b.	array at: 3 put: c.	^ array! !!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:17'!braceWith: a with: b with: c with: d	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	| array |	array _ self new: 4.	array at: 1 put: a.	array at: 2 put: b.	array at: 3 put: c.	array at: 4 put: d.	^ array! !!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:16'!braceWithNone	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	^ self new: 0! !!Decompiler methodsFor: 'instruction decoding' stamp: 'di 11/19/1999 10:59'!send: selector super: superFlag numArgs: numArgs	| args rcvr selNode msgNode messages |	args _ Array new: numArgs.	(numArgs to: 1 by: -1) do:		[:i | args at: i put: stack removeLast].	rcvr _ stack removeLast.	superFlag ifTrue: [rcvr _ constructor codeSuper].	(selector == #blockCopy: and: [self checkForBlock: rcvr])		ifFalse:			[selNode _ constructor codeAnySelector: selector.			rcvr == CascadeFlag				ifTrue:					[self willJumpIfFalse						ifTrue: "= generated by a case macro"							[selector ~= #= ifTrue: [self error: 'bad case: ', selector].							 statements addLast: args first.							 stack addLast: rcvr. "restore CascadeFlag"							 ^self]						ifFalse:							[msgNode _ constructor codeCascadedMessage: selNode arguments: args].					stack last == CascadeFlag						ifFalse:							["Last message of a cascade"							statements addLast: msgNode.							messages _ self popTo: stack removeLast.  "Depth saved by first dup"							msgNode _ constructor								codeCascade: stack removeLast								messages: messages]]				ifFalse:					[msgNode _ constructor								codeMessage: rcvr								selector: selNode								arguments: args].			stack addLast: msgNode]! !!DecompilerConstructor methodsFor: 'constructor' stamp: 'di 11/19/1999 11:06'!codeCascade: receiver messages: messages	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)		ifNil: [CascadeNode new receiver: receiver messages: messages]! !!DecompilerConstructor methodsFor: 'constructor' stamp: 'di 11/19/1999 11:20'!codeMessage: receiver selector: selector arguments: arguments	| symbol |	symbol _ selector key.	^ (BraceNode new matchBraceWithReceiver: receiver selector: symbol arguments: arguments)		ifNil: [MessageNode new receiver: receiver selector: selector					arguments: arguments					precedence: (symbol isInfix									ifTrue: [2]									ifFalse: [symbol isKeyword												ifTrue: [3]												ifFalse: [1]])]! !!MessageNode class methodsFor: 'class initialization' stamp: 'di 11/19/1999 11:30'!initialize		"MessageNode initialize"	MacroSelectors _ 		#(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:			and: or:			whileFalse: whileTrue: whileFalse whileTrue			to:do: to:by:do:			caseOf: caseOf:otherwise:).	MacroTransformers _ 		#(transformIfTrue: transformIfFalse: transformIfTrueIfFalse: transformIfFalseIfTrue:			transformAnd: transformOr:			transformWhile: transformWhile: transformWhile: transformWhile:			transformToDo: transformToDo:			transformCase: transformCase:).	MacroEmitters _ 		#(emitIf:on:value: emitIf:on:value: emitIf:on:value: emitIf:on:value:			emitIf:on:value: emitIf:on:value:			emitWhile:on:value: emitWhile:on:value: emitWhile:on:value: emitWhile:on:value:			emitToDo:on:value: emitToDo:on:value:			emitCase:on:value: emitCase:on:value:).	MacroSizers _ 		#(sizeIf:value: sizeIf:value: sizeIf:value: sizeIf:value:			sizeIf:value: sizeIf:value:			sizeWhile:value: sizeWhile:value: sizeWhile:value: sizeWhile:value:			sizeToDo:value: sizeToDo:value:			sizeCase:value: sizeCase:value:).	MacroPrinters _ 		#(printIfOn:indent: printIfOn:indent: printIfOn:indent: printIfOn:indent:			printIfOn:indent: printIfOn:indent:			printWhileOn:indent: printWhileOn:indent: printWhileOn:indent: printWhileOn:indent:			printToDoOn:indent: printToDoOn:indent:			printCaseOn:indent: printCaseOn:indent:)! !!Parser methodsFor: 'expression types' stamp: 'di 11/19/1999 07:43'!expression	(hereType == #word and: [tokenType == #leftArrow])		ifTrue: [^ self assignment: self variable].	hereType == #leftBrace		ifTrue: [self braceExpression]		ifFalse: [self primaryExpression ifFalse: [^ false]].	(self messagePart: 3 repeat: true)		ifTrue: [hereType == #semicolon ifTrue: [self cascade]].	^ true! !!WriteStream methodsFor: 'private' stamp: 'di 11/18/1999 22:55'!braceArray	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	^ collection! !!WriteStream methodsFor: 'private' stamp: 'di 11/18/1999 22:50'!braceArray: anArray	"This method is used in compilation of brace constructs.	It MUST NOT be deleted or altered."	collection _ anArray.	position _ 0.	readLimit _ 0.	writeLimit _ anArray size.! !BraceNode removeSelector: #matchBraceWithReceiver:arguments:!BraceNode removeSelector: #reverseDo:!BraceNode removeSelector: #emitStore:on:!BraceNode removeSelector: #matchCascadeReceiver:messages:!BraceNode removeSelector: #assignmentCheck:at:!BraceNode removeSelector: #collClass:!BraceNode removeSelector: #emitStorePop:on:!BraceNode removeSelector: #matchCascadeReceiver:messages:else:!BraceNode removeSelector: #do:!BraceNode removeSelector: #sizeForStore:!BraceNode removeSelector: #sizeForStorePop:!BraceNode class removeSelector: #test!Collection class removeSelector: #fromBraceStack:!Decompiler removeSelector: #formBrace!DecompilerConstructor removeSelector: #codeBrace:as:!DecompilerConstructor removeSelector: #codeBrace:fromBytes:!MessageNode removeSelector: #emitAs:on:value:!MessageNode removeSelector: #transformAs:!MessageNode removeSelector: #printAsOn:indent:!MessageNode removeSelector: #sizeAs:value:!MessageNode initialize!RunArray class removeSelector: #fromBraceStack:!Set class removeSelector: #fromBraceStack:!Dictionary class removeSelector: #fromBraceStack:!Smalltalk removeClassNamed: #BraceConstructor!ParseNode subclass: #BraceNode	instanceVariableNames: 'elements sourceLocations emitNode '	classVariableNames: ''	poolDictionaries: ''	category: 'System-Compiler'!