'From Squeak3.1alpha of 5 February 2001 [latest update: #3783] on 5 March 2001 at 10:46:11 am'!"Change Set:		syntax12Date:			5 March 2001Author:			Bob ArningRefactored conversion of SyntaxMorph back to ST code.Fixed quirky message format (#collect: in new style syntax)"!!SyntaxMorph methodsFor: 'initialization' stamp: 'tk 1/19/2001 13:29'!sample: arg1"a comment"| temp1 |temp1 _ 5.temp1 yourself.temp1 min: arg1.! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/4/2001 12:19'!printBlockArgsNodeOn: strm indent: level	| argString |	self		submorphsDoIfSyntax: [ :sub |			(argString _ sub decompile) isEmpty ifFalse: [				strm 					nextPut: $:;					nextPutAll: argString;					space			].		] 		ifString: [ :sub |			"self printSimpleStringMorph: sub on: strm	<<<< do we need this??"		].	strm nextPut: $|; crtab: level.! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/5/2001 10:29'!printBlockNodeOn: strm indent: level	| lev inASyntaxButNotOutermost subNodeClass |	lev _ level.	inASyntaxButNotOutermost _ owner isSyntaxMorph and: [ owner isMethodNode not].	inASyntaxButNotOutermost ifTrue: [strm nextPut: $[.  lev _ lev+1].	self		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: lev.			subNodeClass _ sub parseNode class.			(subNodeClass == BlockArgsNode) | (subNodeClass == ReturnNode) ifFalse: [				strm nextPut: $.			].			subNodeClass == BlockArgsNode				ifTrue: [strm space]				ifFalse: [strm crtab: lev].		] 		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].	inASyntaxButNotOutermost ifTrue: [strm nextPut: $] ].! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/5/2001 10:29'!printCascadeNodeOn: strm indent: level	| parens cnt |	parens _ parseNode receiver notNil.	"do better??"	parens ifTrue: [strm nextPut: $( ].	cnt _ 0.	self		submorphsDoIfSyntax: [ :sub |			cnt _ cnt + 1.			"maybe we want to test sub isCascadePart for the following???"			cnt > 2 ifTrue: [strm nextPutAll: '; '].			sub printOn: strm indent: level.			strm ensureASpace.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].	parens ifTrue: [strm nextPut: $) ].! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/5/2001 10:29'!printMessageNodeOn: strm indent: level	| parens |	parens _ parseNode receiver notNil.	"do better??"	parens ifTrue: [strm nextPut: $( ].	self		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: level.			strm ensureASpace.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].	parens ifTrue: [strm nextPut: $) ].! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/4/2001 12:25'!printMethodNodeOn: strm indent: level	(self findA: SelectorNode) ifNil: [		(self getHeader: strm) ifFalse: [^ self]].		"might fail"	self 		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: level.			strm crtab: level.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		]. 	strm contents last "ugh!!" == $. ifTrue: [strm skip: -1].  "erase last period"! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/4/2001 12:08'!printMethodTempsNodeOn: strm indent: level	strm nextPut: $|; space.	self		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: level.			strm space.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].	strm nextPut: $|; crtab: level.! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/5/2001 10:30'!printOn: strm indent: level	| nodeClass |	(self hasProperty: #ignoreNodeWhenPrinting) ifFalse: [		nodeClass _ parseNode class.		nodeClass == BlockNode ifTrue: [^self printBlockNodeOn: strm indent: level].		nodeClass == BlockArgsNode ifTrue: [^self printBlockArgsNodeOn: strm indent: level].		nodeClass == MethodNode ifTrue: [^self printMethodNodeOn: strm indent: level].		nodeClass == MethodTempsNode ifTrue: [^self printMethodTempsNodeOn: strm indent: level].		nodeClass == VariableNode ifTrue: [^self printVariableNodeOn: strm indent: level].		nodeClass == MessageNode ifTrue: [^self printMessageNodeOn: strm indent: level].		nodeClass == CascadeNode ifTrue: [^self printCascadeNodeOn: strm indent: level].	].	self		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: level.			strm ensureASpace.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/5/2001 10:30'!printVariableNodeOn: strm indent: level	"nil out any old association"	parseNode key class == Association ifTrue: [		parseNode 			name: parseNode name 			key: nil 			code: parseNode code	].	self		submorphsDoIfSyntax: [ :sub |			sub printOn: strm indent: level.			strm ensureASpace.		]		ifString: [ :sub |			self printSimpleStringMorph: sub on: strm		].! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/4/2001 11:55'!submorphsDoIfSyntax: block1 ifString: block2 	^self submorphsDoIfSyntax: block1 ifString: block2 otherwise: [ :sub | ]! !!SyntaxMorph methodsFor: 'printing' stamp: 'RAA 3/4/2001 11:50'!submorphsDoIfSyntax: block1 ifString: block2 otherwise: block3	submorphs do: [ :sub |		sub isSyntaxMorph ifTrue: [			block1 value: sub		] ifFalse: [			(sub isKindOf: StringMorph) ifTrue: [				block2 value: sub			] ifFalse: [				block3 value: sub			].		].	].! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 3/5/2001 10:40'!blockNode: aNode arguments: arguments statements: statements	| row column |	column _ self addColumn: #block on: aNode.	self alansTest1 ifTrue: [column setProperty: #deselectedBorderColor toValue: self lighterColor].	column layoutInset: (self alansTest1 ifTrue: [1] ifFalse: [2@-1]).	aNode addCommentToMorph: column.	arguments size > 0 ifTrue: [		row _ column addRow: #blockarg1 on: (BlockArgsNode new).		row addNoiseString: self noiseBeforeBlockArg.		arguments do: [:arg | 			row addToken: arg name type: #blockarg2 on: arg		]	].	statements do: [ :each | 		(row _ each asMorphicSyntaxIn: column) borderWidth: 1.		self alansTest1 ifTrue: [			row setSpecialOuterTestFormat.		].		each addCommentToMorph: column	].	^ column! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 3/5/2001 09:54'!blockNodeCollect: aNode arguments: arguments statements: statements	| row column c2 r2 r3 |	column _ self addColumn: #blockCollectOnly on: aNode.	column layoutInset: (self alansTest1 ifTrue: [1] ifFalse: [2@-1]).	aNode addCommentToMorph: column.	arguments size > 0 ifTrue: [		row _ column addRow: #blockarg1 on: (BlockArgsNode new).		row addNoiseString: 'collect using' emphasis: 1.		r3 _ row addRow: #blockarg1b on: nil "aNode".		r3 setConditionalPartStyle.		arguments do: [:arg | 			r3 addToken: arg name type: #blockarg2 on: arg		]	].	r2 _ column addRow: #block on: aNode.	r2 setProperty: #ignoreNodeWhenPrinting toValue: true.	r2 addNoiseString: 'from' emphasis: 1.	c2 _ r2 addColumn: #block on: aNode.	c2 setProperty: #ignoreNodeWhenPrinting toValue: true.	statements do: [ :each | 		(each asMorphicSyntaxIn: c2) borderWidth: 1.		each addCommentToMorph: c2	].	^ column! !!WriteStream methodsFor: 'character writing' stamp: 'RAA 3/5/2001 10:26'!ensureASpace	"Append a space character to the receiver IFF there is not one on the end."	(position > 0 and: [(collection at: position) = Character space]) ifTrue: [^self].	self nextPut: Character space! !