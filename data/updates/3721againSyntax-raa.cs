'From Squeak3.1alpha of 5 February 2001 [latest update: #3731] on 26 February 2001 at 2:45:19 pm'!"Change Set:		againSyntaxDate:			26 February 2001Author:			Bob Arninganother round of revisions"!PasteUpMorph subclass: #SyntaxTestMethods	instanceVariableNames: 'letterActors wild leftMargin rightMargin switch current jumpSwitch hotIndex '	classVariableNames: 'Goal '	poolDictionaries: ''	category: 'Morphic-Tile Scriptors'!!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 14:37'!addSingleKeywordRow: aStringLikeItem	| row sMorph modifiedString fontToUse |	self flag: #emphasis:.	"who needs it. who doesn't"	(row _ self class row: #text on: nil) borderWidth: 1.	modifiedString _ self substituteKeywordFor: aStringLikeItem.	sMorph _ self addString: modifiedString.	fontToUse _ self fontToUseForSpecialWord: modifiedString.	sMorph 		font: fontToUse "emphasis: 1";		color: (self colorForKeywords: aStringLikeItem and: modifiedString);		setProperty: #syntacticallyCorrectContents toValue: aStringLikeItem.	row addMorph: sMorph.	self addMorphBack: row.	^row! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 14:37'!addSingleKeywordRow: aStringLikeItem style: aSymbol	| row sMorph modifiedString fontToUse |	self flag: #emphasis:.	"who needs it. who doesn't"	(row _ self class row: #text on: nil) borderWidth: 1.	modifiedString _ self substituteKeywordFor: aStringLikeItem.	sMorph _ self addString: modifiedString.	fontToUse _ self fontToUseForSpecialWord: modifiedString.	sMorph 		font: fontToUse "emphasis: 1";		color: (self colorForKeywords: aStringLikeItem and: modifiedString);		setProperty: #syntacticallyCorrectContents toValue: aStringLikeItem.	aSymbol == #unary ifTrue: [sMorph color: self colorForUnaries].	row addMorph: sMorph.	self addMorphBack: row.	^row! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/26/2001 14:37'!addToken: aString type: aColorOrSymbol on: aNode	| sMorph modifiedString noiseWord col |	self flag: #emphasis:.	"who needs it. who doesn't"	noiseWord _ nil.	sMorph _ self addString: aString.	self specialBlockFormatting ifTrue: [		(aColorOrSymbol == #keyword2) | (aColorOrSymbol == #upArrow) ifTrue: [			modifiedString _ self substituteKeywordFor: aString.			sMorph 				font: (self fontToUseForSpecialWord: modifiedString) "emphasis: 1"; 				color: (self colorForKeywords: aString and: modifiedString);				setProperty: #syntacticallyCorrectContents toValue: aString;				contents: modifiedString.		].		aColorOrSymbol == #binary ifTrue: [sMorph color: self colorForBinaries].	].	self alansTest1 ifTrue: [		aColorOrSymbol == #blockarg1 ifTrue: [			noiseWord _ nil.		].		(aColorOrSymbol == #variable or: [aColorOrSymbol == #tempVariable]) ifTrue: [			aString = 'self' ifTrue: [				sMorph setProperty: #wordyVariantOfSelf toValue: true.			].			noiseWord _ self noiseWordBeforeVariableNode: aNode string: aString.		].	].	col _ (self addRow: aColorOrSymbol on: aNode)		layoutInset: 1.	noiseWord ifNotNil: [		col 			addMorphBack: (self noiseStringMorph: noiseWord);			addMorphBack: (self transparentSpacerOfSize: 3@1)	].	col addMorphBack: sMorph.	^col! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/26/2001 14:37'!addTokenSpecialCase: aString type: aColorOrSymbol on: aNode	| sMorph modifiedString noiseWord col |	self flag: #emphasis:.	"who needs it. who doesn't"	noiseWord _ nil.	sMorph _ self addString: aString.	self specialBlockFormatting ifTrue: [		(aColorOrSymbol == #keyword2) ifTrue: [			modifiedString _ aString = 'if:' ifTrue: ['Test'] ifFalse: ['Yes'].			sMorph 				font: (self fontToUseForSpecialWord: modifiedString) "emphasis: 1"; 				color: (self colorForKeywords: aString and: modifiedString);				setProperty: #syntacticallyCorrectContents toValue: aString;				contents: modifiedString.		].	]."Smalltalk at: #Q put: OrderedCollection new"	self alansTest1 ifTrue: [		(aColorOrSymbol == #variable or: [aColorOrSymbol == #tempVariable]) ifTrue: [			aString = 'self' ifTrue: [				sMorph setProperty: #wordyVariantOfSelf toValue: true.			].			noiseWord _ self noiseWordBeforeVariableNode: aNode string: aString.		].	].	col _ (self addRow: aColorOrSymbol on: aNode)		layoutInset: 1.	noiseWord ifNotNil: [		col 			addMorphBack: (self noiseStringMorph: noiseWord);			addMorphBack: (self transparentSpacerOfSize: 3@1)	].	col addMorphBack: sMorph.	^col! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/26/2001 14:04'!finalAppearanceTweaks	| deletes |	SizeScaleFactor ifNil: [SizeScaleFactor _ 1.0]. 	self usingClassicTiles ifTrue: [		self allMorphsDo: [:each | 			(each isKindOf: SyntaxMorph) ifTrue: [each lookClassic]		].		^self	].	deletes _ OrderedCollection new.	self allMorphsDo: [ :each |		"(each hasProperty: #tokenVerticalSeparator) ifTrue: [			each == each owner lastSubmorph ifTrue: [deletes add: each].		]."		(each respondsTo: #setDeselectedColor) ifTrue: [each setDeselectedColor].		(each hasProperty: #variableInsetSize) ifTrue: [			each layoutInset: 				((each valueOfProperty: #variableInsetSize) * SizeScaleFactor) rounded.		].	].	deletes do: [ :each | each delete].! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 14:22'!alanBinaryPostRcvr: aNode key: key args: args	| nodeWithNilReceiver row |"==Repeat for collection [ collect ( from foo. blah blah foo blah) ]Repeat for 1 to 50� [ do� ( from i. blah blab i blah� )� ]=="	nodeWithNilReceiver _ aNode copy receiver: nil.	(row _ self addRow: #keyword2 on: nodeWithNilReceiver)		borderWidth: 1;		parseNode: (nodeWithNilReceiver as: MessageNode);		borderColor: row stdBorderColor.	row addToken: key asString		type: #binary		on: (SelectorNode new key: key asString code: nil "fill this in?").	args first asMorphicSyntaxIn: row.! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 12:37'!alanKeywordMessage: aNode isAConditional: template key: key args: args	| nodeWithNilReceiver column keywords row onlyOne |"==Repeat for collection [ collect ( from foo. blah blah foo blah) ]Repeat for 1 to 50� [ do� ( from i. blah blab i blah� )� ]=="	nodeWithNilReceiver _ aNode copy receiver: nil.	template = 1 ifTrue: [		self listDirection: #topToBottom.	].	column _ self addColumn: #keyword1 on: nodeWithNilReceiver.	keywords _ key keywords.	onlyOne _ args size = 1.	keywords		with: (args first: keywords size)		do: [:kwd :arg |			template = 1 ifTrue: [				column addMorphBack: (column transparentSpacerOfSize: 3@3).			].			(row _ column addRow: #keyword2 on: nodeWithNilReceiver) borderWidth: 1;				parseNode: (nodeWithNilReceiver as: 						(onlyOne ifTrue: [MessageNode] ifFalse: [MessagePartNode]));				 borderColor: row stdBorderColor.			template = 1 ifTrue: [row addMorphBack: (row transparentSpacerOfSize: 20@6)].			key == #if:do: ifTrue: [				kwd = 'do:' ifTrue: [					row addMorphBack: (row transparentSpacerOfSize: 26@6).				] ifFalse: [					row addMorphBack: (row transparentSpacerOfSize: 10@6).				].				row addTokenSpecialCase: kwd					type: #keyword2					on: KeyWordNode new.			] ifFalse: [				row addToken: kwd					type: #keyword2					on: (onlyOne ifTrue: [SelectorNode new key: kwd code: nil "fill this in?"]									ifFalse: [KeyWordNode new]).				template = 0 ifTrue: [					row addMorphBack: self tokenVerticalSeparator				].			].			arg asMorphicSyntaxIn: row.		].	onlyOne ifTrue: [		self replaceSubmorph: column by: row.		column _ row.	].	template = 2 ifTrue: [		column setSpecialOuterTestFormat.	].			! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 14:18'!alansMessageNode: aNode receiver: receiver selector: selector keywords: key arguments: args	| row receiverMorph testAndReceiver anotherSelf 		wordyMorph template |	template _ self alansTemplateStyleFor: key.	receiver ifNotNil: ["i.e. not a cascade"		anotherSelf _ self constructSelfVariant: receiver and: key.		anotherSelf ifNotNil: [			wordyMorph _ self addString: anotherSelf.			wordyMorph setProperty: #wordyVariantOfSelf toValue: true.			self addMorph: wordyMorph. 			self layoutInset: 1.			^self		].		testAndReceiver _ self.		template = 1 ifTrue: [			testAndReceiver _ self addRow: #keyword1 on: nil.			self setSpecialOuterTestFormat.			testAndReceiver addNoiseString: 'Test'		].		template = 2 ifTrue: [			testAndReceiver _ self addRow: #keyword1 on: nil.			"self setSpecialOuterTestFormat."			testAndReceiver addNoiseString: 'Repeat for'		].		receiverMorph _ receiver asMorphicSyntaxIn: testAndReceiver.		template = 1 ifTrue: [self setConditionalPartStyle: receiverMorph].	].	"unary mssages"	args size = 0 ifTrue: [		row _ (self addSingleKeywordRow: key style: #unary) layoutInset: 1.		^ row parseNode: selector	].	"binary messages"	key last = $: ifFalse: [		^self alanBinaryPostRcvr: aNode key: key args: args	].	"keyword messages"	self		alanKeywordMessage: aNode 		isAConditional: template 		key: key 		args: args! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 12:57'!blockNode: aNode arguments: arguments statements: statements	| row column |	(column _ self addColumn: #block on: aNode) layoutInset: 2@-1.	aNode addCommentToMorph: column.	arguments size > 0 ifTrue: [		row _ column addRow: #blockarg1 on: (BlockArgsNode new).		row addNoiseString: self noiseBeforeBlockArg.		arguments do: [:arg | 			row addToken: arg name type: #blockarg2 on: arg		]	].	statements do: [ :each | 		(each asMorphicSyntaxIn: column) borderWidth: 1.		each addCommentToMorph: column	].	^ column! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 11:55'!messageNode: aNode receiver: receiver selector: selector keywords: key arguments: args	| keywords column row receiverMorph receiverWidth		messageWidth onlyOne nodeWithNilReceiver isAConditional  |	self alansTest1 ifTrue: [		^self			alansMessageNode: aNode 			receiver: receiver 			selector: selector 			keywords: key 			arguments: args	].	isAConditional _ #(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:) includes: key.	receiver ifNotNil: ["i.e. not a cascade"		receiverMorph _ receiver asMorphicSyntaxIn: self.	].	keywords _ key keywords.	args size = 0 ifTrue: [		row _ (self addSingleKeywordRow: key) layoutInset: 1.		^ row parseNode: selector	].	receiverWidth _ receiver				ifNil: [0]				ifNotNil: [receiverMorph fullBounds width].	onlyOne _ args size = 1.	(receiverWidth <= 80 and: [onlyOne]) ifTrue: [		self messageOneArg: key receiver: receiver selector: selector args: args.		^self	].	nodeWithNilReceiver _ aNode copy receiver: nil.	column _ self addColumn: #keyword1 on: nodeWithNilReceiver.	"onlyOne ifTrue: [column parseNode: nil].	is a spacer"	messageWidth _ 0.	keywords		with: (args copyFrom: 1 to: keywords size)		do: [:kwd :arg |			isAConditional ifTrue: [				column addMorphBack: (column transparentSpacerOfSize: 3@3).			].			(row _ column addRow: #keyword2 on: nodeWithNilReceiver) borderWidth: 1;				parseNode: (nodeWithNilReceiver as: 						(onlyOne ifTrue: [MessageNode] ifFalse: [MessagePartNode]));				 borderColor: row stdBorderColor.			isAConditional ifTrue: [row addMorphBack: (row transparentSpacerOfSize: 20@6)].			row addToken: kwd				type: #keyword2				on: (onlyOne ifTrue: [SelectorNode new key: kwd code: nil "fill this in?"]								ifFalse: [KeyWordNode new]).			arg asMorphicSyntaxIn: row.			messageWidth _ messageWidth + row fullBounds width].	onlyOne ifTrue: [self replaceSubmorph: column by: row.  column _ row].	receiverMorph ifNil: [^self].	receiverWidth + messageWidth < 350 ifTrue: [		isAConditional ifFalse: [self unfoldMessage].		^self	].	((receiverWidth > 200						or: [receiverWidth > 80								and: [column fullBounds height > 20]])					or: [receiverMorph fullBounds width > 30							and: [column fullBounds height > 100									or: [column fullBounds width > 250]]])				ifTrue: [^ self foldMessage]! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 14:04'!messageOneArg: key receiver: receiver selector: selector args: args	| row firstArgMorph |	row _ (self addSingleKeywordRow: key) layoutInset: 1.	row parseNode: selector.	firstArgMorph _ args first asMorphicSyntaxIn: self.	receiver ifNil: [^ self].	(firstArgMorph fullBounds height > 100			or: [firstArgMorph fullBounds width > 250])		ifTrue: [self foldMessageOneArg].! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:38'!alansTemplateStyleFor: key	(#(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:) includes: key) ifTrue: [^1].	(#(do: collect:) includes: key) ifTrue: [^2].	(#(if:do:) includes: key) ifTrue: [^3].	^0! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:38'!brownishColor	^Color lightBrown lighter lighter.! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 14:29'!colorForBinaries	^(Color r: 0.333 g: 0.333 b: 0.333)! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 14:06'!colorForKeywords: original and: modified	(#('ifTrue:' 'ifFalse:' 'Test' 'my') includes: original asString) ifTrue: [		^Color black	].	(original size > 0 and: [original last = $:]) ifFalse: [		^Color gray	].	^(Color r: 0.333 g: 0.333 b: 0.333)! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 14:29'!colorForUnaries	^(Color r: 0.666 g: 0.666 b: 0.666)! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:39'!constructSelfVariant: receiver and: key	| wordy |	(receiver isKindOf: VariableNode) ifFalse: [^nil].	receiver name = 'self'  ifFalse: [^nil].	(wordy _ self translateFromWordySelfVariant: key) ifNil: [^nil].	^wordy! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:40'!fontToUseForSpecialWord: aString	^(#('Yes' 'No' 'Test') includes: aString) ifTrue: [		(StrikeFont familyName: 'Helvetica' size: 14)	] ifFalse: [		nil	]! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:41'!lightGreenishColor	^Color paleGreen! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:41'!noiseBeforeBlockArg	^'from'! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 14:37'!noiseStringMorph: aNoiseString	| sMorph |	self flag: #emphasis:.	"who needs it. who doesn't"	sMorph _ StringMorph contents: aNoiseString.	sMorph 		font: (self fontToUseForSpecialWord: aNoiseString) "emphasis: 1"; 		color: (self colorForKeywords: aNoiseString and: aNoiseString);		setProperty: #noiseWord toValue: true.	^sMorph! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:41'!noiseWordBeforeVariableNode: aNode string: aString	(#('self' 'nil') includes: aString) ifFalse: [		aNode type < 4 ifTrue: [^'my']	].	^nil! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:42'!setConditionalPartStyle: aMorph	| c |	c _ self lightGreenishColor.	aMorph specialColor: c andBorder: c."old border (Color r: 0.581 g: 0.774 b: 0.903)"	aMorph useRoundedCorners.	aMorph borderWidth: 1.! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:43'!setSpecialOuterTestFormat	| brownishColor |	brownishColor _ self brownishColor.	self 		specialColor: brownishColor 		andBorder: brownishColor.	"self 		specialColor: (Color r: 1.0 g: 0.935 b: 0.774) 		andBorder: (Color r: 0.581 g: 0.774 b: 0.903)."	self useRoundedCorners.	self layoutInset: 6.	self setProperty: #variableInsetSize toValue: 6.! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:43'!setSpecialTempDeclarationFormat1	| lightBrown |	"the outer template for temp defs"	lightBrown _ self brownishColor.	self 		specialColor: lightBrown 		andBorder: lightBrown.	"self 		specialColor: (Color lightYellow) 		andBorder: (Color r: 0.581 g: 0.774 b: 0.903)."	self useRoundedCorners.	self layoutInset: 1.	self cellPositioning: #center.	"self setProperty: #variableInsetSize toValue: 6."! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:43'!setSpecialTempDeclarationFormat2	| pg |	"the inner template for temp defs"	pg _self lightGreenishColor.	self 		specialColor: pg 		andBorder:  pg.	"self 		specialColor: (Color r: 1.0 g: 1.0 b: 0.548) 		andBorder:  (Color r: 0.581 g: 0.774 b: 0.903)."	self useRoundedCorners.	self layoutInset: 1.	"self setProperty: #variableInsetSize toValue: 6."! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:44'!specialColor: c1 andBorder: c2	self color: (self scaleColorByUserPref: c1).	self setProperty: #deselectedColor toValue: c1.	self borderColor: (self scaleColorByUserPref: c2).	self setProperty: #deselectedBorderColor toValue: c2.! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:44'!substituteKeywordFor: aString	aString asString = '^ ' ifTrue: [^'answer'].	aString asString = 'ifTrue:' ifTrue: [^'Yes'].	aString asString = 'ifFalse:' ifTrue: [^'No'].	aString asString = 'self' ifTrue: [^'self'].	^String streamContents: [ :strm |		aString do: [ :each | 			each = $: ifFalse: [				each isUppercase ifTrue: [strm space; nextPut: each asLowercase]					ifFalse: [strm nextPut: each]			].		]	].! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:45'!tokenVerticalSeparator	^RectangleMorph new		vResizing: #spaceFill;		setProperty: #tokenVerticalSeparator toValue: #true;		extent: 3@15;		borderWidth: 1;		borderColor: Color transparent;		color: (Color r: 0.581 g: 0.774 b: 0.903)! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:45'!translateFromWordySelfVariant: key	#selfWrittenAsMe == key ifTrue: [^'me'].	#selfWrittenAsMy == key ifTrue: [^'my'].	#selfWrittenAsIll == key ifTrue: [^'I''ll'].	#selfWrittenAsIm == key ifTrue: [^'I''m'].	#selfWrittenAsThis == key ifTrue: [^'this'].	^nil! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 2/26/2001 13:45'!translateToWordySelfVariant: aString	| lc |	lc _ aString asLowercase.	lc = 'me' ifTrue: [^#selfWrittenAsMe].	lc = 'my' ifTrue: [^#selfWrittenAsMy].	lc = 'i''ll' ifTrue: [^#selfWrittenAsIll].	lc = 'i''m' ifTrue: [^#selfWrittenAsIm].	lc = 'this' ifTrue: [^#selfWrittenAsThis].	^nil! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/26/2001 11:27'!doAndCollect	self do: [ :j | j isEmpty ifFalse: [j size]].	self collect: [ :each | each asString withBlanksTrimmed].	! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/26/2001 14:00'!makeRandomString	| newString foo |	newString _ String new: Goal contents size.	^newString collect: [ :oldLetter | 'abcdefghijklmnopqrstuvwxyz' atRandom]! !SyntaxMorph removeSelector: #alanMsg1PostRcvr:isAConditional:key:args:!