'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 5 February 2001 update 3892] on 4 April 2001 at 1:16:11 pm'!"Change Set:		settersAndGettersDate:			4 April 2001Author:			Bob ArningFurther refinement of setters and getters in universal tiles- setters written in ST as (foo setX: 0) will be translated as (foo's x _ 0)- getters written in ST as (foo getX) will be written as (foo's x)"!!SyntaxMorph methodsFor: 'accessing' stamp: 'RAA 4/4/2001 12:36'!cleanUpString: stringSubMorph	| style rawData |	^ stringSubMorph 		valueOfProperty: #syntacticallyCorrectContents 		ifAbsent: [			style _ stringSubMorph valueOfProperty: #syntacticReformatting.			rawData _ stringSubMorph contents.			 (#(unary tempVariableDeclaration blockarg2 methodHeader1 tempVariable variable) includes: style) ifTrue: [				rawData _ self unSpaceAndUpShift: rawData appending: nil.			].			style == #keywordGetz ifTrue: [				rawData _ self unSpaceAndUpShift: rawData appending: 'Getz:'.			].			style == #keywordSetter ifTrue: [				rawData _ self unSpaceAndUpShift: 'set ',rawData appending: ':'.			].			style == #unaryGetter ifTrue: [				rawData _ self unSpaceAndUpShift: 'get ',rawData appending: nil.			].			(#(keyword2 methodHeader2) includes: style)  ifTrue: [				rawData _ self unSpaceAndUpShift: rawData appending: ':'.			].			rawData		]! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 4/4/2001 13:15'!addString: literalOrVarName special: aBoolean	| answer |	"Create and return an UpdatingStringMorph containing the value.  Use an UpdatingStringMorph, so it can inform its owner when it has been edited. Keep the getSelector being nil"	answer _ (self anUpdatingStringMorphWith: literalOrVarName special: aBoolean)		target: self;		putSelector: #acceptIgnoring:;		useStringFormat.	^answer! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 4/4/2001 13:15'!addToken: aString type: aColorOrSymbol on: aNode	| sMorph modifiedString noiseWord col |	col _ (self addRow: aColorOrSymbol on: aNode) layoutInset: 1.	self alansTest1 ifFalse: [		sMorph _ self addString: aString special: false.		col addMorphBack: sMorph.		^col	].	noiseWord _ [ :w |		w ifNotNil: [			col 				addMorphBack: (self noiseStringMorph: w);				addMorphBack: (self tokenVerticalSeparator)		].	].	(self shouldBeBrokenIntoWords: aColorOrSymbol) ifTrue: [		modifiedString _ self substituteKeywordFor: aString.		sMorph _ self addString: modifiedString special: (#(unary keywordGetz keywordSetter unaryGetter) includes: aColorOrSymbol).		sMorph 			setProperty: #syntacticReformatting toValue: aColorOrSymbol;			setProperty: #syntacticallyCorrectContents toValue: aString;			contents: modifiedString.	] ifFalse: [		sMorph _ self addString: (modifiedString _ aString) special: false.	].	(#(keyword2 upArrow) includes: aColorOrSymbol) ifTrue: [		sMorph 			font: (self fontToUseForSpecialWord: modifiedString).	].	(#(keyword2 unary assignmentArrow methodHeader1 methodHeader2) includes: aColorOrSymbol) ifTrue: [		sMorph emphasis: 1.	].	aColorOrSymbol == #blockarg1 ifTrue: [	].	(aColorOrSymbol == #variable or: [aColorOrSymbol == #tempVariable]) ifTrue: [		aString = 'self' ifTrue: [			sMorph setProperty: #wordyVariantOfSelf toValue: true.		].		noiseWord value: (self noiseWordBeforeVariableNode: aNode string: aString).	].	col addMorphBack: sMorph.	^col! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 12:48'!alanKeywordMessage: aNode isAConditional: template key: key args: args	| nodeWithNilReceiver column keywords row onlyOne |	(key == #collect: and: [args first isKindOf: BlockNode]) ifTrue: [		^self			alanKwdCollect: aNode 			isAConditional: template 			key: key 			args: args	].	key == #repeatFor:doing: ifTrue: [		^self			alanKwdRepeatForDoing: aNode 			isAConditional: template 			key: key 			args: args	].	key == #if:do: ifTrue: [		^self			alanKwdIfDo: aNode 			isAConditional: template 			key: key 			args: args	].	(args size = 1 and: [key endsWith: 'Getz:']) ifTrue: [		^self			alanKwdSetter: aNode 			isAConditional: 0 			key: key 			args: args	].	(args size = 1 and: [self isStandardSetterKeyword: key]) ifTrue: [		^self			alanKwdSetter2: aNode 			isAConditional: 0 			key: key 			args: args	].	nodeWithNilReceiver _ aNode copy receiver: nil.	template = 1 ifTrue: [		self listDirection: #topToBottom.	].	column _ self addColumn: #keyword1 on: nodeWithNilReceiver.	keywords _ key keywords.	onlyOne _ args size = 1.	keywords		with: (args first: keywords size)		do: [:kwd :arg |			template = 1 ifTrue: [				column addMorphBack: (column transparentSpacerOfSize: 3@3).			].			(row _ column addRow: #keyword2 on: nodeWithNilReceiver)				parseNode: (nodeWithNilReceiver as: 						(onlyOne ifTrue: [MessageNode] ifFalse: [MessagePartNode]));				 borderColor: row stdBorderColor.			template = 1 ifTrue: [row addMorphBack: (row transparentSpacerOfSize: 20@6)].			row addToken: kwd				type: #keyword2				on: (onlyOne ifTrue: [SelectorNode new key: kwd code: nil "fill this in?"]								ifFalse: [KeyWordNode new]).			(arg asMorphicSyntaxIn: row) setConditionalPartStyle.		].	onlyOne ifTrue: [		self replaceSubmorph: column by: row.		column _ row.	].			! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 13:13'!alanKwdSetter2: aNode isAConditional: template key: key args: args	"translates		foo setHeading: 0	to		foo's heading _ 0	"	| nodeWithNilReceiver row kwdHolder |	nodeWithNilReceiver _ aNode copy receiver: nil.	(row _ self addRow: #keyword2 on: nodeWithNilReceiver)		borderWidth: 1;		parseNode: (nodeWithNilReceiver as: MessageNode);		borderColor: row stdBorderColor.	row addNoiseString: '''s' emphasis: 1.	kwdHolder _ row		addToken: key		type: #keywordSetter		on: (SelectorNode new key: key code: nil "fill this in?").	kwdHolder firstSubmorph 		setProperty: #syntacticReformatting toValue: #keywordSetter;		setProperty: #syntacticallyCorrectContents toValue: key asString;		contents: (self splitAtCapsAndDownshifted: 			(key asString allButFirst: 3) allButLast withFirstCharacterDownshifted		);		emphasis: 1.	row addNoiseString: '_' emphasis: 1.	(args first asMorphicSyntaxIn: row) setConditionalPartStyle			! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 13:14'!alanKwdSetter: aNode isAConditional: template key: key args: args	| nodeWithNilReceiver row kwdHolder |	nodeWithNilReceiver _ aNode copy receiver: nil.	(row _ self addRow: #keyword2 on: nodeWithNilReceiver)		borderWidth: 1;		parseNode: (nodeWithNilReceiver as: MessageNode);		borderColor: row stdBorderColor.	row addNoiseString: '''s' emphasis: 1.	kwdHolder _ row		addToken: key		type: #keywordGetz		on: (SelectorNode new key: key code: nil "fill this in?").	kwdHolder firstSubmorph 		setProperty: #syntacticReformatting toValue: #keywordGetz;		setProperty: #syntacticallyCorrectContents toValue: key asString;		contents: (self splitAtCapsAndDownshifted: (key asString allButLast: 5));		emphasis: 1.	row addNoiseString: '_' emphasis: 1.	(args first asMorphicSyntaxIn: row) setConditionalPartStyle			! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 13:13'!alanUnaryGetter: aNode key: key	| nodeWithNilReceiver row kwdHolder |	nodeWithNilReceiver _ aNode copy receiver: nil.	(row _ self addRow: #unary on: nodeWithNilReceiver)		borderWidth: 1;		parseNode: (nodeWithNilReceiver as: MessageNode);		borderColor: row stdBorderColor.	row addNoiseString: '''s' emphasis: 1.	kwdHolder _ row		addToken: key		type: #unaryGetter		on: (SelectorNode new key: key code: nil "fill this in?").	kwdHolder firstSubmorph 		setProperty: #syntacticReformatting toValue: #unaryGetter;		setProperty: #syntacticallyCorrectContents toValue: key asString;		contents: (self splitAtCapsAndDownshifted:			(key asString allButFirst: 3) withFirstCharacterDownshifted		);		emphasis: 1.! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 12:49'!alanUnaryPostRcvr: aNode key: key selector: selector	| row |	(self isStandardGetterSelector: key) ifTrue: [		^self alanUnaryGetter: aNode key: key	].	row _ (self addUnaryRow: key style: #unary) layoutInset: 1.	^ row parseNode: selector! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 12:12'!alansMessageNode: aNode receiver: receiver selector: selector keywords: key arguments: args	| receiverMorph testAndReceiver anotherSelf wordyMorph template |	template _ self alansTemplateStyleFor: key.	receiver ifNotNil: ["i.e. not a cascade"		anotherSelf _ self constructSelfVariant: receiver and: key.		anotherSelf ifNotNil: [			wordyMorph _ self addString: anotherSelf special: false.			wordyMorph setProperty: #wordyVariantOfSelf toValue: true.			self addMorph: wordyMorph. 			self layoutInset: 1.			^self		].		testAndReceiver _ self.		template = 1 ifTrue: [			testAndReceiver _ self addRow: #keyword1 on: nil.			self setSpecialOuterTestFormat.			testAndReceiver addNoiseString: 'Test'		].		false "template = 2" ifTrue: [			testAndReceiver _ self addRow: #keyword1 on: nil.			"self setSpecialOuterTestFormat."			testAndReceiver addNoiseString: 'Repeat for'		].		receiverMorph _ receiver asMorphicSyntaxIn: testAndReceiver.		template = 1 ifTrue: [receiverMorph setConditionalPartStyle].	].	"unary messages"	args size = 0 ifTrue: [		^self alanUnaryPostRcvr: aNode key: key selector: selector	].	"binary messages"	key last = $: ifFalse: [		^self alanBinaryPostRcvr: aNode key: key args: args	].	"keyword messages"	receiverMorph ifNotNil: [receiverMorph setConditionalPartStyle].	self setSpecialOuterTestFormat.	self		alanKeywordMessage: aNode 		isAConditional: template 		key: key 		args: args! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 12:49'!isStandardGetterSelector: key	key size > 3 ifFalse: [^false].	(key beginsWith: 'get') ifFalse: [^false].	key fourth isUppercase ifFalse: [^false].	^true! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 4/4/2001 12:48'!isStandardSetterKeyword: key	key size > 4 ifFalse: [^false].	(key endsWith: ':') ifFalse: [^false].	(key beginsWith: 'set') ifFalse: [^false].	key fourth isUppercase ifFalse: [^false].	^true! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 4/4/2001 12:54'!shouldBeBrokenIntoWords: aSymbol	^#(methodHeader1 methodHeader2 keyword2 upArrow 		tempVariable tempVariableDeclaration blockarg2 variable		keywordGetz keywordSetter unaryGetter) includes: aSymbol! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 4/4/2001 13:12'!splitAtCapsAndDownshifted: aString	^String streamContents: [ :strm |		aString do: [ :each | 			each = $: ifFalse: [				each isUppercase ifTrue: [strm space; nextPut: each asLowercase]					ifFalse: [strm nextPut: each]			].		]	].! !!SyntaxMorph methodsFor: 'alans styles' stamp: 'RAA 4/4/2001 13:12'!substituteKeywordFor: aString	aString isEmpty ifTrue: [^aString asString].	aString asString = '^ ' ifTrue: [^'answer'].	aString asString = 'ifTrue:' ifTrue: [^'Yes'].	aString asString = 'ifFalse:' ifTrue: [^'No'].	aString asString = 'self' ifTrue: [^'self'].	aString first isUppercase ifTrue: [^aString asString].	^self splitAtCapsAndDownshifted: aString! !