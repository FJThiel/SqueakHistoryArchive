'From Squeak3.1alpha of 5 February 2001 [latest update: #3730] on 26 February 2001 at 9:21:03 am'!"Change Set:		moreSyntaxDate:			26 February 2001Author:			Bob Arningmore syntax tweaks for Alan* 'with' (all lower case) instead of 'use'? (seems better generally)* 'return' or 'send back' or 'answer' for '^' (could you try the latter for now?). This could go in a template also .....* need to be able to resize the scriptors to see the whole script (this is really important)* Unaries should be colored differently than keywords (lighter?)* need some better syntax theory for temps (either prefixed by 'with' or put at bottom)��� -- this also could be a template structure: [ with ( this that otherthing ) ]��� using similar conventions as we do for Test, but maybe with a very light different colored template ...* putting the method into something that looks like the etoy scriptor (but so that the name and parameters show up looking nice and in the right spot).-- also adds a handy morphic analysis method: #morphReport"!AlignmentMorph subclass: #SyntaxMorph	instanceVariableNames: 'parseNode markerMorph '	classVariableNames: 'AllSpecs ContrastFactor DownRightArrow SizeScaleFactor '	poolDictionaries: ''	category: 'Morphic-Tile Scriptors'!PasteUpMorph subclass: #SyntaxTestMethods	instanceVariableNames: 'letterActors wild leftMargin rightMargin switch current jumpSwitch hotIndex '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Tile Scriptors'!!CascadeNode methodsFor: 'tiles' stamp: 'RAA 2/22/2001 13:56'!asMorphicSyntaxIn: parent	^parent		cascadeNode: self 		receiver: receiver 		messages: messages! !!Morph methodsFor: 'printing' stamp: 'RAA 2/26/2001 07:22'!morphReport	^self morphReportFor: #(hResizing vResizing bounds)! !!Morph methodsFor: 'printing' stamp: 'RAA 2/25/2001 17:47'!morphReportFor: attributeList	| s |	s _ WriteStream on: String new.	self		morphReportFor: attributeList 		on: s 		indent: 0.	StringHolder new contents: s contents; openLabel: 'morph report'! !!Morph methodsFor: 'printing' stamp: 'RAA 2/25/2001 17:48'!morphReportFor: attributeList on: aStream indent: anInteger	anInteger timesRepeat: [aStream tab].	aStream print: self; space.	attributeList do: [ :a | aStream print: (self perform: a); space].	aStream cr.	submorphs do: [ :sub |		sub morphReportFor: attributeList on: aStream indent: anInteger + 1	].! !!Player methodsFor: 'scripts-kernel' stamp: 'RAA 2/26/2001 07:45'!newScriptorAround: aPhrase	"Sprout a scriptor around aPhrase, thus making a new script.  aPhrase may either be a PhraseTileMorph (classic tiles 1997-2001) or a SyntaxMorph (2001 onward)"	| aScriptEditor aUniclassScript tw blk |	aUniclassScript _ self class permanentUserScriptFor: self unusedScriptName player: self.	aScriptEditor _ aUniclassScript instantiatedScriptEditorForPlayer: self.	Preferences universalTiles ifTrue: [		aScriptEditor install.		aScriptEditor insertUniversalTiles.  "Gets an empty SyntaxMorph for a MethodNode"		tw _ aScriptEditor findA: TwoWayScrollPane.		aPhrase ifNotNil:			[blk _ tw scroller firstSubmorph "MethodNode" lastSubmorph "BlockNode".			blk addMorphFront: aPhrase.			aPhrase accept.		].		SyntaxMorph setSizeAndMakeResizable: aScriptEditor.	] ifFalse: [		aPhrase 				ifNotNil: [aScriptEditor phrase: aPhrase]	"does an install"				ifNil: [aScriptEditor install]	].	self class allSubInstancesDo: [:anInst | anInst scriptInstantiationForSelector: aUniclassScript selector].		"The above assures the presence of a ScriptInstantiation for the new selector in all siblings"	self updateAllViewersAndForceToShow: #scripts.	^ aScriptEditor! !!ReturnNode methodsFor: 'tiles' stamp: 'RAA 2/26/2001 06:44'!asMorphicSyntaxIn: parent	^parent returnNode: self expression: expr! !!ScriptEditorMorph methodsFor: 'other' stamp: 'RAA 2/26/2001 08:20'!extent: x	"override to yellow handle behavior"	^super extent: (x max: self minWidth@self minHeight)! !!ScriptEditorMorph methodsFor: 'other' stamp: 'RAA 2/26/2001 07:17'!insertUniversalTilesForClass: aClass selector: aSelector	"Add a submorph which holds the universal-tiles script for the given class and selector"	|  source tree syn widget header |	source _ aClass sourceCodeAt: aSelector.    	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	syn usingClassicTiles ifTrue: ["the old EToy look, remove method header line"		(header _ syn findA: SelectorNode) ifNotNil: [header delete]].	widget _ syn inAScrollPane.	widget color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true;		setProperty: #maxAutoFitSize toValue: 300@200.	self addMorphBack: widget.	widget extent: (self width - 10 @ 150)."get the default size and then make resizable"	self fullBounds.	widget 		hResizing: #spaceFill;		vResizing: #spaceFill;		removeProperty: #maxAutoFitSize.	self 		hResizing: #rigid;		vResizing: #rigid.! !!ScriptEditorMorph methodsFor: 'other' stamp: 'RAA 2/26/2001 07:14'!useNewTilesNow	| mp source aSelector aClass tree syn widget |	"First make it show source with a method pane, then substitute tiles!!"self halt.		"obsolete"self flag: #bob.	(mp _ self findA: MethodMorph) ifNil: [^ self].	"code pane must be present"	aSelector _ mp model selectedMessageName.	aClass _ mp model selectedClassOrMetaClass.	source _ aClass sourceCodeAt: aSelector.    	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	widget _ syn inAScrollPane.	widget color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true;		setProperty: #maxAutoFitSize toValue: 300@200.	mp delete.	self addMorphBack: widget.	widget extent: (self width - 10 @ 150).	syn finalAppearanceTweaks.! !!SyntaxMorph methodsFor: 'dropping/grabbing' stamp: 'RAA 2/26/2001 07:53'!morphToDropInPasteUp: aPasteUp	"If property #beScript is true, create a scriptor around me."	| actualObject itsSelector aScriptor adjustment handy tw blk |	(self valueOfProperty: #beScript ifAbsent: [false]) ifFalse: [^ self].	self removeProperty: #beScript.	(actualObject _ self actualObject) ifNil: [^ self].	actualObject assureUniClass.	itsSelector _ self userScriptSelector.	aScriptor _ itsSelector isEmptyOrNil		ifFalse:			[adjustment _ 0@0.			actualObject scriptEditorFor: itsSelector]		ifTrue:			[adjustment _ 60 @ 20.			actualObject newScriptorAround: self].	aScriptor ifNil: [^self].	handy _ aPasteUp primaryHand.	aScriptor position: handy position - adjustment.	aPasteUp addMorphFront: aScriptor.	"do this early so can find World"	aScriptor showingMethodPane ifFalse: [		(tw _ aScriptor findA: TwoWayScrollPane) ifNil:			[itsSelector ifNil: ["blank script"				tw _ aScriptor findA: TwoWayScrollPane.				blk _ tw scroller firstSubmorph "MethodNode" lastSubmorph "BlockNode".				blk addMorphFront: self]].		SyntaxMorph setSizeAndMakeResizable: aScriptor.	].	^ aScriptor! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 06:40'!addBlockArg: aMorph	"Add a temporary to a block or the method.  Return true if succeed"	"(aMorph nodeClassIs: TempVariableNode) is known to be true."	"***NOTE: This method should be combined with addTempVar:"	| tempHolder tt var nn |	owner isMethodNode ifTrue: [		^ (self addTempVar: aMorph)].	"Node for them is not inside the block"		"If exists, drop the temp in this block and let use extend it."	tt _ self firstSubmorph.	tempHolder _ tt firstSubmorph isSyntaxMorph 				ifTrue: [(tt nodeClassIs: BlockArgsNode) 							ifTrue: [tt] ifFalse: [nil]]				ifFalse: [nil].	nn _ aMorph parseNode key.	"name"	tempHolder ifNil: ["make whole row"		tempHolder _ self addRow: #blockarg1 on: (BlockArgsNode new).		self addMorphFront: tempHolder.		tempHolder addNoiseString: self noiseBeforeBlockArg.		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var layoutInset: 1.		var addMorphBack: (self addString: nn).		self cleanupAfterItDroppedOnMe.		^ true].	tempHolder submorphsDo:		[:m | "if present. caller adds the temp as a new line of code to be extended"		m isSyntaxMorph and: [m parseNode key = nn ifTrue: [^ false]]].			"If this variable is not present, add it"	tempHolder addNoiseString: self noiseBeforeBlockArg.	aMorph parseNode name: nn key: nn code: nil.	tempHolder addMorphBack: (tempHolder transparentSpacerOfSize: 4@4).	var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.	var layoutInset: 1.	var addMorphBack: (StringMorph contents: nn).	var cleanupAfterItDroppedOnMe.	^ true! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/21/2001 18:31'!addNoiseString: aNoiseString	self noiseWordsAdded ifFalse: [^self].	^(self addColumn: #keyword1 on: nil)		layoutInset: 1;		addMorphBack: (self noiseStringMorph: aNoiseString)! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 06:41'!addTempVar: aMorph 	"know we are a block inside a MethodNode" 	"(aMorph nodeClassIs: TempVariableNode) is known to be true."	| tempHolder ii tt var nn |	tempHolder _ (ii _ owner submorphIndexOf: self) = 1				ifFalse: [tt _ owner submorphs at: ii - 1.						(tt isSyntaxMorph and: [tt nodeClassIs: MethodTempsNode])					 		ifTrue: [tt] ifFalse: [nil]]				ifTrue: [nil].	nn _ aMorph parseNode key.	"name"	tempHolder ifNil: [		tempHolder _ owner addRow: #tempVariable on: MethodTempsNode new.		tempHolder addNoiseString: self noiseBeforeBlockArg.		owner addMorph: tempHolder inFrontOf: self.		aMorph parseNode name: nn key: nn code: nil.		aMorph parseNode asMorphicSyntaxIn: tempHolder.		tempHolder cleanupAfterItDroppedOnMe.		^ true].	tempHolder submorphsDo:		[:m | m isSyntaxMorph and: [m parseNode key = nn ifTrue: [^ false]]].	aMorph parseNode name: nn key: nn code: nil.	tempHolder addNoiseString: self noiseBeforeBlockArg.	tempHolder addMorphBack: (tempHolder transparentSpacerOfSize: 4@4).	var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.	var layoutInset: 1.	var addMorphBack: (self addString: nn).	var cleanupAfterItDroppedOnMe.	^ true! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 08:27'!colorForKeywords: original and: modified	(#('ifTrue:' 'ifFalse:' 'Test') includes: original asString) ifTrue: [		^Color black	].	(original size > 0 and: [original last = $:]) ifFalse: [		^Color gray	].	^(Color r: 0.333 g: 0.333 b: 0.333)! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 06:40'!noiseBeforeBlockArg	^'with'! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/21/2001 18:31'!noiseStringMorph: aNoiseString	| sMorph |	sMorph _ StringMorph contents: aNoiseString.	sMorph 		font: (self fontToUseForSpecialWord: aNoiseString) emphasis: 1; 		color: (self colorForKeywords: aNoiseString and: aNoiseString);		setProperty: #noiseWord toValue: true.	^sMorph! !!SyntaxMorph methodsFor: 'layout' stamp: 'RAA 2/26/2001 06:46'!substituteKeywordFor: aString	aString asString = '^ ' ifTrue: [^'answer'].	aString asString = 'ifTrue:' ifTrue: [^'Yes'].	aString asString = 'ifFalse:' ifTrue: [^'No'].	aString asString = 'self' ifTrue: [^'self'].	^String streamContents: [ :strm |		aString do: [ :each | 			each = $: ifFalse: [				each isUppercase ifTrue: [strm space; nextPut: each asLowercase]					ifFalse: [strm nextPut: each]			].		]	].! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/26/2001 06:46'!addToken: aString type: aColorOrSymbol on: aNode	| sMorph modifiedString noiseWord col |	noiseWord _ nil.	sMorph _ self addString: aString.	self specialBlockFormatting ifTrue: [		(aColorOrSymbol == #keyword2) | (aColorOrSymbol == #upArrow) ifTrue: [			modifiedString _ self substituteKeywordFor: aString.			sMorph 				font: (self fontToUseForSpecialWord: modifiedString) emphasis: 1; 				color: (self colorForKeywords: aString and: modifiedString);				setProperty: #syntacticallyCorrectContents toValue: aString;				contents: modifiedString.		].	].	self alansTest1 ifTrue: [		(aColorOrSymbol == #variable or: [aColorOrSymbol == #tempVariable]) ifTrue: [			aString = 'self' ifTrue: [				sMorph setProperty: #wordyVariantOfSelf toValue: true.			].			(#('self' 'nil') includes: aString) ifFalse: [				aNode type < 4 ifTrue: [noiseWord _ 'my']			].		].	].	col _ (self addRow: aColorOrSymbol on: aNode)		layoutInset: 1.	noiseWord ifNotNil: [		col 			addMorphBack: (self noiseStringMorph: noiseWord);			addMorphBack: (self transparentSpacerOfSize: 3@1)	].	col addMorphBack: sMorph.	^col! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/23/2001 10:59'!addTokenSpecialCase: aString type: aColorOrSymbol on: aNode	| sMorph modifiedString noiseWord col |	noiseWord _ nil.	sMorph _ self addString: aString.	self specialBlockFormatting ifTrue: [		(aColorOrSymbol == #keyword2) ifTrue: [			modifiedString _ aString = 'if:' ifTrue: ['Test'] ifFalse: ['Yes'].			sMorph 				font: (self fontToUseForSpecialWord: modifiedString) emphasis: 1; 				color: (self colorForKeywords: aString and: modifiedString);				setProperty: #syntacticallyCorrectContents toValue: aString;				contents: modifiedString.		].	]."Smalltalk at: #Q put: OrderedCollection new"	self alansTest1 ifTrue: [		(aColorOrSymbol == #variable or: [aColorOrSymbol == #tempVariable]) ifTrue: [			aString = 'self' ifTrue: [				sMorph setProperty: #wordyVariantOfSelf toValue: true.			].			(#('self' 'nil') includes: aString) ifFalse: [				aNode type < 4 ifTrue: [noiseWord _ 'my']			].		].	].	col _ (self addRow: aColorOrSymbol on: aNode)		layoutInset: 1.	noiseWord ifNotNil: [		col 			addMorphBack: (self noiseStringMorph: noiseWord);			addMorphBack: (self transparentSpacerOfSize: 3@1)	].	col addMorphBack: sMorph.	^col! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/23/2001 15:27'!finalAppearanceTweaks	| deletes |	SizeScaleFactor ifNil: [SizeScaleFactor _ 1.0]. 	self usingClassicTiles ifTrue: [		self allMorphsDo: [:each | 			(each isKindOf: SyntaxMorph) ifTrue: [each lookClassic]		].		^self	].	deletes _ OrderedCollection new.	self allMorphsDo: [ :each |		(each hasProperty: #tokenVerticalSeparator) ifTrue: [			each == each owner lastSubmorph ifTrue: [deletes add: each].		].		(each respondsTo: #setDeselectedColor) ifTrue: [each setDeselectedColor].		(each hasProperty: #variableInsetSize) ifTrue: [			each layoutInset: 				((each valueOfProperty: #variableInsetSize) * SizeScaleFactor) rounded.		].	].	deletes do: [ :each | each delete].! !!SyntaxMorph methodsFor: 'menus' stamp: 'RAA 2/22/2001 10:31'!tokenVerticalSeparator	^RectangleMorph new		vResizing: #spaceFill;		setProperty: #tokenVerticalSeparator toValue: #true;		extent: 3@15;		borderWidth: 1;		borderColor: Color transparent;		color: (Color r: 0.581 g: 0.774 b: 0.903)! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 09:12'!addTemporaries: temporaries	| tempMorph outerMorph |	temporaries size > 0 ifFalse: [^self].	self alansTest1 ifFalse: [		tempMorph _ self addRow: #tempVariable on: (MethodTempsNode new).		temporaries 			do: [:temp | temp asMorphicSyntaxIn: tempMorph ]			separatedBy: [tempMorph addMorphBack: (tempMorph transparentSpacerOfSize: 4@4)].		^self	].	outerMorph _ self addRow: #tempVariable on: nil.	outerMorph setSpecialTempDeclarationFormat1.	outerMorph addNoiseString: 'with'.	tempMorph _ outerMorph addRow: #tempVariable on: (MethodTempsNode new).	tempMorph setSpecialTempDeclarationFormat2.	temporaries 		do: [:temp | tempMorph addToken: temp name type: #tempVariableDeclaration on: temp]		separatedBy: [tempMorph addMorphBack: self tokenVerticalSeparator].! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 09:06'!addTemporaryControls	| row stdSize |		stdSize _ 8@8.	row _ AlignmentMorph newRow		color: Color transparent;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap.	self addMorph: row.	{		Morph new			extent: stdSize; 			setBalloonText: 'Change the contrast';			on: #mouseUp send: #controlContrast2: to: self;			on: #mouseMove send: #controlContrast2: to: self;			on: #mouseDown send: #controlContrast2: to: self.		Morph new			extent: stdSize; 			color: Color green;			setBalloonText: 'Change basic spacing';			on: #mouseUp send: #controlSpacing2: to: self;			on: #mouseMove send: #controlSpacing2: to: self;			on: #mouseDown send: #controlSpacing2: to: self.		Morph new			extent: stdSize; 			color: Color red;			setBalloonText: 'Change basic style';			on: #mouseUp send: #changeBasicStyle to: self.	} do: [ :each |		row addMorphBack: each.		row addMorphBack: (self transparentSpacerOfSize: stdSize).	].! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 06:40'!blockNode: aNode arguments: arguments statements: statements	| row column |	(column _ self addColumn: #block on: aNode) layoutInset: 2@-1.	aNode addCommentToMorph: column.	arguments size > 0 ifTrue: [		row _ column addRow: #blockarg1 on: (BlockArgsNode new).		row addNoiseString: self noiseBeforeBlockArg.		arguments do: [:arg | (arg asMorphicSyntaxIn: row) color: #blockarg2]	].	statements do: [ :each | 		(each asMorphicSyntaxIn: column) borderWidth: 1.		each addCommentToMorph: column	].	^ column! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/22/2001 17:00'!cascadeNode: aNode receiver: receiver messages: messages	| row |	self alansTest1 ifTrue: [		row _ self addColumn: #cascade on: aNode.		row setSpecialOuterTestFormat.	] ifFalse: [		row _ self addRow: #cascade on: aNode	].	receiver asMorphicSyntaxIn: row.	messages do: [:m | m asMorphicSyntaxIn: row].	^ row"	(node2 _ aNode copy) receiver: nil messages: messages.	cascadeMorph _ row addColumn: #cascade2 on: node2.	messages do: [ :m | m asMorphicSyntaxIn: cascadeMorph].	^row"! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 06:27'!messageNode: aNode receiver: receiver selector: selector keywords: key arguments: args	| keywords column row receiverMorph firstArgMorph receiverWidth		messageWidth onlyOne nodeWithNilReceiver isAConditional 		testAndReceiver anotherSelf wordyMorph |	isAConditional _ #(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:) includes: key.	receiver ifNotNil: ["i.e. not a cascade"		anotherSelf _ self constructSelfVariant: receiver and: key.		anotherSelf ifNotNil: [			wordyMorph _ self addString: anotherSelf.			wordyMorph setProperty: #wordyVariantOfSelf toValue: true.			self addMorph: wordyMorph. 			self layoutInset: 1.			^self		].		testAndReceiver _ self.		self specialBlockFormatting ifTrue: [			isAConditional ifTrue: [				testAndReceiver _ self addRow: #keyword1 on: nil.				self setSpecialOuterTestFormat.				testAndReceiver addNoiseString: 'Test'			].		].		receiverMorph _ receiver asMorphicSyntaxIn: testAndReceiver.		self specialBlockFormatting ifTrue: [			isAConditional ifTrue: [self setConditionalPartStyle: receiverMorph].		].	].	keywords _ key keywords.	args size = 0 ifTrue: [		self alansTest1 ifTrue: [self addMorphBack: self tokenVerticalSeparator].		row _ (self addSingleKeywordRow: key) layoutInset: 1.		self alansTest1 ifTrue: [self addMorphBack: self tokenVerticalSeparator].		^ row parseNode: selector	].	receiverWidth _ receiver				ifNil: [0]				ifNotNil: [receiverMorph fullBounds width].	onlyOne _ args size = 1.	(receiverWidth <= 80 and: [onlyOne]) ifTrue: [		self alansTest1 ifTrue: [self addMorphBack: self tokenVerticalSeparator].		row _ (self addSingleKeywordRow: keywords first) layoutInset: 1.		row parseNode: selector.		self alansTest1 ifTrue: [self addMorphBack: self tokenVerticalSeparator].		firstArgMorph _ args first asMorphicSyntaxIn: self.		receiver ifNil: [^ self].		self alansTest1 ifFalse: [	"folding messed up in this mode"			(firstArgMorph fullBounds height > 100				or: [firstArgMorph fullBounds width > 250])			ifTrue: [self foldMessageOneArg].		].		^ self	].	nodeWithNilReceiver _ aNode copy receiver: nil.	(isAConditional | (key == #if:do:)) & self specialBlockFormatting ifTrue: [		self listDirection: #topToBottom.	].	column _ self addColumn: #keyword1 on: nodeWithNilReceiver.	"onlyOne ifTrue: [column parseNode: nil].	is a spacer"	messageWidth _ 0.	keywords		with: (args copyFrom: 1 to: keywords size)		do: [:kwd :arg |			isAConditional ifTrue: [				column addMorphBack: (column transparentSpacerOfSize: 3@3).			].			(row _ column addRow: #keyword2 on: nodeWithNilReceiver) borderWidth: 1;				parseNode: (nodeWithNilReceiver as: 						(onlyOne ifTrue: [MessageNode] ifFalse: [MessagePartNode]));				 borderColor: row stdBorderColor.			isAConditional ifTrue: [row addMorphBack: (row transparentSpacerOfSize: 20@6)].			key == #if:do: ifTrue: [				kwd = 'do:' ifTrue: [					row addMorphBack: (row transparentSpacerOfSize: 26@6).					"row addMorphBack: (self downRightArrow asMorph lock)"				] ifFalse: [					row addMorphBack: (row transparentSpacerOfSize: 10@6).				].				row addTokenSpecialCase: kwd					type: #keyword2					on: KeyWordNode new.			] ifFalse: [				row addToken: kwd					type: #keyword2					on: (onlyOne ifTrue: [SelectorNode new key: kwd code: nil "fill this in?"]									ifFalse: [KeyWordNode new]).				self alansTest1 & isAConditional not ifTrue: [					row addMorphBack: self tokenVerticalSeparator				].			].			arg asMorphicSyntaxIn: row.			messageWidth _ messageWidth + row fullBounds width].	onlyOne ifTrue: [self replaceSubmorph: column by: row.  column _ row].	receiverMorph ifNil: [^self].	self alansTest1 ifTrue: [^self].	receiverWidth + messageWidth < 350 ifTrue: [		isAConditional ifFalse: [self unfoldMessage].		^self	].	((receiverWidth > 200						or: [receiverWidth > 80								and: [column fullBounds height > 20]])					or: [receiverMorph fullBounds width > 30							and: [column fullBounds height > 100									or: [column fullBounds width > 250]]])				ifTrue: [^ self foldMessage]! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 08:39'!methodNodeInner: aNode selectorOrFalse: selectorOrFalse precedence: precedence arguments: arguments temporaries: temporaries primitive: primitive block: block	| header selNode |	selNode _ selectorOrFalse class == SelectorNode 		ifTrue: [selectorOrFalse] 		ifFalse: [SelectorNode new key: selectorOrFalse code: nil].	header _ self addRow: Color white on: selNode.	precedence = 1		ifTrue: [header addToken: aNode selector type: #keyword1 on: selNode]		ifFalse: [aNode selector keywords with: arguments do:					[:kwd :arg | 					header addToken: kwd type: #keyword2 on: selNode.					(arg asMorphicSyntaxIn: header) color: #blockarg2]].	aNode addCommentToMorph: self.	self addTemporaries: temporaries.	(primitive > 0 and: [(primitive between: 255 and: 519) not]) ifTrue:		["Dont decompile <prim> for, eg, ^ self "		self addTextRow: (String streamContents: [ :strm | aNode printPrimitiveOn: strm])].	block asMorphicSyntaxIn: self.	^ self! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/23/2001 15:28'!methodNodeOuter: aNode	| block |		self borderWidth: 0.	self alansTest1 ifTrue: [self addTemporaryControls].	aNode asMorphicSyntaxIn: self.	self finalAppearanceTweaks.	block _ self submorphs last.	block submorphs size = 1 ifTrue: [^ self].	"keep '^ self' if that is the only thing in method"	block submorphs last decompile string = '^  self ' ifTrue: [		block submorphs last delete].	^ self! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 06:46'!returnNode: aNode expression: expr	| row |	row _ self addRow: #return on: aNode.	row addToken: '^ ' type: #upArrow on: aNode.	expr asMorphicSyntaxIn: row.	expr addCommentToMorph: row.	^row! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/22/2001 17:00'!setSpecialOuterTestFormat	self 		specialColor: (Color r: 1.0 g: 0.935 b: 0.774) 		andBorder: (Color r: 0.581 g: 0.774 b: 0.903).	self useRoundedCorners.	self layoutInset: 6.	self setProperty: #variableInsetSize toValue: 6.! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 08:56'!setSpecialTempDeclarationFormat1	"the outer template for temp defs"	self 		specialColor: (Color lightYellow) 		andBorder: (Color r: 0.581 g: 0.774 b: 0.903).	self useRoundedCorners.	self layoutInset: 1.	self cellPositioning: #center.	"self setProperty: #variableInsetSize toValue: 6."! !!SyntaxMorph methodsFor: 'node to morph' stamp: 'RAA 2/26/2001 08:54'!setSpecialTempDeclarationFormat2	"the inner template for temp defs"	self 		specialColor: (Color r: 1.0 g: 1.0 b: 0.548) 		andBorder:  (Color r: 0.581 g: 0.774 b: 0.903).	self useRoundedCorners.	self layoutInset: 1.	"self setProperty: #variableInsetSize toValue: 6."! !!SyntaxMorph methodsFor: 'formatting options' stamp: 'RAA 2/26/2001 09:02'!controlContrast2: evt	| origin scale startingContrastX |	evt isMouseUp ifTrue: [		^self removeProperty: #startingPointForSomeAdjustment	].	evt isMouseDown ifTrue: [		^self setProperty: #startingPointForSomeAdjustment toValue: evt cursorPoint	].	ContrastFactor ifNil: [ContrastFactor _ 0.5].	scale _ 200.0.	startingContrastX _ ContrastFactor * scale.	origin _ self valueOfProperty: #startingPointForSomeAdjustment.	ContrastFactor _ (evt cursorPoint x - origin x + startingContrastX) / scale min: 1.0 max: 0.0.	self finalAppearanceTweaks.! !!SyntaxMorph methodsFor: 'formatting options' stamp: 'RAA 2/26/2001 09:07'!controlContrast: evt	"old version. may be some scripts saved with me, so don't crash"	^self! !!SyntaxMorph methodsFor: 'formatting options' stamp: 'RAA 2/26/2001 09:05'!controlSpacing2: evt	| origin scale startingContrastX |	evt isMouseUp ifTrue: [		^self removeProperty: #startingPointForSomeAdjustment	].	evt isMouseDown ifTrue: [		^self setProperty: #startingPointForSomeAdjustment toValue: evt cursorPoint	].	SizeScaleFactor ifNil: [SizeScaleFactor _ 1.0].	scale _ 200.0.	startingContrastX _ SizeScaleFactor * scale.	origin _ self valueOfProperty: #startingPointForSomeAdjustment.	SizeScaleFactor _ (evt cursorPoint x - origin x + startingContrastX) / scale min: 1.0 max: 0.0.	self finalAppearanceTweaks.! !!SyntaxMorph methodsFor: 'formatting options' stamp: 'RAA 2/26/2001 09:07'!controlSpacing: evt	"old version. may be some scripts saved with me, so don't crash"	^self! !!SyntaxMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 2/26/2001 08:21'!setSizeAndMakeResizable: outerMorph	| tw |	(tw _ outerMorph findA: TwoWayScrollPane) ifNil: [^self].	tw 		color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true;		setProperty: #maxAutoFitSize toValue: 300@200.	outerMorph 		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		cellPositioning: #topLeft.	outerMorph fullBounds.	tw 		hResizing: #spaceFill;		vResizing: #spaceFill;		removeProperty: #maxAutoFitSize.	outerMorph 		minWidth: outerMorph width;		minHeight: 100;		hResizing: #rigid;		vResizing: #rigid.! !!SyntaxMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 2/26/2001 07:46'!testClass: aClass andMethod: aSelector	| tree source syn widget outer |	source _ (aClass compiledMethodAt: aSelector) getSourceFromFile.	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	widget _ syn inAScrollPane.	widget color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true;		setProperty: #maxAutoFitSize toValue: 300@200.	(outer _ AlignmentMorph newRow) 		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		borderWidth: 4;		borderColor: outer color darker;		useRoundedCorners;		addMorphBack: widget.	syn finalAppearanceTweaks.	SyntaxMorph setSizeAndMakeResizable: outer.	outer openInWorld! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/22/2001 14:10'!altStyleTester	self doFirstThatWorks		if: [self = 1] do: [self + 1];		if: [self = 2] do: [self + 2];		if: [self = 3] do: [self + 3];		if: [self = 4] do: [self + 4];		if: [true] do: [self + 5]		! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/22/2001 15:13'!bobsplace2: letter after: before newLine: isNewLine 	"Position this letter. Put its left edge where the previous letter's right edge is. Move down to the next line if isNewLine is true. Add some 	leading for condensed or expanded text."	self doFirstThatWorks		if: [before = nil] do: [self selfWrittenAsIll march: letter  to: (leftMargin topRight )   ];		if: [isNewLine] do: [self selfWrittenAsIll march: letter  to: ((leftMargin right ) @ ((before bottom ) + 1   )   )   ];		if: [true] do: [self selfWrittenAsIll march: letter  to: (before topRight )   ]		! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/21/2001 19:03'!bobsplace: letter after: before newLine: isNewLine 	"Position this letter. Put its left edge where the previous letter's right 	edge is. Move down to the next line if isNewLine is true. Add some 	leading for condensed or expanded text."	( (before = nil   )  ifTrue: [(self selfWrittenAsIll march: letter  to: (leftMargin topRight )   ).		]  ifFalse: [( isNewLine  ifTrue: [(self selfWrittenAsIll march: letter  to: ((leftMargin right ) @ ((before bottom ) + 1   )   )   ).			]  ifFalse: [(self selfWrittenAsIll march: letter  to: (before topRight )   ).			]   ).		]   ).	^  self  		! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/23/2001 11:01'!pickUpFood	| newFood isCarryingFood pheromoneDropSize |	"pseudo instvars for syntax testing"	isCarryingFood _ pheromoneDropSize _ nil.	(isCarryingFood not and: [(self get: 'food') > 0]) ifTrue: [		newFood _ (self get: 'food') - 1.		self set: 'food' to: newFood.		newFood = 0 ifTrue: [self patchColor: Color transparent].		isCarryingFood _ true.		pheromoneDropSize _ 800.		self color: Color red.		"drop a blob of pheromone on the side of the food farthest from nest"		self turnTowardsStrongest: 'nestScent'.		self turnRight: 180.		self forward: 4.		self increment: 'pheromone' by: 5000].! !!SyntaxTestMethods methodsFor: 'as yet unclassified' stamp: 'RAA 2/26/2001 08:23'!wordyTestMethod	self selfWrittenAsMe = 1 ifTrue: [		self selfWrittenAsMy size.		self selfWrittenAsIll stop.		self selfWrittenAsIm large.		self selfWrittenAsThis helps.	].! !SyntaxMorph removeSelector: #setSpecialTempDeclarationFormat!SyntaxMorph removeSelector: #wordyTestMethod!ScriptEditorMorph removeSelector: #hResizing:!ScriptEditorMorph removeSelector: #minWidth!