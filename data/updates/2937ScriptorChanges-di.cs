'From Squeak2.9alpha of 12 June 2000 [latest update: #2940] on 8 November 2000 at 3:34:28 pm'!"Change Set:		ScriptorChangesDate:			8 November 2000Author:			Dan IngallsA number of changes to tile scriptors, including...	Grab highlight was not consistently visible - fixed	Drop highlight stayed on sometimes - fixed	Neither highlights showed on multi-keyword exprs - fixed	Selector parts now highlighted	Selector parts were extra high - not now	Reduced inset on all nodes	Sliding stmt insertion space was too slow - now use pop-up caret	Stmt nodes now duplicate on drag like everything else	Lays groundwork for folded messagesNote Prerequisite: InsetPoints.cs"!Smalltalk renameClassNamed: #BobsMessageNode as: #TileMessageNode!!BlockNode methodsFor: 'tiles' stamp: 'di 11/2/2000 10:42'!asMorphicSyntaxIn: parent	| len shown row column |	(column _ parent addColumn: #block on: self) inset: 4; minBorderWidth: 1.		"level _ 1 max: levelOrZero."	self addCommentToMorph: column.	len _ shown _ statements size.	column explanation: 'A block of ',len printString,' Smalltalk statements',(		arguments size = 0 ifTrue: [''] ifFalse: [' and ',arguments size printString,' arguments']	).		"(levelOrZero = 0 and: [statements last isReturnSelf]) ifTrue: [shown _ 1 max: shown - 1]."	arguments size = 0 ifFalse: [		row _ column addRow: #blockarg1 on: (BlockArgsNode new).		arguments do: [:arg | 			(arg asMorphicSyntaxIn: row) color: #blockarg2		].	].	(statements copyFrom: 1 to: shown) do: [ :each | 		(each asMorphicSyntaxIn: column) borderWidth: 1.		each addCommentToMorph: column.	] separatedBy: [		column addMorphBack: (			AlignmentMorph newRow 				vResizing: #rigid; 				color: Color transparent;				extent: 2@2		).			].	^column! !!MessageNode methodsFor: 'tiles' stamp: 'di 11/8/2000 10:58'!asMorphicSyntaxIn: parent	| printer substitute row sel |	sel _ #message.	(parent nodeClassIs: CascadeNode) ifTrue: [sel _ #keyword2].	row _ parent addRow: sel on: self.	special > 0 ifTrue: [printer _ MacroPrinters at: special].	substitute _ self as: TileMessageNode.	(printer == #printCaseOn:indent:) ifTrue: [		self asMorphicCaseOn: row indent: nil.		^parent 	"morph?"	].	(special > 0)		ifTrue: 			[substitute perform: printer with: row with: nil]		ifFalse: 			[substitute 				printKeywords: selector key				arguments: arguments				on: row				indent: nil]."expr addCommentToMorph: morph."	^row! !!MessageNode methodsFor: 'tiles' stamp: 'di 11/8/2000 14:14'!morphFromKeywords: key arguments: args on: parent indent: ignored	| keywords arg thisKey column row selType explanation receiverString getMenuBlock receiverMorph wideReceiver |	getMenuBlock _ [ :aClass | self buildMenuForClass: aClass andSelector: key].	receiver ifNotNil: [	"i.e. not a cascade"		receiverMorph _ receiver asMorphicSyntaxIn: parent.	].	parent getMenuBlock: getMenuBlock.	keywords _ key keywords.	selType _ precedence asPrecedenceName.	receiverString _ receiver ifNil: [		''	] ifNotNil: [		' sent to ',receiver explanation	].	args size = 0 ifTrue: [		row _ parent 			addTextRow: key getMenuBlock: getMenuBlock explanation: nil.		parent explanation: selType,' message #',keywords first,receiverString.		^ row parseNode: selector.	].	wideReceiver _ receiver ~~ nil and: [receiverMorph fullBounds width > 80].	(wideReceiver not and: [args size = 1]) ifTrue: [		row _ parent 			addTextRow: keywords first  			getMenuBlock: getMenuBlock			explanation: selType,' selector'.		row parseNode: selector.		args first asMorphicSyntaxIn: parent.		parent explanation: selType,' message #', keywords first, ' with an argument ',					args first explanation, receiverString.		"(args first asMorphicSyntaxIn: parent)			explanation: selType,' message #',keywords first,' with an argument ',					args first explanation,receiverString."		^ self	].		explanation _ 'A keyword message #',key,				' with ',keywords size printString,' arguments'.	column _ parent addColumn: #keyword1 on: self.	column explanation: explanation;  getMenuBlock: getMenuBlock.	1 to: keywords size do: [:part |		arg _ args at: part.		thisKey _ keywords at: part.		(row _ column addRow: #keyword2 on: self)			borderWidth: 1;			parseNode: (self as: MessagePartNode);			borderColor: row stdBorderColor.		(row addTextRow: thisKey) parseNode: (KeyWordNode new).		arg asMorphicSyntaxIn: row.	].	explanation _ explanation,receiverString.	parent explanation: explanation.	wideReceiver ifTrue: [parent foldWideReceiver]! !!Morph methodsFor: 'classification' stamp: 'di 11/2/2000 13:24'!isSyntaxMorph	^false! !!SelectorNode methodsFor: 'printing' stamp: 'di 11/8/2000 10:04'!printOn: aStream indent: level 	aStream withStyleFor: #keyword		do: [key == nil				ifTrue: [aStream nextPutAll: '<key==nil>']				ifFalse: [aStream nextPutAll: key]]! !!StringHolder methodsFor: 'tiles' stamp: 'di 11/4/2000 11:07'!openSyntaxView	"Open a syntax view on the current method"	| class selector |	(selector _ self selectedMessageName) ifNotNil: [		class _ self selectedClassOrMetaClass.		SyntaxMorph testClass: class andMethod: selector.	]! !!Browser methodsFor: 'message functions' stamp: 'di 11/4/2000 11:08'!messageListMenu: aMenu shifted: shifted	| aList |	aList _ shifted		ifFalse: [#(			('browse full (b)' 						browseMethodFull)			('browse hierarchy (h)'					classHierarchy)			('browse method (O)'					openSingleMessageBrowser)			-			('fileOut'								fileOutMessage)			('printOut'								printOutMessage)			-			('senders of... (n)'						browseSendersOfMessages)			('implementors of... (m)'					browseMessages)			('inheritance (i)'						methodHierarchy)			('tile scriptor'							openSyntaxView)			('versions (v)'							browseVersions)			-			('inst var refs...'						browseInstVarRefs)			('inst var defs...'						browseInstVarDefs)			('class var refs...'						browseClassVarRefs)			('class variables'						browseClassVariables)			('class refs (N)'							browseClassRefs)			-			('remove method (x)'					removeMessage)			-			('more...'								shiftedYellowButtonActivity))]		ifTrue: [#(			('method pane' 							makeIsolatedCodePane)			"('make a scriptor'						makeScriptor)"			('toggle diffing'							toggleDiffing)			('implementors of sent messages'			browseAllMessages)			-			('sample instance'						makeSampleInstance)			('inspect instances'						inspectInstances)			('inspect subinstances'					inspectSubInstances)			-			('remove from this browser'				removeMessageFromBrowser)			('change category...'					changeCategory)			-			('change sets with this method'			findMethodInChangeSets)			('revert to previous version'				revertToPreviousVersion)			('remove from current change set'		removeFromCurrentChanges)			('revert and forget'						revertAndForget)			-			('fetch documentation'					fetchDocPane)			('more...' 								unshiftedYellowButtonActivity))].	^ aMenu addList: aList! !!SyntaxMorph methodsFor: 'accessing' stamp: 'RAA 8/17/1999 09:09'!borderWidth: x	"for the benefit of nodes which should never go to zero"	super borderWidth: (x max: (minBorderWidth ifNil: [0]))! !!SyntaxMorph methodsFor: 'accessing' stamp: 'di 11/6/2000 15:44'!isNoun	"Consider these to be nouns:  TempVariableNode, LiteralNode, VariableNode, (MessageNode with receiver), CascadeNode, AssignmentNode"	(#(TempVariableNode LiteralNode VariableNode CascadeNode AssignmentNode) includes:		(parseNode class name)) ifTrue: [^ true].	(self nodeClassIs: MessageNode) ifTrue: [^ parseNode receiver notNil].	^ false! !!SyntaxMorph methodsFor: 'accessing' stamp: 'di 11/2/2000 13:25'!isSyntaxMorph	^ true! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 16:20'!handlesKeyboard: evt	^ evt keyCharacter = Character backspace! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 08:21'!handlesMouseOverDragging: evt	^ evt hand hasSubmorphs		and: [evt hand firstSubmorph isSyntaxMorph]! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 22:40'!keyStroke: evt	"Handle a keystroke event."	| spacer |	evt keyCharacter = Character backspace ifTrue:		[(owner notNil and: [owner isSyntaxMorph]) ifTrue:			[(owner nodeClassIs: BlockNode) ifTrue:				["Delete a statement."				(spacer _ self submorphAfter) class == AlignmentMorph						ifTrue: [spacer delete].				self delete].			]].! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/8/2000 11:11'!mouseDown: evt 	| dup |	evt yellowButtonPressed		ifTrue: [^ self showMenu: evt].	owner isSyntaxMorph		ifTrue: [(owner nodeClassIs: MethodNode)					ifTrue: [^ self].	"can't take anything out of a MethodNode"				evt hand attachMorph: (dup _ self duplicate).				^ dup align: dup topLeft with: evt hand position + (7@14)]		ifFalse: ["bare, out in the world"				(self nodeClassIs: MethodNode)					ifTrue: [^ self]. "whole method not draggable"				evt hand attachMorph: self.				^ self align: self topLeft with: evt hand position + (7@14)].! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 16:02'!mouseEnter: evt	"Highlight this level as a potential grab target""Transcript cr; print: self; show: ' enter'."	self unhighlightOwner.	self highlightForGrab: evt.	evt hand newKeyboardFocus: self! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 15:28'!mouseEnterDragging: evt	"Highlight this level as a potential drop target""Transcript cr; print: self; show: ' enterDragging'."	self unhighlightOwner.	self highlightForDrop: evt.	(self nodeClassIs: BlockNode) ifTrue: [self startStepping]! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 13:10'!mouseEnterDragging: evt inCaret: caretMorph	"Highlight the caret as a potential drop target"	self unhighlight.	caretMorph borderColor: Color green darker.! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 09:16'!mouseLeave: evt	"Move grab highlight back out a level""Transcript cr; print: self; show: ' leave'."	self unhighlight.	(owner ~~ nil and: [owner isSyntaxMorph])		ifTrue: [owner highlightForGrab: evt].! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 15:37'!mouseLeaveDragging: evt"Transcript cr; print: self; show: ' leaveDragging'."	(self nodeClassIs: BlockNode) ifTrue:		[self stopStepping.		self hideCaret].	"Move drop highlight back out a level"	self unhighlight.	(owner ~~ nil and: [owner isSyntaxMorph])		ifTrue: [owner highlightForDrop: evt].! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 13:12'!mouseLeaveDragging: evt inCaret: caretMorph	"Unhighlight the caret as a potential drop target"	caretMorph borderColor: caretMorph color darker.	self highlightForDrop: evt.! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/6/2000 13:19'!step	self trackCaret! !!SyntaxMorph methodsFor: 'event handling' stamp: 'di 11/3/2000 08:03'!stepTime	^ 50! !!SyntaxMorph methodsFor: 'dropping/grabbing' stamp: 'di 11/6/2000 16:24'!acceptDroppingMorph: aMorph event: evt	| itNoun |	"For the moment, you have to drop it the right place.  We do not look at enclosing morphs"	"Two ways to do this:  Must always destroy old node, then drag in new one.		Or, drop replaces what you drop on.  Nasty with blocks.  see wantsDroppedMorph:event:"	"We know it is acceptable.  Just a matter of which case"	itNoun _ aMorph isNoun.	(self nodeClassIs: BlockNode) & itNoun		ifTrue:			[(aMorph nodeClassIs: TempVariableNode) ifTrue:				["If I am a BlockNode, and it is a TempVariableNode, add it into list"				^ (self addBlockArg: aMorph) ifFalse:					["if already declared, start new line of code with it"					self addToBlock: aMorph event: evt]]			ifFalse:			[^ self addToBlock: aMorph event: evt]].	"If I am a BlockNode and it is a noun add it as a new line"	(parseNode class == BlockNode) ifTrue: [		 (aMorph parseNode class == ReturnNode) ifTrue: [^ self addToBlock: aMorph event: evt]].	"Later add args and keywords.  later allow comments to be dropped"	"Can't put statement, literal, assignment, or cascade into left side of assignment"	(owner isSyntaxMorph) ifTrue:		[(owner nodeClassIs: AssignmentNode) ifTrue:			[(owner submorphIndexOf: self) = 1 ifTrue:				[((aMorph nodeClassIs: TempVariableNode)				or: [aMorph nodeClassIs: VariableNode])  ifFalse: [ ^ self]]]].	owner replaceSubmorph: self by: aMorph.	"do the normal replacement"	aMorph owner layoutChanged.! !!SyntaxMorph methodsFor: 'dropping/grabbing' stamp: 'di 11/6/2000 15:44'!wantsDroppedMorph: aMorph event: evt	"For the moment, you have to drop it the right place.  We do not look at enclosing morphs"	"Two ways to do this:  Must always destroy old node, then drag in new one.		Or, drop replaces what you drop on.  Nasty with blocks."	| meNoun itNoun |	(aMorph isKindOf: SyntaxMorph) ifFalse: [^ false].	"If nodes are of equal class, replace me with new one."	(self nodeClassIs: MessageNode) ifFalse: [		(self nodeClassIs: aMorph parseNode class) ifTrue: [^ true]].	meNoun _ self isNoun.	itNoun _ aMorph isNoun.	"Consider these nouns to be equal:  TempVariableNode, LiteralNode, VariableNode, (MessageNode with receiver), CascadeNode, AssignmentNode"	meNoun & itNoun ifTrue: [^ true].	meNoun & (aMorph nodeClassIs: BlockNode) ifTrue: [^ true].	(self nodeClassIs: BlockNode) & itNoun ifTrue: [^ true].				"If I am a BlockNode, and it is a TempVariableNode, add it into list"				"If I am a BlockNode, and it is a noun, add it as a new line"	(self nodeClassIs: BlockNode) ifTrue:		 [(aMorph nodeClassIs: ReturnNode) ifTrue:			[^ (self submorphs				detect: [:mm | ((mm isSyntaxMorph) and: [mm nodeClassIs: ReturnNode])]				ifNone: [nil]) isNil]].	"none already in this block"				"If I am a BlockNode, and it is a ReturnNode, add to end"	(self nodeClassIs: MethodNode) ifTrue: [^ false].	"Later add args and keywords"		"Later allow comments to be dropped in"		"Add MethodTemps by dropping into the main block"	^ false "otherwise reject"! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/6/2000 13:34'!borderColor: newColor	"Optimize out the case of no change"	newColor = borderColor ifFalse: [super borderColor: newColor]! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/5/2000 07:26'!dropColor	^ Color green darker! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/5/2000 07:26'!grabColor	^ Color paleOrange mixed: 0.5 with: Color brown! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/6/2000 09:22'!highlightForDrop: evt	(self wantsDroppedMorph: evt hand firstSubmorph event: evt)		ifTrue: [self borderColor: self dropColor].! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/6/2000 09:22'!highlightForGrab: evt	self borderColor: self grabColor.! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/6/2000 15:39'!stdBorderColor	"several choices of how to do the border"	(Preferences valueOfFlag: #noTileColor) 		ifTrue: [parseNode class == MessagePartNode ifTrue: [^ color darker].				parseNode class == BlockArgsNode ifTrue: [^ color darker].				parseNode class == MethodTempsNode ifTrue: [^ color darker].				^ color]		ifFalse: [^ color darker]! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/2/2000 09:50'!unhighlight	self borderColor: self stdBorderColor! !!SyntaxMorph methodsFor: 'highlighting' stamp: 'di 11/6/2000 12:24'!unhighlightOwner	"Unhighlight my owner"	(owner ~~ nil and: [owner isSyntaxMorph])		ifTrue: [owner unhighlight]! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 13:16'!addCaretFront: aMorph	"Add a caret morph without triggering re-layout.	This is just a copy of addMorphFront: without layoutChanged"	| newSubmorphs |	aMorph owner ifNotNil: [aMorph owner privateRemoveMorph: aMorph].	aMorph layoutChanged.	aMorph privateOwner: self.	newSubmorphs _ submorphs species new: submorphs size + 1.	newSubmorphs at: 1 put: aMorph.	newSubmorphs		replaceFrom: 2		to: newSubmorphs size		with: submorphs		startingAt: 1.	submorphs _ newSubmorphs.	aMorph changed.! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 13:17'!hideCaret	self valueOfProperty: #caretMorph ifPresentDo:		[:arrow | self removeCaret: arrow.		self removeProperty: #caretMorph].! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 12:06'!insertionIndexForCaret: aPoint	"Like insertionIndexFor:, but only looks at SyntaxMorphs"	| last |	last _ 0.	submorphs withIndexDo:		[:mm :ii | mm isSyntaxMorph ifTrue:			[mm center y > aPoint y ifTrue: [^ ii].			last _ ii]].	^ last! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 13:18'!removeCaret: aMorph	"Remove the caret morph without triggering layout.	This is a copy of privateRemoveMorph: without layoutChanged."	aMorph changed.	submorphs _ submorphs copyWithout: aMorph.! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 13:18'!showCaretAt: location	| newArrow |	self valueOfProperty: #caretMorph ifPresentDo:		[:arrow | ^ arrow align: arrow bounds leftCenter with: location].	newArrow _ (PolygonMorph vertices: {0@0. 15@-10. 15@10}			color: Color lightGreen borderWidth: 1 borderColor: Color lightGreen)			on: #mouseEnterDragging send: #mouseEnterDragging:inCaret: to: self;			on: #mouseLeaveDragging send: #mouseLeaveDragging:inCaret: to: self.	self addCaretFront: newArrow.	self setProperty: #caretMorph toValue: newArrow.	newArrow align: newArrow bounds leftCenter with: location! !!SyntaxMorph methodsFor: 'insertion caret' stamp: 'di 11/6/2000 13:22'!trackCaret	| hand i pt localPt sub |	hand _ self primaryHand.	(hand lastEvent redButtonPressed and: [(self hasOwner: hand) not])	ifTrue:		[localPt _ self globalPointToLocal: hand position.		i _ self insertionIndexForCaret: localPt.		sub _ submorphs at: i.		pt _ localPt y < sub center y			ifTrue: [sub topLeft + (-4@-2)]			ifFalse: [sub bottomLeft + (-4@2)].		self showCaretAt: pt]	ifFalse:		[self stopStepping.		self hideCaret]! !!SyntaxMorph methodsFor: 'menus' stamp: 'di 11/6/2000 15:39'!showMenu: evt	| exp oldColor oldWidth menu val |	exp _ self getExplanation ifNil: [^owner mouseDown: evt].	oldColor _ self borderColor.	oldWidth _ self borderWidth.	self borderWidth: 1.	self borderColor: Color red.	World displayWorldSafely.	exp _ exp withNoLineLongerThan: 35.	(val _ self getCurrentValue) ifNotNil: [		exp _ exp,'Value: ',((val withNoLineLongerThan: 35) truncateWithElipsisTo: 300).	].	menu _ "MenuMorph" SyntaxMenuMorph new.	menu title: exp.	menu add: 'OK' target: self selector: #yourself.	menu addLine.	(self getMenuItemsIn: self hostContext) do: [ :each |		each ifNil: [menu addLine] ifNotNil: [			menu 				add: each first				target: each second 				selector: #valueWithPossibleArgs:				argument: {self parsedInClass}		].	].	(self nodeClassIs: MethodNode) ifTrue:		[menu add: 'accept method' target: self selector: #accept.		menu add: 'show code' target: self selector: #showCode].	menu whenDone: [		self borderColor: oldColor.		self borderWidth: oldWidth].	menu 		popUpAt: evt hand position x @ (self pointInWorld: self bounds bottomLeft) y		forHand: evt hand in: World.! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/6/2000 12:27'!addBlockArg: aMorph	| tempHolder tt var nn row |	"Add a temporary to a block or the method.  Return true if succeed"	owner parseNode class == MethodNode ifTrue: [		^ (self addTempVar: aMorph)].	"Node for them is not insided the block"		"If exists, drop the temp in this block and let use extend it."	tt _ self firstSubmorph.	tempHolder _ tt firstSubmorph class == SyntaxMorph 				ifTrue: [tt parseNode class == BlockArgsNode 							ifTrue: [tt] ifFalse: [nil]]				ifFalse: [nil].	nn _ (aMorph allMorphs detect: [:rr | rr class == StringMorph]) contents.	"name"	tempHolder ifNil: ["make whole row"		row _ self addRow: #blockarg1 on: (BlockArgsNode new).		self addMorphFront: row.		aMorph parseNode name: nn key: nn code: nil.		var _ row addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true].	tempHolder ifNotNil: [		"If this variable is not present, add it"		tempHolder allMorphs do: [:rr | 					rr class == StringMorph ifTrue: [rr contents = nn ifTrue: [^ false]]].				"is present. caller adds the temp as a new line of code to be extended"		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true].! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/8/2000 13:52'!addColumn: aColorOrSymbol on: aNode	| col |	self addMorphBack: (col _ self class column: aColorOrSymbol on: aNode).	aColorOrSymbol == #keyword1 		ifTrue: [col inset: 0]		ifFalse: [col inset: -1].	^ col! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/4/2000 19:53'!addTempVar: aMorph 	| tempHolder ii tt var nn tempMorph |	"know we are a block inside a MethodNode" 	tempHolder _ (ii _ owner submorphIndexOf: self) = 1				ifFalse: [tt _ owner submorphs at: ii - 1.						(tt isSyntaxMorph and: [tt parseNode class == MethodTempsNode])					 		ifTrue: [tt] ifFalse: [nil]]				ifTrue: [nil].	nn _ (aMorph allMorphs detect: [:rr | rr class == StringMorph]) contents.	"name"	tempHolder ifNil: [		tempMorph _ owner addRow: #tempVariable on: MethodTempsNode new.		owner addMorph: tempMorph inFrontOf: self.		tempMorph color: tempMorph color darker;			 explanation: 'These temporary variables are defined for the duration of this method'.		aMorph parseNode name: nn key: nn code: nil.		aMorph parseNode asMorphicSyntaxIn: tempMorph.		^ true].	tempHolder ifNotNil: [		tempHolder allMorphs do: [:rr | 					rr class == StringMorph ifTrue: [rr contents = nn ifTrue: [^ false]]].		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true]! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/2/2000 10:42'!addTextRow: aStringLikeItem	| row text |	(row _ self class row: #text on: nil) borderWidth: 1.	text _ "NonEditableTextMorph" StringMorph contents: 		(aStringLikeItem copyWithout: Character cr).	row addMorph: text.	self addMorphBack: row.	^row! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/2/2000 10:42'!addTextRow: aStringLikeItem getMenuBlock: aBlock explanation: aString	| row text |	(row _ self class row: #text on: nil) borderWidth: 1.	text _ "NonEditableTextMorph" StringMorph new contents: aStringLikeItem.	row addMorph: text.	self addMorphBack: row.	row		getMenuBlock: aBlock;		explanation: aString.	^row! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/6/2000 15:55'!addToBlock: aMorph event: evt	"Insert a new line of code"	| whereDropped dropBefore |	whereDropped _ "self pointFromWorld:" evt cursorPoint.	dropBefore _ self submorphs 		detect: [:each | each isSyntaxMorph and: [whereDropped y < each center y]] 		ifNone: [nil].	(aMorph nodeClassIs: ReturnNode) ifTrue: [dropBefore _ nil].		"Returns are always at the end. (Watch out for comments)"	dropBefore 		ifNil: [self addMorphBack: aMorph]		ifNotNil: [self addMorph: aMorph inFrontOf: dropBefore].	self addMorph: (			AlignmentMorph newRow 				vResizing: #rigid; 				color: Color transparent;				extent: 2@2)		behind: aMorph.	self layoutChanged.! !!SyntaxMorph methodsFor: 'layout' stamp: 'di 11/8/2000 15:30'!foldWideReceiver	"I am a message whose receiver is wide, and whose message part is a column.	Rearrange me so that the message part appear indented under the receiver part."	^ self "Not yet implemented"! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'di 11/6/2000 15:26'!nodeClassIs: aParseNodeClass	"Test the class of my parseNode"	^ parseNode class == aParseNodeClass! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'di 11/6/2000 15:36'!printOn: strm indent: level 	| lev nodeClass first |	"Tree walk and produce text of the code.  #ST80.  Just do it in one big ugly method."	lev _ level.	(nodeClass _ parseNode class) == BlockNode ifTrue: [		(owner respondsTo: #parseNode) ifTrue: [			owner parseNode class == MethodNode ifFalse: [strm nextPut: $[.  lev _ lev+1]]].				"normal block has []"	nodeClass == MessageNode ifTrue: [		color = (SyntaxMorph translateColor: #message) ifTrue: [strm nextPut: $( ]].	nodeClass == MethodTempsNode ifTrue: [strm nextPut: $|; space].	first _ true.	submorphs do: [:sub |		(sub respondsTo: #printOn:indent:) ifTrue: [			nodeClass == CascadeNode & first not ifTrue: [				color = (SyntaxMorph translateColor: #cascade2) ifTrue: [strm nextPut: $;; space ]].			nodeClass == BlockArgsNode ifTrue: [strm nextPut: $:].			sub printOn: strm indent: lev.			(nodeClass == BlockNode) & (sub parseNode class == BlockArgsNode) not	& 				(sub parseNode class == ReturnNode) not					ifTrue: [strm nextPut: $.].			(nodeClass == BlockNode) & (sub parseNode class == BlockArgsNode) not					ifTrue: [strm crtab: lev]				ifFalse: [nodeClass == MethodNode ifTrue: [strm crtab: lev] ifFalse: [strm space]].			first _ false].		sub class == StringMorph ifTrue: [strm nextPutAll: sub contents].		"return indent for ifTrue: ifFalse:"].	nodeClass == MessageNode ifTrue: [		color = (SyntaxMorph translateColor: #message) ifTrue: [strm nextPut: $) ]].	nodeClass  == BlockNode ifTrue: [		(owner respondsTo: #parseNode) ifTrue: [			owner parseNode class == MethodNode ifFalse: [strm nextPut: $] ]]].				"normal block has []"	nodeClass == BlockArgsNode ifTrue: [strm nextPut: $|; crtab: lev].	nodeClass == MethodTempsNode ifTrue: [strm nextPut: $|; crtab: lev].	nodeClass == MethodNode ifTrue: [strm skip: -1].	"erase last period"! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'di 11/6/2000 10:47'!test	3 > 4 ifTrue: [].	^ self! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'di 11/8/2000 10:57'!toDo"Biggies...[ ]	Integrate with EToy scriptors [Ted][ ]	Options:	Show / hide syntax markers (like [], (), ., :, ;, etc)	No color / color-in-focus / full color	Tiles / textiles / text[ ]	ParsedTextMorph -- looks like text but has all same substructure[ ]	Introduce notion of an UnParsedNode -- maybe a flag in ParseNode	Text -> UnParsed -> Parsed -> CodeGen[ ]	Need DnD evaluator, or some sort of '!!' button on any entity (halo?)	Also inspector / browser[ ]	All the type help we can getDetails [me] ...[ ]	Layout needs work -- centering of block keyword NG on big blocks	Reason is can't see expr in which block exists.	How about PP guidelines for all on one line vs		rcvr, kwds on separate lines.[ ]	Not all message parts have morphs	-- in fact only those split on separate lines[ ]	Need autoscroll during drag for drop[ ]	Use, eg, shift-drag to move, del to delete[ ]	Click out of parse tree should bring window to front if not[ ]	Nice option would be full color within the current grab-focus.[X]	Brown highlight is not consistently visible[X]	Green highlight stays on sometimes[X]	Both highlights won't show on multi-keyword expr[X]	Need a normal highlight for selector parts[X]	Selector parts are extra high -- shouldn't be[X]	All nodes could possibly use one less inset.[X]	Sliding stmt insertion space is too slow -- use on/off arrow[-]	Return box border interferes with green, brown highlight[X]	Stmt nodes should duplicate on drag like everything else[ ]	Layout needs work -- centering of block keyword NG on big blocks	Reason is can't see expr in which block exists.	How about PP guidelines for all on one line vs		rcvr, kwds on separate lines.[ ]	Use, eg, shift-drag to move, del to delete[ ]	Click out of parse tree should bring window to front if not[X]	ReturnNodes doesn't seem to get enter/leave	Same with AssignmentNodesSystem...[X]	ChangeSet showActive[X]	System revert last change in emergency eval[ ]	Only keep history 7 deep; option to clear on quit	clear command above spaceLeft[ ]	Compute each page of prefs viewer on demand instead of as now.[ ]	Offer a search command that will gather up all preferences that match a given string (name or help string)Preferences enable: #noTileColor.Preferences disable: #noTileColor.Smalltalk browseAllSelect: [:cm | cm size > 600]SyntaxMorph testAll"! !!SyntaxMorph class methodsFor: 'as yet unclassified' stamp: 'di 11/8/2000 10:10'!row: aColor on: aParseNode	| r color |	color _ self translateColor: aColor.	(r _ self newRow)		parseNode: aParseNode;		inset: self standardInset;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		color: color;		borderWidth: 1;		borderColor: r stdBorderColor;		centering: #topLeft.	^r! !!SyntaxMorph class methodsFor: 'as yet unclassified' stamp: 'di 11/2/2000 15:15'!standardInset	^ 2@0! !!SyntaxMorph class methodsFor: 'as yet unclassified' stamp: 'di 11/8/2000 15:24'!testAllMethodsOver: methodSize  "MessageTally spyOn: [SyntaxMorph testAllMethodsOver: 600]"	"Add up the total layout area for syntax morphs representing all methods	over the given size.  This is a stress-test for SyntaxMorph layout.	A small value for the total area is also a figure of merit in the presentation	of Squeak source code in general.""Results:	#(69 600 180820874 103700)  11/4	70% build morphs, 12% get source, 9% layout, 8% parse, 1% roundoff"	| tree source biggies morph stats time area |	biggies _ Smalltalk allSelect: [:cm | cm size > methodSize].	stats _ OrderedCollection new.'Laying out all ' , biggies size printString , ' methods over ' , methodSize printString , ' bytes...'	displayProgressAt: Sensor cursorPoint	from: 1 to: biggies size	during:		[:bar |		biggies withIndexDo:			[:methodRef :i | bar value: i.			Utilities setClassAndSelectorFrom: methodRef in: 				[:aClass :aSelector |				source _ (aClass compiledMethodAt: aSelector) getSourceFromFile.				time _ Time millisecondsToRun:					[tree _ Compiler new 						parse: source 						in: aClass 						notifying: nil.					morph _ tree asMorphicSyntaxUsing: SyntaxMorph.					area _ morph fullBounds area]].			stats add: {methodRef. area. time}]		].	^ {{biggies size.  methodSize. stats detectSum: [:a | a second]. stats detectSum: [:a | a third]}.		(stats asSortedCollection: [:x :y | x third >= y third]) asArray}! !SyntaxMorph class removeSelector: #dropTargetColor!SyntaxMorph class removeSelector: #grabTargetColor!SyntaxMorph class removeSelector: #layoutMethodsOver:!SyntaxMorph removeSelector: #addInsertionArrowFront:!SyntaxMorph removeSelector: #drawSubmorphsOn:!SyntaxMorph removeSelector: #dropTargetColor!SyntaxMorph removeSelector: #hideInsertionArrow!SyntaxMorph removeSelector: #hideInsertionArrowAt:!SyntaxMorph removeSelector: #highlight:!SyntaxMorph removeSelector: #highlight:event:!SyntaxMorph removeSelector: #highlight:evt:!SyntaxMorph removeSelector: #highlightForDrop!SyntaxMorph removeSelector: #highlightForDropTarget!SyntaxMorph removeSelector: #highlightForGrab!SyntaxMorph removeSelector: #highlightInsertionArrow:!SyntaxMorph removeSelector: #ifNodeClass:then:!SyntaxMorph removeSelector: #indexOfMorphAbove:!SyntaxMorph removeSelector: #ownerBlocksStep:!SyntaxMorph removeSelector: #removeInsertionArrow:!SyntaxMorph removeSelector: #removeSpaces!SyntaxMorph removeSelector: #setAllBorders!SyntaxMorph removeSelector: #setBorder!SyntaxMorph removeSelector: #showInsertionArrowAt:!SyntaxMorph removeSelector: #steptransform!SyntaxMorph removeSelector: #trackInsertionArrow!SyntaxMorph removeSelector: #unhighlightAbove!!SyntaxMorph reorganize!('accessing' borderWidth: color: externalName getCurrentValue getExplanation isNoun isSyntaxMorph parseNode parseNode: parsedInClass parsedInClass:)('event handling' cleanup handlesKeyboard: handlesMouseDown: handlesMouseOver: handlesMouseOverDragging: keyStroke: mouseDown: mouseEnter: mouseEnterDragging: mouseEnterDragging:inCaret: mouseLeave: mouseLeaveDragging: mouseLeaveDragging:inCaret: step stepTime)('dropping/grabbing' acceptDroppingMorph:event: wantsDroppedMorph:event:)('highlighting' borderColor: dropColor grabColor highlightForDrop: highlightForGrab: stdBorderColor unhighlight unhighlightOwner)('insertion caret' addCaretFront: hideCaret insertionIndexForCaret: removeCaret: showCaretAt: trackCaret)('menus' accept decompile getMenuBlock getMenuBlock: getMenuItemsIn: showCode showMenu:)('layout' addBlockArg: addColumn:on: addRow:on: addTempVar: addTextRow: addTextRow:getMenuBlock:explanation: addToBlock:event: foldWideReceiver)('as yet unclassified' balloonText debugger: explanation: hostContext implant inAScrollPane minBorderWidth: nodeClassIs: openInWindow printOn: printOn:indent: test testForNode:andDo: toDo update:)!StringHolder removeSelector: #openBobsSyntaxView!"Postscript:Change cat name."SystemOrganization renameCategory: 'Bob-Syntax' toBe: 'Morphic-Tile Scriptors'.!