'From Squeak2.9alpha of 16 June 2000 [latest update: #2884] on 25 October 2000 at 12:54:54 pm'!"Change Set:		ParseScript2-tkDate:			24 October 2000Author:			Ted KaehlerFurther changes to tile scripts for any Method.  Remove extra SyntaxMorph layer around literals, temps, and braceNodes.Put a SelectorNode on the header.Made up new nodes for morphs needed for spacing -- KeyWordNode, MessagePartNode."!SelectorNode subclass: #KeyWordNode	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'System-Compiler'!!KeyWordNode commentStamp: 'tk 10/25/2000 10:24' prior: 0!I am a part of a selector.   #at:put: is owned by a SelectorNode, and #put: within it is owned by a KeyWordNode.!BobsMessageNode subclass: #MessagePartNode	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Bob-Syntax'!!SyntaxMorph commentStamp: 'tk 10/25/2000 12:38' prior: 0!A single class of morph that holds any piece of Smalltalk, and allows it to be a tile.  Tiles can be dragged in or out of a method. In the message list pane of a Browser, choose 'tile scriptor'.  Bring up a second one to steal parts from.Dragging a statement removes it from a block.  Dragging anything else drags off a copy.  (Can't drag out the original -- it leaves an illegal hole.)Each SyntaxMorph points at a ParseNode.  After editing, the parseNode is only good as a part-of-speech indicator.  Only the classes of the parseNodes are important.  It's state is not kept up to date with the tile edits (but maybe it should be).The correspondence with parseNodes in not one-to-one.  Several extra levels of SyntaxMorph were added as aligners to make the horizontal and vertical layout right.  These have nil for the parseNode.To accept, pass over the tree of SyntaxMorphs, gathering their printStrings and inserting punctuation.  See (SyntaxMorph>>printOn:indent:).  Accept that.  Pretty print to make it look better.Things to do:[ ] Up/down arrows and extend arrow in tiles.  [ ] Some literals, braceNodes, etc must have (Character cr) in them.  Use something better than a StringMorph?.  It can only show one line.[ ] In order to do layout, there are extra morph layers with no corresponding node in the parseTree.  individual keywords are one example, as are a keyword-arg pair.  I have created new parseNode classes whose sole purpose is to mark those morphs.  (done)[ ] MessageNode is used in two different ways.  One is with a receiver, and one is without.  They have different colors to distinguish them.  CascadeNodes are the same.  Test the color, if necessary. (done) [ ] In large statements, make tile be narrow on left, expanding out to tall on the right.[ ] Keep the parseNodes legal and up to date as editing occurs.  Go from parse tree directly to byteCodes.[ ] When user retypes a selector with more colons, split it and put in arg2.  Allow dropping in extra keywords and extra args.[ ] Dan's pretty green slider code when dropping in new line.  Also indicate places in existing tiles where the new tile could drop.[ ] if drop in an illegal place, keep tile in the hand[ ] spacers need attention in temp var list, statements in blocks.[ ] click on window needs to bring it to front.[ ] Change title of window when edit selector name.[ ] remember category and put new message there.[ ] Undo!!Based on Bob Arning's SyntaxMorph exploration in early 2000.!!AssignmentNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:41'!asMorphicSyntaxIn: parent	| row |	row _ parent addRow: #assignment on: self.	variable asMorphicSyntaxIn: row.	row addMorphBack: (StringMorph contents: ' _ ').	value asMorphicSyntaxIn: row.	^row! !!BraceNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:11'!asMorphicSyntaxIn: parent	| row |	row _ parent addRow: #brace on: self.	row addMorphBack: (StringMorph new contents: 		(String streamContents: [:aStream | self printOn: aStream indent: 0])).	^row! !!LiteralNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:37'!asMorphicSyntaxIn: parent	| row |	row _ parent addColumn: #literal on: self.	(key isMemberOf: Association) ifFalse: [		row inset: 1.		^ row addMorphBack: (StringMorph contents: key storeString)].	key key isNil ifTrue: [		^ row addTextRow: ('###',key value soleInstance name)	] ifFalse: [		^ row addTextRow: ('##', key key)	].	! !!MessageNode methodsFor: 'tiles' stamp: 'tk 10/25/2000 11:00'!morphFromKeywords: key arguments: args on: parent indent: ignored	| keywords arg thisKey column row selType explanation receiverString getMenuBlock |	getMenuBlock _ [ :aClass | self buildMenuForClass: aClass andSelector: key].	receiver ifNotNil: [	"i.e. not a cascade"		receiver asMorphicSyntaxIn: parent.	].	parent getMenuBlock: getMenuBlock.	keywords _ key keywords.	selType _ precedence asPrecedenceName.	receiverString _ receiver ifNil: [		''	] ifNotNil: [		' sent to ',receiver explanation	].	args size = 0 ifTrue: [		row _ parent 			addTextRow: key getMenuBlock: getMenuBlock explanation: nil.		parent explanation: selType,' message #',keywords first,receiverString.		^ row parseNode: selector.	].	args size = 1 ifTrue: [		row _ parent 			addTextRow: keywords first  			getMenuBlock: getMenuBlock			explanation: selType,' selector'.		row parseNode: selector.		args first asMorphicSyntaxIn: parent.		parent explanation: selType,' message #', keywords first, ' with an argument ',					args first explanation, receiverString.		"(args first asMorphicSyntaxIn: parent)			explanation: selType,' message #',keywords first,' with an argument ',					args first explanation,receiverString."		^self	].		explanation _ 'A keyword message #',key,				' with ',keywords size printString,' arguments'.	column _ parent addColumn: #keyword1 on: self.	column explanation: explanation;  getMenuBlock: getMenuBlock.	1 to: keywords size do: [:part |		arg _ args at: part.		thisKey _ keywords at: part.		(row _ column addRow: #keyword2 on: self)			borderWidth: 0;			parseNode: (MessagePartNode new).	"need to fill in selector..."		(row addTextRow: thisKey) parseNode: (KeyWordNode new).		arg asMorphicSyntaxIn: row.	].	explanation _ explanation,receiverString.	parent explanation: explanation.! !!MethodNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:27'!asMorphicSyntaxIn: morph	| args header tempMorph selNode |	selNode _ selectorOrFalse class == SelectorNode 		ifTrue: [selectorOrFalse] 		ifFalse: [SelectorNode new key: selectorOrFalse code: nil].	header _ morph addRow: Color white on: selNode.	header explanation: precedence asPrecedenceName,' message header for: #',self selector.	precedence = 1 ifTrue: [		header addMorphBack: (StringMorph contents: self selector)	] ifFalse: [		args _ ReadStream on: arguments.		self selector keywords do: [:s | 			header addMorphBack: (StringMorph contents: s).			(args next asMorphicSyntaxIn: header) color: #blockarg2		].	].	self addCommentToMorph: morph.	temporaries size > 0 ifTrue: [		tempMorph _ morph addRow: #tempVariable on: (MethodTempsNode new).		tempMorph 			color: tempMorph color darker;			explanation: 'These temporary variables are defined for the duration of this method'.		temporaries do: [:temp | 			temp asMorphicSyntaxIn: tempMorph		] separatedBy: [			tempMorph addMorphBack: (tempMorph transparentSpacerOfSize: 4@4)		].	].	(primitive > 0 and: [(primitive between: 255 and: 519) not]) ifTrue: [		" Dont decompile <prim> for, eg, ^ self "		(morph addTextRow: (String streamContents: [ :strm | self printPrimitiveOn: strm]))			explanation: 'A primitive is in the VM. This is where all the hard stuff happens'.	].	block asMorphicSyntaxIn: morph.	^morph! !!ReturnNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:41'!asMorphicSyntaxIn: parent	| row |	row _ parent addRow: #return on: self.	row addMorphBack: (StringMorph contents: '^ ').	expr asMorphicSyntaxIn: row.	expr addCommentToMorph: row.	^row! !!SyntaxMenuMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/23/2000 13:55'!popUpAt: aPoint forHand: hand 	"Present this menu at the given point under control of the given hand."	| selItem delta |	popUpOwner _ hand.	selItem _ self items detect: [:each | true] ifNone: [nil].	self position: aPoint.	delta _ self bounds amountToTranslateWithin: hand worldBounds.	delta = (0 @ 0) ifFalse: [self position: self position + delta].	hand world addMorphFront: self; startSteppingSubmorphsOf: self.	hand newMouseFocus: (selItem ifNil: [self]).	self changed! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/24/2000 14:23'!acceptDroppingMorph: aMorph event: evt	| itNoun |	"For the moment, you have to drop it the right place.  We do not look at enclosing morphs"	"Two ways to do this:  Must always destroy old node, then drag in new one.		Or, drop replaces what you drop on.  Nasty with blocks.  see wantsDroppedMorph:event:"	"We know it is acceptable.  Just a matter of which case"	itNoun _ aMorph isNoun.	(parseNode class == BlockNode) & itNoun ifTrue: [		 (aMorph parseNode class == TempVariableNode) 			ifTrue: ["If I am a BlockNode, and it is a TempVariableNode, add it into list"				^ (self addBlockArg: aMorph) ifFalse: [						self addToBlock: aMorph event: evt]]	"if already declared, start new line of code with it"			ifFalse: [^ self addToBlock: aMorph event: evt]].				"If I am a BlockNode and it is a noun add it as a new line"	(parseNode class == BlockNode) ifTrue: [		 (aMorph parseNode class == ReturnNode) ifTrue: [^ self addToBlock: aMorph event: evt]].	"Later add args and keywords.  later allow comments to be dropped"	"Can't put statement, literal, assignment, or cascade into left side of assignment"	(owner respondsTo: #parseNode) ifTrue: [		owner parseNode class == AssignmentNode ifTrue: [			(owner submorphIndexOf: self) = 1 ifTrue: [				(#(TempVariableNode VariableNode) includes: aMorph parseNode class name) ifFalse: [					^ self]]]].	owner replaceSubmorph: self by: aMorph.	"do the normal replacement"	aMorph owner layoutChanged.! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/25/2000 12:34'!addBlockArg: aMorph	| tempHolder tt var nn row |	"Add a temporary to a block or the method.  Return true if succeed"	owner parseNode class == MethodNode ifTrue: [		^ (self addTempVar: aMorph)].	"Node for them is not insided the block"		"If exists, drop the temp in this block and let use extend it."	tt _ self firstSubmorph.	tempHolder _ tt firstSubmorph class == SyntaxMorph 				ifTrue: [tt parseNode class == BlockArgsNode 							ifTrue: [tt] ifFalse: [nil]]				ifFalse: [nil].	nn _ (aMorph allMorphs detect: [:rr | rr class == StringMorph]) contents.	"name"	tempHolder ifNil: ["make whole row"		row _ self addRow: #blockarg1 on: (BlockArgsNode new).		self addMorphFront: row.		aMorph parseNode name: nn key: nn code: nil.		var _ row addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true].	tempHolder ifNotNil: [		"If this variable is not present, add it"		tempHolder allMorphs do: [:rr | 					rr class == StringMorph ifTrue: [rr contents = nn ifTrue: [^ false]]].				"is present. caller adds the temp as a new line of code to be extended"		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true].! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/25/2000 12:34'!addTempVar: aMorph 	| tempHolder ii tt var nn tempMorph |	"know we are a block inside a MethodNode" 	tempHolder _ (ii _ owner submorphIndexOf: self) = 1				ifFalse: [tt _ owner submorphs at: ii - 1.						((tt respondsTo: #parseNode) and: [tt parseNode class == MethodTempsNode])					 		ifTrue: [tt] ifFalse: [nil]]				ifTrue: [nil].	nn _ (aMorph allMorphs detect: [:rr | rr class == StringMorph]) contents.	"name"	tempHolder ifNil: [		tempMorph _ owner addRow: #tempVariable on: MethodTempsNode new.		owner addMorph: tempMorph inFrontOf: self.		tempMorph color: tempMorph color darker;			 explanation: 'These temporary variables are defined for the duration of this method'.		aMorph parseNode name: nn key: nn code: nil.		aMorph parseNode asMorphicSyntaxIn: tempMorph.		^ true].	tempHolder ifNotNil: [		tempHolder allMorphs do: [:rr | 					rr class == StringMorph ifTrue: [rr contents = nn ifTrue: [^ false]]].		aMorph parseNode name: nn key: nn code: nil.		var _ tempHolder addColumn: #tempVariable on: aMorph parseNode.		var inset: 1.		var addMorphBack: (StringMorph contents: nn).		^ true]! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/24/2000 15:15'!addTextRow: aStringLikeItem	| row text |	(row _ self class row: #text on: nil) borderWidth: 0.	text _ "NonEditableTextMorph" StringMorph contents: 		(aStringLikeItem copyWithout: Character cr).	row addMorph: text.	self addMorphBack: row.	^row! !!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'tk 10/24/2000 15:32'!openInWindow	| window widget sel |	sel _ ''.	self firstSubmorph allMorphs do: [:rr | 			rr class == StringMorph ifTrue: [sel _ sel, rr contents]].	window _ (SystemWindow labelled: 'Tiles for ', parsedInClass printString, '>>',sel).	widget _ self inAScrollPane.	window		addMorph: widget		frame: (0@0 extent: 1.0@1.0).	window openInWorldExtent: (		self extent + (20@40) min: (Display boundingBox extent * 0.8) rounded	)! !!VariableNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:54'!asMorphicSyntaxIn: parent	| column |	column _ parent addColumn: #variable on: self.	column inset: 1.	column addMorphBack: (StringMorph contents: name).	^column! !!TempVariableNode methodsFor: 'tiles' stamp: 'tk 10/24/2000 15:57'!asMorphicSyntaxIn: parent	| column |	column _ parent addColumn: #tempVariable on: self.	column inset: 1.	column addMorphBack: (StringMorph contents: name).	^ column! !