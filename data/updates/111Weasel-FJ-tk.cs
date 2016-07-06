'From Squeak 2.0 of May 22, 1998 on 17 June 1998 at 4:27:21 pm'!!Metaclass methodsFor: 'accessing' stamp: 'tk 6/17/1998 09:48'!isSystemDefined	"Answer false if I am a UniClass (an instance-specific lightweight class)"	^ true! !!Morph methodsFor: 'apr98 additions' stamp: 'tk 6/17/1998 16:17'!addDebuggingItemsTo: aMenu hand: aHandMorph	| subMenu |	subMenu _ MenuMorph new defaultTarget: aHandMorph.	subMenu add: 'control-menu...' target: aHandMorph selector: #invokeMetaMenuFor: argument: aHandMorph argument.	subMenu add: 'inspect morph' action: #inspectArgument.	World ifNil:		[subMenu add: 'inspect morph (morphic)' action: #inspectArgumentInMorphic].	subMenu add: 'browse morph class' action: #browseMorphClass.	costumee ifNotNil:		[subMenu add: 'inspect player' action: #inspectCostumee.		World ifNil: [subMenu add: 'inspect player (morphic)' action: #inspectArgumentsCostumeeInMorphic].		subMenu add: 'browse player class' action: #browsePlayerClass].	subMenu add: 'make own subclass' action: #subclassMorph.	subMenu add: 'internal name' action: #nameMorph.	subMenu add: 'save morph in file' action: #saveOnFile.	subMenu defaultTarget: self.	subMenu add: 'edit balloon help' action: #editBalloonHelpText.	subMenu add: 'temp command' action: #tempCommand.	aMenu add: 'debug...' subMenu: subMenu! !!BookMorph methodsFor: 'private' stamp: 'tk 6/17/1998 16:16'!makePageControls	| b c r |	b _ SimpleButtonMorph new target: self; borderColor: Color black.	c _ AlignmentMorph newColumn.	c color: b color; borderWidth: 0; inset: 0.	c hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	r _ AlignmentMorph newRow.	r color: b color; borderWidth: 0; inset: 0.	r hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	r addMorphBack: (b fullCopy label: '<-';			actionSelector: #previousPage).	r addMorphBack: (b fullCopy label: 'Insert';		actionSelector: #insertPage).	r addMorphBack: (b fullCopy label: 'Delete';		actionSelector: #deletePage).	r addMorphBack: (b fullCopy label: 'Text';		actionSelector: #newTextMorph).	r addMorphBack: (b fullCopy label: '->';			actionSelector: #nextPage).	c addMorphBack: r.	r _ r copy removeAllMorphs.	r addMorphBack: (b fullCopy label: 'Bookmark';	actionSelector: #bookmarkForThisPage).	r addMorphBack: (b fullCopy label: 'Save';		actionSelector: #saveOnFile).	c addMorphBack: r.		^ c! !!HandMorph methodsFor: 'meta menu' stamp: 'tk 6/17/1998 16:17'!buildMorphMenuFor: argMorph	"Build the morph menu. This menu has two sections. The first section contains commands that are handled by the hand; the second contains commands handled by the argument morph."	| menu |	argument _ argMorph.	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'grab' action: #grabMorph.	menu add: 'delete' action: #dismissMorph.	menu add: 'copy to paste buffer' action: #copyToPasteBuffer.	menu add: 'go behind' action: #goBehind.	menu add: 'add halo' action: #addHalo.	menu add: 'duplicate' action: #duplicateMorph.	(argument pasteUpMorph morphsAt: targetOffset) size > 2 ifTrue:		[menu add: 'embed...' action: #placeArgumentIn].	menu add: 'resize' action: #resizeMorph.	(argMorph isKindOf: SketchMorph)  ifFalse: [		menu add: 'fill color' action: #changeColor].	(argMorph morphsAt: targetOffset) size > 1 ifTrue: [		menu add: 'submorphs...'			target: self			selector: #selectSubmorphToOperateOn:sending:event:			argumentList: (Array with: argMorph with: #operateOnSubmorph:event:)].	menu addLine.	menu add: 'inspect' action: #inspectMorph.	menu add: 'inspect in Morphic' action: #inspectMorphInMorphic.	menu add: 'browse' action: #browseMorphClass.	menu add: 'make own subclass' action: #subclassMorph.	menu addLine.	menu add: 'name me' action: #nameMorph.	(argMorph isKindOf: MorphicModel) ifTrue: [		menu add: 'save morph as prototype' action: #saveAsPrototype.		(argMorph ~~ self world modelOrNil) ifTrue: [			 menu add: 'become this world''s model' action: #beThisWorldsModel]].	menu add: 'save morph in file' action: #saveOnFile.	menu add: 'show actions' action: #showActions.	menu addLine.	menu defaultTarget: argMorph.	argMorph addCustomMenuItems: menu hand: self.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'tk 6/17/1998 16:27'!saveOnFile	"Save the guy we clicked on, not the hand"	argument saveOnFile! !!UserScript methodsFor: 'initialization' stamp: 'tk 6/13/1998 14:25'!donorActor: player1 ownActor: player2	player _ player2.	currentScriptEditor ifNotNil: [		currentScriptEditor == #textuallyCoded ifFalse: [			currentScriptEditor donorActor: player1 ownActor: player2]].	self allScriptVersionsDo: [:anEditor | anEditor donorActor: player1 ownActor: player2]! !BookMorph removeSelector: #saveBookToFile!HandMorph removeSelector: #saveMorphInFile!