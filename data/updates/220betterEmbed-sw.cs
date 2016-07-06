'From Squeak 2.1 of June 30, 1998 on 9 September 1998 at 4:10:43 pm'!"Change Set:		betterEmbed-swDate:			9 September 1998Author:			Scott WallaceOpens up the embed command to more choices, abandoning the restrictive roots logic I let myself be talked into last May.  The embed item is now offered whenever the core sample at the selection-point contains more than one possible target, and the user is invited to embed the object in question into any feasible target in the core sample."!!Morph methodsFor: 'menus' stamp: 'sw 9/9/1998 15:46'!addAddHandMenuItemsForHalo: aMenu hand: aHandMorph	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."	| unlockables |	aMenu addLine.	aMenu add: 'copy to paste buffer' action: #copyToPasteBuffer.	self player ifNotNil:		[aMenu add: 'make another instance of me' action: #makeNewPlayerInstance].	aMenu addLine.	aMenu add: 'open viewer' action: #openViewerForArgument.	(self isKindOf: MorphThumbnail) ifFalse:		[aMenu add: 'make alias' action: #makeAliasForArgument].	aMenu add: 'change costume...' action: #chooseNewCostumeForArgument.	((self isKindOf: SketchMorph) and: [GIFImports size > 0]) ifTrue:		[aMenu add: 'use imported graphic...' action: #chooseNewFormForSketchMorph].	self colorSettable ifTrue:		[aMenu add: 'change color...' action: #changeColor].	aHandMorph potentialEmbeddingTargets size > 1 ifTrue:		[aMenu add: 'embed...' action: #placeArgumentIn].	self isLocked		ifFalse:			[aMenu add: 'lock' action: #lockMorph]		ifTrue:			[aMenu add: 'unlock' action: #unlockMorph].  "probably not possible -- wouldn't get halo"	unlockables _ self submorphs select:		[:m | m isLocked].	unlockables size == 1 ifTrue:		[aMenu add: 'unlock "', unlockables first externalName, '"' action: #unlockContents].	unlockables size > 1 ifTrue:		[aMenu add: 'unlock all contents' action: #unlockContents.		aMenu add: 'unlock...' action: #unlockOneSubpart].	"aMenu add: 'make mouse-sensitive' action: #makeMouseSensitive."	(owner == nil or: [self == owner submorphs last]) ifFalse:		[aMenu add: 'send to back' action: #goBehind]! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 9/9/1998 16:05'!buildMorphMenuFor: argMorph	"Build the morph menu. This menu has two sections. The first section contains commands that are handled by the hand; the second contains commands handled by the argument morph."	| menu |	argument _ argMorph.	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'grab' action: #grabMorph.	menu add: 'delete' action: #dismissMorph.	menu add: 'copy to paste buffer' action: #copyToPasteBuffer.	menu add: 'go behind' action: #goBehind.	menu add: 'add halo' action: #addHalo.	menu add: 'duplicate' action: #maybeDuplicateMorph.	self potentialEmbeddingTargets size > 1 ifTrue:		[menu add: 'embed...' action: #placeArgumentIn].	menu add: 'resize' action: #resizeMorph.	(argMorph isKindOf: SketchMorph)  ifFalse:		[menu add: 'fill color' action: #changeColor].	(argMorph morphsAt: targetOffset) size > 1 ifTrue:		[menu add: 'submorphs...'			target: self			selector: #selectSubmorphToOperateOn:sending:event:			argumentList: (Array with: argMorph with: #operateOnSubmorph:event:)].	menu addLine.	menu add: 'inspect' action: #inspectMorph.	menu add: 'inspect in Morphic' action: #inspectMorphInMorphic.	menu add: 'browse' action: #browseMorphClass.	menu add: 'make own subclass' action: #subclassMorph.	menu addLine.	menu add: 'name me' action: #nameMorph.	(argMorph isKindOf: MorphicModel) ifTrue:		[menu add: 'save morph as prototype' action: #saveAsPrototype.		(argMorph ~~ self world modelOrNil) ifTrue:			 [menu add: 'become this world''s model' action: #beThisWorldsModel]].	menu add: 'save morph in file' action: #saveOnFile.	menu add: 'show actions' action: #showActions.	menu addLine.	menu defaultTarget: argMorph.	argMorph addCustomMenuItems: menu hand: self.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 9/9/1998 15:49'!placeArgumentIn 	"Let the user choose a new layer in the core sample for the argument to reside in, but don't allow strange loops"	|  targetMorph |	targetMorph _ self selectEmbedTargetMorph: ('Place ', argument externalName, ' in...').	targetMorph ifNotNil:		[targetMorph addMorphFront: argument fromWorldPosition: argument position]! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 9/9/1998 15:44'!potentialEmbeddingTargets	"Answer a list of targets into which the hand's arguement could be embedded"	| possibleTargets |	possibleTargets _ self world morphsAt: menuTargetOffset.	argument ifNotNil:		[possibleTargets removeAllFoundIn: argument allMorphs].	^ possibleTargets! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 9/9/1998 16:07'!selectEmbedTargetMorph: caption	"Put up a menu of morphs found in a core sample taken of the world at the receiver's menuTargetOffset, with the given caption"	|  menu |	menu _ CustomMenu new.	self potentialEmbeddingTargets  do: [:m | menu add: (self submorphNameFor: m) action: m].	^ caption size == 0		ifTrue:			[menu startUp]		ifFalse:			[menu startUpWithCaption: caption]! !HandMorph removeSelector: #selectTargetMorph:excluding:!