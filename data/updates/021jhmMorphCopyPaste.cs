'From Squeak 2.0 BETA of May 8, 1998 on 13 May 1998 at 3:47:26 pm'!"Change Set:		jhmMorphCopyPasteDate:			13 May 1998Author:			John MaloneyVarious tweaks including:  morph copy/paste  fix for world halo creep  Display flash: fix from Andreas Raab  Float comment fix from Tim Olson  cleaned up chooseColor; eliminated the ColorChart    class variable in HandMorph"!!ColorPickerMorph class methodsFor: 'all' stamp: 'jm 5/13/1998 14:44'!initialize	"ColorPickerMorph initialize"	ColorChart _ Color colorPaletteForDepth: 16 extent: 190@60.	TransparentBox _ ColorChart boundingBox withHeight: 10.	FeedbackBox _ (ColorChart width - 20)@0 extent: 20@9.! !!DisplayScreen methodsFor: 'displaying' stamp: 'jm 5/13/1998 14:27'!flash: aRectangle 	"Flash the area of the screen defined by the given rectangle."	self reverse: aRectangle.	Smalltalk forceDisplayUpdate.	(Delay forMilliseconds: 100) wait.	self reverse: aRectangle.! !!Float commentStamp: 'jm 5/13/1998 15:47' prior: 0!My instances represent IEEE-754 floating-point double-precision numbers.  They have about 16 digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:		8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point.!!HandMorph class methodsFor: 'all' stamp: 'jm 5/13/1998 15:32'!initialize	"HandMorph initialize"	PasteBuffer _ nil.	DoubleClickTime _ 280.	NormalCursor _ ColorForm mappingWhiteToTransparentFrom: Cursor normal.! !!Morph methodsFor: 'menu' stamp: 'jm 5/13/1998 15:29'!addAddHandMenuItemsForHalo: aMenu hand: aHandMorph	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."	| unlockables |	aMenu addLine.	aMenu add: 'copy to paste buffer' action: #copyToPasteBuffer.	aMenu addLine.	aMenu add: 'open viewer' action: #openViewerForArgument.	aMenu add: 'change costume...' action: #chooseNewCostumeForArgument.	((self isKindOf: SketchMorph) and: [GIFImports size > 0]) ifTrue:		[aMenu add: 'use imported graphic...' action: #chooseNewFormForSketchMorph].	costumee ifNotNil:		[aMenu add: 'make another instance of me' action: #makeNewPlayerInstance].	self colorSettable ifTrue:		[aMenu add: 'fill color' action: #changeColor].	(self world morphsAt: aHandMorph argument bounds center) size > 1 ifTrue:		[aMenu add: 'place in...' action: #placeArgumentIn].	self addSensitivityItemsTo: aMenu hand: aHandMorph.	self isLocked		ifFalse:			[aMenu add: 'lock' action: #lockMorph]		ifTrue:			[aMenu add: 'unlock' action: #unlockMorph].  "probably not possible -- wouldn't get halo"	unlockables _ self submorphs select:		[:m | m isLocked].	unlockables size == 1 ifTrue:		[aMenu add: 'unlock "', unlockables first externalName, '"' action: #unlockContents].	unlockables size > 1 ifTrue:		[aMenu add: 'unlock all contents' action: #unlockContents.		aMenu add: 'unlock...' action: #unlockOneSubpart].	"aMenu add: 'make mouse-sensitive' action: #makeMouseSensitive."	(owner == nil or: [self == owner submorphs last]) ifFalse:		[aMenu add: 'send to back' action: #goBehind]! !!ColorTileMorph methodsFor: 'events' stamp: 'jm 5/13/1998 14:47'!mouseUp: evt	evt hand changeColorTarget: colorSwatch selector: #color:.	self acceptNewLiteral.! !!HaloMorph methodsFor: 'stepping' stamp: 'jm 5/13/1998 11:36'!step	| newBounds |	target ifNil: [^ self].	(newBounds _ target fullBoundsInWorld) = self fullBounds ifTrue: [^ self].	growingOrRotating ifFalse:		["adjust halo bounds if appropriate"		submorphs size > 1 ifTrue:			[self addHandles "recreates full set with new bounds"].		self bounds: newBounds]! !!HaloMorph methodsFor: 'private' stamp: 'jm 5/7/1998 16:00'!addHandles	| box |	target isWorldMorph ifTrue: [^ self addHandlesForWorldHalos].	self removeAllMorphs.  "remove old handles, if any"	self bounds: target fullBoundsInWorld.  "update my size"	box _ (self fullBounds expandBy: 17)			intersect: (self world bounds insetBy: 5@5).	(self addHandleAt: box topLeft color: Color red)		on: #mouseDown send: #doMenu:with: to: self.	(self addHandleAt: (box topLeft + (0@18)) color: Color lightBrown)		on: #mouseDown send: #tearOffTile to: innerTarget.	(self addHandleAt: (box topLeft + (18@0)) color: Color transparent)		on: #mouseDown send: #dismiss to: self.	(self addHandleAt: (box leftCenter) color: Color cyan)		on: #mouseDown send: #openViewerForArgument to: innerTarget.	(self addHandleAt: box topCenter color: Color black)		on: #mouseDown send: #doGrab:with: to: self.	(self addHandleAt: box topRight color: Color green)		on: #mouseDown send: #doDup:with: to: self.	target balloonText ifNotNil:		[(self addHandleAt: box bottomCenter color: Color lightBlue)			on: #mouseDown send: #mouseDownOnHelpHandle: to: innerTarget;			on: #mouseUp send: #deleteBalloon to: innerTarget].	(self addHandleAt: box bottomLeft color: Color blue)		on: #mouseDown send: #startRot:with: to: self;		on: #mouseStillDown send: #doRot:with: to: self.	target isFlexMorph		ifTrue: [			(self addHandleAt: box bottomRight color: Color lightOrange)				on: #mouseDown send: #startScale:with: to: self;				on: #mouseStillDown send: #doScale:with: to: self]		ifFalse: [			(self addHandleAt: box bottomRight color: Color yellow)				on: #mouseDown send: #startGrow:with: to: self;				on: #mouseStillDown send: #doGrow:with: to: self].	innerTarget addOptionalHandlesTo: self box: box.	self addNameBeneath: box string: innerTarget externalName.	growingOrRotating _ false.	self layoutChanged.	self changed.! !!HaloMorph methodsFor: 'private' stamp: 'jm 5/13/1998 11:52'!addHandlesForWorldHalos	| box |	self removeAllMorphs.  "remove old handles, if any"	self bounds: target bounds.	box _ self world bounds insetBy: 9.	(self addHandleAt: box topLeft color: Color red)		on: #mouseDown send: #doMenu:with: to: self.	(self addHandleAt: (box topLeft + (0@20)) color: Color lightBrown)		on: #mouseDown send: #tearOffTile to: target.	(self addHandleAt: box leftCenter color: Color cyan)		on: #mouseDown send: #openViewerForArgument to: target.	target balloonText ifNotNil:		[(self addHandleAt: box bottomCenter color: Color lightBlue)			on: #mouseDown send: #mouseDownOnHelpHandle: to: target;			on: #mouseUp send: #deleteBalloon to: target].	innerTarget addOptionalHandlesTo: self box: box.	self addNameBeneath: (box insetBy: (0@0 corner: 0@10)) string: innerTarget externalName.	growingOrRotating _ false.	self layoutChanged.	self changed.! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:40'!buildMorphMenuFor: argMorph	"Build the morph menu. This menu has two sections. The first section contains commands that are handled by the hand; the second contains commands handled by the argument morph."	| menu |	argument _ argMorph.	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'grab' action: #grabMorph.	menu add: 'delete' action: #dismissMorph.	menu add: 'copy to paste buffer' action: #copyToPasteBuffer.	menu add: 'go behind' action: #goBehind.	menu add: 'add halo' action: #addHalo.	menu add: 'duplicate' action: #duplicateMorph.	((self world rootMorphsAt: targetOffset) size > 1)		ifTrue: [menu add: 'embed' action: #embedMorph].	(self world morphsAt: argument bounds center) size > 1 ifTrue:		[menu add: 'place in...' action: #placeArgumentIn].	menu add: 'resize' action: #resizeMorph.	(argMorph isKindOf: SketchMorph)  ifFalse: [		menu add: 'fill color' action: #changeColor].	(argMorph morphsAt: targetOffset) size > 1 ifTrue: [		menu add: 'submorphs...'			target: self			selector: #selectSubmorphToOperateOn:sending:event:			argumentList: (Array with: argMorph with: #operateOnSubmorph:event:)].	menu addLine.	menu add: 'inspect' action: #inspectMorph.	menu add: 'inspect in Morphic' action: #inspectMorphInMorphic.	menu add: 'browse' action: #browseMorphClass.	menu add: 'make own subclass' action: #subclassMorph.	menu addLine.	menu add: 'name me' action: #nameMorph.	(argMorph isKindOf: MorphicModel) ifTrue: [		menu add: 'save morph as prototype' action: #saveAsPrototype.		(argMorph ~~ self world modelOrNil) ifTrue: [			 menu add: 'become this world''s model' action: #beThisWorldsModel]].	menu add: 'save morph in file' action: #saveMorphInFile.	menu add: 'show actions' action: #showActions.	menu addLine.	menu defaultTarget: argMorph.	argMorph addCustomMenuItems: menu hand: self.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:37'!buildWorldMenu	"Build the meta menu for the world."	| menu subMenu |	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	Project current isTopProject ifFalse:		[menu add: 'exit this world' action: #exitWorld.		menu addLine].	menu add: 'new morph...' action: #newMorph.	PasteBuffer ifNotNil: [menu add: 'paste morph' action: #pasteMorph].	World ifNotNil: [		subMenu _ MenuMorph new defaultTarget: self.		subMenu add: 'workspace' action: #openWorkspace.		subMenu add: 'browser' action: #openBrowser.		subMenu add: 'recent changes' action: #openRecentChanges.		subMenu add: 'change sorter' action: #openChangeSorter.		subMenu add: 'changes log' action: #openChangesLog.		subMenu add: 'file list' action: #openFileList.		subMenu add: 'transcript' action: #openTranscript.		subMenu add: 'project (mvc)' action: #openMVCProject.		subMenu add: 'project (morphic)' action: #openMorphicProject."coming soon...		subMenu addLine.		subMenu add: 'collapse all' action: #collapseAll.		subMenu add: 'expand all' action: #expandAll.		subMenu add: 'find window' action: #findWindow."		menu add: 'windows...' subMenu: subMenu].	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'grab drawing from screen' action: #grabDrawingFromScreen.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'make new drawing' target: self presenter associatedMorph action: #makeNewDrawingWithin.	menu add: 'graphics...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'add slot to model' action: #newVariable.	subMenu add: 'write init method for model' action: #writeInitMethodForModel.	subMenu add: 'grab model for this world' action: #grabModel.	menu add: 'model...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'change background color' action: #changeBackgroundColor.	subMenu add: 'set display depth...' action: #setDisplayDepth.	subMenu add: 'use texture background' target: self world action: #setStandardTexture.	subMenu add: 'unlock contents' action: #unlockWorldContents.	subMenu add: 'unhide hidden objects' action: #showHiders.	subMenu add: 'round up stray objects' action: #roundUpStrayObjects.	gridOn		ifTrue: [subMenu add: 'turn gridding off' action: #setGridding]		ifFalse: [subMenu add: 'turn gridding on' action: #setGridding].	menu add: 'options...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'local host address' action: #reportLocalAddress.	subMenu add: 'connect remote user' action: #connectRemoteUser.	subMenu add: 'disconnect remote user' action: #disconnectRemoteUser.	subMenu add: 'disconnect all remote users' action: #disconnectAllRemoteUsers.	menu add: 'remote...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'save world in file' action: #saveWorldInFile.	subMenu add: 'read morph(s) from file' action: #readMorphFile.	menu add: 'file...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self world.	subMenu add: 'add stop, step, & go buttons' target: self world presenter action: #addStopStepGoButtons.	subMenu add: 'add scripting knobs' target: self presenter action: #addStandardControls.	subMenu add: 'remove scripting knobs' target: self world action: #removeScriptingControls.	subMenu addLine.	subMenu add: 'parts bin' target: self presenter action: #createStandardPartsBin.	subMenu add: 'control panel' target: self presenter action: #createControlPanel.	menu add: 'scripting...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'inspect world' action: #inspectWorld.	subMenu add: 'inspect model' action: #inspectWorldModel.	subMenu add: 'clear palette area' action: #clearPaletteArea.	subMenu add: 'flush viewer cache' action: #flushViewerCache.	subMenu add: 'full screen' action: #fullScreen.	subMenu add: 'start MessageTally' action: #startMessageTally.	subMenu add: 'call #tempCommand' action: #callTempCommand.	menu add: 'debug...' subMenu: subMenu.	menu addLine.	menu add: 'save' action: #saveSession.	menu add: 'save as...' action: #saveAs.	menu add: 'save and quit' action: #saveAndQuit.	menu add: 'quit...' action: #quitSession.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:03'!changeColorTarget: aMorph selector: aSymbol	| m box points b |	m _ ColorPickerMorph new		sourceHand: self;		target: aMorph;		selector: aSymbol.	aMorph		ifNil: [box _ Rectangle center: self position extent: 50]		ifNotNil: [box _ aMorph fullBounds].	points _ #(topCenter rightCenter bottomCenter leftCenter).  "possible anchors"	1 to: 4 do: [:i |  "Try the four obvious anchor points"		b _ m bounds				align: (m bounds perform: (points at: i))				with: (box perform: (points atWrap: i + 2)).		(self worldBounds containsRect: b) ifTrue: [  "Yes, it fits"			m position: b topLeft.			self world addMorphFront: m.			m changed.			^ m]].	"when all else fails..."	m position: 20@20.	self world addMorphFront: m.	m changed.	^ m! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:12'!chooseColor	"Wait for the user to select a color and return that color."	"Details: Waiting for the user is implemented by running the interaction loop until the user has selected a color or dismisse the color picker."	| w colorPicker |	w _ self world.	colorPicker _ self changeColorTarget: nil selector: nil.	[colorPicker isInWorld] whileTrue: [w doOneCycle].	^ colorPicker selectedColor! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:38'!copyToPasteBuffer	"Save this morph in the paste buffer. This is mostly useful for copying morphs between projects."	argument isMorph		ifTrue: [PasteBuffer _ argument]		ifFalse: [PasteBuffer _ nil].! !!HandMorph methodsFor: 'meta menu' stamp: 'jm 5/13/1998 15:37'!pasteMorph	self attachMorph: PasteBuffer fullCopy.! !!Socket methodsFor: 'initialize-destroy' stamp: 'jm 5/13/1998 14:30'!initialize	"Initialize a new socket handle. If socket creation fails, socketHandle will be set to nil."	| semaIndex |	semaphore _ Semaphore new.	semaIndex _ Smalltalk registerExternalObject: semaphore.	socketHandle _		self primSocketCreateNetwork: 0			type: 0			receiveBufferSize: 8000			sendBufSize: 8000			semaIndex: semaIndex.	socketHandle = nil ifTrue: [  "socket creation failed"		Smalltalk unregisterExternalObject: semaphore.		semaphore _ nil].! !Morph subclass: #HandMorph	instanceVariableNames: 'eventSubscribers keyboardFocus mouseDownMorph mouseOverMorphs mouseOverTimes clickClient clickState firstClickEvent firstClickTime userInitials lastEvent eventTransform argument targetOffset damageRecorder cacheCanvas cachedCanvasHasHoles temporaryCursor temporaryCursorOffset grid gridOn remoteConnections transmitBuffer lastEventTransmitted lastWorldExtent menuTargetOffset hasChanged savedPatch suppressDrawing formerOwner formerPosition '	classVariableNames: 'CopiedMorph DoubleClickTime NormalCursor PasteBuffer '	poolDictionaries: ''	category: 'Morphic-Kernel'!ColorPickerMorph initialize!HandMorph initialize!