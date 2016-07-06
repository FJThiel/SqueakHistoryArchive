'From Squeak2.9alpha of 13 June 2000 [latest update: #2904] on 26 October 2000 at 3:53:39 pm'!"Change Set:		FollowThatSqueakDate:			26 October 2000Author:			Andreas Raab... into different projects from Nebraska. The CS transfers any remote connection established with the current world into a newly opened world so all visitors will follow."!!HandMorph methodsFor: 'initialization' stamp: 'ar 10/26/2000 14:58'!initialize	super initialize.	self initForEvents.	keyboardFocus _ nil.	mouseFocus _ nil.	bounds _ 0@0 extent: Cursor normal extent.	userInitials _ ''.	damageRecorder _ DamageRecorder new.	cachedCanvasHasHoles _ false.	temporaryCursor _ temporaryCursorOffset _ nil.	self initForEvents.! !!HandMorph methodsFor: 'accessing' stamp: 'ar 10/26/2000 15:18'!userPicture	^self valueOfProperty: #remoteUserPicture! !!HandMorph methodsFor: 'accessing' stamp: 'ar 10/26/2000 15:34'!userPicture: aFormOrNil	^self setProperty: #remoteUserPicture toValue: aFormOrNil! !!HandMorph methodsFor: 'cursor' stamp: 'ar 10/26/2000 15:28'!showTemporaryCursor: cursorOrNil hotSpotOffset: hotSpotOffset	"Set the temporary cursor to the given Form.	If the argument is nil, revert to the normal hardware cursor."	self changed.	cursorOrNil == nil		ifTrue: [temporaryCursor _ temporaryCursorOffset _ nil]		ifFalse: [temporaryCursor _ cursorOrNil asCursorForm.				temporaryCursorOffset _ temporaryCursor offset - hotSpotOffset].	bounds _ self cursorBounds.	self 		userInitials: userInitials andPicture: (self userPicture);		layoutChanged;		changed! !!HandMorph methodsFor: 'geometry' stamp: 'ar 10/26/2000 15:34'!userInitials: aString andPicture: aForm	| qp cb pictRect initRect |	userInitials _ aString.	pictRect _ initRect _ cb _ self cursorBounds.	userInitials isEmpty ifFalse: [		qp _ DisplayScanner quickPrintOn: Display.		initRect _ cb topRight + (0@4) extent: (qp stringWidth: userInitials)@(qp lineHeight).	].	self userPicture: aForm.	aForm ifNotNil: [		pictRect _ (self cursorBounds topRight + (0@24)) extent: aForm extent.	].	self bounds: ((cb merge: initRect) merge: pictRect).! !!HandMorph methodsFor: 'drawing' stamp: 'ar 10/26/2000 15:33'!drawOn: aCanvas	| userPic |	"Draw the hand itself (i.e., the cursor)."	temporaryCursor == nil		ifTrue: [aCanvas paintImage: NormalCursor at: bounds topLeft]		ifFalse: [aCanvas paintImage: temporaryCursor at: bounds topLeft].	self hasUserInformation ifTrue: [		aCanvas 			text: userInitials			at: (self cursorBounds topRight + (0@4))			font: nil			color: color.		(userPic _ self userPicture) ifNotNil: [			aCanvas paintImage: userPic at: (self cursorBounds topRight + (0@24))		].	].! !!HandMorph methodsFor: 'drawing' stamp: 'ar 10/26/2000 15:33'!hasUserInformation	^self userInitials size > 0 or: [self userPicture notNil]! !!HandMorph methodsFor: 'drawing' stamp: 'ar 10/26/2000 15:15'!needsToBeDrawn	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor. This is the case if it (a) it is a remote hand, (b) it is showing a temporary cursor, or (c) it is not empty. If using the software cursor, ensure that the hardware cursor is hidden."	"Details:  Return true if this hand has a saved patch to ensure that is is processed by the world. This saved patch will be deleted after one final display pass when it becomes possible to start using the hardware cursor again. This trick gives us one last display cycle to allow us to remove the software cursor and shadow from the display."	(savedPatch notNil or:[(submorphs size > 0) or:[temporaryCursor ~~ nil or:[self hasUserInformation]]])		ifTrue: [			"using the software cursor; hide the hardware one"			Sensor currentCursor == Cursor blank ifFalse: [Cursor blank show].			^ true].	^ false! !!HandMorph methodsFor: 'drawing' stamp: 'ar 10/26/2000 15:07'!restoreSavedPatchOn: aCanvas	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."	hasChanged _ false.	savedPatch ifNotNil: [		aCanvas drawImage: savedPatch at: savedPatch offset.		self hasUserInformation ifTrue: [^self].	"cannot use hw cursor if so"		submorphs size > 0 ifTrue: [^self].		temporaryCursor ifNotNil: [^self].		"Make the transition to using hardware cursor. Clear savedPatch and		 report one final damage rectangle to erase the image of the software cursor."		super invalidRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).		Sensor currentCursor == Cursor normal ifFalse: [Cursor normal show].  "show hardware cursor"		savedPatch _ nil.	].! !!HandMorph methodsFor: 'private' stamp: 'ar 10/26/2000 15:28'!releaseCachedState	| oo ui |	ui _ userInitials.	super releaseCachedState.	cacheCanvas _ nil.	oo _ owner.	self removeAllMorphs.	self initialize.	"nuke everything"	self privateOwner: oo.	self releaseAllFoci.	self userInitials: ui andPicture: (self userPicture).! !!NebraskaServerMorph class methodsFor: 'as yet unclassified' stamp: 'ar 10/26/2000 14:55'!serveWorld: aWorld	^self serveWorld: aWorld onPort: NebraskaServer defaultPort! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/26/2000 15:25'!handlesMouseOver: evt	^true! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/26/2000 15:24'!mouseEnter: evt	evt hand newKeyboardFocus: self.	evt hand needsToBeDrawn ifTrue:[Cursor blank show].! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/26/2000 15:25'!mouseLeave: evt	evt hand needsToBeDrawn ifTrue:[Cursor normal show].! !!PasteUpMorph methodsFor: 'world state' stamp: 'ar 10/26/2000 14:52'!addHand: aHandMorph	"Add the given hand to the list of hands for this world."	aHandMorph owner ifNotNil:[aHandMorph owner removeHand: aHandMorph].	worldState addHand: aHandMorph.	aHandMorph privateOwner: self.! !!PasteUpMorph methodsFor: 'Nebraska' stamp: 'ar 10/26/2000 15:03'!hasRemoteServer	^self remoteServer notNil! !!PasteUpMorph methodsFor: 'Nebraska' stamp: 'ar 10/26/2000 14:44'!releaseRemoteServer	"My server has been transferred to some other world. Release pending references"	^worldState releaseRemoteServer.! !!PasteUpMorph methodsFor: 'Nebraska' stamp: 'ar 10/26/2000 15:20'!remoteServer: aNebraskaServer	| h |	worldState remoteServer: aNebraskaServer.	h _ self primaryHand.	aNebraskaServer 		ifNil:[h userInitials = 'host' ifTrue:[h userInitials: '' andPicture: nil]]		ifNotNil:[h hasUserInformation ifFalse:[h userInitials: 'host' andPicture: nil]].! !!PasteUpMorph methodsFor: 'Nebraska' stamp: 'ar 10/26/2000 15:19'!transferRemoteServerFrom: aWorld	"Transfer the remote server which was associated with aWorld (if any) to the receiver"	| server |	(aWorld notNil and:[aWorld isMorph and:[aWorld isWorldMorph]]) ifFalse:[^self].	server _ aWorld remoteServer.	server ifNotNil:[		self remoteServer: server.		server clients do:[:each| self addRemoteClient: each].		self primaryHand			userInitials: (aWorld primaryHand userInitials)			andPicture: (aWorld primaryHand userPicture).		aWorld primaryHand userInitials: '' andPicture: nil].	aWorld releaseRemoteServer.! !!Project methodsFor: 'menu messages' stamp: 'ar 10/26/2000 14:45'!enter: returningFlag revert: revertFlag saveForRevert: saveForRevert	"Install my ChangeSet, Transcript, and scheduled views as current globals. If returningFlag is true, we will return to the project from whence the current project was entered; don't change its previousProject link in this case.	If saveForRevert is true, save the ImageSegment of the project being left.	If revertFlag is true, make stubs for the world of the project being left.	If revertWithoutAsking is true in the project being left, then always revert."	| showZoom recorderOrNil old forceRevert response seg newProcess |	(world isKindOf: StringMorph) ifTrue: [		self inform: 'This project is not all here. I will try to load a complete version.'.		^self loadFromServer: true	"try to get a fresh copy"	].	self isCurrentProject ifTrue: [^ self].	"Check the guards"	guards ifNotNil:		[guards _ guards reject: [:obj | obj isNil].		guards do: [:obj | obj okayToEnterProject ifFalse: [^ self]]].	forceRevert _ false.	CurrentProject rawParameters 		ifNil: [revertFlag ifTrue: [^ self inform: 'nothing to revert to']]		ifNotNil: [saveForRevert ifFalse: [				forceRevert _ CurrentProject projectParameters 								at: #revertWithoutAsking ifAbsent: [false]]].	forceRevert not & revertFlag ifTrue: [		response _ SelectionMenu confirm: 'Are you sure you want to destroy this Project\ and revert to an older version?\\(From the parent project, click on this project''s thumbnail.)' withCRs			trueChoice: 'Revert to saved version' 			falseChoice: 'Cancel'.		response ifFalse: [^ self]].	revertFlag | forceRevert 		ifTrue: [seg _ CurrentProject projectParameters at: #revertToMe ifAbsent: [					^ self inform: 'nothing to revert to']]		ifFalse: [			CurrentProject makeThumbnail.			returningFlag == #specialReturn ifTrue: [				ProjectHistory forget: CurrentProject.		"this guy is irrelevant"			] ifFalse: [				ProjectHistory remember: CurrentProject.			].		].	(revertFlag | saveForRevert | forceRevert) ifFalse: [		(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [			self storeToMakeRoom]].	Smalltalk isMorphic ifTrue: [Display bestGuessOfCurrentWorld triggerClosingScripts].	"Update the display depth and make a thumbnail of the current project"	CurrentProject displayDepth: Display depth.	old _ CurrentProject.		"for later"	"Show the project transition.	Note: The project zoom is run in the context of the old project,		so that eventual errors can be handled accordingly"	displayDepth == nil ifTrue: [displayDepth _ Display depth].	self installNewDisplay: Display extent depth: displayDepth.	(showZoom _ self showZoom) ifTrue: [		self displayZoom: CurrentProject parent ~~ self].	(world isMorph and: [world hasProperty: #letTheMusicPlay])		ifTrue: [world removeProperty: #letTheMusicPlay]		ifFalse: [Smalltalk at: #ScorePlayer ifPresent: [:playerClass | 					playerClass allSubInstancesDo: [:player | player pause]]].	returningFlag == #specialReturn ifTrue: [		old removeChangeSetIfPossible.	"keep this stuff from accumulating"		nextProject _ nil	] ifFalse: [		returningFlag			ifTrue: [nextProject _ CurrentProject]			ifFalse: [previousProject _ CurrentProject].	].	CurrentProject saveState.	CurrentProject isolationHead == self isolationHead ifFalse:		[self invokeFrom: CurrentProject].	CurrentProject _ self.	Smalltalk newChanges: changeSet.	TranscriptStream newTranscript: transcript.	Sensor flushKeyboard.	Smalltalk isMorphic ifTrue: [recorderOrNil _ Display pauseMorphicEventRecorder].	ProjectHistory remember: CurrentProject.	world isMorph		ifTrue:			[Display changeMorphicWorldTo: world.  "Signifies Morphic"			world install.			world transferRemoteServerFrom: old world.			"(revertFlag | saveForRevert | forceRevert) ifFalse: [				(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [					self storeSomeSegment]]."			recorderOrNil ifNotNil: [recorderOrNil resumeIn: world].			world triggerOpeningScripts]		ifFalse:			[Display changeMorphicWorldTo: nil.  "Signifies MVC"			Smalltalk at: #ScheduledControllers put: world].	saveForRevert ifTrue: [		Smalltalk garbageCollect.	"let go of pointers"		old storeSegment.		"result _" old world isInMemory 			ifTrue: ['Can''t seem to write the project.']			ifFalse: [old projectParameters at: #revertToMe put: 					old world xxxSegment clone].				'Project written.'].			"original is for coming back in and continuing."	revertFlag | forceRevert ifTrue: [		seg clone revert].	"non-cloned one is for reverting again later"	self removeParameter: #exportState.	"Complete the enter: by launching a new process"	world isMorph ifTrue: [		self finalEnterActions.		world repairEmbeddedWorlds.		Project spawnNewProcessAndTerminateOld: true	] ifFalse: [		SystemWindow clearTopWindow.	"break external ref to this project"		newProcess _ [				ScheduledControllers resetActiveController.	"in case of walkback in #restore"			showZoom ifFalse: [ScheduledControllers restore].			ScheduledControllers searchForActiveController		] fixTemps newProcess priority: Processor userSchedulingPriority.		newProcess resume.		"lose the current process and its referenced morphs"		Processor terminateActive.	]! !!WorldState methodsFor: 'hands' stamp: 'ar 10/26/2000 14:51'!addHand: aHandMorph	"Add the given hand to the list of hands for this world."	hands _ (hands copyWithout: aHandMorph) copyWith: aHandMorph.! !!WorldState methodsFor: 'object fileIn' stamp: 'ar 10/26/2000 15:41'!converthavcdsllcal0: varDict havcdslllcalrm0: smartRefStrm	"These variables are automatically stored into the new instance #('hands' 'activeHand' 'viewBox' 'canvas' 'damageRecorder' 'stepList' 'lastStepTime' 'lastCycleTime' 'commandHistory' 'alarms' 'lastAlarmTime').	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: #('lastStepMessage' 'remoteServer' 'multiCanvas')  If a non-nil value is needed, please assign it."	"Convert the old to new step lists"	self convertStepList.! !!WorldState methodsFor: 'Nebraska support' stamp: 'ar 10/26/2000 14:44'!releaseRemoteServer	"My server has been transferred to some other world. Release pending references"	remoteServer _ nil.	self canvas: nil.! !