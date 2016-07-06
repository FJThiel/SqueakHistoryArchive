'From Squeak 2.3 of January 14, 1999 on 8 March 1999 at 10:42:36 pm'!"Change Set:		trash-swDate:			8 March 1999Author:			Scott WallaceAdds a general scraps-book facility to morphic; things dropped into a trashcan are deposited onto pages of a scraps book, from whence they can be retrieved.  Works cross-project.  Can open up the scraps book either by clicking on a trash can or from a new menu item in the authoring-tools menu, which also now sports a command for emptying said trash book."!Object subclass: #Utilities	instanceVariableNames: ''	classVariableNames: 'AuthorInitials CommonRequestStrings FlapTabs RecentSubmissions ScrapsBook UpdateUrlLists '	poolDictionaries: ''	category: 'System-Support'!!HandMorph methodsFor: 'world menu' stamp: 'sw 3/8/1999 01:40'!scriptingMenu	"Build the scripting menu for the world."	| menu |	menu _ (MenuMorph entitled: 'authoring tools') defaultTarget: self.	menu addStayUpItem.	menu add: 'standard parts bin' target: self presenter action: #createStandardPartsBin.	menu balloonTextForLastItem: 'A bin of standard parts, from which you can drag out useful morphs.'.	menu add: 'custom parts bin' target: self presenter action: #launchCustomPartsBin.	menu balloonTextForLastItem: 'A customized bin of parts.  To define what the custom parts bin is, edit any existing parts bin and tell it to be saved as the custom parts bin.'.	menu add: 'view trash contents' target: self action: #openScrapsBook.	menu balloonTextForLastItem: 'The place where all your trashed morphs go.'.	menu add: 'empty trash can' target: Utilities action: #emptyScrapsBook.	menu balloonTextForLastItem: 'Empty out all the morphs that have accumulated in the trash can.'.	menu add: 'new scripting area' target: self action: #detachableScriptingSpace.	menu balloonTextForLastItem: 'A window set up for simple scripting.'.	menu addLine.	menu add: 'unlock locked objects' action: #unlockWorldContents.	menu balloonTextForLastItem: 'If any items on the world desktop are currently locked, unlock them.'.	menu add: 'unhide hidden objects' action: #showHiders.	menu balloonTextForLastItem: 'If any items on the world desktop are currently hidden, make them visible.'.	menu add: 'round up stray objects' action: #roundUpStrayObjects.	menu balloonTextForLastItem: 'If any items on the desktop are currently off-screen (because their coordinates are outside the bounds of the desktop), bring them back within view.'.	^ menu! !!HandMorph methodsFor: 'world menu commands' stamp: 'sw 3/7/1999 01:22'!openScrapsBook	self attachMorph: Utilities scrapsBook! !!Presenter methodsFor: 'button creation' stamp: 'sw 3/7/1999 00:52'!addTrashCan	| aPosition aCan |	(aCan _ associatedMorph findA: TrashCanMorph) ifNotNil: [^ aCan].	aCan _ TrashCanMorph newSticky.	aPosition _ associatedMorph positionNear: (associatedMorph bottomRight - aCan extent) forExtent: aCan extent adjustmentSuggestion:  (-10 @ 0).	aCan position: aPosition.	associatedMorph addMorph: aCan.	aCan startStepping.	aCan setToAdhereToEdge: #bottomRight.	^ aCan! !!TabbedPalette methodsFor: 'scraps tab' stamp: 'sw 3/7/1999 00:44'!addScrapsTab	| scrapsBook |	self hasScrapsTab ifTrue: [^ self beep].	scrapsBook _ BookMorph new pageSize: pageSize; setNameTo: 'scraps'.	scrapsBook removeEverything; showPageControls; insertPage.	scrapsBook currentPage addMorph: (SketchMorph new form: ScriptingSystem squeakyMouseForm).	scrapsBook setProperty: #scraps toValue: true.	self addTabForBook: scrapsBook  withBalloonText: 'a storage place for anything; also, objects dragged into the trash can will be found here.'.	self presenter ownStandardPalette ifNil:		[self becomeStandardPalette]! !!TrashCanMorph methodsFor: 'initialization' stamp: 'sw 3/7/1999 00:53'!initialize	super initialize.	self image: TrashPicOn;		offImage: TrashPic;		pressedImage: TrashPicOn.	self setNameTo: 'Trash'.	self setBalloonText:'To remove an object, drop it on this trash can.'.! !!TrashCanMorph methodsFor: 'event handling' stamp: 'sw 3/8/1999 14:57'!mouseDown: evt	| palette |	self currentHand endDisplaySuppression.	palette _ self standardPalette.	((palette notNil and: [palette isInWorld]) and: [palette hasScrapsTab])		ifTrue:			[palette showScrapsTab]		ifFalse:			[self currentHand openScrapsBook]! !!TrashCanMorph methodsFor: 'dropping' stamp: 'sw 3/8/1999 12:21'!acceptDroppingMorph: aMorph event: evt	| palette |	Preferences soundsEnabled ifTrue: [self class playDeleteSound].	evt hand endDisplaySuppression.	self state: #off.	aMorph delete.	palette _ self standardPalette.	(palette notNil and: [palette isInWorld])		ifTrue:			[palette addToTrash: aMorph]		ifFalse:			[aMorph == Utilities scrapsBook ifFalse:				[Utilities addToTrash: aMorph]]! !!TrashCanMorph methodsFor: 'dropping' stamp: 'sw 3/7/1999 02:14'!wantsDroppedMorph: aMorph event: evt	^ (aMorph ~~ self) and: [aMorph ~~ Utilities scrapsBook]! !!Utilities class methodsFor: 'scraps' stamp: 'sw 3/7/1999 02:32'!addToTrash: aMorph	"Paste the object onto a page of my Scraps tab. This is only called in situations where a presenter has a standard palette specified."	| aBook aPage |	aBook _ self scrapsBook.	aMorph position: aBook pages first position + (0@40).	aBook pages do: [:pp | 		(pp submorphs size = 1 and: [pp hasProperty: #trash]) ifTrue:  "perhaps remove that property here"			["page is blank"			^ pp addMorph: aMorph]].	aPage _ aBook insertPageLabel: Time dateAndTimeNow printString		morphs: (Array with: aMorph).	aPage setProperty: #trash toValue: true.! !!Utilities class methodsFor: 'scraps' stamp: 'sw 3/8/1999 10:16'!emptyScrapsBook	"Utilities emptyScrapsBook"	| oldScraps |	oldScraps _ ScrapsBook.	ScrapsBook _ nil.	self scrapsBook.  "Creates it afresh"	(oldScraps notNil and: [oldScraps isInWorld])		ifTrue:			[ScrapsBook position: oldScraps position.			oldScraps owner replaceSubmorph: oldScraps by: ScrapsBook.			ScrapsBook changed; layoutChanged]! !!Utilities class methodsFor: 'scraps' stamp: 'sw 3/8/1999 10:46'!scrapsBook	| header aButton |	ScrapsBook ifNil:		[ScrapsBook _ BookMorph new pageSize: 200@300; setNameTo: 'scraps'.		ScrapsBook color: Color yellow muchLighter.		ScrapsBook borderColor: Color darkGray; borderWidth: 2.		ScrapsBook removeEverything; showPageControls; insertPage.		header _ AlignmentMorph newRow centering: #center.		header setProperty: #header toValue: true.		header addMorph: (aButton _ SimpleButtonMorph new label: 'X' font: ScriptingSystem fontForScriptorButtons).		aButton target: ScrapsBook; color:  Color lightRed; actionSelector: #delete;				setBalloonText: 'Delete (click on a trashcan to view again).'.		header addMorphBack: AlignmentMorph newVariableTransparentSpacer beSticky.		header addMorphBack: 	(StringMorph contents: 'T r a s h') beSticky.		header addMorphBack: AlignmentMorph newVariableTransparentSpacer beSticky.		header addTransparentSpacerOfSize: 16@0.		ScrapsBook currentPage addMorph: (TextMorph new contents: 'Objects you drag into the trash will automatically be saved here, one object per page, in case you need them later.You can always open up this book by clicking on a trash can.You can retrieve objects from the trash just by dragging them out from here.You can individually expunge objects by hitting the - control, and you can empty out all the objects in the trash can via the menu item "empty trash can" in the authoring-tools menu'			wrappedTo: 190).		ScrapsBook addMorphFront: header.		ScrapsBook setProperty: #scraps toValue: true].	^ ScrapsBook	"Utilities emptyScrapsBook"! !!Utilities class methodsFor: 'scraps' stamp: 'sw 3/7/1999 02:23'!scrapsBook: aBook	"Utilities scrapsBook: nil"	ScrapsBook _ nil! !