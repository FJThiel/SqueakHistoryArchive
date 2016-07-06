'From Squeak 2.1 of June 30, 1998 on 31 August 1998 at 7:53:52 pm'!StringHolder subclass: #SelectorBrowser	instanceVariableNames: 'selectorIndex selectorList classListIndex classList '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Browser'!!HandMorph methodsFor: 'meta menu' stamp: 'tk 8/31/1998 19:46'!buildWorldMenu	"Build the meta menu for the world."	| menu subMenu |	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'go back' target: owner action: #goBack.	menu add: 'jump to...'		subMenu: (Project buildJumpToMenu: (MenuMorph new defaultTarget: Project)).	menu addLine.	menu add: 'paste morph' action: #pasteMorph.	menu add: 'new morph...' action: #newMorph.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'workspace' action: #openWorkspace.	subMenu add: 'browser' action: #openBrowser.	subMenu add: 'transcript' action: #openTranscript.	subMenu add: 'file list' action: #openFileList.	subMenu add: 'parts bin' target: self presenter action: #launchMyPartsBin.	subMenu addLine.	subMenu add: 'project (mvc)' action: #openMVCProject.	subMenu add: 'project (morphic)' action: #openMorphicProject.	subMenu add: 'project (construction)' action: #openConstructionProject.	subMenu add: 'project link...' action: #projectThumbnail.	subMenu addLine.	subMenu add: 'selector finder' action: #openSelectorBrowser.	subMenu add: 'simple change sorter' selector: #openChangeSorter: argument: 1.	subMenu add: 'dual change sorter' selector: #openChangeSorter: argument: 2.	subMenu add: 'recent submisisons' action: #openRecentChanges.	subMenu add: 'changed methods' action: #browseChangedMessages.	subMenu add: 'changes log' action: #openChangesLog.	menu add: 'open new...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'collapse all' action: #collapseAll.	subMenu add: 'expand all' action: #expandAll.	subMenu add: 'find window' action: #findWindow.	menu add: 'window...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'grab drawing from screen' action: #grabDrawingFromScreen.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'make new drawing' target: self presenter associatedMorph action: #makeNewDrawingWithin.	menu add: 'graphics...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'add slot to model' action: #newVariable.	subMenu add: 'write init method for model' action: #writeInitMethodForModel.	subMenu add: 'grab model for this world' action: #grabModel.	menu add: 'model...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'restore display' action: #restoreDisplay.	subMenu add: 'set display depth...' action: #setDisplayDepth.	subMenu add: 'change background color' action: #changeBackgroundColor.	subMenu add: 'use texture background' target: self world action: #setStandardTexture.	subMenu add: 'unlock contents' action: #unlockWorldContents.	subMenu add: 'unhide hidden objects' action: #showHiders.	subMenu add: 'round up stray objects' action: #roundUpStrayObjects.	gridOn		ifTrue: [subMenu add: 'turn gridding off' action: #setGridding]		ifFalse: [subMenu add: 'turn gridding on' action: #setGridding].	menu add: 'viewing...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'update code from server' action: #updateFromServer.	subMenu addLine.	subMenu add: 'local host address' action: #reportLocalAddress.	subMenu add: 'connect remote user' action: #connectRemoteUser.	subMenu add: 'disconnect remote user' action: #disconnectRemoteUser.	subMenu add: 'disconnect all remote users' action: #disconnectAllRemoteUsers.	menu add: 'remote...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'save world in file' action: #saveWorldInFile.	subMenu add: 'read morph(s) from file' action: #readMorphFile.	menu add: 'file...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self world.	subMenu add: 'detachable scripting space' target: self action: #detachableScriptingSpace.	subMenu add: 'default parts bin' target: self presenter action: #createStandardPartsBin.	subMenu add: 'control panel' target: self presenter action: #createControlPanel.	subMenu add: 'add stop, step, and go buttons' target: self world presenter action: #addStopStepGoButtons.	menu add: 'scripting...' subMenu: subMenu.	menu add: 'do...' target: Utilities action: #offerCommonRequests.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'inspect world' action: #inspectWorld.	subMenu add: 'inspect model' action: #inspectWorldModel.	subMenu add: 'clear palette area' action: #clearPaletteArea.	subMenu add: 'flush viewer cache' action: #flushViewerCache.	subMenu add: 'full screen' action: #fullScreen.	subMenu add: 'start MessageTally' action: #startMessageTally.	subMenu add: 'call #tempCommand' action: #callTempCommand.	subMenu add: 'show space left' action: #showSpaceLeft.	menu add: 'debug...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'save' action: #saveSession.	subMenu add: 'save as...' action: #saveAs.	subMenu add: 'save and quit' action: #saveAndQuit.	subMenu add: 'quit...' action: #quitSession.	menu add: 'save / quit...' subMenu: subMenu.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'tk 8/31/1998 19:52'!openSelectorBrowser	SelectorBrowser new open! !!ScreenController methodsFor: 'menu messages' stamp: 'tk 8/31/1998 16:18'!openSelectorBrowser	"Create and schedule a selector fragment window."	SelectorBrowser new open! !!ScreenController methodsFor: 'nested menus' stamp: 'tk 8/31/1998 19:41'!openMenu	"ScreenController initialize"	OpenMenu == nil ifTrue:		[OpenMenu _ SelectionMenu labelList:		#(	'browser'			'workspace'			'file list'			'transcript'			'selector finder'			'simple change sorter'			'dual change sorter'			'project (mvc)'			'project (morphic)'			'project (construction)'			'morphic window'			'morphic construction window'			'durable open menu'			)		lines: #(7 10 12)		selections: #(openBrowser openWorkspace openFileList openTranscript openSelectorBrowser openSimpleChangeSorter openChangeManager openProject  openMorphicProject  openConstructionProject  openMorphicWorld openMorphicConstructionWorld durableOpenMenu)].	^ OpenMenu"ScreenController  new openMenu startUp"! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 14:20'!classList	^ classList! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 14:23'!classListIndex	^ classListIndex! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/27/1998 17:46'!classListIndex: anInteger	classListIndex _ anInteger.	classListIndex > 0 ifTrue: [		Browser fullOnClass: self selectedClass selector: self selectedMessageName.		"classListIndex _ 0"]! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 14:33'!classListSelectorTitle	^ 'Class List Menu'! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/31/1998 16:08'!contents: aString notifying: aController	"Take what the user typed and find all selectors containing it"	contents _ aString.	classList _ #().  classListIndex _ 0.	selectorIndex _ 0.   	selectorList _ Cursor wait showWhile: [		(Symbol selectorsContaining: contents asString) asSortedArray].	self changed: #messageList.	self changed: #classList.	^ true! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 14:19'!messageList	"Find all the selectors containing what the user typed in."	^ selectorList! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 10:58'!messageListIndex	"Answer the index of the selected message selector."	^ selectorIndex! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/31/1998 19:28'!messageListIndex: anInteger 	"Set the selected message selector to be the one indexed by anInteger.  Find all classes it is in."	selectorIndex _ anInteger.	selectorIndex = 0 ifFalse: [		classList _ Smalltalk allImplementorsOf: (selectorList at: selectorIndex).		classListIndex _ 0.		self changed: #messageListIndex.		"update my selection"		self changed: #classList]! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/31/1998 19:24'!open	"Create a Browser that lets you type part of a selector, shows a list of selectors, shows the classes of the one you chose, and spwns a full browser on it.	SelectorBrowser new open   "	|  selectorListView typeInView topView classListView |	World ifNotNil: [^ self openAsMorph].	selectorIndex _ classListIndex _ 0.	topView _ (StandardSystemView new) model: self.	topView borderWidth: 1.		"label and minSize taken care of by caller"	typeInView _ PluggableTextView on: self 			text: #contents accept: #contents:notifying:			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.	typeInView window: (0@0 extent: 100@14);		askBeforeDiscardingEdits: false.	topView addSubView: typeInView.	selectorListView _ PluggableListView on: self		list: #messageList		selected: #messageListIndex		changeSelected: #messageListIndex:		menu: nil		keystroke: #messageListKey:from:.	selectorListView menuTitleSelector: #messageListSelectorTitle.	selectorListView window: (0 @ 0 extent: 100 @ 86).	topView addSubView: selectorListView below: typeInView.	classListView _ PluggableListView on: self		list: #classList		selected: #classListIndex		changeSelected: #classListIndex:		menu: nil	"never anything selected"		keystroke: #arrowKey:from:.	classListView menuTitleSelector: #classListSelectorTitle.	classListView window: (0 @ 0 extent: 100 @ 100).	topView addSubView: classListView toRightOf: typeInView.	topView label: 'Full Browser from Selector Fragment'.	topView minimumSize: 300 @ 200; maximumSize: 450@200.	topView subViews do: [:each | each controller].	topView controller open.! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/31/1998 19:24'!openAsMorph	"Create a Browser that lets you type part of a selector, shows a list of selectors, shows the classes of the one you chose, and spwns a full browser on it.	SelectorBrowser new open   "	| window typeInView selectorListView classListView |	window _ (SystemWindow labelled: 'later') model: self.	selectorIndex _ classListIndex _ 0.	typeInView _ PluggableTextMorph on: self 		text: #contents accept: #contents:notifying:		readSelection: #contentsSelection menu: #codePaneMenu:shifted:.	"typeInView askBeforeDiscardingEdits: false."	window addMorph: typeInView frame: (0@0 corner: 0.5@0.14).	selectorListView _ PluggableListMorph on: self		list: #messageList		selected: #messageListIndex		changeSelected: #messageListIndex:		menu: nil		keystroke: #messageListKey:from:.	selectorListView menuTitleSelector: #messageListSelectorTitle.	window addMorph: selectorListView frame: (0@0.14 corner: 0.5@1).	classListView _ PluggableListMorph on: self		list: #classList		selected: #classListIndex		changeSelected: #classListIndex:		menu: nil		keystroke: #arrowKey:from:.	classListView menuTitleSelector: #classListSelectorTitle.	window addMorph: classListView frame: (0.5@0 corner: 1@1).	window setLabel: 'Full Browser from Selector Fragment'.	window openInWorldExtent: 450@200! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/27/1998 17:50'!selectedClass	"Answer the currently selected class."	classListIndex = 0 ifTrue: [^nil].	MessageSet parse: (classList at: classListIndex) 		toClassAndSelector: [:cls :sel | ^ cls].! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/27/1998 17:48'!selectedClassName	"Answer the name of the currently selected class."	classListIndex = 0 ifTrue: [^nil].	^ self selectedClass name! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'tk 8/26/1998 14:29'!selectedMessageName	"Answer the name of the currently selected message."	selectorIndex = 0 ifTrue: [^nil].	^ selectorList at: selectorIndex! !!String methodsFor: 'accessing' stamp: 'tk 8/31/1998 17:42'!findInsensitive: subStringLowercase allUpper: subStringUppercase	"Answer the index of subString within the receiver case insensitive. If the receiver does not contain subString, answer 0.  subStringLowercase and subStringUppercase must be identical except one is all lowercase and the other is all uppercase.  (pass these in so don't have to generate them every time when searching all symbols)  Caller must test for subStringLowercase being empty."	| index firstLo firstHi |	firstLo _ subStringLowercase first.	firstHi _ subStringUppercase first.	1 to: self size - subStringLowercase size + 1 do:		[:startIndex |		((self at: startIndex) = firstLo or: [(self at: startIndex) = firstHi]) ifTrue:			[index _ 1.			[(self at: startIndex+index-1) = (subStringLowercase at: index) or:				[(self at: startIndex+index-1) = (subStringUppercase at: index)]]					whileTrue:					[index = subStringLowercase size ifTrue: [^startIndex].					index _ index+1]]].	^ 0! !!Symbol class methodsFor: 'access' stamp: 'tk 8/31/1998 17:48'!selectorsContaining: aString	"Answer a list of selectors that contain aString within them.  Case-insensitive."	| size table candidate selectorList selectorTable keyLo keyHi |	keyLo _ aString asLowercase.	keyHi _ aString asUppercase.	selectorList _ OrderedCollection new.	keyLo size = 0 ifTrue: [^ selectorList].	"this test is necessary!!"	size _ keyLo size.	(SelectorTables size to: 1 by: -1) do:		[:j | selectorTable _ SelectorTables at: j.		1 to: 26 do: [:index |		table _ selectorTable at: index.		1 to: table size do: 			[:t | 			((candidate _ table at: t) == nil) ifFalse:				[candidate size >= size ifTrue:					[((candidate findInsensitive: keyLo 						allUpper: keyHi) > 0) ifTrue:							[selectorList add: candidate]]]]]].	^ selectorList"Symbol selectorsContaining: 'scon'    "! !ScreenController initialize.!