'From Squeak 2.1 of June 30, 1998 on 8 September 1998 at 9:47:35 pm'!"Change Set:		world menuDate:			8 September 1998Author:			Lex Spoonupdates Morphic world menu with the web browser, mail reader, and IRC client"!!HandMorph methodsFor: 'meta menu' stamp: 'ls 9/8/1998 04:58'!buildWorldMenu	"Build the meta menu for the world."	| menu subMenu |	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'go back' target: owner action: #goBack.	menu add: 'jump to...'		subMenu: (Project buildJumpToMenu: (MenuMorph new defaultTarget: Project)).	menu addLine.	menu add: 'paste morph' action: #pasteMorph.	menu add: 'new morph...' action: #newMorph.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'workspace' action: #openWorkspace.	subMenu add: 'browser' action: #openBrowser.	subMenu add: 'transcript' action: #openTranscript.	subMenu add: 'file list' action: #openFileList.	subMenu add: 'parts bin' target: self presenter action: #launchMyPartsBin.	subMenu addLine.	subMenu add: 'email reader' action: #openEmail.	subMenu add: 'web browser' action: #openWebBrowser.	subMenu add: 'IRC' action: #openIRC.	subMenu addLine.	subMenu add: 'project (mvc)' action: #openMVCProject.	subMenu add: 'project (morphic)' action: #openMorphicProject.	subMenu add: 'project (construction)' action: #openConstructionProject.	subMenu add: 'project link...' action: #projectThumbnail.	subMenu addLine.	subMenu add: 'simple change sorter' selector: #openChangeSorter: argument: 1.	subMenu add: 'dual change sorter' selector: #openChangeSorter: argument: 2.	subMenu add: 'recent submisisons' action: #openRecentChanges.	subMenu add: 'changed methods' action: #browseChangedMessages.	subMenu add: 'changes log' action: #openChangesLog.	menu add: 'open new...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'collapse all' action: #collapseAll.	subMenu add: 'expand all' action: #expandAll.	subMenu add: 'find window' action: #findWindow.	menu add: 'window...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'grab drawing from screen' action: #grabDrawingFromScreen.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'make new drawing' target: self presenter associatedMorph action: #makeNewDrawingWithin.	menu add: 'graphics...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'add slot to model' action: #newVariable.	subMenu add: 'write init method for model' action: #writeInitMethodForModel.	subMenu add: 'grab model for this world' action: #grabModel.	menu add: 'model...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'restore display' action: #restoreDisplay.	subMenu add: 'set display depth...' action: #setDisplayDepth.	subMenu add: 'change background color' action: #changeBackgroundColor.	subMenu add: 'use texture background' target: self world action: #setStandardTexture.	subMenu add: 'unlock contents' action: #unlockWorldContents.	subMenu add: 'unhide hidden objects' action: #showHiders.	subMenu add: 'round up stray objects' action: #roundUpStrayObjects.	gridOn		ifTrue: [subMenu add: 'turn gridding off' action: #setGridding]		ifFalse: [subMenu add: 'turn gridding on' action: #setGridding].	menu add: 'viewing...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'update code from server' action: #updateFromServer.	subMenu addLine.	subMenu add: 'local host address' action: #reportLocalAddress.	subMenu add: 'connect remote user' action: #connectRemoteUser.	subMenu add: 'disconnect remote user' action: #disconnectRemoteUser.	subMenu add: 'disconnect all remote users' action: #disconnectAllRemoteUsers.	menu add: 'remote...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'save world in file' action: #saveWorldInFile.	subMenu add: 'read morph(s) from file' action: #readMorphFile.	menu add: 'file...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self world.	subMenu add: 'detachable scripting space' target: self action: #detachableScriptingSpace.	subMenu add: 'default parts bin' target: self presenter action: #createStandardPartsBin.	subMenu add: 'control panel' target: self presenter action: #createControlPanel.	subMenu add: 'add stop, step, and go buttons' target: self world presenter action: #addStopStepGoButtons.	menu add: 'scripting...' subMenu: subMenu.	menu add: 'do...' target: Utilities action: #offerCommonRequests.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'inspect world' action: #inspectWorld.	subMenu add: 'inspect model' action: #inspectWorldModel.	subMenu add: 'clear palette area' action: #clearPaletteArea.	subMenu add: 'flush viewer cache' action: #flushViewerCache.	subMenu add: 'full screen' action: #fullScreen.	subMenu add: 'start MessageTally' action: #startMessageTally.	subMenu add: 'call #tempCommand' action: #callTempCommand.	subMenu add: 'show space left' action: #showSpaceLeft.	subMenu add: 'vm statistics' action: #vmStatistics.	menu add: 'debug...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'save' action: #saveSession.	subMenu add: 'save as...' action: #saveAs.	subMenu add: 'save and quit' action: #saveAndQuit.	subMenu add: 'quit...' action: #quitSession.	menu add: 'save / quit...' subMenu: subMenu.	^ menu! !!HandMorph methodsFor: 'meta menu' stamp: 'ls 9/8/1998 04:57'!openEmail	"open an email interface"	Celeste openOn: 'EMAIL'! !!HandMorph methodsFor: 'meta menu' stamp: 'ls 9/5/1998 01:22'!openIRC	"open an interface to Internet Relay Chat"	IRCConnection new openView; openDirectMessagesObserver! !!HandMorph methodsFor: 'meta menu' stamp: 'ls 7/16/1998 20:05'!openWebBrowser	Scamper openAsMorph openInMVC ! !