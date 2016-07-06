'From Squeak 2.1 of June 30, 1998 on 16 September 1998 at 12:24:10 am'!"Change Set:		misc2-lsDate:			16 September 1998Author:			Lex Spoon	1. makes active URL links work from within text panes	2. Celeste works in MVC	3. Brought another HTTP method up to date"!!Celeste methodsFor: 'sending mail' stamp: 'ls 9/16/1998 00:18'!openSender: initialText	"open a message sender with the given initial text"	Smalltalk isMorphic ifTrue: [ ^self openSenderInMorphic: initialText ].	^self openSenderInMVC: initialText! !!Celeste methodsFor: 'sending mail' stamp: 'ls 9/16/1998 00:21'!openSenderInMVC: initialText	| topWindow textHolder textView sendButton  |	topWindow _ StandardSystemView new		label: 'Mister Postman';		minimumSize: 400@250.	textHolder _ StringHolder new .	textHolder contents: initialText.	textView _ PluggableTextView		on: textHolder		text: #contents		accept: #aceptContents:.	sendButton _ PluggableButtonView 		on: [			textView hasUnacceptedEdits ifTrue: [ textView controller accept ].			self queueMessageWithText: textHolder contents.			topWindow controller close. ] fixTemps		getState: nil		action: #value.	sendButton label: 'Send'.	sendButton borderWidth: 1.	sendButton window: (1@1 extent: 398@38).	topWindow addSubView: sendButton.	textView window: (0@40 corner: 400@250).	topWindow addSubView: textView below: sendButton.	topWindow controller open.		! !!HTTPSocket class methodsFor: 'examples' stamp: 'ls 9/15/1998 23:57'!httpGet: url accept: mimeType	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIMI type 'application/octet-stream'."	^self httpGet: url  args: nil accept: mimeType! !!HandMorph methodsFor: 'meta menu' stamp: 'ls 9/16/1998 00:17'!buildWorldMenu	"Build the meta menu for the world."	| menu subMenu |	menu _ MenuMorph new defaultTarget: self.	menu addStayUpItem.	menu add: 'go back' target: owner action: #goBack.	menu add: 'jump to...'		subMenu: (Project buildJumpToMenu: (MenuMorph new defaultTarget: Project)).	menu addLine.	menu add: 'paste morph' action: #pasteMorph.	menu add: 'new morph...' action: #newMorph.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'workspace' action: #openWorkspace.	subMenu add: 'browser' action: #openBrowser.	subMenu add: 'transcript' action: #openTranscript.	subMenu add: 'file list' action: #openFileList.	subMenu add: 'parts bin' target: self presenter action: #launchMyPartsBin.	subMenu addLine.	subMenu add: 'email reader' action: #openEmail.	subMenu add: 'web browser' action: #openWebBrowser.	"subMenu add: 'IRC' action: #openIRC."	subMenu addLine.	subMenu add: 'project (mvc)' action: #openMVCProject.	subMenu add: 'project (morphic)' action: #openMorphicProject.	subMenu add: 'project (construction)' action: #openConstructionProject.	subMenu add: 'project link...' action: #projectThumbnail.	subMenu addLine.	subMenu add: 'simple change sorter' selector: #openChangeSorter: argument: 1.	subMenu add: 'dual change sorter' selector: #openChangeSorter: argument: 2.	subMenu add: 'recent submisisons' action: #openRecentChanges.	subMenu add: 'changed methods' action: #browseChangedMessages.	subMenu add: 'changes log' action: #openChangesLog.	menu add: 'open new...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'collapse all' action: #collapseAll.	subMenu add: 'expand all' action: #expandAll.	subMenu add: 'find window' action: #findWindow.	menu add: 'window...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'grab drawing from screen' action: #grabDrawingFromScreen.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'make new drawing' target: self presenter associatedMorph action: #makeNewDrawingWithin.	menu add: 'graphics...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'add slot to model' action: #newVariable.	subMenu add: 'write init method for model' action: #writeInitMethodForModel.	subMenu add: 'grab model for this world' action: #grabModel.	menu add: 'model...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'restore display' action: #restoreDisplay.	subMenu add: 'set display depth...' action: #setDisplayDepth.	subMenu add: 'change background color' action: #changeBackgroundColor.	subMenu add: 'use texture background' target: self world action: #setStandardTexture.	subMenu add: 'unlock contents' action: #unlockWorldContents.	subMenu add: 'unhide hidden objects' action: #showHiders.	subMenu add: 'round up stray objects' action: #roundUpStrayObjects.	gridOn		ifTrue: [subMenu add: 'turn gridding off' action: #setGridding]		ifFalse: [subMenu add: 'turn gridding on' action: #setGridding].	menu add: 'viewing...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'update code from server' action: #updateFromServer.	subMenu addLine.	subMenu add: 'local host address' action: #reportLocalAddress.	subMenu add: 'connect remote user' action: #connectRemoteUser.	subMenu add: 'disconnect remote user' action: #disconnectRemoteUser.	subMenu add: 'disconnect all remote users' action: #disconnectAllRemoteUsers.	menu add: 'remote...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'read drawing from file' action: #importImageFromDisk.	subMenu add: 'save world in file' action: #saveWorldInFile.	subMenu add: 'read morph(s) from file' action: #readMorphFile.	menu add: 'file...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self world.	subMenu add: 'detachable scripting space' target: self action: #detachableScriptingSpace.	subMenu add: 'default parts bin' target: self presenter action: #createStandardPartsBin.	subMenu add: 'control panel' target: self presenter action: #createControlPanel.	subMenu add: 'add stop, step, and go buttons' target: self world presenter action: #addStopStepGoButtons.	menu add: 'scripting...' subMenu: subMenu.	menu add: 'do...' target: Utilities action: #offerCommonRequests.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'inspect world' action: #inspectWorld.	subMenu add: 'inspect model' action: #inspectWorldModel.	subMenu add: 'clear palette area' action: #clearPaletteArea.	subMenu add: 'flush viewer cache' action: #flushViewerCache.	subMenu add: 'full screen' action: #fullScreen.	subMenu add: 'start MessageTally' action: #startMessageTally.	subMenu add: 'call #tempCommand' action: #callTempCommand.	subMenu add: 'show space left' action: #showSpaceLeft.	subMenu add: 'vm statistics' action: #vmStatistics.	menu add: 'debug...' subMenu: subMenu.	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'save' action: #saveSession.	subMenu add: 'save as...' action: #saveAs.	subMenu add: 'save and quit' action: #saveAndQuit.	subMenu add: 'quit...' action: #quitSession.	menu add: 'save / quit...' subMenu: subMenu.	^ menu! !!MailDB methodsFor: 'fetch-merge-compact' stamp: 'ls 9/15/1998 22:30'!findDuplicates	"MailDB someInstance findDuplicates"	| msgsAtTime m duplicates list text1 msg1 msg2 id1 id2 |	msgsAtTime _ Dictionary new.	(self messagesIn: '.all.') do: [ :msgID |		m _ indexFile at: msgID.		(msgsAtTime includesKey: m time) ifFalse: [			msgsAtTime at: m time put: OrderedCollection new.		].		(msgsAtTime at: m time) add: (Array with: msgID with: m).	].	duplicates _ Set new.	msgsAtTime associationsDo: [ :assoc |		list _ assoc value asOrderedCollection.		list do: [ :arr1 |  			id1 _ arr1 at: 1.			msg1 _ arr1 at: 2.			text1 _ self getText: id1.			list do: [ :arr2 |				id2 _ arr2 at: 1.				msg2 _ arr2 at: 2.				(id1 < id2 and: 				 [ (duplicates includes: id2) not and:				 [ msg2 _ self getMessage: id2.  msg1 to = msg2 to and:				 [ msg1 subject = msg2 subject and: 				 [ msg1 from = msg2 from and:				 [ text1 = (self getText: id2) ] ] ] ] ]) ifTrue: [ duplicates add: id2 ]. ] ] ].	^duplicates asArray! !!TextURL methodsFor: 'as yet unclassified' stamp: 'ls 9/16/1998 00:00'!actOnClickFor: anObject	"Do what you can with this URL.  Later a web browser."	| response m browser | 	Smalltalk isMorphic ifTrue: [		"if it's a web browser, tell it to jump"		anObject isWebBrowser ifTrue: [			anObject jumpToUrl: url.			^true ].		"if it's a morph, see if it is contained in a web browser"		(anObject isKindOf: Morph) ifTrue: [			m _ anObject.			[ m ~= nil ] whileTrue: [				(m isWebBrowser) ifTrue: [ 					m  jumpToUrl: url.  					^true ].				(m hasProperty: #webBrowserView) ifTrue: [					m model jumpToUrl: url.					^true ].				m _ m owner. ]		].	].	"no browser in sight.  ask if we should start a new browser"	(self confirm: 'open a browser to view this URL?') ifTrue: [		browser _ Scamper new.		browser jumpToUrl: url.		browser openAsMorph.		^self ].		"couldn't display in a browser.  Offer to put up just the source"		response _ (PopUpMenu labels: 'View web page as source\Cancel' withCRs)		startUpWithCaption: 'Couldn''t find a web browser.  View page as source?'.	response = 1 ifTrue: [HTTPSocket httpShowPage: url].	^ true! !Celeste class removeSelector: #postMessage:!Celeste class removeSelector: #openMailSenderOn:!Celeste class removeSelector: #openMorphicMailSenderOn:!