'From Squeak3.1alpha of 28 February 2001 [latest update: #3801] on 6 March 2001 at 3:50:53 pm'!"Change Set:		classListMenusEtc-swDate:			6 March 2001Author:			Scott WallaceFinally fixes up class-list menus such that the correct shifted version gets put up from the more... item.Adds a stay-up item to the FileList's shifted file-list menu"!!StringHolder methodsFor: 'as yet unclassified' stamp: 'sw 3/6/2001 14:51'!offerMenuFrom: menuRetriever shifted: aBoolean	"Pop up, in morphic or mvc as the case may be, a menu whose target is the receiver and whose contents are provided by sending the menuRetriever to the receiver.  The menuRetriever takes two arguments: a menu, and a boolean representing the shift state."	| aMenu |	Smalltalk isMorphic		ifTrue:			[aMenu _ MenuMorph new defaultTarget: self.			self perform: menuRetriever with: aMenu with: aBoolean.			aMenu popUpInWorld]		ifFalse:			[aMenu _ CustomMenu new.			self perform: menuRetriever with: aMenu with: aBoolean.			aMenu invokeOn: self]! !!CodeHolder methodsFor: 'commands' stamp: 'sw 2/27/2001 12:14'!offerShiftedClassListMenu	"Offer the shifted class-list menu."	^ self offerMenuFrom: #classListMenu:shifted: shifted: true! !!CodeHolder methodsFor: 'commands' stamp: 'sw 2/27/2001 12:15'!offerUnshiftedClassListMenu	"Offer the shifted class-list menu."	^ self offerMenuFrom: #classListMenu:shifted: shifted: false! !!CodeHolder methodsFor: 'commands' stamp: 'sw 3/6/2001 15:18'!shiftedYellowButtonActivity	"Offer the shifted selector-list menu"	^ self offerMenuFrom: #messageListMenu:shifted: shifted: true! !!CodeHolder methodsFor: 'commands' stamp: 'sw 3/6/2001 15:19'!unshiftedYellowButtonActivity	"Offer the unshifted shifted selector-list menu"	^ self offerMenuFrom: #messageListMenu:shifted: shifted: false! !!Browser methodsFor: 'class functions' stamp: 'sw 2/27/2001 12:06'!classListMenu: aMenu shifted: shifted	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"	shifted		ifTrue:			[^ self shiftedClassListMenu: aMenu].	aMenu addList: #(		-		('browse full (b)'			browseMethodFull)		('browse hierarchy (h)'		spawnHierarchy)		('browse protocol (p)'		browseFullProtocol)		-		('printOut'					printOutClass)		('fileOut'					fileOutClass)		-		('show hierarchy'			hierarchy)		('show definition'			editClass)		('show comment'			editComment)		-		('inst var refs...'			browseInstVarRefs)		('inst var defs...'			browseInstVarDefs)		-		('class var refs...'			browseClassVarRefs)		('class vars'					browseClassVariables)		('class refs (N)'				browseClassRefs)		-		('rename class ...'			renameClass)		('copy class'				copyClass)		('remove class (x)'			removeClass)		-		('find method...'				findMethod)		-		('more...'					offerShiftedClassListMenu)).	^ aMenu! !!Browser methodsFor: 'class functions' stamp: 'sw 2/24/2001 00:05'!shiftedClassListMenu: aMenu	"Set up the menu to apply to the receiver's class list when the shift key is down"	^ aMenu addList: #(			-			('unsent methods'			browseUnusedMethods)			('unreferenced inst vars'	showUnreferencedInstVars)			('subclass template'			makeNewSubclass)			-			('sample instance'			makeSampleInstance)			('inspect instances'			inspectInstances)			('inspect subinstances'		inspectSubInstances)			-			('fetch documentation'		fetchClassDocPane)			('add all meths to current chgs'		addAllMethodsToCurrentChangeSet)			-			('more...'					offerUnshiftedClassListMenu))! !!FileList methodsFor: 'file list menu' stamp: 'sw 3/6/2001 15:39'!fileSelectedMenu: aMenu	"Fill the menu with items appropriate for the selected file type, or for all file types if the shift key is down"	| firstItems secondItems thirdItems n1 n2 n3 |	firstItems _ self itemsForFileEnding:		(Sensor leftShiftDown			ifFalse:				[self fileNameSuffix asLowercase]			ifTrue:				['*']).	secondItems _ self itemsForAnyFile.	thirdItems _ self itemsForNoFile.	n1 _ firstItems first size.	n2 _ n1 + secondItems first size.	n3 _ n2 + thirdItems first size.	^ aMenu		labels: firstItems first , secondItems first , thirdItems first , #('all operations...')		lines: firstItems second				, (Array with: n1 with: n2)				, (thirdItems second collect: [:n | n + n2])				, (Array with: n3)		selections: firstItems third , secondItems third , thirdItems third , #(offerAllFileOptions)! !!FileList methodsFor: 'file list menu' stamp: 'sw 3/6/2001 15:39'!fullFileListMenu: aMenu shifted: aBoolean	"Fill the menu with all possible items for the file list pane, regardless of selection."	aMenu title: 'all possible file operations'.	Smalltalk isMorphic ifTrue: [aMenu addStayUpItemSpecial].	aMenu addList: #(		('open graphic in a window' 		openImageInWindow)		('read graphic into ImageImports' 	importImage)		('open graphic as background'		openAsBackground)		-		('load as morph'						openMorphFromFile)		('load as project'					openProjectFromFile)		('load as book'						openBookFromFile)		-		('play midi file'						playMidiFile)		('open as movie'					openAsMovie)		('open as Flash'						openAsFlash)		('open true type font'				openAsTTF)		('open 3DS file'						open3DSFile)		('open for playback'				openTapeFromFile)		('open in Wonderland'				openVRMLFile)		('open in browser'					openInBrowser)		-		('fileIn'								fileInSelection)		('file into new change set'			fileIntoNewChangeSet)		('browse changes'					browseChanges)		('browse code'						browseFile)		-		('view decompressed'				viewGZipContents)		('decompress to file'					saveGZipContents)		-		('broadcast as update'				putUpdate)		('remove line feeds'					removeLinefeeds)		('generate HTML'					renderFile)		-		('load Genie Gesture Dictionary'		loadCRDictionary)		('load Genie Display Properties'		loadCRDisplayProperties))! !!FileList methodsFor: 'file list menu' stamp: 'sw 2/27/2001 13:52'!offerAllFileOptions	"Put up a menu offering all possible file options, whatever the suffix of the current selection may be.  Specially useful if you're wanting to keep the menu up"	self offerMenuFrom: #fullFileListMenu:shifted: shifted: true! !