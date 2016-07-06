'From Squeak3.1alpha of 28 February 2001 [latest update: #4219] on 30 July 2001 at 5:51:34 pm'!"Change Set:		messageNames-swDate:			30 July 2001Author:			Scott WallaceAdds a MessageNames tool, sort of a combination of the MethodFinder and a retargetable implementors browser.  There is a type-in pane at the the top; type into it and then hit RETURN, or click on the Search button; a list of known symbols containing the sequence will appear in the list pane right below the search pane.  Now choose any item in that selector-list pane, and a list of all implementors of it appear to the right (so far, this is the same as the original Method Finder.)  Click on any implementor in that right-hand and you will see, and be able to edit the given method right there in the bottom pane.  Additionally, annotation panes and button panes are available to those who want them.If the list of symbols matching the search string is sufficiently small, we actually filter the list down only to symbols that represent methods actually implemented in the system.  If the list is large, then instead we put up the entire list (as the MethodFinder has always done,) but we provide a menu item that allows the user proactively to request such filtering."!MessageSet subclass: #MessageNames	instanceVariableNames: 'searchString selectorList selectorListIndex '	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-Browser'!MessageNames class	instanceVariableNames: ''!!CodeHolder methodsFor: 'commands' stamp: 'sw 7/30/2001 16:31'!abbreviatedWordingFor: aButtonSelector	"Answer the abbreviated form of wording, from a static table which you're welcome to edit.  Answer nil if there is no entry -- in which case the long firm will be used on the corresponding browser button."	#(	(browseMethodFull				'browse')	(browseSendersOfMessages	   	'senders')	(browseMessages				'impl')	(browseVersions					'vers')	(methodHierarchy				'inher')	(classHierarchy					'hier')	(browseInstVarRefs				'iVar')	(browseClassVarRefs				'cVar')	(offerMenu						'menu')) do:		[:pair | pair first == aButtonSelector ifTrue: [^ pair second]].	^ nil! !!CodeHolder methodsFor: 'controls' stamp: 'sw 7/30/2001 16:23'!optionalButtonPairs	"Answer a tuple (formerly pairs) defining buttons, in the format:			button label			selector to send			help message"	| aList |	aList _ #(	('browse'			browseMethodFull			'view this method in a browser')	('senders' 			browseSendersOfMessages	'browse senders of...')	('implementors'		browseMessages				'browse implementors of...')	('versions'			browseVersions				'browse versions')), 	(Preferences decorateBrowserButtons		ifTrue:			[{#('inheritance'		methodHierarchy 'browse method inheritancegreen: sends to supertan: has override(s)mauve: both of the above' )}]		ifFalse:			[{#('inheritance'		methodHierarchy			'browse method inheritance')}]),	#(	('hierarchy'		classHierarchy				'browse class hierarchy')	('inst vars'			browseInstVarRefs			'inst var refs...')	"('class vars'			browseClassVarRefs			'class var refs...')").	^ aList! !!CodeHolder methodsFor: 'controls' stamp: 'sw 7/30/2001 16:28'!optionalButtonRow	"Answer a row of control buttons"	| aRow aButton aLabel |	aRow _ AlignmentMorph newRow.	aRow setNameTo: 'buttonPane'.	aRow beSticky.	aRow hResizing: #spaceFill.	aRow wrapCentering: #center; cellPositioning: #leftCenter.	aRow clipSubmorphs: true.	Preferences menuButtonInToolPane		ifTrue:			[aRow addMorphFront: self menuButton].	self optionalButtonPairs  do:		[:tuple |			aButton _ PluggableButtonMorph				on: self				getState: nil				action: tuple second.			aButton 				useRoundedCorners;				hResizing: #spaceFill;				vResizing: #spaceFill;				onColor: Color transparent offColor: Color transparent.			aLabel _ Preferences abbreviatedBrowserButtons 				ifTrue: [self abbreviatedWordingFor: tuple second]				ifFalse: [nil].			aButton label: (aLabel ifNil: [tuple first asString])" font: (StrikeFont familyName: 'Atlanta' size: 9)".			tuple size > 2 ifTrue: [aButton setBalloonText: tuple third].			aRow addMorphBack: aButton.			aRow addTransparentSpacerOfSize: (3 @ 0)].	aRow addMorphBack: self codePaneProvenanceButton.	^ aRow! !!Flaps class methodsFor: 'predefined flaps' stamp: 'sw 7/30/2001 17:08'!quadsDefiningToolsFlap	"Answer a structure defining the default Tools flap"	^ #(	(Browser 				prototypicalToolWindow		'Browser'			'A Browser is a tool that allows you to view all the code of all the classes in the system')	(Transcript				openAsMorph				'Transcript'			'A Transcript is a window usable for logging and debugging; browse references to #Transcript for examples of how to write to it.')	(Workspace				prototypicalToolWindow		'Workspace'			'A Work is a simple window for editing text.  You can later save the contents to a file if you desire.')	(FileList					prototypicalToolWindow		'File List'			'A File List is a tool for browsing folders and files on disks and on ftp types.')	(DualChangeSorter		prototypicalToolWindow		'Change Sorter'		'Shows two change sets side by side')	(SelectorBrowser			prototypicalToolWindow		'Method Finder'		'A tool for discovering methods by providing sample values for arguments and results')	(MessageNames			prototypicalToolWindow		'Message Names'		'A tool for finding, viewing, and editing all methods whose names contain a given character sequence.')	(Preferences			preferencesControlPanel	'Preferences'			'Allows you to control numerous options')	(Utilities				recentSubmissionsWindow	'Recent'				'A message browser that tracks the most recently-submitted methods')	(ProcessBrowser			prototypicalToolWindow		'Processes'			'A Process Browser shows you all the running processes')	(Preferences			annotationEditingWindow	'Annotations'		'Allows you to specify the annotations to be shown in the annotation panes of browsers, etc.')	(Scamper				newOpenableMorph			'Scamper'			'A web browser')	(Celeste					newOpenableMorph			'Celeste'				'Celeste -- an EMail reader')	(PackagePaneBrowser	prototypicalToolWindow		'Packages'			'Package Browser:  like a System Browser, except that if has extra level of categorization in the top-left pane, such that class-categories are further organized into groups called "packages"')	(ChangeSorter			prototypicalToolWindow		'Change Set'			'A tool that allows you to view and manipulate all the code changes in a single change set'))! !!MessageNames methodsFor: 'search' stamp: 'sw 7/30/2001 17:09'!computeSelectorListFromSearchString	"Compute selector list from search string"	| raw sorted |	searchString _ searchString asString copyWithout: $ .	selectorList _ Cursor wait showWhile:		[raw _ (Symbol selectorsContaining: searchString).		sorted _ raw as: SortedCollection.		sorted sortBlock: [:x :y | x asLowercase <= y asLowercase].		sorted asArray].	selectorList size > 19 ifFalse: "else the following filtering is considered too expensive.  This 19 should be a system-maintained Parameter, someday"		[selectorList _ Smalltalk selectorsWithAnyImplementorsIn: selectorList].	^ selectorList! !!MessageNames methodsFor: 'search' stamp: 'sw 7/30/2001 17:12'!defaultBackgroundColor	"Answer the default background color to use in a window for this tool"	^  (Color r: 0.645 g: 1.0 b: 0.452)	"Color fromUser"! !!MessageNames methodsFor: 'search' stamp: 'sw 7/28/2001 00:32'!doSearchFrom: aPane	"The user hit the Search button -- treat it as a synonym for the user having hit the Return or Enter (or cmd-s) in the type-in pane"	aPane accept.	aPane selectAll! !!MessageNames methodsFor: 'search' stamp: 'sw 7/28/2001 00:43'!searchString	"Answer the current searchString, initializing it if need be"	| pane |	searchString isEmptyOrNil ifTrue:		[searchString _ 'type here, then hit Search'.		pane _ self containingWindow findDeepSubmorphThat:			[:m | m knownName = 'Search'] ifAbsent: ["this happens during window creation" ^ searchString].			pane setText: searchString.			pane setTextMorphToSelectAllOnMouseEnter.			pane selectAll].	^ searchString! !!MessageNames methodsFor: 'search' stamp: 'sw 7/28/2001 02:18'!searchString: aString notifying: aController	"Take what the user typed and find all selectors containing it"	searchString _ aString asString copyWithout: $ .	self containingWindow setLabel: 'Message names containing "', searchString asLowercase, '"'.	selectorList _ nil.	self changed: #selectorList.	self changed: #messageList.	^ true! !!MessageNames methodsFor: 'search' stamp: 'sw 7/24/2001 01:49'!showOnlyImplementedSelectors	"Caution -- can be slow!!  Filter my selector list down such that it only shows selectors that are actually implemented somewhere in the system."	self okToChange ifTrue:		[Cursor wait showWhile:			[selectorList _ Smalltalk selectorsWithAnyImplementorsIn: selectorList.			self changed: #selectorList.			self changed: #messageList]]! !!MessageNames methodsFor: 'selection' stamp: 'sw 7/24/2001 01:46'!selection	"Answer the item in the list that is currently selected, or nil if no selection is present"	^ self messageList at: messageListIndex ifAbsent: [nil]! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:58'!messageList	"Answer the receiver's message list, computing it if necessary.  The way to force a recomputation is to set the messageList to nil"	messageList ifNil:		[messageList _ selectorListIndex == 0			ifTrue:				[#()]			ifFalse:				[Smalltalk allImplementorsOf: (selectorList at: selectorListIndex)].		self messageListIndex: (messageList size > 0			ifTrue: [1]			ifFalse: [0])].	^ messageList! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:46'!selectorList	"Answer the selectorList"	selectorList ifNil:		[self computeSelectorListFromSearchString.		selectorListIndex _  selectorList size > 0			ifTrue:	[1]			ifFalse: [0].		messageList _ nil].	^ selectorList! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:55'!selectorListIndex	"Answer the selectorListIndex"	^ selectorListIndex! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:59'!selectorListIndex: anInteger 	"Set the selectorListIndex as specified, and propagate consequences"	selectorListIndex _ anInteger.	selectorListIndex = 0		ifTrue: [^ self].	messageList _ nil.	self changed: #selectorListIndex.	self changed: #messageList! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:58'!selectorListMenu: aMenu	"Answer the menu associated with the selectorList"	aMenu addList: #(		('senders (n)'				browseSenders		'browse senders of the chosen selector')		('copy selector to clipboard'	copyName			'copy the chosen selector to the clipboard, for subsequent pasting elsewhere')		-		('show only implemented selectors'	showOnlyImplementedSelectors		'remove from the selector-list all symbols that do not represent implemented methods')).	^ aMenu! !!MessageNames methodsFor: 'selector list' stamp: 'sw 7/24/2001 01:47'!selectorListMenuTitle	"Answer the title to supply for the menu belonging to the selector-list pane"	^ 'Click on any item in the listto see all implementors of it'! !!MessageNames methodsFor: 'initialization' stamp: 'sw 7/28/2001 02:16'!inMorphicWindowLabeled: labelString	"Answer a morphic window with the given label that can display the receiver""MessageNames openMessageNames"	^ self inMorphicWindowWithInitialSearchString: nil! !!MessageNames methodsFor: 'initialization' stamp: 'sw 7/28/2001 02:21'!inMorphicWindowWithInitialSearchString: initialString	"Answer a morphic window with the given initial search string, nil if none""MessageNames openMessageNames"	| window selectorListView firstDivider secondDivider horizDivider typeInPane searchButton plugTextMor |	window _ (SystemWindow labelled: 'Message Names') model: self.	firstDivider _ 0.07.	secondDivider _ 0.5.	horizDivider _ 0.5.	typeInPane _ AlignmentMorph newRow vResizing: #spaceFill; height: 14.	typeInPane hResizing: #spaceFill.	typeInPane listDirection: #leftToRight.	plugTextMor _ PluggableTextMorph on: self					text: #searchString accept: #searchString:notifying:					readSelection: nil menu: nil.	plugTextMor setProperty: #alwaysAccept toValue: true.	plugTextMor askBeforeDiscardingEdits: false.	plugTextMor acceptOnCR: true.	plugTextMor setTextColor: Color brown.	plugTextMor setNameTo: 'Search'.	plugTextMor vResizing: #spaceFill; hResizing: #spaceFill.	plugTextMor hideScrollBarIndefinitely.	plugTextMor setTextMorphToSelectAllOnMouseEnter.	searchButton _ SimpleButtonMorph new 		target: self;		beTransparent;		label: 'Search';		actionSelector: #doSearchFrom:;		arguments: {plugTextMor}.	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list pane below.  Click on any one of them, and all the implementors of that selector will be shown in the right-hand pane, and you can view and edit their code without leaving this tool.'.	typeInPane addMorphFront: searchButton.	typeInPane addTransparentSpacerOfSize: 6@0.	typeInPane addMorphBack: plugTextMor.	initialString isEmptyOrNil ifFalse:		[plugTextMor setText: initialString].	window addMorph: typeInPane frame: (0@0 corner: horizDivider @ firstDivider).	selectorListView _ PluggableListMorph on: self		list: #selectorList		selected: #selectorListIndex		changeSelected: #selectorListIndex:		menu: #selectorListMenu:		keystroke: #selectorListKey:from:.	selectorListView menuTitleSelector: #selectorListMenuTitle.	window addMorph: selectorListView frame: (0 @ firstDivider corner: horizDivider @ secondDivider).	window addMorph: self buildMorphicMessageList frame: (horizDivider @ 0 corner: 1@ secondDivider).	self 		addLowerPanesTo: window 		at: (0 @ secondDivider corner: 1@1) 		with: nil.	initialString isEmptyOrNil ifFalse:		[self searchString: initialString notifying: nil].	^ window! !!MessageNames methodsFor: 'initialization' stamp: 'sw 7/24/2001 01:35'!selectorListKey: aChar from: view	"Respond to a Command key in the message-list pane."	aChar == $n ifTrue: [^ self browseSenders].	aChar == $c ifTrue: [^ self copyName].	aChar == $b ifTrue: [^ self browseMethodFull].! !!MessageNames class methodsFor: 'instance creation' stamp: 'sw 7/28/2001 00:54'!methodBrowserSearchingFor: searchString	"Answer an method-browser window whose search-string is initially as indicated"	| aWindow |	aWindow _ self new inMorphicWindowWithInitialSearchString: searchString.	aWindow applyModelExtent.	^ aWindow! !!MessageNames class methodsFor: 'instance creation' stamp: 'sw 7/24/2001 18:03'!openMessageNames	"Open a new instance of the receiver in the active world"	self new openAsMorphNamed: 'Message Names' inWorld: ActiveWorld	"MessageNames openMessageNames"! !!MessageNames class methodsFor: 'instance creation' stamp: 'sw 7/28/2001 00:56'!prototypicalToolWindow	"Answer an example of myself seen in a tool window, for the benefit of parts-launching tools"	^ self methodBrowserSearchingFor: nil! !!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 7/27/2001 17:34'!findAMessageNamesWindow: evt	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"	| aWindow |	submorphs do:		[:aMorph | (((aWindow _ aMorph renderedMorph) isKindOf: SystemWindow) and:			[aWindow model isKindOf: MessageNames])				ifTrue:					[aWindow isCollapsed ifTrue: [aWindow expand].					aWindow activateAndForceLabelToShow.					^ self]].	"None found, so create one"	MessageNames openMessageNames! !!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 7/30/2001 17:40'!keystrokeInWorld: evt	"A keystroke was hit when no keyboard focus was in set, so it is sent here to the world instead.  This current implementation is regrettably hard-coded; until someone cleans this up, you may wish to edit this method to suit your personal taste in interpreting cmd-keys issued to the desktop."	|  aChar isCmd |	aChar _ evt keyCharacter.	isCmd _ evt commandKeyPressed and: [Preferences cmdKeysInText].	(isCmd and: [Preferences honorDesktopCmdKeys]) ifTrue:		[Preferences eToyFriendly			ifTrue:				[(aChar == $W) ifTrue: [^ self putUpWorldMenu: evt]]			ifFalse:				[(aChar == $z) ifTrue: [^ self commandHistory undoOrRedoCommand].				(aChar == $w) ifTrue: [^ SystemWindow closeTopWindow].				(aChar == $\) ifTrue: [^ SystemWindow sendTopWindowToBack].				(aChar == $t) ifTrue: [^ self findATranscript: evt].				(aChar == $b) ifTrue: [^ Browser openBrowser].				(aChar == $k) ifTrue: [^ Workspace open].				(aChar == $m) ifTrue: [^ TheWorldMenu new adaptToWorld: World; newMorph].				(aChar == $C) ifTrue: [^ self findAChangeSorter: evt].				(aChar == $R) ifTrue: [^ self openRecentSubmissionsBrowser: evt].				(aChar == $P) ifTrue: [^ self findAPreferencesPanel: evt].				(aChar == $W) ifTrue: [^ self findAMessageNamesWindow: evt].				(aChar == $r) ifTrue: [^ World restoreMorphicDisplay]]]! !!PluggableButtonMorph methodsFor: 'accessing' stamp: 'sw 7/30/2001 15:55'!label: aStringOrTextOrMorph font: aFont	"Label this button with the given string or morph."	| r |	self removeAllMorphs.	"nest label in a row for centering"	r _ AlignmentMorph newRow		borderWidth: 0;		layoutInset: 0;		color: Color transparent;		hResizing: #shrinkWrap;		vResizing: #spaceFill;		wrapCentering: #center; cellPositioning: #leftCenter.	aStringOrTextOrMorph isMorph		ifTrue: [			label _ aStringOrTextOrMorph.			r addMorph: aStringOrTextOrMorph]		ifFalse: [			label _ aStringOrTextOrMorph asString.			r addMorph: (StringMorph contents: label font: aFont)].	self addMorph: r.! !!SystemDictionary methodsFor: 'browsing' stamp: 'sw 7/28/2001 00:46'!browseMethodsWhoseNamesContain: aString	"Launch a tool which shows all methods whose names contain the given string; case-insensitive.�	1/16/1996 sw, at the dawn of Squeak: this was the classic implementation that provided the underpinning for the 'method names containing it' (cmd-shift-W) feature that has always been in Squeak -- the feature that later inspired the MethodFinder (aka SelectorBrowser).�	sw 7/27/2001:	Switched to showing a MessageNames tool rather than a message-list browser, if in Morphic."	| aList |	Smalltalk isMorphic		ifFalse:			[aList _ Symbol selectorsContaining: aString.			aList size > 0 ifTrue:				[self browseAllImplementorsOfList: aList asSortedCollection title: 'Methods whose names contain ''', aString, '''']]		ifTrue:			[(MessageNames methodBrowserSearchingFor: aString) openInWorld]	! !!SystemDictionary methodsFor: 'retrieving' stamp: 'sw 7/27/2001 18:39'!isThereAnImplementorOf: aSelector  	"Answer true if there is at least one implementor of the selector found in the system, false if there are no implementors"	self allBehaviorsDo:		[:class |			(class includesSelector: aSelector)				ifTrue: [^ true]].	^ false"Smalltalk isThereAnImplementorOf: #contents.Smalltalk isThereAnImplementorOf: #nobodyImplementsThis."! !!SystemDictionary methodsFor: 'retrieving' stamp: 'sw 7/27/2001 18:39'!selectorsWithAnyImplementorsIn: selectorList  	"Answer the subset of the given list which represent method selectors which have at least one implementor in the system."	| good |	good _ OrderedCollection new.	self allBehaviorsDo:		[:class |			selectorList do:				[:aSelector |					(class includesSelector: aSelector) ifTrue:						[good add: aSelector]]].	^ good asSet asSortedArray"Smalltalk selectorsWithAnyImplementorsIn: #( contents contents: nuts)"! !!TheWorldMenu methodsFor: 'construction' stamp: 'sw 7/30/2001 16:49'!openMenu        "Build the open window menu for the world."        | menu |        menu _ self menu: 'open...'.        self fillIn: menu from: {                {'browser (b)' . { Browser . #openBrowser} }.                {'package browser' . { PackagePaneBrowser . #openBrowser} }.                {'workspace (k)' . {self . #openWorkspace} }.                {'file list' . {self . #openFileList} }.                {'file...' . { FileList . #openFileDirectly} }.                {'transcript (t)' . {self . #openTranscript} }.                "{'inner world' . { WorldWindow . #test1} }."                nil.                {'method finder' . { self . #openSelectorBrowser} }.                {'message names (W)' . { self . #openMessageNames} }.			 nil.                {'simple change sorter' . {self . #openChangeSorter1} }.                {'dual change sorter' . {self . #openChangeSorter2} }.                nil.                {'email reader' . {self . #openEmail} }.                {'web browser' . { Scamper . #openAsMorph} }.                {'IRC chat' . {self . #openIRC} }.                nil.        }.        self mvcProjectsAllowed ifTrue:                [self fillIn: menu from: { {'mvc project' . {self. #openMVCProject} } }].        ^ self fillIn: menu from: {                 {'morphic project' . {self. #openMorphicProject} }.        }.! !!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'sw 7/30/2001 17:16'!windowsMenu        "Build the windows menu for the world."        ^ self fillIn: (self menu: 'windows') from: {                  { 'find window' . { #myWorld . #findWindow: }. 'Presents a list of all windows; if you choose one from the list, it becomes the active window.'}.                { 'find changed browsers...' . { #myWorld . #findDirtyBrowsers: }. 'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.                { 'find changed windows...' . { #myWorld . #findDirtyWindows: }. 'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.			nil.                { 'find a transcript (t)' . { #myWorld . #findATranscript: }. 'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'}.               { 'find a change sorter (C)' . { #myWorld . #findAChangeSorter: }. 'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'}.			{ 'find message names (W)' . { #myWorld . #findAMessageNamesWindow: }. 'Brings an open MessageNames window to the front, creating one if necessary, and makes it the active window'}.			"{ 'find a treasury (T))' . {#myWorld . #activateMorphTreasury} . 'Brings a Treasury to the front, creating one if necsary, and makes it the active window.  A Treasury is the key to getting any kind of new object.'}."			 nil.                { #staggerPolicyString . { self . #toggleWindowPolicy }. 'stagger: new windows positioned so you can see a portion of each one.                tile: new windows positioned so that they do not overlap others, if possible.'}.                nil.                { 'collapse all windows' . { #myWorld . #collapseAll }. 'Reduce all open windows to collapsed forms that only show titles.'}.                { 'expand all windows' . { #myWorld . #expandAll }. 'Expand all collapsed windows back to their expanded forms.'}.                { 'close top window (w)' . { SystemWindow . #closeTopWindow }. 'Close the topmost window if possible.'}.                { 'send top window to back (\)' . { SystemWindow . #sendTopWindowToBack  }. 'Make the topmost window become the backmost one, and activate the window just beneath it.'}.			 { 'move windows onscreen' . { #myWorld . #bringWindowsFullOnscreen }. 'Make all windows fully visible on the screen'}.                nil.                { 'delete unchanged windows' . { #myWorld . #closeUnchangedWindows }. 'Deletes all windows that do not have unsaved text edits.'}.                { 'delete non-windows' . { #myWorld . #deleteNonWindows }. 'Deletes all non-window morphs lying on the world.'}.                { 'delete both of the above' . { self . #cleanUpWorld }. 'deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'}.        }! !!TheWorldMenu methodsFor: 'action' stamp: 'sw 7/28/2001 02:11'!openMessageNames	"Bring a MessageNames tool to the front"	MessageNames openMessageNames! !"Postscript:"Preferences okToReinitializeFlaps ifTrue: [Flaps replaceToolsFlap].!