'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 5 June 2000 at 6:07:36 pm'!"Change Set:		124MorphMenuRef-smaDate:			5 June 2000Author:			Stefan Matthias AustTrying to understand how a morphic menu works, I wondered which would be the right way to call it up.  There where multiple ways and all too difficult.  I reduced them to two - and this could even further reduced to just one call."!!Morph methodsFor: 'menus' stamp: 'sma 6/5/2000 13:59'!adhereToEdge	| menu |	menu _ MenuMorph new defaultTarget: self.	#(top right bottom left - center - topLeft topRight bottomRight bottomLeft - none)		do: [:each |			each == #-				ifTrue: [menu addLine]				ifFalse: [menu add: each asString selector: #setToAdhereToEdge: argument: each]].	menu popUpEvent: self currentEvent! !!BookMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:46'!invokeBookMenu	"Invoke the book's control panel menu."	| aMenu controlsShowing |	aMenu _ MenuMorph new defaultTarget: self.	aMenu add: 'find...' action: #textSearch.	aMenu add: 'go to page...' action: #goToPage.	aMenu addLine.	aMenu addList:		#(('sort pages'			sortPages)		('uncache page sorter'	uncachePageSorter)).	(self hasProperty: #dontWrapAtEnd)		ifTrue: [aMenu add: 'wrap after last page' selector: #setWrapPages: argument: true]		ifFalse: [aMenu add: 'stop at last page' selector: #setWrapPages: argument: false].	aMenu addList:		#(('make bookmark'		bookmarkForThisPage)		('make thumbnail'		thumbnailForThisPage)).	controlsShowing _ self hasSubmorphWithProperty: #pageControl.	controlsShowing		ifTrue:			[aMenu add: 'hide page controls' action: #hidePageControls.			aMenu add: 'fewer page controls' action: #fewerPageControls]		ifFalse:			[aMenu add: 'show page controls' action: #showPageControls].	aMenu addLine.	aMenu add: 'sound effect for all pages' action: #menuPageSoundForAll:.	aMenu add: 'sound effect this page only' action: #menuPageSoundForThisPage:.	aMenu add: 'visual effect for all pages' action: #menuPageVisualForAll:.	aMenu add: 'visual effect this page only' action: #menuPageVisualForThisPage:.	aMenu addLine.	(self primaryHand classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[aMenu add: 'paste book page'   action: #pasteBookPage].	aMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil: [		aMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	aMenu add: (self dragNDropEnabled ifTrue: ['close'] ifFalse: ['open']) , ' dragNdrop'			action: #toggleDragNDrop.	aMenu add: 'make all pages this size' action: #makeUniformPageSize.	aMenu add: 'send all pages to server' action: #savePagesOnURL.	aMenu add: 'send this page to server' action: #saveOneOnURL.	aMenu add: 'reload all from server' action: #reload.	aMenu add: 'copy page url to clipboard' action: #copyUrl.	aMenu add: 'keep in one file' action: #keepTogether.	aMenu addLine.	aMenu add: 'load PPT images from slide #1' action: #loadImagesIntoBook.	aMenu add: 'background color for all pages...' action: #setPageColor.	aMenu popUpEvent: self world activeHand lastEvent! !!BookMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:44'!menuPageSoundFor: target event: evt	| tSpec menu |	tSpec _ self transitionSpecFor: target.	menu _ (MenuMorph entitled: 'Choose a sound(it is now ' , tSpec first , ')') defaultTarget: target.	SampledSound soundNames do:		[:soundName |		menu add: soundName target: target			selector: #setProperty:toValue:			argumentList: (Array with: #transitionSpec								with: (tSpec copy at: 1 put: soundName; yourself))].	menu popUpEvent: evt! !!BookMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:44'!menuPageVisualFor: target event: evt	| tSpec menu subMenu directionChoices |	tSpec _ self transitionSpecFor: target.	menu _ (MenuMorph entitled: 'Choose an effect(it is now ' , tSpec second , ')') defaultTarget: target.	TransitionMorph allEffects do:		[:effect |		directionChoices _ TransitionMorph directionsForEffect: effect.		directionChoices isEmpty		ifTrue: [menu add: effect target: target					selector: #setProperty:toValue:					argumentList: (Array with: #transitionSpec									with: (Array with: tSpec first with: effect with: #none))]		ifFalse: [subMenu _ MenuMorph new.				directionChoices do:					[:dir |					subMenu add: dir target: target						selector: #setProperty:toValue:						argumentList: (Array with: #transitionSpec									with: (Array with: tSpec first with: effect with: dir))].				menu add: effect subMenu: subMenu]].	menu popUpEvent: evt! !!EnvelopeEditorMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:23'!chooseDenominator: evt	| menu |	menu _ MenuMorph new.	(Integer primesUpTo: 30) do:		[:i |		menu add: i printString			target: self selector: #setDenominator:			argument: i].	menu addLine.	menu add: 'none' target: self selector: #setDenominator: argument: 9999.	menu popUpEvent: evt! !!EnvelopeEditorMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:23'!chooseSound: evt	| menu |	menu _ MenuMorph new.	menu add: 'new' target: self selector: #editNewSound.	menu addLine.	AbstractSound soundNames do:		[:name |		menu add: name			target: self selector: #editSoundNamed:			argument: name].	menu popUpEvent: evt! !!EnvelopeEditorMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:24'!readFromDisk: evt	| menu |	menu _ MenuMorph new.	(FileDirectory default fileNamesMatching: '*.fmp') do:		[:fileName |		menu add: fileName			target: self selector: #readFileNamed:			argument: fileName].	menu popUpEvent: evt! !!FatBitsPaint methodsFor: 'events' stamp: 'sma 6/5/2000 13:26'!toolMenu: evt	| menu |	menu _ MenuMorph new.	menu		addTitle: 'Tools';		addStayUpItem.	{		{'paint brush'. self toolsForPaintBrush}.		{'selections'. self toolsForSelection}	} do: [:each |		menu add: each first			target: self			selector: #setCurrentToolTo:			argumentList: {each second}].	menu toggleStayUp: nil.	menu popUpEvent: evt! !!FatBitsPaint methodsFor: 'menu' stamp: 'sma 6/5/2000 13:24'!selectionMenu: evt        | menu |         (menu _ MenuMorph new)                addTitle: 'Edit';                addStayUpItem.        {                {'edit separately'. #editSelection}.                {'copy'. #copySelection}.                {'cut'. #cutSelection}.                {'paste'. #pasteSelection}        } do: [:each |                menu add: each first                        target: self                        selector: each second                        argumentList: #()].        menu toggleStayUp: nil.        menu popUpEvent: evt! !!FatBitsPaint methodsFor: 'menu' stamp: 'sma 6/5/2000 13:25'!setMagnification: evt	| menu |	menu _ MenuMorph new.	((1 to: 8), #(16 24 32)) do: [:w |		menu add: w printString			target: self			selector: #magnification:			argumentList: (Array with: w).		magnification = w ifTrue: [menu lastSubmorph color: Color red]].	menu popUpEvent: evt! !!FatBitsPaint methodsFor: 'menu' stamp: 'sma 6/5/2000 13:25'!setPenSize: evt	| menu sizes | 	menu _ MenuMorph new.	sizes _ (1 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).	sizes do: [:w |		menu add: w printString			target: self			selector: #penSize:			argumentList: (Array with: w).		(brushSize // magnification) = w ifTrue: [menu lastSubmorph color: Color red]].	menu popUpEvent: evt! !!FlapTab methodsFor: 'edge' stamp: 'sma 6/5/2000 13:27'!setEdgeToAdhereTo	| aMenu |	aMenu _ MenuMorph new defaultTarget: self.	#(left top right bottom) do:		[:sym | aMenu add: sym asString target: self selector:  #setEdge: argument: sym].	aMenu popUpEvent: self currentEvent! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:51'!appearanceDo	"Build and show the appearance menu for the world."	self appearanceMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:52'!changesDo	"Build and show the changes menu for the world."	self changesMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:52'!debugDo	"Build and show the debug menu for the world."	self debugMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:52'!helpDo	"Build and show the help menu for the world."	self helpMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:52'!newMorph	| menu subMenu catDict shortCat class |	menu _ (MenuMorph entitled: 'Add a new morph') defaultTarget: self.	menu addStayUpItem.	menu add: 'from paste buffer' action: #pasteMorph.	menu add: 'from a file...' target: self action: #readMorphFromAFile.	"menu add: 'from alphabetical list...' action: #newMorphFromAlphabeticalList."	menu add: 'from alphabetical list' subMenu: self alphabeticalMorphMenu.	menu add: 'grab patch from screen' action: #grabDrawingFromScreen.	menu add: 'make new drawing' target: self action: #newDrawingFromMenu.	menu add: 'make link to project...' target: self action: #projectThumbnail.	menu addLine.	catDict _ Dictionary new.	SystemOrganization categories do:		[:cat |		((cat beginsWith: 'Morphic-')				and: [(#('Morphic-Menus' 'Morphic-Support') includes: cat) not])		ifTrue:			[shortCat _ cat copyFrom: 'Morphic-' size+1 to: cat size.			(SystemOrganization listAtCategoryNamed: cat) do:				[:cName | class _ Smalltalk at: cName.				((class inheritsFrom: Morph)					and: [class includeInNewMorphMenu])					ifTrue:					[(catDict includesKey: shortCat) 					ifTrue: [(catDict at: shortCat) addLast: class]					ifFalse: [catDict at: shortCat put: (OrderedCollection with: class)]]]]].	catDict keys asSortedCollection do:		[:categ |		subMenu _ MenuMorph new.		((catDict at: categ) asSortedCollection: [:c1 :c2 | c1 name < c2 name]) do:			[:cl | subMenu add: cl name					target: self					selector: #newMorphOfClass:event:					argument: cl].		menu add: categ subMenu: subMenu].	menu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:53'!openWindow	"Build and show the open menu for the world."	self openMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:53'!playfieldDo	"Build and show the playfield menu for the world."	self world playfieldOptionsMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:56'!projectDo	"Build and show the project menu for the world."	self projectMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:56'!remoteDo	self remoteMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:56'!scriptingDo	self scriptingMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:57'!standardFontDo	"Build and show the standard font menu"	Preferences fontConfigurationMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu' stamp: 'sma 6/5/2000 13:57'!windowsDo	"Build the windows menu for the world."	self windowsMenu popUpForHand: self! !!HandMorph methodsFor: 'world menu commands' stamp: 'sma 6/5/2000 13:28'!invokeMenu: menu event: evt	"Invoke the given menu for the given event."	menu popUpEvent: evt! !!HandMorph methodsFor: 'world menu commands' stamp: 'sma 6/5/2000 13:52'!jumpToProject	(Project buildJumpToMenu: (MenuMorph new defaultTarget: Project; addTitle: 'Projects'))		popUpForHand: self! !!HandMorph methodsFor: 'world menu commands' stamp: 'sma 6/5/2000 13:42'!selectSubmorphToOperateOn: rootMorph sending: aSymbol event: evt	"Let the user select a submorph of the given root morph. When selected, the given selector is sent with the selected submorph as an argument."	| possibleTargets menu |	possibleTargets _ rootMorph morphsAt: targetOffset.	possibleTargets size = 1 ifTrue: [^ self perform: aSymbol with: possibleTargets first with: evt].	menu _ MenuMorph new.	possibleTargets do: [:m |		menu add: (self submorphNameFor: m)			target: self			selector: aSymbol			argumentList: (Array with: m with: evt)].	menu popUpEvent: evt! !!MIDIControllerMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:29'!setChannel: evt	| menu |	menu _ MenuMorph new.	1 to: 16 do: [:chan |		menu add: chan printString			target: self			selector: #channel:			argumentList: (Array with: chan - 1)].	menu popUpEvent: evt! !!MIDIControllerMorph methodsFor: 'menu' stamp: 'sma 6/5/2000 13:29'!setController: evt	| menu |	menu _ MenuMorph new.	self controllerList do: [:pair |		menu add: (pair last)			target: self			selector: #controller:			argumentList: (Array with: pair first)].	menu popUpEvent: evt! !!MenuItemMorph methodsFor: 'drawing' stamp: 'sma 6/5/2000 14:50'!drawOn: aCanvas	| selectionColor |	isSelected & isEnabled		ifTrue:			[selectionColor _ Display depth <= 2				ifTrue: [Color gray]				ifFalse: [owner color darker].			aCanvas fillRectangle: self bounds color: selectionColor].	super drawOn: aCanvas.	subMenu ifNotNil:		[aCanvas			paintImage: SubMenuMarker			at: self right - 8 @ (self top + self bottom - SubMenuMarker height // 2)]! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 14:56'!popUpAdjacentTo: rightOrLeftPoint forHand: hand from: sourceItem	"Present this menu at the given point under control of the given hand."	| selectedItem delta tryToPlace selectedOffset |	hand world startSteppingSubmorphsOf: self.	popUpOwner _ sourceItem.	originalEvent _ hand lastEvent.	selectedItem _ self selectedItem.	self fullBounds.  "ensure layout is current"	selectedOffset _ selectedItem position - self position.	tryToPlace _		[:where :mustFit |		self position: where - selectedOffset.		delta _ self fullBoundsInWorld amountToTranslateWithin: hand worldBounds.		(delta x = 0 or: [mustFit]) ifTrue:			[delta = (0@0) ifFalse: [self position: self position + delta].			sourceItem owner owner addMorphFront: self.			^ self]].	tryToPlace 		value: rightOrLeftPoint first value: false;		value: rightOrLeftPoint last  - (self width @ 0) value: false;		value: rightOrLeftPoint first value: true	! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 13:55'!popUpAt: aPoint event: evt	"Present this menu at the given point in response to the given event."	self popUpAt: aPoint forHand: evt hand! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 14:42'!popUpAt: aPoint forHand: hand	"Present this menu at the given point under control of the given hand."	| selectedItem |	self items isEmpty ifTrue: [^ self].	popUpOwner _ hand.	originalEvent _ hand lastEvent.	selectedItem _ self selectedItem.	self positionAt: aPoint relativeTo: selectedItem.	self openInWorld: hand world.	hand newMouseFocus: selectedItem.	self changed! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 14:43'!popUpAt: aPoint forHand: hand from: sourceItem	"Present this menu at the given point under control of the given hand."	| selectedItem delta |	popUpOwner _ sourceItem.	originalEvent _ hand lastEvent.	selectedItem _ self selectedItem.	self fullBounds.  "ensure layout is current"	self position: aPoint - (selectedItem position - self position).	sourceItem owner owner addMorphFront: self.	delta _ self fullBoundsInWorld amountToTranslateWithin: hand worldBounds.	delta = (0@0) ifFalse: [self position: self position + delta]! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 13:54'!popUpEvent: evt	"Present this menu in response to the given event."	self popUpForHand: evt hand! !!MenuMorph methodsFor: 'control' stamp: 'sma 6/5/2000 13:50'!popUpForHand: hand 	"Present this menu under control of the given hand."	self popUpAt: hand position forHand: hand! !!MenuMorph methodsFor: 'private' stamp: 'sma 6/5/2000 14:40'!positionAt: aPoint	"Note: items may not be laid out yet (I found them all to be at 0@0),  	so we have to add up heights of items above the selected item."	| i yOffset selectedItem sub delta |	i _ 0.	yOffset _ 0.	selectedItem _ self selectedItem.	[(sub _ self submorphs at: (i _ i + 1)) == selectedItem]		whileFalse: [yOffset _ yOffset + sub height].	self position: aPoint - (2 @ (yOffset + 8)).	"If it doesn't fit, show it to the left, not to the right of the hand."	self right > popUpOwner worldBounds right		ifTrue: [self left: self left - self width + 4].	"Make sure that the menu fits in the world."	delta _ self bounds amountToTranslateWithin: popUpOwner worldBounds.	delta = (0 @ 0) ifFalse: [self position: self position + delta]! !!MenuMorph methodsFor: 'private' stamp: 'sma 6/5/2000 14:42'!positionAt: aPoint relativeTo: selectedItem	"Note: items may not be laid out yet (I found them all to be at 0@0),  	so we have to add up heights of items above the selected item."	| i yOffset sub delta |	i _ 0.	yOffset _ 0.	[(sub _ self submorphs at: (i _ i + 1)) == selectedItem]		whileFalse: [yOffset _ yOffset + sub height].	self position: aPoint - (2 @ (yOffset + 8)).	"If it doesn't fit, show it to the left, not to the right of the hand."	self right > popUpOwner worldBounds right		ifTrue: [self left: self left - self width + 4].	"Make sure that the menu fits in the world."	delta _ self bounds amountToTranslateWithin: popUpOwner worldBounds.	delta = (0 @ 0) ifFalse: [self position: self position + delta]! !!MenuMorph methodsFor: 'private' stamp: 'sma 6/5/2000 14:38'!selectedItem	| items |	items _ self items.	^ items detect: [:each | each == lastSelection] ifNone: [items first]! !!MultiuserTinyPaint methodsFor: 'menu' stamp: 'sma 6/5/2000 13:30'!setPenSize: evt	| menu sizes |	menu _ MenuMorph new.	sizes _ (0 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).	sizes do: [:w |		menu add: w printString			target: self			selector: #penSize:hand:			argumentList: (Array with: w with: evt hand)].	menu popUpEvent: evt! !!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sma 6/5/2000 13:59'!presentPlayfieldMenu	self playfieldOptionsMenu popUpForHand: self activeHand! !!Player methodsFor: 'scripts-kernel' stamp: 'sma 6/5/2000 14:00'!slotInfoButtonHitFor: aSlotName inViewer: aViewer	"The user made a gesture asking for info/menu relating"	| aMenu slotSym aType |	slotSym _ aSlotName asSymbol.	aType _ self typeForSlot: aSlotName asSymbol.	aMenu _ MenuMorph new defaultTarget: self.	aMenu add: 'simple watcher' selector: #tearOffWatcherFor: argument: slotSym.	aType == #number "later others" ifTrue:		[aMenu add: 'fancier watcher' selector: #tearOffFancyWatcherFor: argument: slotSym].	(self slotInfo includesKey: slotSym)		ifTrue:  "User slot"			[aMenu add: 'change data type' selector: #chooseSlotTypeFor: argument: slotSym.			aType == #number ifTrue:				[aMenu add: 'decimal places...' selector: #setPrecisionFor: argument: slotSym].			aMenu add: 'remove "', aSlotName, '"' selector: #removeSlotNamed: argument: slotSym.			aMenu add: 'rename  "', aSlotName, '"' selector: #renameSlot: argument: slotSym].	aMenu items size == 0 ifTrue:		[aMenu add: 'ok' action: nil].	aMenu addTitle: (aSlotName asString, ' (', aType, ')').	aMenu popUpForHand: aViewer primaryHand! !!Player methodsFor: 'misc' stamp: 'sma 6/5/2000 13:31'!thumbnailMenuEvt: anEvent forMorph: aMorph	"The mouse went down in the thumbnail of a Viewer for the receiver"	| aMenu aWorld aViewer |	aWorld _ aMorph world.	aViewer _ aMorph ownerThatIsA: Viewer.	aMenu _ MenuMorph new defaultTarget: self.	aMenu add: 'set new costume...' action: #newCostume.	costumes ifNotNil:		[(costumes size > 1 or: [costumes size == 1 and: [costumes first ~~ costume]])			ifTrue:				[aMenu add: 'forget other costumes' target: self selector: #forgetOtherCostumes]].	aMenu addLine.	aMenu add: 'add a new instance variable' target: self action: #addInstanceVariable.	"aMenu add: 'add an empty new script' target: aViewer action: #newEmptyScript."	aMenu add: 'add a new script' target: aViewer action: #newPermanentScript.	aMenu add: 'expunge empty scripts' target: self action: #expungeEmptyScripts.	aMenu addLine.	aMenu add: 'tile representing me' action: #tearOffTileForSelf.	aMenu add: 'reveal me' target: self selector: #revealPlayerIn: argument: aWorld.	aMenu add: 'grab me' target: self selector: #grabPlayerIn: argument: aWorld.	aMenu popUpEvent: aWorld primaryHand lastEvent.	aMenu addLine.	aMenu add: 'inspect morph' target: costume selector: #inspect.	aMenu add: 'inspect player' target: self selector: #inspect.	self belongsToUniClass ifTrue:		[aMenu add: 'browse class' target: self action: #browsePlayerClass.		aMenu add: 'inspect class' target: self class action: #inspect]."	aMenu add: 'switch costume...' target: self selector: #chooseCostumeIn: argument: aWorld.""	aMenu add: 'get info...' action: #getInfo.  "! !!PluggableButtonMorph methodsFor: 'private' stamp: 'sma 6/5/2000 13:31'!invokeMenu: evt	"Invoke my menu in response to the given event."	| menu |	menu _ self getMenu: evt shiftPressed.	menu ifNotNil: [menu popUpEvent: evt]! !!Scamper methodsFor: 'menus' stamp: 'sma 6/5/2000 14:00'!displayHistory	"Let the user selecet a previous page to view."	| menu |	menu _ MenuMorph entitled: 'Recent URLs'.	menu defaultTarget: self.	menu addStayUpItem.	menu addLine.	recentDocuments reverseDo:		[:doc |		menu add: doc url toText selector: #displayDocument: argument: doc].	menu popUpForHand: self currentHand! !!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sma 6/5/2000 13:34'!chooseFrequency	| currentFrequency aMenu |	currentFrequency _ self scriptInstantiation frequency.	currentFrequency = 0 ifTrue: [currentFrequency _ 1].	aMenu _ MenuMorph new defaultTarget: self.	#(1 2 5 10 25 50 100 1000 5000 10000) do:		[:i | aMenu add: i printString selector: #setFrequencyTo: argument: i].		aMenu add: 'other...' action: #typeInFrequency.	aMenu addTitle: 'Choose frequency (current: ', currentFrequency printString, ')'.	aMenu  popUpEvent: self currentEvent! !!ScrollPane methodsFor: 'scroll bar events' stamp: 'sma 6/5/2000 13:36'!yellowButtonActivity: shiftKeyState	| menu |	(menu _ self getMenu: shiftKeyState) ifNotNil:		[menu setInvokingView: self.		menu popUpEvent: self activeHand lastEvent]! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'sma 6/5/2000 13:33'!scrollBarMenuButtonPressed: event	| menu |	(menu _ self getMenu: event shiftPressed) ifNotNil:		["Set up to use perform:orSendTo: for model/view dispatch"		menu setInvokingView: self.		menu popUpEvent: event]! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'sma 6/5/2000 13:33'!yellowButtonActivity	"Called when the shifted-menu's 'more' item is chosen"	| menu |	(menu _ self getMenu: false) ifNotNil:		["Set up to use perform:orSendTo: for model/view dispatch"		menu setInvokingView: self.		menu popUpEvent: self currentEvent]! !!SystemWindow methodsFor: 'menu' stamp: 'sma 6/5/2000 13:34'!offerWindowMenu	| aMenu |	aMenu _ self buildWindowMenu.	model ifNotNil:		[model addModelItemsToWindowMenu: aMenu].	aMenu popUpEvent: self currentEvent! !!TwoWayScrollPane methodsFor: 'scroll bar events' stamp: 'sma 6/5/2000 13:36'!yellowButtonActivity: shiftKeyState	| menu |	(menu _ self getMenu: shiftKeyState) ifNotNil:		[menu setInvokingView: self.		menu popUpEvent: self activeHand lastEvent]! !!Utilities class methodsFor: 'text styles and fonts' stamp: 'sma 6/5/2000 14:07'!promptForFont: aPrompt andSendTo: aTarget withSelector: aSelector        "Utilities promptForFont: 'Choose system font:' andSendTo: Utilities withSelector: #setSystemFontTo:"        "NOTE: Morphic ONLY!!!!.  Derived from a method written by Robin Gibson"        | menu subMenu |        menu _ MenuMorph entitled: aPrompt.        Utilities actualTextStyles keys do: [:styleName|                subMenu _ self fontMenuForStyle: styleName target: aTarget selector: aSelector.                menu add: styleName subMenu: subMenu.                menu lastItem font: ((TextStyle named: styleName) fontOfSize: 18)].        menu popUpForHand: self currentHand! !MenuMorph removeSelector: #positionMenuAt:relativeTo:!!MenuMorph reorganize!('accessing' allWordings allWordingsNotInSubMenu: hasSubMenu: itemWithWording: items lastItem lastSelection lastSelection: popUpOwner popUpOwner: stayUp stayUp:)('construction' add:action: add:selector:argument: add:subMenu: add:target:action: add:target:selector: add:target:selector:argument: add:target:selector:argumentList: addLine addList: addStayUpItem addTitle: addTitle:updatingSelector:updateTarget: addUpdating:action: addUpdating:enablement:action: addUpdating:enablementSelector:target:selector:argumentList: addUpdating:target:action: addUpdating:target:selector:argumentList: balloonTextForLastItem: defaultTarget: labels:lines:selections: title:)('control' deleteIfPopUp deleteIfPopUpFrom:event: invokeItem: invokeItem:event: isCandidateForAutomaticViewing justDroppedInto:event: popUpAdjacentTo:forHand:from: popUpAt:event: popUpAt:forHand: popUpAt:forHand:from: popUpEvent: popUpForHand: willingToBeEmbeddedUponLanding)('copying' veryDeepFixupWith: veryDeepInner:)('initialization' initialize setDefaultParameters setTitleParametersFor:)('layout' minHeightWhenEmpty minWidthWhenEmpty)('menu' addCustomMenuItems:hand: addItem addTitle canDetachSubMenu: detachSubMenu: removeItem: setInvokingView: setTarget: toggleStayUp:)('private' positionAt: positionAt:relativeTo: selectedItem)!MenuItemMorph removeSelector: #handlesMouseOver:!