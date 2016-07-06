'From Squeak3.3alpha of 18 January 2002 [latest update: #4956] on 17 August 2002 at 10:14:57 pm'!"Change Set:		searchInViewer-swDate:			17 August 2002Author:			Scott WallacePublished as 4961searchInViewer-sw.cs to 3.3a.Adds a search-pane to the Viewer.  Type in to it, and hit RETURN or press the Search button. A fresh Viewer now comes with a search pane at the top, though this can be subsequently deleted if desired, and multiple search panes are allowed -- a new search pane can be added at any time by choosing 'add search pane' from the viewer's menu.In the case of etoy vocabularies, the search is done in the current natural language; in the case of a viewer on an arbitrary non-morph object, the search is done for keywords in actual method selectors.NOTE: This work was prepared for, and published to, Squeak 3.3a, but it is compatible with 3.2 as well."!CategoryViewer subclass: #SearchingViewer	instanceVariableNames: 'searchString '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Scripting'	module: #(Squeak EToy Scripting)!!SearchingViewer commentStamp: 'sw 8/16/2002 22:43' prior: 0!A SearchingViewer is a custom Viewer which has a type-in 'search' pane; the user types a word or fragment into the search pane and hits the 'search' button (or hits Return or Enter) and the pane gets populated with all the phrases that match (in the currently-installed language) the search-string.!!StandardViewer commentStamp: 'sw 8/17/2002 02:04' prior: 0!A structure that allows you to view state and behavior of an object; it consists of a header and then any number of CategoryViewers.!!Object methodsFor: 'viewer' stamp: 'sw 8/11/2002 02:03'!offerViewerMenuFor: aViewer event: evt	"Offer the primary Viewer menu to the user.  Copied up from Player code, but most of the functions suggested here don't work for non-Player objects, many aren't even defined, some relate to exploratory sw work not yet reflected in the current corpus.  We are early in the life cycle of this method..."	| aMenu |	aMenu _ MenuMorph new defaultTarget: self.	aMenu addStayUpItem.	aMenu title: '**CAUTION -- UNDER CONSTRUCTION!!**Many things may not work!!', self nameForViewer.	(aViewer affordsUniclass and: [self belongsToUniClass not]) ifTrue:		[aMenu add: 'give me a Uniclass' action: #assureUniClass.		aMenu addLine].	aMenu add: 'choose vocabulary...' target: aViewer action: #chooseVocabulary.	aMenu add: 'choose limit class...' target: aViewer action: #chooseLimitClass.	aMenu add: 'add search pane' target: aViewer action: #addSearchPane.	aMenu balloonTextForLastItem: 'Specify which class should be the most generic one to have its methods shown in this Viewer'.	aMenu addLine.	self belongsToUniClass ifTrue:		[aMenu add: 'add a new instance variable' target: self selector: #addInstanceVariableIn: argument: aViewer.		aMenu add: 'add a new script' target: aViewer selector: #newPermanentScriptIn: argument: aViewer.		aMenu addLine.		aMenu add: 'make my class be first-class' target: self selector: #makeFirstClassClassIn: argument: aViewer.		aMenu add: 'move my changes up to my superclass' target: self action: #promoteChangesToSuperclass.		aMenu addLine].	aMenu add: 'tear off a tile' target: self selector: #launchTileToRefer.	aMenu addLine.	aMenu add: 'inspect me' target: self selector: #inspect.	aMenu add: 'inspect my class' target: self class action: #inspect.	aMenu addLine.	aMenu add: 'browse vocabulary' action: #haveFullProtocolBrowsed.	aMenu add: 'inspect this Viewer' target: aViewer action: #inspect.	aMenu popUpEvent: evt in: aViewer currentWorld"	aMenu add: 'references to me' target: aViewer action: #browseReferencesToObject.	aMenu add: 'toggle scratch pane' target: aViewer selector: #toggleScratchPane.	aMenu add: 'make a nascent script for me' target: aViewer selector: #makeNascentScript.	aMenu add: 'rename me' target: aViewer selector: #chooseNewNameForReference.	aMenu add: 'browse full' action: #browseOwnClassFull.	aMenu add: 'browse hierarchy' action: #browseOwnClassHierarchy.	aMenu add: 'set user level...' target: aViewer action: #setUserLevel.	aMenu add: 'browse sub-protocol' action: #browseOwnClassSubProtocol.	aMenu addLine."! !!Object methodsFor: 'viewer' stamp: 'sw 12/11/2001 16:31'!tilePhrasesForSelectorList: aList inViewer: aViewer	"Return a collection of phrases for the list.  If using classic tiles, only include phrases that have fewer than two arguments, because all that they can handle."	| interfaces itsSelector toSuppress aVocab |	aVocab _ aViewer currentVocabulary.	interfaces _ aList collect: [:aSel | aVocab methodInterfaceForSelector: aSel class: self class].	interfaces _ self methodInterfacesInPresentationOrderFrom: interfaces forCategory: #search.	toSuppress _ aViewer currentVocabulary phraseSymbolsToSuppress.	interfaces _ interfaces select: [:int | (toSuppress includes: int selector) not].	Preferences messengersInViewers ifTrue:		[^ interfaces collect:			[:anInterface | self messengerFrom: anInterface freeStanding: false]].	Preferences universalTiles ifFalse:		[interfaces _ interfaces select:			[:int |				itsSelector _ int selector.				itsSelector numArgs < 2 or:					"The lone two-arg loophole in classic tiles"					[#(color:sees:) includes: itsSelector]]].	^ interfaces collect:		[:aMethodInterface |			aMethodInterface wantsReadoutInViewer				ifTrue:					[aViewer phraseForVariableFrom: aMethodInterface]				ifFalse:					[aViewer phraseForCommandFrom: aMethodInterface]]! !!CategoryViewer methodsFor: 'initialization' stamp: 'sw 8/17/2002 01:54'!initializeFor: aPlayer categoryChoice: aChoice	"Initialize the receiver to be associated with the player and category specified"	self listDirection: #topToBottom;		hResizing: #spaceFill;		vResizing: #spaceFill;		borderWidth: 1;		beSticky.	self color: Color green muchLighter muchLighter.	scriptedPlayer _ aPlayer.	self addHeaderMorph.	self chosenCategorySymbol: aChoice! !!CategoryViewer methodsFor: 'initialization' stamp: 'sw 8/17/2002 01:23'!setCategorySymbolFrom: aChoice	"Set my category symbol"	self chosenCategorySymbol: aChoice asSymbol! !!CategoryViewer methodsFor: 'header pane' stamp: 'sw 12/11/2001 19:08'!addHeaderMorph	"Add the header at the top of the viewer, with a control for choosing the category, etc."	| header aFont aButton |	header _ AlignmentMorph newRow color: self color; wrapCentering: #center; cellPositioning: #leftCenter.	aFont _ Preferences standardButtonFont.	header addMorph: (aButton _ SimpleButtonMorph new label: 'O' font: aFont).	aButton target: self;			color:  Color tan;			actionSelector: #delete;			setBalloonText: 'remove this pane from the screendon''t worry -- nothing will be lost!!.'.	self maybeAddArrowsTo: header.	header beSticky.	self addMorph: header.	self addNamePaneTo: header.	chosenCategorySymbol _ #basic! !!CategoryViewer methodsFor: 'header pane' stamp: 'sw 12/11/2001 15:37'!addNamePaneTo: header	"Add the namePane, which may be a popup or a type-in depending on the type of CategoryViewer"	| aButton |	namePane _ RectangleMorph newSticky color: Color brown veryMuchLighter.	namePane borderWidth: 0.	aButton _ (StringButtonMorph contents: '-----' font: (StrikeFont familyName: #NewYork size: 12)) color: Color black.	aButton target: self; arguments: Array new; actionSelector: #chooseCategory.	aButton actWhen: #buttonDown.	namePane addMorph: aButton.	aButton position: namePane position.	namePane align: namePane topLeft with: (bounds topLeft + (50 @ 0)).	namePane setBalloonText: 'category (click here to choose a different one)'.	header addMorphBack: namePane.	(namePane isKindOf: RectangleMorph) ifTrue:		[namePane addDropShadow.		namePane shadowColor: Color gray]! !!CategoryViewer methodsFor: 'header pane' stamp: 'sw 12/11/2001 19:12'!maybeAddArrowsTo: header	"Maybe add up/down arrows to the header"	| wrpr |	header addTransparentSpacerOfSize: 5@5.	header addUpDownArrowsFor: self.	(wrpr _ header submorphs last) submorphs second setBalloonText: 'previous category'.		wrpr submorphs first  setBalloonText: 'next category'! !!CategoryViewer methodsFor: 'support' stamp: 'sw 8/17/2002 01:11'!categoryRestorationInfo	"Answer info needed to reincarnate myself"	^ self chosenCategorySymbol! !!Player methodsFor: 'misc' stamp: 'sw 12/11/2001 15:48'!offerViewerMenuFor: aViewer event: evt	"Put up the Viewer menu on behalf of the receiver.  If the shift key is held down, put up the alternate menu."	| aMenu aWorld  |	(evt notNil and: [evt shiftPressed]) ifTrue:		[^ self offerAlternateViewerMenuFor: aViewer event: evt].	aWorld _ aViewer world.	aMenu _ MenuMorph new defaultTarget: self.	aMenu add: 'add a new instance variable' target: self action: #addInstanceVariable.	aMenu balloonTextForLastItem: 'Add a new instance variable to this object and all of its siblings.  You will be asked to supply a name for it.'.	aMenu add: 'add a new script' target: aViewer action: #newPermanentScript.	aMenu balloonTextForLastItem: 'Add a new script that will work for this object and all of its siblings'.	aMenu addLine.	aMenu add: 'grab me' target: self selector: #grabPlayerIn: argument: aWorld.	aMenu balloonTextForLastItem: 'This will actually pick up the object this Viewer is looking at, and hand it to you.  Click the (left) button to drop it'.	aMenu add: 'reveal me' target: self selector: #revealPlayerIn: argument: aWorld.	aMenu balloonTextForLastItem: 'If you have misplaced the object that this Viewer is looking at, use this item to (try to) make it visible'.	aMenu addLine.	aMenu add: 'tile representing me' action: #tearOffTileForSelf.	aMenu add: 'add search pane' target: aViewer action: #addSearchPane.	aMenu addLine.	aMenu add: 'more...' target: self selector: #offerAlternateViewerMenuFor:event: argumentList: {aViewer. evt}.	aMenu popUpEvent: evt in: aWorld! !!Preferences class methodsFor: 'hard-coded prefs' stamp: 'sw 8/11/2002 02:18'!messengersInViewers	"A coming technology..."	^ false! !!Presenter methodsFor: 'viewer' stamp: 'sw 8/17/2002 22:11'!updateViewer: aViewer forceToShow: aCategorySymbol	"Update the given viewer to make sure it is in step with various possible changes in the outside world, and when reshowing it be sure it shows the given category"	| aPlayer aPosition newViewer oldOwner wasSticky barHeight itsVocabulary aCategory categoryInfo |	aCategory _ aCategorySymbol ifNotNil: [aViewer currentVocabulary translatedWordingFor: aCategorySymbol].	categoryInfo _ aViewer categoryMorphs  asOrderedCollection collect:		[:aMorph | aMorph categoryRestorationInfo].	itsVocabulary _ aViewer currentVocabulary.	aCategory ifNotNil: [(categoryInfo includes: aCategorySymbol) ifFalse: [categoryInfo addFirst: aCategorySymbol]].	aPlayer _ aViewer scriptedPlayer.	aPosition _ aViewer position.	wasSticky _ aViewer isSticky.	newViewer _ aViewer species new visible: false.	barHeight _ aViewer submorphs first listDirection == #topToBottom		ifTrue:			[aViewer submorphs first submorphs first height]		ifFalse:			[0].	Preferences viewersInFlaps ifTrue:		[newViewer setProperty: #noInteriorThumbnail toValue: true].	newViewer rawVocabulary: itsVocabulary.	newViewer limitClass: aViewer limitClass.	newViewer initializeFor: aPlayer barHeight: barHeight includeDismissButton: aViewer hasDismissButton showCategories: categoryInfo.	wasSticky ifTrue: [newViewer beSticky].	oldOwner _ aViewer owner.	oldOwner ifNotNil:		[oldOwner replaceSubmorph: aViewer by: newViewer].		"It has happened that old readouts are still on steplist.  We may see again!!"	newViewer position: aPosition.	newViewer enforceTileColorPolicy.	newViewer visible: true.	newViewer world doIfNotNil: [:aWorld | aWorld startSteppingSubmorphsOf: newViewer].	newViewer layoutChanged! !!SearchingViewer methodsFor: 'initialization' stamp: 'sw 12/11/2001 15:44'!addNamePaneTo: header	"Add the namePane, which may be a popup or a type-in depending on the type of CategoryViewer"	| plugTextMor searchButton |	namePane _ AlignmentMorph newRow vResizing: #spaceFill; height: 14.	namePane hResizing: #spaceFill.	namePane listDirection: #leftToRight.	plugTextMor _ PluggableTextMorph on: self					text: #searchString accept: #searchString:notifying:					readSelection: nil menu: nil.	plugTextMor setProperty: #alwaysAccept toValue: true.	plugTextMor askBeforeDiscardingEdits: false.	plugTextMor acceptOnCR: true.	plugTextMor setTextColor: Color brown.	plugTextMor setNameTo: 'Search'.	plugTextMor vResizing: #spaceFill; hResizing: #spaceFill.	plugTextMor hideScrollBarIndefinitely.	plugTextMor setTextMorphToSelectAllOnMouseEnter.	searchButton _ SimpleButtonMorph new 		target: self;		beTransparent;		label: 'Search';		actionSelector: #doSearchFrom:;		arguments: {plugTextMor}.	searchButton setBalloonText: 'Type some letters into the pane at right, and then press this Search button (or hit RETURN) and all method selectors that match what you typed will appear in the list below.'.	namePane addMorphFront: searchButton.	namePane addTransparentSpacerOfSize: 6@0.	namePane addMorphBack: plugTextMor.	header addMorphBack: namePane! !!SearchingViewer methodsFor: 'initialization' stamp: 'sw 8/17/2002 02:03'!initializeFor: aPlayer categoryChoice: aChoice	"Initialize the receiver to be associated with the player and category specified"	super initializeFor: aPlayer categoryChoice: #search.	(namePane findA: PluggableTextMorph) setText: aChoice second asText.	self setCategorySymbolFrom: aChoice! !!SearchingViewer methodsFor: 'initialization' stamp: 'sw 12/11/2001 19:12'!maybeAddArrowsTo: header	"Maybe add up/down arrows to the header"	header addTransparentSpacerOfSize: 5@5! !!SearchingViewer methodsFor: 'initialization' stamp: 'sw 8/17/2002 01:24'!setCategorySymbolFrom: aChoice	"Set my category symbol"	self chosenCategorySymbol: #search.	self searchString: aChoice second notifying: nil! !!SearchingViewer methodsFor: 'search' stamp: 'sw 8/17/2002 01:58'!doSearchFrom:  aString	"Perform the search operation"	| searchFor aVocab aList all anInterface useTranslations scriptNames |	searchString _ aString asString.	searchFor _ searchString asString asLowercase withBlanksTrimmed.	aVocab _ self outerViewer currentVocabulary.	((scriptedPlayer isKindOf: Player) and: [aVocab isKindOf: EToyVocabulary])		ifTrue:			[all _ scriptedPlayer costume selectorsForViewer.			all addAll: (scriptNames _ scriptedPlayer class namedTileScriptSelectors).			useTranslations _ true]		ifFalse:			[all _ scriptedPlayer class allSelectorsUnderstood.			useTranslations _ false].	aList _ all select:		[:aSelector | (aVocab includesSelector: aSelector forInstance: scriptedPlayer ofClass: scriptedPlayer class limitClass: ProtoObject) and:			[(useTranslations and: [(anInterface _ aVocab methodInterfaceAt: aSelector ifAbsent: [nil]) notNil and: [anInterface elementWording includesSubstring: searchFor caseSensitive: false]])				or:					[((scriptNames includes: aSelector) or: [useTranslations not]) and:						[aSelector includesSubstring: searchFor caseSensitive: false]]]].	aList _ aList asSortedArray.	self removeAllButFirstSubmorph. "that being the header"	self addAllMorphs:		((scriptedPlayer tilePhrasesForSelectorList: aList inViewer: self)).	self enforceTileColorPolicy.	self secreteCategorySymbol.	self world ifNotNil: [self world startSteppingSubmorphsOf: self].	self adjustColorsAndBordersWithin.	owner ifNotNil: [owner isStandardViewer ifTrue: [owner fitFlap]]! !!SearchingViewer methodsFor: 'search' stamp: 'sw 8/11/2002 02:14'!searchString	"Answer the search string"	^ searchString ifNil: [searchString _ '']! !!SearchingViewer methodsFor: 'search' stamp: 'sw 12/11/2001 19:14'!searchString: aString notifying: znak	"Set the search string as indicated and carry out a search"	searchString _ aString.	self doSearchFrom: aString! !!SearchingViewer methodsFor: 'category wording' stamp: 'sw 12/11/2001 15:52'!categoryWording: aCategoryWording	"okay, thanks"! !!SearchingViewer methodsFor: 'category wording' stamp: 'sw 12/11/2001 19:13'!currentCategory	"Answer the symbol associated with the pane"	^ #search! !!SearchingViewer methodsFor: 'categories' stamp: 'sw 8/11/2002 01:59'!updateCategoryNameTo: aName	"Update the category name, because of a language change."	self doSearchFrom: (namePane findA: PluggableTextMorph) text.	self flag: #deferred.  "A nice touch would be to change the Button wording here"! !!SearchingViewer methodsFor: 'support' stamp: 'sw 8/17/2002 01:13'!categoryRestorationInfo	"Answer info needed to reincarnate myself"	^ Array with: self chosenCategorySymbol with: self searchString! !!StandardViewer methodsFor: 'categories' stamp: 'sw 8/17/2002 01:37'!addCategoryViewerFor: categoryInfo	"Add a category viewer for the given category info"	| aViewer |	aViewer _ ((categoryInfo isKindOf: Collection) and: [categoryInfo first == #search])		ifFalse:			[CategoryViewer new]		ifTrue:			[SearchingViewer new].	self addMorphBack: aViewer.	aViewer initializeFor: scriptedPlayer categoryChoice: categoryInfo.	self world ifNotNil: [self world startSteppingSubmorphsOf: aViewer].	self fitFlap.! !!StandardViewer methodsFor: 'categories' stamp: 'sw 8/17/2002 01:47'!addSearchPane	"Add a search pane"	self addCategoryViewerFor: #(search '')! !!StandardViewer methodsFor: 'initialization' stamp: 'sw 8/17/2002 01:26'!initializeFor: aPlayer barHeight: anInteger includeDismissButton: aBoolean showCategories: categoryInfo	"Initialize the receiver to be a look inside the given Player.  The categoryInfo, if present, describes which categories should be present in it, in which order"	scriptedPlayer _ aPlayer.	self listDirection: #topToBottom;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		borderWidth: 1.	self color: self standardViewerColor.	self addHeaderMorphWithBarHeight: anInteger includeDismissButton: aBoolean.	categoryInfo isEmptyOrNil		ifFalse:  "Reincarnating an pre-existing list"			[categoryInfo do:				[:aCat | self addCategoryViewerFor: aCat]]		ifTrue:  "starting fresh"			[self addSearchPane. 			self addCategoryViewer.			self addCategoryViewer].! !!Vocabulary methodsFor: 'queries' stamp: 'sw 8/11/2002 02:25'!methodInterfaceForSelector: aSelector class: aClass	"Answer a method interface for the selector"	^ self methodInterfaceAt: aSelector ifAbsent:		[MethodInterface new conjuredUpFor: aSelector class: aClass]! !StandardViewer removeSelector: #initializeFlapVersionFor:!