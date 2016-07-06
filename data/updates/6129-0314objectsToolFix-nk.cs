'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #308] on 4 September 2004 at 7:17:25 pm'!"Change Set:		ObjectsToolLayoutFix-nkDate:			3 September 2004Author:			Ned KonzA number of problems have plagued the ObjectsTool with respect to its layout.This change set fixes most of them by replacing the tableLayout in the outer morph with a proportional layout.It also reduces the amount of code.Also, it gets the objects tool working in the flap again.It adds the Objects flap to the list of standard flaps, but does not enable it by default."!!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 9/3/2004 12:56'!addStandardFlaps	"Initialize the standard default out-of-box set of global flaps. 	This method creates them and places them in my class 	variable #SharedFlapTabs, but does not itself get them 	displayed. "	SharedFlapTabs		ifNil: [SharedFlapTabs := OrderedCollection new].	SharedFlapTabs add: self newSqueakFlap.	SharedFlapTabs add: self newSuppliesFlap.	SharedFlapTabs add: self newToolsFlap.	SharedFlapTabs add: self newWidgetsFlap.	SharedFlapTabs add: self newStackToolsFlap.	SharedFlapTabs add: self newNavigatorFlap.	SharedFlapTabs add: self newPaintingFlap.	SharedFlapTabs add: self newObjectsFlap.	self disableGlobalFlapWithID: 'Stack Tools' translated.	self disableGlobalFlapWithID: 'Painting' translated.	self disableGlobalFlapWithID: 'Navigator' translated.	self disableGlobalFlapWithID: 'Objects' translated.	^ SharedFlapTabs! !!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 9/3/2004 12:51'!newObjectsFlap	"Answer a fully-instantiated flap named 'Objects' to be placed at the top of the screen."	|  aFlapTab anObjectsTool |	anObjectsTool _ ObjectsTool new.	anObjectsTool initializeForFlap.	aFlapTab _ FlapTab new referent: anObjectsTool beSticky.	aFlapTab setName: 'Objects' translated edge: #top color: Color red lighter.	aFlapTab position: ((Display width - (aFlapTab width + 22)) @ 0).	aFlapTab setBalloonText: aFlapTab balloonTextForFlapsMenu.	anObjectsTool extent: self currentWorld width @ 200.	anObjectsTool beFlap: true.	anObjectsTool color: Color red muchLighter.	anObjectsTool clipSubmorphs: true.	anObjectsTool showCategories.	^ aFlapTab! !!ObjectsTool methodsFor: 'alphabetic' stamp: 'nk 9/3/2004 13:47'!installQuads: quads fromButton: aButton	"Install items in the bottom pane that correspond to the given set of quads, as triggered from the given button"	| aPartsBin sortedQuads oldResizing |	aPartsBin _ self partsBin.	oldResizing := aPartsBin vResizing.	aPartsBin removeAllMorphs.	sortedQuads _ (PartsBin translatedQuads: quads)							asSortedCollection: [:a :b | a third < b third].	aPartsBin listDirection: #leftToRight quadList: sortedQuads.	aButton ifNotNil: [self tabsPane highlightOnlySubmorph: aButton].	aPartsBin vResizing: oldResizing.	aPartsBin layoutChanged; fullBounds.	self isFlap ifFalse: [ self minimizePartsBinSize ].! !!ObjectsTool methodsFor: 'alphabetic' stamp: 'nk 9/3/2004 12:13'!showAlphabeticTabs	"Switch to the mode of showing alphabetic tabs"	modeSymbol == #alphabetic ifTrue: [ ^self ].	self partsBin removeAllMorphs.	self initializeWithTabs: self alphabeticTabs.	self modeSymbol: #alphabetic.	self tabsPane submorphs first doButtonAction! !!ObjectsTool methodsFor: 'categories' stamp: 'nk 9/3/2004 13:43'!showCategories	"Set the receiver up so that it shows tabs for each of the standard categories"	modeSymbol == #categories ifTrue: [ ^self ].	self partsBin removeAllMorphs.	self initializeWithTabs: self tabsForCategories.	self modeSymbol: #categories.	self tabsPane submorphs first doButtonAction.! !!ObjectsTool methodsFor: 'categories' stamp: 'nk 9/3/2004 13:51'!showCategory: aCategoryName fromButton: aButton 	"Project items from the given category into my lower pane"	| quads |	self partsBin removeAllMorphs.	Cursor wait		showWhile: [quads := OrderedCollection new.			Morph withAllSubclasses				do: [:aClass | aClass theNonMetaClass						addPartsDescriptorQuadsTo: quads						if: [:aDescription | aDescription translatedCategories includes: aCategoryName]].			quads := quads						asSortedCollection: [:q1 :q2 | q1 third <= q2 third].			self installQuads: quads fromButton: aButton]! !!ObjectsTool methodsFor: 'initialization' stamp: 'nk 9/3/2004 13:46'!initializeForFlap	"Initialize the receiver to operate in a flap at the top of the screen."	"	Flaps newObjectsFlap openInWorld	"	| buttonPane aBin aColor heights tabsPane |	self basicInitialize.	self layoutInset: 0;		layoutPolicy: ProportionalLayout new;		hResizing: #shrinkWrap;		vResizing: #rigid;		borderWidth: 2; borderColor: Color darkGray;		extent: (self minimumWidth @ self minimumHeight).	"mode buttons"	buttonPane := self paneForTabs: self modeTabs.	buttonPane		vResizing: #shrinkWrap;		setNameTo: 'ButtonPane';		color: (aColor := buttonPane color) darker;		layoutInset: 6;		wrapDirection: nil;		width: self width;		layoutChanged; fullBounds.	"Place holder for a tabs or text pane"	tabsPane := Morph new		setNameTo: 'TabPane';		hResizing: #spaceFill;		yourself.	heights := { buttonPane height. 40 }.	buttonPane vResizing: #spaceFill.	self		addMorph: buttonPane		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 0)				offsets: (0 @ 0 corner: 0 @ heights first)).	self		addMorph: tabsPane		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 0)				offsets: (0 @ heights first corner: 0 @ (heights first + heights second))).	aBin := (PartsBin newPartsBinWithOrientation: #leftToRight from: #())		listDirection: #leftToRight;		wrapDirection: #topToBottom;		color: aColor lighter lighter;		setNameTo: 'Parts';		dropEnabled: false;		vResizing: #spaceFill;		yourself.	self		addMorph: aBin		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 1)				offsets: (0 @ (heights first + heights second) corner: 0 @ 0)).	aBin color: (Color orange muchLighter);		setNameTo: 'Objects' translated.	self color: (Color orange muchLighter);		setNameTo: 'Objects' translated.! !!ObjectsTool methodsFor: 'initialization' stamp: 'nk 9/3/2004 12:06'!initializeToStandAlone	"Initialize the receiver so that it can live as a stand-alone morph"	| buttonPane aBin aColor heights tabsPane |	self basicInitialize.	self layoutInset: 6;		layoutPolicy: ProportionalLayout new;		useRoundedCorners;		hResizing: #rigid;		vResizing: #rigid;		extent: (self minimumWidth @ self minimumHeight).	"mode buttons"	buttonPane := self paneForTabs: self modeTabs.	buttonPane		vResizing: #shrinkWrap;		setNameTo: 'ButtonPane';		addMorphFront: self dismissButton;		addMorphBack: self helpButton;		color: (aColor := buttonPane color) darker;		layoutInset: 6;		wrapDirection: nil;		width: self width;		layoutChanged; fullBounds.	"Place holder for a tabs or text pane"	tabsPane := Morph new		setNameTo: 'TabPane';		hResizing: #spaceFill;		yourself.	heights := { buttonPane height. 40 }.	buttonPane vResizing: #spaceFill.	self		addMorph: buttonPane		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 0)				offsets: (0 @ 0 corner: 0 @ heights first)).	self		addMorph: tabsPane		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 0)				offsets: (0 @ heights first corner: 0 @ (heights first + heights second))).	aBin := (PartsBin newPartsBinWithOrientation: #leftToRight from: #())		listDirection: #leftToRight;		wrapDirection: #topToBottom;		color: aColor lighter lighter;		setNameTo: 'Parts';		dropEnabled: false;		vResizing: #spaceFill;		yourself.	self		addMorph: aBin		fullFrame: (LayoutFrame				fractions: (0 @ 0 corner: 1 @ 1)				offsets: (0 @ (heights first + heights second) corner: 0 @ 0)).	self color: (Color r: 0.0 g: 0.839 b: 0.226);		setNameTo: 'Objects' translated;		showCategories.! !!ObjectsTool methodsFor: 'initialization' stamp: 'nk 9/3/2004 13:19'!tweakAppearanceAfterModeShift	"After the receiver has been put into a given mode, make an initial selection of category, if appropriate, and highlight the mode button."	self buttonPane submorphs do:		[:aButton | 			aButton borderWidth: 0.			(aButton valueOfProperty: #modeSymbol) = modeSymbol				ifTrue:					[aButton firstSubmorph color: Color red]				ifFalse:					[aButton firstSubmorph color: Color black]].! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 12:35'!extent: anExtent	"The user has dragged the grow box such that the receiver's extent would be anExtent.  Do what's needed"	self extent = anExtent ifTrue: [ ^self ].	super extent: anExtent.	self fixLayoutFrames.! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 13:44'!fixLayoutFrames	"Adjust the boundary between the tabs or search pane and the parts bin, giving preference to the tabs."	| oldY newY tp tpHeight |	oldY := ((tp := self tabsPane						ifNil: [self searchPane])				ifNil: [^ self]) layoutFrame bottomOffset.	tpHeight := tp hasSubmorphs				ifTrue: [(tp submorphBounds outsetBy: tp layoutInset) height]				ifFalse: [tp height].	newY := (self buttonPane				ifNil: [^ self]) height + tpHeight.	oldY = newY		ifTrue: [^ self].	tp layoutFrame bottomOffset: newY.	(self partsBin		ifNil: [^ self]) layoutFrame topOffset: newY.	submorphs		do: [:m | m layoutChanged ]! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 13:47'!minimizePartsBinSize	self layoutChanged; fullBounds.	self fixLayoutFrames.	self setExtentFromHalo: (self minimumWidth @ self minimumHeight) ! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 10:35'!minimumBottom	| iconsBottom partsBin |	partsBin := self partsBin ifNil: [ ^self bottom ].	iconsBottom := partsBin submorphs isEmpty		ifTrue: [ partsBin top + 60 ]		ifFalse: [ partsBin submorphBounds bottom + partsBin layoutInset ].	^iconsBottom + self layoutInset + self borderWidth! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 11:53'!minimumHeight	^(self minimumBottom - self top) max: 280! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 12:06'!minimumWidth	"Answer a width that assures that the alphabet fits in two rows"	^ 300! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 12:40'!position: aPoint	"The user has dragged the grow box such that the receiver's extent would be anExtent.  Do what's needed"	self position = aPoint ifTrue: [ ^self ].	super position: aPoint.	self fixLayoutFrames.! !!ObjectsTool methodsFor: 'layout' stamp: 'nk 9/3/2004 12:44'!setExtentFromHalo: anExtent	"The user has dragged the grow box such that the receiver's extent would be anExtent.  Do what's needed"	super setExtentFromHalo: ((anExtent x max: self minimumWidth) @ (anExtent y max: self minimumHeight)).! !!ObjectsTool methodsFor: 'major modes' stamp: 'nk 9/3/2004 13:32'!modeSymbol: aSymbol	"Set the receiver's modeSymbol as indicated"	modeSymbol _ aSymbol.	self tweakAppearanceAfterModeShift.! !!ObjectsTool methodsFor: 'search' stamp: 'nk 9/3/2004 11:20'!newSearchPane	"Answer a type-in pane for searches"	| aTextMorph |	aTextMorph _ TextMorph new		setProperty: #defaultContents toValue: ('' asText allBold addAttribute: (TextFontChange font3));		setTextStyle: (TextStyle fontArray: { Preferences standardEToysFont });		setDefaultContentsIfNil;		on: #keyStroke send: #searchPaneCharacter: to: self;		setNameTo: 'SearchPane';		setBalloonText: 'Type here and all entries that match will be shown.' translated;		vResizing: #shrinkWrap;		hResizing: #spaceFill;		margins: 4@6;		backgroundColor: Color white.	^ aTextMorph! !!ObjectsTool methodsFor: 'search' stamp: 'nk 9/3/2004 10:39'!setSearchStringFromSearchPane	"Set the search string by obtaining its contents from the search pane, and doing a certain amount of munging"	searchString _ self searchPane text string asLowercase withBlanksTrimmed.	searchString _ searchString copyWithoutAll: {Character enter. Character cr}! !!ObjectsTool methodsFor: 'search' stamp: 'nk 9/3/2004 13:51'!showMorphsMatchingSearchString	"Put items matching the search string into my lower pane"	| quads |	self setSearchStringFromSearchPane.	self partsBin removeAllMorphs.	Cursor wait		showWhile: [quads := OrderedCollection new.			Morph withAllSubclasses				do: [:aClass | aClass						addPartsDescriptorQuadsTo: quads						if: [:info | info formalName translated includesSubstring: searchString caseSensitive: false]].			self installQuads: quads fromButton: nil]! !!ObjectsTool methodsFor: 'search' stamp: 'nk 9/3/2004 12:13'!showSearchPane	"Set the receiver up so that it shows the search pane"	| tabsPane aPane frame |	modeSymbol == #search ifTrue: [ ^self ].	self partsBin removeAllMorphs.	tabsPane := self tabsPane.	aPane _ self newSearchPane.	aPane layoutChanged; fullBounds.	aPane layoutFrame: (frame := tabsPane layoutFrame copy).	frame bottomOffset: (frame topOffset + aPane height).	self replaceSubmorph: tabsPane by: aPane.	self partsBin layoutFrame topOffset: frame bottomOffset.	self modeSymbol: #search.	self showMorphsMatchingSearchString.	ActiveHand newKeyboardFocus: aPane! !!ObjectsTool methodsFor: 'submorph access' stamp: 'nk 9/3/2004 08:06'!buttonPane	"Answer the receiver's button pane, nil if none"	^ self submorphNamed: 'ButtonPane' ifNone: [].! !!ObjectsTool methodsFor: 'submorph access' stamp: 'nk 9/3/2004 08:09'!partsBin	^self findDeeplyA: PartsBin.! !!ObjectsTool methodsFor: 'submorph access' stamp: 'nk 9/3/2004 10:40'!searchPane	"Answer the receiver's search pane, nil if none"	^ self submorphNamed: 'SearchPane' ifNone: [].! !!ObjectsTool methodsFor: 'submorph access' stamp: 'nk 9/3/2004 13:51'!showAlphabeticCategory: aString fromButton: aButton 	"Blast items beginning with a given letter into my lower pane"	| eligibleClasses quads uc |	self partsBin removeAllMorphs.	uc := aString asUppercase asCharacter.	Cursor wait		showWhile: [eligibleClasses := Morph withAllSubclasses.			quads := OrderedCollection new.			eligibleClasses				do: [:aClass | aClass theNonMetaClass						addPartsDescriptorQuadsTo: quads						if: [:info | info formalName translated asUppercase first = uc]].			self installQuads: quads fromButton: aButton]! !!ObjectsTool methodsFor: 'submorph access' stamp: 'nk 9/3/2004 08:06'!tabsPane	"Answer the receiver's tabs pane, nil if none"	^ self submorphNamed: 'TabPane' ifNone: [].! !!ObjectsTool methodsFor: 'tabs' stamp: 'nk 9/3/2004 13:47'!initializeWithTabs: tabList	"Initialize the receiver to have the given tabs"	| oldPane newPane |	oldPane := self tabsPane ifNil: [ self searchPane ].	newPane := (self paneForTabs: tabList)		setNameTo: 'TabPane';		yourself.	newPane layoutFrame: oldPane layoutFrame.	self replaceSubmorph: oldPane by: newPane.	newPane layoutChanged; fullBounds.	self fixLayoutFrames.! !!ObjectsTool methodsFor: 'tabs' stamp: 'nk 9/3/2004 11:29'!paneForTabs: tabList 	"Answer a pane bearing tabs for the given list"	| aPane |	tabList do: [:t |			t color: Color transparent.			t borderWidth: 1;				borderColor: Color black].	aPane := AlignmentMorph newRow				listDirection: #leftToRight;				wrapDirection: #topToBottom;				vResizing: #spaceFill;				hResizing: #spaceFill;				cellInset: 6;				layoutInset: 4;				listCentering: #center;				listSpacing: #equal;				addAllMorphs: tabList;				yourself.	aPane width: self layoutBounds width.	^ aPane! !!Project methodsFor: 'language' stamp: 'nk 9/3/2004 13:00'!setFlaps	| flapTabs flapIDs sharedFlapTabs navigationMorph |	flapTabs _ ActiveWorld flapTabs.	flapIDs _ flapTabs collect: [:tab | tab knownName].	flapTabs		do: [:tab | (tab isMemberOf: ViewerFlapTab)				ifFalse: [tab isGlobalFlap						ifTrue: [Flaps removeFlapTab: tab keepInList: false.							tab currentWorld reformulateUpdatingMenus]						ifFalse: [| referent | 							referent _ tab referent.							referent isInWorld								ifTrue: [referent delete].							tab delete]]].	sharedFlapTabs _ Flaps classPool at: #SharedFlapTabs.	flapIDs		do: [:id | 			id = 'Navigator' translated				ifTrue: [sharedFlapTabs add: Flaps newNavigatorFlap].			id = 'Widgets' translated				ifTrue: [sharedFlapTabs add: Flaps newWidgetsFlap].			id = 'Tools' translated				ifTrue: [sharedFlapTabs add: Flaps newToolsFlap].			id = 'Squeak' translated				ifTrue: [sharedFlapTabs add: Flaps newSqueakFlap].			id = 'Supplies' translated				ifTrue: [sharedFlapTabs add: Flaps newSuppliesFlap].			id = 'Stack Tools' translated				ifTrue: [sharedFlapTabs add: Flaps newStackToolsFlap].			id = 'Painting' translated				ifTrue: [sharedFlapTabs add: Flaps newPaintingFlap].			id = 'Objects' translated				ifTrue: [sharedFlapTabs add: Flaps newObjectsFlap ]].	2 timesRepeat: [flapIDs do: [:id | Flaps enableDisableGlobalFlapWithID: id]].	ActiveWorld flapTabs		do: [:flapTab | flapTab isCurrentlyTextual				ifTrue: [flapTab changeTabText: flapTab knownName]].	Flaps positionNavigatorAndOtherFlapsAccordingToPreference.	navigationMorph _ World findDeeplyA: ProjectNavigationMorph preferredNavigator.	navigationMorph isNil		ifTrue: [^ self].	navigationMorph allMorphs		do: [:morph | morph class == SimpleButtonDelayedMenuMorph				ifTrue: [(morph findA: ImageMorph) isNil						ifTrue: [| label | 							label _ morph label.							label isNil								ifFalse: [| name | 									name _ morph knownName.									name isNil										ifTrue: [morph name: label.											name _ label].									morph label: name translated]]]]! !ObjectsTool removeSelector: #prepareInitialAppearanceForTabs:!!ObjectsTool reorganize!('alphabetic' alphabeticTabs installQuads:fromButton: showAlphabeticTabs)('categories' showCategories showCategory:fromButton: tabsForCategories)('initialization' initializeForFlap initializeToStandAlone tweakAppearanceAfterModeShift)('layout' extent: fixLayoutFrames minimizePartsBinSize minimumBottom minimumHeight minimumWidth position: setExtentFromHalo:)('major modes' modeSymbol modeSymbol: modeTabs)('menu' addCustomMenuItems:hand: resetThumbnails)('miscellaneous')('search' newSearchPane searchPaneCharacter: setSearchStringFromSearchPane showMorphsMatchingSearchString showSearchPane)('submorph access' buttonPane partsBin searchPane showAlphabeticCategory:fromButton: tabsPane)('tabs' initializeWithTabs: paneForTabs: presentHelp)!