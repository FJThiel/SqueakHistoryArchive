'From Squeak 2.2 of Sept 23, 1998 on 10 November 1998 at 1:03:13 pm'!"Change Set:		Oops-swDate:			10 November 1998Author:			Scott WallaceQuick fixes for two oversights in the Kubwa update:*  The tile-script-command to 'initiate painting' is restored to the viewer.*  The obsolete 'establish as custom parts bin' menu item is removed from BookMorph's menu"!!BookMorph methodsFor: 'menu' stamp: 'sw 11/10/1998 12:57'!addBookMenuItemsTo: aMenu hand: aHandMorph	| controlsShowing subMenu |	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'previous page' action: #previousPage.	subMenu add: 'next page' action: #nextPage.	subMenu add: 'insert a page' action: #insertPage.	subMenu add: 'delete this page' action: #deletePage.	controlsShowing _ self hasSubmorphWithProperty: #pageControl.	controlsShowing		ifTrue: [subMenu add: 'hide page controls' action: #hidePageControls]		ifFalse: [subMenu add: 'show page controls' action: #showPageControls].	subMenu add: 'sort pages' action: #sortPages:.	subMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil:		[subMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	(aHandMorph classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[subMenu add: 'paste book page'	action: #pasteBookPage].	aMenu add: 'book...' subMenu: subMenu! !!Player methodsFor: 'slots-kernel' stamp: 'sw 11/10/1998 12:45'!categories	| aList |	aList _ #('basic' ) asOrderedCollection.	self slotNames size > 0 ifTrue:		[aList add: 'instance variables'].	self class scripts size > 0 ifTrue:		[aList add: 'scripts'].	(self hasCostumeOfClass: JoystickMorph)		ifTrue:	[aList add: 'joystick'].	(self hasCostumeOfClass: BookMorph)		ifTrue:	[aList add: 'book navigation'].	(self hasCostumeOfClass: PasteUpMorph)		ifTrue:	[aList addAll: #('pen trails' 'card/stack' 'playfield')].	aList addAll: #('tests' 'color & border' 'geometry' 'motion' 'pen use' 'miscellaneous' ).	Preferences showPlayerSource ifTrue:		[aList addAll: Player organization categories].	aList removeAllFoundIn: #('card/stack commands' 'object fileIn').	^ aList! !!Player methodsFor: 'slots-kernel' stamp: 'sw 11/10/1998 13:03'!tilePhrasesSpecsForCategory: aCategory	"Return an array of slot and script names and info for use in a viewer on the receiver.  These can be of two flavors - script and slot.		(slot		heading		number				readWrite	getHeading		setHeading:)		(script		command 	wearCostumeOf: 	player)"	| aList nameString |	(aCategory = 'instance variables') ifTrue:		[^ self slotNames collect: [:aName |		nameString _ aName asString capitalized.		Array			with:	#slot			with: 	aName 								"name"			with: 	(self typeForSlot: aName asSymbol)	"type"			with:	#readWrite							"r/w"			with:	('get', nameString) asSymbol		"get selector"			with:	('set', nameString, ':') asSymbol]].	"set selector"	(aCategory = 'scripts') ifTrue:		[^ self tileScriptCommands].	(aCategory = 'basic') ifTrue:		[aList _ #((slot x) (slot y) (slot heading) (slot colorUnder) (script forward:) (script turn:) (script beep:))].	(aCategory = 'tests') ifTrue:		[aList _ #((slot isOverColor) (slot isUnderMouse) (slot colorSees) )].	(aCategory = 'color & border') ifTrue:		[aList _ #((slot color) (slot colorUnder) (slot borderColor) (slot borderWidth))].	(aCategory = 'geometry') ifTrue:		[aList _ #((slot left) (slot right) (slot top) (slot bottom) (slot width) (slot height) (slot x) (slot y) (slot heading))].	(aCategory = 'miscellaneous') ifTrue:		[aList _ #((script show) (script hide) (script wearCostumeOf:) (script startScript:) (script stopScript:) (script pauseScript:))]. 	(aCategory = 'motion') ifTrue:		[aList _ #((slot x) (slot y) (slot heading) (script forward:) (script moveToward:) (script turn:) (script bounce:) (script wrap) (script goToRightOf:))].	(aCategory = 'pen trails') ifTrue:		[aList _ #((script liftAllPens) (script lowerAllPens) (script clearTurtleTrails))].	(aCategory = 'pen use') ifTrue:		[aList _ #((slot penColor) (slot penSize) (slot penDown))].	(aCategory = 'card/stack') ifTrue:		[aList _ #((script goToNextCard) (script goToPreviousCard) (script deleteCard) (script newCard))].	(aCategory = 'joystick') ifTrue:		[aList _ #((slot amount) (slot angle) (slot leftRight) (slot upDown))].	(aCategory = 'playfield') ifTrue:		[aList _ #((script initiatePainting))].	(aCategory = 'book navigation') ifTrue:		[aList _ #((script nextPage) (script previousPage) (script firstPage) (script lastPage) (script goto:))].	aList ifNil:		[^ (Player organization categories includes: aCategory)			ifTrue:				[self tilePhraseSpecsForPlayerCategory: aCategory]			ifFalse:				[self error: 'faulty category: ', aCategory.				Array new]].	^ aList collect: [:aPair | self phraseSpecFor: aPair]! !