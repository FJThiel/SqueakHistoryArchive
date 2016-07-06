'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 12 July 2003 at 12:38:56 pm'!"Change Set:		RemHardcodedFonts-dgd-nkDate:			26 November 2003Author:			Diego Gomez Deck <DiegoGomezDeck@ConsultAr.com>Combination of:- RemoveOfHardcodedEToysFonts-dgd (http://lists.squeakfoundation.org/pipermail/squeak-dev/2003-July/062488.html)- RemoveHardcodedFontsFix-nk (http://lists.squeakfoundation.org/pipermail/squeak-dev/2003-July/062501.html).- and objectsthumbnailfix-nk.1.cs (agreed with marcus)4 simple conflicts with 3.7a (5566) solved:- CategoryViewer class>>addNamePaneTo:- Preferences>>fontConfigurationMenu- ScriptEditorMorph class>>buttonRowForEditor- ScriptStatusControl class>>initializeFor:Preferences class>>fontConfigurationMenu also includes missing #translated messagesAlso includes the changes in ObjectsThumbnailFix-nk.1.cs to avoid another conflict (agreed with Marcus Denker)"!!Morph methodsFor: 'WiW support' stamp: 'nk 7/12/2003 08:59'!eToyRejectDropMorph: morphToDrop event: evt	| tm am |	tm _ TextMorph new 		beAllFont: ((TextStyle named: Preferences standardEToysFont familyName) fontOfSize: 24);		contents: 'GOT IT!!'.	(am _ AlignmentMorph new)		color: Color yellow;		layoutInset: 10;		useRoundedCorners;		vResizing: #shrinkWrap;		hResizing: #shrinkWrap;		addMorph: tm;		fullBounds;		position: (self bounds center - (am extent // 2));		openInWorld: self world.	(SampledSound soundNames includes: 'yum') ifFalse: [		(FileDirectory default fileExists: '') ifTrue: [			SampledSound addLibrarySoundNamed: 'yum' fromAIFFfileNamed: 'yum.aif'		].	].	(SampledSound soundNames includes: 'yum') ifTrue: [		SampledSound playSoundNamed: 'yum'	].	morphToDrop rejectDropMorphEvent: evt.		"send it back where it came from"	am delete! !!CategoryViewer methodsFor: 'entries' stamp: 'nk 7/12/2003 06:58'!phraseForVariableFrom: aMethodInterface 	"Return a structure consisting of tiles and controls and a 	readout representing a 'variable' belonging to the player, 	complete with an appropriate readout when indicated. 	Functions in both universalTiles mode and classic mode. 	Slightly misnamed in that this path is used for any 	methodInterface that indicates an interesting resultType."	| anArrow slotName getterButton cover inner aRow doc setter tryer universal buttonFont |	aRow := ViewerLine newRow color: self color;				 beSticky;				 elementSymbol: (slotName := aMethodInterface selector);				 wrapCentering: #center;				 cellPositioning: #leftCenter.	(universal := scriptedPlayer isUniversalTiles)		ifFalse: [ buttonFont _ Preferences standardEToysFont.			aRow addMorphBack: (Morph new color: self color;					 extent: (((buttonFont widthOfString: '!!') + 6) @ (buttonFont height + 6));					 yourself)].	"spacer"	aRow		addMorphBack: (self infoButtonFor: slotName).	aRow addMorphBack: (Morph new color: self color;			 extent: 0 @ 10).	"spacer"	universal		ifTrue: [inner := scriptedPlayer universalTilesForGetterOf: aMethodInterface.			cover := Morph new color: Color transparent.			cover extent: inner fullBounds extent.			(getterButton := cover copy) addMorph: cover;				 addMorphBack: inner.			cover				on: #mouseDown				send: #makeUniversalTilesGetter:event:from:				to: self				withValue: aMethodInterface.			aRow addMorphFront: (tryer := ScriptingSystem tryButtonFor: inner).			tryer color: tryer color lighter lighter]		ifFalse: [aRow addMorphBack: self tileForSelf bePossessive.			aRow addMorphBack: (Morph new color: self color;					 extent: 2 @ 10).			"spacer"			getterButton := self getterButtonFor: aMethodInterface selector type: aMethodInterface resultType].	aRow addMorphBack: getterButton.	(doc := aMethodInterface documentationOrNil)		ifNotNil: [getterButton setBalloonText: doc].	universal		ifFalse: [slotName == #seesColor:				ifTrue: [self addIsOverColorDetailTo: aRow.					^ aRow].			slotName == #touchesA:				ifTrue: [self addTouchesADetailTo: aRow.					^ aRow].			slotName == #overlaps:				ifTrue: [self addOverlapsDetailTo: aRow.					^ aRow]].	aRow addMorphBack: AlignmentMorph new beTransparent.	"flexible spacer"	(setter := aMethodInterface companionSetterSelector)		ifNotNil: [aRow addMorphBack: (Morph new color: self color;					 extent: 2 @ 10).			"spacer"			anArrow := universal						ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from: args: aMethodInterface]						ifFalse: [self								arrowSetterButton: #makeSetter:from:forPart:								args: (Array with: slotName with: aMethodInterface resultType)].			aRow addMorphBack: anArrow].	(#(#color:sees: #playerSeeingColor #copy #touchesA: #overlaps: ) includes: slotName)		ifFalse: [(universal					and: [slotName == #seesColor:])				ifFalse: [aRow						addMorphBack: (self								readoutFor: slotName								type: aMethodInterface resultType								readOnly: setter isNil								getSelector: aMethodInterface selector								putSelector: setter)]].	anArrow		ifNotNil: [anArrow step].	^ aRow! !!CategoryViewer methodsFor: 'header pane' stamp: 'dgd 11/26/2003 15:04'!addNamePaneTo: header 	"Add the namePane, which may be a popup or a type-in 	depending on the type of CategoryViewer"	| aButton |	namePane := RectangleMorph newSticky color: Color brown veryMuchLighter.	namePane borderWidth: 0.	aButton := (StringButtonMorph				contents: '-----'				font: Preferences standardEToysFont)				color: Color black.	aButton target: self;		 arguments: Array new;		 actionSelector: #chooseCategory.	aButton actWhen: #buttonDown.	namePane addMorph: aButton.	aButton position: namePane position.	namePane align: namePane topLeft with: bounds topLeft + (50 @ 0).	namePane setBalloonText: 'category (click here to choose a different one)' translated.	header addMorphBack: namePane.	(namePane isKindOf: RectangleMorph)		ifTrue: [namePane addDropShadow.			namePane shadowColor: Color gray]! !!EToyGenericDialogMorph methodsFor: 'as yet unclassified' stamp: 'dgd 7/12/2003 12:33'!genericTextFieldNamed: aString 	| newField |	newField := ShowEmptyTextMorph new beAllFont: self myFont;				 extent: 400 @ 20;				 contentsWrapped: ''.	namedFields at: aString put: newField.	^ newField! !!EToyGenericDialogMorph methodsFor: 'as yet unclassified' stamp: 'dgd 7/12/2003 12:29'!myFont	^ Preferences standardEToysFont! !!EToyMorphsWelcomeMorph methodsFor: 'initialization' stamp: 'nk 7/12/2003 08:58'!initialize	"initialize the state of the receiver"	| earMorph |	super initialize.	""		self layoutInset: 8 @ 8.	"earMorph _ (EToyListenerMorph makeListeningToggle: true)  	asMorph."	earMorph _ TextMorph new contents: 'Morphswelcomehere';				 fontName: Preferences standardEToysFont familyName size: 18;				 centered;				 lock.	self addARow: {earMorph}.	self setBalloonText: 'My presence in this world means received morphs may appear automatically'! !!EtoyLoginMorph methodsFor: 'building' stamp: 'nk 7/12/2003 08:40'!myFont	^ Preferences standardEToysFont! !!IconicButton methodsFor: 'initialization' stamp: 'dgd 11/26/2003 15:26'!initializeWithThumbnail: aThumbnail withLabel: aLabel andSend: aSelector to: aReceiver 		"Initialize the receiver to show aThumbnail on its face, giving it the label supplied and arranging for it, when the button goes down on it, to obtain a new morph by sending the supplied selector to the supplied receiver"	| labeledItem  |	labeledItem _ AlignmentMorph newColumn beTransparent.	labeledItem borderWidth: 0.	labeledItem addMorph: aThumbnail.	labeledItem addMorphBack: (Morph new extent: (4@4)) beTransparent.	labeledItem addMorphBack: (BorderedStringMorph contents: aLabel font: (StrikeFont familyName: Preferences standardEToysFont familyName size: 15)).	self		beTransparent;		labelGraphic: (labeledItem imageForm asFormOfDepth: 16);		borderWidth: 0;		target: aReceiver;		actionSelector: #launchPartVia:label:;		arguments: {aSelector. aLabel};		actWhen: #buttonDown.	self stationarySetup.! !!KidNavigationMorph methodsFor: 'as yet unclassified' stamp: 'nk 7/12/2003 08:46'!fontForButtons	^Preferences standardEToysFont! !!ObjectsTool methodsFor: 'menu' stamp: 'nk 9/7/2003 07:42'!resetThumbnails	"Reset the thumbnail cache"	PartsBin clearThumbnailCache.	modeSymbol == #categories ifTrue: [self showCategories] ifFalse: [self showAlphabeticTabs]! !!PhonemeRecognizerMorph methodsFor: 'private' stamp: 'nk 7/12/2003 08:59'!addTitle	"Add a title."	| font title r |	font _ StrikeFont familyName: Preferences standardEToysFont familyName size: 20.	title _ StringMorph contents: 'Phoneme Recognizer' font: font.	r _ AlignmentMorph newColumn		color: color;		layoutInset: 0;		wrapCentering: #center; cellPositioning: #topCenter;		hResizing: #spaceFill;		vResizing: #rigid;		height: 20.	r addMorphBack: title.	self addMorphBack: r.	self addMorphBack: (Morph new extent: 5@8; color: Color transparent).  "spacer"! !!Preferences class methodsFor: 'fonts' stamp: 'dgd 7/12/2003 11:57'!chooseEToysFont	"present a menu with the possible fonts for the eToys"	self		chooseFontWithPrompt: 'eToys font'		andSendTo: self		withSelector: #setEToysFontTo:! !!Preferences class methodsFor: 'fonts' stamp: 'dgd 11/26/2003 15:16'!fontConfigurationMenu	| aMenu |	aMenu := MenuMorph new defaultTarget: Preferences.	aMenu addTitle: 'Standard System Fonts' translated.		aMenu add: 'default text font...' translated action: #chooseSystemFont.	aMenu balloonTextForLastItem: 'Choose the default font to be used for code and  in workspaces, transcripts, etc.' translated.	aMenu lastItem font: TextStyle defaultFont.		aMenu add: 'list font...' translated action: #chooseListFont.	aMenu lastItem font: Preferences standardListFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in list panes' translated.		aMenu add: 'flaps font...' translated action: #chooseFlapsFont.	aMenu lastItem font: Preferences standardFlapFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on textual flap tabs' translated.	aMenu add: 'eToys font...' translated action: #chooseEToysFont.	aMenu lastItem font: Preferences standardEToysFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on eToys environment' translated.	aMenu add: 'menu font...' translated action: #chooseMenuFont.	aMenu lastItem font: Preferences standardMenuFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in menus' translated.		aMenu add: 'window-title font...' translated action: #chooseWindowTitleFont.	aMenu lastItem font: Preferences windowTitleFont emphasis: 1.	aMenu balloonTextForLastItem: 'Choose the font to be used in window titles.' translated.	aMenu add: 'balloon-help font...' translated target: BalloonMorph action: #chooseBalloonFont.	aMenu lastItem font: BalloonMorph balloonFont.	aMenu balloonTextForLastItem: 'choose the font to be used when presenting balloon help.' translated.		aMenu add: 'code font...' translated action: #chooseCodeFont. 	aMenu lastItem font: Preferences standardCodeFont. 	aMenu balloonTextForLastItem: 'Choose the font to be used in code panes.' translated.		aMenu addLine.	aMenu add: 'restore default font choices' translated action: #restoreDefaultFonts.	aMenu balloonTextForLastItem: 'Use the standard system font defaults' translated.		^ aMenu! !!Preferences class methodsFor: 'fonts' stamp: 'nk 7/12/2003 07:59'!restoreDefaultFonts	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."	"Preferences restoreDefaultFonts"	| map response |	map _ IdentityDictionary new.	#(ComicBold ComicPlain NewYork) do: [ :originalName | | style |		style _ map at: originalName put: (TextStyle named: originalName).		style ifNil: [			response _ TextStyle modalStyleSelectorWithTitle: 'Choose replacement for text style ', originalName.			map at: originalName put: (response ifNil: [ TextStyle default ]).		].	].	#(	(setButtonFontTo:		ComicBold		16)		"(setCodeFontTo:			NewYork		12)"  "Later"		(setFlapsFontTo:			ComicBold		16)		(setEToysFontTo:			ComicBold		16)		(setListFontTo:			NewYork		12)		(setMenuFontTo:			NewYork		12)		(setWindowTitleFontTo:	NewYork		15)		(setSystemFontTo:		NewYork		12)) do:			[:triplet |				self perform: triplet first with: ((map at: triplet second) fontOfSize: triplet third)].	Smalltalk at: #BalloonMorph ifPresent:		[:thatClass | thatClass setBalloonFontTo: ((map at: #ComicPlain) fontOfSize: 12)].	"Note:  The standardCodeFont is not currently used -- the default font is instead; later hopefully we can split the code font out as  a separate choice, but only after we're able to have the protocols reorganized such that we can know whether it's code or not when we launch the text object.	Note:  The standard button font is reset by this code but is not otherwise settable by a public UI (too many things can go afoul) "! !!Preferences class methodsFor: 'fonts' stamp: 'dgd 7/12/2003 11:52'!setEToysFontTo: aFont 	"change the font used in eToys environment"	Parameters at: #eToysFont put: aFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 7/12/2003 08:50'!standardEToysFont	"Answer the font to be used in the eToys environment"	^ Parameters		at: #eToysFont		ifAbsent: [Parameters at: #eToysFont put: self standardButtonFont]! !!ProgressMorph methodsFor: 'private' stamp: 'nk 7/12/2003 08:59'!fontOfPointSize: size	^ (TextConstants at: Preferences standardEToysFont familyName ifAbsent: [TextStyle default]) fontOfPointSize: size! !!ScriptEditorMorph methodsFor: 'dropping/grabbing' stamp: 'dgd 11/26/2003 15:08'!buttonRowForEditor	"Answer a row of buttons that comprise the header at the top of the Scriptor"	| aRow aString buttonFont aStatusMorph aButton aColumn aTile |	buttonFont _ Preferences standardButtonFont.	aRow _ AlignmentMorph newRow color: Color transparent; layoutInset: 0.	aRow hResizing: #shrinkWrap.	aRow vResizing: #shrinkWrap.	self hasParameter ifFalse:		[aRow addMorphFront:			(SimpleButtonMorph new				label: '!!' font: Preferences standardEToysFont;				target: self;				color: Color yellow;				borderWidth: 0;				actWhen: #whilePressed;				actionSelector: #tryMe;				balloonTextSelector: #tryMe).		aRow addTransparentSpacerOfSize: 6@10].	self addDismissButtonTo: aRow.	aRow addTransparentSpacerOfSize: 6@1.	aColumn _ AlignmentMorph newColumn beTransparent.	aColumn addTransparentSpacerOfSize: 0@4.	aButton _ UpdatingThreePhaseButtonMorph checkBox.	aButton		target: self;		actionSelector: #toggleWhetherShowingTiles;		getSelector: #showingMethodPane.	aButton setBalloonText: 'toggle between showing tiles and showing textual code'.	aColumn addMorphBack: aButton.	aRow addMorphBack: aColumn.	aRow addTransparentSpacerOfSize: 6@10.	aString _ playerScripted externalName.	aRow addMorphBack:		(aButton _ SimpleButtonMorph new useSquareCorners label: aString font: buttonFont; target: self; setNameTo: 'title').	aButton actWhen: #buttonDown; actionSelector: #offerScriptorMenu.	aButton		on: #mouseEnter send: #menuButtonMouseEnter: to: aButton;		on: #mouseLeave send: #menuButtonMouseLeave: to: aButton.	aButton borderColor: (Color fromRgbTriplet: #(0.065 0.258 1.0)).	aButton color: ScriptingSystem uniformTileInteriorColor.	aButton balloonTextSelector: #offerScriptorMenu.	aRow addTransparentSpacerOfSize: 4@1.	aButton _ (Preferences universalTiles ifTrue: [SyntaxUpdatingStringMorph] 					ifFalse: [UpdatingStringMorph]) new.	aButton useStringFormat;		target:  self;		getSelector: #scriptTitle;		setNameTo: 'script name';		font: ScriptingSystem fontForNameEditingInScriptor;		putSelector: #setScriptNameTo:;		setProperty: #okToTextEdit toValue: true;		step.	aRow addMorphBack: aButton.	aButton setBalloonText: 'Click here to edit the name of the script.'.	aRow addTransparentSpacerOfSize: 6@0.	self hasParameter		ifTrue:			[aTile _ TypeListTile new choices: Vocabulary typeChoices dataType: nil.			aTile addArrows.			aTile setLiteral: #Number.			aTile on: #mouseDown send: #handUserParameterTile to: self.	"(aButton _ SimpleButtonMorph new useSquareCorners label: 'parameter' font: buttonFont; target: self; setNameTo: 'parameter').			aButton actWhen: #buttonDown; actionSelector: #handUserParameterTile."			aRow addMorphBack: aTile.			aTile borderColor: Color red.			aTile color: ScriptingSystem uniformTileInteriorColor.			aTile setBalloonText: 'Drag from here to get a parameter tile']		ifFalse:			[aRow addMorphBack: (aStatusMorph _ self scriptInstantiation statusControlMorph)].	aRow addTransparentSpacerOfSize: 6@1.	aRow addMorphBack:		(IconicButton new borderWidth: 0;			labelGraphic: (ScriptingSystem formAtKey: 'AddTest'); color: Color transparent; 			actWhen: #buttonDown;			target: self;			actionSelector: #addYesNoToHand;			shedSelvedge;			balloonTextSelector: #addYesNoToHand).	aRow addTransparentSpacerOfSize: 12@10.	self addDestroyButtonTo: aRow.	(playerScripted existingScriptInstantiationForSelector: scriptName)		ifNotNilDo:			[:inst | inst updateStatusMorph: aStatusMorph].	^ aRow! !!ScriptStatusControl methodsFor: 'initialization' stamp: 'dgd 11/26/2003 15:02'!initializeFor: aScriptInstantiation	"Answer a control that will serve to reflect and allow the user to change the status of the receiver"	|  statusReadout |	self hResizing: #shrinkWrap.	self cellInset: 2@0.	scriptInstantiation _ aScriptInstantiation.	tickPauseButtonsShowing _ false.	self addMorphBack: (statusReadout _ UpdatingSimpleButtonMorph new).	statusReadout label: aScriptInstantiation status asString font: Preferences standardButtonFont.	statusReadout setNameTo: 'trigger'.	statusReadout target: aScriptInstantiation; wordingSelector: #translatedStatus; actionSelector: #presentScriptStatusPopUp.	statusReadout setBalloonText: 'when this script should run' translated.	statusReadout actWhen: #buttonDown.	self assurePauseTickControlsShow.	aScriptInstantiation updateStatusMorph: self! !!StackMorph class methodsFor: 'authoring prototype' stamp: 'nk 7/12/2003 08:59'!designationsExplainer	"Answer a morph that contains designation explanation"	| aMorph aSwatch aTextMorph |	aMorph _ AlignmentMorph newColumn color: Color black; layoutInset: 1.	#((green		'Shared items onBackground.Exact same itemshared by every card')	(orange'Data items onBackgroundEach card has itsown data')	(red'Instance-specificitemsuniqueto this card')) do:	[:aPair |		aSwatch _ AlignmentMorph new extent: 132 @80; color: (Color perform: aPair first); lock.		aSwatch hResizing: #rigid; vResizing: #rigid; layoutInset: 0.		aSwatch borderColor: Color black.		aTextMorph _ TextMorph new string: aPair second fontName: Preferences standardEToysFont familyName size: 18.		aTextMorph width: 130.		aTextMorph centered.		aSwatch addMorphBack: aTextMorph.		aMorph addMorphBack: aSwatch].	aMorph hResizing: #shrinkWrap; vResizing: #shrinkWrap.	^ aMorph	"StackMorph designationsExplainer openInHand"! !!StandardScriptingSystem methodsFor: 'font & color choices' stamp: 'dgd 7/12/2003 12:06'!fontForNameEditingInScriptor	^ Preferences standardEToysFont! !!StandardScriptingSystem methodsFor: 'font & color choices' stamp: 'dgd 7/12/2003 12:05'!fontForTiles	^ Preferences standardEToysFont! !!StandardScriptingSystem methodsFor: 'font & color choices' stamp: 'nk 7/12/2003 08:59'!smallBoldFont	"Answer a small bold font for use in some standard scripting-support structures"	^ StrikeFont familyName: Preferences standardEToysFont familyName size: 12! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'nk 7/12/2003 08:59'!holderWithAlphabet	"Answer a fully instantiated Holder that has submorphs that represent the letters of the uppercase alphabet, with each one having an 'index' slot which bears the letter's index in the alphabet -- 1 for A, 2 for B, etc.   A few special characters are provided as per ack request 10/00; for these the index provided is rather arbitrarily assigned"	| aMorph aPlayer newMorph oneCharString aContainer aWrapper |	"ScriptingSystem holderWithAlphabet openInHand"	aContainer _ self prototypicalHolder useRoundedCorners.	aContainer borderColor: Color blue lighter.	aWrapper _ AlignmentMorph new hResizing: #shrinkWrap; vResizing: #shrinkWrap; layoutInset: 0.	aWrapper addMorphBack: (aMorph _ TextMorph new contents: 'A').	aMorph beAllFont: ((TextStyle named: Preferences standardEToysFont familyName) fontOfSize: 24).	aMorph width: 14; lock.	aWrapper beTransparent; setNameTo: 'A'.	aPlayer _ aWrapper assuredPlayer.	aPlayer addInstanceVariableNamed: #index type: #Number value: 1.	aContainer addMorphBack: aWrapper.	2 to: 26 do:		[:anIndex |			newMorph _ aWrapper usableSiblingInstance.			newMorph player perform: #setIndex: with: anIndex.			newMorph firstSubmorph contents: (oneCharString _ ($A asciiValue + anIndex - 1) asCharacter asString).			newMorph setNameTo: oneCharString.			aContainer addMorphBack: newMorph].	#(' ' '.' '#') with: #(27 28 29) do:		[:aString :anIndex |			newMorph _ aWrapper usableSiblingInstance.			newMorph player perform: #setIndex: with: anIndex.			newMorph firstSubmorph contents: aString.			aString = ' '				ifTrue:					[newMorph setNameTo: 'space'.					newMorph color: (Color gray alpha: 0.2)]				ifFalse:					[newMorph setNameTo: aString].			aContainer addMorphBack: newMorph].	aContainer setNameTo: 'alphabet'.	aContainer isPartsBin: true.	aContainer enableDrop: false.	aContainer indicateCursor: false; width: 162.	aContainer color: (Color r: 0.839 g: 1.0 b: 1.0).  "Color fromUser"	^ aContainer! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'dgd 7/12/2003 12:05'!tryButtonFor: aPhraseTileMorph 	| aButton |	aButton := SimpleButtonMorph new.	aButton target: aPhraseTileMorph;		 actionSelector: #try;				label: '!!'		font: Preferences standardEToysFont;		 color: Color yellow;		 borderWidth: 0.	aButton actWhen: #whilePressed.	aButton balloonTextSelector: #try.	^ aButton! !!TextMorph methodsFor: '*connectors-accessing' stamp: 'nk 7/12/2003 08:39'!fontName: fontName size: fontSize	| newTextStyle |	newTextStyle _ ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy.	textStyle _ newTextStyle.	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfSize: fontSize)).	paragraph ifNotNil: [paragraph textStyle: newTextStyle]! !!TextMorph class methodsFor: 'parts bin' stamp: 'nk 7/12/2003 08:59'!fancyPrototype	| t |	t _ self authoringPrototype.	t autoFit: false; extent: 150@75.	t borderWidth: 2; margins: 4@0; useRoundedCorners.	"Why not rounded?"	"fancy font, shadow, rounded"	t fontName: Preferences standardEToysFont familyName size: 18; textColor: Color blue; backgroundColor: Color lightBrown.	t addDropShadow."Strangeness here in order to avoid two offset copies of the default contents when operating in an mvc project before cursor enters the morphic window"	t paragraph.	^ t! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 7/12/2003 07:54'!modalMVCStyleSelectorWithTitle: title	"MVC Only!! Presents a modal font-style choice menu, answers a TextStyle or nil."	"TextStyle modalMVCStyleSelectorWithTitle: 'testing'"		| aMenu |	aMenu _ CustomMenu new.	self actualTextStyles keysAndValuesDo: [ :styleName :style |		aMenu add: styleName action: style	].	^aMenu startUpWithCaption: title.! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 7/12/2003 08:03'!modalStyleSelectorWithTitle: title	"Presents a modal font-style choice menu, answers a TextStyle or nil."	"TextStyle modalStyleSelectorWithTitle: 'testing'"		| menu |	Smalltalk isMorphic ifFalse: [ ^self modalMVCStyleSelectorWithTitle: title ].	menu _ MenuMorph entitled: title.	self actualTextStyles keysAndValuesDo: [ :styleName :style |		menu add: styleName target: menu selector: #modalSelection: argument: style.		menu lastItem font: (style fontOfSize: 18)	].	^menu invokeModal.! !!TranslucentProgessMorph methodsFor: 'drawing' stamp: 'nk 7/12/2003 08:59'!drawOn: aCanvas	| revealPercentage revealingStyle revealingColor revealingBounds revealToggle x baseColor revealTimes secondsRemaining stringToDraw where fontToUse innerBounds |		innerBounds _ bounds.	opaqueBackgroundColor ifNotNil: [		aCanvas 			frameAndFillRectangle: bounds			fillColor: opaqueBackgroundColor			borderWidth: 8			borderColor: Color blue.		innerBounds _ innerBounds insetBy: 8.	].	revealTimes _ (self valueOfProperty: #revealTimes) ifNil: [^self].	revealPercentage _ (revealTimes first / revealTimes second) asFloat.	revealingStyle _ self revealingStyle.	x _ self valueOfProperty: #progressStageNumber ifAbsent: [1].	baseColor _ Color perform: (#(red blue green magenta cyan yellow) atPin: x).	revealingColor _ baseColor alpha: 0.2.	revealingStyle = 3 ifTrue: [	"wrap and change color"		revealPercentage > 1.0 ifTrue: [			revealingColor _ baseColor alpha: (0.2 + (revealingStyle / 10) min: 0.5).		].		revealPercentage _ revealPercentage fractionPart.	].	revealingStyle = 2 ifTrue: [	"peg at 75 and blink"		revealPercentage > 0.75 ifTrue: [			revealToggle _ self valueOfProperty: #revealToggle ifAbsent: [true].			self setProperty: #revealToggle toValue: revealToggle not.			revealToggle ifTrue: [revealingColor _ baseColor alpha: 0.8.].		].		revealPercentage _ revealPercentage min: 0.75.	].	revealingBounds _ innerBounds withLeft: innerBounds left + (innerBounds width * revealPercentage) truncated.	aCanvas 		fillRectangle: revealingBounds		color: revealingColor.	secondsRemaining _ (revealTimes second - revealTimes first / 1000) rounded.	secondsRemaining > 0 ifTrue: [		fontToUse _ StrikeFont familyName: Preferences standardEToysFont familyName size: 24.		stringToDraw _ secondsRemaining printString.		where _ innerBounds corner - ((fontToUse widthOfString: stringToDraw) @ fontToUse height).		aCanvas 			drawString: stringToDraw 			in: (where corner: innerBounds corner)			font: fontToUse			color: Color black.		aCanvas			drawString: stringToDraw 			in: (where - (1@1) corner: innerBounds corner)			font: fontToUse			color: Color white.	]. ! !EToyProjectRenamerMorph removeSelector: #myFont!"Postscript From ObjectsThumbnailFix-nkRe-build the icons."| oldDepth |(Display supportsDisplayDepth: 32) ifFalse: [ ^self inform: 'This system does not support 32 bit display depth.' ].oldDepth _ Display depth.Display newDepth: 32.PartsBin clearThumbnailCache.Flaps replaceToolsFlap.ObjectsTool allSubInstancesDo: [ :ea | ea tweakAppearanceAfterModeShift ].Display newDepth: oldDepth.!