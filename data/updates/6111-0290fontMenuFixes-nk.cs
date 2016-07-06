'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #286] on 1 September 2004 at 2:56:03 pm'!"Change Set:		FontMenuFixes-nkDate:			1 September 2004Author:			Ned KonzSome fixes and improvements for the various font menus:* alphabetize the names of text styles in menus* allow the selection of derivative fonts from menus if any are available (bold, italic, etc.)* fixes derivativeFonts to not return nils* changes the world menu/appearance/system fonts/print system fonts (which wrote to the Transcript) to display a text window instead* adds emphasis names (bold, etc.) to that display* allows Preferences>>setDefaultFonts: to be given font names ending in emphasis names (Bold, etc.). This allows for a quick setting of the defaults* removes new default font names (DefaultMultiStyle, etc.) from some displays of font names where the other default style names were already being suppressed* changes restoreDefaultFonts to use the fonts found in the 286 image (which probably should be changed)."!!AbstractFont class methodsFor: 'as yet unclassified' stamp: 'nk 9/1/2004 11:41'!emphasisStringFor: emphasisCode	"Answer a translated string that represents the attributes given in emphasisCode."	| emphases bit |	emphasisCode = 0 ifTrue: [ ^'Normal' translated ].	emphases := (IdentityDictionary new)		at: 1 put: 'Bold' translated;		at: 2 put: 'Italic' translated;		at: 4 put: 'Underlined' translated;		at: 8 put: 'Narrow' translated;		at: 16 put: 'StruckOut' translated;		yourself.	bit := 1.	^String streamContents: [ :s |		[ bit < 32 ] whileTrue: [ | code |			code := emphasisCode bitAnd: bit.			code isZero ifFalse: [ s nextPutAll: (emphases at: code); space ].			bit := bit bitShift: 1 ].		s position isZero ifFalse: [ s skip: -1 ].	]! !!ArchiveViewer methodsFor: 'initialization' stamp: 'nk 9/1/2004 11:09'!createButtonBar	| bar button narrowFont registeredFonts |	registeredFonts _ OrderedCollection new.	TextStyle knownTextStylesWithoutDefault do:		[:st | (TextStyle named: st) fonts do: [:f | registeredFonts addLast: f]].			narrowFont := registeredFonts detectMin:			[:ea | ea widthOfString: 'Contents' from: 1 to: 8].	bar := AlignmentMorph newRow.	bar		color: self backgroundColor;		rubberBandCells: false;		vResizing: #shrinkWrap;		cellInset: 6 @ 0.	#(#('New\Archive' #canCreateNewArchive #createNewArchive 'Create a new, empty archive and discard this one') #('Load\Archive' #canOpenNewArchive #openNewArchive 'Open another archive and discard this one') #('Save\Archive As' #canSaveArchive #saveArchive 'Save this archive under a new name') #('Extract\All' #canExtractAll #extractAll 'Extract all this archive''s members into a directory') #('Add\File' #canAddMember #addMember 'Add a file to this archive') #('Add from\Clipboard' #canAddMember #addMemberFromClipboard 'Add the contents of the clipboard as a new file') #('Add\Directory' #canAddMember #addDirectory 'Add the entire contents of a directory, with all of its subdirectories') #('Extract\Member As' #canExtractMember #extractMember 'Extract the selected member to a file') #('Delete\Member' #canDeleteMember #deleteMember 'Remove the selected member from this archive') #('Rename\Member' #canRenameMember #renameMember 'Rename the selected member') #('View All\Contents' #canViewAllContents #changeViewAllContents 'Toggle the view of all the selected member''s contents')) 		do: 			[:arr | 			| buttonLabel |			buttonLabel := (TextMorph new)						string: arr first withCRs							fontName: narrowFont familyName							size: narrowFont pointSize							wrap: false;						hResizing: #shrinkWrap;						lock;						yourself.			(button := PluggableButtonMorph 						on: self						getState: arr second						action: arr third)				vResizing: #shrinkWrap;				hResizing: #spaceFill;				onColor: self buttonOnColor offColor: self buttonOffColor;				label: buttonLabel;				setBalloonText: arr fourth.			bar addMorphBack: button.			buttonLabel composeToBounds].	^bar! !!BalloonMorph class methodsFor: 'utility' stamp: 'nk 9/1/2004 10:47'!chooseBalloonFont	"BalloonMorph chooseBalloonFont"	Preferences chooseFontWithPrompt:  'Select the font to beused for balloon help' translated		andSendTo: self withSelector: #setBalloonFontTo: highlight: BalloonFont! !!ListParagraph class methodsFor: 'initialization' stamp: 'nk 9/1/2004 10:27'!initialize 	"ListParagraph initialize"	| aFont |	"Allow different line spacing for lists"	aFont _ Preferences standardListFont.	ListStyle _ TextStyle fontArray: { aFont }.	ListStyle gridForFont: 1 withLead: 1! !!PluggableListMorph methodsFor: 'menus' stamp: 'nk 9/1/2004 10:48'!setListFont	"set the font for the list"	Preferences chooseFontWithPrompt: 'Choose the font for this list' translated andSendTo: self withSelector: #font: highlight: self listMorph font! !!PopUpMenu class methodsFor: 'class initialization' stamp: 'nk 9/1/2004 10:27'!setMenuFontTo: aFont	"Set the menu font as indicated"	MenuStyle _ TextStyle fontArray: { aFont }.	MenuStyle 		gridForFont: 1 withLead: 0;		centered.	self allSubInstancesDo: [:m | m rescan]! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:48'!chooseCodeFont	"Not currently sent, but once protocols are sorted out so that we can disriminate on whether a text object being launched is for code or not, will be reincorporated"	self chooseFontWithPrompt: 'Choose the font to be used for displaying code' translated andSendTo: self withSelector: #setCodeFontTo: highlight: self standardCodeFont.! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:48'!chooseEToysFont	"present a menu with the possible fonts for the eToys"	self		chooseFontWithPrompt: 'Choose the eToys font' translated		andSendTo: self		withSelector: #setEToysFontTo:		highlight: self standardEToysFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:48'!chooseFlapsFont	self chooseFontWithPrompt: 'Choose a flaps font' translated andSendTo: self withSelector: #setFlapsFontTo: highlight: self standardFlapFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:48'!chooseListFont	self chooseFontWithPrompt: 'Choose the standard list font' translated andSendTo: self withSelector: #setListFontTo: highlight: self standardListFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:49'!chooseMenuFont	self chooseFontWithPrompt: 'Choose the standard menu font' translated andSendTo: self withSelector: #setMenuFontTo: highlight: self standardMenuFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:49'!chooseSystemFont	self chooseFontWithPrompt: 'Choose the default text font' translated andSendTo: self withSelector: #setSystemFontTo: highlight: (TextConstants at: #DefaultTextStyle) defaultFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:49'!chooseWindowTitleFont	self chooseFontWithPrompt: 'Choose the window title font' translated andSendTo: self withSelector: #setWindowTitleFontTo: highlight: self windowTitleFont! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 11:11'!fontConfigurationMenu	| aMenu |	aMenu := MenuMorph new defaultTarget: Preferences.	aMenu addTitle: 'Standard System Fonts' translated.		aMenu addStayUpIcons.		aMenu add: 'default text font...' translated action: #chooseSystemFont.	aMenu balloonTextForLastItem: 'Choose the default font to be used for code and  in workspaces, transcripts, etc.' translated.	aMenu lastItem font: Preferences standardDefaultTextFont.		aMenu add: 'list font...' translated action: #chooseListFont.	aMenu lastItem font: Preferences standardListFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in list panes' translated.		aMenu add: 'flaps font...' translated action: #chooseFlapsFont.	aMenu lastItem font: Preferences standardFlapFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on textual flap tabs' translated.	aMenu add: 'eToys font...' translated action: #chooseEToysFont.	aMenu lastItem font: Preferences standardEToysFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on eToys environment' translated.	aMenu add: 'menu font...' translated action: #chooseMenuFont.	aMenu lastItem font: Preferences standardMenuFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in menus' translated.		aMenu add: 'window-title font...' translated action: #chooseWindowTitleFont.	aMenu lastItem font: Preferences windowTitleFont emphasis: 1.	aMenu balloonTextForLastItem: 'Choose the font to be used in window titles.' translated.	aMenu add: 'balloon-help font...' translated action: #chooseBalloonHelpFont.	aMenu lastItem font: Preferences standardBalloonHelpFont.	aMenu balloonTextForLastItem: 'choose the font to be used when presenting balloon help.' translated.		aMenu add: 'code font...' translated action: #chooseCodeFont. 	aMenu lastItem font: Preferences standardCodeFont. 	aMenu balloonTextForLastItem: 'Choose the font to be used in code panes.' translated.		aMenu addLine.	aMenu add: 'restore default font choices' translated action: #restoreDefaultFonts.	aMenu balloonTextForLastItem: 'Use the standard system font defaults' translated.		aMenu add: 'display default font choices' translated action: #printStandardSystemFonts.	aMenu balloonTextForLastItem: 'Show the standard system font choices in a window' translated.	^ aMenu! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 11:37'!printStandardSystemFonts	"self printStandardSystemFonts"	| string |	string := String streamContents: [ :s |	#(standardDefaultTextFont standardListFont standardFlapFont 	standardEToysFont standardMenuFont windowTitleFont 	standardBalloonHelpFont standardCodeFont standardButtonFont) do: [:selector |		| font |		font _ Preferences perform: selector.		s			nextPutAll: selector; space;			nextPutAll: font familyName; space;			nextPutAll: (AbstractFont emphasisStringFor: font emphasis);			nextPutAll: ' points: ';			print: font pointSize;			nextPutAll: ' height: ';			print: font height;			cr		]].	(StringHolder new)		contents: string;		openLabel: 'Current system font settings' translated.! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:13'!restoreDefaultFonts	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."	"	Preferences restoreDefaultFonts	"	self setDefaultFonts: #(		(setSystemFontTo:		Accuny				10)		(setListFontTo:			Accuny				10)		(setFlapsFontTo:			Accushi				12)		(setEToysFontTo:			BitstreamVeraSansMono				9)		(setMenuFontTo:			Accuny				10)		(setWindowTitleFontTo:	BitstreamVeraSans	12)		(setBalloonHelpFontTo:	Accujen				9)		(setCodeFontTo:			Accuny				10)		(setButtonFontTo:		BitstreamVeraSansMono				9)	)! !!Preferences class methodsFor: 'fonts' stamp: 'nk 9/1/2004 10:19'!setDefaultFonts: defaultFontsSpec	"Since this is called from menus, we can take the opportunity to prompt for missing font styles."	| fontNames map emphases |	fontNames _ defaultFontsSpec collect: [:array | array second].	map _ IdentityDictionary new.	emphases _ IdentityDictionary new.	fontNames do: [:originalName | | decoded style response |		decoded := TextStyle decodeStyleName: originalName.		style _ map at: originalName put: (TextStyle named: decoded second).		emphases at: originalName put: decoded first.		style ifNil: [			response _ TextStyle modalStyleSelectorWithTitle: 'Choose replacement for text style ', originalName.			map at: originalName put: (response ifNil: [TextStyle default])]].	defaultFontsSpec do: [:triplet | self		perform: triplet first		with: (((map at: triplet second) fontOfPointSize: triplet third) emphasis: (emphases at: triplet second))]! !!StandardScriptingSystem class methodsFor: 'utilities' stamp: 'nk 9/1/2004 10:53'!applyNewEToyLook	"Apply the new EToy look based on free fonts, approximating the classic look as closely as possible."	"StandardScriptingSystem applyNewEToyLook""	| aTextStyle aFont | 	aTextStyle _ TextStyle named: #BitstreamVeraSansMono.	aFont _ aTextStyle fontOfSize: 12.	aFont _ aFont emphasis: 1.	Preferences setEToysFontTo: aFont.	Preferences setButtonFontTo: aFont.	aTextStyle _ TextStyle named: #Accushi.	aFont _ aTextStyle fontOfSize: 12.	Preferences setFlapsFontTo: aFont.	(aTextStyle _ TextStyle named: #Accuny)		ifNotNil:			[Preferences setSystemFontTo: (aTextStyle fontOfSize: 12)]"	Preferences setDefaultFonts: #(		(setEToysFontTo:			BitstreamVeraSansBold	10)		(setButtonFontTo:		BitstreamVeraSansMono	9)		(setFlapsFontTo:			Accushi				12)		(setSystemFontTo:		Accuny				10)		(setWindowTitleFontTo:	BitstreamVeraSansBold	12)	)! !!StandardSystemView class methodsFor: 'class initialization' stamp: 'nk 9/1/2004 10:26'!setLabelStyle	| aFont |	"StandardSystemView setLabelStyle"	aFont _ Preferences windowTitleFont.	LabelStyle _ TextStyle fontArray: { aFont }.	LabelStyle gridForFont: 1 withLead: 0! !!StrikeFont class methodsFor: 'accessing' stamp: 'nk 9/1/2004 11:00'!actualFamilyNames	"Answer a sorted list of actual family names, without the Default aliases"	^(self familyNames copyWithoutAll: TextStyle defaultFamilyNames) asOrderedCollection! !!StrikeFontSet methodsFor: 'as yet unclassified' stamp: 'nk 9/1/2004 12:06'!derivativeFonts	^derivativeFonts copyWithout: nil! !!TTCFont methodsFor: 'friend' stamp: 'nk 9/1/2004 13:01'!derivativeFonts	derivatives ifNil: [^ #()].	^derivatives copyWithout: nil! !!TextStyle class methodsFor: 'TextConstants access' stamp: 'nk 9/1/2004 11:00'!actualTextStyles	| aDict |	"TextStyle actualTextStyles"	"Answer dictionary whose keys are the names of styles in the system and whose values are the actual styles"	aDict _ TextConstants select: [:thang | thang isKindOf: self ].	self defaultFamilyNames do: [ :sym | aDict removeKey: sym ].	^ aDict! !!TextStyle class methodsFor: 'TextConstants access' stamp: 'nk 9/1/2004 10:59'!defaultFamilyNames	^#(DefaultTextStyle DefaultFixedTextStyle DefaultMultiStyle)! !!TextStyle class methodsFor: 'TextConstants access' stamp: 'nk 9/1/2004 11:08'!knownTextStylesWithoutDefault	"Answer the names of the known text styles, sorted in alphabetical order without default"	"TextStyle knownTextStylesWithoutDefault"	| result |	result := self knownTextStyles asOrderedCollection.	^ result copyWithoutAll: self defaultFamilyNames! !!TextStyle class methodsFor: 'mime file in/out' stamp: 'nk 9/1/2004 11:03'!replaceStyle: oldStyle with: newStyle	"	TextStyle replaceStyle: (TextStyle named: #AccunyOLD) with: (TextStyle named: #Accuny)	"	"Try to find corresponding fonts in newStyle and substitute the fonts in oldStyle for them."	| oldKeys |	oldKeys _ Set new.	TextConstants keysAndValuesDo: [ :k :v | v = oldStyle ifTrue: [ oldKeys add: k ]].	oldKeys removeAllFoundIn: self defaultFamilyNames.	self replaceFontsIn: oldStyle fontArray with: newStyle.	oldStyle becomeForward: newStyle.	oldKeys do: [ :k | TextConstants removeKey: k ].! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 13:18'!emphasisMenuForFont: font target: target selector: selector highlight: currentEmphasis	"Offer a font emphasis menu for the given style. If one is selected, pass that font to target with a call to selector. The fonts will be displayed in that font.	Answer nil if no derivatives exist.	" 	| aMenu derivs |	derivs := font derivativeFonts.	derivs isEmpty ifTrue: [ ^nil ].	aMenu _ MenuMorph entitled: 'emphasis' translated.	derivs := derivs asOrderedCollection.	derivs addFirst: font.	derivs do: [ :df | 			aMenu 				add: (AbstractFont emphasisStringFor: df emphasis)				target: target 				selector: selector				argument: df.                aMenu lastItem font: df.                df emphasis == currentEmphasis ifTrue: [aMenu lastItem color: Color blue darker]].        ^ aMenu! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 13:31'!fontMenuForStyle: styleName target: target selector: selector highlight: currentFont 	"Offer a font menu for the given style. If one is selected, pass 	that font to target with a  	call to selector. The fonts will be displayed in that font."	| aMenu |	aMenu := MenuMorph entitled: styleName.	(TextStyle named: styleName)		ifNotNilDo: [:s | s isTTCStyle				ifTrue: [aMenu						add: 'New Size'						target: self						selector: #chooseTTCFontSize:						argument: {styleName. target. selector}]].	(self pointSizesFor: styleName)		do: [:pointSize | 			| font subMenu | 			font := (self named: styleName)						fontOfPointSize: pointSize.			subMenu := self						emphasisMenuForFont: font						target: target						selector: selector						highlight: (currentFont								ifNotNilDo: [:cf | (cf familyName = styleName											and: [cf pointSize = font pointSize])										ifTrue: [currentFont emphasis]]).			subMenu				ifNil: [aMenu						add: pointSize asString , ' Point'						target: target						selector: selector						argument: font]				ifNotNil: [aMenu add: pointSize asString , ' Point' subMenu: subMenu].			aMenu lastItem font: font.			currentFont				ifNotNilDo: [:cf | (cf familyName = styleName							and: [cf pointSize = font pointSize])						ifTrue: [aMenu lastItem color: Color blue darker]]].	^ aMenu! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 10:38'!modalMVCStyleSelectorWithTitle: title	"MVC Only!! Presents a modal font-style choice menu, answers a TextStyle or nil."	"TextStyle modalMVCStyleSelectorWithTitle: 'testing'"		| aMenu actualStyles |	aMenu _ CustomMenu new.	actualStyles := self actualTextStyles.	actualStyles keysSortedSafely do: [ :styleName | | style |		style := actualStyles at: styleName.		aMenu add: styleName action: style	].	^aMenu startUpWithCaption: title.! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 10:34'!modalStyleSelectorWithTitle: title	"Presents a modal font-style choice menu, answers a TextStyle or nil."	"TextStyle modalStyleSelectorWithTitle: 'testing'"		| menu actualStyles |	Smalltalk isMorphic ifFalse: [ ^self modalMVCStyleSelectorWithTitle: title ].	menu _ MenuMorph entitled: title.	actualStyles := self actualTextStyles.	actualStyles keysSortedSafely do: [ :styleName | | style |		style := actualStyles at: styleName.		menu add: styleName target: menu selector: #modalSelection: argument: style.		menu lastItem font: (style fontOfSize: 18)	].	^menu invokeModal.! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 10:37'!mvcPromptForFont: aPrompt andSendTo: aTarget withSelector: aSelector	"MVC Only!! prompt for a font and if one is provided, send it to aTarget using a message with selector aSelector."	| aMenu aChoice aStyle namesAndSizes aFont |	"TextStyle mvcPromptForFont: 'Choose system font style' andSendTo: TextStyle withSelector: #setSystemFontTo:"	aMenu _ CustomMenu new.	self actualTextStyles keysSortedSafely do:		[:styleName |			aMenu add: styleName action: styleName].	aChoice _ aMenu startUpWithCaption: aPrompt.	aChoice ifNil: [^ self].	aMenu _ CustomMenu new.	aStyle _ self named: aChoice.	(namesAndSizes _ aStyle fontNamesWithPointSizes) do:		[:aString | aMenu add: aString action: aString].	aChoice _ aMenu startUpWithCaption: nil.	aChoice ifNil: [^ self].	aFont _ aStyle fontAt: (namesAndSizes indexOf: aChoice).	aTarget perform: aSelector with: aFont! !!TextStyle class methodsFor: 'user interface' stamp: 'nk 9/1/2004 13:19'!promptForFont: aPrompt andSendTo: aTarget withSelector: aSelector highlight: currentFont 	"Morphic Only!! prompt for a font and if one is provided, send it to aTarget using a 	message with selector aSelector."	"TextStyle promptForFont: 'Choose system font:' andSendTo: Preferences withSelector: 	#setSystemFontTo: "	"Derived from a method written by Robin Gibson"	| menu subMenu currentTextStyle |	currentTextStyle := currentFont				ifNotNil: [currentFont textStyleName].	menu := MenuMorph entitled: aPrompt.	self actualTextStyles keysSortedSafely		do: [:styleName | 			subMenu := self						fontMenuForStyle: styleName						target: aTarget						selector: aSelector						highlight: currentFont.			menu add: styleName subMenu: subMenu.			menu lastItem				font: ((self named: styleName)						fontOfSize: 18).			styleName = currentTextStyle				ifTrue: [menu lastItem color: Color blue darker]].	menu popUpInWorld: self currentWorld! !StrikeFont class removeSelector: #defaultFamilyNames!ListParagraph initialize!