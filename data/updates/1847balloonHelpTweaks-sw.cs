'From Squeak2.8alpha of 13 January 2000 [latest update: #1844] on 7 February 2000 at 12:15:45 pm'!"Change Set:		balloonHelpTweaks-swDate:			9 February 2000Author:			Scott WallaceA couple of tweaks in conjunction with Stephan Aust's balloonHelpPatch:*  Allow the balloon-help font to be changed via the Appearance menu.*  If the user cancels out of the set-balloon-help dialog, former contents will no longer be clobbered.*  Adds another edgeToAdhereTo type, #adjustedCenter, which can be overridden in any class to produce a specially-skewed center for the computation of adherence by a submorph.*  Use the above edgeToAdhereTo capability to get the text within the balloon to be properly centered within the *bubble* rather than within the outer bounds of the balloonMorph.*  Added a way to get a help balloon pointed at a morph other than the actual morph that provided its text.   Used this new facility to get the help-balloons for Preferences buttons to point at the actual button, even if the mouseover that triggered the balloon happens over the corresponding preference name.*  Fixed a bug in BalloonMorph.getBestLocation:for:corner: that made the balloon-placement quest drop into a debugger in some circumstances in morphic worlds opened in mvc windows.*  Fixed another bug in that same method that had made it not properly respect the special circumstances of an embed morphic world within an mvc project, resulting in faulty placement.  This improvement is most immediately appreciated by looking at the balloon help for a new Preferences window launched in an mvc project."!!Morph methodsFor: 'menus' stamp: 'sw 2/7/2000 10:57'!adhereToEdge: edgeSymbol	(owner == nil or: [owner isHandMorph]) ifTrue: [^ self].	self perform: (edgeSymbol, ':') asSymbol withArguments: (Array with: (owner perform: edgeSymbol))! !!Morph methodsFor: 'menus' stamp: 'sw 2/3/2000 00:14'!adjustedCenter	"Provides a hook for objects to provide a reference point other than the receiver's center,for the purpose of centering a submorph under special circumstances, such as BalloonMorph"	^ self center! !!Morph methodsFor: 'menus' stamp: 'sw 2/3/2000 00:12'!adjustedCenter: c	"Set the receiver's position based on the #adjustedCenter protocol for adhereToEdge.  By default this simply sets the receiver's center.   Though there are (at its inception anyway) no other implementors of this method, it is required in use with the #adhereToEdge when the centering of a submorph is to be with reference to a rectangle  other than the receiver's center."	self center: c! !!Morph methodsFor: 'halos and balloon help' stamp: 'sw 2/7/2000 11:27'!balloonHelpAligner	"Answer the morph to which the receiver's balloon help should point"	^ (self valueOfProperty: #balloonTarget) ifNil: [self]! !!Morph methodsFor: 'halos and balloon help' stamp: 'sw 1/31/2000 11:12'!editBalloonHelpContent: aString	| reply |	reply _ FillInTheBlank		multiLineRequest: 'Edit the balloon help text for ' , self externalName		centerAt: Sensor cursorPoint		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])		answerHeight: 200.	reply ifNil: [^ self].  "User cancelled out of the dialog"	(reply isEmpty or: [reply asString = self noHelpString])		ifTrue: [self setBalloonText: nil]		ifFalse: [self setBalloonText: reply]! !!Morph methodsFor: 'halos and balloon help' stamp: 'sw 2/7/2000 11:28'!showBalloon: msgString	"Pop up a balloon containing the given string,	first removing any existing BalloonMorphs in the world."	| w balloon worldBounds |	(w _ self world) ifNil: [^ self].	w submorphsDo: [:m | (m isKindOf: BalloonMorph) ifTrue: [m delete]].	balloon _ BalloonMorph string: msgString for: self balloonHelpAligner.	balloon lock.	w addMorphFront: balloon.	"So that if the translation below makes it overlap the receiver, it won't	interfere with the rootMorphsAt: logic and hence cause flashing.  Without	this, flashing happens, believe me!!"	((worldBounds _ w bounds) containsRect: balloon bounds) ifFalse:		[balloon bounds: (balloon bounds translatedToBeWithin: worldBounds)].	self setProperty: #balloon toValue: balloon! !!BalloonMorph methodsFor: 'initialization' stamp: 'sw 2/3/2000 00:16'!initialize	super initialize.	color _ Color paleYellow.	borderColor _ Color black.	borderWidth _ 1.	offsetFromTarget _ 0@0! !!BalloonMorph methodsFor: 'private' stamp: 'sw 2/7/2000 01:49'!adjustedCenter	"This horizontal adjustment is needed because we want the interior TextMorph to be centered within the visual balloon rather than simply within the BalloonMorph's bounding box.  Without this, balloon-help text would be a bit off-center"	^ self center + (offsetFromTarget x sign * (5 @ 0))! !!BalloonMorph class methodsFor: 'private' stamp: 'sw 2/7/2000 12:10'!getBestLocation: vertices for: morph corner: cornerName	"Try four rel locations of the balloon for greatest unclipped area.   12/99 sma"	| rect maxArea verts rectCorner morphPoint mbc a mp dir bestVerts result usableArea |	rect _ vertices first rect: (vertices at: 5).	maxArea _ -1.	verts _ vertices.	usableArea _ morph world viewBox.	1 to: 4 do: [:i |		dir _ #(vertical horizontal) atWrap: i.		verts _ verts collect: [:p | p flipBy: dir centerAt: rect center].		rectCorner _ #(bottomLeft bottomRight topRight topLeft) at: i.		morphPoint _ #(topCenter topCenter bottomCenter bottomCenter) at: i.		a _ ((rect			align: (rect perform: rectCorner)			with: (mbc _ morph boundsInWorld perform: morphPoint))				intersect: usableArea) area.		(a > maxArea or: [a = rect area and: [rectCorner = cornerName]]) ifTrue:			[maxArea _ a.			bestVerts _ verts.			mp _ mbc]].	result _ bestVerts collect: [:p | p + (mp - bestVerts first)] "Inlined align:with:".	^ result! !!BalloonMorph class methodsFor: 'private' stamp: 'sw 2/2/2000 22:13'!getTextMorph: aStringOrMorph	"Construct text morph."	| m text |	aStringOrMorph isMorph		ifTrue: [m _ aStringOrMorph]		ifFalse:			[BalloonFont				ifNil: [text _ aStringOrMorph]				ifNotNil: [text _ Text					string: aStringOrMorph					attribute: (TextFontReference toFont: BalloonFont)].			m _ (TextMorph new contents: text) centered].	m setToAdhereToEdge: #adjustedCenter.	^ m! !!BalloonMorph class methodsFor: 'utility' stamp: 'sw 1/31/2000 15:43'!balloonFont	^ BalloonFont! !!BalloonMorph class methodsFor: 'utility' stamp: 'sw 1/31/2000 15:39'!chooseBalloonFont	"BalloonMorph chooseBalloonFont"	Preferences chooseFontWithPrompt:  'Select the font to beused for balloon help'		andSendTo: self withSelector: #setBalloonFontTo:! !!BalloonMorph class methodsFor: 'utility' stamp: 'sw 1/31/2000 15:40'!setBalloonFontTo: aFont	aFont ifNotNil: [BalloonFont _ aFont]! !!Preferences class methodsFor: 'pref buttons' stamp: 'sw 2/7/2000 11:30'!buttonRepresenting: prefSymbol wording: aString color: aColor	"self currentHand attachMorph: (Preferences buttonRepresenting: #balloonHelpEnabled wording: 'Balloon Help' color: nil)"	"Return a button that controls the setting of prefSymbol.  It will keep up to date even if the preference value is changed in a different place"	| outerButton aButton str aHelp miniWrapper |	(FlagDictionary includesKey: prefSymbol) ifFalse: [self error: 'Unknown preference: ', prefSymbol printString].	outerButton _ AlignmentMorph newRow height: 24.	outerButton color:  (aColor ifNil: [Color r: 0.645 g: 1.0 b: 1.0]).	outerButton hResizing: #spaceFill; vResizing: #shrinkWrap.	outerButton addMorph: (aButton _ UpdatingThreePhaseButtonMorph checkBox).	aButton		target: self;		actionSelector: #togglePreference:;		arguments: (Array with: prefSymbol);		target: Preferences;		getSelector: prefSymbol.	outerButton addTransparentSpacerOfSize: (2 @ 0).	str _ StringMorph contents: aString font: (StrikeFont familyName: 'NewYork' size: 12).	miniWrapper _ AlignmentMorph newRow hResizing: #shrinkWrap; vResizing: #shrinkWrap.	miniWrapper beTransparent addMorphBack: str lock.	outerButton addMorphBack: miniWrapper.	aButton setBalloonText: (aHelp _ Preferences helpMessageForPreference: prefSymbol).	miniWrapper setBalloonText: aHelp; setProperty: #balloonTarget toValue: aButton.	^ outerButton! !!Preferences class methodsFor: 'fonts' stamp: 'sw 1/31/2000 15:45'!fontConfigurationMenu	| aMenu |	aMenu _ MenuMorph new defaultTarget: Preferences.	aMenu addTitle: 'Standard System Fonts'.	aMenu add: 'default text font...' action: #chooseSystemFont.	aMenu balloonTextForLastItem: 'Choose the default font to be used for code and  in workspaces, transcripts, etc.'.	aMenu lastItem font: TextStyle defaultFont.	aMenu add: 'list font...' action: #chooseListFont.	aMenu lastItem font: Preferences standardListFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in list panes'.	aMenu add: 'flaps font...' action: #chooseFlapsFont.	aMenu lastItem font: Preferences standardFlapFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on textual flap tabs'.	aMenu add: 'menu font...' action: #chooseMenuFont.	aMenu lastItem font: Preferences standardMenuFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in menus'.	aMenu add: 'window-title font...' action: #chooseWindowTitleFont.	aMenu lastItem font: Preferences windowTitleFont emphasis: 1.	aMenu balloonTextForLastItem: 'Choose the font to be used in window titles.'.	aMenu add: 'balloon-help font...' target: BalloonMorph action: #chooseBalloonFont.	aMenu lastItem font: BalloonMorph balloonFont.	aMenu balloonTextForLastItem: 'choose the font to be used when presenting balloon help.'.	"aMenu add: 'code font...' action: #chooseCodeFont.	aMenu lastItem font: Preferences standardCodeFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in code panes.'."	aMenu addLine.	aMenu add: 'restore default font choices' action: #restoreDefaultFonts.	aMenu balloonTextForLastItem: 'Use the standard system font defaults'.	^ aMenu! !!Preferences class methodsFor: 'fonts' stamp: 'sw 1/31/2000 15:54'!restoreDefaultFonts	"Preferences restoreDefaultFonts"	#(	(setButtonFontTo:		ComicBold		16)		"(setCodeFontTo:			NewYork		12)"  "Later"		(setFlapsFontTo:			ComicBold		16)		(setListFontTo:			NewYork		12)		(setMenuFontTo:			NewYork		12)		(setWindowTitleFontTo:	NewYork		15)		(setSystemFontTo:		NewYork		12)) do:			[:triplet |				self perform: triplet first with: (StrikeFont familyName: triplet second size: triplet third)].	Smalltalk at: #BalloonMorph ifPresent:		[:thatClass | thatClass setBalloonFontTo: (StrikeFont familyName: #ComicPlain size: 12)].	"Note:  The standardCodeFont is not currently used -- the default font is instead; later hopefully we can split the code font out as  a separate choice, but only after we're able to have the protocols reorganized such that we can know whether it's code or not when we launch the text object.	Note:  The standard button font is reset by this code but is not otherwise settable by a public UI (too many things can go afoul) "! !