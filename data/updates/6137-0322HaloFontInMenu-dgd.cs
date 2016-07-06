'From Squeakland 3.8.5976 of 19 August 2004 [latest update: #266] on 25 August 2004 at 5:18:05 pm'!"Change Set:		HaloFontInMenu-dgdDate:			25 August 2004Author:			Diego Gomez DeckPut the new font preference in 'system fonts...' menu"!!Preferences class methodsFor: 'fonts' stamp: 'dgd 8/25/2004 17:14'!chooseHaloLabelFont	"present a menu with the possible fonts for label in halo"	self		chooseFontWithPrompt: 'halo label font'		andSendTo: self		withSelector: #setHaloLabelFontTo:		highlight: self standardHaloLabelFont! !!Preferences class methodsFor: 'fonts' stamp: 'dgd 8/25/2004 17:12'!fontConfigurationMenu	| aMenu |	aMenu := MenuMorph new defaultTarget: Preferences.	aMenu addTitle: 'Standard System Fonts' translated.		aMenu addStayUpIcons.		aMenu add: 'default text font...' translated action: #chooseSystemFont.	aMenu balloonTextForLastItem: 'Choose the default font to be used for code and  in workspaces, transcripts, etc.' translated.	aMenu lastItem font: Preferences standardDefaultTextFont.		aMenu add: 'list font...' translated action: #chooseListFont.	aMenu lastItem font: Preferences standardListFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in list panes' translated.		aMenu add: 'flaps font...' translated action: #chooseFlapsFont.	aMenu lastItem font: Preferences standardFlapFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on textual flap tabs' translated.	aMenu add: 'eToys font...' translated action: #chooseEToysFont.	aMenu lastItem font: Preferences standardEToysFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on eToys environment' translated.	aMenu add: 'halo label font...' translated action: #chooseHaloLabelFont.	aMenu lastItem font: Preferences standardHaloLabelFont.	aMenu balloonTextForLastItem: 'Choose the font to be used on labels ih halo' translated.	aMenu add: 'menu font...' translated action: #chooseMenuFont.	aMenu lastItem font: Preferences standardMenuFont.	aMenu balloonTextForLastItem: 'Choose the font to be used in menus' translated.		aMenu add: 'window-title font...' translated action: #chooseWindowTitleFont.	aMenu lastItem font: Preferences windowTitleFont emphasis: 1.	aMenu balloonTextForLastItem: 'Choose the font to be used in window titles.' translated.	aMenu add: 'balloon-help font...' translated action: #chooseBalloonHelpFont.	aMenu lastItem font: Preferences standardBalloonHelpFont.	aMenu balloonTextForLastItem: 'choose the font to be used when presenting balloon help.' translated.		aMenu add: 'code font...' translated action: #chooseCodeFont. 	aMenu lastItem font: Preferences standardCodeFont. 	aMenu balloonTextForLastItem: 'Choose the font to be used in code panes.' translated.		aMenu addLine.	aMenu add: 'restore default font choices' translated action: #restoreDefaultFonts.	aMenu balloonTextForLastItem: 'Use the standard system font defaults' translated.		aMenu add: 'print default font choices' translated action: #printStandardSystemFonts.	aMenu balloonTextForLastItem: 'Print the standard system font defaults to the Transcript' translated.	^ aMenu! !