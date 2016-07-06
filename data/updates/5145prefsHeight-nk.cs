'From Squeak3.4beta of ''1 December 2002'' [latest update: #5138] on 3 December 2002 at 4:02:26 pm'!"Change Set:		PrefsHeight-nkDate:			3 December 2002Author:			Ned KonzMakes sure that all the buttons on the front pane of the Preferences panel are visible.Also adds a little space between a button group."!!Preferences class methodsFor: 'preferences panel' stamp: 'nk 12/3/2002 15:55'!initializePreferencePanel: aPanel in: aPasteUpMorph	"Initialize the given Preferences panel. in the given pasteup, which is the top-level panel installed in the container window.  Also used to reset it after some change requires reformulation"	| tabbedPalette controlPage aColor aFont maxEntriesPerCategory tabsMorph anExtent  prefObjects cc |	aPasteUpMorph removeAllMorphs.	aFont _ StrikeFont familyName: 'NewYork' size: 19.	aColor _ aPanel defaultBackgroundColor.	tabbedPalette _ TabbedPalette newSticky.	tabbedPalette dropEnabled: false.	(tabsMorph _ tabbedPalette tabsMorph) color: aColor darker;		 highlightColor: Color red regularColor: Color brown darker darker.	tabbedPalette on: #mouseDown send: #yourself to: #().	maxEntriesPerCategory _ 0.	self listOfCategories do: 		[:aCat | 			controlPage _ AlignmentMorph newColumn beSticky color: aColor.			controlPage on: #mouseDown send: #yourself to: #().			controlPage dropEnabled: false.			Preferences alternativeWindowLook ifTrue:				[cc _ Color transparent.				controlPage color: cc].			controlPage borderColor: aColor;				 layoutInset: 4.			(prefObjects _ self preferenceObjectsInCategory: aCat) do:				[:aPreference | controlPage						addMorphBack: (aPreference representativeButtonWithColor: cc inPanel: aPanel)].			controlPage setNameTo: aCat asString.			aCat = #?				ifTrue:	[aPanel addHelpItemsTo: controlPage].			aCat = #halos				ifTrue: [aPanel addHaloControlsTo: controlPage].			tabbedPalette addTabFor: controlPage font: aFont.			aCat = 'search results' ifTrue:				[(tabbedPalette tabNamed: aCat) setBalloonText:					'Use the ? category to find preferences by keyword; the results of your search will show up here'].		maxEntriesPerCategory _ maxEntriesPerCategory max: prefObjects size].	tabbedPalette selectTabNamed: '?'.	tabsMorph rowsNoWiderThan: aPasteUpMorph width.	aPasteUpMorph on: #mouseDown send: #yourself to: #().	anExtent _ aPasteUpMorph width @ (490 max: (25 + tabsMorph height + (20 * maxEntriesPerCategory))).	aPasteUpMorph extent: anExtent.	aPasteUpMorph color: aColor.	aPasteUpMorph 	 addMorphBack: tabbedPalette.! !!PreferencesPanel methodsFor: 'find' stamp: 'nk 12/3/2002 16:01'!addHelpItemsTo: panelPage	"Add the items appropriate the the ? page of the receiver"	| aButton aTextMorph aMorph firstTextMorph |	panelPage hResizing: #shrinkWrap; vResizing: #shrinkWrap.	firstTextMorph _  TextMorph new contents: 'Search Preferences for:'.	firstTextMorph beAllFont: ((TextStyle default fontOfSize: 13) emphasized: 1).	panelPage addMorphBack: firstTextMorph lock.	panelPage addTransparentSpacerOfSize: 0@10.	aMorph _ RectangleMorph new clipSubmorphs: true; beTransparent; borderWidth: 2; borderColor: Color black; extent: 250 @ 36.	aMorph vResizing: #rigid; hResizing: #rigid.	aTextMorph _  PluggableTextMorph new				on: self				text: #searchString				accept: #setSearchStringTo:				readSelection: nil				menu: nil."	aTextMorph hResizing: #rigid."	aTextMorph borderWidth: 0.	aTextMorph font: ((TextStyle default fontOfSize: 21) emphasized: 1); setTextColor: Color red.	aMorph addMorphBack: aTextMorph.	aTextMorph acceptOnCR: true.	aTextMorph position: (aTextMorph position + (6@5)).	aMorph clipLayoutCells: true.	aTextMorph extent: 240 @ 25.	panelPage addMorphBack: aMorph.	aTextMorph setBalloonText: 'Type what you want to search for here, then hit the "Search" button, or else hit RETURN or ENTER'.	aTextMorph setTextMorphToSelectAllOnMouseEnter.	aTextMorph hideScrollBarIndefinitely.	panelPage addTransparentSpacerOfSize: 0@10.	aButton _ SimpleButtonMorph new target: self; color: Color transparent; actionSelector: #initiateSearch:; arguments: {aTextMorph}; label: 'Search'.	panelPage addMorphBack: aButton.	aButton setBalloonText: 'Type what you want to search for in the box above, then click here (or hit RETURN or ENTER) to start the search; results will appear in the "search results" category.'.	panelPage addTransparentSpacerOfSize: 0@30.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Restore all Default Preference Settings'; target: Preferences; actionSelector: #chooseInitialSettings; setBalloonText: 'Click here to reset all the preferences to their standard default values.'; yourself).	panelPage addTransparentSpacerOfSize: 0@14.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Save Current Settings as my Personal Preferences'; 		target: Preferences; actionSelector: #savePersonalPreferences; setBalloonText: 'Click here to save the current constellation of Preferences settings as your personal defaults; you can get them all reinstalled with a single gesture by clicking the "Restore my Personal Preferences".'; yourself).	panelPage addTransparentSpacerOfSize: 0@14.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Restore my Personal Preferences'; target: Preferences; actionSelector: #restorePersonalPreferences; setBalloonText: 'Click here to reset all the preferences to their values in your Personal Preferences.'; yourself).	panelPage addTransparentSpacerOfSize: 0@30.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Save Current Settings to Disk'; 		target: Preferences; actionSelector: #storePreferencesToDisk; setBalloonText: 'Click here to save the current constellation of Preferences settings to a file; you can get them all reinstalled with a single gesture by clicking "Restore Settings From Disk".'; yourself).	panelPage addTransparentSpacerOfSize: 0@14.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Restore Settings from Disk'; target: Preferences; actionSelector: #restorePreferencesFromDisk; setBalloonText: 'Click here to load all the preferences from their saved values on disk.'; yourself).	panelPage addTransparentSpacerOfSize: 0@30.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Inspect Parameters'; target: Preferences; actionSelector: #inspectParameters; setBalloonText: 'Click here to view all the values stored in the system Parameters dictionary'; yourself).	panelPage addTransparentSpacerOfSize: 0@10.	panelPage addMorphBack: (Preferences themeChoiceButtonOfColor: Color transparent font: TextStyle defaultFont).	panelPage addTransparentSpacerOfSize: 0@10.	panelPage addMorphBack: (SimpleButtonMorph new color: Color transparent; label: 'Help!!'; target: Preferences; actionSelector: #giveHelpWithPreferences; setBalloonText: 'Click here to get some hints on use of this Preferences Panel'; yourself).	panelPage wrapCentering: #center.! !