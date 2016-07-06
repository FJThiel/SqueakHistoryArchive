'From Squeak2.8alpha of 13 January 2000 [latest update: #2067] on 6 May 2000 at 3:23:52 am'!"Change Set:		prefs-swDate:			6 May 2000Author:			Scott WallaceChanges related to 'Preferences'.�	Minimum size now enforced for prefs panel.�	Prefs panel no longer accepts drops gratuituously.�	Adds new preference #okToReinitializeFlaps - an update or fileout which feels a need to reinitialize flaps in the system can first query the setting of this preference, and only do the reinitialization if it finds the preference set to true.  If it finds it false, it can suppress the urge to reinitialize.  Users who have manually-constructed flaps that they are loath to lose can set this preference to false.� 	Adds new preference #isFlagship -- implemented as a 'hard-coded' preference,  Any image that wishes to mark itself as a 'flagship' needs to change this hard-coded method such that it returns false; in future updates, this preference may be queried to decide whether or not to carry out some not-so-backward-compatible action.�	Includes Stefan Rudolph's method designed to be called for example in the postscript of a changeset, allowing you to add a new preference, declare a category for it, give it a default value, and supply a help message, all in the same do-it."!!Preferences class methodsFor: 'parameters' stamp: 'sw 3/28/2000 13:38'!parameterAt: aKey default: defaultValueBlock	"Return the Parameter setting at the given key.  If there is no entry for this key in the Parameters dictionary, create one with the value of defaultValueBlock as its value"	^ Parameters at: aKey ifAbsent: [Parameters at: aKey put: defaultValueBlock value]! !!Preferences class methodsFor: 'hard-coded prefs' stamp: 'sw 4/25/2000 14:07'!isFlagship	"Manually change this to return true if you wish your system to be marked as a 'flagship'.  The intent here is to allow an update to query this flag before undertaking some radical do-it that might clobber important content is such an image."	^ false! !!Preferences class methodsFor: 'window colors' stamp: 'sw 3/28/2000 13:39'!darkerWindows	"Preferences darkerWindows"	| windowColorDict |	windowColorDict _ self parameterAt: #windowColors default: [IdentityDictionary new].	windowColorDict associationsDo:		[:assoc | windowColorDict at: assoc key put: assoc value darker]! !!Preferences class methodsFor: 'factored pref panel' stamp: 'sw 5/6/2000 03:22'!openFactoredPanelWithWidth: aWidth	"Preferences openFactoredPanelWithWidth: 325"	| tabbedPalette controlPage window playfield aColor aFont  maxEntriesPerCategory tabsMorph anExtent |	aFont _ StrikeFont familyName: 'NewYork' size: 19.	aColor _ Color r: 0.645 g: 1.0 b: 1.0.	tabbedPalette _ TabbedPalette newSticky.	(tabsMorph _ tabbedPalette tabsMorph) color: aColor darker; highlightColor: Color red regularColor: Color brown darker darker.	maxEntriesPerCategory _ 0.	"tabbedPalette addTabFor: self helpPaneForFactoredPanel font: aFont.  LATER!!"	self factoredCategories do:		[:aCat |			controlPage _ AlignmentMorph newColumn beSticky color: aColor.			controlPage borderColor: aColor; inset: 4.			aCat second do:				[:aPrefSymbol |					controlPage addMorphBack:						(Preferences buttonRepresenting: aPrefSymbol wording: aPrefSymbol color: nil)].			controlPage setNameTo: aCat first asString.			aCat first == #halos ifTrue:				[self addHaloControlsTo: controlPage].			tabbedPalette addTabFor: controlPage font: aFont.			maxEntriesPerCategory _ maxEntriesPerCategory max: aCat second size].	tabbedPalette selectTabNamed: 'general'.	tabsMorph rowsNoWiderThan: aWidth.	playfield _ Morph newSticky.	anExtent _ aWidth @ (25 + tabsMorph height + (20 * maxEntriesPerCategory)).	playfield extent: anExtent.	playfield color: aColor.	playfield addMorphBack: tabbedPalette.	Smalltalk isMorphic		ifTrue:			[window _ (SystemWindow labelled: 'Preferences') model: nil.			window bounds: ((100@100- ((0@window labelHeight) + window borderWidth))								extent: (playfield extent + (2 * window borderWidth))).			window addMorph: playfield frame: (0@0 extent: 1@1).			window updatePaneColors.			window setProperty: #minimumExtent toValue: (anExtent + (2@2)).			window position: 200 @ 20.			self currentHand attachMorph: window.			self currentWorld startSteppingSubmorphsOf: window]		ifFalse:			[(window _ MVCWiWPasteUpMorph newWorldForProject: nil) addMorph: playfield.			window startSteppingSubmorphsOf: playfield.			MorphWorldView openOn: window  label: 'Preferences' extent: window fullBounds extent]! !!Preferences class methodsFor: 'add preferences' stamp: 'sr 4/19/200016:54'!addPreference: prefSymbol category: categorySymbol default: defaultFlagballoonHelp: helpString 	self class compileProgrammatically: (#initialValuesAddition ,categorySymbol , prefSymbol) asString , '	^ #((' , prefSymbol asSymbol, ' ' , defaultFlag printString , ' (' ,categorySymbol asSymbol, ' ) ) )' classified: 'initial values'.	self class compileProgrammatically: (#helpMsgsAddition , categorySymbol, prefSymbol) asString , '	^ #((' , prefSymbol, ' ', helpString printString, ' ) )' classified:'help'.	self absorbAdditions! !"Postscript:"Preferences addPreference: #okToReinitializeFlaps category: #morphic default: true balloonHelp: 'if true, then then code in updates will feel free to reinitialize the global flaps; if false, flaps will never be reinitialized by updates -- thus, set it to false if you have a serious investment in the content of the global flaps in your configuration, strong enough that you don''t want your flaps modernized when advances would otherwise indicate a need to'.!