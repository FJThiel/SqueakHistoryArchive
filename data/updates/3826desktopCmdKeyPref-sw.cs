'From Squeak3.1alpha of 28 February 2001 [latest update: #3835] on 14 March 2001 at 9:54:34 am'!"Change Set:		desktopCmdKeyPref-swDate:			13 March 2001Author:			Scott WallaceAdds a preference which allows you disable the use of desktop command-key shortcuts, such as cmd-t for bringing up a transcript.  The name of the new preference is honorDesktopCmdKeys.Also, fixes a bug that could bring up a spurious warning regarding universal tiles whenever a new preference was added."!!PasteUpMorph methodsFor: 'world menu' stamp: 'sw 3/10/2001 23:01'!keystrokeInWorld: evt	"A keystroke was hit when no keyboard focus was in set, so it is sent here to the world instead.  This current implementation is regrettably hard-coded; until someone cleans this up, you may be tempted to edit this method to suit your personal taste in interpreting cmd-keys issued to the desktop."	|  aChar isCmd |	aChar _ evt keyCharacter.	isCmd _ evt commandKeyPressed and: [Preferences cmdKeysInText].	(isCmd and: [Preferences honorDesktopCmdKeys]) ifTrue:		[(aChar == $z) ifTrue: [^ self commandHistory undoOrRedoCommand].		(aChar == $w) ifTrue: [^ SystemWindow closeTopWindow].		(aChar == $\) ifTrue: [^ SystemWindow sendTopWindowToBack].		(aChar == $t) ifTrue: [^ self findATranscript: evt].		(aChar == $b) ifTrue: [^ Browser openBrowser].		(aChar == $k) ifTrue: [^ Workspace open].		(aChar == $m) ifTrue: [^ TheWorldMenu new adaptToWorld: World; newMorph].		(aChar == $C) ifTrue: [^ self findAChangeSorter: evt].		(aChar == $R) ifTrue: [^ self openRecentSubmissionsBrowser: evt].		(aChar == $P) ifTrue: [^ self findAPreferencesPanel: evt].		(aChar == $r) ifTrue: [^ Display restoreMorphicDisplay].		(aChar == $W) ifTrue: [^ self invokeWorldMenu: evt]]			"This last item is a weirdo feature requested by the Open School in Fall of 2000 as a keyhole to the world menu in systems that normally do not offer a world menu"! !!Preferences class methodsFor: 'initialization' stamp: 'sw 3/13/2001 20:54'!initializeAddedPreferences	"Initialize any preference not yet known to the prefs dictionary as per descriptions in the 'initial values' category, but don't change the setting of any existing preference.	Also, compile accessor methods for retrieving any preference that lacks one"	"Preferences initializeAddedPreferences"	|  selectors sym |	self flagsHeldByProjects keysDo:  "Place any preference held by projects which is NOT in the global dict into it now"		[:pref | 			(FlagDictionary includesKey: pref) ifFalse:				[FlagDictionary at: pref put: (Project current projectPreferenceAt: pref ifAbsent: [false])]].	selectors _ self class selectors.  "(this can take some time)"	self allPreferenceInitializationSpecs do:		[:triplet |			(selectors includes: (sym _ triplet first))				ifFalse:					[self compileAccessMethodFor: sym].			(FlagDictionary includesKey: sym) ifFalse:				[triplet second == #true					ifTrue:						[self enable: sym]					ifFalse:						[self disable: sym]]].	self resetCategoryInfo! !"Postscript:"Preferences addPreference: #honorDesktopCmdKeys categories: #(general menus)  default: true balloonHelp: 'if true, then various command keys shortcuts, such as cmd-t for bringing up a Transcript, are enabled for the desktop (i.e., when there is no other keyboard focus); when false, such command keys are ignored'.!