'From Squeak3.1alpha of 5 February 2001 [latest update: #3517] on 6 February 2001 at 4:52:14 am'!"Change Set:		defaultFlaps-swDate:			6 February 2001Author:			Scott WallaceSet up default flaps for the 3.0 release.Get balloon help updated for items in tools flaps even after a tool change."!!PasteUpMorph methodsFor: 'options' stamp: 'sw 2/5/2001 16:59'!replaceTallSubmorphsByThumbnails	"Any submorphs that seem to tall get replaced by thumbnails; their balloon text is copied over to the thumbnail"	|  itsThumbnail heightForThumbnails maxHeightToAvoidThumbnailing maxWidthForThumbnails existingHelp |	heightForThumbnails _ self heightForThumbnails.	maxHeightToAvoidThumbnailing _ self maxHeightToAvoidThumbnailing.	maxWidthForThumbnails _ self maximumThumbnailWidth.	self submorphs do:		[:aMorph |			itsThumbnail _ aMorph representativeNoTallerThan: maxHeightToAvoidThumbnailing norWiderThan: maxWidthForThumbnails thumbnailHeight: heightForThumbnails.			(aMorph == itsThumbnail)				ifFalse:					[existingHelp _ aMorph balloonText.					self replaceSubmorph: aMorph by: itsThumbnail.					existingHelp ifNotNil:						[itsThumbnail setBalloonText: existingHelp]]]! !!TheWorldMenu methodsFor: 'windows & flaps menu' stamp: 'sw 2/5/2001 13:03'!windowsMenu        "Build the windows menu for the world."        ^ self fillIn: (self menu: 'windows & flaps...') from: {                  { 'find window' . { #myWorld . #findWindow: }. 'Presents a list of all windows; if you choose one from the list, it becomes the active window.'}.                { 'find changed browsers...' . { #myWorld . #findDirtyBrowsers: }. 'Presents a list of browsers that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.                { 'find changed windows...' . { #myWorld . #findDirtyWindows: }. 'Presents a list of all windows that have unsubmitted changes; if you choose one from the list, it becomes the active window.'}.			nil.                { 'find a transcript (t)' . { #myWorld . #findATranscript: }. 'Brings an open Transcript to the front, creating one if necessary, and makes it the active window'}.               { 'find a change sorter (C)' . { #myWorld . #findAChangeSorter: }. 'Brings an open change sorter to the front, creating one if necessary, and makes it the active window'}.			 nil.                { #staggerPolicyString . { self . #toggleWindowPolicy }. 'stagger: new windows positioned so you can see a portion of each one.                tile: new windows positioned so that they do not overlap others, if possible.'}.                nil.                { 'collapse all windows' . { #myWorld . #collapseAll }. 'Reduce all open windows to collapsed forms that only show titles.'}.                { 'expand all windows' . { #myWorld . #expandAll }. 'Expand all collapsed windows back to their expanded forms.'}.                { 'close top window (w)' . { SystemWindow . #closeTopWindow }. 'Close the topmost window if possible.'}.                { 'send top window to back (\)' . { SystemWindow . #sendTopWindowToBack  }. 'Make the topmost window become the backmost one, and activate the window just beneath it.'}.			 { 'move windows onscreen' . { #myWorld . #bringWindowsFullOnscreen }. 'Make all windows fully visible on the screen'}.                nil.                { 'delete unchanged windows' . { #myWorld . #closeUnchangedWindows }. 'Deletes all windows that do not have unsaved text edits.'}.                { 'delete non-windows' . { #myWorld . #deleteNonWindows }. 'Deletes all non-window morphs lying on the world.'}.                { 'delete both of the above' . { self . #cleanUpWorld }. 'deletes all unchanged windows and also all non-window morphs lying on the world, other than flaps.'}.                nil.            "    { #suppressFlapsString . { self . #toggleFlapSuppressionInProject }. 'Governs whether flaps should be shown in this project'}."                { #useGlobalFlapsString . { self. #toggleWhetherToUseGlobalFlaps }. 'Governs whether a universal set of "global" flaps should be sharable by all morphic projects.'}.			{ #whichGlobalFlapsString . { Utilities. #offerGlobalFlapsMenu }. 'Put up a menu that allows you to choose which global flaps to show in this project'. #globalFlapsEnabled}.                { #newGlobalFlapString  . { Utilities . #addGlobalFlap }. 'Create a new flap that will be shared by all morphic projects'.  #globalFlapsEnabled}.                nil.                { 'new project flap...'  . { Utilities . #addLocalFlap }. 'Create a new flap to be used only in this project.'}.                { 'add stack-tools flap'  . { Utilities . #addStackToolsFlap }. 'Add a flap in this project that offers tools for creating stacks and cards.'}.                { 'add menu flap'  . { Utilities . #addMenuFlap }. 'Add a flap in this project that shows most of the standard world menus all at once.'}.				nil.                { 'about flaps...' . { Utilities . #explainFlaps }. 'Gives a window full of details about how to use flaps.'}.        }! !!Utilities class methodsFor: 'flaps'!addMenuFlap	"Add a flap with system menus aggregated on it.  This will be a local flap, though the user can later make it a global one if she prefers."	| aFlap aFlapTab aHolder verticalHolder aMenu |	aFlap _ PasteUpMorph newSticky color: Color transparent; extent: self currentWorld width @ 264; borderWidth: 0; padding: 0.	aFlapTab _ FlapTab new referent: aFlap.	aFlapTab color: Color brown lighter.	aFlapTab assumeString: 'Menus' font: Preferences standardFlapFont orientation: #horizontal color: Color blue muchLighter.	aFlapTab setToPopOutOnMouseOver: false.	aFlapTab setToPopOutOnDragOver: false.	aFlapTab edgeToAdhereTo: #top; inboard: false.	aFlapTab position: ((Display width - aFlapTab width) // 2) @ 0.	aFlap beFlap: true.	aFlap color: (Color blue muchLighter alpha: 0.6).	aFlap extent: self currentWorld width @ 267.	aHolder _ AlignmentMorph newRow beSticky beTransparent hResizing: #shrinkWrap; vResizing: #shrinkWrap; cellPositioning: #topLeft.	#(openMenu helpMenu windowsMenu (changesMenu debugMenu ) (playfieldMenu scriptingMenu )) do:		[:elem |			(elem isKindOf: Array)				ifTrue:					[verticalHolder _ AlignmentMorph newColumn beSticky beTransparent.					verticalHolder hResizing: #shrinkWrap; layoutInset: 0; wrapCentering: #center; cellPositioning: #topCenter.					elem do:						[:aMenuSymbol |							verticalHolder addMorphBack: ((aMenu _ self currentWorld getWorldMenu: aMenuSymbol) beSticky; stayUp: true).							aMenu beSticky.							aMenu borderWidth: 1.							aMenu submorphs second delete].					aHolder addMorphBack: verticalHolder]				ifFalse:					[aHolder addMorphBack: ((aMenu _ self currentWorld getWorldMenu: elem) beSticky; stayUp: true).					aMenu submorphs second delete.					aMenu beSticky.					aMenu borderWidth: 1]].	aFlap addMorphBack: aHolder.	self currentWorld addMorphFront: aFlapTab.  	"a local flap, but we could as easily make it global by:		self addGlobalFlap: aFlapTab.  self currentWorld addGlobalFlaps"! !!Utilities class methodsFor: 'flaps'!currentMenuFlap	"answer a menu flap if there currently is at least one."	^ self currentWorld flapTabs detect: [:aTab | 		(aTab submorphs size > 0) and:  [(aTab submorphs first isKindOf: TextMorph) and: 			[(aTab submorphs first contents string copyWithout: $ ) = 'Menus']]] ifNone: [nil]! !!Utilities class methodsFor: 'flaps' stamp: 'sw 2/5/2001 13:07'!initializeStandardFlaps	"Initialize the standard default out-of-box set of global flaps. This method creates them and places them in my class variable #FlapTabs, but does not itself get them displayed."	"Utilities initializeStandardFlaps"	FlapTabs _ OrderedCollection new.	FlapTabs add: (self standardLeftFlap setToPopOutOnDragOver: false).	FlapTabs add: self standardBottomFlap.	FlapTabs add: self standardRightFlap.	FlapTabs do:		[:aFlapTab | 			aFlapTab setToPopOutOnMouseOver: false].	^ FlapTabs! !!Utilities class methodsFor: 'flaps'!removeFlapTab: aFlapTab keepInList: aBoolean	"Remove the given flap tab from the screen, and, if aBoolean is true, also from the global list"	(FlapTabs ~~ nil and: [FlapTabs includes: aFlapTab])		ifTrue:			[aBoolean ifFalse: [self removeFromGlobalFlapTabList: aFlapTab]].	aFlapTab ifNotNil:		[aFlapTab referent delete.		aFlapTab delete]! !!Utilities class methodsFor: 'flaps' stamp: 'sw 2/5/2001 17:12'!replaceBrowserInToolsFlap	"Replace the browsers shown in the Tools flap, if any, with updated versions"	self replacePartSatisfying: [:el |  (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isMemberOf: Browser]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  ((Browser new openAsMorphEditing: nil) applyModelExtent; setLabel: 'System Browser'; setBalloonText: 'System Browser'; yourself).	self replacePartSatisfying: [:el |  (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isMemberOf: PackagePaneBrowser]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  ((PackagePaneBrowser new openAsMorphEditing: nil) applyModelExtent; setLabel: 'Package Browser'; setBalloonText: 'Package browser'; yourself)	"Utilities replaceBrowserInToolsFlap"! !!Utilities class methodsFor: 'flaps' stamp: 'sw 2/5/2001 17:03'!replaceChangeSortersInToolsFlap	"Get prototypes of the latest versions of the the Change Sorters into the Tools flap"	self replacePartSatisfying: [:el |  (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isMemberOf: ChangeSorter]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  (ChangeSorter new morphicWindow applyModelExtent; setBalloonText: 'Single Change Sorter'; yourself).	self replacePartSatisfying: [:el |  (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isMemberOf: DualChangeSorter]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  (DualChangeSorter new morphicWindow applyModelExtent; setBalloonText: 'Dual Change Sorter'; yourself)! !!Utilities class methodsFor: 'flaps'!replaceMenuFlap 	"if there is a menu flap, replace it with an updated one." 	| aFlapTab wasGlobal |	aFlapTab _ self currentMenuFlap ifNil: [^ self].	wasGlobal _ aFlapTab isGlobal.	self removeFlapTab: aFlapTab keepInList: false.	self addMenuFlap.  "This will be project local"	wasGlobal ifTrue:		[self currentMenuFlap toggleIsGlobalFlap].	Smalltalk isMorphic ifTrue: [Display bestGuessOfCurrentWorld addGlobalFlaps]"Utilities replaceMenuFlap"! !!Utilities class methodsFor: 'flaps' stamp: 'sw 2/5/2001 17:11'!replaceScriptingAreaInToolsFlap	"Replace the scripting area in the tools flap with an updated one.  However, this is now obsolete because the scripting area in the Tools flap is no longer in a window and hence won't be found by this.  In any case, it was only called from do-its in postscripts of fileouts."	self replacePartSatisfying: [:el |  (el isKindOf: MorphThumbnail) and: [(el morphRepresented isKindOf: SystemWindow) and: [el morphRepresented model isKindOf: ScriptingDomain]]]inGlobalFlapSatisfying: [:f1 | f1 wording = 'Tools'] with:  ScriptingSystem newScriptingSpace	"Utilities replaceScriptingAreaInToolsFlap"! !!Utilities class methodsFor: 'flaps' stamp: 'sw 2/5/2001 16:53'!standardRightFlap	"Answer a newly-created flap which adheres to the right edge of the screen and which holds prototypes of standard tools"	|  aFlapTab aPage |	aPage _ self newPartsFlapPage.	aFlapTab _ FlapTab new referent: aPage beSticky.	aFlapTab color: Color red lighter.	aFlapTab assumeString: 'Tools' font: Preferences standardFlapFont orientation: #vertical color: Color orange lighter.	aFlapTab edgeToAdhereTo: #right; inboard: false.	aFlapTab setToPopOutOnDragOver: true.	aFlapTab setToPopOutOnMouseOver: false.	aPage extent: (90 @ self currentWorld height).	self addSampleWindowsTo: aPage.	aPage addMorphBack: ScriptingSystem newScriptingSpace.	aPage addMorphBack: RecordingControlsMorph authoringPrototype.	aPage replaceTallSubmorphsByThumbnails.  "This hard-coded list is regrettable but expedient"	#('System Browser' 'Package-Pane Browser' 'Workspace' 'File List' 'Dual Change Sorter' 'Single Change Sorter' 'Selector Browser' 'Assembly Area' 'Scripting Area' 'Sound Recorder') doWithIndex: 		[:help :index |			(aPage submorphs at: index) setBalloonText: help].	aFlapTab position: (self currentWorld width - aFlapTab width) @ ((Display height - aFlapTab height) // 2).	aPage beFlap: true.	aPage color: (Color brown muchLighter alpha: 0.5).	aPage extent: (90 @ self currentWorld height).		^ aFlapTab! !Utilities class removeSelector: #menuFlap!"Postscript:"Preferences okToReinitializeFlaps ifTrue: [Utilities reinstateDefaultFlaps].!