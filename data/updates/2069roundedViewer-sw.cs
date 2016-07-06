'From Squeak2.8alpha of 13 January 2000 [latest update: #2067] on 6 May 2000 at 2:33:38 am'!"Change Set:		roundedViewer-swDate:			6 May 2000Author:			Scott WallaceMakes corners of viewers and scriptors be rounded, by default -- also the tabbed palette in the standard scripting space, and its playfield."!!ScriptEditorMorph methodsFor: 'initialization' stamp: 'sw 5/5/2000 14:34'!setDefaultBorderCharacteristics		borderWidth _ 1.	borderColor _ Color black.	inset _ 2.	self useRoundedCorners! !!ScriptEvaluatorMorph methodsFor: 'display' stamp: 'sw 5/5/2000 14:35'!setDefaultBorderCharacteristics		borderWidth _ 2.	borderColor _ Color blue.	inset _ 2.	self useRoundedCorners! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 5/6/2000 02:25'!newScriptingSpace	"Answer a complete scripting space"	| aTemplate  palette aPlayfield aWindow itsModel |	aWindow _ (SystemWindow labelled: 'scripting area') model: (itsModel _ ScriptingDomain new).	aWindow setStripeColorsFrom: itsModel defaultBackgroundColor.	aWindow extent: 640 @ 480.	aTemplate _ PasteUpMorph new setNameTo: 'etoy'.	aTemplate extent: 638 @ 470.	aWindow addMorph: aTemplate frame: (0@0 corner: 1@1).	aTemplate setStandardTexture.	aTemplate impartPrivatePresenter; setProperty: #automaticPhraseExpansion toValue: true; beSticky.	aTemplate presenter addStopStepGoButtons; addTrashCan.	palette _ TabbedPalette new pageSize: 200@320.	palette beSticky; useRoundedCorners.	aTemplate addMorph: palette.	palette addMenuTab.	palette addPartsTab.	palette addControlsTab.	palette addTilesTab.	palette selectTabNamed: 'supplies'.	palette becomeStandardPalette.	aPlayfield _ PasteUpMorph new setNameTo: 'playfield'.	aPlayfield useRoundedCorners.	(Preferences valueOfFlag: #eToyScheme)  "Not widely advertised"		ifFalse:				[aPlayfield setToAdhereToEdge: #topRight.			palette setToAdhereToEdge: #topLeft]		ifTrue:			[aPlayfield setToAdhereToEdge: #topLeft.			palette setToAdhereToEdge: #topRight].	aTemplate addMorph: aPlayfield.	aPlayfield extent: 340@300; position: aTemplate topRight - (400@0).	aPlayfield beSticky.	aPlayfield automaticViewing: true.	aPlayfield wantsMouseOverHalos: true.	aTemplate presenter standardPlayfield: aPlayfield.		^ aWindow! !!Viewer methodsFor: 'as yet unclassified' stamp: 'sw 5/5/2000 14:33'!initialize	super initialize.	self useRoundedCorners! !"Postscript:"Utilities replaceScriptingAreaInToolsFlap.!