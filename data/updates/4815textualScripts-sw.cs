'From Squeak3.3alpha of 12 January 2002 [latest update: #4812] on 28 March 2002 at 12:43:12 am'!"Change Set:		textualScripts-swDate:			27 March 2002Author:			Scott WallaceFixes up two long-standing issues relating to textually-coded scripts:(1)  When in a Scriptor you edit a script textually and then try to revert to tiles, you're now given a warning that your textual changes will be lost, and a chance to back out.(2)  Makes it possible for the selector of a script to be changed (by editing the script-name in the Scriptor header) while the script is in textual mode -- formerly this had had to be disallowed, and in the one case that had slipped through the cracks, had led to errors."!MethodWithInterface subclass: #UniclassScript	instanceVariableNames: 'currentScriptEditor formerScriptingTiles isTextuallyCoded lastSourceString '	classVariableNames: ''	module: #(Squeak EToy Scripting)!!MethodHolder methodsFor: 'miscellaneous' stamp: 'sw 3/28/2002 00:36'!changeMethodSelectorTo: aSelector	"Change my method selector as noted.  Reset currentCompiledMethod"	methodSelector _ aSelector.	currentCompiledMethod _ methodClass compiledMethodAt: aSelector ifAbsent: [nil]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 3/28/2002 00:37'!renameScriptTo: newSelector	"Rename the receiver's script so that it bears a new selector"	| aMethodNodeMorph methodMorph methodSource pos newMethodSource |	scriptName _ newSelector.	self updateHeader.	Preferences universalTiles		ifFalse:  "classic tiles"			[self showingMethodPane				ifTrue:					["textually coded -- need to change selector"					methodMorph _ self findA: MethodMorph.					methodSource _ methodMorph text string.					pos _ methodSource indexOf: Character cr ifAbsent: [self error: 'no cr'].					newMethodSource _ newSelector, (methodSource copyFrom: pos to: methodSource size).					methodMorph editString: newMethodSource.					methodMorph model changeMethodSelectorTo: newSelector.					playerScripted class compile: newMethodSource classified: 'scripts'.					methodMorph accept]				ifFalse:					[self install]]		ifTrue:  "universal tiles..."			[(aMethodNodeMorph _ self methodNodeMorph) ifNotNil:				[aMethodNodeMorph acceptInCategory: 'scripts']]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 3/27/2002 22:57'!toggleWhetherShowingTiles	"Toggle between showing the method pane and showing the tiles pane"	self showingMethodPane		ifFalse:				"currently showing tiles"			[self showSourceInScriptor]		ifTrue:				"current showing textual source"			[Preferences universalTiles				ifTrue: [^ self revertToTileVersion].			self savedTileVersionsCount >= 1				ifTrue:					[(self userScriptObject lastSourceString = (playerScripted class compiledMethodAt: scriptName) decompileString)						ifFalse:							[(self confirm: 'Caution -- this script was changedtextually; if you revert to tiles at thispoint you will lose all the changes youmay have made textually.  Do youreally want to do this?') ifFalse: [^ self]].					self revertToTileVersion]				ifFalse:					[self beep]]! !!UniclassScript methodsFor: 'textually coded' stamp: 'sw 3/28/2002 00:39'!becomeTextuallyCoded	"Transform the receiver into one which is textually coded"	isTextuallyCoded _ true.	lastSourceString _ (playerClass compiledMethodAt: selector) decompileString 		"Save this to compare when going back to tiles"! !!UniclassScript methodsFor: 'textually coded' stamp: 'sw 3/27/2002 22:43'!lastSourceString	"Answer the most recent source string"	^ lastSourceString! !!UniclassScript methodsFor: 'textually coded' stamp: 'sw 3/28/2002 00:40'!okayToRename	"Answer whether the receiver is in a state to be renamed.  Formerly, we had not allowed renaming while in textual-scripting mode, but now we do, so this protocol is likely now moot"	^ true! !