'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:49 pm'!
"Change Set:	q01-etoyFixes-sw
Date:			11 March 2003
Author:			Scott Wallace

¥  When the user renames a script, script-firing buttons will now automatically be relabeled, unless they have already manually been edited, and will be fixed up so that they properly fire the renamed script.
¥  When the user asks to remove an instance variable, a check is now made to see if any scripts belonging to the object currently use that variable -- if so, permission to remove the variable is denied.  If not, a further check is made to see if there are any extant scripts in *other* objects that may reference that instance variable, and if any are found, a warning is put up.
¥  The menu item that offered the user the opportunity to rename an instance variable is suppressed -- too many ways things can go wrong there.
¥  The halo menu for script-activation-buttons is considerably fixed up -- it had been showing quite a few items in two places.
¥  Provides a class comment for ScriptActivationButton.

This version adapted 3/2/04 by Scott Wallace from Squeakland update 0147eToyFixes-sw.  The methods that required manual reintegration with 3.7a were:

Player.slotInfoButtonHitFor:inViewer:
ScriptActivationButton.addCustomMenuItems:hand:
"!

SimpleButtonMorph subclass: #ScriptActivationButton
	instanceVariableNames: 'uniclassScript '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Scripting'!

!ScriptActivationButton commentStamp: 'sw 3/11/2003 00:24' prior: 0!
A button associated with a particular player and script.  You can "tear off" such a button for any etoy script, using menu items available in both Viewers and Scriptors.  The button initially is given a label reflecting its player and script name, but this can be edited via the button's halo menu, as can its other appearance parameters.  Such buttons are automatically kept in synch when the object's name or the script name change.!


!MethodWithInterface methodsFor: 'rename' stamp: 'sw 3/11/2003 00:01'!
renameScript: newSelector fromPlayer: aPlayer
	"The receiver's selector has changed to the new selector.  Get various things right, including the physical appearance of any Scriptor open on this method"

	self allScriptEditors do:
		[:aScriptEditor | aScriptEditor renameScriptTo: newSelector].

	(selector numArgs = 0 and: [newSelector numArgs = 1])
		ifTrue:
			[self argumentVariables: (OrderedCollection with:
				(Variable new name: #parameter type: #Number))].
	(selector numArgs = 1 and: [newSelector numArgs = 0])
		ifTrue:
			[self argumentVariables: OrderedCollection new].

	selector _ newSelector asSymbol.
	self bringUpToDate.
	self playerClass atSelector: selector putScript: self.
	self allScriptActivationButtons do:
		[:aButton | aButton bringUpToDate].

! !

!MethodWithInterface methodsFor: 'script editor' stamp: 'sw 3/10/2003 23:58'!
allScriptActivationButtons
	"Answer all the script-activation buttons that exist for this interface"

	^ ScriptActivationButton allInstances select: 
		[:aButton | aButton uniclassScript == self]! !


!Player methodsFor: 'costume' stamp: 'sw 3/11/2003 00:32'!
tearOffButtonToFireScriptForSelector: aSelector
	"Tear off a button to fire the script for the given selector"

	| aButton props |
	Preferences useButtonProprtiesToFire ifFalse:
		[aButton _ ScriptActivationButton new.
		aButton initializeForPlayer: self uniclassScript:  (self class scripts at: aSelector).
		^ aButton openInHand].

	(aButton _ RectangleMorph new) useRoundedCorners; color: Color yellow.
	props _ aButton ensuredButtonProperties.
	props
		target: self;
		actionSelector: #runScript:;
		arguments: {aSelector};
		delayBetweenFirings: 80;
		actWhen: #mouseUp;
		mouseDownHaloWidth: 8;
		wantsRolloverIndicator: true;
		mouseOverHaloWidth: 5;
		establishEtoyLabelWording.
	aButton width: aButton submorphs first width + 20; height: 20.
	self currentHand attachMorph: aButton.
! !

!Player methodsFor: 'scripts-kernel' stamp: 'sw 3/2/2004 20:56'!
slotInfoButtonHitFor: aGetterSymbol inViewer: aViewer
	"The user made a gesture asking for slot menu for the given getter symbol in a viewer; put up the menu."

	| aMenu slotSym aType typeVocab |
	slotSym _ Utilities inherentSelectorForGetter: aGetterSymbol.
	aType _ self typeForSlotWithGetter: aGetterSymbol asSymbol.
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: (slotSym asString, ' (', aType, ')').

	(typeVocab _ Vocabulary vocabularyForType: aType) addWatcherItemsToMenu: aMenu forGetter: aGetterSymbol.

	(self slotInfo includesKey: slotSym)
		ifTrue:
			[aMenu add: 'change data type' translated selector: #chooseSlotTypeFor: argument: aGetterSymbol.
			typeVocab addUserSlotItemsTo: aMenu slotSymbol: slotSym.
			aMenu add: ('remove "{1}"' translated format: {slotSym}) selector: #removeSlotNamed: argument: slotSym.
			aMenu add: ('rename "{1}"' translated format: {slotSym}) selector: #renameSlot: argument: slotSym.			aMenu addLine].

	typeVocab addExtraItemsToMenu: aMenu forSlotSymbol: slotSym.  "e.g. Player type adds hand-me-tiles"

	aMenu add: 'show categories....' translated target: aViewer selector: #showCategoriesFor: argument: aGetterSymbol.
	self addIdiosyncraticMenuItemsTo: aMenu forSlotSymol: slotSym.

	aMenu items isEmpty ifTrue:
		[aMenu add: 'ok' translated action: #yourself].

	aMenu popUpForHand: aViewer primaryHand in: aViewer world! !

!Player methodsFor: 'slots-user' stamp: 'sw 3/10/2003 14:54'!
removeSlotNamed: aSlotName
	"The user has requested that an instance variable be removed..."

	| aSetter aGetter |
	(self okayToRemoveSlotNamed: aSlotName) ifFalse:
		[^ self inform: 'Sorry, ', aSlotName, ' is in
use in a script.'].

	aSetter _ Utilities setterSelectorFor: aSlotName.
	aGetter _ Utilities getterSelectorFor: aSlotName.
	((Smalltalk allCallsOn: aSetter) size > 0 or: [(Smalltalk allCallsOn: aGetter) size > 0]) ifTrue:
		[self inform: 
'Caution!!  There may be scripts belonging to
other objects that may rely on the presence of
this variable.  If there are, they may now be broken.
You may need to fix them up manually.'].

	self class removeInstVarName: aSlotName asString.

	self updateAllViewers! !


!ScriptActivationButton methodsFor: 'initialization' stamp: 'sw 3/11/2003 00:31'!
initializeForPlayer: aPlayer uniclassScript: aUniclassScript
	"Initialize the receiver for the given player and uniclass script"

	target _ aPlayer.
	uniclassScript _ aUniclassScript.
	actionSelector _ #runScript:.
	arguments _ Array with: uniclassScript selector.
	self establishLabelWording
	! !

!ScriptActivationButton methodsFor: 'menu' stamp: 'sw 3/2/2004 20:58'!
addCustomMenuItems: aMenu hand: aHandMorph
	"Add morph-specific items to the given menu which was invoked by the given hand."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'open underlying scriptor' translated target: target selector: #openUnderlyingScriptorFor: argument: arguments first

! !

!ScriptActivationButton methodsFor: 'access' stamp: 'sw 3/10/2003 23:57'!
uniclassScript
	"Answer the receiver's uniclassScript.  For old buttons, this might initially be nil but will get set, when possible herein."

	^ uniclassScript ifNil:
		[uniclassScript _ target class scripts at: arguments first ifAbsent: [nil]]! !

!ScriptActivationButton methodsFor: 'miscellaneous' stamp: 'sw 3/11/2003 00:35'!
bringUpToDate
	"The object's name, or the script name, or both, may have changed.  Make sure I continue to look and act right"

	uniclassScript ifNotNil:
		[arguments _ Array with: uniclassScript selector].
	self establishLabelWording! !


!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 3/10/2003 14:34'!
hasScriptReferencing: aSlotName ofPlayer: aPlayer
	"Answer whether the receiver has any tiles in it which reference the given slot of the given player.  By doing a text search on the decompiled method, this is able to work both with text and with tiles.  The approach is still not perfect, because we can't really know until run-time whom the getters and setters are sent to.  But practically speaking, this is all presumably a positive."

	| stringToSearch |
	(aPlayer isKindOf: playerScripted class) ifFalse: [^ false].

	stringToSearch _ (playerScripted class compiledMethodAt: scriptName) decompileString.
	{Utilities getterSelectorFor: aSlotName. Utilities setterSelectorFor: aSlotName} do:
		[:searchee |
			(stringToSearch findString: searchee startingAt: 1) = 0
				ifFalse:
					[^ true]]. 

	^ false! !

SimpleButtonMorph subclass: #ScriptActivationButton
	instanceVariableNames: 'uniclassScript'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Scripting'!

!ScriptActivationButton reorganize!
('initialization' initializeForPlayer:uniclassScript:)
('menu' addCustomMenuItems:hand:)
('access' uniclassScript)
('label' establishLabelWording isTileScriptingElement)
('miscellaneous' bringUpToDate setLabel)
!

