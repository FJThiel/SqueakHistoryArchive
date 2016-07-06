'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:53 pm'!
"Change Set:		q12-viewerFeedback-sw
Date:			3 March 2004
Author:			Scott Wallace

Derived from the following Squeakland updates:
  0145grabVarFromNoun-sw.cs (27 February 2003)
  0171getSetFeedback-sw (22 May 03)
  0175viewerMouseover-sw.cs (6 June 03)
  0178mouseOverFix-sw.cs (17 June 03)


A drag from the noun at the head of a variable/slot line (e.g. 'car's heading') now hands you a getter for the slot.
Provides mouse-over feedback for getters and setters in etoy viewers.
Further refinements to the mouseover feedback for Viewer items introduced earlier:
¥  Mouseover feedback is also provided for command phrases.
¥  Colors used for the slot-tearoff potential and phrase-tearoff potential are different.
Fixes the bug that caused spurious feedback in a viewer for a mouseover of a phrase in a scriptor."!


!CategoryViewer methodsFor: 'entries' stamp: 'sw 3/4/2004 13:25'!
phraseForCommandFrom: aMethodInterface
	"Answer a phrase for the non-slot-like command represented by aMethodInterface - classic tiles"

	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst aDocString universal tileBearingHelp |
	aDocString _ aMethodInterface documentationOrNil.
	aDocString = 'no help available' ifTrue: [aDocString _ nil].
	names _ scriptedPlayer class namedTileScriptSelectors.

	resultType _ aMethodInterface resultType.
	cmd _ aMethodInterface selector.
	(universal _ scriptedPlayer isUniversalTiles)
		ifTrue:
			[aPhrase _ scriptedPlayer universalTilesForInterface: aMethodInterface]
		ifFalse: [cmd numArgs == 0
			ifTrue:
				[aPhrase _ PhraseTileMorph new vocabulary: self currentVocabulary.
				aPhrase setOperator: cmd
					type: resultType
					rcvrType: #Player]
			ifFalse:
				["only one arg supported in classic tiles, so if this is fed
				with a selector with > 1 arg, results will be very strange"
				argType _ aMethodInterface typeForArgumentNumber: 1.
				aPhrase _ PhraseTileMorph new vocabulary: self currentVocabulary.
				aPhrase setOperator: cmd
					type: resultType
					rcvrType: #Player
					argType: argType.
				argTile _ ScriptingSystem tileForArgType: argType.
				argTile position: aPhrase lastSubmorph position.
				aPhrase lastSubmorph addMorph: argTile]].

	(scriptedPlayer slotInfo includesKey: cmd)
		ifTrue: [balloonTextSelector _ #userSlot].

	(scriptedPlayer belongsToUniClass and: [scriptedPlayer class includesSelector: cmd])
		ifTrue:
			[aDocString ifNil:
				[aDocString _ (scriptedPlayer class userScriptForPlayer: scriptedPlayer selector: cmd) documentationOrNil].
			aDocString ifNil:
				[balloonTextSelector _ #userScript]].

	tileBearingHelp _ universal ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]. 
	aDocString
		ifNotNil:
			[tileBearingHelp setBalloonText: aDocString translated]
		ifNil:
			[balloonTextSelector ifNil:
				[tileBearingHelp setProperty: #inherentSelector toValue: cmd.
				balloonTextSelector _ #methodComment].
			tileBearingHelp balloonTextSelector: balloonTextSelector].
	aPhrase markAsPartsDonor.
	cmd == #emptyScript ifTrue:
		[aPhrase setProperty: #newPermanentScript toValue: true.
		aPhrase setProperty: #newPermanentPlayer toValue: scriptedPlayer.
		aPhrase submorphs second setBalloonText: 
'drag and drop to 
add a new script' translated].

	universal ifFalse:
		[selfTile _ self tileForSelf.
		selfTile position: aPhrase firstSubmorph position.
		aPhrase firstSubmorph addMorph: selfTile].

	aRow _ ViewerLine newRow borderWidth: 0; color: self color.
	aRow elementSymbol: cmd asSymbol.

	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).
	aRow addMorphBack: (Morph new extent: 2@2; beTransparent).
	aRow addMorphBack: (self infoButtonFor: cmd).
	aRow addMorphBack: aPhrase.
	aPhrase on: #mouseEnter send: #addCommandFeedback to: aRow.
	aPhrase on: #mouseLeave send: #removeHighlightFeedback to: aRow.
	aPhrase on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

	(names includes: cmd) ifTrue:
		[aPhrase userScriptSelector: cmd.
		cmd numArgs == 0 ifTrue:
			[aPhrase beTransparent.
			aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.
			aRow addMorphBack: (stat _ (inst _ scriptedPlayer scriptInstantiationForSelector: cmd) statusControlMorph).
			inst updateStatusMorph: stat]].

	aRow beSticky; disableDragNDrop.

	^ aRow! !

!CategoryViewer methodsFor: 'entries' stamp: 'sw 3/4/2004 13:29'!
phraseForVariableFrom: aMethodInterface
	"Return a structure consisting of tiles and controls and a readout representing a 'variable' belonging to the player, complete with an appropriate readout when indicated.  Functions in both universalTiles mode and classic mode.  Slightly misnamed in that this path is used for any methodInterface that indicates an interesting resultType."

	| anArrow slotName getterButton cover inner aRow doc setter tryer universal hotTileForSelf spacer buttonFont |

	aRow _ ViewerLine newRow
		color: self color;
		beSticky;
		elementSymbol: (slotName _ aMethodInterface selector);
		wrapCentering: #center;
		cellPositioning: #leftCenter.

	(universal _ scriptedPlayer isUniversalTiles) ifFalse:
		[buttonFont _ Preferences standardEToysFont.
			aRow addMorphBack: (Morph new color: self color;
					 extent: (((buttonFont widthOfString: '!!') + 6) @ (buttonFont height + 6));
					 yourself)].  "spacer"

	aRow addMorphBack: (self infoButtonFor: slotName).
	aRow addMorphBack: (Morph new color: self color; extent: 0@10).  " spacer"

	universal
		ifTrue:
			[inner _ scriptedPlayer universalTilesForGetterOf: aMethodInterface.
			cover _ Morph new color: Color transparent.
			cover extent: inner fullBounds extent.
			(getterButton _ cover copy) addMorph: cover; addMorphBack: inner.
			cover on: #mouseDown send: #makeUniversalTilesGetter:event:from: 
					to: self withValue: aMethodInterface.
			aRow addMorphFront:  (tryer _ ScriptingSystem tryButtonFor: inner).
			tryer color: tryer color lighter lighter]
		ifFalse:
			[hotTileForSelf _ self tileForSelf bePossessive.
			hotTileForSelf  on: #mouseDown send: #makeGetter:event:from:
				to: self
				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).

			aRow addMorphBack: hotTileForSelf.
			aRow addMorphBack: (spacer _ Morph new color: self color; extent: 2@10).
			spacer on: #mouseEnter send: #addGetterFeedback to: aRow.
			spacer on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			spacer on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.
			spacer  on: #mouseDown send: #makeGetter:event:from:
				to: self
				withValue: (Array with: aMethodInterface selector with: aMethodInterface resultType).
			hotTileForSelf on: #mouseEnter send: #addGetterFeedback to: aRow.
			hotTileForSelf on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			hotTileForSelf on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

			getterButton _ self getterButtonFor: aMethodInterface selector type: aMethodInterface resultType].

	aRow addMorphBack: getterButton.
	getterButton on: #mouseEnter send: #addGetterFeedback to: aRow.
	getterButton on: #mouseLeave send: #removeHighlightFeedback to: aRow.
	getterButton on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow.

	(doc _ aMethodInterface documentationOrNil) ifNotNil:
		[getterButton setBalloonText: doc translated].

	universal ifFalse:
		[(slotName == #seesColor:) ifTrue:
			[self addIsOverColorDetailTo: aRow.
			^ aRow].
		(slotName == #touchesA:) ifTrue:
			[self addTouchesADetailTo: aRow.
			^ aRow].
		(slotName == #overlaps:) ifTrue:
			[self addOverlapsDetailTo: aRow.
			^ aRow]].

	aRow addMorphBack: (AlignmentMorph new beTransparent).  "flexible spacer"
	(setter _ aMethodInterface companionSetterSelector) ifNotNil:
		[aRow addMorphBack: (Morph new color: self color; extent: 2@10).  " spacer"
		anArrow _ universal 
			ifTrue: [self arrowSetterButton: #newMakeSetterFromInterface:evt:from:  
						args: aMethodInterface]
			ifFalse: [self arrowSetterButton: #makeSetter:from:forPart:
						args: (Array with: slotName with: aMethodInterface resultType)].
		anArrow beTransparent.
		universal ifFalse:
			[anArrow on: #mouseEnter send: #addSetterFeedback to: aRow.
			anArrow on: #mouseLeave send: #removeHighlightFeedback to: aRow.
			anArrow on: #mouseLeaveDragging send: #removeHighlightFeedback to: aRow].

		aRow addMorphBack: anArrow].
	(#(color:sees: playerSeeingColor copy touchesA: overlaps:) includes: slotName) ifFalse:
 		[(universal and: [slotName == #seesColor:]) ifFalse:
			[aMethodInterface wantsReadoutInViewer ifTrue: 
				[aRow addMorphBack: (self readoutFor: slotName type: aMethodInterface resultType readOnly: setter isNil getSelector: aMethodInterface selector putSelector: setter)]]].

	anArrow ifNotNil: [anArrow step].
	^ aRow! !


!PhraseTileMorph methodsFor: 'mouse' stamp: 'sw 6/17/2003 16:03'!
mouseDown: evt 
	"Handle a mouse-down on the receiver"

	| ed guyToTake dup enclosingPhrase |
	self isPartsDonor ifTrue:
		[dup _ self duplicate.
		dup eventHandler: nil.   "Remove viewer-related evt mouseover feedback"
		evt hand attachMorph: dup.
		dup position: evt position.
		"So that the drag vs. click logic works"
		dup formerPosition: evt position.
		^ self].
	submorphs isEmpty
		ifTrue: [^ self].

	guyToTake _ self.
	[(enclosingPhrase _ guyToTake ownerThatIsA: PhraseTileMorph) notNil] whileTrue:
		[guyToTake _ enclosingPhrase].  "This logic always grabs the outermost phrase, for now anyway"
	
	"the below had comment: 'picking me out of another phrase'"
	"owner class == TilePadMorph
		ifTrue:
			[(ss _ submorphs first) class == TilePadMorph
				ifTrue: [ss _ ss submorphs first].
			guyToTake _  ss veryDeepCopy]."

	(ed _ self enclosingEditor) ifNil: [^ evt hand grabMorph: guyToTake].
	evt hand grabMorph: guyToTake.
	ed startStepping.
	ed mouseEnterDragging: evt.
	ed setProperty: #justPickedUpPhrase toValue: true.
! !


!ViewerLine methodsFor: 'slot' stamp: 'sw 6/6/2003 21:47'!
addCommandFeedback
	"Add screen feedback showing what would be torn off in a drag"

	| aMorph |
	aMorph _ RectangleMorph new bounds: ((submorphs fourth topLeft - (2@1)) corner: (submorphs last bottomRight) + (2@0)).
	aMorph useRoundedCorners; beTransparent; borderWidth: 2; borderColor: (Color r: 1.0 g: 0.548 b: 0.452); lock.
	aMorph setProperty: #highlight toValue: true.
	ActiveWorld addMorphFront: aMorph! !

!ViewerLine methodsFor: 'slot' stamp: 'sw 6/4/2003 02:28'!
addGetterFeedback
	"Add feedback during mouseover of a getter"

	| aMorph |
	aMorph _ RectangleMorph new useRoundedCorners bounds: ((submorphs fourth topLeft - (2@-1)) corner: (submorphs sixth bottomRight) + (2@-1)).
	aMorph beTransparent; borderWidth: 2; borderColor: (Color r: 1.0 g: 0.355 b: 0.839); lock.
	aMorph setProperty: #highlight toValue: true.
	ActiveWorld addMorphFront: aMorph

"
Color fromUser (Color r: 1.0 g: 0.355 b: 0.839)
"! !

!ViewerLine methodsFor: 'slot' stamp: 'sw 5/22/2003 04:30'!
addSetterFeedback
	"Add screen feedback showing what would be torn off to make a setter"

	| aMorph |
	aMorph _ RectangleMorph new bounds: ((submorphs fourth topLeft - (2@1)) corner: (submorphs last bottomRight) + (2@0)).
	aMorph useRoundedCorners; beTransparent; borderWidth: 2; borderColor: (Color r: 1.0 g: 0.548 b: 0.452); lock.
	aMorph setProperty: #highlight toValue: true.
	ActiveWorld addMorphFront: aMorph! !

!ViewerLine methodsFor: 'slot' stamp: 'sw 6/4/2003 02:36'!
removeGetterFeedback
	"Remove any existing getter feedback.  Backward-compatibility only"

	self removeHighlightFeedback! !

!ViewerLine methodsFor: 'slot' stamp: 'sw 6/4/2003 02:30'!
removeHighlightFeedback
	"Remove any existing highlight feedback"

	(ActiveWorld submorphs select: [:m | m hasProperty: #highlight]) do:
		[:m | m delete]! !

!ViewerLine methodsFor: 'slot' stamp: 'sw 6/4/2003 02:37'!
removeSetterFeedback
	"Remove any existing setter feedback"

	self removeHighlightFeedback  "backward compatibility with previously-launched viewer panes only"! !

