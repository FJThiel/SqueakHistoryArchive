'From Squeak2.9alpha of 5 August 2000 [latest update: #3327] on 31 January 2001 at 1:11:11 am'!"Change Set:		expansionRestraint-swDate:			29 January 2001Author:			Scott WallaceThe magical expansion of a line of script into a complete Scriptor appears around a dropped phrase will now only take place if the phrase directly originated in a Viewer. Prevents many annoyances.A do-it in the postscript converts all existing PhraseTileMorphs such that their state accurately reflects whether or not they are in viewers.  Old content not touched by that do-it but later reloaded may show occasional glitches, limited however to overly-aggressive sprouting of scriptors as of old."!TileLikeMorph subclass: #PhraseTileMorph	instanceVariableNames: 'resultType brightenedOnEnter userScriptSelector justGrabbedFromViewer '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Scripting Tiles'!!PhraseTileMorph methodsFor: 'initialization' stamp: 'sw 1/19/2001 14:18'!initialize	"Initialize a nascent instance"	super initialize.	resultType = #unknown.	brightenedOnEnter _ false.	self wrapCentering: #center; cellPositioning: #leftCenter.	self hResizing: #shrinkWrap.	borderWidth _ 0.	self layoutInset: 0.	self extent: 5@5.  "will grow to fit"	justGrabbedFromViewer _ true.  "All new PhraseTileMorphs that go through the initialize process (rather than being copied) are placed in viewers; the clones dragged out from them will thus have this set the right way; the drop code resets this to false"! !!PhraseTileMorph methodsFor: 'mouse' stamp: 'sw 1/19/2001 14:20'!justDroppedInto: newOwner event: evt	"Phrase tiles only auto-expand if they originate from viewers.  Any phrase tile, once dropped, loses its auto-phrase-expansion thing"	justGrabbedFromViewer _ false! !!PhraseTileMorph methodsFor: 'mouse' stamp: 'sw 1/29/2001 21:46'!morphToDropInPasteUp: aPasteUp	"Answer the morph to drop in aPasteUp, given that the receiver is the putative droppee"	| actualObject itsSelector aScriptor adjustment handy |	self isCommand ifFalse: [^ self].	(actualObject _ self actualObject) ifNil: [^ self].	self justGrabbedFromViewer ifFalse: [^ self].	actualObject assureUniClass.	itsSelector _ self userScriptSelector.	aScriptor _ itsSelector isEmptyOrNil		ifFalse:			[adjustment _ 0@0.			actualObject scriptEditorFor: itsSelector]		ifTrue:			["It's a system-defined selector; construct an anonymous scriptor around it"			adjustment _ 60 @ 20.			actualObject newScriptorAround: self].	handy _ aPasteUp primaryHand.	aScriptor ifNotNil: [aScriptor position: handy position - adjustment].	^ aScriptor ifNil: [self]! !!PhraseTileMorph methodsFor: 'miscellaneous' stamp: 'sw 1/25/2001 10:52'!addAddHandMenuItemsForHalo: aMenu hand: aHand	"Add additional items to the halo manu"	super addAddHandMenuItemsForHalo: aMenu hand: aHand.	aMenu add: 'Sprout a new scriptor around this phrase' target: self action: #sproutNewScriptor! !!PhraseTileMorph methodsFor: 'miscellaneous' stamp: 'sw 1/19/2001 15:06'!justGrabbedFromViewer	"Answer whether the receiver originated in a Viewer.  Only tiles that originated in a viewer will ever do that infernal sprouting of a new script around them.  The nil branch is only for backward compatibility."	^ justGrabbedFromViewer ifNil: [justGrabbedFromViewer _ true]! !!PhraseTileMorph methodsFor: 'miscellaneous' stamp: 'sw 1/19/2001 14:25'!justGrabbedFromViewer: aBoolean	"Set the receiver's justGrabbedFromViewer instance variable"	justGrabbedFromViewer _ aBoolean! !!PhraseTileMorph methodsFor: 'miscellaneous' stamp: 'sw 1/19/2001 20:26'!justGrabbedFromViewerOrNil	"Answer the value of the receiver's justGrabbedFromViewer slot.  Needed only for conversion methods"	^ justGrabbedFromViewer! !!PhraseTileMorph methodsFor: 'miscellaneous' stamp: 'sw 1/25/2001 11:02'!sproutNewScriptor	"The receiver, operating as a naked phrase tile, wishes to get iself placed in a nascent script"	| newScriptor |	self actualObject assureUniClass.	newScriptor _ self actualObject newScriptorAround:		((self ownerThatIsA: Viewer orA: ScriptEditorMorph)			ifNotNil:				[self fullCopy]			ifNil:				[self]).	self currentHand attachMorph: newScriptor! !!PhraseTileMorph class methodsFor: 'backward compatibility' stamp: 'sw 1/19/2001 22:25'!markViewerOrigination	"For bringing old content forward"	| hadIt gotIt didntWantIt |	hadIt _ 0.	gotIt _ 0.	didntWantIt _ 0.	self allSubInstancesDo:		[:m | (m ownerThatIsA: CategoryViewer)			ifNil:				[m justGrabbedFromViewer: false.				didntWantIt _ didntWantIt + 1]			ifNotNil:				[(m justGrabbedFromViewerOrNil == true)					ifTrue:						[hadIt _ hadIt + 1]					ifFalse:						[m justGrabbedFromViewer: true.						gotIt _ gotIt + 1]]].	Transcript cr; show: 'updating phrase tiles -- already ok: '; show: hadIt; show: '  marked as in-viewer: '; show: gotIt; show: '  marked as not-in-viewer: '; show: didntWantIt.	"PhraseTileMorph markViewerOrigination"! !!Player methodsFor: 'scripts-kernel' stamp: 'sw 1/28/2001 23:27'!"Translate commandSpec into a PhraseTileMorph.  Put appropriate balloon help into the phrase"commandPhraseFor: commandSpec inViewer: aViewer	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst ut aDocString |	names _ self class namedTileScriptSelectors.	resultType _ (commandSpec at: 1).	cmd _ (commandSpec at: 2).	(ut _ self isUniversalTiles)		ifTrue:			[aPhrase _ (CategoryViewer new) newTilesFor: self command: commandSpec]		ifFalse: [commandSpec size = 3			ifTrue:				[aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player]			ifFalse: "commandSpec size is four"				[argType _ commandSpec at: 4.				aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player					argType: argType.				argTile _ self tileForArgType: argType inViewer: aViewer.				argTile position: aPhrase lastSubmorph position.				aPhrase lastSubmorph addMorph: argTile]].	(self slotInfo includesKey: cmd)		ifTrue: [balloonTextSelector _ #userSlot].	(self belongsToUniClass and: [self class includesSelector: cmd])		ifTrue:			[aDocString _ (self class userScriptForPlayer: self selector: cmd) documentationOrNil.			aDocString ifNotNil:					[aPhrase submorphs second setBalloonText: aDocString]				ifNil:					[balloonTextSelector _ #userScript]].	(ut ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]) balloonTextSelector: 			(balloonTextSelector ifNil: [cmd]).	aPhrase markAsPartsDonor.	cmd == #emptyScript ifTrue:		[aPhrase setProperty: #newPermanentScript toValue: true.		aPhrase setProperty: #newPermanentPlayer toValue: self.		aPhrase submorphs second setBalloonText: 'drag and drop to add a new script'].	ut ifFalse: [		selfTile _ aViewer tileForSelf.		selfTile position: aPhrase firstSubmorph position.		aPhrase firstSubmorph addMorph: selfTile].	aRow _ ViewerRow newRow borderWidth: 0; color: aViewer color.	aRow elementSymbol: cmd asSymbol.	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).	aRow addMorphBack: (Morph new extent: 4@2; beTransparent).	aRow addMorphBack: (aViewer infoButtonFor: cmd).	aRow addMorphBack: aPhrase.	(names includes: cmd) ifTrue:		[aPhrase userScriptSelector: cmd.		aPhrase beTransparent.		aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.		aRow addMorphBack: (stat _ (inst _ self scriptInstantiationForSelector: cmd) statusControlMorph).		inst updateStatusMorph: stat].	aRow beSticky; disableDragNDrop.	^ aRow! !!ScriptEditorMorph methodsFor: 'dropping/grabbing' stamp: 'sw 1/28/2001 02:43'!acceptDroppingMorph: aMorph event: evt	"Allow the user to add tiles and program fragments just by dropping them on this morph."	| i slideMorph p1 p2 |	self prepareToUndoDropOf: aMorph.	"Find where it will go, and prepare to animate the move..."	i _ self rowInsertionIndexFor: aMorph fullBounds center.	slideMorph _ aMorph imageForm offset: 0@0.	p1 _ aMorph screenRectangle topLeft.	aMorph delete.	self stopStepping.	self world displayWorld.  "Clear old image prior to animation"	(aMorph isKindOf: PhraseTileMorph) ifTrue:		[aMorph unbrightenTiles.		aMorph justGrabbedFromViewer: false].	aMorph tileRows do: [:tileList |		self insertTileRow: (Array with:				(tileList first rowOfRightTypeFor: owner forActor: aMorph associatedPlayer))			after: i.		i _ i + 1].	self removeSpaces.	self enforceTileColorPolicy.	self layoutChanged.	self fullBounds. "force layout"	"Now animate the move, before next Morphic update.		NOTE: This probably should use ZoomMorph instead"	p2 _ (self submorphs atPin: (i-1 max: firstTileRow)) screenRectangle topLeft.	slideMorph slideFrom: p1 to: p2 nSteps: 5 delay: 50 andStay: true.	self playSoundNamed: 'scritch'.	self topEditor install  "Keep me for editing, a copy goes into lastAcceptedScript"! !!PhraseTileMorph class reorganize!('backward compatibility' markViewerOrigination)!"Postscript:"PhraseTileMorph markViewerOrigination.!