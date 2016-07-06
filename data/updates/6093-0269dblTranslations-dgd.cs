'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #267] on 26 August 2004 at 1:46:56 pm'!"Change Set:		DoubleTranslations-dgdDate:			26 August 2004Author:			Diego Gomez Deck- Removes a lot of double translations (#translated sends to already translated strings).- Addes translation for the help part of the quads for PartsBinTo detect double translations you have to switch to a complete translation (like Spanish) and hack NLT>>translate: in this way:	translate: aString		^self generics			at: aString			ifAbsent: [				self localeID hasParent					ifTrue: [(self class localeID: self localeID parent) translate: aString]					ifFalse: [						(generics values includes: aString)							ifTrue:[self error:'double translation'].						aString					]			]"!!AlignmentMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'Alignment'! !!CategoryViewer methodsFor: 'entries' stamp: 'dgd 8/26/2004 12:04'!phraseForCommandFrom: aMethodInterface
	"Answer a phrase for the non-slot-like command represented by aMethodInterface - classic tiles"

	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst aDocString universal tileBearingHelp |
	aDocString _ aMethodInterface documentation.
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
				argTile _ ScriptingSystem tileForArgType: argType.				(#(bounce: wrap:) includes: cmd) ifTrue:					["help for the embattled bj"					argTile setLiteral: #silence].
				argTile position: aPhrase lastSubmorph position.
				aPhrase lastSubmorph addMorph: argTile]].

	(scriptedPlayer slotInfo includesKey: cmd)
		ifTrue: [balloonTextSelector _ #userSlot].

	(scriptedPlayer belongsToUniClass and: [scriptedPlayer class includesSelector: cmd])
		ifTrue:
			[aDocString ifNil:
				[aDocString _ (scriptedPlayer class userScriptForPlayer: scriptedPlayer selector: cmd) documentation].
			aDocString ifNil:
				[balloonTextSelector _ #userScript]].

	tileBearingHelp _ universal ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]. 
	aDocString
		ifNotNil:
			[tileBearingHelp setBalloonText: aDocString]
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

	^ aRow! !!MethodMorph class methodsFor: 'as yet unclassified' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'Method'! !!ObjectsTool methodsFor: 'alphabetic' stamp: 'dgd 8/26/2004 12:26'!installQuads: quads fromButton: aButton	"Install items in the bottom pane that correspond to the given set of quads, as triggered from the given button"	| aPartsBin sortedQuads |	aPartsBin _ self findDeeplyA: PartsBin.	aPartsBin removeAllMorphs.	sortedQuads _ (PartsBin translatedQuads: quads)							asSortedCollection: [:a :b | a third < b third].	aPartsBin listDirection: #leftToRight quadList: sortedQuads.	aPartsBin width: self innerBounds width.	aButton ifNotNil: [self tabsPane highlightOnlySubmorph: aButton].	aPartsBin vResizing: #shrinkWrap.! !!PartsBin methodsFor: 'initialization' stamp: 'dgd 8/26/2004 12:06'!listDirection: aListDirection quadList: quadList	"Initialize the receiver to run horizontally or vertically, obtaining its elements from the list of tuples of the form:		(<receiver> <selector> <label> <balloonHelp>)"	| aButton aClass |	self layoutPolicy: TableLayout new.	self listDirection: aListDirection.	self wrapCentering: #topLeft.	self layoutInset: 2.	self cellPositioning: #bottomCenter.	aListDirection == #leftToRight		ifTrue:			[self vResizing: #rigid.			self hResizing: #spaceFill.			self wrapDirection: #topToBottom]		ifFalse:			[self hResizing: #rigid.			self vResizing: #spaceFill.			self wrapDirection: #leftToRight].	quadList do:		[:tuple |			aClass _ Smalltalk at: tuple first.			aButton _ IconicButton new initializeWithThumbnail: (self class thumbnailForQuad: tuple) withLabel: tuple third andColor: self color andSend: tuple second to: aClass.			(tuple size > 3 and: [tuple fourth isEmptyOrNil not]) ifTrue:				[aButton setBalloonText: tuple fourth]. 			self addMorphBack: aButton]! !!PartsBin class methodsFor: 'instance creation' stamp: 'dgd 8/26/2004 12:24'!newPartsBinWithOrientation: aListDirection andColor: aColor from: quadList 	"Answer a new PartBin object, to run horizontally or vertically,  	obtaining its elements from the list of tuples of the form:  	(<receiver> <selector> <label> <balloonHelp>)"	^ (self newPartsBinWithOrientation: aListDirection from: quadList) color: aColor; yourself! !!PartsBin class methodsFor: 'instance creation' stamp: 'dgd 8/26/2004 12:24'!newPartsBinWithOrientation: aListDirection from: quadList 	"Answer a new PartBin object, to run horizontally or vertically,  	obtaining its elements from the list of tuples of the form:  	(<receiver> <selector> <label> <balloonHelp>)"	^ self new		listDirection: aListDirection		quadList: (self translatedQuads: quadList) ! !!PartsBin class methodsFor: 'private' stamp: 'dgd 8/26/2004 12:23'!translatedQuads: quads	"private - convert the given quads to a translated one"		| translatedQuads |	translatedQuads := quads collect: [:each |		| element |		element := each copy. 		element at: 3 put: each third translated.		element at: 4 put: each fourth translated.		element.	].	^ translatedQuads! !!PhraseTileMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'PhraseTile'! !!ProjectViewMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'ProjectView'! !!SelectionMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'Selection'! !!SimpleButtonMorph class methodsFor: 'printing' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'button'! !!SketchMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:11'!defaultNameStemForInstances	^ 'Sketch'! !!TextMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:12'!defaultNameStemForInstances	^ 'Text'! !!TileMorph methodsFor: 'initialization' stamp: 'dgd 8/26/2004 12:04'!setToReferTo: anObject	"Set the receiver to bear an object reference to the given object."	self flag: #yo.	type _ #objRef.	actualObject _ anObject.	self line1: anObject nameForViewer.	self typeColor: (ScriptingSystem colorForType: #Player).	self enforceTileColorPolicy! !!TileMorph class methodsFor: 'scripting' stamp: 'dgd 8/26/2004 12:12'!defaultNameStemForInstances	^ 'Tile'! !!ViewerFlapTab class methodsFor: 'printing' stamp: 'dgd 8/26/2004 12:12'!defaultNameStemForInstances	^ 'viewerFlapTab'! !