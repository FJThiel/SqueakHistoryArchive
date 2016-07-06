'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #328] on 8 October 2004 at 3:27:16 pm'!"Change Set:		ScriptFontFix3-nkDate:			8 October 2004Author:			Ned KonzBrute force repair for old ScriptEditorMorphs: convert all the fonts used by their StringMorphs into the current Etoys font and fix the layout.Fixes bug# 0000089v2: also fix non-visible ScriptEditorMorphs. Still doesn't fix assignment arrows, though. Also prevents related walkback on updateLocaleDependents with old projects.v3: also fix assignment arrows and loose PhraseTileMorphs. Don't touch button font, though."!!ScriptEditorMorph methodsFor: 'menu' stamp: 'nk 10/8/2004 15:27'!fixLayout	"Tell all morphs that their layout has changed, using a sledge-hammer"	"fix fonts and layout"	self		allMorphsDo: [:m | 			m ~~ self				ifTrue: [(m respondsTo: #fixLayout)						ifTrue: [m fixLayout]].			m layoutChanged]! !!ScriptEditorMorph methodsFor: 'other' stamp: 'nk 10/8/2004 11:47'!unhibernate	"I have been loaded as part of an ImageSegment.	Make sure that I am fixed up properly."	self topEditor == self		ifFalse: [^ self]. "Part of a compound test"	self updateHeader.	self fixLayout.	"Recreate my tiles from my method if i have new universal tiles."	self world		ifNil: [(playerScripted isNil					or: [playerScripted isUniversalTiles not])				ifTrue: [^ self]]		ifNotNil: [Preferences universalTiles				ifFalse: [^ self]].	self insertUniversalTiles.	self showingMethodPane: false! !!TileLikeMorph methodsFor: 'user interface' stamp: 'nk 10/8/2004 15:11'!fixLayout	"Having just been loaded from a project, do repairs as needed."	self minCellSize: 0 @ (Preferences standardEToysFont height rounded + 10).	self allMorphsDo: [:m | (m isKindOf: StringMorph)				ifTrue: [m font: Preferences standardEToysFont;						 fitContents].		(m respondsTo: #fixLayout)			ifTrue: [ m ~~ self ifTrue: [ m fixLayout ] ]			ifFalse: [ m layoutChanged ] ].	self layoutChanged; fullBounds! !!TileLikeMorph methodsFor: 'user interface' stamp: 'nk 10/8/2004 15:13'!unhibernate	self fixLayout.! !!CompoundTileMorph methodsFor: 'initialization' stamp: 'nk 10/8/2004 11:56'!updateWordingToMatchVocabulary	| labels |	labels _ OrderedCollection new.	self submorphs do: [:submorph |		submorph submorphs do: [:subsubmorph |			subsubmorph class == StringMorph ifTrue: [labels add: subsubmorph]]].	labels do: [:label | label knownName ifNotNilDo: [ :nm | label acceptValue: nm translated ]]! !!TileMorph methodsFor: 'as yet unclassified' stamp: 'nk 10/8/2004 15:11'!fixLayout	"Having just been loaded from a project, do repairs as needed."	self allMorphsDo: [:m | (m isKindOf: StringMorph)				ifTrue: [m font: Preferences standardEToysFont;						 fitContents].		(m respondsTo: #fixLayout)			ifTrue: [ m ~~ self ifTrue: [ m fixLayout ] ]			ifFalse: [ m layoutChanged ] ].	self layoutChanged; fullBounds! !!TileMorph methodsFor: 'as yet unclassified' stamp: 'nk 10/8/2004 15:14'!unhibernate	self fixLayout.! !!AssignmentTileMorph methodsFor: 'arrow' stamp: 'nk 10/8/2004 14:27'!addArrowsIfAppropriate	"If the receiver's slot is of an appropriate type, add arrows to the tile."	(Vocabulary vocabularyForType: dataType)		ifNotNilDo:			[:aVocab | aVocab wantsAssignmentTileVariants ifTrue:				[self addArrows]].	(assignmentSuffix = ':') ifTrue:		[ self addMorphBack: (ImageMorph new image: (ScriptingSystem formAtKey: #NewGets)).		(self findA: StringMorph) ifNotNilDo: [ :sm |			(sm contents endsWith: ' :') ifTrue: [ sm contents: (sm contents allButLast: 2) ]]]! !!AssignmentTileMorph methodsFor: 'as yet unclassified' stamp: 'nk 10/8/2004 15:05'!fixLayout	super fixLayout.	self updateLiteralLabel; updateWordingToMatchVocabulary; layoutChanged; fullBounds! !