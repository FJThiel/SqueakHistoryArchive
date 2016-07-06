'From Squeak3.8alpha of ''17 July 2004'' [latest update: #5976] on 12 August 2004 at 7:18:31 pm'!"Change Set:		assignmentRescue-swDate:			12 August 2004Author:			Scott WallaceAttempts a rescue of the assignment-tile situation recently broken...(1)  The correct wordings occur again on the tiles, in English at least.(2)  An iconic left-arrow is used, thus circumventing the problem that the new fonts incorporated in the image don't have a left-arrow glyph for the underbar character."!!AssignmentTileMorph methodsFor: 'arrow' stamp: 'sw 8/12/2004 15:13'!addArrowsIfAppropriate	"If the receiver's slot is of an appropriate type, add arrows to the tile."	(Vocabulary vocabularyForType: dataType)		ifNotNilDo:			[:aVocab | aVocab wantsAssignmentTileVariants ifTrue:				[self addArrows]].	(assignmentSuffix = ':') ifTrue:		[self addMorphBack: (ImageMorph new image: (ScriptingSystem formAtKey: #NewGets))]! !!PhraseTileMorph methodsFor: 'initialization' stamp: 'sw 8/12/2004 18:58'!setAssignmentRoot: opSymbol type: opType rcvrType: rcvrType argType: argType vocabulary: aVocabulary	"Add submorphs to make me constitute a setter of the given symbol"	| anAssignmentTile |	resultType _ opType.	self color: (ScriptingSystem colorForType: opType).	self removeAllMorphs.	self addMorph: (TilePadMorph new setType: rcvrType).	anAssignmentTile _ AssignmentTileMorph new rawVocabulary: aVocabulary.	self addMorphBack: (anAssignmentTile typeColor: color).	anAssignmentTile setRoot: opSymbol asString dataType: argType.	anAssignmentTile setAssignmentSuffix: #:.	self addMorphBack: (TilePadMorph new setType: argType)! !!TextStyle class methodsFor: 'user interface' stamp: 'sw 8/12/2004 19:08'!importFontsFromStyleFiles	"Import any and all of the fonts found in the default directory in files named ComicBold.style, ComicPlain.style, NewYork.style, Palatino.style, Courier.style"	| aName |	#('ComicBold' 'ComicPlain' 'NewYork' 'Palatino' 'Courier') do:		[:frag |			(TextStyle knownTextStyles includes: frag) ifFalse:				[(FileDirectory default fileExists: (aName _ frag, '.style'))						ifTrue:							[TextStyle default collectionFromFileNamed: aName]]].! !!Vocabulary methodsFor: 'translation' stamp: 'sw 8/12/2004 19:04'!translatedWordingFor: aSymbol	"If I have a translated wording for aSymbol, return it, else return aSymbol.  Caveat: at present, this mechanism is only germane for *assignment-operator wordings*"	#(: Incr: Decr: Mult:) with: #('' 'increase by' 'decrease by' 'multiply by') do:		[:a :b | aSymbol == a ifTrue: [^ b translated]].	^ aSymbol translated! !"Postscript:"ScriptingSystem saveForm: (Form extent: 9@16 depth: 16	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 65536 0 0 0 1 65536 0 0 0 65537 0 0 0 1 65537 65537 65537 65536 65537 65537 65537 65537 0 1 65537 65537 65537 65536 0 65537 0 0 0 0 1 65536 0 0 0 0 65536 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)	offset: 0@0) atKey: #NewGets.Vocabulary initialize.!