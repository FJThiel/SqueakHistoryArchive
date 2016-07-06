'From Squeak 2.1 of June 30, 1998 on 1 July 1998 at 4:12:22 pm'!"Change Set:		EmbedInTextFixDate:			1 July 1998Author:			Dan Ingalls1.  Makes textAnchor offset work correctly within TransformMorphs.2.  Corrects a problem with emphasis being ignored following a textAnchor.3.  Amends = test for textAnchors to use == with the anchoredMorph."!!CharacterScanner methodsFor: 'scanning' stamp: 'di 7/1/1998 14:55'!placeEmbeddedObject: anchoredMorph	"Place the anchoredMorph or return false if it cannot be placed.	In any event, advance destX by its width."	destX _ destX + (width _ anchoredMorph width).	(destX > rightMargin and: [(leftMargin + width) <= rightMargin])		ifTrue: ["Won't fit, but would on next line"				^ false].	lastIndex _ lastIndex + 1.	self setFont.  "Force recalculation of emphasis fo rnext run"	^ true! !!DisplayScanner methodsFor: 'scanning' stamp: 'di 7/1/1998 16:03'!placeEmbeddedObject: anchoredMorph	(super placeEmbeddedObject: anchoredMorph) ifFalse: [^ false].	anchoredMorph isMorph 		ifTrue: [anchoredMorph position: ((destX - width)@lineY) - morphicOffset]		ifFalse: [destY _ lineY.				height _ anchoredMorph height.				runX _ destX.				anchoredMorph displayOn: destForm at: destX - width@destY].	^ true! !!TextAnchor methodsFor: 'all' stamp: 'di 7/1/1998 14:35'!= other 	^ (other class == self class) 		and: [other anchoredMorph == anchoredMorph]! !CharacterScanner removeSelector: #ifCharIn:at:fits:do:!CharacterScanner removeSelector: #setFontAt:!CharacterScanner removeSelector: #beginAt:!