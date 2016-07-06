'From Squeak 2.3 beta of Nov 25, 1998 on 5 January 1999 at 4:56:13 pm'!"Change Set:		tuesdayTinyDate:			5 January 1999Author:			Scott WallaceFixes a bug introduced to TabbedPalettes by recent BookMorph method #goToPageMorph:  transitionSpec:, by moving #updateCachedThumbnail up from PasteUpMorph to Morph. so that non-Pasteups don't stumble when sent this message.Makes it possible for a viewer to be looking at the 'playfield' group of phrases in a viewer even when the referent is ownerless (formerly fell into a debugger)."!!Morph methodsFor: 'e-toy support' stamp: 'sw 1/5/1999 16:30'!updateCachedThumbnail	"If I have a cached thumbnail, then update it.  Copied up from Dan's original version in PasteUpMorph so it can be used by all morphs."	| cachedThumbnail |	(cachedThumbnail _ self valueOfProperty: #cachedThumbnail) ifNotNil:		[cachedThumbnail computeThumbnail].! !!PasteUpMorph methodsFor: 'misc' stamp: 'sw 1/5/1999 16:07'!mouseX	^ self isInWorld		ifTrue:			[(self cursorPoint x) - self left]		ifFalse:			[0]! !!PasteUpMorph methodsFor: 'misc' stamp: 'sw 1/5/1999 16:55'!mouseY	^ self isInWorld		ifTrue:			[self bottom - (self cursorPoint y)]		ifFalse:			[0]! !PasteUpMorph removeSelector: #updateCachedThumbnail!