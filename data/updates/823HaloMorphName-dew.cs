'From Squeak 2.3 of January 14, 1999 on 12 April 1999 at 10:13:12 pm'!"Change Set:		HaloMorphName-dewDate:			12 April 1999Author:			Doug WayAdds a white background behind the name of the morph in its halo, so that it is more easily readable.  (The string can otherwise be difficult to read if it appears over other text.)"!!HaloMorph methodsFor: 'private' stamp: 'dew 4/12/1999 21:48'!addNameBeneath: outerRectangle string: aString	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."	| nameMorph nameBackgroundMorph |	nameMorph _ UpdatingStringMorph contents: aString.	nameMorph useStringFormat; target: innerTarget; putSelector: #renameTo:.	nameMorph position: outerRectangle bottomCenter - (((nameMorph width + 6) // 2) @ 8 negated).	nameMorph balloonTextSelector: #objectNameInHalo.	nameBackgroundMorph _ RectangleMorph newBounds: (nameMorph bounds expandBy: 1).	nameBackgroundMorph color: Color white; borderWidth: 0.	self addMorph: nameBackgroundMorph.	self addMorph: nameMorph.	^ nameMorph! !