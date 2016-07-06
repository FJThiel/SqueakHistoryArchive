'From Squeak 2.2 of Sept 23, 1998 on 3 October 1998 at 12:21:48 am'!"Change Set:		FlexBoundsFix-diDate:			3 October 1998Author:			Dan IngallsFlexed bounds required an extra pixel on the right and bottom to cover subsequent truncation of floats."!!MorphicTransform methodsFor: 'transformations' stamp: 'di 10/3/1998 00:18'!invertBoundsRect: aRectangle	"Return a rectangle whose coordinates have been transformed	from local back to global coordinates.  NOTE: if the transformation	is not just a translation, then it will compute the bounding box	in global coordinates."	| outerRect |	self isPureTranslation	ifTrue:		[^ (self invert: aRectangle topLeft)			corner: (self invert: aRectangle bottomRight)]	ifFalse:		[outerRect _ Rectangle encompassing:			(aRectangle innerCorners collect: [:p | self invert: p]).		"Following asymmetry due to likely subsequent truncation"		^ outerRect topLeft - (1@1) corner: outerRect bottomRight + (2@2)]! !!MorphicTransform methodsFor: 'transformations' stamp: 'di 10/3/1998 00:18'!transformBoundsRect: aRectangle	"Return a rectangle whose coordinates have been transformed	from global to local coordinates.  NOTE: if the transformation	is not just a translation, then it will compute the bounding box	in global coordinates."	| outerRect |	self isPureTranslation	ifTrue:		[^ (self transform: aRectangle topLeft)			corner: (self transform: aRectangle bottomRight)]	ifFalse:		[outerRect _ Rectangle encompassing:			(aRectangle innerCorners collect: [:p | self transform: p]).		"Following asymmetry due to likely subsequent truncation"		^ outerRect topLeft - (1@1) corner: outerRect bottomRight + (2@2)]! !