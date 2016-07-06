'From Squeak2.9alpha of 17 July 2000 [latest update: #2684] on 22 September 2000 at 4:26:35 pm'!"Change Set:		transPSDate:			22 September 2000Author:			Bob Arning- provides the ability to print SketchMorphs (currently only non-rotated ones) with transparent areas on a PostScript area- also tweaks Color class>>colorFromPixelValue: to return Color transparent where appropriate in 16 and 32-bit depths (used to return Color black for transparent)"!!Color class methodsFor: 'instance creation' stamp: 'RAA 9/22/2000 15:19'!colorFromPixelValue: p depth: d	"Convert a pixel value for the given display depth into a color."	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."	| r g b alpha |	d = 8 ifTrue: [^ IndexedColors at: (p bitAnd: 16rFF) + 1].	d = 4 ifTrue: [^ IndexedColors at: (p bitAnd: 16r0F) + 1].	d = 2 ifTrue: [^ IndexedColors at: (p bitAnd: 16r03) + 1].	d = 1 ifTrue: [^ IndexedColors at: (p bitAnd: 16r01) + 1].	(d = 16) | (d = 15) ifTrue: [		"five bits per component"		r _ (p bitShift: -10) bitAnd: 16r1F.		g _ (p bitShift: -5) bitAnd: 16r1F.		b _ p bitAnd: 16r1F.		(r = 0 and: [g = 0 and: [b = 0]])  ifTrue: [^Color transparent].		^ Color r: r g: g b: b range: 31].	d = 32 ifTrue: [		"eight bits per component; 8 bits of alpha"		r _ (p bitShift: -16) bitAnd: 16rFF.		g _ (p bitShift: -8) bitAnd: 16rFF.		b _ p bitAnd: 16rFF.		alpha _ p bitShift: -24.		alpha = 0 ifTrue: [^Color transparent].		(r = 0 and: [g = 0 and: [b = 0]])  ifTrue: [^Color transparent].		alpha < 255			ifTrue: [^ (Color r: r g: g b: b range: 255) alpha: (alpha asFloat / 255.0)]			ifFalse: [^ (Color r: r g: g b: b range: 255)]].	d = 12 ifTrue: [		"four bits per component"		r _ (p bitShift: -8) bitAnd: 16rF.		g _ (p bitShift: -4) bitAnd: 16rF.		b _ p bitAnd: 16rF.		^ Color r: r g: g b: b range: 15].	d = 9 ifTrue: [		"three bits per component"		r _ (p bitShift: -6) bitAnd: 16r7.		g _ (p bitShift: -3) bitAnd: 16r7.		b _ p bitAnd: 16r7.		^ Color r: r g: g b: b range: 7].	self error: 'unknown pixel depth: ', d printString! !!GeePrinter methodsFor: 'as yet unclassified' stamp: 'RAA 9/22/2000 13:58'!drawOn: aCanvas	pasteUp drawOn: aCanvas! !!GeePrinter methodsFor: 'as yet unclassified' stamp: 'RAA 9/22/2000 14:28'!fullDrawOn: aCanvas	pasteUp fullDrawOn: aCanvas! !!SketchMorph methodsFor: 'drawing' stamp: 'RAA 9/22/2000 15:14'!drawPostscriptOn: aCanvas	| top f2 c2 tfx clrs |	tfx _ self transformFrom: self world.	tfx angle = 0.0 ifFalse: [^super drawPostscriptOn: aCanvas].	"can't do rotated yet"	clrs _ self rotatedForm colorsUsed.	(clrs includes: Color transparent) 		ifFalse: [^super drawPostscriptOn: aCanvas].		"no need for this, then""Smalltalk at: #Q put: OrderedCollection new""Q add: {self. tfx. clrs}.""(self hasProperty: #BOB) ifTrue: [self halt]."	top _ aCanvas topLevelMorph.	f2 _ Form extent: self extent depth: self rotatedForm depth.	c2 _ f2 getCanvas.	c2 fillColor: Color white.	c2 translateBy: bounds origin negated clippingTo: f2 boundingBox during: [ :c |		top fullDrawOn: c	].	aCanvas paintImage: f2 at: bounds origin! !