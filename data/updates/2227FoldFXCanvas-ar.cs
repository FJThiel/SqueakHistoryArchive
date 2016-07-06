'From Squeak2.8alpha of 16 February 2000 [latest update: #2226] on 28 May 2000 at 3:05:46 pm'!"Change Set:		FoldFXCanvasDate:			28 May 2000Author:			Andreas RaabThe change set folds FXFormCanvas with FormCanvas. This is mainly to make BalloonCanvas work since it's a subclass of FormCanvas -- which in fact it shouldn't be. It would be much better to have BalloonCanvas forward drawing operations to the actual form canvas but that's the way it is right now. The good side of it is that we're getting rid of a class as well as some duplicated code.The change also fixes a couple of small problems and reclassifies FXBlt under a more appropriate category."!!B3DHardwareRasterizer class methodsFor: 'testing' stamp: 'ar 5/28/2000 15:04'!isAvailableFor: aForm	"Return true if this part of the engine is available for the given output medium"	aForm ifNil:[^false].	(aForm isDisplayScreen and:[aForm isB3DDisplayScreen]) ifFalse:[^false].	^self isAvailable! !!BalloonEngine methodsFor: 'accessing' stamp: 'ar 5/28/2000 15:02'!bitBlt: aBitBlt	bitBlt _ aBitBlt.	bitBlt isNil ifTrue:[^self].	self class primitiveSetBitBltPlugin: bitBlt getPluginName.	self clipRect: bitBlt clipRect.	bitBlt 		sourceForm: (Form extent: span size @ 1 depth: 32 bits: span);		sourceRect: (0@0 extent: 1@span size);		colorMap: (Color colorMapIfNeededFrom: 32 to: bitBlt destForm depth);		combinationRule: (bitBlt destForm depth >= 8 ifTrue:[34] ifFalse:[Form paint]).! !!Form methodsFor: 'testing' stamp: 'ar 5/28/2000 14:58'!isBltAccelerated: ruleInteger for: sourceForm	"Return true if the receiver can perform accelerated blts operations by itself"	^false! !!Form methodsFor: 'testing' stamp: 'ar 5/28/2000 15:04'!isDisplayScreen	^false! !!Form methodsFor: 'testing' stamp: 'ar 5/28/2000 14:58'!isFillAccelerated: ruleInteger for: aColor	"Return true if the receiver can perform accelerated fill operations by itself"	^false! !!Form methodsFor: 'testing' stamp: 'ar 5/28/2000 14:58'!shouldPreserveContents	"Return true if the receiver should preserve it's contents when flagged to be clean. Most forms can not be trivially restored by some drawing operation but some may."	^true! !!FormCanvas methodsFor: 'accessing' stamp: 'ar 5/28/2000 14:42'!contentsOfArea: aRectangle into: aForm	self flush.	port contentsOfArea: ((aRectangle origin + origin) negated extent: aRectangle extent)		into: aForm.	^aForm! !!FormCanvas methodsFor: 'private' stamp: 'ar 5/28/2000 14:52'!portClass	"Return the class used as port"	^Display defaultBitBltClass asGrafPort! !!FormCanvas methodsFor: 'private' stamp: 'ar 5/28/2000 14:43'!setClearColor: aColor	"Install a new clear color - e.g., a color is used for clearing the background"	| clearColor |	port isFXBlt ifTrue:[port sourceMap: nil; destMap: nil; colorMap: nil; sourceKey: nil].	clearColor _ aColor ifNil:[Color transparent].	clearColor isColor ifFalse:[		(clearColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].		^port fillPattern: clearColor; combinationRule: Form over].	"Okay, so clearColor really *is* a color"	port sourceForm: nil.	port combinationRule: Form over.	port fillPattern: clearColor.	self depth = 8 ifTrue:[		"Use a stipple pattern"		port fillColor: (clearColor balancedPatternForDepth: 8)].! !!FormCanvas methodsFor: 'private' stamp: 'ar 5/28/2000 14:45'!setFillColor: aColor	"Install a new color used for filling."	| screen patternWord fillColor |	port isFXBlt ifTrue:[port sourceMap: nil; destMap: nil; colorMap: nil; sourceKey: nil].	fillColor _ self shadowColor ifNil:[aColor].	fillColor ifNil:[fillColor _ Color transparent].	fillColor isColor ifFalse:[		(fillColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].		^port fillPattern: fillColor; combinationRule: Form over].	"Okay, so fillColor really *is* a color"	port sourceForm: nil.	fillColor isTranslucent ifFalse:[		port combinationRule: Form over.		port fillPattern: fillColor.		self depth = 8 ifTrue:[			"In 8 bit depth it's usually a good idea to use a stipple pattern"			port fillColor: (fillColor balancedPatternForDepth: 8)].		^self].	"fillColor is some translucent color"	(port isFXBlt and:[self depth >= 8]) ifTrue:[		"FXBlt setup for full alpha mapped transfer"		port fillColor: (fillColor bitPatternForDepth: 32).		port destMap: form colormapToARGB.		port colorMap: form colormapFromARGB.		^port combinationRule: Form blend].	self depth > 8 ifTrue:[		"BitBlt setup for alpha masked transfer"		port fillPattern: fillColor.		self depth = 16			ifTrue:[port alphaBits: fillColor privateAlpha; combinationRule: 30]			ifFalse:[port combinationRule: Form blend].		^self].	"Can't represent actual transparency -- use stipple pattern"	screen _ Color translucentMaskFor: fillColor alpha depth: self depth.	patternWord _ fillColor pixelWordForDepth: self depth.	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).	port combinationRule: Form paint.! !!FormCanvas methodsFor: 'private' stamp: 'ar 5/28/2000 14:49'!setPaintColor: aColor	"Install a new color used for filling."	| paintColor screen patternWord |	port isFXBlt ifTrue:[port sourceMap: nil; destMap: nil; colorMap: nil; sourceKey: nil].	paintColor _ self shadowColor ifNil:[aColor].	paintColor ifNil:[paintColor _ Color transparent].	paintColor isColor ifFalse:[		(paintColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].		^port fillPattern: paintColor; combinationRule: Form paint].	"Okay, so paintColor really *is* a color"	port sourceForm: nil.	(paintColor isTranslucent) ifFalse:[		port fillPattern: paintColor.		port combinationRule: Form paint.		self depth = 8 ifTrue:[			port fillColor: (paintColor balancedPatternForDepth: 8)].		^self].	"paintColor is translucent color"	(port isFXBlt and:[self depth >= 8]) ifTrue:[		"FXBlt setup for alpha mapped transfer"		port fillPattern: paintColor.		port fillColor: (paintColor bitPatternForDepth: 32).		port destMap: form colormapToARGB.		port colorMap: form colormapFromARGB.		port combinationRule: Form blend.		^self].	self depth > 8 ifTrue:[		"BitBlt setup for alpha mapped transfer"		port fillPattern: paintColor.		self depth = 16			ifTrue:[port alphaBits: paintColor privateAlpha; combinationRule: 31]			ifFalse:[port combinationRule: Form blend].		^self].	"Can't represent actual transparency -- use stipple pattern"	screen _ Color translucentMaskFor: paintColor alpha depth: self depth.	patternWord _ paintColor pixelWordForDepth: self depth.	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).	port combinationRule: Form paint! !!FormCanvas methodsFor: 'private' stamp: 'ar 5/28/2000 14:50'!setStencilColor: aColor form: sourceForm	"Install a new color used for stenciling through FXBlt.	Stenciling in general is done mapping all colors of source form	to the stencil color and installing the appropriate source key.	However, due to possible transparency we may have to install the	color map as source map so that sourceForm gets mapped to a 32bit	ARGB pixel value before the color combination is done. If we don't	need translucency we can just use the regular color map (faster!!)"	| stencilColor screen patternWord |	port isFXBlt ifFalse:[^self]. "Not appropriate for BitBlt"	port sourceMap: nil; destMap: nil; colorMap: nil; sourceKey: nil.	stencilColor _ self shadowColor ifNil:[aColor].	stencilColor isColor ifFalse:[^self]. "No way"	(stencilColor isTranslucent) ifFalse:[		"If the paint color is not translucent we can use a simpler		transformation going through a single color map."		port sourceKey: 0. "The transparent source key"		port fillPattern: stencilColor.		port colorMap: (ColorMap colors: port fillColor).		port fillColor: nil.		^port combinationRule: Form over].	(self depth >= 8) ifTrue:[		"For transparent stenciling, things are more complicated.		We need to install the transparent stencil color as source map		so that all colors are mapped to the stencil color and afterwards		blended with the destination."		port sourceKey: 0. "The transparent source key"		port fillPattern: stencilColor.		port destMap: form colormapToARGB.		port colorMap: form colormapFromARGB.		port sourceMap: (ColorMap colors: (stencilColor bitPatternForDepth: 32)).		port fillColor: nil.		port combinationRule: Form blend.		^self].	"Translucent stenciling in < 8bit depth requires three parts,	a color map, a fill pattern and the appropriate combination rule."	port colorMap: (ColorMap colors: (Color maskingMap: form depth)).	screen _ Color translucentMaskFor: stencilColor alpha depth: self depth.	patternWord _ stencilColor pixelWordForDepth: self depth.	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).	port combinationRule: Form paint! !!FormCanvas methodsFor: 'drawing-images' stamp: 'ar 5/28/2000 14:39'!image: aForm at: aPoint sourceRect: sourceRect rule: rule 	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."	port isFXBlt 		ifTrue:[port sourceKey: nil; sourceMap: nil; destMap: nil;					colorMap: (aForm colormapIfNeededFor: form); fillColor: nil]		ifFalse:[port colorMap: (aForm colormapIfNeededForDepth: form depth); fillColor: nil].	port image: aForm at: aPoint + origin sourceRect: sourceRect rule: rule.! !!FormCanvas methodsFor: 'drawing-images' stamp: 'ar 5/28/2000 14:40'!stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"	port isFXBlt "FXBlt has a very different setup"		ifTrue:[self setStencilColor: aColor form: stencilForm]		ifFalse:[self setPaintColor: aColor.				port colorMap: (Color maskingMap: stencilForm depth)].	port stencil: stencilForm		at: aPoint + origin		sourceRect: sourceRect.! !!GrafPort methodsFor: 'accessing' stamp: 'ar 5/28/2000 14:41'!contentsOfArea: aRectangle into: aForm	destForm 		displayOn: aForm 		at:  aRectangle origin		clippingBox: (0@0 extent: aRectangle extent).	^aForm! !ExternalScreen removeSelector: #defaultCanvasClass!Smalltalk removeClassNamed: #FXFormCanvas!"Postscript:Rename FXBlts category."SystemOrganization renameCategory: 'Experimental-FXBlt' toBe:'Graphics-FXBlt'.!