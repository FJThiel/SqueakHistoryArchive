'From Squeak 2.2 of Sept 23, 1998 on 3 October 1998 at 12:11:10 am'!"Change Set:		BWMorphic-diDate:			3 October 1998Author:			Dan IngallsA number of fixes that make Morphic work reasonably in B&W, to wit...Made handMorph>>restoreSavedPatch use opaque white in B&W.Made flexes do a prefatory erasure of the (warped) shape of the submorphs."!!Canvas methodsFor: 'drawing' stamp: 'di 10/3/1998 00:01'!image: aForm at: aPoint	"Draw the given Form, which is assumed to be a Form or ColorForm following the convention that zero is the transparent pixel value."	self image: aForm		at: aPoint		sourceRect: (0@0 extent: aForm extent)		rule: Form paint.! !!Canvas methodsFor: 'drawing' stamp: 'di 10/2/1998 21:39'!imageWithOpaqueWhite: aForm at: aPoint	"Draw the given Form, which is assumed to be a Form or ColorForm but with the convention that zero represents opaque white."	self image: aForm		at: aPoint		sourceRect: (0@0 extent: aForm extent)		rule: Form over.! !!Canvas methodsFor: 'private' stamp: 'di 10/2/1998 22:28'!shadowDrawing	^ shadowDrawing! !!FormCanvas methodsFor: 'private' stamp: 'di 10/2/1998 22:27'!stipple	^ shadowStipple! !!HandMorph methodsFor: 'drawing' stamp: 'di 10/2/1998 21:53'!fullDrawOn: aCanvas	"A HandMorph has unusual drawing requirements:		1. the hand itself (i.e., the cursor) appears in front of its submorphs		2. morphs being held by the hand cast a shadow on the world/morphs below	The illusion is that the hand plucks up morphs and carries them above the world."	"Note: This version caches an image of the morphs being held by the hand for	 better performance. This cache is invalidated if one of those morphs changes."	| disableCaching myBnds shadowCanvas |	self suppressDisplay ifTrue: [^ self].	disableCaching _ false.	disableCaching ifTrue: [self nonCachingFullDrawOn: aCanvas. ^ self].	submorphs isEmpty ifTrue: [		cacheCanvas _ nil.		^ self drawOn: aCanvas].  "just draw the hand itself"	myBnds _ super fullBounds.  "my full bounds without my shadow"	self updateCacheCanvasDepth: aCanvas depth.	(cacheCanvas == nil or: [aCanvas depth = 1]) ifTrue:		["could not use caching due to translucency; do full draw"		self nonCachingFullDrawOn: aCanvas. ^ self].	"draw the shadow"	shadowCanvas _ aCanvas copyForShadowDrawingOffset: self shadowOffset.	"Note: it's 3x faster to fill a rectangle rather than draw the shadow of a Form"	cachedCanvasHasHoles		ifTrue: [shadowCanvas image: cacheCanvas form at: myBnds origin]		ifFalse: [shadowCanvas fillRectangle: myBnds color: color].	"draw morphs in front of the shadow using the cached Form"	aCanvas image: cacheCanvas form at: myBnds origin.	self drawOn: aCanvas.  "draw the hand itself in front of morphs"! !!HandMorph methodsFor: 'drawing' stamp: 'di 10/2/1998 23:57'!restoreSavedPatchOn: aCanvas	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."	hasChanged _ false.	savedPatch ifNotNil: [		aCanvas depth = 1			ifTrue: [aCanvas imageWithOpaqueWhite: savedPatch at: savedPatch offset]			ifFalse: [aCanvas image: savedPatch at: savedPatch offset].		((userInitials size = 0) and:		 [(submorphs size = 0) and:		 [temporaryCursor == nil]]) ifTrue: [			"Make the transition to using hardware cursor. Clear savedPatch and			 report one final damage rectangle to erase the image of the software cursor."			super invalidRect: (savedPatch offset extent: savedPatch extent + self shadowOffset).			Sensor currentCursor == Cursor normal ifFalse: [Cursor normal show].  "show hardware cursor"			savedPatch _ nil]].! !!TransformMorph methodsFor: 'drawing' stamp: 'di 10/3/1998 00:10'!fullDrawOn: aCanvas	"Overridden to clip submorph drawing to my bounds,	and to translate, rotate and scale as appropriate."	| clippingCanvas sourceQuad warp innerRect patchRect subCanvas offsetCanvas start |	(aCanvas isVisible: self bounds) ifFalse: [^ self].	self drawOn: aCanvas.	self hasSubmorphs ifFalse: [^ self].	transform isPureTranslation		ifTrue:		[clippingCanvas _ aCanvas copyOffset: transform offset negated truncated									clipRect: self innerBounds.		submorphs reverseDo: [:m | m fullDrawOn: clippingCanvas]]		ifFalse:		["Prepare an appropriate warp from patch to innerRect"		innerRect _ self innerBounds.		patchRect _ transform transformBoundsRect:						(aCanvas clipRect intersect: innerRect).		sourceQuad _ (transform sourceQuadFor: innerRect)						collect: [:p | p - patchRect topLeft].		warp _ aCanvas warpFrom: sourceQuad toRect: innerRect.		warp cellSize: smoothing.  "installs a colormap if smoothing > 1"		"Render the submorphs visible in the clipping rectangle, as patchForm"		start _ (aCanvas depth = 1 and: [aCanvas shadowDrawing not])			"If this is true B&W, then we need a first pass for erasure."			ifTrue: [1] ifFalse: [2].		start to: 2 do:			[:i | "If i=1 we first make a shadow and erase it for opaque whites in B&W"			subCanvas _ FormCanvas extent: patchRect extent depth: aCanvas depth.			i=1	ifTrue: [subCanvas setShadowDrawing; stipple: Color black.						warp combinationRule: Form erase]				ifFalse: [aCanvas shadowDrawing ifTrue:						[subCanvas setShadowDrawing; stipple: aCanvas stipple].						warp combinationRule: Form paint].			offsetCanvas _ subCanvas copyOffset: patchRect topLeft negated.			submorphs reverseDo: [:m | m fullDrawOn: offsetCanvas].			warp sourceForm: subCanvas form; warpBits.			warp sourceForm: nil.  subCanvas _ nil "release space for next loop"]]! !