'From Squeak2.9BJ of 22 September 2000 [latest update: #2991] on 4 December 2000 at 11:09:21 am'!"Change Set:		Repainting-arDate:			4 December 2000Author:			Andreas RaabHide sketches during repaint."!!SketchMorph methodsFor: 'menu' stamp: 'ar 12/4/2000 10:45'!editDrawingIn: aPasteUpMorph forBackground: forBackground	| w bnds sketchEditor pal aPaintTab aWorld aPaintBox tfx |	self world assureNotPaintingElse: [^ self].	w _ aPasteUpMorph world.	w stopRunningAll; abandonAllHalos.	w displayWorld.	self visible: false.	forBackground		ifTrue:			[bnds _ aPasteUpMorph boundsInWorld]		ifFalse:			[bnds _ (self boundsInWorld expandBy: (60 @ 60)) intersect: self world bounds.			bnds _ (aPasteUpMorph paintingBoundsAround: bnds center) merge: bnds].	sketchEditor _ SketchEditorMorph new.	forBackground ifTrue: [sketchEditor setProperty: #background toValue: true].	w addMorphFront: sketchEditor.	sketchEditor initializeFor: self inBounds: bnds pasteUpMorph: aPasteUpMorph.	sketchEditor		afterNewPicDo: [:aForm :aRect |			self visible: true.			self form: aForm.			tfx _ aPasteUpMorph transformFrom: aPasteUpMorph world.			self topRendererOrSelf position: (tfx globalPointToLocal: aRect origin).			self rotationStyle: sketchEditor rotationStyle.			self forwardDirection: sketchEditor forwardDirection.			self presenter drawingJustCompleted: self.			forBackground ifTrue: [self goBehind]]  "shouldn't be necessary"		ifNoBits: ["If no bits drawn.  Must keep old pic.  Can't have no picture"			self visible: true.			aWorld _ self currentWorld.				"sometimes by now I'm no longer in a world myself, but we still need				 to get ahold of the world so that we can deal with the palette"			((pal _ aPasteUpMorph standardPalette) notNil and: [pal isInWorld])				ifTrue:					[(aPaintBox _ aWorld paintBox) ifNotNil: [aPaintBox delete].					pal viewMorph: self]				ifFalse:					[(aPaintTab _ aWorld paintingFlapTab)						ifNotNil:							[aPaintTab hideFlap]						ifNil:							[(aPaintBox _ aWorld paintBox) ifNotNil: [aPaintBox delete]]]].! !