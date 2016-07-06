'From Squeak3.1alpha of 28 February 2001 [latest update: #4018] on 14 May 2001 at 12:19:50 am'!"Change Set:		RepaintFix-arDate:			14 May 2001Author:			Andreas RaabFixes a problem when repainting an object partially out of the world's boundaries (in which case the painting was truncated there)."!!SketchMorph methodsFor: 'menu' stamp: 'ar 5/14/2001 00:16'!editDrawingIn: aPasteUpMorph forBackground: forBackground	| w bnds sketchEditor pal aPaintTab aWorld aPaintBox tfx |	self world assureNotPaintingElse: [^ self].	w _ aPasteUpMorph world.	w stopRunningAll; abandonAllHalos.	w displayWorld.	self visible: false.	forBackground		ifTrue:			[bnds _ aPasteUpMorph boundsInWorld]		ifFalse:			[bnds _ (self boundsInWorld expandBy: (60 @ 60)).			bnds _ (aPasteUpMorph paintingBoundsAround: bnds center) merge: bnds].	sketchEditor _ SketchEditorMorph new.	forBackground ifTrue: [sketchEditor setProperty: #background toValue: true].	w addMorphFront: sketchEditor.	sketchEditor initializeFor: self inBounds: bnds pasteUpMorph: aPasteUpMorph.	sketchEditor		afterNewPicDo: [:aForm :aRect |			self visible: true.			self form: aForm.			tfx _ aPasteUpMorph transformFrom: aPasteUpMorph world.			self topRendererOrSelf position: (tfx globalPointToLocal: aRect origin).			self rotationStyle: sketchEditor rotationStyle.			self forwardDirection: sketchEditor forwardDirection.			(aPaintTab _ (aWorld _ self world) paintingFlapTab)				ifNotNil:[aPaintTab hideFlap]				ifNil:[(aPaintBox _ aWorld paintBox) ifNotNil:[aPaintBox delete]].			self presenter drawingJustCompleted: self.			forBackground ifTrue: [self goBehind]]  "shouldn't be necessary"		ifNoBits: ["If no bits drawn.  Must keep old pic.  Can't have no picture"			self visible: true.			aWorld _ self currentWorld.				"sometimes by now I'm no longer in a world myself, but we still need				 to get ahold of the world so that we can deal with the palette"			((pal _ aPasteUpMorph standardPalette) notNil and: [pal isInWorld])				ifTrue:					[(aPaintBox _ aWorld paintBox) ifNotNil: [aPaintBox delete].					pal viewMorph: self]				ifFalse:[					(aPaintTab _ (aWorld _ self world) paintingFlapTab)						ifNotNil:[aPaintTab hideFlap]						ifNil:[(aPaintBox _ aWorld paintBox) ifNotNil:[aPaintBox delete]]]]! !