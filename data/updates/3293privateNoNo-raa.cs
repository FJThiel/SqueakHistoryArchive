'From Squeak2.9alpha of 17 July 2000 [latest update: #3357] on 1 February 2001 at 6:22:26 pm'!"Change Set:		privateNoNoDate:			1 February 2001Author:			Bob ArningFix bug in keeping a painting. Someone was breaking the owner<->submorph invariant and got caught. Shame!!"!!PasteUpMorph methodsFor: 'world menu' stamp: 'RAA 2/1/2001 18:21'!makeNewDrawing: evt	| w newSketch newPlayer sketchEditor aPaintBox aPalette tfx whereToPresent rect ownerBeforeHack |	w _ self world.	w assureNotPaintingElse: [^ self].	rect _ self paintingBoundsAround: evt position.	aPalette _ self standardPalette.	aPalette ifNotNil: [aPalette showNoPalette; layoutChanged].	w stopRunningAll; abandonAllHalos.	newSketch _ self drawingClass new player: (newPlayer _ UnscriptedPlayer newUserInstance).	newPlayer costume: newSketch.	newSketch form: (Form extent: rect extent depth: w assuredCanvas depth).	newSketch bounds: rect.	sketchEditor _ SketchEditorMorph new.	w addMorphFront: sketchEditor.	sketchEditor initializeFor: newSketch inBounds: rect pasteUpMorph: self.	sketchEditor		afterNewPicDo: [:aForm :aRect |			whereToPresent _ self presenter.			newSketch form: aForm.			tfx _ self transformFrom: w.			newSketch position: (tfx globalPointToLocal: aRect origin).			newSketch rotationStyle: sketchEditor rotationStyle.			newSketch forwardDirection: sketchEditor forwardDirection.			ownerBeforeHack _ newSketch owner.	"about to break the invariant!!!!"			newSketch privateOwner: self. "temp for halo access"			newPlayer setHeading: sketchEditor forwardDirection.			"Includes  newSketch rotationDegrees: sketchEditor forwardDirection."			newSketch privateOwner: ownerBeforeHack. "probably nil, but let's be certain"			self addMorphFront: newPlayer costume.			w startSteppingSubmorphsOf: newSketch.			whereToPresent drawingJustCompleted: newSketch]		 ifNoBits:			[(aPaintBox _ w paintBox) ifNotNil:				[aPaintBox delete].			aPalette ifNotNil: [aPalette showNoPalette].]! !