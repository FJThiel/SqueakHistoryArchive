'From Squeak2.9alpha of 17 July 2000 [latest update: #3171] on 13 December 2000 at 12:31:56 pm'!"Change Set:		evtRecStoryDate:			13 December 2000Author:			Bob Arning- provides subclasses of HandMorph with their own implementation of #needsToBeDrawn to avoid hiding the real cursor when drawing a secondary one- enhances DisplayScreen>>restoreMorphicDisplay to restore the cursor as well- removes an unused method from PasteUpMorph (I know there are more, but I ran into this one, so it's history)"!!DisplayScreen methodsFor: 'other' stamp: 'RAA 12/12/2000 15:50'!restoreMorphicDisplay	DisplayScreen startUp.	(self getOuterMorphicWorld ifNil: [^ self])		extent: self extent;		viewBox: self boundingBox;		handsDo: [:h | h visible: true; showTemporaryCursor: nil];		restoreFlapsDisplay;		fullRepaintNeeded.	WorldState addDeferredUIMessage: [		Cursor normal show.	].! !!HandMorphForReplay methodsFor: 'event dispatching' stamp: 'RAA 12/12/2000 14:45'!needsToBeDrawn	^true! !!RemoteControlledHandMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/12/2000 14:45'!needsToBeDrawn	^true! !!RemoteHandMorph methodsFor: 'event handling' stamp: 'RAA 12/12/2000 14:45'!needsToBeDrawn	^true! !PasteUpMorph removeSelector: #selectHandsToDrawForDamage:!