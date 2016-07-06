'From Squeak3.1alpha of 28 February 2001 [latest update: #4152] on 13 June 2001 at 3:45:02 pm'!"Change Set:		flashHolder2Date:			13 June 2001Author:			Michael RuegerFixes some problems with the first version of the flash holder:- rendered images are no longer added to the player's costume list- the player's scaling and rotation now works"!!Morph methodsFor: 'player' stamp: 'mir 6/13/2001 14:45'!shouldRememberCostumes	^true! !!Morph methodsFor: 'e-toy support' stamp: 'mir 6/13/2001 14:34'!asWearableCostumeOfExtent: extent	"Return a wearable costume for some player"	^self asWearableCostume! !!FlashPlayerMorph methodsFor: 'holder' stamp: 'mir 6/13/2001 14:42'!asWearableCostumeOfExtent: extent	"Return a wearable costume for some player"	| image oldExtent |	oldExtent _ self extent.	self extent: extent.	image _ self imageForm.	self extent: oldExtent.	image mapColor: self color to: Color transparent.	^(SketchMorph withForm: image) copyCostumeStateFrom: self! !!FlashPlayerMorph methodsFor: 'holder' stamp: 'mir 6/13/2001 14:45'!shouldRememberCostumes	^false! !!Player methodsFor: 'costume' stamp: 'mir 6/13/2001 15:28'!renderedCostume: aMorph	"Make aMorph be the receiver's rendered costume; if flexing is currently in effect, make the new morph be flexed correspondingly"	self renderedCostume: aMorph remember: true! !!Player methodsFor: 'costume' stamp: 'mir 6/13/2001 15:28'!renderedCostume: aMorph remember: rememberCostume	"Make aMorph be the receiver's rendered costume; if flexing is currently in effect, make the new morph be flexed correspondingly"	| renderedMorph known anEventHandler w |	renderedMorph _ costume renderedMorph.	renderedMorph == aMorph ifTrue: [^ self].	rememberCostume		ifTrue: [self rememberCostume: renderedMorph].	renderedMorph changed.	w _ renderedMorph world.	"Copy 'player state' (e.g., state which should be associated with the player but is stored in the morph itself these days) from the old rendered morph the new morph."	aMorph rotationStyle: renderedMorph rotationStyle.	aMorph forwardDirection: renderedMorph forwardDirection.	"Note: referencePosition is *not* state but #moveTo: behavior"	aMorph referencePosition: renderedMorph referencePosition.	anEventHandler _ renderedMorph eventHandler.	costume isFlexMorph		ifTrue:			[costume adjustAfter:				[costume replaceSubmorph: renderedMorph by: aMorph]]		ifFalse:			[costume owner ifNotNil: [costume owner replaceSubmorph: costume by: aMorph].			aMorph player: self.			aMorph actorState: costume actorState.			(known _ costume knownName) ifNotNil:				[aMorph setNameTo: known].			costume _ aMorph.			w ifNotNil:				[w stopStepping: renderedMorph.				w startStepping: aMorph]].	aMorph eventHandler: anEventHandler.	aMorph changed! !!Player methodsFor: 'costume' stamp: 'mir 6/13/2001 15:29'!wearCostumeOf: anotherPlayer	"Put on a costume similar to the one currently worn by anotherPlayer"	self renderedCostume: (anotherPlayer costume renderedMorph asWearableCostumeOfExtent: self costume extent) remember: anotherPlayer costume shouldRememberCostumes! !