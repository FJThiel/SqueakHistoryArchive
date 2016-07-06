'From Squeak3.1alpha of 28 February 2001 [latest update: #4289] on 24 August 2001 at 12:00:47 am'!"Change Set:		SoundSliderTweaks-arDate:			23 August 2001Author:			Andreas RaabMake the volume control slider reflect the currently set volume. BTW, the volume control is part of the common sound support primitives (and not Mac only) so rename it from setMacVolume to setSoundVolume (setting the Mac volume could have interesting implications if you get my meaning ... ;-)"!!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'ar 8/23/2001 23:52'!getSoundVolume	^SoundPlayer soundVolume average! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'ar 8/23/2001 23:49'!setSoundVolume: x	SoundPlayer setVolumeLeft: x volumeRight: x.! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'ar 8/23/2001 23:57'!soundDownEvt: a morph: b	soundSlider ifNotNil: [soundSlider delete].	(soundSlider _ RectangleMorph new)		setProperty: #morphicLayerNumber toValue: 1;		extent: b width @ (b width * 3);		color: self colorForButtons;		borderColor: #raised;		bottomLeft: b boundsInWorld origin.	soundSlider addMorph: (		RectangleMorph new			color: self colorForButtons;			borderColor: #raised;			extent: b width @ 8;			center: soundSlider center x @ 				(soundSlider bottom - (soundSlider height * self getSoundVolume) asInteger)	).	soundSlider openInWorld.! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'ar 8/23/2001 23:49'!soundStillDownEvt: evt morph: b	| y pct |	soundSlider ifNil: [^self].	y _ evt hand position y.	(y between: soundSlider top and: soundSlider bottom) ifTrue: [		pct _ (soundSlider bottom - y) asFloat / soundSlider height.		self setSoundVolume: pct.		soundSlider firstSubmorph top: y - 5.	]. ! !ProjectNavigationMorph removeSelector: #seSoundVolume:!ProjectNavigationMorph removeSelector: #setMacVolume:!