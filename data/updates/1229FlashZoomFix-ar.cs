'From Squeak 2.4b of April 23, 1999 on 5 June 1999 at 12:29:06 am'!"Change Set:		FlashZoomFix-arDate:			5 June 1999Author:			Andreas RaabFixes transitions defined for 'leaving' a project."!!FlashPlayerMorph methodsFor: 'drawing' stamp: 'ar 6/5/1999 00:10'!drawOn: aCanvas 	"Draw the background of the player"	| box bgImage |	box _ self bounds.	bgImage _ self valueOfProperty: #transitionBackground ifAbsent:[nil].	bgImage 		ifNil:[aCanvas fillRectangle: box color: color]		ifNotNil:[aCanvas image: bgImage at: box origin sourceRect: bgImage boundingBox rule: Form over].! !!FlashPlayerMorph methodsFor: 'project transition' stamp: 'ar 6/5/1999 00:17'!playProjectTransitionFrom: oldProject to: newProject entering: aBoolean	"Play the transition from the old to the new project."	World ifNil:[^self]. "Not in MVC"	self stopPlaying.	owner ifNotNil:[		self stopStepping.		owner privateRemoveMorph: self.		owner _ nil].	aBoolean ifTrue:[		self updateProjectFillsFrom: newProject.	] ifFalse:[		self updateProjectFillsFrom: oldProject.		self setProperty: #transitionBackground toValue: newProject imageForm.	].	self frameNumber: 1.	self loopFrames: false.	(self valueOfProperty: #fullScreenTransition ifAbsent:[false])		ifTrue:[self bounds: World bounds].	World addMorphFront: self.	self startStepping.	self startPlaying.	[playing] whileTrue:[World doOneCycleNow].	self stopPlaying.	self stopStepping.	owner privateRemoveMorph: self.	owner _ nil.	self removeProperty: #transitionBackground.	Display deferUpdates: true.	World fullDrawOn: (FormCanvas on: Display).	Display deferUpdates: false.! !!Project methodsFor: 'displaying' stamp: 'ar 6/5/1999 00:11'!displayZoom: entering	"Show the project transition when entering a new project"	| newDisplay vanishingPoint |	"Play the flash transition if any."	self projectParameters at: #flashTransition ifPresent:[:dict|		dict at: CurrentProject ifPresent:[:player| ^player playProjectTransitionFrom: CurrentProject to: self entering: entering]].	"Show animated zoom to new display"	newDisplay _ self imageForm.	entering		ifTrue: [vanishingPoint _ Sensor cursorPoint]		ifFalse: [vanishingPoint _ self viewLocFor: CurrentProject].	Display zoomIn: entering orOutTo: newDisplay at: 0@0			vanishingPoint: vanishingPoint.	Display copyFrom: newDisplay.! !FlashPlayerMorph removeSelector: #playProjectEntering:!