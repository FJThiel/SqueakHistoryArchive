'From Squeak3.7alpha of 11 September 2003 [latest update: #5816] on 31 March 2004 at 1:10:10 pm'!"Change Set:		EyedropperEventFix-nkDate:			31 March 2004Author:			Ned KonzWith the changes to the EventSensor, the eyedropper in the PaintBoxMorph would cause the system to hang. Even with the MVCSensorFixes-nk changeset loaded, the Hand position was not getting updated while the mouse button was down.This changeset fixes both problems, and makes the colors update in realtime.I'm not sure that this is what's desired, though; it could slow down response."!!PaintBoxMorph methodsFor: 'actions' stamp: 'nk 3/31/2004 13:06'!eyedropper: aButton action: aSelector cursor: aCursor evt: evt 	"Take total control and pick up a color!!!!"	| pt feedbackColor |	aButton state: #on.	tool ifNotNil: [tool state: #off].	currentCursor := aCursor.	evt hand showTemporaryCursor: currentCursor		hotSpotOffset: 6 negated @ 4 negated.	"<<<< the form was changed a bit??"	feedbackColor := Display colorAt: Sensor cursorPoint.	colorMemory align: colorMemory bounds topRight		with: colorMemoryThin bounds topRight.	self addMorphFront: colorMemory.	"Full color picker"	[Sensor anyButtonPressed] whileFalse: 			[pt := Sensor cursorPoint.			"deal with the fact that 32 bit displays may have garbage in the 			alpha bits"			feedbackColor := Display depth = 32 						ifTrue: 							[Color colorFromPixelValue: ((Display pixelValueAt: pt) bitOr: 4278190080)								depth: 32]						ifFalse: [Display colorAt: pt].			"the hand needs to be drawn"			evt hand position: pt.			currentColor ~= feedbackColor ifTrue: [				currentColor _ feedbackColor.				self showColor ].			self world displayWorldSafely].	"Now wait for the button to be released."	[Sensor anyButtonPressed] whileTrue:		[ pt := Sensor cursorPoint.		"the hand needs to be drawn"		evt hand position: pt.		self world displayWorldSafely ].	evt hand showTemporaryCursor: nil hotSpotOffset: 0 @ 0.	self currentColor: feedbackColor evt: evt.	colorMemory delete.	tool ifNotNil: 			[tool state: #on.			currentCursor := tool arguments third].	aButton state: #off! !