'From Squeak3.1alpha of 12 February 2001 [latest update: #4164] on 25 June 2001 at 12:30:34 pm'!"Change Set:		WnldClick-bfDate:			25 June 2001Author:			Bert FreudenbergMakes sure camera controls exist in 1st-person mode."!!WonderlandCameraMorph methodsFor: 'event handling' stamp: 'bf 6/25/2001 12:24'!mouseDown: evt 	"When the user clicks in a camera window, determine which actor the    	         user clicked on and have that actor respond to the event"	| newEvent reactions |	mode == #view ifTrue:[mode _ nil].	"ar 5/28/2001 - remove the above at some point; it's compensating for old wonderlands"	self on: #mouseMove send: #mouseMoveDefault: to: self.	self on: #mouseUp send: #mouseUpDefault: to: self.	(evt redButtonPressed and:[evt anyModifierKeyPressed not]) ifTrue:[		(mode == nil and:[firstPersonControls == true]) ifTrue:[			self getCameraControls. 		"create if necessary"			myControls setCenter: evt position.			myControls mouseDown: evt.			evt hand needsToBeDrawn ifFalse:[Cursor crossHair show].			self on: #mouseMove send: #mouseMoveFirstPersonControl: to: self.			self on: #mouseUp send: #mouseUpFirstPersonControl: to: self.			^self		].		mode == #stroke ifTrue:[			self on: #mouseMove send: #mouseMoveStroke: to: self.			self on: #mouseUp send: #mouseUpStroke: to: self.			^self recordStroke: evt cursorPoint].		newEvent _ self convertEvent: evt.		newEvent ifNil:[^self].		mode == #paint ifTrue:[			newEvent getVertex ifNil:[^self].			self prepareAction: newEvent.			self on: #mouseMove send: #mouseMovePaint: to: self.			self on: #mouseUp send: #mouseUpPaint: to: self.			^self perform: palette action with: newEvent].	] ifFalse:[		newEvent _ self convertEvent: evt.		newEvent ifNil:[^self].	].	newEvent getActor hasActiveTexture		ifTrue: [^ newEvent getActor morphicMouseDown: newEvent].	(evt redButtonPressed or:[evt controlKeyPressed]) ifTrue:[		reactions _ newEvent getActor getReactionsTo: leftMouseDown.		mouseUpButton _ leftMouseUp.	] ifFalse: [evt yellowButtonPressed ifTrue:[ 		reactions _ newEvent getActor getReactionsTo: rightMouseDown.		mouseUpButton _ rightMouseUp.	] ifFalse: [reactions _ nil]].	reactions ifNotNil: [reactions do: [:aReaction | aReaction reactTo: newEvent]].! !