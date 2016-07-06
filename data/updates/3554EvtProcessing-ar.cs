'From Squeak3.1alpha of 4 February 2001 [latest update: #3548] on 9 February 2001 at 2:04:38 pm'!"Change Set:		EvtProcessing-arDate:			9 February 2001Author:			Andreas RaabAn attempt of giving faster feedback when processing mouse events. The world will be updated immediately after the first mouse event came in."!!HandMorph methodsFor: 'event handling' stamp: 'ar 2/9/2001 14:03'!processEvents	"Process user input events from the local input devices."	| evt evtBuf type hadAny |	hadAny _ false.	[(evtBuf _ Sensor nextEvent) == nil] whileFalse:[		evt _ nil. "for unknown event types"		type _ evtBuf at: 1.		(type = EventTypeMouse)			ifTrue:[evt _ self generateMouseEvent: evtBuf].		(type = EventTypeKeyboard) 			ifTrue:[evt _ self generateKeyboardEvent: evtBuf].		(type = EventTypeDragDropFiles)			ifTrue:[evt _ self generateDropFilesEvent: evtBuf].		"All other events are ignored"		evt == nil ifFalse:[			"Finally, handle it"			self handleEvent: evt.			hadAny _ true.			"For better user feedback, return immediately after a mouse event has been processed."			evt isMouse ifTrue:[^self].		].	].	"note: if we come here we didn't have any mouse events"	(mouseClickState notNil) ifTrue:[		"No mouse events during this cycle. Make sure click states time out accordingly"		mouseClickState handleEvent: lastMouseEvent asMouseMove from: self].	hadAny ifFalse:[		"No pending events. Make sure z-order is up to date"		self mouseOverHandler processMouseOver: lastMouseEvent.	].! !