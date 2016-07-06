'From Squeak3.7alpha of 11 September 2003 [latest update: #5707] on 17 February 2004 at 12:04:17 pm'!"Change Set:		StartDragFix-nkDate:			17 February 2004Author:			Ned KonzGhanges the start-drag logic to send the mouse-down event, instead of the event that is detected as the start of drag.This avoids the 10 pixel offset that can sometimes lead to incorrect behavior when dragging from PluggableListMorphs."!!MouseClickState methodsFor: 'event handling' stamp: 'nk 2/17/2004 12:01'!handleEvent: evt from: aHand	"Process the given mouse event to detect a click, double-click, or drag.	Return true if the event should be processed by the sender, false if it shouldn't.	NOTE: This method heavily relies on getting *all* mouse button events."	| localEvt timedOut isDrag |	timedOut _ (evt timeStamp - firstClickTime) > dblClickTime.	localEvt _ evt transformedBy: (clickClient transformedFrom: aHand owner).	isDrag _ (localEvt position - firstClickDown position) r > dragThreshold.	clickState == #firstClickDown ifTrue: [		"Careful here - if we had a slow cycle we may have a timedOut mouseUp event"		(timedOut and:[localEvt isMouseUp not]) ifTrue:[			"timeout before #mouseUp -> keep waiting for drag if requested"			clickState _ #firstClickTimedOut.			dragSelector ifNil:[				aHand resetClickState.				self doubleClickTimeout; click "***"].			^true].		localEvt isMouseUp ifTrue:[			(timedOut or:[dblClickSelector isNil]) ifTrue:[				self click.				aHand resetClickState.				^true].			"Otherwise transfer to #firstClickUp"			firstClickUp _ evt copy.			clickState _ #firstClickUp.			"If timedOut or the client's not interested in dbl clicks get outta here"			self click.			aHand handleEvent: firstClickUp.			^false].		isDrag ifTrue:["drag start"			self doubleClickTimeout. "***"			aHand resetClickState.			dragSelector "If no drag selector send #click instead"				ifNil: [self click]				ifNotNil: [self drag: firstClickDown].			^true].		^false].	clickState == #firstClickTimedOut ifTrue:[		localEvt isMouseUp ifTrue:["neither drag nor double click"			aHand resetClickState.			self doubleClickTimeout; click. "***"			^true].		isDrag ifTrue:["drag start"			aHand resetClickState.			self doubleClickTimeout; drag: firstClickDown. "***"			^true].		^false].	clickState = #firstClickUp ifTrue:[		(timedOut) ifTrue:[			"timed out after mouseUp - send #click: and mouseUp"			aHand resetClickState.			self doubleClickTimeout. "***"			^true].		localEvt isMouseDown ifTrue:["double click"			clickState _ #secondClickDown.			^false]].	clickState == #secondClickDown ifTrue: [		localEvt isMouseUp ifTrue: ["double click"			aHand resetClickState.			self doubleClick.			^false]	].	^true! !