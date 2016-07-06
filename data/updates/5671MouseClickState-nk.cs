'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5205] on 9 May 2003 at 10:41:42 am'!"Change Set:		MouseClickStateDate:			9 May 2003Author:			Ned KonzThis makes a double click event be noticed on the second mouseUp rather than on the second mouseDown event."!!MouseClickState methodsFor: 'event handling' stamp: 'nk 5/8/2003 19:10'!handleEvent: evt from: aHand	"Process the given mouse event to detect a click, double-click, or drag.	Return true if the event should be processed by the sender, false if it shouldn't.	NOTE: This method heavily relies on getting *all* mouse button events."	| localEvt timedOut isDrag |	timedOut _ (evt timeStamp - firstClickTime) > dblClickTime.	localEvt _ evt transformedBy: (clickClient transformedFrom: aHand owner).	isDrag _ (localEvt cursorPoint - firstClickDown cursorPoint) r > dragThreshold.	clickState == #firstClickDown ifTrue: [		"Careful here - if we had a slow cycle we may have a timedOut mouseUp event"		(timedOut and:[localEvt isMouseUp not]) ifTrue:[			"timeout before #mouseUp -> keep waiting for drag if requested"			clickState _ #firstClickTimedOut.			dragSelector ifNil:[				aHand resetClickState.				self doubleClickTimeout; click "***"].			^true].		localEvt isMouseUp ifTrue:[			(timedOut or:[dblClickSelector isNil]) ifTrue:[				self click.				aHand resetClickState.				^true].			"Otherwise transfer to #firstClickUp"			firstClickUp _ evt copy.			clickState _ #firstClickUp.			"If timedOut or the client's not interested in dbl clicks get outta here"			self click.			aHand handleEvent: firstClickUp.			^false].		isDrag ifTrue:["drag start"			self doubleClickTimeout. "***"			aHand resetClickState.			dragSelector "If no drag selector send #click instead"				ifNil: [self click]				ifNotNil: [self drag: localEvt].			^true].		^false].	clickState == #firstClickTimedOut ifTrue:[		localEvt isMouseUp ifTrue:["neither drag nor double click"			aHand resetClickState.			self doubleClickTimeout; click. "***"			^true].		isDrag ifTrue:["drag start"			aHand resetClickState.			self doubleClickTimeout; drag: localEvt. "***"			^true].		^false].	clickState = #firstClickUp ifTrue:[		(timedOut) ifTrue:[			"timed out after mouseUp - send #click: and mouseUp"			aHand resetClickState.			self doubleClickTimeout. "***"			^true].		localEvt isMouseDown ifTrue:["double click"			clickState _ #secondClickDown.			^false]].	clickState == #secondClickDown ifTrue: [		localEvt isMouseUp ifTrue: ["double click"			aHand resetClickState.			self doubleClick.			^false]	].	^true! !