'From Squeak 1.31 of Feb 4, 1998 on 18 February 1998 at 1:53:17 pm'!

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'stp 2/18/98 13:53'!
scrollAbsolute
	| lastY thisY |
	[Sensor redButtonPressed]
		whileTrue:  
			[thisY := Sensor cursorPoint y.
			lastY isNil
				ifTrue: [lastY := thisY.
					self reallyScrollAbsolute]
				ifFalse: [lastY = thisY
					ifFalse: [lastY := thisY.
						self reallyScrollAbsolute]]]! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'stp 2/18/98 13:50'!
reallyScrollAbsolute

	| height center offset |
	center _ self view elevatorBox center y.
	offset _ (center - Sensor cursorPoint y). 
	height _ self view elevatorShaft height.
	(self viewToScroll percentPreceedingContent ~= 0.0 or:
			[self viewToScroll percentVisibleContent < 1.0])
		ifTrue:[self scroller scrollView: ((offset / height) 
				* (self scroller view totalContentHeight 
					* self scroller view unitContentHeight)) asInteger.
			self view updateElevator]! !
