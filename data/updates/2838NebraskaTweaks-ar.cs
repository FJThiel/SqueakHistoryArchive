'From Squeak2.9alpha of 13 June 2000 [latest update: #2885] on 24 October 2000 at 6:32:26 pm'!"Change Set:		NebraskaTweaks-arDate:			24 October 2000Author:			AndiTwo tweaks for Nebraska:#1: Allow remote halos.#2: Add a menu item for a '1x1 view'."!!NetworkTerminalBorderMorph methodsFor: 'as yet unclassified' stamp: 'ar 10/24/2000 18:30'!addCustomMenuItems: aMenu hand: aHand	super addCustomMenuItems: aMenu hand: aHand.	aMenu add: 'original size' action: #makeFullView.! !!NetworkTerminalBorderMorph methodsFor: 'as yet unclassified' stamp: 'ar 10/24/2000 18:30'!makeFullView	"Toggle the full view for network terminal"	self extent: self worldIEnclose extent + (2 * self borderWidth).! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:02'!handleKeyDown: anEvent	anEvent wasHandled ifTrue:[^self].	(self handlesKeyboard: anEvent) ifFalse:[^self].	anEvent wasHandled: true.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:02'!handleKeyUp: anEvent	anEvent wasHandled ifTrue:[^self].	(self handlesKeyboard: anEvent) ifFalse:[^self].	anEvent wasHandled: true.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:11'!handleKeystroke: anEvent	anEvent wasHandled ifTrue:[^self].	anEvent wasHandled: true.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:03'!handleMouseDown: anEvent	anEvent wasHandled ifTrue:[^self].	anEvent hand removePendingBalloonFor: self.	anEvent hand removePendingHaloFor: self.	anEvent wasHandled: true.	anEvent hand newMouseFocus: self event: anEvent.	anEvent hand removeHaloFromClick: anEvent on: self.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:04'!handleMouseMove: anEvent	anEvent wasHandled ifTrue:[^self]. "not interested"	(anEvent hand hasSubmorphs) ifTrue:[^self].	(anEvent anyButtonPressed and:[anEvent hand mouseFocus ~~ self]) ifTrue:[^self].	anEvent wasHandled: true.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:03'!handleMouseUp: anEvent	anEvent wasHandled ifTrue:[^self]. "not interested"	anEvent hand mouseFocus == self ifFalse:[^self]. "Not interested in other parties"	anEvent hand releaseMouseFocus: self.	anEvent wasHandled: true.	self sendEventAsIs: anEvent.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'ar 10/24/2000 18:06'!handlerForMouseDown: anEvent	^self! !NetworkTerminalMorph removeSelector: #handlesKeyboard:!NetworkTerminalMorph removeSelector: #handlesMouseDown:!NetworkTerminalMorph removeSelector: #handlesMouseOver:!NetworkTerminalMorph removeSelector: #keyStroke:!NetworkTerminalMorph removeSelector: #mouseDown:!NetworkTerminalMorph removeSelector: #mouseMove:!NetworkTerminalMorph removeSelector: #mouseUp:!