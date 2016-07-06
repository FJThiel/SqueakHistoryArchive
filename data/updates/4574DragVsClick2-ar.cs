'From Squeak3.2alpha of 2 October 2001 [latest update: #4573] on 3 December 2001 at 12:34:15 pm'!"Change Set:		DragVsClick2-arDate:			3 December 2001Author:			Andreas RaabFixes the drag vs. click logic for tile scripting elements."!!Morph methodsFor: 'testing' stamp: 'ar 12/3/2001 12:33'!shouldDropOnMouseUp	| former |	former _ self formerPosition ifNil:[^false].	^(former dist: self position) > 10! !!PhraseTileMorph methodsFor: 'mouse' stamp: 'ar 12/3/2001 12:33'!mouseDown: evt 	"Pretend we picked up the tile and then put it down for a trial   positioning. -- The essence of ScriptEditor mouseEnter:"	| ed guyToTake dup enclosingPhrase |	self isPartsDonor ifTrue:		[dup _ self duplicate.		evt hand attachMorph: dup.		dup position: evt position.		"So that the drag vs. click logic works"		dup formerPosition: evt position.		^ self].	submorphs isEmpty		ifTrue: [^ self].	guyToTake _ self.	[(enclosingPhrase _ guyToTake ownerThatIsA: PhraseTileMorph) notNil] whileTrue:		[guyToTake _ enclosingPhrase].  "This logic always grabs the outermost phrase, for now anyway"		"the below had comment: 'picking me out of another phrase'"	"owner class == TilePadMorph		ifTrue:			[(ss _ submorphs first) class == TilePadMorph				ifTrue: [ss _ ss submorphs first].			guyToTake _  ss veryDeepCopy]."	(ed _ self enclosingEditor) ifNil: [^ evt hand grabMorph: guyToTake].	evt hand grabMorph: guyToTake.	ed startStepping.	ed mouseEnterDragging: evt.	ed setProperty: #justPickedUpPhrase toValue: true.! !