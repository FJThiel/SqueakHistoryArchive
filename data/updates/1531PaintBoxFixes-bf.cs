'From Squeak 2.5 of August 6, 1999 [latest update: #1514] on 4 October 1999 at 11:30:50 pm'!"Change Set:		paintBoxFixes-bfDate:			4 October 1999Author:			Bert Freudenberg* Restores the PaintBox's stamp deleting feature (dropping into trash can)* Makes transparent stamps work in display depth > 8"!!SketchEditorMorph methodsFor: 'actions & preps' stamp: 'bf 10/4/1999 23:02'!pickupMouseUp: evt	"Grab a part of the picture (or screen) and store it in a known place. Like Copy on the Mac menu. Then switch to the stamp tool."	| rr pp pForm ii oldRect curs |	lastEvent == nil ifFalse: [			"Last draw will stick out, must erase the area"			oldRect _ strokeOrigin rect: lastEvent cursorPoint + (14@14).			self restoreRect: (oldRect insetBy: -2)].	self primaryHand showTemporaryCursor: nil.	"later get rid of this"		rr _ strokeOrigin rect: evt cursorPoint + (14@14).	pp _ rr translateBy: self world viewBox origin.	ii _ rr translateBy: (0@0) - bounds origin.	(rr intersects: bounds) ifTrue: [		pForm _ paintingForm copy: ii.		pForm isAllWhite "means transparent" 			ifFalse: []	"normal case.  Can be transparent in parts"			ifTrue: [pForm _ nil.			"Get an un-dimmed picture of other objects on the playfield"			"don't know how yet"]].	pForm ifNil: [pForm _ Form fromDisplay: pp].		"Anywhere on the screen"	palette pickupForm: pForm.	curs _ palette actionCursor.	evt hand showTemporaryCursor: curs.! !!TrashCanMorph methodsFor: 'event handling' stamp: 'bf 10/4/1999 16:47'!mouseDown: evt	| paintBox palette |	self currentHand endDisplaySuppression.	"See if a stamp is being dropped into the trash. It is not held by the hand."	(paintBox _ self findActivePaintBox) ifNotNil: [		paintBox getSpecial == #stamp: ifTrue: [			paintBox deleteCurrentStamp.  "throw away stamp..."			self primaryHand showTemporaryCursor: nil.			^ self]].	  "... and don't open trash"	palette _ self standardPalette.	((palette notNil and: [palette isInWorld]) and: [palette hasScrapsTab])		ifTrue:			[palette showScrapsTab]		ifFalse:			[self currentHand openScrapsBook]! !TrashCanMorph removeSelector: #openTrash!