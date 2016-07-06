'From Squeak3.2alpha of 2 October 2001 [latest update: #4571] on 2 December 2001 at 10:58:31 pm'!"Change Set:		DragVsClick-arDate:			2 December 2001Author:			Andreas RaabA common problem seen with newbies using Squeak is the 'drag vs. click' problem. People tend to click rather than drag and clicking in regions such as supply flaps or viewers may give pretty unexpected results. This change set attempts to fix this problem.Note that this is still somewhat experimental. If we don't like it we can easily revert back."!!Morph methodsFor: 'testing' stamp: 'ar 12/2/2001 21:47'!shouldDropOnMouseUp	| former |	self isTileLike ifTrue:[^false].	former _ self formerPosition ifNil:[^false].	^(former dist: self position) > 10! !!HandMorph methodsFor: 'grabbing/dropping' stamp: 'ar 12/2/2001 21:42'!dropMorph: aMorph event: anEvent	"Drop the given morph which was carried by the hand"	| event dropped |	(anEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue:[^self].	self privateRemoveMorph: aMorph.	dropped _ aMorph.	(dropped hasProperty: #addedFlexAtGrab) 		ifTrue:[dropped _ aMorph removeFlexShell].	event _ DropEvent new setPosition: self position contents: dropped hand: self.	self sendEvent: event focus: nil.	event wasHandled ifFalse:[aMorph rejectDropMorphEvent: event].	aMorph owner == self ifTrue:[aMorph delete].	self mouseOverHandler processMouseOver: anEvent.! !!ScriptEditorMorph methodsFor: 'dropping/grabbing' stamp: 'ar 12/2/2001 21:48'!mouseEnter: evt	| hand tile |	self flag: #bob.		"needed renderedMorph due to transformations"	hand _ evt hand.	hand submorphs size = 1 ifFalse: [^self].false ifTrue:[	evt "hand lastEvent" redButtonPressed ifFalse: [^self] ].	tile _ hand firstSubmorph renderedMorph.	(self wantsDroppedMorph: tile event: evt) ifFalse: [^self].	handWithTile _ hand.	self startStepping! !!ScriptEditorMorph methodsFor: 'dropping/grabbing' stamp: 'ar 12/2/2001 22:55'!step	| hand insertion i space1 d space2 insHt nxtHt prevBot ht2 c1 c2 ii where |	hand _ handWithTile ifNil: [self primaryHand].	"ar 12/2/2001: Commented out the following for drag vs. click"	("hand lastEvent redButtonPressed" true and: [(self hasOwner: hand) not]) ifTrue: [		hand submorphCount > 0 ifTrue: [			insertion _ hand firstSubmorph renderedMorph.			insHt _ insertion height.			self removeSpaces.			where _ self globalPointToLocal: hand position"insertion fullBounds topLeft".			i _ (ii _ self indexOfMorphAbove: where) min: submorphs size-1.			prevBot _ i <= 0 ifTrue: [(self innerBounds) top]							ifFalse: [(self submorphs at: i) bottom].			nxtHt _ (submorphs isEmpty				ifTrue: [insertion]				ifFalse: [self submorphs at: i+1]) height.			d _ ii > i ifTrue: [nxtHt "for consistent behavior at bottom"]					ifFalse: [0 max: (where y - prevBot min: nxtHt)].			"Top and bottom spacer heights cause continuous motion..."			c1 _ Color green.  c2 _ Color transparent.			ht2 _ d*insHt//nxtHt.			space1 _ Morph newBounds: (0@0 extent: 30@(insHt-ht2))                                        color: ((insHt-ht2) > (insHt//2+1) ifTrue: [c1] ifFalse: [c2]).			self privateAddMorph: space1 atIndex: (i+1 max: 1).			space2 _ Morph newBounds: (0@0 extent: 30@ht2)                                        color: (ht2 > (insHt//2+1) ifTrue: [c1] ifFalse: [c2]).			self privateAddMorph: space2 atIndex: (i+3 min: submorphs size+1).		]	] ifFalse: [		self stopStepping.		self removeSpaces.	]! !!SystemWindow methodsFor: 'testing' stamp: 'ar 12/2/2001 21:43'!shouldDropOnMouseUp	"Return true for consistency with fastdrag"	^true! !