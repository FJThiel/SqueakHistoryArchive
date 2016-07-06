'From Squeak2.7 of 5 January 2000 [latest update: #1796] on 24 January 2000 at 9:17:21 pm'!"Change Set:		nixWorldAutoViewing-swDate:			24 January 2000Author:			Scott WallaceDon't offer the 'automatic viewing' item in a playfield-options menu presented on behalf of the World"!!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 1/24/2000 21:14'!playfieldOptionsMenu	| aMenu isWorld |	isWorld _ self isWorldMorph.	aMenu _ MenuMorph new defaultTarget: self.	aMenu addStayUpItem.	aMenu add: 'save on file...' action: #saveOnFile.	aMenu add: 'save as SqueakPage at url...' action: #saveOnURL.	aMenu add: 'update all from resources' action: #updateAllFromResources.	(self valueOfProperty: #classAndMethod) ifNotNil:		[aMenu add: 'broadcast as documentation' action: #saveDocPane].	aMenu add: 'round up strays' action: #roundUpStrays.	aMenu balloonTextForLastItem:  'Bring back all objects whose current coordinates keep them from being visible, so that at least a portion of each of my interior objects can be seen.'.	aMenu addLine.	#(	(autoLineLayoutString	toggleAutoLineLayout			'whether submorphs should automatically be laid out in lines')		(indicateCursorString	toggleIndicateCursor			'whether the "current" submorph should be indicated with a dark black border')		(isPartsBinString		toggleIsPartsBin			'whether dragging an object from the interior should produce a COPY of the object')		(isOpenForDragNDropString	toggleOpenToDragNDrop			'whether objects can be dropped into and dragged out of me')		(mouseOverHalosString	toggleMouseOverHalos			'whether objects should put up halos when the mouse is over them')		(autoExpansionString	toggleAutomaticPhraseExpansion			'whether tile phrases, dropped on me, should automatically sprout Scriptors around them')		(originAtCenterString	toggleOriginAtCenter			'whether the cartesian origin of the playfield should be at its lower-left corner or at the center of the playfield')		(showThumbnailString	toggleAlwaysShowThumbnail			'whether large objects should be represented by thumbnail miniatures of themselves')) do:			[:triplet |				(isWorld and: [#(toggleAutoLineLayout toggleIndicateCursor toggleIsPartsBin toggleAlwaysShowThumbnail) includes: triplet second]) ifFalse:					[aMenu addUpdating: triplet first action: triplet second.					aMenu balloonTextForLastItem: triplet third]]. 	isWorld ifFalse:		[aMenu add: 'set thumbnail height...' action: #setThumbnailHeight.		aMenu balloonTextForLastItem: 'if currently showing thumbnails governs the standard height for them'.		aMenu addUpdating: #autoViewingString action: #toggleAutomaticViewing.		aMenu balloonTextForLastItem:  'governs whether, when an object is touched inside me, a viewer should automatically be launched for it.'].	isWorld ifFalse:		[aMenu add: 'behave like a Holder' action: #becomeLikeAHolder.		aMenu balloonTextForLastItem: 'Set properties to make this object nicely set up to hold frames of a scripted animation.'].	self backgroundSketch ifNotNil:		[aMenu add: 'delete background painting' action: #deleteBackgroundPainting.		aMenu balloonTextForLastItem: 'delete the graphic that forms the background for this me.'].	presenter ifNil:		[aMenu add: 'make detachable' action: #makeDetachable.		aMenu balloonTextForLastItem: 'Allow this area to be separately governed by its own controls.'].	aMenu addLine.	aMenu add: 'use standard texture' action: #setStandardTexture.	aMenu balloonTextForLastItem: 'use a pale yellow-and-blue background texture here.'.	aMenu add: 'make graph paper...' action: #makeGraphPaper.	aMenu balloonTextForLastItem: 'Design your own graph paper and use it as the background texture here.'.	aMenu addTitle: 'playfield options...'.	^ aMenu! !"Postscript:"Utilities replaceMenuFlap.!