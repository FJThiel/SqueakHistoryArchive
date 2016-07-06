'From Squeak2.9alpha of 17 July 2000 [latest update: #3236] on 8 January 2001 at 3:26:55 pm'!"Change Set:		showAllViewersDate:			8 January 2001Author:			Bob ArningAdds two items to the playfield menu:- 'hide all players' will delete viewer flap tabs from the playfield. This can save a lot of space when publishing.- 'show all players' will restore them after using the above (or any other deletion as well). Only those players which have user-written scripts will be shown."!!PasteUpMorph methodsFor: 'menu & halo' stamp: 'RAA 1/8/2001 15:14'!playfieldOptionsMenu	"Answer an auxiliary menu with options specific to playfields -- too many to be housed in the main menu"	| aMenu isWorld |	isWorld _ self isWorldMorph.	aMenu _ MenuMorph new defaultTarget: self.	aMenu addStayUpItem.	aMenu add: 'save on file...' action: #saveOnFile.	aMenu add: 'save as SqueakPage at url...' action: #saveOnURL.	aMenu add: 'update all from resources' action: #updateAllFromResources.	(self valueOfProperty: #classAndMethod) ifNotNil:		[aMenu add: 'broadcast as documentation' action: #saveDocPane].	aMenu add: 'round up strays' action: #roundUpStrays.	aMenu balloonTextForLastItem:  'Bring back all objects whose current coordinates keep them from being visible, so that at least a portion of each of my interior objects can be seen.'.	aMenu add: 'show all players' action: #showAllPlayers.	aMenu balloonTextForLastItem:  'Make visible the viewers for all players which have user-written scripts in this playfield.'.	aMenu add: 'hide all players' action: #hideAllPlayers.	aMenu balloonTextForLastItem:  'Make invisible the viewers for all players in this playfield. This will save space before you publish this project'.	aMenu addLine.	aMenu add: 'shuffle contents' action: #shuffleSubmorphs.	aMenu balloonTextForLastItem: 'Rearranges my contents in random order'.	self griddingOn		ifTrue: [aMenu add: 'turn gridding off' action: #griddingOnOff.				aMenu add: (self gridVisible ifTrue: ['hide'] ifFalse: ['show']) , ' grid'						action: #gridVisibleOnOff.				aMenu add: 'set grid spacing...' action: #setGridSpec]		ifFalse: [aMenu add: 'turn gridding on' action: #griddingOnOff].	aMenu addLine.	#(	(autoLineLayoutString	toggleAutoLineLayout			'whether submorphs should automatically be laid out in lines')		(indicateCursorString	toggleIndicateCursor			'whether the "current" submorph should be indicated with a dark black border')		(isPartsBinString		toggleIsPartsBin			'whether dragging an object from the interior should produce a COPY of the object')		(isOpenForDragNDropString	toggleDragNDrop			'whether objects can be dropped into and dragged out of me')		(mouseOverHalosString	toggleMouseOverHalos			'whether objects should put up halos when the mouse is over them')		(autoExpansionString	toggleAutomaticPhraseExpansion			'whether tile phrases, dropped on me, should automatically sprout Scriptors around them')		(originAtCenterString	toggleOriginAtCenter			'whether the cartesian origin of the playfield should be at its lower-left corner or at the center of the playfield')		(showThumbnailString	toggleAlwaysShowThumbnail			'whether large objects should be represented by thumbnail miniatures of themselves')		(fenceEnabledString	toggleFenceEnabled			'whether moving objects should stop at the edge of their container')		(batchPenTrailsString	toggleBatchPenTrails 			'if true, detailed movement of pens between display updates is ignored.  Thus multiple line segments drawn within a script may not be seen individually.')	) do:			[:triplet |				(isWorld and: [#(toggleAutoLineLayout toggleIndicateCursor toggleIsPartsBin toggleAlwaysShowThumbnail) includes: triplet second]) ifFalse:					[aMenu addUpdating: triplet first action: triplet second.					aMenu balloonTextForLastItem: triplet third]]. 	aMenu addUpdating: #autoViewingString action: #toggleAutomaticViewing.	aMenu balloonTextForLastItem:  'governs whether, when an object is touched inside me, a viewer should automatically be launched for it.'.	((isWorld not or: [self backgroundSketch notNil]) or: [presenter isNil])		ifTrue:			[aMenu addLine].	isWorld ifFalse:		[aMenu add: 'set thumbnail height...' action: #setThumbnailHeight.		aMenu balloonTextForLastItem: 'if currently showing thumbnails governs the standard height for them'.		aMenu add: 'behave like a Holder' action: #becomeLikeAHolder.		aMenu balloonTextForLastItem: 'Set properties to make this object nicely set up to hold frames of a scripted animation.'].	self backgroundSketch ifNotNil:		[aMenu add: 'delete background painting' action: #deleteBackgroundPainting.		aMenu balloonTextForLastItem: 'delete the graphic that forms the background for this me.'].	presenter ifNil:		[aMenu add: 'make detachable' action: #makeDetachable.		aMenu balloonTextForLastItem: 'Allow this area to be separately governed by its own controls.'].	aMenu addLine.	aMenu add: 'use standard texture' action: #setStandardTexture.	aMenu balloonTextForLastItem: 'use a pale yellow-and-blue background texture here.'.	aMenu add: 'make graph paper...' action: #makeGraphPaper.	aMenu balloonTextForLastItem: 'Design your own graph paper and use it as the background texture here.'.	aMenu addTitle: 'playfield options...'.	^ aMenu! !!PasteUpMorph methodsFor: 'scripting' stamp: 'RAA 1/8/2001 15:17'!hideAllPlayers	| a |	a _ OrderedCollection new.	self allMorphsDo: [ :x | 		(x isKindOf: ViewerFlapTab) ifTrue: [a add: x]	].	a do: [ :each | each delete].! !!PasteUpMorph methodsFor: 'scripting' stamp: 'RAA 1/8/2001 15:24'!showAllPlayers	| a |	a _ OrderedCollection new.	self allMorphsDo: [ :x | 		(x player notNil and: [x player hasUserDefinedScripts]) ifTrue: [a add: x]	].	a do: [ :each | each openViewerForArgument].! !