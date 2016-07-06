'From Squeak3.7beta of ''1 April 2004'' [latest update: #5905] on 3 May 2004 at 1:49:59 pm'!!ScrollPane methodsFor: 'geometry' stamp: 'sps 5/3/2004 13:49'!extent: newExtent		| oldW oldH wasHShowing wasVShowing noVPlease noHPlease minH minW |		oldW _ self width.	oldH _ self height.	wasHShowing _ self hIsScrollbarShowing.	wasVShowing _ self vIsScrollbarShowing.	"Figure out the minimum width and height for this pane so that scrollbars will appear"	noVPlease _ self valueOfProperty: #noVScrollBarPlease ifAbsent: [false]. 	noHPlease _ self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]. 	minH _ self scrollBarThickness + 16.	minW _ self scrollBarThickness + 20.	noVPlease ifTrue:[ 		noHPlease			ifTrue:[minH _ 1. minW _ 1 ]			ifFalse:[minH _ self scrollBarThickness ].	] ifFalse:[		noHPlease			ifTrue:[minH _ self scrollBarThickness + 5].	].	super extent: (newExtent max: (minW@minH)).	"Now reset widget sizes"	self resizeScrollBars; resizeScroller; hideOrShowScrollBars.		"Now resetScrollDeltas where appropriate, first the vScrollBar..."	((self height ~~ oldH) or: [ wasHShowing ~~ self hIsScrollbarShowing]) ifTrue:		[(retractableScrollBar or: [ self vIsScrollbarShowing ]) ifTrue:			[ self vSetScrollDelta ]].				"...then the hScrollBar"	((self width ~~ oldW) or: [wasVShowing ~~ self vIsScrollbarShowing]) ifTrue:		[(retractableScrollBar or: [ self hIsScrollbarShowing ]) ifTrue:			[ self hSetScrollDelta ]].! !