'From Squeak2.7 of 5 January 2000 [latest update: #1808] on 1 February 2000 at 2:07:26 pm'!"Change Set:		paintArea-swDate:			1 February 2000Author:			Scott WallaceMakes the default paint area for a new painting be larger.  The memory-saving heuristic now only really kicks in if you have a huge playfield."!!Preferences class methodsFor: 'parameters' stamp: 'sw 2/1/2000 14:05'!defaultPaintingExtent	"Answer the preferred size for the onion-skin paint area when launching a new painting within a paste-up morph.  Feel free to change the parameters to suit your configuration."	^ 800 @ 600! !