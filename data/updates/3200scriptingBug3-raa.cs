'From Squeak2.9alpha of 17 July 2000 [latest update: #3262] on 13 January 2001 at 8:56 am'!"Change Set:		scriptingBug3Date:			13 January 2001Author:			Bob Arning- walkback prevention when something (the resize handle for a SystemWindow, in this case) is dropped into a ScriptEditorMorph that has a nil <showingMethodPane>"!!ScriptEditorMorph methodsFor: 'dropping/grabbing' stamp: 'RAA 1/13/2001 08:08'!repelsMorph: aMorph event: ev	(showingMethodPane == true and: 		[self world valueOfProperty: #universalTiles ifAbsent: [false]]) ifTrue: [			^ (aMorph respondsTo: #parseNode) not].	^ aMorph isTileLike not! !