'From Squeak 2.1 of June 30, 1998 on 22 July 1998 at 2:10:35 pm'!"Change Set:		dontDrop-swDate:			22 July 1998Author:			Scott WallaceDon't allow arbitrary dropping onto an IndexTabs, nor onto a ViewerBook"!!IndexTabs methodsFor: 'layout' stamp: 'sw 7/21/1998 22:05'!openToDragNDrop	^ false! !!ViewerBook methodsFor: 'all' stamp: 'sw 7/21/1998 23:21'!repelsMorph:  aMorph event: evt	^ true! !