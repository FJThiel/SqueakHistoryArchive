'From Squeak3.1alpha of 28 February 2001 [latest update: #4116] on 1 June 2001 at 1:51:35 pm'!!WonderlandCameraControls methodsFor: 'event handling' stamp: 'ar 6/1/2001 13:50'!mouseDown: evt	"Handle a mouse down event."	myUndoStack push: (UndoPOVChange for: myCamera from: (myCamera getPointOfView)).	moveAction _ myCamera doEachFrame: [self moveCamera: World primaryHand lastEvent].! !