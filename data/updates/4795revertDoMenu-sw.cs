'From Squeak3.3alpha of 12 January 2002 [latest update: #4794] on 6 March 2002 at 12:38 pm'!"Change Set:		revertDoMenu-swDate:			6 March 2002Author:			Scott WallaceReverts a method that got clobbered, presumably inadvertently, by update  4684RegFileList-hgglsd, resulting in a breaking of the functioning of the 'do menu command' function"!!Morph methodsFor: 'menus' stamp: 'ar 10/25/2000 23:17'!doMenuItem: menuString	| aMenu anItem aNominalEvent aHand |	aMenu _ self buildHandleMenu: (aHand _ self currentHand).	aMenu allMorphsDo: [:m | m step].  "Get wordings current"	anItem _ aMenu itemWithWording: menuString.	anItem ifNil:		[^ self player scriptingError: 'Menu item not found: ', menuString].	aNominalEvent _  MouseButtonEvent new		setType: #mouseDown		position: anItem bounds center		which: 4 "red"		buttons: 4 "red"		hand: aHand		stamp: nil.	anItem invokeWithEvent: aNominalEvent! !