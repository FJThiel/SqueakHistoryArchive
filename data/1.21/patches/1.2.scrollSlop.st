"scrollSlop.st
Make the flop out scroll bars stay around a little longer.

	TITLE			scrollSlop.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			1.0
	IMAGE VERSION	1.16 - 1.2
	PREREQUISITES	simplerScrollbars.st
	DATE			July 3, 1997"!

'From Squeak 1.2 of June 29, 1997 on 1 July 1997 at 1:58:09 pm'!
!ScrollController methodsFor: 'control defaults'!
isControlActive 
	view isNil ifTrue: [^ false].
"	^ (view insetDisplayBox merge: scrollBar inside)"
	^ (view insetDisplayBox merge: (scrollBar inside translateBy: (-12@0)))
		containsPoint: sensor cursorPoint! !
