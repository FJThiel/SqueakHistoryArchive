!MSWScrollBarView methodsFor: 'display box access' stamp: 'stp 02/18/98 0-11:'!
insetDisplayBox
	"Answer the inset displayBox reduced by the horizontal space for the scroll bar"
	"Changed to left-side scroll bars--stp."
	"MSWScrollBarView someInstance"

	| box |
	box _ self realInsetDisplayBox.
"	^box origin extent: box width - (self borderWidth left  + self scrollBarWidth) @ box height
"
	^((box left + self scrollBarWidth) @ box top)
		extent: (box width - (self borderWidth left + self scrollBarWidth) @ box height)
! !
