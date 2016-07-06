'From Squeak 1.31 of Feb 4, 1998 on 18 February 1998 at 12:30:08 am'!

!MSWSystemView methodsFor: 'displaying' stamp: 'stp 02/18/98 0-13:'!
drawCloseBoxForm
	"Answer a thinner 'X' for the close box."
	"CloseBoxForm bitEdit"

	^Form
		extent: 15@13
		depth: 8
		fromArray: #( 50529027 50529027 50529027 50529024 51252750 235802126 235802126 235801600 
			51249923 50529027 50529027 50531328 51249921 16974595 50528513 50531328 51249923 16843523 
			50397443 50531328 51249923 50397443 16843523 50531328 51249923 50528513 16974595 50531328 
			51249923 50397443 16843523 50531328 51249923 16843523 50397443 50531328 51249921 16974595 
			50528513 50531328 51249923 50529027 50529027 50531328 51249923 50529027 50529027 50531328 
			51121164 202116108 202116108 202116096)
		offset: 0@0! !

MSWSystemView initializeCache!


!MSWSystemView methodsFor: 'framing' stamp: 'stp 02/18/98 0-15:'!
collapse
	"If the receiver is not already collapsed, change its view to be that of its 
	label only."

	self isCollapsed ifFalse:
			[expandedViewport _ self viewport.
			savedSubViews _ subViews.
			self resetSubViews.
			labelText isNil ifTrue: [self label: nil.  bitsValid _ false.].
			self window: (self inverseDisplayTransform:
					((self labelDisplayBox topLeft extent: (labelText extent x + 80) @ self labelHeight)
						 intersect: self labelDisplayBox))]! !
