"simplerScrollbars.st
Simpler and smaller scrollbars that have just a dragable marker and paging regions above and below. UI preferences taken from STPs SmallColoredScrollBars.st.

	TITLE			simplerScrollbars.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			1.1
	IMAGE VERSION	1.16 - 1.2
	PREREQUISITES	none
	DATE			November 25, 1996"!

!Preferences class methodsFor: 'ui prefs'!
scrollBarBackground
	"Answer the preferred color for scroll bar backgrounds."

	^Color yellow!
scrollBarColor
	"Answer the preferred color for scroll bar markers."

	^Color cyan!
scrollBarWidth
	"Answer the preferred width for scroll bars."

	^8! !
!ScrollController methodsFor: 'initialize-release'!
initialize
	super initialize.
	scrollBar _ Quadrangle new.
	scrollBar borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	marker _ Quadrangle new.
	marker insideColor: Preferences scrollBarColor.! !
!ScrollController methodsFor: 'basic control sequence'!
controlInitialize
	"Recompute scroll bars.  Save underlying image unless it is already saved."

	super controlInitialize.
	scrollBar region: 
	 (0 @ 0 extent: Preferences scrollBarWidth + 4 @ view apparentDisplayBox height).
	scrollBar insideColor: Preferences scrollBarBackground.
	marker region: self computeMarkerRegion.
	scrollBar _ scrollBar align: scrollBar topRight with: view apparentDisplayBox topLeft.
	marker _ marker align: marker topLeft with: scrollBar inside topLeft + (1@0).
	savedArea isNil ifTrue: [savedArea _ Form fromDisplay: scrollBar].
	scrollBar displayOn: Display.
	self moveMarker
! !
!ScrollController methodsFor: 'marker adjustment'!
markerRegion: aRectangle 
	"Set the area defined by aRectangle as the marker. Fill it with gray tone."

	Display fill: marker fillColor: scrollBar insideColor.
	marker region: aRectangle.
	marker _ marker align: marker topLeft with: scrollBar inside topLeft + (1@0).! !
!ScrollController methodsFor: 'marker adjustment'!
computeMarkerRegion
	"Answer the rectangular area in which the gray area of the scroll bar 
	should be displayed."

	^0@0 extent: Preferences scrollBarWidth @
		(((view window height asFloat / view boundingBox height * scrollBar inside height)
			rounded min: scrollBar inside height) max: Preferences scrollBarWidth)! !
ScrollController removeSelector: #menuBarContainsCursor!
ScrollController removeSelector: #upLine!
ScrollController removeSelector: #yellowLine!
ScrollController removeSelector: #anyButtonActivity!
ScrollController removeSelector: #downLine!
ScrollController removeSelector: #upDownLine!
!ParagraphEditor methodsFor: 'scrolling'!
computeMarkerRegion 
	"Refer to the comment in ScrollController|computeMarkerRegion."

	paragraph compositionRectangle height = 0
		ifTrue:	[^0@0 extent: Preferences scrollBarWidth @ scrollBar inside height]
		ifFalse:	[^0@0 extent:
					Preferences scrollBarWidth 
						@ (((paragraph clippingRectangle height asFloat /
							self scrollRectangleHeight * scrollBar inside height) rounded
							min: scrollBar inside height) max: Preferences scrollBarWidth)]! !
!ListController methodsFor: 'marker adjustment'!
computeMarkerRegion 
	"Refer to the comment in ScrollController|computeMarkerRegion."

	| viewList |
	viewList _ view list.
	viewList compositionRectangle height = 0
		ifTrue: [^ 0@0 extent: Preferences scrollBarWidth@scrollBar inside height].
	^ 0@0 extent: Preferences scrollBarWidth@
			(((viewList clippingRectangle height asFloat /
						viewList compositionRectangle height *
							scrollBar inside height)
					rounded min: scrollBar inside height) max: Preferences scrollBarWidth)! !
!ScrollController methodsFor: 'scrolling'!
scroll
	"Check to see whether the user wishes to jump, scroll up, or scroll down."

	| savedCursor y |
	savedCursor _ sensor currentCursor.
	[self scrollBarContainsCursor] whileTrue: [
		Processor yield.
		y _ sensor cursorPoint y.
		y < marker top
			ifTrue: [ self scrollUp ]
			ifFalse: [ y > marker bottom
				ifTrue: [ self scrollDown ]
				ifFalse: [ self scrollAbsolute ] ] ].
	savedCursor show! !
!ScrollController methodsFor: 'scrolling'!
scrollViewUp
	"Scroll the receiver's view up the default amount.
	Return true only if scrolling actually took place."
	^ self scrollView: self scrollAmount! !
!ScrollController methodsFor: 'scrolling'!
scrollViewDown
	"Scroll the receiver's view down the default amount.
	Return true only if scrolling actually took place."
	^ self scrollView: self scrollAmount negated! !
!ListController methodsFor: 'scrolling'!
scrollAmount 
	"Refer to the comment in ScrollController|scrollAmount."

	"^sensor cursorPoint y - scrollBar inside top"
	^scrollBar height - 10! !
!ParagraphEditor methodsFor: 'scrolling'!
scrollAmount 
	"Refer to the comment in ScrollController|scrollAmount."

	"^sensor cursorPoint y - scrollBar top"
	^scrollBar height - 10! !
ScrollController removeInstVarName: 'menuBar'!
ScrollController removeInstVarName: 'savedMenuBarArea'!
ScrollController removeSelector: #markerContainsCursor!

!CRFillInTheBlankController methodsFor: 'basic control sequence'!
controlInitialize

	super controlInitialize. "Do this to avoid stray marker drawing. go-22.11.1996"
	startBlock _ paragraph characterBlockForIndex: startBlock stringIndex.
	stopBlock _ paragraph characterBlockForIndex: stopBlock stringIndex.
	self initializeSelection.
	beginTypeInBlock _ nil! !

!ScrollController methodsFor: 'private'!
scrollAbsolute
	| markerOutline oldY markerForm offset |
	self changeCursor: Cursor rightArrow.

	sensor anyButtonPressed ifTrue: [
	  markerOutline _ marker deepCopy.
	  markerForm _ Form fromDisplay: marker.
	  Display fill: marker fillColor: scrollBar insideColor.
	  Display border: markerOutline width: 1 fillColor: Color gray.
	  oldY _ sensor cursorPoint y.
	  offset _ oldY - marker center y.
	  markerForm 
		follow: 
			[oldY ~= sensor cursorPoint y
				ifTrue: 
					[oldY _ sensor cursorPoint y.
					marker _ marker translateBy: 
					  0 @ ((oldY - offset - marker center y 
						min: scrollBar inside bottom - marker bottom) 
						max: scrollBar inside top - marker top).
					self scrollView].
				marker origin] 
		while: [sensor anyButtonPressed].

	  Display fill: markerOutline fillColor: scrollBar insideColor.
	  self moveMarker
	]! !
