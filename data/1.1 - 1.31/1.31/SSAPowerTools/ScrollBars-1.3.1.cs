'From Squeak 1.3 of Jan 16, 1998 on 26 January 1998 at 9:22:44 pm'!
MouseMenuController subclass: #SSAScrollBarController
	instanceVariableNames: 'unitScrollDelay '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PowerTools'!
View subclass: #SSAScrollBarView
	instanceVariableNames: 'scrollBarWidth scrollBarBox upButtonBox downButtonBox elevatorShaft elevatorCache upButtonCache downButtonCache '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PowerTools'!

!Pen methodsFor: 'accessing' stamp: 'ssa 1/15/98 15:53'!
destForm: aForm
	"2/14/97 ssa added for compatibility."
	self flag:#compatibility.

	destForm _ aForm ! !


!ScrollController methodsFor: 'basic control sequence' stamp: 'ssa 12/10/97 15:24'!
controlInitialize
	"Recompute scroll bars.  Save underlying image unless it is already saved."
	"Hacked to disable flop-out scroll bars when inside an SSAScrollBarView - ssa 12/10/97 15:21"

	super controlInitialize.
	scrollBar region: (0 @ 0 extent: 24 @ view apparentDisplayBox height).
	scrollBar insideColor: view backgroundColor.
	marker region: self computeMarkerRegion.
	scrollBar _ scrollBar align: scrollBar topRight with: view apparentDisplayBox topLeft.
	marker _ marker align: marker topCenter with: self upDownLine @ (scrollBar top + 2).
	savedArea isNil ifTrue: [savedArea _ Form fromDisplay: scrollBar].

	(self view superView isKindOf: SSAScrollBarView) "_______HERE IS THE HACK"
		ifFalse:[scrollBar displayOn: Display].

	"Show a border around yellow-button (menu) region"
"
	yellowBar _ Rectangle left: self yellowLine right: scrollBar right + 1
		top: scrollBar top bottom: scrollBar bottom.
	Display border: yellowBar width: 1 mask: Form veryLightGray.
"
	self moveMarker
! !

!ScrollController methodsFor: 'control defaults' stamp: 'ssa 12/10/97 17:10'!
controlActivity
	"Hacked to supprt SSAScrollBarView - ssa 12/10/97 17:07"

	(self view superView isKindOf: SSAScrollBarView)
		ifFalse:[
	self scrollBarContainsCursor
				ifTrue: [self scroll]
				ifFalse: [super controlActivity]]
		ifTrue:[super controlActivity]! !

!ScrollController methodsFor: 'control defaults' stamp: 'ssa 1/8/98 16:26'!
isControlActive 
	"Viva la Junta!!"
	"HACKED to ignore scrollbar in the activity test if contained in a ScrollbarView - ssa 1/8/98 16:24"
	view isNil ifTrue: [^ false].

	(self view superView isKindOf: SSAScrollBarView) "_______HERE IS THE HACK"
		ifFalse:["original code" ^(view insetDisplayBox merge: scrollBar inside) containsPoint: sensor cursorPoint]
		ifTrue:[^view insetDisplayBox containsPoint: sensor cursorPoint]! !

!ScrollController methodsFor: 'marker adjustment' stamp: 'ssa 12/10/97 15:25'!
moveMarker
	"The view window has changed. Update the marker."
	"Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an SSAScrollBarView - ssa 12/10/97 15:24"

	(self view superView isKindOf: SSAScrollBarView) "_______HERE IS THE HACK"
		ifFalse:[self moveMarker: self markerDelta negated anchorMarker: nil]! !

!ScrollController methodsFor: 'marker adjustment' stamp: 'ssa 12/10/97 15:26'!
moveMarkerTo: aRectangle 
	"Same as markerRegion: aRectangle; moveMarker, except a no-op if the marker
	 would not move."
	"Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an SSAScrollBarView - ssa 12/10/97 15:24"

	(self view superView isKindOf: SSAScrollBarView) "_______HERE IS THE HACK"
		ifFalse:[	(aRectangle height = marker height and: [self viewDelta = 0])
					ifFalse:
						[self markerRegion: aRectangle.
						self moveMarker]]! !


!ParagraphEditor methodsFor: 'controlling' stamp: 'ssa 12/11/97 15:36'!
controlActivity
	"Hacked to supprt SSAScrollBarView - ssa 12/10/97 17:07"
	
	self scrollBarContainsCursor
		ifTrue: [(self view superView isKindOf: SSAScrollBarView)
					ifFalse:[self scroll]]
		ifFalse: [self processKeyboard.
				self processMouseButtons]! !

!ParagraphEditor methodsFor: 'scrolling' stamp: 'ssa 1/15/98 15:28'!
scrollView: anInteger 
	"Paragraph scrolling uses opposite polarity"
	"Adjusted to keep text from scrolling off the view - ssa 1/15/98 15:22.  The - 30 below is the hack to allow for some empty space to show at the bottom."
	| maximumAmount minimumAmount amount |
	maximumAmount _
		paragraph clippingRectangle top -
		paragraph compositionRectangle top  max: 0.
	minimumAmount _
		paragraph clippingRectangle bottom -
		paragraph compositionRectangle bottom - 30 min: 0.
	amount _
		(anInteger min: maximumAmount) max:
		minimumAmount.
	^ self scrollBy: amount negated! !

!ParagraphEditor methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:27'!
updateMarker
	"Hacked to catch this scrolling 'event'.  ssa 12/5/97 16:22"
	"A variation of computeMarkerRegion--only redisplay the marker in the scrollbar if an actual change has occurred in the positioning of the paragraph."
	self moveMarkerTo: self computeMarkerRegion.

		"A hack to notify the SSAScrollBarController"
	(self view superView isKindOf: SSAScrollBarView)
		ifTrue:[self view superView updateElevator]! !


!SSAScrollBarController reorganize!
('delays' defaultUnitScrollDelay wait)
('accessing' elevatorBox scroller unitScrollDelay unitScrollDelay: viewToScroll)
('scrolling' pageDown pageHeight pageUp scrollAbsolute unitDown unitHeight unitUp)
('control activity' controlActivity redButtonActivity)
!


!SSAScrollBarController methodsFor: 'delays' stamp: 'ssa 1/15/98 13:51'!
defaultUnitScrollDelay
	"Answer the delay to use when unit scrolling, i.e., pressing the up or down button."

	^Delay forMilliseconds: 50! !

!SSAScrollBarController methodsFor: 'delays' stamp: 'ssa 1/15/98 13:52'!
wait

	self defaultUnitScrollDelay wait! !

!SSAScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/10/97 15:50'!
elevatorBox
	"Compute this each time since the view content my change."

	| box originY extentY |
	box _ self view elevatorShaft.
	extentY _ self viewToScroll  percentVisibleContent * box height.
	originY _  (self viewToScroll percentPreceedingContent * box height) min: box height - extentY.
	^(box origin x asInteger @ (box origin y + originY) asInteger max: box origin) extent: ((box extent x asInteger @ extentY asInteger) min: box extent)! !

!SSAScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/5/97 16:00'!
scroller

	^self viewToScroll controller! !

!SSAScrollBarController methodsFor: 'accessing' stamp: 'ssa 1/15/98 13:50'!
unitScrollDelay
	"<^hOf Delay>"
	"ssa 1/15/98 13:50 - Answer the instance variable, unitScrollDelay"

	unitScrollDelay isNil ifTrue:[self unitScrollDelay: self defaultUnitScrollDelay].
	^unitScrollDelay! !

!SSAScrollBarController methodsFor: 'accessing' stamp: 'ssa 1/15/98 13:50'!
unitScrollDelay: aDelay 
	"<aDelay: hOf Delay, ^self>"
	"ssa 1/15/98 13:50 - Set unitScrollDelay to be aDelay."

	unitScrollDelay _ aDelay! !

!SSAScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:40'!
viewToScroll

	^self view subViews first! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:07'!
pageDown
	"Scroll down by one page length."
	self scroller scrollView: self pageHeight negated.
	self view updateElevator! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:00'!
pageHeight
	"Answer the height of a page for the scrolling view."

	^self viewToScroll displayBox height! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:07'!
pageUp
	"Scroll up by one page length."
	self scroller scrollView: self pageHeight.
	self view updateElevator! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 1/20/98 13:45'!
scrollAbsolute
	| box  offset height center |
	box _ self view elevatorShaft.
	height _ box height.
	[Sensor redButtonPressed]
		whileTrue:  
			[center _ self view elevatorBox center y.
			offset _ (center - Sensor cursorPoint y). 
			(self viewToScroll percentPreceedingContent ~= 0.0 or:[self viewToScroll percentVisibleContent < 1.0])
				ifTrue:[self scroller scrollView: ((offset / height) * (self scroller view totalContentHeight * self scroller view unitContentHeight)) asInteger.
					self view updateElevator]] ! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 1/15/98 13:54'!
unitDown
	"Scroll down by one content unit."

	self view displayDownButtonPressed.
	[Sensor redButtonPressed] whileTrue:[
		self scroller scrollView: self unitHeight negated.
		self view updateElevator.
		self wait].
	self view displayDownButton.
! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:00'!
unitHeight
	"Answer the height of a content unit for the scrolling view."

	^self viewToScroll unitContentHeight! !

!SSAScrollBarController methodsFor: 'scrolling' stamp: 'ssa 1/15/98 13:54'!
unitUp
	"Scroll up by one content unit."

	self view displayUpButtonPressed.
	[Sensor redButtonPressed] whileTrue:[
		self scroller scrollView: self unitHeight.
		self view updateElevator.
		self wait].
	self view displayUpButton.
! !

!SSAScrollBarController methodsFor: 'control activity' stamp: 'ssa 1/8/98 16:28'!
controlActivity 
	"Refer to the comment in Controller|controlActivity."

	(Sensor redButtonPressed and:[self view scrollBarBox containsPoint: Sensor cursorPoint])
				ifTrue: [^self redButtonActivity].
	super controlActivity
! !

!SSAScrollBarController methodsFor: 'control activity' stamp: 'ssa 1/20/98 13:39'!
redButtonActivity

	| point |
"	self scroller view visibleContentHeight >= (self scroller view totalContentHeight + 2)
		ifTrue:[^self]."
	point _ Sensor cursorPoint.
	(self view upButtonBox containsPoint: point)
		ifTrue:[^self unitUp].
	(self view downButtonBox containsPoint: point)
		ifTrue:[^self unitDown].
	(self view elevatorBox containsPoint: point)
		ifTrue:[^self scrollAbsolute].
	self view elevatorBox center y > point y
		ifTrue:[self pageUp]
		ifFalse:[self pageDown].

! !


!SSAScrollBarView class reorganize!
('examples' exampleBrowser exampleWithListView exampleWithSILView exampleWithStringView exampleWithTextView)
('instance creation' on:)
!


!SSAScrollBarView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:24'!
on: aScrollingView
	"<aScrollingView: {hOf ListView | hOf TextView | hOf StringHolderView | hOf ParagraphEditor | hOf CodeView | hOf PPSListView | hOf SelectionInListView}, ^iOf self>"
	"Answer an instance of me that encapsulates aScrollingView by providing Windows-style scroll bars"

	^self new on: aScrollingView! !


!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:00'!
percentContentVisible
	"Answer the percent of my content that is visible.  ssa 12/5/97 15:37"

	^self visibleContentHeight / self totalContentHeight! !

!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:00'!
percentPrecedingContent
	"Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

	^0.0! !

!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:31'!
percentPreceedingContent
	"Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

	^0.0! !

!View methodsFor: 'scrolling support' stamp: 'ssa 1/15/98 15:19'!
percentVisibleContent
	"Answer the percent of my content that is visible.  ssa 12/5/97 15:37"

	^self totalContentHeight = 0
		ifTrue:[0.0]
		ifFalse:[self visibleContentHeight / self totalContentHeight]! !

!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 15:15'!
totalContentHeight
	"Answer the height of my content, visible or not.  ssa 12/5/97 15:04"

	^100 / self unitContentHeight! !

!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 15:15'!
unitContentHeight
	"Answer the unit height of my content.  ssa 12/5/97 15:04"

	^10.0! !

!View methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 15:15'!
visibleContentHeight
	"Answer the height of my visible content.  ssa 12/5/97 15:04"

	^100 / self unitContentHeight! !


!ListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:19'!
update: aSymbol 
	"Refer to the comment in View|update:."
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aSymbol == #list
		ifTrue: 
			[self list: model list.
			self displayView]
		ifFalse:[
			aSymbol == #listIndex
				ifTrue: 
					[self moveSelectionBox: model listIndex]].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !

!ListView methodsFor: 'scrolling support' stamp: 'ssa 12/10/97 16:09'!
percentPreceedingContent
	"Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

	| para lineIndex |
	para _ self list.
	lineIndex _ para lineIndexOfTop: para visibleRectangle top.
	lineIndex = 1 ifTrue:[^0.0].
	^lineIndex / para numberOfLines asFloat
! !

!ListView methodsFor: 'scrolling support' stamp: 'ssa 12/10/97 16:26'!
scrollBy: anInteger 
	"Scroll up by this amount adjusted by lineSpacing and list limits"
	"Hacked to support SSAScrollBarView - ssa 12/10/97 16:26"

	| maximumAmount minimumAmount amount |
	maximumAmount _ 0 max:
		list clippingRectangle top - list compositionRectangle top.
	minimumAmount _ 0 min:
		list clippingRectangle bottom - list compositionRectangle bottom.
	amount _ (anInteger min: maximumAmount) max: minimumAmount.
	amount ~= 0
		ifTrue: [list scrollBy: amount negated.
				(self superView isKindOf: SSAScrollBarView)  "______HERE IS THE HACK"
					ifTrue:[self superView updateElevator].
			  ^ true]
		ifFalse: [^ false]  "Return false if no scrolling took place"! !

!ListView methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 15:16'!
totalContentHeight
	"Answer the total height of my contents. ssa 12/5/97 15:16"
	^ self list compositionRectangle height / self unitContentHeight! !

!ListView methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:00'!
unitContentHeight
	"Answer the unit height of my contents. ssa 12/5/97 15:16"
	^ self list lineGrid asFloat! !

!ListView methodsFor: 'scrolling support' stamp: 'ssa 12/10/97 16:04'!
visibleContentHeight
	"Answer the total height of my contents. ssa 12/5/97 15:16"
	^ self list clippingRectangle height / self unitContentHeight! !


!ClassListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:01'!
update: aSymbol
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	(aSymbol == #systemCategorySelectionChanged) |
	(aSymbol == #editSystemCategories) |
	(aSymbol == #classListChanged)
		ifTrue:  [self updateClassList].
	(aSymbol == #classSelectionChanged)
		ifTrue: [self updateClassSelection].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!ContextStackListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:01'!
update: aSymbol
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aSymbol == #contextStackIndex
		ifTrue: [self moveSelectionBox: model contextStackIndex].
	aSymbol == #contextStackList
		ifTrue: 
			[self list: model contextStackList.
			self displayView].
	aSymbol == #notChanged ifTrue: [self flash].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!FileListView methodsFor: 'as yet unclassified' stamp: 'ssa 12/11/97 17:02'!
update: aSymbol 
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"
	aSymbol = #relabel ifTrue: [^ self].
	aSymbol == #fileList ifTrue: 
			[self list: model fileList.
			self displayView].
	aSymbol == #fileListIndex ifTrue: 
			[self moveSelectionBox: model fileListIndex].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!InspectListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:02'!
update: aSymbol
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aSymbol == #inspectObject
		ifTrue: 
			[self list: model fieldList.
			selection _ model selectionIndex.
			self displayView].
	aSymbol == #selection ifTrue: [self moveSelectionBox: model selectionIndex].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!MessageCategoryListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:04'!
update: aSymbol
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	(aSymbol == #systemCategorySelectionChanged) |
	(aSymbol == #editSystemCategories)
		ifTrue: [self resetAndDisplayView].
	(aSymbol == #classSelectionChanged)
		ifTrue: [self getListAndDisplayView].
	(aSymbol == #messageCategorySelectionChanged)
		ifTrue:  [self moveSelectionBox: model messageCategoryListIndex].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!MessageListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:06'!
update: aSymbol
	"What to do to the message list when Browser changes. If there is only one item, select and show it.
	 : as part of adding a new feature that was subsequently removed, simplified the code here enough to justify using it"
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aSymbol == #messageSelectionChanged
		ifTrue: [self updateMessageSelection]
		ifFalse:[

	(#(systemCategorySelectionChanged editSystemCategories editClass editMessageCategories) includes: aSymbol)
		ifTrue: [self resetAndDisplayView]
		ifFalse:[

	(aSymbol == #messageCategorySelectionChanged) | (aSymbol == #messageListChanged) 
		ifTrue: [self updateMessageList.]
		ifFalse:[

	(aSymbol == #classSelectionChanged) ifTrue:
		[model messageCategoryListIndex = 1
			ifTrue: ["self updateMessageList."]
			ifFalse: [self resetAndDisplayView]]]]].

		(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!PluggableListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:07'!
update: aSymbol 
	"Refer to the comment in View|update:."
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	| oldIndex newIndex |
	aSymbol == getListSelector ifTrue: [
		oldIndex _ self getCurrentSelectionIndex.
		self list: self getList.
		newIndex _ self getCurrentSelectionIndex.
		(oldIndex > 0 and: [newIndex = 0]) ifTrue: [
			"new list did not include the old selection; deselecting"
			self changeModelSelection: newIndex].
		self displayView.
		self displaySelectionBox].
	aSymbol == getSelectionSelector ifTrue: [
		self moveSelectionBox: self getCurrentSelectionIndex].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!SSAScrollBarView reorganize!
('displaying' deEmphasizeView displayDownButton displayDownButtonPressed displayElevator displayElevatorShaft displayUpButton displayUpButtonPressed displayView drawDownButton drawElevator drawUpButton emphasizeView updateElevator)
('lock access' unlock)
('display box access' determineDownButtonBox determineElevatorBox determineElevatorShaft determineScrollBarBox determineUpButtonBox displayBox insetDisplayBox realInsetDisplayBox)
('subview additions' addSubView:in:borderWidth: on: on:borderWidth:)
('display transformation' displayTransform: displayTransformation realDisplayTransformation)
('delegation' doesNotUnderstand:)
('accessing' downButtonBox downButtonBox: downButtonCache downButtonCache: elevatorBox elevatorCache elevatorCache: elevatorShaft elevatorShaft: getWindow scrollBarBox scrollBarBox: scrollBarWidth scrollBarWidth: scrollingView upButtonBox upButtonBox: upButtonCache upButtonCache: window)
('control defaults' defaultControllerClass)
('testing' containsPoint:)
('bordering' borderWidth:)
!


!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 16:49'!
deEmphasizeView

	self displayView.
	super deEmphasizeView! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:13'!
displayDownButton
	| box |
	box _ self downButtonBox.
	self downButtonCache extent = box extent
		ifFalse:[self drawDownButton].
	self downButtonCache displayOn: Display at: box origin! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
displayDownButtonPressed
	| box cf bb |
	box _ self downButtonBox.
	Display fill: box fillColor: Color darkGray.
	Display fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	cf _ ColorForm mappingWhiteToTransparentFrom: (Form
					extent: 13 @ 13
					depth: 1
					fromArray: #(0 0 0 0 0 260046848 117440512 33554432 0 0 0 0 0 )
					offset: 0 @ 0).
	bb _ BitBlt
				destForm: Display
				sourceForm: cf
				fillColor: Color black
				combinationRule: Form paint
				destOrigin: box origin + 2
				sourceOrigin: 0 @ 0
				extent: cf extent
				clipRect: box truncated.
	bb copyBits!
]style[(24 197 30 363)f1b,f1,f1b,f1! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:19'!
displayElevator
	| box |
	box _ self elevatorBox.
	self elevatorCache extent = box extent
		ifFalse:[self drawElevator].
	self elevatorCache displayOn: Display at: box origin! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/10/97 15:13'!
displayElevatorShaft

	| box |
	box _ self elevatorShaft.
	Display fill: box fillColor: Color lightGray.
! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:18'!
displayUpButton
	| box |
	box _ self upButtonBox.
	self upButtonCache extent = box extent
		ifFalse:[self drawUpButton].
	self upButtonCache displayOn: Display at: box origin! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
displayUpButtonPressed
	| box cf bb |
	box _ self upButtonBox.
	Display fill: box fillColor: Color darkGray.
	Display fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	cf _ ColorForm mappingWhiteToTransparentFrom: (Form
					extent: 13 @ 13
					depth: 1
					fromArray: #(0 0 0 0 0 33554432 117440512 260046848 0 0 0 0 0 )
					offset: 0 @ 0).
	bb _ BitBlt
				destForm: Display
				sourceForm: cf
				fillColor: Color black
				combinationRule: Form paint
				destOrigin: box origin + 2
				sourceOrigin: 0 @ 0
				extent: cf extent
				clipRect: box truncated.
	bb copyBits!
]style[(22 195 30 363)f1b,f1,f1b,f1! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 14:50'!
displayView

	self displayElevatorShaft.
	self displayUpButton.
	self displayDownButton.
	self displayElevator
! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
drawDownButton
	| box pen cf bb form |
	form _ Form extent: self downButtonBox extent depth: Display depth.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight. 
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	cf _ ColorForm mappingWhiteToTransparentFrom: (Form
					extent: 13 @ 13
					depth: 1
					fromArray: #(0 0 0 0 0 260046848 117440512 33554432 0 0 0 0 0 )
					offset: 0 @ 0).
	bb _ BitBlt
				destForm: form
				sourceForm: cf
				fillColor: Color black
				combinationRule: Form paint
				destOrigin: box origin + 1
				sourceOrigin: 0 @ 0
				extent: cf extent
				clipRect: box truncated.
	bb copyBits.
	self downButtonCache: form!
]style[(14 758 30 389)f1b,f1,f1b,f1! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:20'!
drawElevator
	| box pen form |
	form _ Form extent: self controller elevatorBox extent depth: Display depth.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft. 
	pen goto: box topLeft.
	pen goto: box topRight.
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	self elevatorCache: form! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
drawUpButton
	| box pen cf bb form |
	form _ Form extent: self upButtonBox extent depth: Display depth.
	box _ form boundingBox.
	pen _ Pen new.
	pen destForm: form.
	pen color: Color gray.
	pen place: box bottomLeft.
	pen goto: box topLeft.
	pen goto: box topRight. 
	pen color: Color veryLightGray.
	pen place: box bottomLeft + (1 @ 0).
	pen goto: box topLeft + 1.
	pen goto: box topRight + (0 @ 1).
	pen color: Color darkGray.
	pen place: box bottomLeft + (1 @ 1 negated).
	pen goto: box bottomRight - (1 @ 1).
	pen goto: box topRight + (1 negated @ 1).
	pen color: Color black.
	pen place: box bottomLeft.
	pen goto: box bottomRight.
	pen goto: box topRight.
	form fill: (box origin + (2 @ 2) corner: box corner - (1 @ 1))
		fillColor: Color gray.
	cf _ ColorForm mappingWhiteToTransparentFrom: (Form
					extent: 13 @ 13
					depth: 1
					fromArray: #(0 0 0 0 0 33554432 117440512 260046848 0 0 0 0 0 )
					offset: 0 @ 0).
	bb _ BitBlt
				destForm: form
				sourceForm: cf
				fillColor: Color black
				combinationRule: Form paint
				destOrigin: box origin + 1
				sourceOrigin: 0 @ 0
				extent: cf extent
				clipRect: box truncated.
	bb copyBits.
	self upButtonCache: form!
]style[(12 756 30 387)f1b,f1,f1b,f1! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 14:49'!
emphasizeView

	self displayView.
	super emphasizeView! !

!SSAScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 16:08'!
updateElevator

	self displayElevatorShaft.
	self displayElevator! !

!SSAScrollBarView methodsFor: 'lock access' stamp: 'ssa 12/5/97 16:10'!
unlock
	"Flush the cache."
	self scrollBarBox: nil.
	self upButtonBox: nil.
	self downButtonBox: nil.
	self elevatorShaft: nil.
	super unlock! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:48'!
determineDownButtonBox
	"Answer the rectangle for the scroll bar down button."

	^self scrollBarBox corner -  self scrollBarBox width asPoint extent: self scrollBarBox width asPoint! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:38'!
determineElevatorBox
	"Answer the rectangle for the scroll bar elevator."

	^self scrollBarBox center -  (self scrollBarWidth asPoint // 2) extent: self scrollBarWidth asPoint! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 14:19'!
determineElevatorShaft
	"Answer the rectangle for the scroll bar down button."

	^self upButtonBox bottomLeft corner: self downButtonBox topRight! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:47'!
determineScrollBarBox
	"Answer the rectangle for the scroll bar region."

	^(self realInsetDisplayBox areasOutside: self insetDisplayBox) first! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:52'!
determineUpButtonBox
	"Answer the rectangle for the scroll bar up button."

	^self scrollBarBox origin extent: self scrollBarBox width asPoint! !

!SSAScrollBarView methodsFor: 'display box access'!
displayBox
	"tah -- (17 July 1989 6:37:46 pm ) -- Answer the real displayBox"

	^self realInsetDisplayBox expandBy: borderWidth! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:00'!
insetDisplayBox
	"Answer the inset displayBox reduced by the horizontal space for the scroll bar"

	| box |
	box _ self realInsetDisplayBox.
	^box origin extent: box width - (self borderWidth left  + self scrollBarWidth) @ box height

! !

!SSAScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/15/97 14:50'!
realInsetDisplayBox
	"tah -- (17 July 1989 6:05:48 pm ) -- answer the real inset displayBox "

	^super insetDisplayBox! !

!SSAScrollBarView methodsFor: 'subview additions' stamp: 'ssa 1/15/98 15:50'!
addSubView: aView in: aRelativeRectangle borderWidth: width
	"11/3/96 ssa - added for compatibility."
 
	"Make 'aView' into a subview. Use 'aRelativeRectangle' and the 
	super view's window to compute (1) a viewport within the 
	superview for 'aView' and (2) the window extent for 'aView'. Note: 
	defining the windowing transformation and deriving the viewport is 
	logically equivalent but does not seem to be easily done"

	| subViewPort myWindow myExtent myOrigin |
	self addSubView: aView ifCyclic: [self error: 'cycle in subView structure.'].
	aView borderWidth: width.
	myWindow _ self window.
	myExtent _ myWindow extent.
	myOrigin _ myWindow origin.
	subViewPort _ myExtent * aRelativeRectangle origin + myOrigin 
						corner: myExtent * aRelativeRectangle corner + myOrigin.
	aView window: aView window viewport: subViewPort
! !

!SSAScrollBarView methodsFor: 'subview additions'!
on: aView
	"tah -- (17 July 1989 7:17:34 pm ) -- Add a subview to this view"

	self on: aView borderWidth: 0! !

!SSAScrollBarView methodsFor: 'subview additions'!
on: aView borderWidth: aBorderWidth
	"tah -- (17 July 1989 7:17:34 pm ) -- Add a subview to this view"

	self addSubView: aView in: (0@0 extent: (1@1)) borderWidth: aBorderWidth! !

!SSAScrollBarView methodsFor: 'display transformation'!
displayTransform: anObject 
	"Apply the display transformation of the receiver to anObject (see 
	View|displayTransformation) and answer the resulting scaled, translated 
	object. It is normally applied to Rectangles, Points, and other objects with 
	coordinates defined in the View's local coordinate system in order to get a 
	corresponding object in display coordinates."

	^(self realDisplayTransformation applyTo: anObject) rounded! !

!SSAScrollBarView methodsFor: 'display transformation' stamp: 'ssa 12/15/97 14:49'!
displayTransformation
	"This is a hook for to get the real displayTransformation"

	^self scrollBarWidth = 0
		ifTrue: [self realDisplayTransformation]
		ifFalse: [WindowingTransformation window: self getWindow viewport: (self realInsetDisplayBox expandBy: self borderWidth)]! !

!SSAScrollBarView methodsFor: 'display transformation'!
realDisplayTransformation
	"This is a hook for labeledView to get the real displayTransformation"

	^super displayTransformation! !

!SSAScrollBarView methodsFor: 'delegation' stamp: 'ssa 1/23/98 14:56'!
doesNotUnderstand: aMessage
	"Here it comes, the dreaded doesNotUnderstand:  HACK   ssa 1/23/98 14:55"

	^self scrollingView perform: aMessage selector withArguments: aMessage arguments! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 12:24'!
downButtonBox
	"<^hOf Rectangle>"
	"ssa 12/5/97 11:15 - Answer the instance variable, downButtonBox"

	downButtonBox isNil ifTrue:[self downButtonBox: self determineDownButtonBox].
	^downButtonBox! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
downButtonBox: aRectangle 
	"<aRectangle: hOf Rectangle, ^self>"
	"ssa 12/5/97 11:15 - Set downButtonBox to be aRectangle."

	downButtonBox _ aRectangle! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
downButtonCache
	"<^hOf Form>"
	"ssa 12/11/97 11:43 - Answer the instance variable, downButtonCache"

	downButtonCache isNil ifTrue:[self downButtonCache: (Form extent:1@1)].
	^downButtonCache! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:43'!
downButtonCache: aColorForm 
	"<aColorForm: hOf ColorForm, ^self>"
	"ssa 12/11/97 11:43 - Set downButtonCache to be aColorForm."

	downButtonCache _ aColorForm! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 16:29'!
elevatorBox
	"Answer the rectangle for the elevator."

	^self controller elevatorBox! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
elevatorCache
	"<^hOf Form>"
	"ssa 12/10/97 16:32 - Answer the instance variable, elevatorCache"

	elevatorCache isNil ifTrue:[self elevatorCache: (Form extent:1@1)].
	^elevatorCache! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/10/97 16:32'!
elevatorCache: aForm 
	"<aForm: hOf Form, ^self>"
	"ssa 12/10/97 16:32 - Set elevatorCache to be aForm."

	elevatorCache _ aForm! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:18'!
elevatorShaft
	"<^hOf Rectangle>"
	"ssa 12/5/97 14:18 - Answer the instance variable, elevatorShaft"

	elevatorShaft isNil ifTrue:[self elevatorShaft: self determineElevatorShaft].
	^elevatorShaft! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:18'!
elevatorShaft: aRectangle 
	"<aRectangle: hOf Rectangle, ^self>"
	"ssa 12/5/97 14:18 - Set elevatorShaft to be aRectangle."

	elevatorShaft _ aRectangle! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 12:43'!
getWindow
	"This is here to break a recursive loop caused by the indirection of my display transformation."
	self window isNil ifTrue:[self window: Display boundingBox].
	^self window! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:28'!
scrollBarBox
	"<^hOf Rectangle>"
	"ssa 12/5/97 11:15 - Answer the instance variable, scrollBarBox"

	scrollBarBox isNil ifTrue:[self scrollBarBox: self determineScrollBarBox].
	^scrollBarBox! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
scrollBarBox: aRectangle 
	"<aRectangle: hOf Rectangle, ^self>"
	"ssa 12/5/97 11:15 - Set scrollBarBox to be aRectangle."

	scrollBarBox _ aRectangle! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/10/97 14:05'!
scrollBarWidth
	"<^hOf Integer>"
	"ssa 12/5/97 11:27 - Answer the instance variable, scrollBarWidth"

	scrollBarWidth isNil ifTrue:[self scrollBarWidth: 12].
	^scrollBarWidth! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:27'!
scrollBarWidth: anInteger 
	"<anInteger: hOf Integer, ^self>"
	"ssa 12/5/97 11:27 - Set scrollBarWidth to be anInteger."

	scrollBarWidth _ anInteger! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 1/24/98 16:06'!
scrollingView

	^self subViews isEmpty ifTrue:[nil] ifFalse:[self subViews first]! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 13:59'!
upButtonBox
	"<^hOf Rectangle>"
	"ssa 12/5/97 11:15 - Answer the instance variable, upButtonBox"

	upButtonBox isNil ifTrue:[self upButtonBox: self determineUpButtonBox].
	^upButtonBox! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
upButtonBox: aRectangle 
	"<aRectangle: hOf Rectangle, ^self>"
	"ssa 12/5/97 11:15 - Set upButtonBox to be aRectangle."

	upButtonBox _ aRectangle! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
upButtonCache
	"<^hOf Form>"
	"ssa 12/11/97 11:43 - Answer the instance variable, upButtonCache"

	upButtonCache isNil ifTrue:[self upButtonCache: (Form extent:1@1)].
	^upButtonCache! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:43'!
upButtonCache: aColorForm 
	"<aColorForm: hOf ColorForm, ^self>"
	"ssa 12/11/97 11:43 - Set upButtonCache to be aColorForm."

	upButtonCache _ aColorForm! !

!SSAScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 12:44'!
window
	"This is here to break a recursive loop caused by the indirection of my display transformation."
	window isNil ifTrue:[self window: Display boundingBox].
	^window! !

!SSAScrollBarView methodsFor: 'control defaults' stamp: 'ssa 12/5/97 12:16'!
defaultControllerClass

	^SSAScrollBarController! !

!SSAScrollBarView methodsFor: 'testing' stamp: 'ssa 1/8/98 16:13'!
containsPoint: aPoint
	"Overriden to access my real insetDsiplayBox"

	^ self realInsetDisplayBox containsPoint: aPoint! !

!SSAScrollBarView methodsFor: 'bordering' stamp: 'ssa 1/24/98 16:05'!
borderWidth: anything

	super borderWidth:1.
! !


!StringHolderView methodsFor: 'updating' stamp: 'ssa 1/15/98 14:39'!
updateDisplayContents
	"Make the text that is displayed be the contents of the receiver's model."
	"VIVA LA JUNTA!!!!  hack this to update the scroll bar when the contents changes  - ssa 1/15/98 14:39"

	self editString: model contents.
	self displayView.
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !

!StringHolderView methodsFor: 'scrolling support' stamp: 'ssa 12/10/97 14:49'!
percentPreceedingContent
	"Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"
	| para lineIndex |
	para _ self displayContents.
	lineIndex _ para lineIndexOfTop: para visibleRectangle top.
	lineIndex = 1 ifTrue:[^0.0].
	^lineIndex / para numberOfLines asFloat
! !

!StringHolderView methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 15:17'!
totalContentHeight
	"Answer the total height of my contents. ssa 12/5/97 15:16"
	^ self displayContents compositionRectangle height / self unitContentHeight! !

!StringHolderView methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:00'!
unitContentHeight
	"Answer the unit height of my contents. ssa 12/5/97 15:16"
	^ self displayContents lineGrid asFloat! !

!StringHolderView methodsFor: 'scrolling support' stamp: 'ssa 12/5/97 16:00'!
visibleContentHeight
	"Answer the total height of my contents. ssa 12/5/97 15:16"
	^ self displayContents clippingRectangle height / self unitContentHeight! !


!BrowserCodeView methodsFor: 'updating' stamp: 'ssa 1/15/98 14:47'!
updateDisplayContents 
	"Refer to the comment in StringHolderView|updateDisplayContents."
	"VIVA LA JUNTA!!!!  hack this to update the scroll bar when the 
	contents changes  - ssa 1/15/98 14:39"
	| contents |
	contents _ model contents.
	displayContents asString ~= contents
		ifTrue: 
			[model messageListIndex ~= 0
				ifTrue: [contents _ contents asText
								makeSelectorBoldIn: model selectedClassOrMetaClass].
			self editString: contents.
			self displayView.
			model editSelection == #newMessage ifTrue:
				[controller selectFrom: 1 to: contents size]].

	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!ContextStackCodeView methodsFor: 'updating' stamp: 'ssa 1/15/98 15:34'!
updateDisplayContents 
	"Refer to the comment in StringHolderView|updateDisplayContents."
	"VIVA LA JUNTA!!!!  hack this to update the scroll bar when the 
	contents changes  - ssa 1/15/98 14:39"

	| contents |
	contents _ model contents.
	displayContents string ~= contents
		ifTrue: 
			[displayContents _
				(contents asText makeSelectorBoldIn: model selectedClassOrMetaClass) asParagraph.
			self positionDisplayContents.
			self controller changeParagraph: displayContents.
			self displayView.
			self highlightPC].

	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!SystemCategoryListView methodsFor: 'updating' stamp: 'ssa 12/11/97 17:09'!
update: aSymbol
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aSymbol == #systemCategorySelectionChanged
		ifTrue: [self updateSystemCategorySelection].
	aSymbol == #systemCategoriesChanged
		ifTrue: [self updateSystemCategoryList].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator]! !


!TextCollectorView methodsFor: 'updating' stamp: 'ssa 1/15/98 15:06'!
update: aParameter
	"Transcript cr; show: 'qwre'.    Transcript clear."
	"Hacked to support SSAScrollBars - ssa 12/11/97 16:42"

	aParameter == #appendEntry ifTrue:
		[controller doOccluded: [controller appendEntry]].
	aParameter == #update ifTrue:
		[controller doOccluded:
				[controller changeText: model contents asText]].
	(self superView isKindOf: SSAScrollBarView)
		ifTrue:[self superView updateElevator].
	(aParameter == #update) | (aParameter == #appendEntry)
		ifFalse:[^ super update: aParameter]! !



