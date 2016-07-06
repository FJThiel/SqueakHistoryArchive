"      NAME	Windoze-2-1-1.cs
       AUTHOR	ssadams@us.ibm.com (Sam S. Adams (contribs from Andreas Raab and Stephen Pope))
       URL	(none)
       FUNCTION	Partial WIndoze look and feel
       KEYWORDS	Windoze Scrollbars Windows Microsoft Squeak
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak Version 2.1
       CONFLICTS	Overrides exisiting window code
       DISTRIBUTION	world
       VERSION	2.1.1
       DATE	04-Sep-98

SUMMARY

Overrides Squeak UI to simulate Windows look and feel.This version is a port of Windoze-1-3-4 to Squeak version 2.1.Not perfect, but most of the hard stuff.Includes window titlebar and borders, along with Windows-style scrollbars.Needs buttons and a way to change color scheme to match current Windows desktop.A few graphics need tweaking and bit cacheing needs work.

				Sam S. Adams (contribs from Andreas Raab and Stephen Pope)
"!
'From Squeak 2.1 of June 30, 1998 on 4 September 1998 at 5:31:28 pm'!
MouseMenuController subclass: #MSWScrollBarController
	instanceVariableNames: 'unitScrollDelay '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-MSWLook'!
StandardSystemController subclass: #MSWSystemController
	instanceVariableNames: 'lastSystemActivity '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-MSWLook'!
View subclass: #MSWScrollBarView
	instanceVariableNames: 'scrollBarWidth scrollBarBox upButtonBox downButtonBox elevatorShaft elevatorCache upButtonCache downButtonCache '
	classVariableNames: 'WhereToLocateScrollBars '
	poolDictionaries: ''
	category: 'Interface-MSWLook'!
StandardSystemView subclass: #MSWSystemView
	instanceVariableNames: 'growFrame '
	classVariableNames: 'CloseBoxForm GrowBoxForm ShrinkBoxForm SystemBoxForm '
	poolDictionaries: ''
	category: 'Interface-MSWLook'!

!ChangeList class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 17:25'!
open: aChangeList name: aString multiSelect: multiSelect
	"Create a standard system view for the messageSet, whose label is aString.
	The listView may be either single or multiple selection type"
	"Added scrollingView in support of Windoze - ssa"
	| topView aBrowserCodeView aListView |

	World ifNotNil: [^ self openAsMorph: aChangeList name: aString multiSelect: multiSelect].

	topView _ (StandardSystemView new) model: aChangeList.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	topView borderWidth: 1.

	aListView _ (multiSelect ifTrue: [PluggableListViewOfMany]
							ifFalse: [PluggableListView])
		on: aChangeList list: #list
		selected: #listIndex changeSelected: #toggleListIndex:
		menu: #changeListMenu:
		keystroke: #messageListKey:from:.
	aListView window: (0 @ 0 extent: 180 @ 100).
	topView addSubView: aListView.

	aBrowserCodeView _ PluggableTextView on: aChangeList 
			text: #contents accept: #contents:
			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.
	aBrowserCodeView scrollingView controller: ReadOnlyTextController new.
	aBrowserCodeView window: (0 @ 0 extent: 180 @ 300).
	topView addSubView: aBrowserCodeView below: aListView.
	topView controller open! !


!DisplayTextView class methodsFor: 'examples' stamp: 'ssa 9/4/1998 17:29'!
example2	
	"Create a standarad system view with two parts, one editable, the other not."
	"Modified to support Windoze - ssa"
	| topView aDisplayTextView |
	topView _ StandardSystemView new.
	topView label: 'Text Editor'.
	aDisplayTextView _ MSWScrollBarView on: (self new model: 'test string label' asDisplayText).
	aDisplayTextView scrollingView controller: NoController new.
	aDisplayTextView window: (0 @ 0 extent: 100 @ 100).
	aDisplayTextView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	topView addSubView: aDisplayTextView.

	aDisplayTextView _ MSWScrollBarView on: (self new model: 'test string' asDisplayText).
	aDisplayTextView window: (0 @ 0 extent: 100 @ 100).
	aDisplayTextView borderWidth: 2.
	topView
		addSubView: aDisplayTextView
		align: aDisplayTextView viewport topLeft
		with: topView lastSubView viewport topRight.
	topView controller open

	"DisplayTextView example2"! !

!DisplayTextView class methodsFor: 'examples'!
open: textOrString label: aLabel
        "Create a system view with a paragraph editor in it.  6/2/96 sw"
        "VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

        | topView aDisplayTextView |
        aDisplayTextView _ MSWScrollBarView on: (DisplayTextView new model: textOrString asDisplayText).
        aDisplayTextView borderWidth: 2.
        topView _ StandardSystemView new.
        topView label: aLabel.
        topView addSubView: aDisplayTextView.
        topView controller open

        "DisplayTextView open: 'Great green gobs' label: 'Gopher Guts'"! !


!FileList class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 17:14'!
open
	"Open a view of an instance of me on the default directory."
	"Added scrollingView to support Windoze - ssa"
	"FileList open"
	| dir aFileList topView volListView templateView fileListView fileContentsView |
	World ifNotNil: [^ self openAsMorph].

	dir _ FileDirectory default.
	aFileList _ self new directory: dir.
	topView _ StandardSystemView new.
	topView
		model: aFileList;
		label: dir pathName;
		minimumSize: 200@200.
	topView borderWidth: 1.

	volListView _ PluggableListView on: aFileList
		list: #volumeList
		selected: #volumeListIndex
		changeSelected: #volumeListIndex:
		menu: #volumeMenu:.
	volListView autoDeselect: false.
	volListView window: (0@0 extent: 80@45).
	topView addSubView: volListView.

	templateView _ PluggableTextView on: aFileList
		text: #pattern
		accept: #pattern:.
	templateView askBeforeDiscardingEdits: false.
	templateView window: (0@0 extent: 80@15).
	topView addSubView: templateView below: volListView.

	fileListView _ PluggableListView on: aFileList
		list: #fileList
		selected: #fileListIndex
		changeSelected: #fileListIndex:
		menu: #fileListMenu:.
	fileListView window: (0@0 extent: 120@60).
	topView addSubView: fileListView toRightOf: volListView.
	"fileListView scrollingView controller terminateDuringSelect: true. " "Pane to left may change under scrollbar"

	fileContentsView _ PluggableTextView on: aFileList
		text: #contents accept: #put:
		readSelection: #contentsSelection menu: #fileContentsMenu:shifted:.
	fileContentsView window: (0@0 extent: 200@140).
	topView addSubView: fileContentsView below: templateView.

	topView controller open.
! !


!MSWScrollBarController methodsFor: 'delays' stamp: 'ssa 1/15/98 13:51'!
defaultUnitScrollDelay
        "Answer the delay to use when unit scrolling, i.e., pressing the up or down button."

        ^Delay forMilliseconds: 50! !

!MSWScrollBarController methodsFor: 'delays' stamp: 'ssa 1/15/98 13:52'!
wait

        self defaultUnitScrollDelay wait! !

!MSWScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/10/97 15:50'!
elevatorBox
        "Compute this each time since the view content my change."

        | box originY extentY |
        box _ self view elevatorShaft.
        extentY _ self viewToScroll  percentVisibleContent * box height.
        originY _  (self viewToScroll percentPreceedingContent * box height) min: box height - extentY.
        ^(box origin x asInteger @ (box origin y + originY) asInteger max: box origin) extent: ((box extent x asInteger @ extentY asInteger) min: box extent)! !

!MSWScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/5/97 16:00'!
scroller

        ^self viewToScroll controller! !

!MSWScrollBarController methodsFor: 'accessing' stamp: 'ssa 1/15/98 13:50'!
unitScrollDelay
        "<^hOf Delay>"
        "ssa 1/15/98 13:50 - Answer the instance variable, unitScrollDelay"

        unitScrollDelay isNil ifTrue:[self unitScrollDelay: self defaultUnitScrollDelay].
        ^unitScrollDelay! !

!MSWScrollBarController methodsFor: 'accessing' stamp: 'ssa 1/15/98 13:50'!
unitScrollDelay: aDelay 
        "<aDelay: hOf Delay, ^self>"
        "ssa 1/15/98 13:50 - Set unitScrollDelay to be aDelay."

        unitScrollDelay _ aDelay! !

!MSWScrollBarController methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:40'!
viewToScroll

        ^self view subViews first! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:07'!
pageDown
        "Scroll down by one page length."
        self scroller scrollView: self pageHeight negated.
        self view updateElevator! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:00'!
pageHeight
        "Answer the height of a page for the scrolling view."

        ^self viewToScroll displayBox height! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:07'!
pageUp
        "Scroll up by one page length."
        self scroller scrollView: self pageHeight.
        self view updateElevator! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'stp 2/18/98 13:50'!
reallyScrollAbsolute

        | height center offset |
        center _ self view elevatorBox center y.
        offset _ (center - Sensor cursorPoint y). 
        height _ self view elevatorShaft height.
        (self viewToScroll percentPreceedingContent ~= 0.0 or:
                        [self viewToScroll percentVisibleContent < 1.0])
                ifTrue:[self scroller scrollView: ((offset / height) 
                                * (self scroller view totalContentHeight 
                                        * self scroller view unitContentHeight)) asInteger.
                        self view updateElevator]! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'stp 2/18/98 13:53'!
scrollAbsolute
        | lastY thisY |
        [Sensor redButtonPressed]
                whileTrue:  
                        [thisY := Sensor cursorPoint y.
                        lastY isNil
                                ifTrue: [lastY := thisY.
                                        self reallyScrollAbsolute]
                                ifFalse: [lastY = thisY
                                        ifFalse: [lastY := thisY.
                                                self reallyScrollAbsolute]]]! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 1/15/98 13:54'!
unitDown
        "Scroll down by one content unit."

        self view displayDownButtonPressed.
        [Sensor redButtonPressed] whileTrue:[
                self scroller scrollView: self unitHeight negated.
                self view updateElevator.
                self wait].
        self view displayDownButton.
! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 12/5/97 16:00'!
unitHeight
        "Answer the height of a content unit for the scrolling view."

        ^self viewToScroll unitContentHeight! !

!MSWScrollBarController methodsFor: 'scrolling' stamp: 'ssa 1/15/98 13:54'!
unitUp
        "Scroll up by one content unit."

        self view displayUpButtonPressed.
        [Sensor redButtonPressed] whileTrue:[
                self scroller scrollView: self unitHeight.
                self view updateElevator.
                self wait].
        self view displayUpButton.
! !

!MSWScrollBarController methodsFor: 'control activity' stamp: 'ssa 1/8/98 16:28'!
controlActivity 
        "Refer to the comment in Controller|controlActivity."

        (Sensor redButtonPressed and:[self view scrollBarBox containsPoint: Sensor cursorPoint])
                                ifTrue: [^self redButtonActivity].
        super controlActivity
! !

!MSWScrollBarController methodsFor: 'control activity' stamp: 'ssa 1/20/98 13:39'!
redButtonActivity

        | point |
"       self scroller view visibleContentHeight >= (self scroller view totalContentHeight + 2)
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


!MSWScrollBarView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:24'!
on: aScrollingView
        "<aScrollingView: {hOf ListView | hOf TextView | hOf StringHolderView | hOf ParagraphEditor | hOf CodeView | hOf PPSListView | hOf SelectionInListView}, ^iOf self>"
        "Answer an instance of me that encapsulates aScrollingView by providing Windows-style scroll bars"

        ^self new on: aScrollingView! !

!MSWScrollBarView class methodsFor: 'class initialization' stamp: 'ssa 2/27/98 11:08'!
initialize
        "Make an initial decision as to which side to locate the scrollbars.  See preferences to change sides."

        self locateScrollBarsOnLeftSide! !

!MSWScrollBarView class methodsFor: 'preferences' stamp: 'ssa 2/27/98 11:40'!
locateScrollBarsOnLeftSide
        "Set the flag to locate the ScrollBars on the left hand side of the view."
        "MSWScrollBarView locateScrollBarsOnLeftSide"

        | openWindows |
        WhereToLocateScrollBars _ #left.
        self allInstances do:[:each| each unlock].
        openWindows _ ScheduledControllers controllersSatisfying:
                [:each| (each view  respondsTo:#isCollapsed)
                                and:[each view isCollapsed not]].
        openWindows reverseDo:[:each| each view uncacheBits display].
        ScheduledControllers restore! !

!MSWScrollBarView class methodsFor: 'preferences' stamp: 'ssa 2/27/98 11:39'!
locateScrollBarsOnRightSide
        "Set the flag to locate the ScrollBars on the right hand side of the view."
        "MSWScrollBarView locateScrollBarsOnRightSide"

        | openWindows |
        WhereToLocateScrollBars _ #right.
        self allInstances do:[:each| each unlock].
        openWindows _ ScheduledControllers controllersSatisfying:
                [:each| (each view  respondsTo:#isCollapsed)
                                and:[each view isCollapsed not]].
        openWindows reverseDo:[:each| each view uncacheBits display].
        ScheduledControllers restore! !


!Pen methodsFor: 'accessing'!
destForm: aForm
        "2/14/97 ssa added for compatibility."
        self flag:#compatibility.

        destForm _ aForm ! !


!PluggableListView class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 16:02'!
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See aboutPluggability comment."
	"Added Windoze wrapper - ssa"

	^ MSWScrollBarView on: (self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: nil
		keystroke: #arrowKey:from:		"default")
! !

!PluggableListView class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 16:01'!
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See aboutPluggability comment."
	"Added Windoze wrapper - ssa"

	^ MSWScrollBarView on: (self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: #arrowKey:from:		"default")

! !

!PluggableListView class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 16:00'!
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel
	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See aboutPluggability comment."
	"Added Windoze wrapper - ssa"

	^ MSWScrollBarView on: (self new
		on: anObject
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel
		menu: getMenuSel
		keystroke: keyActionSel)
! !


!ScrollController methodsFor: 'basic control sequence'!
controlInitialize
        "Recompute scroll bars.  Save underlying image unless it is already saved."
        "Hacked to disable flop-out scroll bars when inside an MSWScrollBarView - ssa 12/10/97 15:21"

        super controlInitialize.
        scrollBar region: (0 @ 0 extent: 24 @ view apparentDisplayBox height).
        scrollBar insideColor: view backgroundColor.
        marker region: self computeMarkerRegion.
        scrollBar _ scrollBar align: scrollBar topRight with: view apparentDisplayBox topLeft.
        marker _ marker align: marker topCenter with: self upDownLine @ (scrollBar top + 2).
        savedArea isNil ifTrue: [savedArea _ Form fromDisplay: scrollBar].

        (self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
                ifFalse:[scrollBar displayOn: Display].

        "Show a border around yellow-button (menu) region"
"
        yellowBar _ Rectangle left: self yellowLine right: scrollBar right + 1
                top: scrollBar top bottom: scrollBar bottom.
        Display border: yellowBar width: 1 mask: Form veryLightGray.
"
        self moveMarker
! !

!ScrollController methodsFor: 'control defaults'!
controlActivity
        "Hacked to supprt MSWScrollBarView - ssa 12/10/97 17:07"

        (self view superView isKindOf: MSWScrollBarView)
                ifFalse:[
        self scrollBarContainsCursor
                                ifTrue: [self scroll]
                                ifFalse: [super controlActivity]]
                ifTrue:[super controlActivity]! !

!ScrollController methodsFor: 'control defaults'!
isControlActive 
        "Viva la Junta!!"
        "HACKED to ignore scrollbar in the activity test if contained in a ScrollbarView - ssa 1/8/98 16:24"
        view isNil ifTrue: [^ false].

        (self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
                ifFalse:["original code" ^(view insetDisplayBox merge: scrollBar inside) containsPoint: sensor cursorPoint]
                ifTrue:[^view insetDisplayBox containsPoint: sensor cursorPoint]! !

!ScrollController methodsFor: 'marker adjustment'!
moveMarker
        "The view window has changed. Update the marker."
        "Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an MSWScrollBarView - ssa 12/10/97 15:24"

        (self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
                ifFalse:[self moveMarker: self markerDelta negated anchorMarker: nil]! !

!ScrollController methodsFor: 'marker adjustment'!
moveMarkerTo: aRectangle 
        "Same as markerRegion: aRectangle; moveMarker, except a no-op if the marker
         would not move."
        "Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an MSWScrollBarView - ssa 12/10/97 15:24"

        (self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
                ifFalse:[       (aRectangle height = marker height and: [self viewDelta = 0])
                                        ifFalse:
                                                [self markerRegion: aRectangle.
                                                self moveMarker]]! !


!ParagraphEditor methodsFor: 'controlling'!
controlActivity
        "Hacked to supprt MSWScrollBarView - ssa 12/10/97 17:07"
        
        self scrollBarContainsCursor
                ifTrue: [(self view superView isKindOf: MSWScrollBarView)
                                        ifFalse:[self scroll]]
                ifFalse: [self processKeyboard.
                                self processMouseButtons]! !

!ParagraphEditor methodsFor: 'scrolling'!
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

!ParagraphEditor methodsFor: 'scrolling'!
updateMarker
        "Hacked to catch this scrolling 'event'.  ssa 12/5/97 16:22"
        "A variation of computeMarkerRegion--only redisplay the marker in the scrollbar if an actual change has occurred in the positioning of the paragraph."
        self moveMarkerTo: self computeMarkerRegion.

                "A hack to notify the MSWScrollBarController"
        (self view superView isKindOf: MSWScrollBarView)
                ifTrue:[self view superView updateElevator]! !


!StandardSystemController methodsFor: 'borders'!
adjustWindowCorners 
        | box cornerBox p clicked f2 |
        box _ view windowBox.
        clicked _ false.
        #(topLeft topRight bottomRight bottomLeft)
                do: [:readCorner |
                        cornerBox _ ((box insetBy: 2) perform: readCorner) - (10@10) extent: 20@20.
                        (cornerBox containsPoint: sensor cursorPoint)
                                ifTrue: 
                                ["Display reverse: cornerBox."
                                (Cursor perform: readCorner) showWhile:
                                        [[(cornerBox containsPoint: (p _ sensor cursorPoint))
                                                and: [(clicked _ sensor anyButtonPressed) not]]
                                                whileTrue.
                                "Display reverse: cornerBox."
                                clicked ifTrue:
                                        [view newFrame:
                                                [:f | p _ sensor cursorPoint.
                                                readCorner = #topLeft ifTrue:
                                                        [f2 _ p corner: f bottomRight].
                                                readCorner = #bottomLeft ifTrue:
                                                        [f2 _ (f withBottom: p y) withLeft: p x].
                                                readCorner = #bottomRight ifTrue:
                                                        [f2 _ f topLeft corner: p].
                                                readCorner = #topRight ifTrue:
                                                        [f2 _ (f withTop: p y) withRight: p x].
                                                f2]]]]].
        ^ clicked! !

!StandardSystemController methodsFor: 'borders'!
checkForReframe
        "2/18/97 ssa - added a leftshift hack to prevent over aggressive offerings of reframing help for subpanes."

        | cp |
        view isCollapsed ifTrue: [^ self].
        cp _ sensor cursorPoint.
        ((view closeBoxFrame expandBy: 2) containsPoint: cp)
                | ((view growBoxFrame expandBy: 2) containsPoint: cp)
                ifTrue: [^ self].  "Dont let reframe interfere with close/grow"
        self adjustWindowCorners.
        self cursorOnBorder ifFalse: [^ self].
        ((view insetDisplayBox insetBy: 2@2) containsPoint: cp)
                ifFalse: [^ self adjustWindowBorders].

        Sensor leftShiftDown 
                ifTrue:[        
                        (view subviewWithLongestSide: [:s | ] near: cp) == nil
                                ifFalse: [^ self adjustPaneBorders]].! !


!MSWSystemController methodsFor: 'accessing' stamp: 'ar 1/22/98 23:30'!
lastSystemActivity
        ^lastSystemActivity ifNil:[lastSystemActivity _ 0]! !

!MSWSystemController methodsFor: 'accessing' stamp: 'ar 1/22/98 23:30'!
lastSystemActivity: aNumber
        lastSystemActivity _ aNumber! !

!MSWSystemController methodsFor: 'basic control sequence' stamp: 'ar 1/22/98
23:33'!
redButtonActivity       | box p |       p _ sensor cursorPoint.         ((box _ view
systemBoxFrame) containsPoint: p)               ifTrue: [^self systemActivity].         ((box _
view shrinkBoxFrame) containsPoint: p)          ifTrue: [Utilities awaitMouseUpIn: box
repeating: [] ifSucceed: [self collapse. ^ self].                               ^ self].        ((box _ view
growBoxFrame) containsPoint: p)                 ifTrue: [Utilities awaitMouseUpIn: box
repeating: [] ifSucceed:                                        [view isCollapsed                                               ifTrue:[self expand]
                                                ifFalse:[view isFullScreen ifTrue:[self restore] ifFalse:[self
fullScreen]].                                    ^ self].                               ^ self].

        super redButtonActivity.! !

!MSWSystemController methodsFor: 'basic control sequence' stamp: 'ar 1/22/98
23:33'!
systemActivity  "The system menu button has been pressed"       | time |
        time _ Time millisecondClockValue.      (time- self lastSystemActivity) < self
doubleClickTime                 ifTrue:[^self close].   self lastSystemActivity: time.  ^self
blueButtonActivity! !

!MSWSystemController methodsFor: 'menu messages' stamp: 'ar 1/22/98 22:41'!
restore
        view restore! !

!MSWSystemController methodsFor: 'private' stamp: 'ar 1/22/98 23:32'!
doubleClickTime
        "Return the maximum delay time for double clicks.
        This value is in milliseconds."
        ^500! !

!MSWSystemController methodsFor: 'borders' stamp: 'ssa 1/25/98 00:00'!
adjustWindowCorners 
        | box cornerBox p clicked f2 |
        box _ view windowBox.
        clicked _ false.
        #(topLeft topRight bottomRight bottomLeft)
                do: [:readCorner |
                        cornerBox _ ((box insetBy: 2) perform: readCorner) - (4@4) extent: 8@8.
                        (cornerBox containsPoint: sensor cursorPoint)
                                ifTrue: 
                                ["Display reverse: cornerBox."
                                (Cursor perform: readCorner) showWhile:
                                        [[(cornerBox containsPoint: (p _ sensor cursorPoint))
                                                and: [(clicked _ sensor anyButtonPressed) not]]
                                                whileTrue.
                                "Display reverse: cornerBox."
                                clicked ifTrue:
                                        [view newFrame:
                                                [:f | p _ sensor cursorPoint.
                                                readCorner = #topLeft ifTrue:
                                                        [f2 _ p corner: f bottomRight].
                                                readCorner = #bottomLeft ifTrue:
                                                        [f2 _ (f withBottom: p y) withLeft: p x].
                                                readCorner = #bottomRight ifTrue:
                                                        [f2 _ f topLeft corner: p].
                                                readCorner = #topRight ifTrue:
                                                        [f2 _ (f withTop: p y) withRight: p x].
                                                f2]]]]].
        ^ clicked! !

!MSWSystemController methodsFor: 'borders' stamp: 'ssa 1/24/98 23:47'!
cursorOnBorder 
        | cp i box |
        view isCollapsed ifTrue: [^ false].
        cp _ sensor cursorPoint.
        ((view labelDisplayBox insetBy: 5@5) containsPoint: cp)
                ifTrue: [^ false].
        (i _ view subViews findFirst: [:v | v displayBox containsPoint: cp]) = 0
                ifTrue: [box _ view windowBox]
                ifFalse: [box _ (view subViews at: i) insetDisplayBox].
        ^ ((box insetBy: 3) containsPoint: cp) not
                and: [(box expandBy: 4) containsPoint: cp]! !


!StandardSystemView class methodsFor: 'instance creation'!
new
        "This is a rather dirty hack -- but we don't have a window builder
yet. (ar 1/22/98 23:36)"

        ^Preferences nicerSystemViews
                ifTrue:[MSWSystemView basicNew initialize]
                ifFalse:[self basicNew initialize]! !


!MSWSystemView class methodsFor: 'class initialization' stamp: 'ssaq 2/27/98 11:17'!
initialize
        "MSWSystemView initialize"

        self install! !

!MSWSystemView class methodsFor: 'class initialization' stamp: 'ssa 2/27/98 11:02'!
initializeCache
        "MSWSystemView initializeCache"
        GrowBoxForm _ nil.
        ShrinkBoxForm _ nil.
        SystemBoxForm _ nil.
        CloseBoxForm _ nil.! !

!MSWSystemView class methodsFor: 'class initialization' stamp: 'ssa 2/27/98 11:02'!
install
        "MSWSystemView install"
        
        Preferences setPreference:#nicerSystemViews toValue: true.! !

!MSWSystemView class methodsFor: 'class initialization' stamp: 'ssa 2/27/98 11:02'!
unInstall
        "MSWSystemView unInstall"

        Preferences setPreference: #nicerSystemViews toValue: false.! !


!StringHolderView class methodsFor: 'instance creation'!
open: aStringHolder label: aString 
        "Create a standard system view of the model, aStringHolder, as viewed by 
        an instance of me. The label of the view is aString."
        "VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
        | aStringHolderView topView shv |
        aStringHolderView _ self container: aStringHolder.
        topView _ StandardSystemView new.
        topView model: aStringHolderView model.
        topView addSubView: (shv _ MSWScrollBarView on: aStringHolderView ).
        shv borderWidth: 2.
        topView label: aString.
        topView minimumSize: 100 @ 50.
        topView controller open! !


!PluggableTextView class methodsFor: 'instance creation' stamp: 'jm 3/29/98 07:24'!
on: anObject text: getTextSel accept: setTextSel

	^ self on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: nil
		menu: nil
! !

!PluggableTextView class methodsFor: 'instance creation' stamp: 'ssa 9/4/1998 16:04'!
on: anObject text: getTextSel accept: setTextSel readSelection: getSelectionSel menu: getMenuSel
	"Added Windoze wrapper - ssa"

	^ MSWScrollBarView on: (self new on: anObject
		text: getTextSel
		accept: setTextSel
		readSelection: getSelectionSel
		menu: getMenuSel)
! !


!View methodsFor: 'lock access'!
unlock
        "Unlock the receiver and all of its subViews (see View|isUnlocked). This 
        has the effect of forcing the display transformation (see 
        View|displayTransformation) and inset display box (see 
        View|insetDisplayBox) of the receiver and all its subViews to be 
        recomputed the next time they are referenced. The locking and 
        unlocking of a View is handled automatically by the internal methods of 
        the View, but can also be done explicitly if desired."
        "Added a guard clause to protect against empty subview collections.  This is one reason why lazy initialization and consistent use of accessor methods is better than direct access.  - ssa 2/27/98 11:26"

        self isUnlocked ifTrue: [^self].
        displayTransformation _ nil.
        insetDisplayBox _ nil.
        subViews isNil ifFalse:[subViews do: [:aSubView | aSubView unlock]]! !

!View methodsFor: 'scrolling support'!
percentContentVisible
        "Answer the percent of my content that is visible.  ssa 12/5/97 15:37"

        ^self visibleContentHeight / self totalContentHeight! !

!View methodsFor: 'scrolling support'!
percentPrecedingContent
        "Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

        ^0.0! !

!View methodsFor: 'scrolling support'!
percentPreceedingContent
        "Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

        ^0.0! !

!View methodsFor: 'scrolling support'!
percentVisibleContent
        "Answer the percent of my content that is visible.  ssa 12/5/97 15:37"

        ^self totalContentHeight = 0
                ifTrue:[0.0]
                ifFalse:[self visibleContentHeight / self totalContentHeight]! !

!View methodsFor: 'scrolling support'!
totalContentHeight
        "Answer the height of my content, visible or not.  ssa 12/5/97 15:04"

        ^100 / self unitContentHeight! !

!View methodsFor: 'scrolling support'!
unitContentHeight
        "Answer the unit height of my content.  ssa 12/5/97 15:04"

        ^10.0! !

!View methodsFor: 'scrolling support'!
visibleContentHeight
        "Answer the height of my visible content.  ssa 12/5/97 15:04"

        ^100 / self unitContentHeight! !


!ListView methodsFor: 'updating'!
update: aSymbol 
        "Refer to the comment in View|update:."
        "Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

        aSymbol == #list
                ifTrue: 
                        [self list: model list.
                        self displayView]
                ifFalse:[
                        aSymbol == #listIndex
                                ifTrue: 
                                        [self moveSelectionBox: model listIndex]].
        (self superView isKindOf: MSWScrollBarView)
                ifTrue:[self superView updateElevator]! !

!ListView methodsFor: 'scrolling support'!
percentPreceedingContent
        "Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"

        | para lineIndex |
        para _ self list.
        lineIndex _ para lineIndexOfTop: para visibleRectangle top.
        lineIndex = 1 ifTrue:[^0.0].
        ^lineIndex / para numberOfLines asFloat
! !

!ListView methodsFor: 'scrolling support'!
scrollBy: anInteger 
        "Scroll up by this amount adjusted by lineSpacing and list limits"
        "Hacked to support MSWScrollBarView - ssa 12/10/97 16:26"

        | maximumAmount minimumAmount amount |
        maximumAmount _ 0 max:
                list clippingRectangle top - list compositionRectangle top.
        minimumAmount _ 0 min:
                list clippingRectangle bottom - list compositionRectangle bottom.
        amount _ (anInteger min: maximumAmount) max: minimumAmount.
        amount ~= 0
                ifTrue: [list scrollBy: amount negated.
                                (self superView isKindOf: MSWScrollBarView)  "______HERE IS THE HACK"
                                        ifTrue:[self superView updateElevator].
                          ^ true]
                ifFalse: [^ false]  "Return false if no scrolling took place"! !

!ListView methodsFor: 'scrolling support'!
totalContentHeight
        "Answer the total height of my contents. ssa 12/5/97 15:16"
        ^ self list compositionRectangle height / self unitContentHeight! !

!ListView methodsFor: 'scrolling support'!
unitContentHeight
        "Answer the unit height of my contents. ssa 12/5/97 15:16"
        ^ self list lineGrid asFloat! !

!ListView methodsFor: 'scrolling support'!
visibleContentHeight
        "Answer the total height of my contents. ssa 12/5/97 15:16"
        ^ self list clippingRectangle height / self unitContentHeight! !


!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 16:49'!
deEmphasizeView

        self displayView.
        super deEmphasizeView! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:13'!
displayDownButton
        | box |
        box _ self downButtonBox.
        self downButtonCache extent = box extent
                ifFalse:[self drawDownButton].
        self downButtonCache displayOn: Display at: box origin! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
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
        bb copyBits! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:19'!
displayElevator
        | box |
        box _ self elevatorBox.
        self elevatorCache extent = box extent
                ifFalse:[self drawElevator].
        self elevatorCache displayOn: Display at: box origin! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/10/97 15:13'!
displayElevatorShaft

        | box |
        box _ self elevatorShaft.
        Display fill: box fillColor: Color lightGray.
! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:18'!
displayUpButton
        | box |
        box _ self upButtonBox.
        self upButtonCache extent = box extent
                ifFalse:[self drawUpButton].
        self upButtonCache displayOn: Display at: box origin! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
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
        bb copyBits! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 14:50'!
displayView

        self displayElevatorShaft.
        self displayUpButton.
        self displayDownButton.
        self displayElevator
! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
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
        self downButtonCache: form! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/11/97 13:20'!
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

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 1/20/98 21:45'!
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
        self upButtonCache: form! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 14:49'!
emphasizeView

        self displayView.
        super emphasizeView! !

!MSWScrollBarView methodsFor: 'displaying' stamp: 'ssa 12/5/97 16:08'!
updateElevator

        self displayElevatorShaft.
        self displayElevator! !

!MSWScrollBarView methodsFor: 'lock access' stamp: 'ssa 12/5/97 16:10'!
unlock
        "Flush the cache."
        self scrollBarBox: nil.
        self upButtonBox: nil.
        self downButtonBox: nil.
        self elevatorShaft: nil.
        super unlock! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:48'!
determineDownButtonBox
        "Answer the rectangle for the scroll bar down button."

        ^self scrollBarBox corner -  self scrollBarBox width asPoint extent: self scrollBarBox width asPoint! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:38'!
determineElevatorBox
        "Answer the rectangle for the scroll bar elevator."

        ^self scrollBarBox center -  (self scrollBarWidth asPoint // 2) extent: self scrollBarWidth asPoint! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 14:19'!
determineElevatorShaft
        "Answer the rectangle for the scroll bar down button."

        ^self upButtonBox bottomLeft corner: self downButtonBox topRight! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:47'!
determineScrollBarBox
        "Answer the rectangle for the scroll bar region."

        ^(self realInsetDisplayBox areasOutside: self insetDisplayBox) first! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/5/97 12:52'!
determineUpButtonBox
        "Answer the rectangle for the scroll bar up button."

        ^self scrollBarBox origin extent: self scrollBarBox width asPoint! !

!MSWScrollBarView methodsFor: 'display box access'!
displayBox
        "tah -- (17 July 1989 6:37:46 pm ) -- Answer the real displayBox"

        ^self realInsetDisplayBox expandBy: borderWidth! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 2/27/98 11:10'!
insetDisplayBox
        "Answer the inset displayBox reduced by the horizontal space for the scroll bar"

       ^WhereToLocateScrollBars = #left
                        ifTrue:[self insetDisplayBoxLeft]
                        ifFalse:[self insetDisplayBoxRight]! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 2/27/98 11:09'!
insetDisplayBoxLeft
        "Answer the inset displayBox reduced by the horizontal space for the scroll bar"

        | box |
        box _ self realInsetDisplayBox.
        ^box origin extent: box width - (self borderWidth left  + self scrollBarWidth) @ box height

! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 2/27/98 11:10'!
insetDisplayBoxRight
        "Answer the inset displayBox reduced by the horizontal space for the scroll bar"
        "Changed to left-side scroll bars--stp."
        "MSWScrollBarView someInstance"

        | box |
        box _ self realInsetDisplayBox.
        ^((box left + self scrollBarWidth) @ box top)
                extent: (box width - (self borderWidth left + self scrollBarWidth) @ box height)
! !

!MSWScrollBarView methodsFor: 'display box access' stamp: 'ssa 12/15/97 14:50'!
realInsetDisplayBox
        "tah -- (17 July 1989 6:05:48 pm ) -- answer the real inset displayBox "

        ^super insetDisplayBox! !

!MSWScrollBarView methodsFor: 'subview additions' stamp: 'ssa 1/15/98 15:50'!
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

!MSWScrollBarView methodsFor: 'subview additions'!
on: aView
        "tah -- (17 July 1989 7:17:34 pm ) -- Add a subview to this view"

        self on: aView borderWidth: 0! !

!MSWScrollBarView methodsFor: 'subview additions'!
on: aView borderWidth: aBorderWidth
        "tah -- (17 July 1989 7:17:34 pm ) -- Add a subview to this view"

        self addSubView: aView in: (0@0 extent: (1@1)) borderWidth: aBorderWidth! !

!MSWScrollBarView methodsFor: 'display transformation'!
displayTransform: anObject 
        "Apply the display transformation of the receiver to anObject (see 
        View|displayTransformation) and answer the resulting scaled, translated 
        object. It is normally applied to Rectangles, Points, and other objects with 
        coordinates defined in the View's local coordinate system in order to get a 
        corresponding object in display coordinates."

        ^(self realDisplayTransformation applyTo: anObject) rounded! !

!MSWScrollBarView methodsFor: 'display transformation' stamp: 'ssa 12/15/97 14:49'!
displayTransformation
        "This is a hook for to get the real displayTransformation"

        ^self scrollBarWidth = 0
                ifTrue: [self realDisplayTransformation]
                ifFalse: [WindowingTransformation window: self getWindow viewport: (self realInsetDisplayBox expandBy: self borderWidth)]! !

!MSWScrollBarView methodsFor: 'display transformation'!
realDisplayTransformation
        "This is a hook for labeledView to get the real displayTransformation"

        ^super displayTransformation! !

!MSWScrollBarView methodsFor: 'delegation' stamp: 'ssa 1/23/98 14:56'!
doesNotUnderstand: aMessage
        "Here it comes, the dreaded doesNotUnderstand:  HACK   ssa 1/23/98 14:55"

        ^self scrollingView perform: aMessage selector withArguments: aMessage arguments! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 12:24'!
downButtonBox
        "<^hOf Rectangle>"
        "ssa 12/5/97 11:15 - Answer the instance variable, downButtonBox"

        downButtonBox isNil ifTrue:[self downButtonBox: self determineDownButtonBox].
        ^downButtonBox! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
downButtonBox: aRectangle 
        "<aRectangle: hOf Rectangle, ^self>"
        "ssa 12/5/97 11:15 - Set downButtonBox to be aRectangle."

        downButtonBox _ aRectangle! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
downButtonCache
        "<^hOf Form>"
        "ssa 12/11/97 11:43 - Answer the instance variable, downButtonCache"

        downButtonCache isNil ifTrue:[self downButtonCache: (Form extent:1@1)].
        ^downButtonCache! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:43'!
downButtonCache: aColorForm 
        "<aColorForm: hOf ColorForm, ^self>"
        "ssa 12/11/97 11:43 - Set downButtonCache to be aColorForm."

        downButtonCache _ aColorForm! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 16:29'!
elevatorBox
        "Answer the rectangle for the elevator."

        ^self controller elevatorBox! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
elevatorCache
        "<^hOf Form>"
        "ssa 12/10/97 16:32 - Answer the instance variable, elevatorCache"

        elevatorCache isNil ifTrue:[self elevatorCache: (Form extent:1@1)].
        ^elevatorCache! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/10/97 16:32'!
elevatorCache: aForm 
        "<aForm: hOf Form, ^self>"
        "ssa 12/10/97 16:32 - Set elevatorCache to be aForm."

        elevatorCache _ aForm! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:18'!
elevatorShaft
        "<^hOf Rectangle>"
        "ssa 12/5/97 14:18 - Answer the instance variable, elevatorShaft"

        elevatorShaft isNil ifTrue:[self elevatorShaft: self determineElevatorShaft].
        ^elevatorShaft! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 14:18'!
elevatorShaft: aRectangle 
        "<aRectangle: hOf Rectangle, ^self>"
        "ssa 12/5/97 14:18 - Set elevatorShaft to be aRectangle."

        elevatorShaft _ aRectangle! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 12:43'!
getWindow
        "This is here to break a recursive loop caused by the indirection of my display transformation."
        self window isNil ifTrue:[self window: Display boundingBox].
        ^self window! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:28'!
scrollBarBox
        "<^hOf Rectangle>"
        "ssa 12/5/97 11:15 - Answer the instance variable, scrollBarBox"

        scrollBarBox isNil ifTrue:[self scrollBarBox: self determineScrollBarBox].
        ^scrollBarBox! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
scrollBarBox: aRectangle 
        "<aRectangle: hOf Rectangle, ^self>"
        "ssa 12/5/97 11:15 - Set scrollBarBox to be aRectangle."

        scrollBarBox _ aRectangle! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/10/97 14:05'!
scrollBarWidth
        "<^hOf Integer>"
        "ssa 12/5/97 11:27 - Answer the instance variable, scrollBarWidth"

        scrollBarWidth isNil ifTrue:[self scrollBarWidth: 12].
        ^scrollBarWidth! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:27'!
scrollBarWidth: anInteger 
        "<anInteger: hOf Integer, ^self>"
        "ssa 12/5/97 11:27 - Set scrollBarWidth to be anInteger."

        scrollBarWidth _ anInteger! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 1/24/98 16:06'!
scrollingView

        ^self subViews isEmpty ifTrue:[nil] ifFalse:[self subViews first]! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 13:59'!
upButtonBox
        "<^hOf Rectangle>"
        "ssa 12/5/97 11:15 - Answer the instance variable, upButtonBox"

        upButtonBox isNil ifTrue:[self upButtonBox: self determineUpButtonBox].
        ^upButtonBox! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/5/97 11:15'!
upButtonBox: aRectangle 
        "<aRectangle: hOf Rectangle, ^self>"
        "ssa 12/5/97 11:15 - Set upButtonBox to be aRectangle."

        upButtonBox _ aRectangle! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:46'!
upButtonCache
        "<^hOf Form>"
        "ssa 12/11/97 11:43 - Answer the instance variable, upButtonCache"

        upButtonCache isNil ifTrue:[self upButtonCache: (Form extent:1@1)].
        ^upButtonCache! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 11:43'!
upButtonCache: aColorForm 
        "<aColorForm: hOf ColorForm, ^self>"
        "ssa 12/11/97 11:43 - Set upButtonCache to be aColorForm."

        upButtonCache _ aColorForm! !

!MSWScrollBarView methodsFor: 'accessing' stamp: 'ssa 12/11/97 12:44'!
window
        "This is here to break a recursive loop caused by the indirection of my display transformation."
        window isNil ifTrue:[self window: Display boundingBox].
        ^window! !

!MSWScrollBarView methodsFor: 'control defaults' stamp: 'stp 02/18/98 0-11:'!
defaultControllerClass

        ^MSWScrollBarController! !

!MSWScrollBarView methodsFor: 'testing' stamp: 'ssa 1/8/98 16:13'!
containsPoint: aPoint
        "Overriden to access my real insetDsiplayBox"

        ^ self realInsetDisplayBox containsPoint: aPoint! !

!MSWScrollBarView methodsFor: 'bordering' stamp: 'ssa 1/24/98 16:05'!
borderWidth: anything

        super borderWidth:1.
! !


!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:32'!
closeBoxFrame

        | boxHeight |
        boxHeight _ self labelDisplayBox height -  11.
        ^ Rectangle origin: (self labelDisplayBox topRight + (boxHeight negated-9@7)) extent:
boxHeight+2@boxHeight

        "NiceSystemView initializeCache"! !

!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:35'!
growBoxFrame

        | boxHeight |
        boxHeight _ self labelDisplayBox height -  11.
        ^ Rectangle origin: (self labelDisplayBox topRight + (2*boxHeight negated-12@7)) extent:
boxHeight+2@boxHeight

        "NiceSystemView initializeCache"! !

!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 2/6/98 14:24'!
labelHeight
        ^self isCollapsed
                ifFalse:[18 + 6]
                ifTrue:[(LabelStyle fontAt: 2) height + 14]! !

!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:56'!
labelTextRegion
        labelText == nil ifTrue: [^ self labelDisplayBox center extent: 0@0].
        ^ (labelText boundingBox
                        align: labelText boundingBox leftCenter
                        with: self labelDisplayBox leftCenter + (25@0))
                intersect: (self labelDisplayBox origin corner: self labelDisplayBox corner -
(55@0))! !

!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:34'!
shrinkBoxFrame

        | boxHeight |
        boxHeight _ self labelDisplayBox height -  11.
        ^ Rectangle origin: (self labelDisplayBox topRight + (3*boxHeight negated-15@7)) extent:
boxHeight+2@boxHeight

        "NiceSystemView initializeCache"! !

!MSWSystemView methodsFor: 'label accessing' stamp: 'ssa 1/24/98 23:17'!
systemBoxFrame

        | boxHeight |
        boxHeight _ self labelDisplayBox height - 11 .
        ^ Rectangle origin: (self labelDisplayBox origin + (7@7)) extent:
boxHeight@boxHeight

        "NiceSystemView initializeCache"! !

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

!MSWSystemView methodsFor: 'framing' stamp: 'ar 1/22/98 22:47'!
fullScreen
        | portRect |
        portRect _ self viewport.
        growFrame _ portRect topLeft - self labelOffset
                                corner: portRect corner.
        ^super fullScreen! !

!MSWSystemView methodsFor: 'framing' stamp: 'ar 1/22/98 22:49'!
restore
        self reframeTo: ( growFrame isNil ifTrue:[self initialFrame]
ifFalse:[growFrame])! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:46'!
deEmphasizeLabel
        "Un-Highlight the label."
        labelFrame height = 0 ifTrue: [^ self].  "no label"
        self displayLabelBackground: false.
        self displayLabelTextDeEmphasized.
        self displayLabelBorder! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 9/4/1998 16:40'!
displayDeEmphasized
        "Display this view with emphasis off.
        If windowBits is not nil, then simply BLT if possible,
                but force full display for top window so color is preserved."

	self displayBorder.
	super displayDeEmphasized.
	self deEmphasizeLabel! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 9/4/1998 16:39'!
displayEmphasized
        "Display with label highlighted to indicate that it is active."

	self displayBorder.
	super displayEmphasized.
	self emphasizeLabel ! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:42'!
displayLabelBackground: emphasized
        "Clear or emphasize the inner region of the label"

        emphasized
                ifTrue:[Display
                                        fill: self labelDisplayBox
                                        fillColor:(Color r: 0.0 g: 0.4 b: 0.4)]
                ifFalse:[Display
                                        fill: self labelDisplayBox
                                        fillColor:(Color r: 0.344 g: 0.344 b: 0.344).].
                self displayLabelBoxes! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:13'!
displayLabelBoxes
        CloseBoxForm ifNil:[CloseBoxForm _ self drawCloseBoxForm].
        GrowBoxForm ifNil:[GrowBoxForm _ self drawGrowBoxForm].
        ShrinkBoxForm ifNil:[ShrinkBoxForm _ self drawShrinkBoxForm].
        SystemBoxForm ifNil:[SystemBoxForm _ self drawSystemBoxForm].
        CloseBoxForm displayOn: Display at: self closeBoxFrame origin.
        GrowBoxForm displayOn: Display at: self growBoxFrame origin.
        self isCollapsed 
                ifFalse:[ShrinkBoxForm displayOn: Display at: self 
shrinkBoxFrame origin].

        SystemBoxForm displayOn: Display at: self 
systemBoxFrame origin.! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelText
        "The label goes in the center of the window"
        | labelRect |
        labelText foregroundColor: Color white
                        backgroundColor: (Color r: 0.0 g: 0.4 b: 0.4).
        labelRect _ self labelTextRegion.
        "Display fill: (labelRect expandBy: 3@0) fillColor: self labelColor."
        labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
                        rule: Form under fillColor: labelText fillColor! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelTextDeEmphasized
        | labelRect |
        labelText foregroundColor: Color gray
                        backgroundColor: (Color r: 0.344 g: 0.344 b: 0.344).
        labelRect _ self labelTextRegion.
        labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
                        rule: Form under fillColor: labelText fillColor! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:38'!
displayLabelTextEmphasized
        | labelRect |
        labelText foregroundColor: Color white
                        backgroundColor: (Color r: 0.0 g: 0.4 b: 0.4).
        labelRect _ self labelTextRegion.
        labelText displayOn: Display at: labelRect topLeft +(0@1) clippingBox: labelRect
                        rule: Form under fillColor: labelText fillColor! !

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

!MSWSystemView methodsFor: 'displaying' stamp: 'ar 1/22/98 22:10'!
drawGrowBoxForm
        | box pen form |
        form _ Form extent: self growBoxFrame extent depth: 8.
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
        pen color: Color black.
        pen place: box topLeft + (3@3).
        pen goto: box bottomLeft + (3@-3).
        pen goto: box bottomRight + (-3@-3).
        pen goto: box topRight + (-3@3).
        pen defaultNib: 2.
        pen place: box topRight + (-4@3).
        pen goto: box topLeft + (3@3).
        ^form! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ar 1/22/98 22:11'!
drawShrinkBoxForm
        | box pen form |
        form _ Form extent: self growBoxFrame extent depth: 8.
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
        pen color: Color black.
        pen defaultNib: 2.
        pen place: box bottomRight + (-4@-3).
        pen goto: box bottomLeft + (3@-3).
        ^form! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 23:09'!
drawSystemBoxForm
        | box pen form |
        form _ Form extent: self systemBoxFrame extent depth: 8.
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
        ^form! !

!MSWSystemView methodsFor: 'displaying' stamp: 'ssa 1/24/98 22:49'!
emphasizeLabel
        "Highlight the label."
        labelFrame height = 0 ifTrue: [^ self].  "no label"
        self displayLabelBackground: true.
        self displayLabelBoxes.
        self displayLabelTextEmphasized.
        self displayLabelBorder! !

!MSWSystemView methodsFor: 'bordering' stamp: 'ssa 9/4/1998 16:52'!
borderWidth: anInteger
	"Override to support standard borders"! !

!MSWSystemView methodsFor: 'bordering' stamp: 'ssa 2/6/98 14:22'!
displayBorder
        "Display the receiver's border (using the receiver's borderColor)."

        | box |
        self isCollapsed ifTrue:[^self].
        borderWidth = 0
                ifTrue:
                        [insideColor == nil
                                ifFalse: 
                                        [Display fill: self displayBox fillColor: self backgroundColor]]
                ifFalse:
                        [box _ self displayBox.
        "left edge"
        Display fill: (box origin extent: 1@(box height - 1))
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: 1@(box height - 2))
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: 2@(box height-4))
                fillColor: Color gray.
        Display fill: (box origin +(4@3) extent: 1@(box height-6))
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@4) extent: 1@(box height-8))
                fillColor: Color black.

        "right edge"
        Display fill: (box topRight -(1@0)extent: 1@(box height - 1))
                fillColor: Color black.
        Display fill: (box topRight +(-2@1) extent: 1@(box height - 2))
                fillColor: Color veryDarkGray.
        Display fill: (box topRight +(-4@2) extent: 2@(box height-4))
                fillColor: Color gray.
        Display fill: (box topRight +(-5@5) extent: 1@(box height-10))
                fillColor: Color veryLightGray.
        Display fill: (box topRight +(-6@6) extent: 1@(box height-12))
                fillColor: Color gray.

        "top edge"
        Display fill: (box origin extent: box width@1)
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: (box width - 2)@1)
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: (box width-4)@2)
                fillColor: Color gray.
        Display fill: (box origin +(4@4) extent: (box width-8)@1)
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@5) extent: (box width-10)@1)
                fillColor: Color black.

        "bottom edge"
        Display fill: (box bottomLeft -(0@1) extent: box width @1)
                fillColor: Color black.
        Display fill: (box bottomLeft +(1@-2) extent: (box width - 2)@1)
                fillColor: Color veryDarkGray.
        Display fill: (box bottomLeft +(2@-4) extent: (box width-4)@2)
                fillColor: Color gray.
        Display fill: (box bottomLeft +(4@-5) extent: (box width-7)@1)
                fillColor: Color veryLightGray.
        Display fill: (box bottomLeft +(5@-6) extent: (box width-9)@1)
                fillColor: Color gray.
                        insideColor == nil ifFalse:
                                [Display fill: self insetDisplayBox fillColor: self backgroundColor]]! !

!MSWSystemView methodsFor: 'bordering' stamp: 'ssa 2/6/98 14:21'!
displayCollaspedLabelBorder
        "Display the receiver's label border."

        | box |
        borderWidth = 0
                ifTrue:
                        [insideColor == nil
                                ifFalse: 
                                        [Display fill: self displayBox fillColor: self backgroundColor]]
                ifFalse:
                        [box _ self labelDisplayBox.
        "left edge"
        Display fill: (box origin extent: 1@(box height))
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: 1@(box height - 1))
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: 2@(box height - 2))
                fillColor: Color gray.
        Display fill: (box origin +(4@3) extent: 1@(box height - 3))
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@4) extent: 1@(box height - 4))
                fillColor: Color black.

        "right edge"
        Display fill: (box topRight -(1@0)extent: 1@(box height))
                fillColor: Color black.
        Display fill: (box topRight +(-2@1) extent: 1@(box height - 1))
                fillColor: Color veryDarkGray.
        Display fill: (box topRight +(-4@2) extent: 2@(box height - 2))
                fillColor: Color gray.
        Display fill: (box topRight +(-5@5) extent: 1@(box height - 5))
                fillColor: Color veryLightGray.
        Display fill: (box topRight +(-6@6) extent: 1@(box height - 6))
                fillColor: Color gray.

        "top edge"
        Display fill: (box origin extent: box width@1)
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: (box width - 2)@1)
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: (box width-4)@2)
                fillColor: Color gray.
        Display fill: (box origin +(4@4) extent: (box width-8)@1)
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@5) extent: (box width-10)@1)
                fillColor: Color black.

        "bottom edge"
        Display fill: (box bottomLeft -(0@1) extent: box width - 5 @1)
                fillColor: Color black.
        Display fill: (box bottomLeft +(1@-2) extent: (box width - 2)@1)
                fillColor: Color veryDarkGray.
        Display fill: (box bottomLeft +(2@-4) extent: (box width-4)@2)
                fillColor: Color gray.
        Display fill: (box bottomLeft +(4@-5) extent: (box width-7)@1)
                fillColor: Color veryLightGray.
        Display fill: (box bottomLeft +(5@-6) extent: (box width-9)@1)
                fillColor: Color gray]! !

!MSWSystemView methodsFor: 'bordering' stamp: 'ssa 2/6/98 14:17'!
displayLabelBorder
        "Display the receiver's label border."

        | box |
        self isCollapsed ifTrue:[^self displayCollaspedLabelBorder].
        borderWidth = 0
                ifTrue:
                        [insideColor == nil
                                ifFalse: 
                                        [Display fill: self displayBox fillColor: self backgroundColor]]
                ifFalse:
                        [box _ self labelDisplayBox.
        "left edge"
        Display fill: (box origin extent: 1@(box height))
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: 1@(box height))
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: 2@(box height))
                fillColor: Color gray.
        Display fill: (box origin +(4@3) extent: 1@(box height))
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@4) extent: 1@(box height))
                fillColor: Color black.

        "right edge"
        Display fill: (box topRight -(1@0)extent: 1@(box height))
                fillColor: Color black.
        Display fill: (box topRight +(-2@1) extent: 1@(box height))
                fillColor: Color veryDarkGray.
        Display fill: (box topRight +(-4@2) extent: 2@(box height))
                fillColor: Color gray.
        Display fill: (box topRight +(-5@5) extent: 1@(box height))
                fillColor: Color veryLightGray.
        Display fill: (box topRight +(-6@6) extent: 1@(box height))
                fillColor: Color gray.

        "top edge"
        Display fill: (box origin extent: box width@1)
                fillColor: Color gray.
        Display fill: (box origin +(1@1) extent: (box width - 2)@1)
                fillColor: Color veryLightGray.
        Display fill: (box origin +(2@2) extent: (box width-4)@2)
                fillColor: Color gray.
        Display fill: (box origin +(4@4) extent: (box width-8)@1)
                fillColor: Color veryDarkGray.
        Display fill: (box origin +(5@5) extent: (box width-10)@1)
                fillColor: Color black]! !

!MSWSystemView methodsFor: 'testing' stamp: 'ar 1/22/98 22:07'!
cacheBitsAsTwoTone
        ^false! !

!MSWSystemView methodsFor: 'testing' stamp: 'ar 1/22/98 22:37'!
isFullScreen
        | frame |
        frame _ model fullScreenSize.
        ^(frame topLeft + self labelOffset corner: frame corner) = self viewport
! !

!MSWSystemView methodsFor: 'controller access' stamp: 'stp 02/18/98 0-11:'!
defaultControllerClass
        ^MSWSystemController! !

!MSWSystemView methodsFor: 'initialize' stamp: 'ssa 9/4/1998 16:50'!
initialize
        "change the default border to 5 to support Windoze style edges.  ssa 1/24/98 15:43"


        super initialize. 
        self borderWidthLeft: 5 right: 5 top: 0 bottom: 5! !

!MSWSystemView methodsFor: 'window access' stamp: 'ssa 1/24/98 15:56'!
defaultWindow
        "Build the minimum Rectangle that encloses all the windows of the 
        receiver's subViews. The answer is a Rectangle obtained by expanding 
        this minimal Rectangle by the borderWidth of the receiver. If the 
        receiver has no subViews, then a Rectangle enclosing the entire display 
        screen is answered. It is used internally by View methods if no window 
        has been specified for the View. Specialized subclasses of View should 
        redefine View|defaultWindow to handle the default case for instances 
        that have no subViews."

        | aRectangle |
        subViews isEmpty ifTrue: [^DisplayScreen boundingBox].
        aRectangle _ self firstSubView viewport.
        subViews do: [:aView | aRectangle _ aRectangle merge: aView viewport].
        ^aRectangle! !


!PluggableListView methodsFor: 'updating' stamp: 'ssa 9/4/1998 15:57'!
update: aSymbol 
	"Refer to the comment in View|update:."
	"Hacked to support Windoze - ssa"
	aSymbol == getListSelector ifTrue:
		[self list: self getList.
		self displayView.
		self displaySelectionBox].
	aSymbol == getSelectionSelector ifTrue:
		[self moveSelectionBox: self getCurrentSelectionIndex].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!StringHolderView methodsFor: 'updating'!
updateDisplayContents
        "Make the text that is displayed be the contents of the receiver's model."
        "VIVA LA JUNTA!!!!  hack this to update the scroll bar when the contents changes  - ssa 1/15/98 14:39"

        self editString: model contents.
        self displayView.
        (self superView isKindOf: MSWScrollBarView)
                ifTrue:[self superView updateElevator]! !

!StringHolderView methodsFor: 'scrolling support'!
percentPreceedingContent
        "Answer the percent of my content that not visible since it has been scrolled of the top of the screen.  ssa 12/5/97 15:37"
        | para lineIndex |
        para _ self displayContents.
        lineIndex _ para lineIndexOfTop: para visibleRectangle top.
        lineIndex = 1 ifTrue:[^0.0].
        ^lineIndex / para numberOfLines asFloat
! !

!StringHolderView methodsFor: 'scrolling support'!
totalContentHeight
        "Answer the total height of my contents. ssa 12/5/97 15:16"
        ^ self displayContents compositionRectangle height / self unitContentHeight! !

!StringHolderView methodsFor: 'scrolling support'!
unitContentHeight
        "Answer the unit height of my contents. ssa 12/5/97 15:16"
        ^ self displayContents lineGrid asFloat! !

!StringHolderView methodsFor: 'scrolling support'!
visibleContentHeight
        "Answer the total height of my contents. ssa 12/5/97 15:16"
        ^ self displayContents clippingRectangle height / self unitContentHeight! !


MSWScrollBarView initialize!
MSWSystemView initialize!

