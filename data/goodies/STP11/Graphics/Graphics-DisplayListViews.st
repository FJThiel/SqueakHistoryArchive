"Graphics-DisplayListViews.st
This is a very simple display list editor (i.e., structured graphics drawing program) that lets one scroll, select, drag and re-size items in a display list. It is not intended for general use, but rather to serve as the basis for more sophisticated tools that are implemented as subclasses.
	The red mouse button selects, drags, and rubber-bands items in the display list.
	The yellow button brings up a simple menu of basic functions ()only redraw has been tested so far).
	The blue button is used for 'drag-scrolling' of the view--try it...
	For a simple demo, try
		DisplayListView openOnList: (DisplayLine linesX: 800 byY: 800)
		DisplayListView openOnList: (DisplayRectangle rectanglesX: 800 byY: 800)
		DisplayListView openOnList: (DisplayRectangle rectanglesX: 800 byY: 800) 
				size: 400@300 withGrid: 32@16 inColor: Color lightBlue

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.01 BETA
    PREREQUISITES   Graphics-DisplayLists.st, Kernel-Values.st, STP system additions
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    July 8, 1997"!

MouseMenuController subclass: #DisplayListController
	instanceVariableNames: 'selection actions keys '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayListViews'!
DisplayListController comment:
'Instances of DisplayListController (or its subclasses) are used by DisplayListViews (or subclass instances) for menu management and selection.By default, a simple YellowButton menu allows for group/inspect/redraw/zoom.The RedButton is used for display item selection (by pointing or boxing-in).LeftShift extends the selection, and dragging a selection moves it.Instance Variables:
	selection		<DisplayList> the object''s selected items'!

!DisplayListController methodsFor: 'initialize-release'!
initialize
	"Set up the receiver's action and key dictionaries"

	super initialize.
	self initializeMenu.
	self initializeKeys.
	self initializeActions.! !

!DisplayListController methodsFor: 'initialize-release'!
initializeActions
	"Initialize up the receiver's action dictionary."

	actions := Dictionary new.
	actions at: #click put: #select.
	actions at: #doubleClick put: #deselect.
	actions at: #shiftClick put: #extendSelection.
	actions at: #ctrlClick put: #delete.
	actions at: #orangeClick put: #selectAll.
	actions at: #drag put: #drag.
	actions at: #shiftDrag put: #drag.
	actions at: #ctrlDrag put: #drag.! !

!DisplayListController methodsFor: 'initialize-release'!
initializeKeys
	"Initialize up the receiver's keys dictionary."

	keys := Dictionary new.
	keys at: #Left put: #scrollLeft.
	keys at: #Right put: #scrollRight.
	keys at: #Up put: #scrollUp.
	keys at: #Down put: #scrollDown.
	keys at: #Space put: #delete.
	keys at: #Tab put: #scrollRight.! !

!DisplayListController methodsFor: 'initialize-release'!
initializeMenu
	"Set up the receiver's YellowButtonMenu"

	self yellowButtonMenu: (PopUpMenu labels: 'group
ungroup
inspect
redraw
zoom' lines: #(2 4))
		yellowButtonMessages: #(group ungroup doInspect redraw zoom)! !


!DisplayListController methodsFor: 'accessing'!
selection
	"Answer the receiver's 'selection'."

	^selection! !

!DisplayListController methodsFor: 'accessing'!
selection: anObject
	"Set the receiver's instance variable 'selection' to be anObject."

	selection := anObject! !


!DisplayListController methodsFor: 'control defaults'!
blueButtonActivity
	"Drag scroll amplified by 2@2 with the blue button."

	| pt1 pt2 |
	pt1 := sensor cursorPoint.
	[sensor anyButtonPressed] whileTrue:
		[pt2 := sensor cursorPoint.
		(pt2 = pt1) 
			ifFalse: [view scrollBy: (pt2 - pt1) * 2.
"				Transcript show: (pt2 - pt1) printString; cr."
				pt1 := pt2]]! !

!DisplayListController methodsFor: 'control defaults'!
controlActivity

	self viewHasCursor
		ifFalse: [^super controlActivity].
	((self sensor redButtonPressed) & (self sensor yellowButtonPressed))
		ifTrue: [^self orangeButtonActivity].
	self sensor redButtonPressed
		ifTrue: [^self redButtonActivity].
	self sensor yellowButtonPressed
		ifTrue: [^self yellowButtonActivity].
	self sensor blueButtonPressed
		ifTrue: [^self blueButtonActivity].
	self sensor keyboardPressed
		ifTrue: [^self processKeyboard]! !

!DisplayListController methodsFor: 'control defaults'!
extendSelection
	"Hit-detect items from the display list."

	| pt item |
	model == nil ifTrue: [^nil].
	sensor waitNoButton.
	pt := sensor cursorPoint.
"	Transcript show: 'Select at: ', pt printString; cr."
	item := view list itemWithin: 1 of: pt - view class inset.
"	Transcript show: 'ExtSelect ', item printString; cr."
	item == nil
		ifFalse: [model addSelection: item]! !

!DisplayListController methodsFor: 'control defaults'!
orangeButtonActivity
	"Do the default orange-click action."

	self performAction: (keys at: #orangeClick)! !

!DisplayListController methodsFor: 'control defaults'!
redButtonActivity
	"Hit-detect items from the display list or drag a selection."

	(Delay forMilliseconds: 200) wait.		"wait to see if it's a click or a drag."
	self sensor anyButtonPressed
		ifTrue: [self sensor leftShiftDown
				ifTrue: [^self performAction: #shiftDrag].
			self sensor controlKeyPressed
				ifTrue: [^self performAction: #ctrlDrag].
			^self performAction: #drag]
		ifFalse: [self sensor leftShiftDown
				ifTrue: [^self performAction: #shiftClick].
			self sensor controlKeyPressed
				ifTrue: [^self performAction: #ctrlClick].
			^self performAction: #click]! !


!DisplayListController methodsFor: 'menu messages'!
doInspect
	"Inspect the model (if leftShiftDown) or the model's list."

	sensor leftShiftDown
		ifTrue: [model inspect]
		ifFalse: [model list inspect]! !

!DisplayListController methodsFor: 'menu messages'!
group
	"Group the current selections together."

	model group! !

!DisplayListController methodsFor: 'menu messages'!
redraw
	"Redraw the view and clear the selection."

	view displayView.
	model == nil
		ifFalse: [model select: nil]! !

!DisplayListController methodsFor: 'menu messages'!
ungroup
	"Ungroup the current selection (if possible)."

	model ungroup! !

!DisplayListController methodsFor: 'menu messages'!
zoom

	model doZoom

	"Prompt the user for a scale point, zoom the view's scale, and redraw."
"	| pt prompt |
	prompt := (view scaleX == nil)
		ifTrue: ['1.5@1.5']
		ifFalse: [(view scaleX @ view scaleY) printString].
	pt := DialogView request: 'zoom to scale:'
			initialAnswer: prompt.
	pt = '' ifFalse: [pt := Compiler evaluate: pt.
			pt class == Point
				ifTrue: [view scaleX: pt x; scaleY: pt y]]
"! !


!DisplayListController methodsFor: 'selecting'!
drag
	"Drag the selection."

	| pt0 dl |
	dl := model selection.
	dl == nil ifTrue: [^self].
	pt0 := view relativePointFor: sensor cursorPoint.
	dl selectionHandles do:
		[ :sh |
		(pt0 - sh) r < 4
			ifTrue: [^self rubberBandSelection: dl corner: sh while: [sensor anyButtonPressed]]].
	^self dragSelection: dl while: [sensor anyButtonPressed]! !

!DisplayListController methodsFor: 'selecting'!
dragSelection: dl while: dBlock
	"Drag the selection for as long as the given block is true."

	| pt0 pt1 |
	pt0 := sensor cursorPoint.
	dBlock whileTrue:
		[pt1 := sensor cursorPoint.
		pt0 = pt1
			ifFalse: [dl translateBy: (pt1 - pt0).
				pt0 := pt1.
				view displayView]]! !

!DisplayListController methodsFor: 'selecting'!
rubberBandSelection: dl corner: sh while: dBlock
	"Rubber-band one corner of the selection for as long as the given block is true."

	| pt0 pt1 operator |
	operator := dl operatorForCorner: sh.
	pt0 := view relativePointFor: sensor cursorPoint.
	dBlock whileTrue:
		[pt1 := view relativePointFor: sensor cursorPoint.
		pt0 = pt1
			ifFalse: [dl perform: operator with: pt1.
				pt0 := pt1.
				view displayView]]! !

!DisplayListController methodsFor: 'selecting'!
select
	"Hit-detect items from the display list."

	| pt item |
	model == nil ifTrue: [^nil].
	sensor waitNoButton.
	pt := view relativePointFor: sensor cursorPoint.
"	Transcript show: 'Select: ', pt printString; cr."
	item := view list itemWithin: 4 of: pt.
"	Transcript show: 'Select ', item printString; cr."
	model select: item! !

!DisplayListController methodsFor: 'selecting'!
selectFromPoint: aPoint toPoint: anotherPoint
	"Hit-detect the model for the given range."

	| item |
	(aPoint dist: anotherPoint) < 8
		ifTrue: [item := view list itemWithin: 24 of: aPoint.
			Transcript show: 'Select ', item printString; cr.
			item == nil
				ifTrue: [model select: item]
				ifFalse: [model select: item]]
		ifFalse: [model select:
				(view list itemsWithin: 
						(aPoint corner: anotherPoint))]! !

!DisplayListController methodsFor: 'selecting'!
selectRangeWhile: durationBlock
	"Hit-detect items from the display list or drag a selection as long as the block is true."

	| pt1 pt2 gc color1 color2 |
	gc := view graphicsContext.
	color1 := LookPreferences defaultForWindows backgroundColor.
	color2 := LookPreferences defaultForWindows foregroundColor.
	pt1 := sensor cursorPoint.
	pt2 := pt1 + (4@4).
	sensor cursorPoint: pt2.
	durationBlock whileTrue:
		[gc paint: color1.
		gc displayRectangularBorder: (pt1 corner: pt2).
		pt2 := sensor cursorPoint.
		gc paint: color2.
		gc displayRectangularBorder: (pt1 corner: pt2)]! !


!DisplayListController methodsFor: 'private'!
performAction: actionSymbol
	"Do the given action safely."

	(actions includesKey: actionSymbol)
		ifFalse: [^self].
	^self perform: (actions at: actionSymbol)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayListController class
	instanceVariableNames: ''!


Model subclass: #DisplayListEditor
	instanceVariableNames: 'selection displayList copyBuffer view controller '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-DisplayListViews'!
DisplayListEditor comment:
'An instance of DisplayListEditor is used as the intermediary model for viewing lists.
It handles selection and operation on the list.
It adds itself as a dependent of the list and echoes its change messages (i.e., the update: message says self changed).

Instance Variable:
	list			<DisplayList> the ''subject'' list model
	selection		<DisplayItem orDisplayLIst> the current selection or nil
'!

!DisplayListEditor methodsFor: 'initialize-release'!
release
	"Remove the dependency to the displayList."

	displayList removeDependent: self.
	displayList release! !


!DisplayListEditor methodsFor: 'accessing'!
addSelection: sel
	"Add the argument to the receiver's selection."

	| dl |
"	sel == nil ifTrue: [Transcript show: 'clear'; cr]."
	selection hasItems
		ifTrue: [selection add: sel]
		ifFalse: [dl := DisplayList new.
			dl add: selection.
			dl add: sel.
			selection := dl].
	self changed: #selection with: sel! !

!DisplayListEditor methodsFor: 'accessing'!
displayList
	"Answer the receiver's list."

	^displayList! !

!DisplayListEditor methodsFor: 'accessing'!
list
	"Answer the receiver's display list."

	^displayList! !

!DisplayListEditor methodsFor: 'accessing'!
list: aList
	"Set the receiver's display list, removing the dependency to the former list if necessary."

	displayList == nil
		ifFalse: [displayList removeDependent: self].
	displayList := aList.
	displayList addDependent: self! !

!DisplayListEditor methodsFor: 'accessing'!
moveSelectionTo: newPoint
	"Move the editor's selection to the new point in the displayList."

	| oldOff |
	selection hasItems
		ifFalse: [oldOff := selection origin.
			selection origin: newPoint.
			self changed": #position with: (oldOff corner: newPoint)"]
		ifTrue: [selection components do: [ : i | displayList remove: i].
			selection origin: newPoint.
			displayList add: selection.
			self changed]! !

!DisplayListEditor methodsFor: 'accessing'!
select: sel
	"Set the receiver's selection."

"	sel == nil ifTrue: [Transcript show: 'clear'; cr]."
	selection := sel.
	self changed: #selection with: sel! !

!DisplayListEditor methodsFor: 'accessing'!
selection
	"Answer the receiver's selection."

	^selection! !


!DisplayListEditor methodsFor: 'updating'!
update: anAspect with: anArg
	"Echo the model's changes, assuming a view is a dependent of the receiver."

	self changed: anAspect with: anArg! !


!DisplayListEditor methodsFor: 'editing'!
copy
	"Copy the selection into the copy buffer."

	copyBuffer := selection! !

!DisplayListEditor methodsFor: 'editing'!
doDisplay
	"inspect button"

	self changed! !

!DisplayListEditor methodsFor: 'editing'!
doEdit
	"edit button"

	self doInspect! !

!DisplayListEditor methodsFor: 'editing'!
doFile
	"file i/o button"

	Transcript show: 'No File.'; cr.! !

!DisplayListEditor methodsFor: 'editing'!
doInspect
	"Inspect the editor (if shiftDown) or the model."

	InputState default shiftDown
		ifTrue: [self inspect]
		ifFalse: [displayList inspect]! !

!DisplayListEditor methodsFor: 'editing'!
doZoom
	"Zoom in or out by 25%."

	InputState default shiftDown
		ifTrue: [self changed: #scaleX with: 0.75.
			self changed: #scaleY with: 0.75]
		ifFalse: [self changed: #scaleX with: 1.25.
			self changed: #scaleY with: 1.25].
	self changed! !

!DisplayListEditor methodsFor: 'editing'!
group
	"Group the selections together."

	| dl |
	selection == nil
		ifTrue: [^self].
	selection hasItems
		ifFalse: [^self].
	dl := DisplayList new.
	selection do:
		[ :item | dl add: item.
			displayList remove: item].
	displayList add: dl.
	self select: (DisplayList new add: dl)! !

!DisplayListEditor methodsFor: 'editing'!
ungroup
	"Ungroup the selection (if possible)."

	selection == nil
		ifTrue: [^self].
	selection hasItems
		ifFalse: [^self].
	displayList remove: selection components first.
	selection components first do:
		[ :item | displayList add: item].
	self select: selection components first! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayListEditor class
	instanceVariableNames: ''!

!DisplayListEditor class methodsFor: 'instance creation'!
on: model
	^self new list: model! !


View subclass: #DisplayListView
	instanceVariableNames: 'displayList form background scrollOffset scaleX scaleY backgroundColor foregroundColor '
	classVariableNames: 'SelectionHandle '
	poolDictionaries: ''
	category: 'Graphics-DisplayListViews'!
DisplayListView comment:
'DisplayListViews can display and scroll structured graphics display lists generated by various models or layout managers.
Using their page offset, they can scroll over very large display lists.
They can have colored or gridded background pixmaps and can scroll/page on demand; they display their lists on their graphics contexts.

Subclasses generally override the displayOn: or displayOnImage methods, and add initialization or transformation methods.

Instance Variables
	displayList		<DisplayList> the view''s display list
	form			<Form> the view''s cached display pixel map (optional)
	background		<Form or nil> the view''s background form (e.g., gridding)
	scaleX/Y			<Numbers or nil> the display list''s zoom-in factor or nil
	backgroundColor	<ColorValue> graphics background color
	foregroundColor		<ColorValue> graphics display color
	scrollOffset		<Point> the view scroll origin

See the class examples for numerous ways of using display list views.
The subclasses add knowledge of smart display list generation, background pixmap generation (e.g., gridding), display of item or x/y-range selection, x- or y-scaling or step/grid, property->color mapping, "clef forms" or other special pixmaps, multiple-model viewing, etc.
'!

!DisplayListView methodsFor: 'initialize-release'!
initialize
	"Set up the default values of the receiver."

	super initialize.
	scaleX := 1.
	scaleY := 1.
	scrollOffset := 0@0! !

!DisplayListView methodsFor: 'initialize-release'!
initializeWithGrid: gPoint color: gColor
	"Initialize the receiver's instance variables--the default is to cache."

	| cacheE line color |
	self initialize.
	cacheE := self class defaultCacheExtent.
	background := Form extent: cacheE depth: Display depth.
	background fillColor: DisplayListView backgroundColor.
	gColor isNil
		ifTrue: [color := Color black]
		ifFalse: [color := gColor].
	line _ BitBlt
		destForm: background
		sourceForm: nil
		fillColor: color
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: 1@1
		clipRect: (0@0 extent: cacheE).
	gPoint x to: cacheE x by: gPoint x do:
		[ :x | line drawFrom: (x@0) to: (x @ cacheE y)].
	gPoint y to: cacheE y by: gPoint y do:
		[ :y | line drawFrom: (0@y) to: (cacheE x @ y)].! !

!DisplayListView methodsFor: 'initialize-release'!
release
	"Remove the receiver as a dependent of its model."

	model release.
	model removeDependent: self.
	model := nil.
	displayList release.
	super release! !


!DisplayListView methodsFor: 'accessing'!
background: anObject
	"Set the receiver's 'background' to be anObject."

	background := anObject! !

!DisplayListView methodsFor: 'accessing'!
backgroundColor
	"Answer the receiver's backgroundColor or the default."

	^backgroundColor == nil
		ifTrue: [self class backgroundColor]
		ifFalse: [backgroundColor]! !

!DisplayListView methodsFor: 'accessing'!
boundingBox
	"Answer the displayList's boundingBox."

	| rect |
	displayList == nil
		ifTrue: [^super boundingBox].
	rect := displayList boundingBox.
	^(rect origin) extent: (rect extent + (self class inset * 2))! !

!DisplayListView methodsFor: 'accessing'!
displayList: aDisplayList
	"Set the receiver's display list."

	self list: aDisplayList! !

!DisplayListView methodsFor: 'accessing'!
foregroundColor
	"Answer the receiver's foregroundColor or the default."

	^foregroundColor == nil
		ifTrue: [self class foregroundColor]
		ifFalse: [foregroundColor]! !

!DisplayListView methodsFor: 'accessing'!
isColor
	"Answer whether or not to use colors in displaying."

	^Display depth > 1! !

!DisplayListView methodsFor: 'accessing'!
list
	"Answer the receiver's display list."

	^displayList! !

!DisplayListView methodsFor: 'accessing'!
list: aDisplayList
	"Set the receiver's display list."

	| ext |
	displayList := aDisplayList.
	ext := aDisplayList extent max: (self class defaultCacheExtent).
	form := Form extent: ext depth: 8! !

!DisplayListView methodsFor: 'accessing' stamp: 'stp 07/09/97 00:04'!
model: aDisplayListEditor
	"Accept the given display list editor as the model of the view."

	| ext |
	super model: aDisplayListEditor.
	displayList := model list.
	aDisplayListEditor displayList isNil
		ifFalse: [ext := aDisplayListEditor displayList extent max: (self class defaultCacheExtent).
			form := Form extent: ext depth: 8].	 "default backing store size--dangrous for huge lists"
! !

!DisplayListView methodsFor: 'accessing'!
offsetX
	"Answer the receiver's 'offsetX.'"

	^scrollOffset x! !

!DisplayListView methodsFor: 'accessing'!
offsetY
	"Answer the receiver's 'offsetY.'"

	^scrollOffset y! !

!DisplayListView methodsFor: 'accessing'!
scaleX
	"Answer the receiver's 'scaleX.'"

	^scaleX! !

!DisplayListView methodsFor: 'accessing'!
scaleX: aNumber
	"Set the receiver's 'scaleX' to be aNumber."

	scaleX = aNumber ifTrue: [^self].
	scaleX := aNumber.
	self invalidate! !

!DisplayListView methodsFor: 'accessing'!
scaleY
	"Answer the receiver's 'scaleY.'"

	^scaleY! !

!DisplayListView methodsFor: 'accessing'!
scaleY: aNumber
	"Set the receiver's 'scaleY' to be aNumber."

	scaleY = aNumber ifTrue: [^self].
	scaleY := aNumber.
	self invalidate! !

!DisplayListView methodsFor: 'accessing'!
setScrollOffsetHolder: aValueHolder

	scrollOffset := aValueHolder.! !


!DisplayListView methodsFor: 'controller access'!
defaultControllerClass

	^DisplayListController! !


!DisplayListView methodsFor: 'displaying'!
display: box onForm: theForm
	"Display the receiver's display list on the cached form."

	((scaleX = 1) & (scaleY = 1))
		ifTrue: [displayList displayOn: theForm at: 0@0 clippingBox: box]
		ifFalse: [(displayList scaledBy: scaleX@scaleY) displayOn: theForm at: 0@0 clippingBox: box].
! !

!DisplayListView methodsFor: 'displaying'!
displaySelection: sel on: aDisplayMedium at: aPoint
	"Display little white-bordered black 'handles' at the corners of the selection."

	| rect inset |
	sel == nil ifTrue: [^self].
	rect := sel boundingBox translateBy: aPoint.
	sel selectionHandles do:
		[ :pt |
		SelectionHandle displayOn: aDisplayMedium at: (pt + aPoint - (2@2))]! !

!DisplayListView methodsFor: 'displaying'!
displaySelectionOn: aDisplayMedium at: origin clippingBox: box
	"Display little white-bordered black 'handles' at the corners of the selection."

	| sel |
	sel := model selection.
	sel == nil ifTrue: [^self].
	sel hasItems
		ifFalse: [self displaySelection: sel on: aDisplayMedium at: origin]
		ifTrue: [sel do:
			[ :item | self displaySelection: item on: aDisplayMedium at: origin]]! !

!DisplayListView methodsFor: 'displaying'!
displayView
	"Display the receiver's background form and display list on the given medium."

	| box |
	super displayView.
	box := form boundingBox.
	form fillColor: self backgroundColor.
	background == nil
		ifFalse: [background displayOn: form].
	self display: box onForm: form.
	self displaySelectionOn: form at: 0@0 clippingBox: box.
	form displayOn: Display at: (self insetDisplayBox origin - scrollOffset) clippingBox: self insetDisplayBox! !


!DisplayListView methodsFor: 'updating'!
update: anAspect with: anArg
	"Echo the model's changes, assuming a view is a dependent of the receiver."

	anAspect == #scaleX
		ifTrue: [^scaleX := scaleX * anArg].
	anAspect == #scaleY
		ifTrue: [^scaleY := scaleY * anArg].
	anAspect == #selection
		ifTrue: [^self displayView].
	super update: anAspect with: anArg! !


!DisplayListView methodsFor: 'scrolling'!
relativePointFor: aPoint
	"Answer the transformed point representing the argument (faster than doing a full windowing transformation)"

	^aPoint - self insetDisplayBox origin + scrollOffset! !

!DisplayListView methodsFor: 'scrolling' stamp: 'stp 07/09/97 00:54'!
scrollBy: aPoint
	"Scroll the receiver's display by the given point."

	scrollOffset := scrollOffset - aPoint.
"	scrollOffset := scrollOffset max: (0@0).
	scrollOffset := scrollOffset min: (form extent - self insetDisplayBox extent).
"	self displayView! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DisplayListView class
	instanceVariableNames: ''!

!DisplayListView class methodsFor: 'instance creation'!
on: anEditor

	| me |
	me := self new initialize.
	me model: anEditor.
	^me! !

!DisplayListView class methodsFor: 'instance creation'!
onList: dlist
	"DisplayListView example"

	^self onList: dlist size: 400@300! !

!DisplayListView class methodsFor: 'instance creation'!
onList: dlist size: extent
	"Open a display list view on the given list."
	"DisplayListView example"

	^self onList: dlist size: extent withGrid: nil inColor: nil! !

!DisplayListView class methodsFor: 'instance creation'!
onList: dlist size: extent withGrid: gPoint inColor: gColor 
	"Open a DLView's window with a background grid"
	"DisplayListView onList: (DisplayList exampleHierarchical) 
			size: 400@300 withGrid: 32@32 inColor: Color lightGray"

	| topView container me wrapper editor |
	editor := DisplayListEditor on: dlist.
	me := self on: editor.
	gPoint == nil
		ifFalse: [me initializeWithGrid: gPoint color: gColor].
	topView := StandardSystemView new.
	topView model: editor.
	topView label: 'Display List Editor'.
	topView minimumSize: extent.
	topView borderWidth: 2.
	topView addSubView: me.
	topView controller open! !

!DisplayListView class methodsFor: 'instance creation'!
openOnList: dlist
	"IndentedListLayoutManager listExample display"
	"DisplayListView openOnList: (DisplayList linesX: 600 byY: 400)"

	| topView me editor |
	editor := DisplayListEditor on: dlist.
	me := self on: editor.
	topView := StandardSystemView new.
	topView model: editor.
	topView label: 'Display List Editor'.
	topView minimumSize: 300@200.
	topView maximumSize: 640@480.
	topView borderWidth: 2.
	topView addSubView: me.
	topView controller open! !

!DisplayListView class methodsFor: 'instance creation'!
openOnList: dlist size: extent withGrid: gPoint inColor: gColor 
	"Open a DLView's window with a background grid"
	"DisplayListView openOnList: (DisplayLine linesX: 800 byY: 800) 
			size: 400@300 withGrid: 64@32 inColor: Color lightGray"

	| topView container me wrapper editor |
	editor := DisplayListEditor on: dlist.
	me := self on: editor.
	gPoint == nil
		ifFalse: [me initializeWithGrid: gPoint color: gColor].
	topView := StandardSystemView new.
	topView model: editor.
	topView label: 'Display List Editor'.
	topView minimumSize: extent.
	topView borderWidth: 2.
	topView addSubView: me.
	topView controller open! !


!DisplayListView class methodsFor: 'class initialization'!
initialize
	"Set up the class variable."
	"DisplayListView initialize"

	SelectionHandle := Form extent: 4@4.
	SelectionHandle fill: ((0@0) extent: (4@4)) fillColor: Color white.
	SelectionHandle fill: ((1@1) extent: (2@2)) fillColor: Color black.! !


!DisplayListView class methodsFor: 'constant access'!
backgroundColor
	"Answer the default background color for display list views."
	"ColorValue constantNames"

	^Display depth > 1
		ifTrue: [Color gray]
		ifFalse: [Color white]! !

!DisplayListView class methodsFor: 'constant access'!
defaultCacheExtent
	"Answer the default extent of the receiver's instances' cache."

	^600@600! !

!DisplayListView class methodsFor: 'constant access'!
foregroundColor
	"Answer the default foreground color for display list views."

	^Display depth > 1
		ifTrue: [Color yellow]
		ifFalse: [Color black]! !

!DisplayListView class methodsFor: 'constant access'!
inset
	"Answer the default display inset."

	^0@0! !

!DisplayListView class methodsFor: 'constant access'!
selectionHandle
	"Answer the class's selection handle pixmap"

	^SelectionHandle! !


!DisplayListView class methodsFor: 'examples'!
classListExample
	"DisplayListView classListExample"

	IndentedListLayoutManager listExample display! !

!DisplayListView class methodsFor: 'examples'!
classTreeExample
	"DisplayListView classTreeExample"

	(IndentedTreeLayoutManager listExample) display! !

!DisplayListView class methodsFor: 'examples'!
colorClassListExample
	"DisplayListView colorClassListExample"

	IndentedListLayoutManager colorListExample! !

!DisplayListView class methodsFor: 'examples'!
example1
	"DisplayListView example1"

	DisplayListView openOnList: (DisplayLine linesX: 600 byY: 600)! !

!DisplayListView class methodsFor: 'examples'!
example2
	"DisplayListView example2"

	DisplayListView openOnList: (DisplayRectangle rectanglesX: 800 byY: 800)! !

!DisplayListView class methodsFor: 'examples'!
exampleGridded
	"Open an example of a gridded-background display list view on the example hierarchical display list."
	"DisplayListView exampleGridded"

	DisplayListView onList: (DisplayList exampleHierarchical)
			size: 400@300
			withGrid: 32@32
			inColor: Color lightGray! !

!DisplayListView class methodsFor: 'examples'!
exampleHierarchical
	"DisplayListView exampleHierarchical"

	self onList: (DisplayList exampleHierarchical) size: 400@400! !


!DisplayListView class methodsFor: 'documentation'!
about
	"Answer a string that describes the DisplayListView application."

	^'The DisplayListView is a general-purpose viewer/editor 
for MODE structured graphics display lists.
	The controller''s middle-button popup menu includes items 
for inspecting, redrawing, or adjusting the zoom or grid.'! !

!DisplayListView class methodsFor: 'documentation'!
README
	"This is a very simple display list editor (i.e., structured graphics drawing program) that lets one scroll, select, and re-size items in a display list. It is not intended for general use, but rather to serve as the basis for more sophisticated tools that are implemented as subclasses.
	The red mouse botton selects, drage, and rubber-bands items in the display list.
	The yellow button brings up a simple menu of basic functions ()only redraw has been tested so far).
	The blue button is used for 'drag-scrolling' of the view--try it!!"

	self flag: #Documentation! !


DisplayListView initialize!
