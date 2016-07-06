"      NAME	Minor fixes to Sam and Andreas's Windows look-and-feel
       AUTHOR	Stephen Pope (stp@create.ucsb.edu)
       URL	(none)
       FUNCTION	Stylistic and Look&Feel fixes to Windoze look
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 1.3
       CONFLICTS	This file-in breaks Squeaks subview resizing.
       DISTRIBUTION	world
       VERSION	1.3.0  (first version for Squeak 1.3)
       DATE	2/18/98

SUMMARY

Renames the classes and category in a more Smalltalk-like names, moves scroll bars to the left side of the pane, and a few fix-ups. This requires the 4 files of the SA/AR look junta:
	ScrollBars-1.3.1.cs
	ScrollJunta-1.3.1.cs
	Windoze-1.3.0.cs
	Windoze-1.3.2.cs

The files in this goodie are:
	SSA-to-MSW.rename.cs	--Rename SSA* and Nice* to MSW* (MS-Windows look)
								move to category Interface-MSWLook.
	MSWSystemView-closeBoxForm.st	--Make the 'x in the close box a bit lighter and 
								fix the collapsed view size.
	MSW-leftScrollBars.cs	--Move scroll bars to the left side of the window.
	MSWScrollAbsolute.st		--Fix scrollAbsolute so scroll bars don't flash.

"!


'From Squeak 1.31 of Feb 4, 1998 on 18 February 1998 at 12:10:49 am'!

Smalltalk renameClassNamed: #NiceSystemController as: #MSWSystemController!
Smalltalk renameClassNamed: #NiceSystemView as: #MSWSystemView!
Smalltalk renameClassNamed: #SSAScrollBarController as: #MSWScrollBarController!
Smalltalk renameClassNamed: #SSAScrollBarView as: #MSWScrollBarView!

!MSWScrollBarView methodsFor: 'control defaults' stamp: 'stp 02/18/98 0-11:'!
defaultControllerClass

	^MSWScrollBarController! !

!MSWSystemView methodsFor: 'controller access' stamp: 'stp 02/18/98 0-11:'!
defaultControllerClass
	^MSWSystemController! !


!ChangeList class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
open: aChangeList name: aString withListView: aListView
	"Create a standard system view for the messageSet, whose label is aString.
	The listView supplied may be either single or multiple selection type"
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| topView codeView lv cv |
	topView _ StandardSystemView new.
	topView model: aChangeList.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aListView model: aChangeList.
	aListView list: aChangeList list.
	aListView window: (0 @ 0 extent: 180 @ 100).
	topView addSubView: (lv _ MSWScrollBarView on: aListView ).
	lv borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	codeView _ StringHolderView new.
	codeView model: aChangeList.
	codeView window: (0 @ 0 extent: 180 @ 300).
	topView
		addSubView: ( cv _ MSWScrollBarView on: codeView )
		align: cv viewport topLeft
		with: lv viewport bottomLeft.
	cv borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	topView controller open ! !


!ChangeSorter methodsFor: 'creation' stamp: 'stp 02/18/98 0-11:'!
openView: topView
	"Create change sorter on one changeSet only.  Two of these in a DualChangeSorter."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| classView messageView codeView |

	buttonView _ SwitchView new.
	buttonView model: self controller: TriggerController new.
	buttonView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	buttonView selector: #whatPolarity.
	buttonView controller selector: #cngSetActivity.
	buttonView window: (0 @ 0 extent: 360 @ 20).
	buttonView label: myChangeSet name asParagraph.

	classView _ MSWScrollBarView on: GeneralListView new.
	classView scrollingView controllerClass: GeneralListController.
	classView scrollingView model: classList.
	classView scrollingView window: (0 @ 0 extent: 180 @ 160).
	classView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	classView scrollingView controller yellowButtonMenu: ClassMenu 
		yellowButtonMessages: ClassSelectors.
	classList scrollingView controller: classView controller.


	messageView _ MSWScrollBarView on: GeneralListView new.
	messageView scrollingView controllerClass: GeneralListController.
	messageView scrollingView model: messageList.
	messageView scrollingView window: (0 @ 0 extent: 180 @ 160).
	messageView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	messageView scrollingView controller yellowButtonMenu: MsgListMenu 
		yellowButtonMessages: MsgListSelectors.
	messageList scrollingView controller: messageView controller.

	codeView _ MSWScrollBarView on: BrowserCodeView new.
	codeView scrollingView model: self.
	codeView scrollingView window: (0 @ 0 extent: 360 @ 180).
	codeView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.

	topView addSubView: buttonView.
	topView addSubView: classView below: buttonView.
	topView addSubView: messageView toRightOf: classView.
	topView addSubView: codeView below: classView.
"
	classView 
		align: classView viewport topLeft 	
		with: buttonView viewport bottomLeft.
	messageView 
		align: messageView viewport topLeft 	
		with: classView viewport topRight.
	codeView 
		align: codeView viewport topLeft 	
		with: classView viewport bottomLeft.
"! !

!ChangeSorter methodsFor: 'creation' stamp: 'stp 02/18/98 0-11:'!
openView: topView offsetBy: offset
	"Create change sorter on one changeSet with 0@0.
	Two of these in a DualChangeSorter, right one is offset by 360@0."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| classView messageView codeView |
	buttonView _ SwitchView new.
	buttonView model: self controller: TriggerController new.
	buttonView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	buttonView selector: #whatPolarity.
	buttonView controller selector: #cngSetActivity.
	buttonView window: ((0 @ 0 extent: 360 @ 20) translateBy: offset).
	buttonView label: myChangeSet name asParagraph.

	classView _ MSWScrollBarView on: GeneralListView new.
	classView scrollingView controllerClass: GeneralListController.
	classView scrollingView model: classList.
	classView window: (0 @ 0 extent: 180 @ 160).
	classView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	classView scrollingView controller yellowButtonMenu: ClassMenu 
		yellowButtonMessages: ClassSelectors.
	classList  controller: classView scrollingView controller.

	messageView _ MSWScrollBarView on: GeneralListView new.
	messageView scrollingView controllerClass: GeneralListController.
	messageView scrollingView model: messageList.
	messageView window: (0 @ 0 extent: 180 @ 160).
	messageView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	messageView scrollingView controller yellowButtonMenu: MsgListMenu 
		yellowButtonMessages: MsgListSelectors.
	messageList  controller: messageView scrollingView controller.

	codeView _ MSWScrollBarView on: BrowserCodeView new.
	codeView scrollingView model: self.
	codeView window: (0 @ 0 extent: 360 @ 180).
	codeView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.

	topView addSubView: buttonView.
	topView addSubView: classView below: buttonView.
	topView addSubView: messageView toRightOf: classView.
	topView addSubView: codeView below: classView.
! !


!DisplayTextView class methodsFor: 'examples' stamp: 'stp 02/18/98 0-11:'!
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


!FileModel class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
open: aFileModel named: aString 
	"Answer a scheduled view whose model is aFileModel and whose label is aString. "
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| topView aView |
	topView _ StandardSystemView new.
	topView model: aFileModel.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aView _ MSWScrollBarView on: (FileView new model: aFileModel).
	aView window: (0 @ 0 extent: 180 @ 120).
	aView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 2.
	topView addSubView: aView.
	topView controller open! !


!FileList class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
openWithEditPane: withEdit  "FileList open"
	"Open a view of an instance of me on the default directory.   2/14/96 sw: use standard directory.  (6/96 functionality substantially changed by di)
	 7/12/96 sw: set the label to the pathname"
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| topView aTemplateView fileListView aFileView aFileList aFileTemplateHolder dir volListView |
	topView _ StandardSystemView new.
	aFileList _ self new directory: (dir _ FileDirectory default).
	topView model: aFileList.
	topView label: dir pathName.
	topView minimumSize:
		200 @ (withEdit ifTrue: [200] ifFalse: [60]).

	volListView _ MSWScrollBarView on:ListView new.
	volListView scrollingView model: aFileList.
	volListView scrollingView list: aFileList list.
	volListView window: (0 @ 0 extent: 80 @ 45).
	volListView borderWidthLeft: 2 right: 1 top: 2 bottom: 1.
	topView addSubView: volListView.

	aFileTemplateHolder _ FileTemplateHolder on: aFileList.
	aTemplateView _ MSWScrollBarView on:StringHolderView new.
	aTemplateView scrollingView controller: FileTemplateController new.
	aTemplateView scrollingView model: aFileTemplateHolder.
	aTemplateView window: (0 @ 0 extent: 80 @ 15).
	aTemplateView borderWidthLeft: 2 right: 1 top: 1 bottom: 1.
	topView addSubView: aTemplateView below: volListView.

	fileListView _ MSWScrollBarView on:FileListView new.
	fileListView scrollingView model: aFileList.
	fileListView scrollingView controller: FileListController new.
	fileListView scrollingView list: aFileList fileList.
	fileListView window: (0 @ 0 extent: 120 @ 60).
	fileListView borderWidthLeft: 1 right: 2 top: 2 bottom: 1.
	topView addSubView: fileListView toRightOf: volListView.

	withEdit ifTrue: [
	aFileView _ MSWScrollBarView on:FileView new.
	aFileView scrollingView model: aFileList.
	aFileView window: (0 @ 0 extent: 200 @ 140).
	aFileView borderWidthLeft: 2 right: 2 top: 1 bottom: 2.
	topView addSubView: aFileView below: aTemplateView.
	].

	topView controller open! !


!Inspector methodsFor: 'browser support' stamp: 'stp 02/18/98 0-11:'!
buildAndArrangeSubViewsInside: aTopView
	"2/25/97 ssa - Added to support browse protocol"
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	|  inspector listView valueView evalView |
	inspector _ self.
	aTopView model: inspector.

	listView _ InspectListView new model: inspector.
		(inspector isMemberOf: DictionaryInspector)
			ifTrue: [listView controller: DictionaryListController new].
		aTopView 
			addSubView: (MSWScrollBarView on: listView )
			in: (0@0 corner: (1/3)@(4/7))
			borderWidth: 1.

	valueView _ InspectCodeView new model: inspector.
		aTopView 
			addSubView: (MSWScrollBarView on: valueView )
			in: ((1/3)@0 corner: 	1@(4/7))
			borderWidth: 1.

	evalView _ StringHolderView new
					model: (InspectorTrash for: inspector object).
		aTopView
			addSubView: (MSWScrollBarView on: evalView )
			in: (0@(4/7) corner: 	1@1)
			borderWidth: 1! !


!Inspector class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
openOn: anObject withEvalPane: withEval withLabel: label valueViewClass: valueViewClass
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| topView inspector listView valueView evalView lv vv ev |
	inspector _ self inspect: anObject.
	topView _ StandardSystemView new model: inspector.

	listView _ InspectListView new model: inspector.
		(inspector isMemberOf: DictionaryInspector)
			ifTrue: [listView controller: DictionaryListController new].
		topView addSubView: (lv _ MSWScrollBarView on: listView ).
		lv window: (0 @ 0 extent: 40 @ 40).
		lv borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	valueView _ valueViewClass new model: inspector.
		topView addSubView: (vv _ MSWScrollBarView on: valueView ) toRightOf: lv.
		vv window: (0 @ 0 extent: 75 @ 40).
		vv borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	withEval ifTrue:
		[evalView _ StringHolderView new
					model: (InspectorTrash for: inspector object).
		topView addSubView: (ev _ MSWScrollBarView on: evalView ) below: lv.
		ev window: (0 @ 0 extent: 115 @ 20).
		ev borderWidthLeft: 2 right: 2 top: 0 bottom: 2].
	topView label: label.
	topView minimumSize: 180 @ 120.
	topView controller open! !


!ListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
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

!ListView methodsFor: 'scrolling support' stamp: 'stp 02/18/98 0-11:'!
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


!ClassListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	(aSymbol == #systemCategorySelectionChanged) |
	(aSymbol == #editSystemCategories) |
	(aSymbol == #classListChanged)
		ifTrue:  [self updateClassList].
	(aSymbol == #classSelectionChanged)
		ifTrue: [self updateClassSelection].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!ContextStackListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	aSymbol == #contextStackIndex
		ifTrue: [self moveSelectionBox: model contextStackIndex].
	aSymbol == #contextStackList
		ifTrue: 
			[self list: model contextStackList.
			self displayView].
	aSymbol == #notChanged ifTrue: [self flash].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!FileListView methodsFor: 'as yet unclassified' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol 
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"
	aSymbol = #relabel ifTrue: [^ self].
	aSymbol == #fileList ifTrue: 
			[self list: model fileList.
			self displayView].
	aSymbol == #fileListIndex ifTrue: 
			[self moveSelectionBox: model fileListIndex].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!InspectListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	aSymbol == #inspectObject
		ifTrue: 
			[self list: model fieldList.
			selection _ model selectionIndex.
			self displayView].
	aSymbol == #selection ifTrue: [self moveSelectionBox: model selectionIndex].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!MessageCategoryListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	(aSymbol == #systemCategorySelectionChanged) |
	(aSymbol == #editSystemCategories)
		ifTrue: [self resetAndDisplayView].
	(aSymbol == #classSelectionChanged)
		ifTrue: [self getListAndDisplayView].
	(aSymbol == #messageCategorySelectionChanged)
		ifTrue:  [self moveSelectionBox: model messageCategoryListIndex].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!MessageListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"What to do to the message list when Browser changes. If there is only one item, select and show it.
	 : as part of adding a new feature that was subsequently removed, simplified the code here enough to justify using it"
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

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

		(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!MessageSet class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
open: aMessageSet name: aString 
	"Create a standard system view for the messageSet, aMessageSet, whose label is aString."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| topView aListView aBrowserCodeView |
	topView _ StandardSystemView new.
	topView model: aMessageSet.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aListView _ MSWScrollBarView on:MessageListView new.
	aListView scrollingView model: aMessageSet.
	aListView scrollingView list: aMessageSet messageList.
	aListView window: (0 @ 0 extent: 180 @ 100).
	aListView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 0.
	topView addSubView: aListView.
	aBrowserCodeView _ MSWScrollBarView on: BrowserCodeView new.
	aBrowserCodeView scrollingView model: aMessageSet.
	aBrowserCodeView window: (0 @ 0 extent: 180 @ 300).
	aBrowserCodeView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 2.
	topView
		addSubView: aBrowserCodeView
		align: aBrowserCodeView viewport topLeft
		with: aListView viewport bottomLeft.
	topView controller open! !


!PluggableListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol 
	"Refer to the comment in View|update:."
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

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
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!ScrollController methodsFor: 'basic control sequence' stamp: 'stp 02/18/98 0-11:'!
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

!ScrollController methodsFor: 'control defaults' stamp: 'stp 02/18/98 0-11:'!
controlActivity
	"Hacked to supprt MSWScrollBarView - ssa 12/10/97 17:07"

	(self view superView isKindOf: MSWScrollBarView)
		ifFalse:[
	self scrollBarContainsCursor
				ifTrue: [self scroll]
				ifFalse: [super controlActivity]]
		ifTrue:[super controlActivity]! !

!ScrollController methodsFor: 'control defaults' stamp: 'stp 02/18/98 0-11:'!
isControlActive 
	"Viva la Junta!!"
	"HACKED to ignore scrollbar in the activity test if contained in a ScrollbarView - ssa 1/8/98 16:24"
	view isNil ifTrue: [^ false].

	(self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
		ifFalse:["original code" ^(view insetDisplayBox merge: scrollBar inside) containsPoint: sensor cursorPoint]
		ifTrue:[^view insetDisplayBox containsPoint: sensor cursorPoint]! !

!ScrollController methodsFor: 'marker adjustment' stamp: 'stp 02/18/98 0-11:'!
moveMarker
	"The view window has changed. Update the marker."
	"Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an MSWScrollBarView - ssa 12/10/97 15:24"

	(self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
		ifFalse:[self moveMarker: self markerDelta negated anchorMarker: nil]! !

!ScrollController methodsFor: 'marker adjustment' stamp: 'stp 02/18/98 0-11:'!
moveMarkerTo: aRectangle 
	"Same as markerRegion: aRectangle; moveMarker, except a no-op if the marker
	 would not move."
	"Hacked to suppress flop-out scrollbar updates when the view is encapsulated by an MSWScrollBarView - ssa 12/10/97 15:24"

	(self view superView isKindOf: MSWScrollBarView) "_______HERE IS THE HACK"
		ifFalse:[	(aRectangle height = marker height and: [self viewDelta = 0])
					ifFalse:
						[self markerRegion: aRectangle.
						self moveMarker]]! !


!ParagraphEditor methodsFor: 'controlling' stamp: 'stp 02/18/98 0-11:'!
controlActivity
	"Hacked to supprt MSWScrollBarView - ssa 12/10/97 17:07"
	
	self scrollBarContainsCursor
		ifTrue: [(self view superView isKindOf: MSWScrollBarView)
					ifFalse:[self scroll]]
		ifFalse: [self processKeyboard.
				self processMouseButtons]! !

!ParagraphEditor methodsFor: 'scrolling' stamp: 'stp 02/18/98 0-11:'!
updateMarker
	"Hacked to catch this scrolling 'event'.  ssa 12/5/97 16:22"
	"A variation of computeMarkerRegion--only redisplay the marker in the scrollbar if an actual change has occurred in the positioning of the paragraph."
	self moveMarkerTo: self computeMarkerRegion.

		"A hack to notify the MSWScrollBarController"
	(self view superView isKindOf: MSWScrollBarView)
		ifTrue:[self view superView updateElevator]! !


!StandardSystemView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
new
 	"This is a rather dirty hack -- but we don't have a window builder
yet. (ar 1/22/98 23:36)"

 	^Preferences nicerSystemViews
 		ifTrue:[MSWSystemView basicNew initialize]
 		ifFalse:[self basicNew initialize]! !


!BrowserView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildBrowserCodeView: aBrowser editString: aString
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| aBrowserCodeView |
	aBrowserCodeView _ MSWScrollBarView on: BrowserCodeView new.
	aBrowserCodeView scrollingView model: aBrowser.
	aBrowserCodeView window: (0 @ 0 extent: 200 @ 110).
	aBrowserCodeView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.
	aString ~~ nil ifTrue: [aBrowserCodeView scrollingView editString: aString].
	^aBrowserCodeView! !

!BrowserView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildClassListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| aClassListView |
	aClassListView _ MSWScrollBarView on:ClassListView new.
	aClassListView scrollingView model: aBrowser.
	aClassListView window: (0 @ 0 extent: 50 @ 62).
	aClassListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aClassListView! !

!BrowserView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildMessageCategoryListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| aMessageCategoryListView |
	aMessageCategoryListView _ MSWScrollBarView on:MessageCategoryListView new.
	aMessageCategoryListView scrollingView model: aBrowser.
	aMessageCategoryListView window: (0 @ 0 extent: 50 @ 70).
	aMessageCategoryListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aMessageCategoryListView! !

!BrowserView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildMessageListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| aMessageListView |

	aMessageListView _ MSWScrollBarView on: MessageListView new.
	aMessageListView scrollingView model: aBrowser.
	aMessageListView window: (0 @ 0 extent: 50 @ 70).
	aMessageListView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	^ aMessageListView! !

!BrowserView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildSystemCategoryListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| aSystemCategoryListView |
	aSystemCategoryListView _ MSWScrollBarView on: SystemCategoryListView new.
	aSystemCategoryListView scrollingView model: aBrowser.
	aSystemCategoryListView window: (0 @ 0 extent: 50 @ 70).
	aSystemCategoryListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aSystemCategoryListView! !


!DebuggerView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
debugger: aDebugger 
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	"Answer a DebuggerView whose model is aDebugger. It consists of three 
	subviews, a ContextStackView (the ContextStackListView and 
	ContextStackCodeView), an InspectView of aDebugger's variables, and an 
	InspectView of the variables of the currently selected method context."
	| topView stackListView stackCodeView rcvrVarView rcvrValView ctxtVarView ctxtValView |
	aDebugger expandStack.
	topView _ self new model: aDebugger.
	stackListView _ MSWScrollBarView on:  (ContextStackListView new model: aDebugger).
		stackListView window: (0 @ 0 extent: 150 @ 50).
		stackListView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
		topView addSubView: stackListView.
	stackCodeView _ MSWScrollBarView on:  (ContextStackCodeView new model: aDebugger).
		stackCodeView scrollingView controller: ContextStackCodeController new.
		stackCodeView window: (0 @ 0 extent: 150 @ 75).
		stackCodeView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
		topView addSubView: stackCodeView below: stackListView.
	rcvrVarView _ MSWScrollBarView on:(InspectListView new model: aDebugger receiverInspector).
		rcvrVarView window: (0 @ 0 extent: 25 @ 50).
		rcvrVarView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: rcvrVarView below: stackCodeView.
	rcvrValView _ MSWScrollBarView on:(InspectCodeView new model: aDebugger receiverInspector).
		rcvrValView window: (0 @ 0 extent: 50 @ 50).
		rcvrValView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: rcvrValView toRightOf: rcvrVarView.
	ctxtVarView _ MSWScrollBarView on:(InspectListView new model: aDebugger contextVariablesInspector).
		ctxtVarView window: (0 @ 0 extent: 25 @ 50).
		ctxtVarView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: ctxtVarView toRightOf: rcvrValView.
	ctxtValView _ MSWScrollBarView on:(InspectCodeView new model: aDebugger contextVariablesInspector).
		ctxtValView window: (0 @ 0 extent: 50 @ 50).
		ctxtValView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
		topView addSubView: ctxtValView toRightOf: ctxtVarView.
	^ topView! !

!DebuggerView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
openNotifier: aDebugger contents: msgString label: label
	"Create and schedule a simple view with a debugger which can be opened later."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| msg aStringHolderView topView nLines displayPoint shv |
	Cursor normal show.
	Sensor flushKeyboard.
	msg _ msgString.
	(label beginsWith: 'Space is low')
		ifTrue: [msg _ self lowSpaceChoices, msg].
	aStringHolderView _
		StringHolderView container: (StringHolder new contents: msg).
	aStringHolderView controller: (NotifyStringHolderController debugger: aDebugger).
	topView _ StandardSystemView new.
	topView model: aStringHolderView model.
	topView addSubView: (shv _ MSWScrollBarView on: aStringHolderView ).
	shv borderWidth:2.
	topView label: label.
	nLines _ 1 + (msg occurrencesOf: Character cr).
	topView minimumSize: 350 @ (14 * nLines + 6).
	displayPoint _
		ScheduledControllers activeController == nil
			ifTrue: [Display boundingBox center]
			ifFalse: [ScheduledControllers activeController view displayBox center].
	topView controller openNoTerminateDisplayAt: displayPoint.
	^ topView
! !


!InspectorView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
dictionaryInspector: anInspector 
	"Answer an instance of me on the model, anInspector. The instance 
	consists of an InspectListView and an InspectCodeView."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| anInspectorView anInspectorListView aCodeView |
	anInspectorView _ View new.
		anInspectorView model: anInspector.
		anInspectorListView _ MSWScrollBarView on:(InspectListView new model: anInspector;
				controller: DictionaryListController new;yourself).
		anInspectorListView window: (0 @ 0 extent: 40 @ 40).
		anInspectorListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	anInspectorView addSubView: anInspectorListView.
	aCodeView _ self buildCodeView: anInspector.
	anInspectorView
		addSubView: aCodeView
		align: aCodeView viewport topLeft
		with: anInspectorListView viewport topRight.
	^anInspectorView! !

!InspectorView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
formDictionaryInspector: anInspector 
	"Answer an instance of me on the model, anInspector. The instance 
	consists of an InspectListView and an InspectFormView  6/28/96 sw."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| anInspectorView anInspectorListView aFormView |
	anInspectorView _ View new.
		anInspectorView model: anInspector.
		anInspectorListView _ MSWScrollBarView on: (InspectListView new model: anInspector;
				controller: DictionaryListController new; yourself).
		anInspectorListView window: (0 @ 0 extent: 40 @ 40).
		anInspectorListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	anInspectorView addSubView: anInspectorListView.
	aFormView _ self buildFormView: anInspector.
	anInspectorView
		addSubView: aFormView
		align: aFormView viewport topLeft
		with: anInspectorListView viewport topRight.
	^anInspectorView! !

!InspectorView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildCodeView: anInspector
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| inspectCodeView |

	inspectCodeView _ MSWScrollBarView on:(InspectCodeView new model: anInspector).
	inspectCodeView window: (0 @ 0 extent: 75 @ 40).
	inspectCodeView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	^ inspectCodeView! !

!InspectorView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildInspectListView: anInspector
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	| anInspectListView |

	anInspectListView _ MSWScrollBarView on:(InspectListView new model: anInspector).
	anInspectListView window: (0 @ 0 extent: 40 @ 40).
	anInspectListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.

	^ anInspectListView! !

!InspectorView class methodsFor: 'private' stamp: 'stp 02/18/98 0-11:'!
buildTrashView: anInspector
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| inspectTrashView |

	inspectTrashView _ MSWScrollBarView on: StringHolderView new.
	inspectTrashView scrollingView model: (InspectorTrash for: anInspector object).
	inspectTrashView scrollingView controller turnLockingOff.
	inspectTrashView window: (0 @ 0 extent: 115 @ 20).
	inspectTrashView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.

	^ inspectTrashView! !

!StringHolderView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
updateDisplayContents
	"Make the text that is displayed be the contents of the receiver's model."
	"VIVA LA JUNTA!!!!  hack this to update the scroll bar when the contents changes  - ssa 1/15/98 14:39"

	self editString: model contents.
	self displayView.
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!BrowserCodeView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
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

	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!ContextStackCodeView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
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

	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!StringHolderView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
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


!SyntaxError class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
open: aSyntaxError
	"Answer a standard system view whose model is an instance of me.  TK 15 May 96"
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"

	|  topView aListView aCodeView |
	topView _ StandardSystemView new.
	topView model: aSyntaxError.
	topView label: 'Syntax Error'.
	topView minimumSize: 380 @ 220.
	aListView _ MSWScrollBarView on:SyntaxErrorListView new.
	aListView scrollingView model: aSyntaxError.
	aListView window: (0 @ 0 extent: 380 @ 20).
	aListView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 0.
	topView addSubView: aListView.
	aCodeView _ MSWScrollBarView on:BrowserCodeView new.
	aCodeView scrollingView model: aSyntaxError.
	aCodeView window: (0 @ 0 extent: 380 @ 200).
	aCodeView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 2.
	topView
		addSubView: aCodeView
		align: aCodeView viewport topLeft
		with: aListView viewport bottomLeft.
	topView controller openNoTerminateDisplayAt: Display extent // 2.
	Processor activeProcess suspend! !


!SystemCategoryListView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aSymbol
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	aSymbol == #systemCategorySelectionChanged
		ifTrue: [self updateSystemCategorySelection].
	aSymbol == #systemCategoriesChanged
		ifTrue: [self updateSystemCategoryList].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator]! !


!TextCollectorView methodsFor: 'updating' stamp: 'stp 02/18/98 0-11:'!
update: aParameter
	"Transcript cr; show: 'qwre'.    Transcript clear."
	"Hacked to support MSWScrollBars - ssa 12/11/97 16:42"

	aParameter == #appendEntry ifTrue:
		[controller doOccluded: [controller appendEntry]].
	aParameter == #update ifTrue:
		[controller doOccluded:
				[controller changeText: model contents asText]].
	(self superView isKindOf: MSWScrollBarView)
		ifTrue:[self superView updateElevator].
	(aParameter == #update) | (aParameter == #appendEntry)
		ifFalse:[^ super update: aParameter]! !


!TextCollectorView class methodsFor: 'instance creation' stamp: 'stp 02/18/98 0-11:'!
open: aTextCollector label: aString 
	"Answer an instance of me on the argument, aTextCollector. The
	label of the StandardSystemView should be aString."
	"VIVA LA JUNTA!!  Modified to use MSWScrollBarViews - ssa 12/11/97 12:00"
	| topView aView |
	topView _ StandardSystemView new.
	topView model: aTextCollector.
	topView label: aString.
	topView minimumSize: 100 @ 50.
	aView _ MSWScrollBarView on: (self new model: aTextCollector).
	aView borderWidth: 2.
	topView addSubView: aView.
	topView controller open! !
