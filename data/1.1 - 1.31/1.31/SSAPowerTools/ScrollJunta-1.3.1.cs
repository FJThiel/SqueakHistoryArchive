'From Squeak 1.3 of Jan 16, 1998 on 20 January 1998 at 1:47:18 pm'!
ColorSystemView subclass: #BrowserView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-Browser'!

!BrowserView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 13:26'!
classBrowser: aBrowser editString: aString 
	"Answer an instance of me on the model, aBrowser. The instance consists 
	of four subviews, starting with the list view of classes in the model's 
	currently selected system category. The initial text view part is a view 
	of the characters in aString."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| browserView classListView messageCategoryListView switchView
	messageListView browserCodeView |

	browserView _ self new model: aBrowser.
	classListView _ self buildClassListView: aBrowser.
	switchView _ self buildInstanceClassSwitchView: aBrowser.
	messageCategoryListView _ self buildMessageCategoryListView: aBrowser.
	messageListView _ self buildMessageListView: aBrowser.
	browserCodeView _ self buildBrowserCodeView: aBrowser editString: aString.

	classListView borderWidthLeft: 2 right: 0 top: 2 bottom: 0.
	classListView scrollingView singleItemMode: true.
	classListView scrollingView noTopDelimiter.
	classListView scrollingView noBottomDelimiter.
	classListView scrollingView list: classListView scrollingView getList.
	switchView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.

	browserView addSubView: classListView.
	browserView addSubView: switchView.
	browserView addSubView: messageCategoryListView.
	browserView addSubView: messageListView.
	browserView addSubView: browserCodeView.

	messageListView 
		align: messageListView viewport topLeft 
		with: messageCategoryListView viewport topRight.
	classListView 
		window: classListView window 
		viewport: (messageCategoryListView viewport topLeft - (0 @ 12) 
					corner: messageCategoryListView viewport topRight).
	switchView 
		window: switchView window 
		viewport: (messageListView viewport topLeft - (0 @ 12) 
					corner: messageListView viewport topRight).
	browserCodeView 
		window: browserCodeView window 
		viewport: (messageCategoryListView viewport bottomLeft 
					corner: messageListView viewport bottomRight + (0 @ 110)).
    
	aString notNil ifTrue: [aBrowser lock].

	^browserView! !

!BrowserView class methodsFor: 'private' stamp: 'ssa 12/11/97 13:13'!
buildBrowserCodeView: aBrowser editString: aString
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| aBrowserCodeView |
	aBrowserCodeView _ SSAScrollBarView on: BrowserCodeView new.
	aBrowserCodeView scrollingView model: aBrowser.
	aBrowserCodeView window: (0 @ 0 extent: 200 @ 110).
	aBrowserCodeView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.
	aString ~~ nil ifTrue: [aBrowserCodeView scrollingView editString: aString].
	^aBrowserCodeView! !

!BrowserView class methodsFor: 'private' stamp: 'ssa 12/11/97 13:25'!
buildClassListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| aClassListView |
	aClassListView _ SSAScrollBarView on:ClassListView new.
	aClassListView scrollingView model: aBrowser.
	aClassListView window: (0 @ 0 extent: 50 @ 62).
	aClassListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aClassListView! !

!BrowserView class methodsFor: 'private' stamp: 'ssa 12/11/97 13:28'!
buildMessageCategoryListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| aMessageCategoryListView |
	aMessageCategoryListView _ SSAScrollBarView on:MessageCategoryListView new.
	aMessageCategoryListView scrollingView model: aBrowser.
	aMessageCategoryListView window: (0 @ 0 extent: 50 @ 70).
	aMessageCategoryListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aMessageCategoryListView! !

!BrowserView class methodsFor: 'private' stamp: 'ssa 12/11/97 13:29'!
buildMessageListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| aMessageListView |

	aMessageListView _ SSAScrollBarView on: MessageListView new.
	aMessageListView scrollingView model: aBrowser.
	aMessageListView window: (0 @ 0 extent: 50 @ 70).
	aMessageListView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	^ aMessageListView! !

!BrowserView class methodsFor: 'private' stamp: 'ssa 12/11/97 13:29'!
buildSystemCategoryListView: aBrowser
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| aSystemCategoryListView |
	aSystemCategoryListView _ SSAScrollBarView on: SystemCategoryListView new.
	aSystemCategoryListView scrollingView model: aBrowser.
	aSystemCategoryListView window: (0 @ 0 extent: 50 @ 70).
	aSystemCategoryListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	^aSystemCategoryListView! !


!ChangeList class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:24'!
open: aChangeList name: aString withListView: aListView
	"Create a standard system view for the messageSet, whose label is aString.
	The listView supplied may be either single or multiple selection type"
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| topView codeView lv cv |
	topView _ StandardSystemView new.
	topView model: aChangeList.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aListView model: aChangeList.
	aListView list: aChangeList list.
	aListView window: (0 @ 0 extent: 180 @ 100).
	topView addSubView: (lv _ SSAScrollBarView on: aListView ).
	lv borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	codeView _ StringHolderView new.
	codeView model: aChangeList.
	codeView window: (0 @ 0 extent: 180 @ 300).
	topView
		addSubView: ( cv _ SSAScrollBarView on: codeView )
		align: cv viewport topLeft
		with: lv viewport bottomLeft.
	cv borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	topView controller open ! !


!ChangeSorter methodsFor: 'creation' stamp: 'ssa 12/11/97 12:24'!
openView: topView
	"Create change sorter on one changeSet only.  Two of these in a DualChangeSorter."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| classView messageView codeView |

	buttonView _ SwitchView new.
	buttonView model: self controller: TriggerController new.
	buttonView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	buttonView selector: #whatPolarity.
	buttonView controller selector: #cngSetActivity.
	buttonView window: (0 @ 0 extent: 360 @ 20).
	buttonView label: myChangeSet name asParagraph.

	classView _ SSAScrollBarView on: GeneralListView new.
	classView scrollingView controllerClass: GeneralListController.
	classView scrollingView model: classList.
	classView scrollingView window: (0 @ 0 extent: 180 @ 160).
	classView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	classView scrollingView controller yellowButtonMenu: ClassMenu 
		yellowButtonMessages: ClassSelectors.
	classList scrollingView controller: classView controller.


	messageView _ SSAScrollBarView on: GeneralListView new.
	messageView scrollingView controllerClass: GeneralListController.
	messageView scrollingView model: messageList.
	messageView scrollingView window: (0 @ 0 extent: 180 @ 160).
	messageView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	messageView scrollingView controller yellowButtonMenu: MsgListMenu 
		yellowButtonMessages: MsgListSelectors.
	messageList scrollingView controller: messageView controller.

	codeView _ SSAScrollBarView on: BrowserCodeView new.
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

!ChangeSorter methodsFor: 'creation' stamp: 'ssa 12/11/97 16:25'!
openView: topView offsetBy: offset
	"Create change sorter on one changeSet with 0@0.
	Two of these in a DualChangeSorter, right one is offset by 360@0."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| classView messageView codeView |
	buttonView _ SwitchView new.
	buttonView model: self controller: TriggerController new.
	buttonView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	buttonView selector: #whatPolarity.
	buttonView controller selector: #cngSetActivity.
	buttonView window: ((0 @ 0 extent: 360 @ 20) translateBy: offset).
	buttonView label: myChangeSet name asParagraph.

	classView _ SSAScrollBarView on: GeneralListView new.
	classView scrollingView controllerClass: GeneralListController.
	classView scrollingView model: classList.
	classView window: (0 @ 0 extent: 180 @ 160).
	classView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	classView scrollingView controller yellowButtonMenu: ClassMenu 
		yellowButtonMessages: ClassSelectors.
	classList  controller: classView scrollingView controller.

	messageView _ SSAScrollBarView on: GeneralListView new.
	messageView scrollingView controllerClass: GeneralListController.
	messageView scrollingView model: messageList.
	messageView window: (0 @ 0 extent: 180 @ 160).
	messageView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	messageView scrollingView controller yellowButtonMenu: MsgListMenu 
		yellowButtonMessages: MsgListSelectors.
	messageList  controller: messageView scrollingView controller.

	codeView _ SSAScrollBarView on: BrowserCodeView new.
	codeView scrollingView model: self.
	codeView window: (0 @ 0 extent: 360 @ 180).
	codeView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.

	topView addSubView: buttonView.
	topView addSubView: classView below: buttonView.
	topView addSubView: messageView toRightOf: classView.
	topView addSubView: codeView below: classView.
! !


!DebuggerView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 15:31'!
debugger: aDebugger 
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	"Answer a DebuggerView whose model is aDebugger. It consists of three 
	subviews, a ContextStackView (the ContextStackListView and 
	ContextStackCodeView), an InspectView of aDebugger's variables, and an 
	InspectView of the variables of the currently selected method context."
	| topView stackListView stackCodeView rcvrVarView rcvrValView ctxtVarView ctxtValView |
	aDebugger expandStack.
	topView _ self new model: aDebugger.
	stackListView _ SSAScrollBarView on:  (ContextStackListView new model: aDebugger).
		stackListView window: (0 @ 0 extent: 150 @ 50).
		stackListView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
		topView addSubView: stackListView.
	stackCodeView _ SSAScrollBarView on:  (ContextStackCodeView new model: aDebugger).
		stackCodeView scrollingView controller: ContextStackCodeController new.
		stackCodeView window: (0 @ 0 extent: 150 @ 75).
		stackCodeView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
		topView addSubView: stackCodeView below: stackListView.
	rcvrVarView _ SSAScrollBarView on:(InspectListView new model: aDebugger receiverInspector).
		rcvrVarView window: (0 @ 0 extent: 25 @ 50).
		rcvrVarView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: rcvrVarView below: stackCodeView.
	rcvrValView _ SSAScrollBarView on:(InspectCodeView new model: aDebugger receiverInspector).
		rcvrValView window: (0 @ 0 extent: 50 @ 50).
		rcvrValView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: rcvrValView toRightOf: rcvrVarView.
	ctxtVarView _ SSAScrollBarView on:(InspectListView new model: aDebugger contextVariablesInspector).
		ctxtVarView window: (0 @ 0 extent: 25 @ 50).
		ctxtVarView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: ctxtVarView toRightOf: rcvrValView.
	ctxtValView _ SSAScrollBarView on:(InspectCodeView new model: aDebugger contextVariablesInspector).
		ctxtValView window: (0 @ 0 extent: 50 @ 50).
		ctxtValView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
		topView addSubView: ctxtValView toRightOf: ctxtVarView.
	^ topView! !

!DebuggerView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 13:13'!
openNotifier: aDebugger contents: msgString label: label
	"Create and schedule a simple view with a debugger which can be opened later."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

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
	topView addSubView: (shv _ SSAScrollBarView on: aStringHolderView ).
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


!DisplayTextView class methodsFor: 'examples' stamp: 'ssa 12/11/97 12:44'!
open: textOrString label: aLabel
	"Create a system view with a paragraph editor in it.  6/2/96 sw"
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| topView aDisplayTextView |
	aDisplayTextView _ SSAScrollBarView on: (DisplayTextView new model: textOrString asDisplayText).
	aDisplayTextView borderWidth: 2.
	topView _ StandardSystemView new.
	topView label: aLabel.
	topView addSubView: aDisplayTextView.
	topView controller open

	"DisplayTextView open: 'Great green gobs' label: 'Gopher Guts'"! !


!FileModel class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 15:43'!
open: aFileModel named: aString 
	"Answer a scheduled view whose model is aFileModel and whose label is aString. "
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| topView aView |
	topView _ StandardSystemView new.
	topView model: aFileModel.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aView _ SSAScrollBarView on: (FileView new model: aFileModel).
	aView window: (0 @ 0 extent: 180 @ 120).
	aView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 2.
	topView addSubView: aView.
	topView controller open! !


!FileList class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:48'!
openWithEditPane: withEdit  "FileList open"
	"Open a view of an instance of me on the default directory.   2/14/96 sw: use standard directory.  (6/96 functionality substantially changed by di)
	 7/12/96 sw: set the label to the pathname"
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| topView aTemplateView fileListView aFileView aFileList aFileTemplateHolder dir volListView |
	topView _ StandardSystemView new.
	aFileList _ self new directory: (dir _ FileDirectory default).
	topView model: aFileList.
	topView label: dir pathName.
	topView minimumSize:
		200 @ (withEdit ifTrue: [200] ifFalse: [60]).

	volListView _ SSAScrollBarView on:ListView new.
	volListView scrollingView model: aFileList.
	volListView scrollingView list: aFileList list.
	volListView window: (0 @ 0 extent: 80 @ 45).
	volListView borderWidthLeft: 2 right: 1 top: 2 bottom: 1.
	topView addSubView: volListView.

	aFileTemplateHolder _ FileTemplateHolder on: aFileList.
	aTemplateView _ SSAScrollBarView on:StringHolderView new.
	aTemplateView scrollingView controller: FileTemplateController new.
	aTemplateView scrollingView model: aFileTemplateHolder.
	aTemplateView window: (0 @ 0 extent: 80 @ 15).
	aTemplateView borderWidthLeft: 2 right: 1 top: 1 bottom: 1.
	topView addSubView: aTemplateView below: volListView.

	fileListView _ SSAScrollBarView on:FileListView new.
	fileListView scrollingView model: aFileList.
	fileListView scrollingView controller: FileListController new.
	fileListView scrollingView list: aFileList fileList.
	fileListView window: (0 @ 0 extent: 120 @ 60).
	fileListView borderWidthLeft: 1 right: 2 top: 2 bottom: 1.
	topView addSubView: fileListView toRightOf: volListView.

	withEdit ifTrue: [
	aFileView _ SSAScrollBarView on:FileView new.
	aFileView scrollingView model: aFileList.
	aFileView window: (0 @ 0 extent: 200 @ 140).
	aFileView borderWidthLeft: 2 right: 2 top: 1 bottom: 2.
	topView addSubView: aFileView below: aTemplateView.
	].

	topView controller open! !


!Inspector methodsFor: 'browser support' stamp: 'ssa 12/11/97 12:50'!
buildAndArrangeSubViewsInside: aTopView
	"2/25/97 ssa - Added to support browse protocol"
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	|  inspector listView valueView evalView |
	inspector _ self.
	aTopView model: inspector.

	listView _ InspectListView new model: inspector.
		(inspector isMemberOf: DictionaryInspector)
			ifTrue: [listView controller: DictionaryListController new].
		aTopView 
			addSubView: (SSAScrollBarView on: listView )
			in: (0@0 corner: (1/3)@(4/7))
			borderWidth: 1.

	valueView _ InspectCodeView new model: inspector.
		aTopView 
			addSubView: (SSAScrollBarView on: valueView )
			in: ((1/3)@0 corner: 	1@(4/7))
			borderWidth: 1.

	evalView _ StringHolderView new
					model: (InspectorTrash for: inspector object).
		aTopView
			addSubView: (SSAScrollBarView on: evalView )
			in: (0@(4/7) corner: 	1@1)
			borderWidth: 1.

! !


!Inspector class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 15:40'!
openOn: anObject withEvalPane: withEval withLabel: label valueViewClass: valueViewClass
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| topView inspector listView valueView evalView lv vv ev |
	inspector _ self inspect: anObject.
	topView _ StandardSystemView new model: inspector.

	listView _ InspectListView new model: inspector.
		(inspector isMemberOf: DictionaryInspector)
			ifTrue: [listView controller: DictionaryListController new].
		topView addSubView: (lv _ SSAScrollBarView on: listView ).
		lv window: (0 @ 0 extent: 40 @ 40).
		lv borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
	valueView _ valueViewClass new model: inspector.
		topView addSubView: (vv _ SSAScrollBarView on: valueView ) toRightOf: lv.
		vv window: (0 @ 0 extent: 75 @ 40).
		vv borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
withEval ifTrue:
	[evalView _ StringHolderView new
					model: (InspectorTrash for: inspector object).
		topView addSubView: (ev _ SSAScrollBarView on: evalView ) below: lv.
		ev window: (0 @ 0 extent: 115 @ 20).
		ev borderWidthLeft: 2 right: 2 top: 0 bottom: 2].
	topView label: label.
	topView minimumSize: 180 @ 120.
	topView controller open! !


!InspectorView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 15:39'!
dictionaryInspector: anInspector 
	"Answer an instance of me on the model, anInspector. The instance 
	consists of an InspectListView and an InspectCodeView."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| anInspectorView anInspectorListView aCodeView |
	anInspectorView _ View new.
		anInspectorView model: anInspector.
		anInspectorListView _ SSAScrollBarView on:(InspectListView new model: anInspector;
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

!InspectorView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 15:41'!
formDictionaryInspector: anInspector 
	"Answer an instance of me on the model, anInspector. The instance 
	consists of an InspectListView and an InspectFormView  6/28/96 sw."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| anInspectorView anInspectorListView aFormView |
	anInspectorView _ View new.
		anInspectorView model: anInspector.
		anInspectorListView _ SSAScrollBarView on: (InspectListView new model: anInspector;
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

!InspectorView class methodsFor: 'private' stamp: 'ssa 12/11/97 15:33'!
buildCodeView: anInspector
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| inspectCodeView |

	inspectCodeView _ SSAScrollBarView on:(InspectCodeView new model: anInspector).
	inspectCodeView window: (0 @ 0 extent: 75 @ 40).
	inspectCodeView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	^ inspectCodeView! !

!InspectorView class methodsFor: 'private' stamp: 'ssa 12/11/97 15:34'!
buildInspectListView: anInspector
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| anInspectListView |

	anInspectListView _ SSAScrollBarView on:(InspectListView new model: anInspector).
	anInspectListView window: (0 @ 0 extent: 40 @ 40).
	anInspectListView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.

	^ anInspectListView! !

!InspectorView class methodsFor: 'private' stamp: 'ssa 12/11/97 12:56'!
buildTrashView: anInspector
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| inspectTrashView |

	inspectTrashView _ SSAScrollBarView on: StringHolderView new.
	inspectTrashView scrollingView model: (InspectorTrash for: anInspector object).
	inspectTrashView scrollingView controller turnLockingOff.
	inspectTrashView window: (0 @ 0 extent: 115 @ 20).
	inspectTrashView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.

	^ inspectTrashView! !


!MessageSet class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 13:13'!
open: aMessageSet name: aString 
	"Create a standard system view for the messageSet, aMessageSet, whose label is aString."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	| topView aListView aBrowserCodeView |
	topView _ StandardSystemView new.
	topView model: aMessageSet.
	topView label: aString.
	topView minimumSize: 180 @ 120.
	aListView _ SSAScrollBarView on:MessageListView new.
	aListView scrollingView model: aMessageSet.
	aListView scrollingView list: aMessageSet messageList.
	aListView window: (0 @ 0 extent: 180 @ 100).
	aListView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 0.
	topView addSubView: aListView.
	aBrowserCodeView _ SSAScrollBarView on: BrowserCodeView new.
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


!StringHolderView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:54'!
open: aStringHolder label: aString 
	"Create a standard system view of the model, aStringHolder, as viewed by 
	an instance of me. The label of the view is aString."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| aStringHolderView topView shv |
	aStringHolderView _ self container: aStringHolder.
	topView _ StandardSystemView new.
	topView model: aStringHolderView model.
	topView addSubView: (shv _ SSAScrollBarView on: aStringHolderView ).
	shv borderWidth: 2.
	topView label: aString.
	topView minimumSize: 100 @ 50.
	topView controller open! !


!SyntaxError class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 13:13'!
open: aSyntaxError
	"Answer a standard system view whose model is an instance of me.  TK 15 May 96"
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"

	|  topView aListView aCodeView |
	topView _ StandardSystemView new.
	topView model: aSyntaxError.
	topView label: 'Syntax Error'.
	topView minimumSize: 380 @ 220.
	aListView _ SSAScrollBarView on:SyntaxErrorListView new.
	aListView scrollingView model: aSyntaxError.
	aListView window: (0 @ 0 extent: 380 @ 20).
	aListView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 0.
	topView addSubView: aListView.
	aCodeView _ SSAScrollBarView on:BrowserCodeView new.
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


!TextCollectorView class methodsFor: 'instance creation' stamp: 'ssa 12/11/97 12:47'!
open: aTextCollector label: aString 
	"Answer an instance of me on the argument, aTextCollector. The
	label of the StandardSystemView should be aString."
	"VIVA LA JUNTA!!  Modified to use SSAScrollBarViews - ssa 12/11/97 12:00"
	| topView aView |
	topView _ StandardSystemView new.
	topView model: aTextCollector.
	topView label: aString.
	topView minimumSize: 100 @ 50.
	aView _ SSAScrollBarView on: (self new model: aTextCollector).
	aView borderWidth: 2.
	topView addSubView: aView.
	topView controller open! !



