"ViewSizes.st
This makes the default sizes of most of the standard views a bit larger--edit to taste.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    July 8, 1997"!

!BrowserView class methodsFor: 'instance scheduling' stamp: 'stp 07/01/97 11:34'!
openBrowserView: aBrowserView label: aString 
	"Schedule aBrowserView, labelling the view aString."
	
	aBrowserView label: aString.
	aBrowserView minimumSize: 500 @ 400.
	aBrowserView subViews do: [:each | each controller].
	aBrowserView controller open! !


!ChangeList class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:43'!
open: aChangeList name: aString withListView: aListView
	"Create a standard system view for the messageSet, whose label is aString.
	The listView supplied may be either single or multiple selection type"
	| topView codeView |
	topView _ StandardSystemView new.
	topView model: aChangeList.
	topView label: aString.
	topView minimumSize: 400 @ 400.
	aListView model: aChangeList.
	aListView list: aChangeList list.
	aListView window: (0 @ 0 extent: 400 @ 100).
	aListView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	topView addSubView: aListView.
	codeView _ StringHolderView new.
	codeView model: aChangeList.
	codeView window: (0 @ 0 extent: 400 @ 300).
	codeView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	topView
		addSubView: codeView
		align: codeView viewport topLeft
		with: aListView viewport bottomLeft.
	topView controller open ! !


!DebuggerView class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:35'!
openNoSuspendDebugger: aDebugger label: aString 
	"Answer a standard system view containing an instance of me on the model, aDebugger. The label is aString. Do not terminate the current active process. "
	| debuggerView |
	debuggerView _ self debugger: aDebugger.
	debuggerView label: aString.
	debuggerView minimumSize: 500 @ 400.
	debuggerView controller openNoTerminate.
	^ debuggerView! !


!FileList class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:41'!
openWithEditPane: withEdit  "FileList open"
	"Open a view of an instance of me on the default directory.   2/14/96 sw: use standard directory.  (6/96 functionality substantially changed by di)
	 7/12/96 sw: set the label to the pathname"

	| topView aTemplateView fileListView aFileView aFileList aFileTemplateHolder dir volListView |
	topView _ StandardSystemView new.
	aFileList _ self new directory: (dir _ FileDirectory default).
	topView model: aFileList.
	topView label: dir pathName.
	topView minimumSize:
		400 @ (withEdit ifTrue: [400] ifFalse: [160]).

	volListView _ ListView new.
	volListView model: aFileList.
	volListView list: aFileList list.
	volListView window: (0 @ 0 extent: 80 @ 45).
	volListView borderWidthLeft: 2 right: 1 top: 2 bottom: 1.
	topView addSubView: volListView.

	aFileTemplateHolder _ FileTemplateHolder on: aFileList.
	aTemplateView _ StringHolderView new.
	aTemplateView controller: FileTemplateController new.
	aTemplateView model: aFileTemplateHolder.
	aTemplateView window: (0 @ 0 extent: 80 @ 15).
	aTemplateView borderWidthLeft: 2 right: 1 top: 1 bottom: 1.
	topView addSubView: aTemplateView below: volListView.

	fileListView _ FileListView new.
	fileListView model: aFileList.
	fileListView controller: FileListController new.
	fileListView list: aFileList fileList.
	fileListView window: (0 @ 0 extent: 120 @ 60).
	fileListView borderWidthLeft: 1 right: 2 top: 2 bottom: 1.
	topView addSubView: fileListView toRightOf: volListView.

	withEdit ifTrue: [
	aFileView _ FileView new.
	aFileView model: aFileList.
	aFileView window: (0 @ 0 extent: 200 @ 140).
	aFileView borderWidthLeft: 2 right: 2 top: 1 bottom: 2.
	topView addSubView: aFileView below: aTemplateView.
	].

	topView controller open! !


!Inspector class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:44'!
openOn: anObject withEvalPane: withEval withLabel: label valueViewClass: valueViewClass
	| topView inspector listView valueView evalView |
	inspector _ self inspect: anObject.
	topView _ StandardSystemView new model: inspector.

	listView _ InspectListView new model: inspector.
		(inspector isMemberOf: DictionaryInspector)
			ifTrue: [listView controller: DictionaryListController new].
		listView window: (0 @ 0 extent: 80 @ 140).
		listView borderWidthLeft: 2 right: 0 top: 2 bottom: 2.
		topView addSubView: listView.
	valueView _ valueViewClass new model: inspector.
		valueView window: (0 @ 0 extent: 120 @ 140).
		valueView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
		topView addSubView: valueView toRightOf: listView.
withEval ifTrue:
	[evalView _ StringHolderView new
					model: (InspectorTrash for: inspector object).
		evalView window: (0 @ 0 extent: 200 @ 60).
		evalView borderWidthLeft: 2 right: 2 top: 0 bottom: 2.
		topView addSubView: evalView below: listView].
	topView label: label.
	topView minimumSize: 200 @ 200.
	topView controller open! !


!MessageSet class methodsFor: 'instance creation' stamp: 'stp 07/02/97 09:32'!
open: aMessageSet name: aString 
	"Create a standard system view for the messageSet, aMessageSet, whose label is aString."

	| topView aListView aBrowserCodeView |
	topView _ StandardSystemView new.
	topView model: aMessageSet.
	topView label: aString.
	topView minimumSize: 400 @ 400.
	aListView _ MessageListView new.
	aListView model: aMessageSet.
	aListView list: aMessageSet messageList.
	aListView window: (0 @ 0 extent: 180 @ 100).
	aListView
		borderWidthLeft: 2
		right: 2
		top: 2
		bottom: 0.
	topView addSubView: aListView.
	aBrowserCodeView _ BrowserCodeView new.
	aBrowserCodeView model: aMessageSet.
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


!OutlineBrowser class methodsFor: 'view creation' stamp: 'stp 07/01/97 11:40'!
openOn: anOrganization label: aLabel
	"OutlineBrowser openOn: Outline new label: 'Maintenance' "

	| me topView listView textView|
	me :=  self new model: anOrganization.
	topView := StandardSystemView new.
	topView model: me.
	topView label: aLabel.
	topView minimumSize: 250@400.
	listView := PluggableListView on: me 
		list: #organizationList 
		selected: #currentCategory 
		changeSelected: #currentCategory:
		menu: #organizationMenu.
	listView window: (0 @ 0 extent: 200 @ 80).
	listView borderWidthLeft: 2 right: 2 top: 2 bottom: 0.
	textView :=  PluggableTextView 
		on: me
		aspect: #text
		change: #acceptText:
		menu: #textMenu.
	textView window: (0 @ 0 extent: 200 @ 300).
	textView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.
	topView addSubView: listView.
	topView
		addSubView: textView
		align: textView viewport topLeft
		with: listView viewport bottomLeft.
	topView controller open! !


!SelectionMenu class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:36'!
confirm: queryString 
	"Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no."
	"SelectionMenu confirm: 'Are you hungry?'"

	| menu choice |
	menu _ self selections: #('  yes  ' '  no  ').
	[true] whileTrue: [
		choice _ menu startUpWithCaption: queryString.
		choice = '  yes  ' ifTrue: [^ true].
		choice = '  no  ' ifTrue: [^ false]]! !


!StringHolderView class methodsFor: 'instance creation' stamp: 'stp 07/01/97 11:41'!
open: aStringHolder label: aString 
	"Create a standard system view of the model, aStringHolder, as viewed by 
	an instance of me. The label of the view is aString."
	| aStringHolderView topView |
	aStringHolderView _ self container: aStringHolder.
	topView _ StandardSystemView new.
	topView model: aStringHolderView model.
	topView addSubView: aStringHolderView.
	topView label: aString.
	topView minimumSize: 300 @ 200.
	topView controller open! !


