'From Squeak2.9alpha of 5 August 2000 [latest update: #3300] on 25 January 2001 at 9:04:17 am'!"Change Set:		chgListFeatures-swDate:			25 January 2001Author:			Scott Wallace� Added two keyboard accelerators to changeList list pane:	toggle diffs by cmd-D	select-all by cmd-a� Changelist menu now built with the much more convenient #addList: mechanism.� Changelist menu can now be made durable.� Adds (from Ned Konz) an 'invert selections' feature to the changelist menu.� Adds a feature to the changelist browser allowing you to remove the selections from the system.  Consult its comment for details."!!ChangeList methodsFor: 'initialization-release' stamp: 'sw 1/18/2001 13:09'!openAsMorphName: labelString multiSelect: multiSelect	"Open a morphic view for the messageSet, whose label is labelString.	The listView may be either single or multiple selection type"	| window listHeight listPane |	listHeight _ 0.4.	window _ (SystemWindow labelled: labelString)		model: self.	listPane _ (multiSelect				ifTrue: [PluggableListMorphOfMany]				ifFalse: [PluggableListMorph])			on: self			list: #list			selected: #listIndex			changeSelected: #toggleListIndex:			menu: (self showsVersions				ifTrue: [#versionsMenu:]				ifFalse: [#changeListMenu:]).	listPane keystrokeActionSelector: #changeListKey:from:.	window addMorph: listPane		frame: (0 @ 0 extent: 1 @ listHeight).	self 		addLowerPanesTo: window 		at: (0@listHeight corner: 1@1) 		with: nil.	^ window openInWorld! !!ChangeList methodsFor: 'initialization-release' stamp: 'sw 1/18/2001 16:35'!optionalButtonsView	"Answer the a View containing the optional buttons"	| view bHeight vWidth first offset previousView bWidth button |	vWidth _ 200.	bHeight _ self optionalButtonHeight.	previousView _ nil.	offset _ 0.	first _ true.	view _ View new		model: self;		window: (0 @ 0 extent: vWidth @ bHeight).	self changeListButtonSpecs do: [:triplet |		button _ PluggableButtonView			on: self			getState: nil			action: triplet second.		button label: triplet first asParagraph.		bWidth _ button label boundingBox width // 2.		button			window: (offset@0 extent: bWidth@bHeight);			borderWidthLeft: 0 right: 1 top: 0 bottom: 0.		offset _ offset + bWidth.		first			ifTrue:				[view addSubView: button.				first _ false.]			ifFalse:				[view addSubView: button toRightOf: previousView].		previousView _ button].	button _ PluggableButtonView		on: self		getState: #showDiffs		action: #toggleDiffing.	button		label: 'diffs' asParagraph;		window: (offset@0 extent: (vWidth - offset)@bHeight).	view addSubView: button toRightOf: previousView.	^ view! !!ChangeList methodsFor: 'menu actions' stamp: 'sw 1/25/2001 07:22'!changeListKey: aChar from: view	"Respond to a Command key in the list pane."	aChar == $D ifTrue: [^ self toggleDiffing].	aChar == $a ifTrue: [^ self selectAll].	^ self arrowKey: aChar from: view! !!ChangeList methodsFor: 'menu actions' stamp: 'sw 1/25/2001 08:40'!changeListMenu: aMenu	"Fill aMenu up so that it comprises the primary changelist-browser menu"	Smalltalk isMorphic ifTrue:		[aMenu addTitle: 'change list'.		aMenu addStayUpItemSpecial].	aMenu addList: #(	('fileIn selections'							fileInSelections)	('fileOut selections...	'						fileOutSelections)	-	('compare to current'						compareToCurrentVersion)	('toggle diffing (D)'							toggleDiffing)	-	('select conflicts with any changeset'		selectAllConflicts)	('select conflicts with current changeset'	selectConflicts)	-	('select conflicts with...'						selectConflictsWith)	('select unchanged methods'					selectUnchangedMethods)	('select methods for this class'				selectMethodsForThisClass)	('invert selections'							invertSelections)	-	('select all (a)'								selectAll)	('deselect all'								deselectAll)	-	('browse current versions of selections'		browseCurrentVersionsOfSelections)	('remove current methods of selections'		destroyCurrentCodeOfSelections)	-	('remove doIts'								removeDoIts)	('remove older versions'						removeOlderMethodVersions)	('remove selected items'						removeSelections)	('remove unselected items'					removeNonSelections)).	^ aMenu! !!ChangeList methodsFor: 'menu actions' stamp: 'sw 1/25/2001 08:38'!deselectAll 	"Deselect all items in the list pane, and clear the code pane"	listIndex _ 0.	listSelections atAllPut: false.	self changed: #allSelections.	self contentsChanged! !!ChangeList methodsFor: 'menu actions' stamp: 'sw 1/25/2001 09:04'!destroyCurrentCodeOfSelections	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!!"	|  aClass aChange aList |	aList _ OrderedCollection new.	1 to: changeList size do:		[:index |			(listSelections at: index) ifTrue:				[aChange _ changeList at: index.				(aChange type = #method					and: [(aClass _ aChange methodClass) notNil					and: [aClass includesSelector: aChange methodSelector]])						ifTrue:							[aList add: {aClass. aChange methodSelector}]]].	aList size > 0 ifTrue:		[(self confirm: 'Warning!! This will actually remove ', aList size printString,  ' method(s) from the system!!') ifFalse: [^ self]].	aList do:		[:aPair | Transcript cr; show: 'Removed: ', aPair first printString, '.', aPair second.			aPair first removeSelector: aPair second]! !!ChangeList methodsFor: 'menu actions' stamp: 'sw 1/25/2001 08:35'!invertSelections	"Invert the selectedness of each item in the changelist"	listSelections _ listSelections collect: [ :ea | ea not].	listIndex _ 0.	self changed: #allSelections.	self contentsChanged! !!ChangeList class methodsFor: 'instance creation' stamp: 'sw 1/25/2001 08:44'!open: aChangeList name: aString multiSelect: multiSelect	"Create a standard system view for the messageSet, whose label is aString.	The listView may be either single or multiple selection type"	| topView listHeight annoHeight optButtonHeight codeHeight aListView underPane annotationPane buttonsView aBrowserCodeView |	Smalltalk isMorphic		ifTrue: [^ self openAsMorph: aChangeList name: aString multiSelect: multiSelect].	listHeight _ 70.	annoHeight _ 10.	optButtonHeight _ aChangeList optionalButtonHeight.	codeHeight _ 110.	topView _ (StandardSystemView new)		model: aChangeList;		label: aString;		minimumSize: 200 @ 120;		borderWidth: 1.	aListView _ (multiSelect			ifTrue: [PluggableListViewOfMany]			ifFalse: [PluggableListView])		on: aChangeList		list: #list		selected: #listIndex		changeSelected: #toggleListIndex:		menu: (aChangeList showsVersions			ifTrue: [#versionsMenu:]			ifFalse: [#changeListMenu:])		keystroke: #changeListKey:from:.	aListView window: (0 @ 0 extent: 200 @ listHeight).	topView addSubView: aListView.	underPane _ aListView.	aChangeList wantsAnnotationPane		ifTrue:			[annotationPane _ PluggableTextView				on: aChangeList				text: #annotation				accept: nil				readSelection: nil				menu: nil.			annotationPane window: (0 @ 0 extent: 200 @ 10).			topView addSubView: annotationPane below: underPane.			underPane _ annotationPane.			codeHeight _ codeHeight - annoHeight].	aChangeList wantsOptionalButtons		ifTrue:			[buttonsView _ aChangeList optionalButtonsView.			buttonsView borderWidth: 1.			topView addSubView: buttonsView below: underPane.			underPane _ buttonsView.			codeHeight _ codeHeight - optButtonHeight].	aBrowserCodeView _ PluggableTextView			on: aChangeList			text: #contents			accept: #contents:			readSelection: #contentsSelection			menu: #codePaneMenu:shifted:.	aBrowserCodeView			controller: ReadOnlyTextController new;			window: (0 @ 0 extent: 200 @ codeHeight).	topView addSubView: aBrowserCodeView below: underPane.	topView controller open.! !!PluggableListMorph methodsFor: 'initialization' stamp: 'sw 1/18/2001 13:08'!keystrokeActionSelector: keyActionSel	"Set the keystroke action selector as specified"	keystrokeActionSelector _ keyActionSel! !