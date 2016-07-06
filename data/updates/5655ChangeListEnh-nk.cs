'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5623] on 7 January 2004 at 11:13:47 am'!"Change Set:		ChangeListEnh-nkDate:			7 January 2004Author:			Ned KonzSome enhancements for the ChangeList and VersionsBrowser:- compare (one or several) change(s) in the ChangeList with the current and all the historical versions in the image.- ignore LF characters when diffing method text. May want this to be a preference, I'm not sure.- ignore whitespace changes when looking for unchanged methods. May want this to be a preference, I'm not sure."!ChangeList subclass: #VersionsBrowser	instanceVariableNames: 'classOfMethod selectorOfMethod addedChangeRecord '	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-Changes'!!ChangeRecord methodsFor: 'access' stamp: 'nk 1/7/2004 10:28'!fileName	^(file ifNotNil: [ file name ]) 			ifNil: [ '<no file>' ]! !!StringHolder methodsFor: 'message list menu' stamp: 'nk 1/7/2004 10:20'!browseVersions	"Create and schedule a Versions Browser, showing all versions of the 	currently selected message. Answer the browser or nil."	| selector class | 	(selector _ self selectedMessageName)		ifNil:[ self inform: 'Sorry, only actual methods have retrievable versions.'. ^nil ]		ifNotNil: [(MessageSet isPseudoSelector: selector)				ifTrue: ["Eliminates Definition and Hierarchy"					^ self classCommentIndicated						ifTrue: [ ClassCommentVersionsBrowser browseCommentOf: self selectedClass. nil ]].			class _ self selectedClassOrMetaClass.			^VersionsBrowser				browseVersionsOf: (class compiledMethodAt: selector)				class: self selectedClass				meta: class isMeta				category: (class organization categoryOfElement: selector)				selector: selector]! !!ChangeList methodsFor: 'menu actions' stamp: 'nk 1/7/2004 11:08'!browseAllVersionsOfSelections	"Opens a Versions browser on all the currently selected methods, showing each alongside all of their historical versions."	|  oldSelection aList |	oldSelection _ self listIndex.	aList _ OrderedCollection new.	Cursor read showWhile: [		1 to: changeList size do: [:i |			(listSelections at: i) ifTrue: [				listIndex _ i.				self browseVersions.				aList add: i.				]]].	listIndex _ oldSelection.	aList size == 0 ifTrue: [^ self inform: 'no selected methods have in-memory counterparts'].! !!ChangeList methodsFor: 'menu actions' stamp: 'nk 1/7/2004 10:23'!browseVersions	| change class browser |	listIndex = 0		ifTrue: [^ nil ].	change _ changeList at: listIndex.	((class _ change methodClass) notNil			and: [class includesSelector: change methodSelector])		ifFalse: [ ^nil ].	browser _ super browseVersions.	browser ifNotNil: [ browser addedChangeRecord: change ].	^browser! !!ChangeList methodsFor: 'menu actions' stamp: 'nk 1/7/2004 11:11'!changeListMenu: aMenu	"Fill aMenu up so that it comprises the primary changelist-browser menu"	Smalltalk isMorphic ifTrue:		[aMenu addTitle: 'change list'.		aMenu addStayUpItemSpecial].	aMenu addList: #(	('fileIn selections'							fileInSelections						'import the selected items into the image')	('fileOut selections...	'						fileOutSelections						'create a new file containing the selected items')	-	('compare to current'						compareToCurrentVersion			'open a separate window which shows the text differences between the on-file version and the in-image version.' )	('toggle diffing (D)'							toggleDiffing						'start or stop showing diffs in the code pane.')	-	('select conflicts with any changeset'		selectAllConflicts					'select methods in the file which also occur in any change-set in the system')	('select conflicts with current changeset'	selectConflicts						'select methods in the file which also occur in the current change-set')	('select conflicts with...'						selectConflictsWith					'allows you to designate a file or change-set against which to check for code conflicts.')	-	('select unchanged methods'					selectUnchangedMethods				'select methods in the file whose in-image versions are the same as their in-file counterparts' )	('select new methods'						selectNewMethods					'select methods in the file that do not current occur in the image')	('select methods for this class'				selectMethodsForThisClass			'select all methods in the file that belong to the currently-selected class')	-	('select all (a)'								selectAll								'select all the items in the list')	('deselect all'								deselectAll							'deselect all the items in the list')	('invert selections'							invertSelections						'select every item that is not currently selected, and deselect every item that *is* currently selected')	-	('browse all versions of single selection'			browseVersions		'open a version browser showing the versions of the currently selected method')	('browse all versions of selections'			browseAllVersionsOfSelections		'open a version browser showing all the versions of all the selected methods')	('browse current versions of selections'		browseCurrentVersionsOfSelections	'open a message-list browser showing the current (in-image) counterparts of the selected methods')	('destroy current methods of selections'		destroyCurrentCodeOfSelections		'remove (*destroy*) the in-image counterparts of all selected methods')	-	('remove doIts'								removeDoIts							'remove all items that are doIts rather than methods')	('remove older versions'						removeOlderMethodVersions			'remove all but the most recent versions of methods in the list')	('remove up-to-date versions'				removeExistingMethodVersions		'remove all items whose code is the same as the counterpart in-image code')	('remove selected items'						removeSelections					'remove the selected items from the change-list')	('remove unselected items'					removeNonSelections					'remove all the items not currently selected from the change-list')).	^ aMenu! !!ChangeList methodsFor: 'menu actions' stamp: 'nk 1/7/2004 09:16'!selectUnchangedMethods	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"	| change class |	Cursor read showWhile: 	[1 to: changeList size do:		[:i | change _ changeList at: i.		listSelections at: i put:			((change type = #method and:				[(class _ change methodClass) notNil]) and:					[(class includesSelector: change methodSelector) and:						[change string withBlanksCondensed = (class sourceCodeAt: change methodSelector) asString withBlanksCondensed ]])]].	self changed: #allSelections! !!ChangeList methodsFor: 'viewing access' stamp: 'nk 1/7/2004 09:50'!selectedClass	^self selectedClassOrMetaClass theNonMetaClass ! !!TextDiffBuilder methodsFor: 'initialize' stamp: 'nk 1/7/2004 09:24'!formatLine: aString	^aString copyWithout: Character lf! !!VersionsBrowser methodsFor: 'init & update' stamp: 'nk 1/7/2004 10:10'!addedChangeRecord	^addedChangeRecord! !!VersionsBrowser methodsFor: 'init & update' stamp: 'nk 1/7/2004 10:23'!addedChangeRecord: aChangeRecord	addedChangeRecord _ aChangeRecord.	self reformulateList.! !!VersionsBrowser methodsFor: 'init & update' stamp: 'nk 1/7/2004 10:29'!scanVersionsOf: method class: class meta: meta category: category selector: selector	| position prevPos prevFileIndex preamble tokens sourceFilesCopy stamp |	selectorOfMethod _ selector.	currentCompiledMethod _ method.	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].	changeList _ OrderedCollection new.	list _ OrderedCollection new.	self addedChangeRecord ifNotNilDo: [ :change |		self addItem: change text: ('{1} (in {2})' translated format: { change stamp. change fileName }) ].	listIndex _ 0.	position _ method filePosition.	sourceFilesCopy _ SourceFiles collect:		[:x | x isNil ifTrue: [ nil ]				ifFalse: [x readOnlyCopy]].	method fileIndex == 0 ifTrue: [^ nil].	file _ sourceFilesCopy at: method fileIndex.	[position notNil & file notNil]		whileTrue:		[file position: (0 max: position-150).  "Skip back to before the preamble"		[file position < (position-1)]  "then pick it up from the front"			whileTrue: [preamble _ file nextChunk].		"Preamble is likely a linked method preamble, if we're in			a changes file (not the sources file).  Try to parse it			for prior source position and file index"		prevPos _ nil.		stamp _ ''.		(preamble findString: 'methodsFor:' startingAt: 1) > 0			ifTrue: [tokens _ Scanner new scanTokens: preamble]			ifFalse: [tokens _ Array new  "ie cant be back ref"].		((tokens size between: 7 and: 8)			and: [(tokens at: tokens size-5) = #methodsFor:])			ifTrue:				[(tokens at: tokens size-3) = #stamp:				ifTrue: ["New format gives change stamp and unified prior pointer"						stamp _ tokens at: tokens size-2.						prevPos _ tokens last.						prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.						prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]				ifFalse: ["Old format gives no stamp; prior pointer in two parts"						prevPos _ tokens at: tokens size-2.						prevFileIndex _ tokens last].				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos _ nil]].		((tokens size between: 5 and: 6)			and: [(tokens at: tokens size-3) = #methodsFor:])			ifTrue:				[(tokens at: tokens size-1) = #stamp:				ifTrue: ["New format gives change stamp and unified prior pointer"						stamp _ tokens at: tokens size]]. 		self addItem:				(ChangeRecord new file: file position: position type: #method						class: class name category: category meta: meta stamp: stamp)			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.		position _ prevPos.		prevPos notNil ifTrue:			[file _ sourceFilesCopy at: prevFileIndex]].	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].	listSelections _ Array new: list size withAll: false! !!VersionsBrowser class methodsFor: 'as yet unclassified' stamp: 'nk 1/7/2004 10:19'!browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer 	| changeList browser |	Cursor read showWhile:		[changeList _ (browser _ self new)			scanVersionsOf: method class: class meta: meta			category: msgCategory selector: selector].	changeList ifNil: [ self inform: 'No versions available'. ^nil ].	sourcePointer ifNotNil:		[changeList setLostMethodPointer: sourcePointer].	self open: changeList name: 'Recent versions of ' ,selector multiSelect: false.	^browser! !ChangeList subclass: #VersionsBrowser	instanceVariableNames: 'classOfMethod selectorOfMethod addedChangeRecord'	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-Changes'!