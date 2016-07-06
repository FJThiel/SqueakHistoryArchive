'From Squeak 2.4c of May 10, 1999 on 27 July 1999 at 5:25:42 pm'!"Change Set:		ObjectExplorerDate:			21 June 1999Author:			Bob ArningObjectExplorer provides a hierarchical alternative to #inspect.Simply evaluate an expression like:World exploreand enjoy."!Object subclass: #AbstractHierarchicalList	instanceVariableNames: 'currentSelection myBrowser '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!Object subclass: #ListItemWrapper	instanceVariableNames: 'item model '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!MenuMorph subclass: #DumberMenuMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!StringMorph subclass: #IndentingListItemMorph	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling highlightedForDrop isBeingDragged '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!AbstractHierarchicalList subclass: #ObjectExplorer	instanceVariableNames: 'rootObject '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!SystemWindow subclass: #ObjectExplorerWindow	instanceVariableNames: 'topPanelOffset '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!ListItemWrapper subclass: #ObjectExplorerWrapper	instanceVariableNames: 'itemName '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!ScrollPane subclass: #SimpleHierarchicalListMorph	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect expandedForm notExpandedForm columns sortingSelector getSelectionSelector setSelectionSelector '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!Morph subclass: #VeryPickyMorph	instanceVariableNames: 'passengerMorph '	classVariableNames: ''	poolDictionaries: ''	category: 'Interface-Explorer'!!Object methodsFor: 'user interface' stamp: 'RAA 6/21/1999 11:35'!asExplorerString	^self asString! !!Object methodsFor: 'user interface' stamp: 'RAA 6/21/1999 15:32'!explore	^ObjectExplorer new openExplorerFor: self! !!Object methodsFor: 'user interface' stamp: 'RAA 6/21/1999 11:27'!hasContentsInExplorer	^self basicSize > 0 or: [self class allInstVarNames isEmpty not]! !!Object methodsFor: 'converting' stamp: 'RAA 3/31/1999 12:13'!withoutListWrapper	^self! !!AbstractHierarchicalList commentStamp: 'di 7/27/1999 17:15' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!AbstractHierarchicalList methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:22'!genericMenu: aMenu	aMenu add: 'no menu yet' target: self selector: #yourself.	^aMenu! !!AbstractHierarchicalList methodsFor: 'as yet unclassified' stamp: 'RAA 4/7/1999 16:44'!getCurrentSelection	^currentSelection! !!AbstractHierarchicalList methodsFor: 'as yet unclassified' stamp: 'RAA 4/7/1999 16:46'!noteNewSelection: x	currentSelection _ x.	self changed: #getCurrentSelection.	currentSelection ifNil: [^self].	currentSelection sendSettingMessageTo: self.! !!AbstractHierarchicalList methodsFor: 'as yet unclassified' stamp: 'RAA 4/7/1999 16:53'!perform: selector orSendTo: otherTarget	"Selector was just chosen from a menu by a user.  If can respond, thenperform it on myself. If not, send it to otherTarget, presumably theeditPane from which the menu was invoked."	(self respondsTo: selector)		ifTrue: [^ self perform: selector]		ifFalse: [^ otherTarget perform: selector]! !!AbstractHierarchicalList methodsFor: 'as yet unclassified' stamp: 'RAA 4/7/1999 16:47'!update: aSymbol	aSymbol == #hierarchicalList ifTrue: [		^self changed: #getList	].	super update: aSymbol! !!ByteArray methodsFor: 'accessing' stamp: 'RAA 6/21/1999 15:53'!asExplorerString	^self printString	! !!ListItemWrapper commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 18:13'!acceptDroppingObject: anotherItem	^item acceptDroppingObject: anotherItem! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 18:17'!asString	^item asString! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 12:25'!canBeDragged	^true! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:32'!contents	^Array new! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:24'!handlesMouseOver: evt	^false! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 4/1/1999 20:09'!hasContents	^self contents isEmpty not! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 12:15'!hasEquivalentIn: aCollection	aCollection detect: [ :each | 		each withoutListWrapper = item withoutListWrapper	] ifNone: [^false].	^true! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 4/2/1999 15:14'!preferredColor	^nil! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 4/4/1999 17:58'!sendSettingMessageTo: aModel	aModel 		perform: (self settingSelector ifNil: [^self])		with: self withoutListWrapper! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 18:27'!setItem: anObject	item _ anObject! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:44'!setItem: anObject model: aModel	item _ anObject.	model _ aModel.! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 21:31'!settingSelector	^nil! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:32'!wantsDroppedObject: anotherItem	^false! !!ListItemWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 12:13'!withoutListWrapper	^item withoutListWrapper! !!ListItemWrapper class methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 18:28'!with: anObject	^self new setItem: anObject! !!ListItemWrapper class methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:44'!with: anObject model: aModel	^self new setItem: anObject model: aModel! !!Morph methodsFor: 'submorphs-add/remove' stamp: 'RAA 4/2/1999 16:56'!addAllMorphs: aCollection after: anotherMorph	| index |	index _ submorphs indexOf: anotherMorph ifAbsent: [submorphs size].	aCollection do: [:m |		m owner ifNotNil: [m owner privateRemoveMorph: m].		m layoutChanged.		m privateOwner: self].	submorphs _ (submorphs copyFrom: 1 to: index), aCollection,			(submorphs copyFrom: index+1 to: submorphs size).	self layoutChanged.! !!Morph methodsFor: 'submorphs-add/remove' stamp: 'RAA 6/21/1999 14:57'!removeAllMorphsIn: aCollection	| set |	self changed.	aCollection do: [:m | m privateOwner: nil].	set _ aCollection asSet.	submorphs _ submorphs reject: [ :each | set includes: each].	self layoutChanged.! !!Morph methodsFor: 'debug and other' stamp: 'di 7/27/1999 17:04'!debuggingMenuFor: aHandMorph	| aMenu aPlayer |	aMenu _ MenuMorph new defaultTarget: self.	(self hasProperty: #errorOnDraw) ifTrue:		[aMenu add: 'start drawing again' action: #resumeAfterDrawError.		aMenu addLine].	(self hasProperty: #errorOnStep) ifTrue:		[aMenu add: 'start stepping again' action: #resumeAfterStepError.		aMenu addLine].	aMenu add: 'control-menu...' target: aHandMorph selector: #invokeMetaMenuFor: argument: self.	aMenu add: 'inspect morph' action: #inspectInMorphic.	Smalltalk isMorphic ifFalse:		[aMenu add: 'inspect morph (in MVC)' action: #inspectArgument].     aMenu add: 'explore morph' target: aHandMorph selector: #exploreArgument.	aMenu add: 'browse morph class' target: aHandMorph selector: #browseMorphClassFor: argument: self.	(aPlayer _ self player) ifNotNil:		[aMenu add: 'inspect player' target: aPlayer action: #inspect.		World ifNil: [aMenu add: 'inspect player (morphic)' action: #inspectArgumentsPlayerInMorphic].		aMenu add: 'browse player class' target: aPlayer action: #inspect].	aMenu add: 'make own subclass' action: #subclassMorph.	aMenu add: 'internal name ' action: #choosePartName.	aMenu add: 'save morph in file'  action: #saveOnFile.	aMenu addLine.	aMenu add: 'call #tempCommand' target: aHandMorph action: #callTempCommand.	aMenu add: 'define #tempCommand' target: aHandMorph action: #defineTempCommand.	aMenu addLine.	aMenu add: 'edit balloon help' action: #editBalloonHelpText.	^ aMenu! !!DumberMenuMorph commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!DumberMenuMorph methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:40'!setInvokingView: invokingView	"I'd rather not, if that's OK"! !!HandMorph methodsFor: 'world menu' stamp: 'di 7/27/1999 17:01'!debugMenu	"Build the scripting menu for the world."	| menu |	menu _ (MenuMorph entitled: 'debug...') defaultTarget: self.	menu addStayUpItem.	menu add: 'inspect world' target: owner action: #inspect.	menu add: 'explore world' target: owner action: #explore.	menu add: 'inspect model' action: #inspectWorldModel.	menu add: 'start MessageTally' action: #startMessageTally.	menu addLine.	menu add: 'call #tempCommand' action: #callTempCommand.	menu add: 'define #tempCommand' action: #defineTempCommand.	^ menu! !!HandMorph methodsFor: 'world menu commands' stamp: 'Kafka 6/24/1999 05:43'!exploreArgument    self attachMorph: (ObjectExplorer new explorerFor: argument)! !!IndentingListItemMorph commentStamp: 'di 7/27/1999 17:13' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.Don't blame him if it's not perfect.  We wanted to get it out for people to play with.!!IndentingListItemMorph reorganize!('mouse events' acceptDroppingMorph:event: handlesMouseDown: handlesMouseOver: handlesMouseOverDragging: inToggleArea: mouseDown: mouseEnter: mouseEnterDragging: mouseLeave: mouseLeaveDragging: wantsDroppedMorph:event:)('drawing' drawOn: drawToggleOn:in:)('initialization' initWithContents:prior:forList: initialize)('as yet unclassified' addChildrenForList:addingTo:withExpandedItems: canExpand clearDropHighlighting complexContents indentLevel: isExpanded isExpanded: nextSibling nextSibling: noLongerBeingDragged recomputeAllForList:addingTo:withExpandedItems: recomputeForList:addingTo:withExpandedItems: recursiveAddTo: recursiveDelete toggleExpandedState toggleRectangle withSiblingsDo: withoutListWrapper)!!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 7/16/1998 12:02'!acceptDroppingMorph: toDrop event: evt. 	complexContents acceptDroppingObject: toDrop complexContents.	toDrop delete.	self clearDropHighlighting.! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 7/11/1998 17:32'!handlesMouseDown: evt	^true! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 4/11/1999 18:58'!handlesMouseOver: evt	^false	"^complexContents handlesMouseOver: evt"! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 4/11/1999 18:58'!handlesMouseOverDragging: evt	^complexContents handlesMouseOver: evt! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 7/13/1998 17:15'!inToggleArea: event	^self toggleRectangle containsPoint: event cursorPoint	! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 3/31/1999 14:57'!mouseDown: evt		| carrier firstPoint |	firstPoint _ Sensor cursorPoint.	"ugly"	complexContents canBeDragged ifFalse: [^container mouseDown: evt onItem: self]. 	[true] whileTrue: [		Sensor anyButtonPressed ifFalse: [^container mouseDown: evt onItem: self]. 		(Sensor cursorPoint dist: firstPoint) > 3 ifTrue: [			carrier _ VeryPickyMorph new.			carrier passengerMorph: self.			evt hand grabMorph: carrier.			carrier position: evt hand position - (carrier extent // 2).			isBeingDragged _ true.			self changed.			^self		].	].! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 4/11/1999 18:57'!mouseEnter: evt		evt hand hasSubmorphs ifTrue: [ highlightedForDrop _ true].	self changed! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 4/11/1999 18:59'!mouseEnterDragging: evt		evt hand hasSubmorphs ifTrue: [ highlightedForDrop _ true].	self changed! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 7/16/1998 12:02'!mouseLeave: evt	self clearDropHighlighting! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 4/11/1999 18:59'!mouseLeaveDragging: evt	self clearDropHighlighting! !!IndentingListItemMorph methodsFor: 'mouse events' stamp: 'RAA 7/16/1998 12:08'!wantsDroppedMorph: aMorph event: evt	^(aMorph isKindOf: VeryPickyMorph) and: 		[complexContents wantsDroppedObject: aMorph complexContents]! !!IndentingListItemMorph methodsFor: 'drawing' stamp: 'RAA 4/2/1999 15:17'!drawOn: aCanvas	| tRect sRect columnRect columnScanner columnData columnLeft colorToUse |	tRect := self toggleRectangle.	sRect := bounds withLeft: tRect right + 3.	self drawToggleOn: aCanvas in: tRect.	colorToUse _ complexContents preferredColor ifNil: [color].	(container columns isNil or: [(contents asString indexOf: Character tab) = 0]) ifTrue: [		aCanvas text: contents asString bounds: sRect font: font color: colorToUse.	] ifFalse: [		columnLeft _ sRect left.		columnScanner _ ReadStream on: contents asString.		container columns do: [ :width |			columnRect _ columnLeft @ sRect top extent: width @ sRect height.			columnData _ columnScanner upTo: Character tab.			columnData isEmpty ifFalse: [				aCanvas text: columnData bounds: columnRect font: font color: colorToUse.			].			columnLeft _ columnRect right + 5.		].	].	highlightedForDrop ifNotNil: [aCanvas frameRectangle: bounds color: Color blue].! !!IndentingListItemMorph methodsFor: 'drawing' stamp: 'RAA 6/1/1999 10:28'!drawToggleOn: aCanvas in: aRectangle	| aForm |	aCanvas 		fillRectangle: (bounds withRight: aRectangle right)		color: container color.	complexContents hasContents ifFalse: [^self].	aForm _ isExpanded 		ifTrue: [container expandedForm]		ifFalse: [container notExpandedForm].	^aCanvas 		image: aForm 		at: aRectangle topLeft		sourceRect: aForm boundingBox 		rule: Form paint! !!IndentingListItemMorph methodsFor: 'initialization' stamp: 'RAA 4/13/1999 21:45'!initWithContents: anObject prior: priorMorph forList: hostList	container _ hostList.	complexContents _ anObject.	self initWithContents: anObject asString font: nil emphasis: nil.	indentLevel _ 0.	isExpanded _ false. 	nextSibling _ firstChild _ nil.	priorMorph ifNotNil: [		priorMorph nextSibling: self.	].! !!IndentingListItemMorph methodsFor: 'initialization' stamp: 'RAA 7/15/1998 00:14'!initialize	super initialize.	indentLevel := 0.	isExpanded := false.! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 22:28'!addChildrenForList: hostList addingTo: aCollection withExpandedItems: expandedItems	| newChildren |	firstChild ifNotNil: [		firstChild withSiblingsDo: [ :aNode | aNode delete].	].	complexContents hasContents ifFalse: [^self].	newChildren _ hostList 		morphsFromCollection: complexContents contents 		allowSorting: true		withExpandedItems: expandedItems.	firstChild _ newChildren first.	firstChild withSiblingsDo: [ :aNode |		aNode 			indentLevel: indentLevel + 1;			recomputeForList: hostList addingTo: aCollection withExpandedItems: expandedItems.	].	! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 17:44'!canExpand	^complexContents hasContents! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/16/1998 12:02'!clearDropHighlighting	highlightedForDrop ifNil: [^self].	highlightedForDrop _ nil.	self changed! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/1998 14:34'!complexContents	^complexContents! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/1998 18:39'!indentLevel: anInteger	indentLevel _ anInteger! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/31/1998 00:30'!isExpanded	^isExpanded! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/31/1998 00:48'!isExpanded: aBoolean	isExpanded _ aBoolean! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/1998 12:15'!nextSibling	^nextSibling! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 8/1/1998 01:05'!nextSibling: anotherMorph	nextSibling _ anotherMorph! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/22/1998 01:04'!noLongerBeingDragged	isBeingDragged _ nil.! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 16:34'!recomputeAllForList: hostList addingTo: aCollection withExpandedItems: expandedItems	self withSiblingsDo: [ :aNode |		aNode 			recomputeForList: hostList 			addingTo: aCollection 			withExpandedItems: expandedItems.	].! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 23:36'!recomputeForList: hostList addingTo: aCollection withExpandedItems: expandedItems	self contents: complexContents asString.	aCollection add: self.	isExpanded ifTrue: [		self 			addChildrenForList: hostList 			addingTo: aCollection 			withExpandedItems: expandedItems].! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 14:54'!recursiveAddTo: aCollection	firstChild ifNotNil: [		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: aCollection].	].	aCollection add: self	! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 4/2/1999 18:02'!recursiveDelete	firstChild ifNotNil: [		firstChild withSiblingsDo: [ :aNode | aNode recursiveDelete].	].	self delete	! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 14:56'!toggleExpandedState 	| newChildren toDelete |	isExpanded _ isExpanded not.	toDelete _ OrderedCollection new.	firstChild ifNotNil: [		firstChild withSiblingsDo: [ :aNode | aNode recursiveAddTo: toDelete].	].	owner removeAllMorphsIn: toDelete.	(isExpanded and: [complexContents hasContents]) ifFalse: [		^self changed	].	newChildren _ container 		addSubmorphsAfter: self 		fromCollection: complexContents contents 		allowSorting: true.	firstChild _ newChildren first.	firstChild withSiblingsDo: [ :aNode |		aNode indentLevel: indentLevel + 1.	].! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/1998 11:12'!toggleRectangle	| h |	h _ bounds height.	^(bounds left + (h * indentLevel)) @ bounds top extent: h@h! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/1998 14:25'!withSiblingsDo: aBlock	| node |	node _ self.	[node isNil] whileFalse: [		aBlock value: node.		node _ node nextSibling	].! !!IndentingListItemMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 12:13'!withoutListWrapper	^complexContents withoutListWrapper! !!ObjectExplorer commentStamp: '<historical>' prior: 0!ObjectExplorer provides a hierarchical alternative to #inspect. Simply evaluate an expression like:World exploreand enjoy.!]style[(101 13 12)f1,f3cblue;,f1!!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'Kafka 6/24/1999 05:40'!explorerFor: anObject	| window listMorph |	rootObject _ anObject.	(window _ ObjectExplorerWindow labelled: 'Explorer')		model: self;		color: Color red.	window addMorph: (listMorph _ SimpleHierarchicalListMorph 			on: self			list: #getList			selected: #getCurrentSelection			changeSelected: #noteNewSelection:			menu: #genericMenu:)		frame: (0@0 corner: 1@1).	listMorph autoDeselect: false.     ^ window! !!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:51'!genericMenu: aMenu	| insideObject menu |	currentSelection ifNil: [		menu _ aMenu.		menu add: '*nothing selected*' target: self selector: #yourself.	] ifNotNil: [		menu _ DumberMenuMorph new defaultTarget: self.		insideObject _ currentSelection withoutListWrapper.		menu 			add: 'explore' target: insideObject  selector: #explore;			add: 'inspect' target: insideObject  selector: #inspect;			add: 'basic inspect' target: insideObject  selector: #basicInspect;			add: 'smart inspect' target: insideObject  selector: #smartInspect.	].	^menu! !!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:22'!getList	^Array with: (ObjectExplorerWrapper with: rootObject name: 'root' model: self)! !!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'Kafka 6/24/1999 05:47'!openExplorerFor: anObject"ObjectExplorer new openExplorerFor: Smalltalk"    (self explorerFor: anObject) openInWorldExtent: 300@500.    ^ self! !!ObjectExplorer class methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:55'!about	StringHolder new textContents: self comment; openLabel: 'about ',self asString! !!ObjectExplorerWindow commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:50'!initialize	super initialize.	label on: #mouseDown send: nil to: nil.	"kill the titlebar relabeling"! !!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 4/3/1999 00:23'!mouseDown: evt	| offset newBounds |	self activate.	(Sensor redButtonPressed "If mouse is really still down after activate"		and: [self labelRect containsPoint: evt cursorPoint]) ifTrue:		[Preferences fastDragWindowForMorphic		ifTrue: [offset _ self position - Sensor cursorPoint.				newBounds _ self bounds newRectFrom:					[:f | Sensor cursorPoint + offset extent: self extent].				^ self position: newBounds topLeft]		ifFalse: [^ evt hand grabMorph: self topRendererOrSelf]].	true "model windowActiveOnFirstClick" ifTrue:		["Normally window keeps control of first click.		Need explicit transmission for first-click activity."		evt hand handleMouseDown: evt.	"this is more like it"	]! !!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 3/15/1999 00:20'!panelRect	"Return the area below title bar, devoted to panes"	^ self innerBounds topLeft + (0@(self labelHeight + (topPanelOffset ifNil: [0])))					corner: self innerBounds bottomRight! !!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 3/19/1999 23:41'!relabel	! !!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 3/22/1999 18:37'!spawnPaneFrameHandle: event	| resizer localPt side growingPane newBounds r |	r _ self panelRect.	r _ r withTop: r top + 4.	(r containsPoint: event cursorPoint)		ifFalse: [^ self "in label or top of top pane"].	growingPane _ self paneWithLongestSide: [:s | side _ s] near: event cursorPoint.	growingPane ifNil: [^ self].	resizer _ NewHandleMorph new followHand: event hand		forEachPointDo:			[:p | localPt _ self pointFromWorld: p.			side = #top ifTrue: [newBounds _ growingPane bounds withTop: localPt y].			side = #bottom ifTrue: [newBounds _ growingPane bounds withBottom: localPt y].			side = #left ifTrue: [newBounds _ growingPane bounds withLeft: localPt x].			side = #right ifTrue: [newBounds _ growingPane bounds withRight: localPt x].			self reframePanesAdjoining: growingPane along: side to: newBounds]		lastPointDo: [:p | ].	event hand world addMorph: resizer.	resizer startStepping! !!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'RAA 3/15/1999 00:20'!topPanelOffset: anInteger	topPanelOffset _ anInteger! !!ObjectExplorerWrapper commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 11:34'!asString	^itemName,': ',item asExplorerString! !!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 15:48'!canBeDragged	^false! !!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 11:13'!contents	| answer |	answer _ OrderedCollection new.	item class allInstVarNames asArray doWithIndex: [ :each :index |		answer add: (ObjectExplorerWrapper with: (item instVarAt: index) name: each model: item)	].	1 to: item basicSize do: [ :index |		answer add: (ObjectExplorerWrapper with: (item basicAt: index) name: index printString model: item)	].	^answer! !!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 11:27'!hasContents	^item hasContentsInExplorer	! !!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 10:49'!setItem: anObject name: aString model: aModel	item _ anObject.	model _ aModel.	itemName _ aString.! !!ObjectExplorerWrapper class methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 10:50'!with: anObject name: aString model: aModel	^self new 		setItem: anObject name: aString model: aModel! !!SimpleHierarchicalListMorph commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.Don't blame him if it's not perfect.  We wanted to get it out for people to play with.!!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'di 4/10/98 16:20'!autoDeselect: trueOrFalse	"Enable/disable autoDeselect (see class comment)"	autoDeselect _ trueOrFalse.! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'RAA 3/30/1999 22:29'!currentlyExpanded	^(scroller submorphs select: [ :each | each isExpanded]) collect: [ :each |		each complexContents	].	! !!SimpleHierarchicalListMorph methodsFor: 'initialization'!extent: newExtent	super extent: newExtent.	self setScrollDeltas ! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'di 5/6/1998 21:19'!installModelIn: aWorld	"No special inits for new components"	^ self! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'RAA 3/30/1999 22:30'!list: aCollection	| wereExpanded |	wereExpanded _ self currentlyExpanded.	scroller removeAllMorphs.	(aCollection isNil or: [aCollection isEmpty]) ifTrue: [^ self selectedMorph: nil].	self insertNewMorphs: 		(self 			morphsFromCollection: aCollection 			allowSorting: false			withExpandedItems: wereExpanded).	! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'di 5/22/1998 00:32'!listItemHeight	"This should be cleaned up.  The list should get spaced by this parameter."	^ 12! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'RAA 8/1/1998 00:19'!on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel	self model: anObject.	getListSelector _ getListSel.	getSelectionSelector _ getSelectionSel.	setSelectionSelector _ setSelectionSel.	getMenuSelector _ getMenuSel.	keystrokeActionSelector _ keyActionSel.	autoDeselect _ true.	self borderWidth: 1.	self list: self getList.! !!SimpleHierarchicalListMorph methodsFor: 'initialization' stamp: 'RAA 3/30/1999 18:37'!setScrollDeltas	| range delta |	scroller hasSubmorphs ifFalse: [^ scrollBar interval: 1.0].	range _ self leftoverScrollRange.	delta _ scroller firstSubmorph height.	range = 0 ifTrue: [^ scrollBar scrollDelta: 0.02 pageDelta: 0.2; interval: 1.0].	"Set up for one line, or a full pane less one line"	scrollBar scrollDelta: (delta / range) asFloat 			pageDelta: (self innerBounds height - delta / range) asFloat.	scrollBar interval: ((self innerBounds height - delta) / self totalScrollRange) asFloat.! !!SimpleHierarchicalListMorph methodsFor: 'drawing' stamp: 'RAA 3/30/1999 17:44'!drawOn: aCanvas	super drawOn: aCanvas.	selectedMorph ifNotNil:		[aCanvas fillRectangle:			(((scroller transformFrom: self) invertBoundsRect: selectedMorph bounds)						intersect: scroller bounds)				color: color darker]! !!SimpleHierarchicalListMorph methodsFor: 'drawing'!highlightSelection	selectedMorph ifNotNil: [selectedMorph color: Color red]! !!SimpleHierarchicalListMorph methodsFor: 'drawing' stamp: 'RAA 4/14/1999 10:17'!unhighlightSelection	selectedMorph ifNotNil: [selectedMorph color: Color black]! !!SimpleHierarchicalListMorph methodsFor: 'events' stamp: 'RAA 5/16/1999 09:58'!mouseDown: event onItem: aMorph	| oldState |	(aMorph inToggleArea: event) ifTrue: [		"self setSelectedMorph: aMorph."		event macOptionKeyPressed ifTrue: [			oldState _ aMorph isExpanded.			scroller submorphs copy do: [ :each |				(each canExpand and: [each isExpanded = oldState]) ifTrue: [					each toggleExpandedState				].			].		] ifFalse: [			aMorph toggleExpandedState.		].		self adjustSubmorphPositions.		^self	].	event yellowButtonPressed ifTrue: [^ self yellowButtonActivity: event shiftPressed].	model okToChange ifFalse: [^ self].  "No change if model is locked"	((autoDeselect == nil or: [autoDeselect]) and: [aMorph == selectedMorph])		ifTrue: [self setSelectedMorph: nil]		ifFalse: [self setSelectedMorph: aMorph].! !!SimpleHierarchicalListMorph methodsFor: 'events' stamp: 'di 5/22/1998 00:02'!mouseEnter: event	super mouseEnter: event.	event hand newKeyboardFocus: self! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'RAA 7/31/1998 00:25'!maximumSelection	^ scroller submorphs size! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'di 5/22/1998 00:20'!minimumSelection	^ 1! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'di 5/22/1998 00:32'!numSelectionsInView	^ self height // self listItemHeight! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'RAA 8/1/1998 00:13'!selectedMorph: aMorph	self unhighlightSelection.	selectedMorph _ aMorph.	self highlightSelection! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'RAA 4/3/1999 16:20'!selection: item	"Called from outside to request setting a new selection.	Assumes scroller submorphs is exactly our list.	Note: MAY NOT work right if list includes repeated items"	| i |	i _ scroller submorphs findFirst: [:m | m complexContents == item].	i > 0 ifTrue: [^self selectionIndex: i].	i _ scroller submorphs findFirst: [:m | m withoutListWrapper = item withoutListWrapper].	self selectionIndex: i! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'di 5/6/1998 11:02'!selectionIndex: index	"Called internally to select the index-th item."	| theMorph range |	index ifNil: [^ self].	(theMorph _ index = 0 ifTrue: [nil] ifFalse: [scroller submorphs at: index])		ifNotNil:		[((theMorph bounds top - scroller offset y) >= 0			and: [(theMorph bounds bottom - scroller offset y) <= bounds height]) ifFalse:			["Scroll into view -- should be elsewhere"			range _ self totalScrollRange.			scrollBar value: (range > 0				ifTrue: [((index-1 * theMorph height) / self totalScrollRange)									truncateTo: scrollBar scrollDelta]				ifFalse: [0]).			scroller offset: -3 @ (range * scrollBar value)]].	self selectedMorph: theMorph! !!SimpleHierarchicalListMorph methodsFor: 'selection' stamp: 'RAA 3/31/1999 21:28'!setSelectedMorph: aMorph	model 		perform: (setSelectionSelector ifNil: [^self]) 		with: aMorph complexContents	"leave last wrapper in place" ! !!SimpleHierarchicalListMorph methodsFor: 'model access' stamp: 'RAA 8/1/1998 00:10'!getList 	"Answer the list to be displayed."	^(model perform: (getListSelector ifNil: [^#()])) ifNil: [#()]! !!SimpleHierarchicalListMorph methodsFor: 'model access' stamp: 'di 5/22/1998 00:34'!keyStroke: event	"Process potential command keys"	| args aCharacter |	keystrokeActionSelector == nil ifTrue: [^ nil].	aCharacter _ event keyCharacter.	(args _ keystrokeActionSelector numArgs) = 1		ifTrue: [^ model perform: keystrokeActionSelector with: aCharacter].	args = 2		ifTrue: [^ model perform: keystrokeActionSelector with: aCharacter with: self].	^ self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !!SimpleHierarchicalListMorph methodsFor: 'updating' stamp: 'RAA 8/1/1998 00:19'!update: aSymbol 	aSymbol == getSelectionSelector ifTrue:		[self selection: self getCurrentSelectionItem.		^ self].	aSymbol == getListSelector ifTrue: 		[self list: self getList.		^ self].! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 4/2/1999 16:53'!addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean	| priorMorph morphList newCollection |	priorMorph _ nil.	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [		(aCollection asSortedCollection: [ :a :b | 			(a perform: sortingSelector) <= (b perform: sortingSelector)]) asOrderedCollection	] ifFalse: [		aCollection	].	morphList _ OrderedCollection new.	newCollection do: [:item | 		priorMorph _ IndentingListItemMorph basicNew 			initWithContents: item 			prior: priorMorph 			forList: self.		morphList add: priorMorph.	].	scroller addAllMorphs: morphList after: parentMorph.	^morphList	! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 6/21/1999 11:25'!adjustSubmorphPositions	| p h |	p _ 0@0.	scroller submorphsDo: [ :each |		h _ each height.		each privateBounds: (p extent: 9999@h).		p _ p + (0@h)	].	self 		changed;		layoutChanged;		setScrollDeltas.! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/24/1998 22:52'!columns	^columns! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/18/1998 23:18'!columns: anArray	columns _ anArray! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 5/16/1999 10:26'!expandedForm	^expandedForm ifNil: [expandedForm _ Form	extent: 10@10	depth: 8	fromArray: #( 4294967295 4294967295 4294901760 4294967295 4294967295 4294901760 4294967295 4294967295 4294901760 4278255873 16843009 16842752 4294902089 1229539657 33488896 4294967041 1229539585 4294901760 4294967295 21561855 4294901760 4294967295 4278321151 4294901760 4294967295 4294967295 4294901760 4294967295 4294967295 4294901760)	offset: 0@0.	expandedForm replaceColor: Color white withColor: color.	expandedForm	].! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 8/1/1998 00:19'!getCurrentSelectionItem	^model perform: (getSelectionSelector ifNil: [^nil])	! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 4/2/1999 15:33'!insertNewMorphs: morphList	scroller addAllMorphs: morphList.	self adjustSubmorphPositions.	self selection: self getCurrentSelectionItem.	self setScrollDeltas.! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/30/1999 23:38'!morphsFromCollection: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems	| priorMorph morphList newCollection |	priorMorph _ nil.	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [		(aCollection asSortedCollection: [ :a :b | 			(a perform: sortingSelector) <= (b perform: sortingSelector)]) asOrderedCollection	] ifFalse: [		aCollection	].	morphList _ OrderedCollection new.	newCollection do: [:item | 		priorMorph _ IndentingListItemMorph basicNew 			initWithContents: item 			prior: priorMorph 			forList: self.		morphList add: priorMorph.		(item hasEquivalentIn: expandedItems) ifTrue: [			priorMorph isExpanded: true.			priorMorph 				addChildrenForList: self 				addingTo: morphList				withExpandedItems: expandedItems.		].	].	^morphList	! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 5/16/1999 10:23'!notExpandedForm	^notExpandedForm ifNil: [notExpandedForm _ Form	extent: 10@10	depth: 8	fromArray: #( 4294967295 4294967295 4294901760 4294967041 4294967295 4294901760 4294967041 33554431 4294901760 4294967041 1224867839 4294901760 4294967041 1229521407 4294901760 4294967041 1229539585 4294901760 4294967041 1229521407 4294901760 4294967041 1224867839 4294901760 4294967041 33554431 4294901760 4294967041 4294967295 4294901760)	offset: 0@0.	notExpandedForm replaceColor: Color white withColor: color.	notExpandedForm]! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 12:15'!selectionOneOf: aListOfItems	"Set the selection to the first item in the list which is represented by one of my submorphs"	| index |	aListOfItems do: [ :item |		index _ scroller submorphs findFirst: [:m | 			m withoutListWrapper = item withoutListWrapper		].		index > 0 ifTrue: [^self selectionIndex: index].	].	self selectionIndex: 0.! !!SimpleHierarchicalListMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/20/1998 12:09'!sortingSelector: s	sortingSelector _ s! !!SimpleHierarchicalListMorph class methodsFor: 'instance creation' stamp: 'di 5/22/1998 00:17'!on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See ListView>>aboutPluggability comment."	^ self new		on: anObject		list: getListSel		selected: getSelectionSel		changeSelected: setSelectionSel		menu: nil		keystroke: #arrowKey:from:		"default"! !!SimpleHierarchicalListMorph class methodsFor: 'instance creation' stamp: 'di 5/22/1998 00:17'!on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See ListView>>aboutPluggability comment."	^ self new		on: anObject		list: getListSel		selected: getSelectionSel		changeSelected: setSelectionSel		menu: getMenuSel		keystroke: #arrowKey:from:		"default"! !!SimpleHierarchicalListMorph class methodsFor: 'instance creation' stamp: 'di 5/6/1998 21:45'!on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel	"Create a 'pluggable' list view on the given model parameterized by the given message selectors. See ListView>>aboutPluggability comment."	^ self new		on: anObject		list: getListSel		selected: getSelectionSel		changeSelected: setSelectionSel		menu: getMenuSel		keystroke: keyActionSel! !!String methodsFor: 'testing' stamp: 'RAA 6/21/1999 11:34'!asExplorerString	^self asString! !!String methodsFor: 'testing' stamp: 'RAA 6/21/1999 11:28'!hasContentsInExplorer	^false! !!VeryPickyMorph commentStamp: 'di 7/27/1999 17:14' prior: 0!Contributed by Bob Arning as part of the ObjectExplorer package.!!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/15/1998 21:55'!complexContents	^passengerMorph complexContents! !!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/15/1998 22:02'!delete	passengerMorph ifNotNil: [passengerMorph delete].	super delete! !!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 14:58'!drawOn: aCanvas	aCanvas frameRectangle: bounds width: 1 color: Color red! !!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 14:58'!initialize	super initialize.	bounds _ 0@0 extent: 8@10	"bounds _ 0@0 extent: 17@22"! !!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/31/1999 15:23'!justDroppedInto: targetMorph event: evt	passengerMorph ifNil: [^self "delete"].	passengerMorph noLongerBeingDragged.	(targetMorph isKindOf: IndentingListItemMorph) ifFalse: [		passengerMorph changed.		passengerMorph _ nil.		owner privateRemoveMorph: self.		self privateOwner: nil.	].! !!VeryPickyMorph methodsFor: 'as yet unclassified' stamp: 'RAA 7/15/1998 21:53'!passengerMorph: anotherMorph	passengerMorph _ anotherMorph! !"Postscript:"!