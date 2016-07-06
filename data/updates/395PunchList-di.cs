'From Squeak 2.2 of Sept 23, 1998 on 10 November 1998 at 11:43:42 am'!"Change Set:		PunchList-diDate:			10 November 1998Author:			Dan IngallsNumerous simple fixes requested by Scott Wallace and Ivan Brusic, including...If you attempt to add an instVar that is already used in a sublcass, you are now warned about the problem before the class hierarchy gets trashed.When you open a browser in morphic with the directive that it pre-select something (class, or more commonly class + method) -- such via a browse-full or browse-it request in a different browser -- it now properly scrolls the selections into view.The size of a window's collapsed image is adjusted appropriately whenever the label is changed.A flash in a text pane is now limited to its visible boundary on the display.Frame adjustment handles are now only offered for the active window.colorUnder now works correctly whether an object's immediate owner is the World or not."!!ClassDescription methodsFor: 'instance variables' stamp: 'di 11/9/1998 20:21'!checkForInstVarsOK: instVarString	"Return true if instVarString does no include any names used in a subclass"	| instVarArray |	instVarArray _ Scanner new scanFieldNames: instVarString.	self allSubclasses do:		[:cl | cl instVarNames do:			[:n | (instVarArray includes: n)				ifTrue: [self error: n , ' is already used in ' , cl name.						^ false]]].	^ true! !!Metaclass methodsFor: 'class hierarchy' stamp: 'di 11/9/1998 20:18'!name: newName inEnvironment: environ subclassOf: sup instanceVariableNames: instVarString variable: v words: w pointers: p weak: beWeak classVariableNames: classVarString poolDictionaries: poolString category: categoryName comment: commentString changed: changed 	"This is the standard initialization message for creating a new Metaclass. 	Answer an instance of me from the information provided in the 	arguments. Create an error notification if the name does not begin with 	an uppercase letter or if a class of the same name already exists.	1/22/96 sw: don't ever do addClass, always do changeClass"	| wasPresent oldClass newClass invalidFields invalidMethods |	newName first isUppercase		ifFalse: 			[self error: 'Class names must be capitalized'.			^false].	(wasPresent _ environ includesKey: newName)		ifTrue: 			[oldClass _ environ at: newName.			(oldClass isKindOf: Behavior)				ifFalse: 					[self error: newName , ' already exists!!  Proceed will store over it'.					wasPresent _ false.					oldClass _ self newNamed: newName].			(oldClass checkForInstVarsOK: instVarString)				ifFalse: [^ false]]		ifFalse: [oldClass _ self newNamed: newName.				Smalltalk flushClassNameCache].	newClass _ oldClass copy.	invalidFields _ changed | (newClass					subclassOf: sup					oldClass: oldClass					instanceVariableNames: instVarString					variable: v					words: w					pointers: p					weak: beWeak					ifBad: [^false]).	invalidFields not & (oldClass instSize = newClass instSize)		ifTrue: [newClass _ oldClass].	invalidMethods _ invalidFields | (newClass declare: classVarString) | 		(newClass sharing: poolString).	commentString == nil ifFalse: [newClass comment: commentString].	(environ includesKey: newName)		ifFalse: [environ declare: newName from: Undeclared].	environ at: newName put: newClass.	SystemOrganization classify: newClass name under: categoryName asSymbol.	newClass		validateFrom: oldClass		in: environ		instanceVariableNames: invalidFields		methods: invalidMethods		wasPresent: wasPresent.	"update subclass lists"	newClass superclass removeSubclass: oldClass.	newClass superclass addSubclass: newClass.	"Update Changes"	Smalltalk changes changeClass: newClass.	^ newClass! !!Morph methodsFor: 'drawing' stamp: 'di 11/10/1998 10:15'!flash	| c w |	c _ self color.	self color: Color black.	(w _ self world) ifNotNil: [w doOneCycle].	self color: c! !!PluggableListMorph methodsFor: 'initialization' stamp: 'di 11/10/1998 09:31'!on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel menu: getMenuSel keystroke: keyActionSel	self model: anObject.	getListSelector _ getListSel.	getIndexSelector _ getSelectionSel.	setIndexSelector _ setSelectionSel.	getMenuSelector _ getMenuSel.	keystrokeActionSelector _ keyActionSel.	autoDeselect _ true.	self borderWidth: 1.	self list: self getList.	self selectionIndex: self getCurrentSelectionIndex.! !!SystemWindow methodsFor: 'label' stamp: 'di 11/10/1998 09:48'!setLabel: aString	labelString _ aString.	label ifNil: [^ self].	label contents: aString.	self isCollapsed		ifTrue: [self extent: (label width + 50) @ (self labelHeight + 2)]		ifFalse: [label fitContents; setWidth: (label width min: bounds width - 50).				label align: label bounds topCenter with: bounds topCenter + (0@borderWidth).				collapsedFrame ifNotNil:					[collapsedFrame _ collapsedFrame withWidth: label width + 50]]! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'di 11/10/1998 10:20'!spawnReframeHandle: event	"The mouse has crossed a pane border.  Spawn a reframe handle."	| resizer localPt pt ptName newBounds |	owner ifNil: [^ self  "Spurious mouseLeave due to delete"].	self isActive ifFalse: [^ self  "Mouse-over of an inactive window"].	paneMorphs do: [:p | ((p fullBounds insetBy: 1) containsPoint: event cursorPoint)			ifTrue: [^ self  "Don't activate resizer if in a scrollbar"]].	pt _ event cursorPoint.	self bounds forPoint: pt closestSideDistLen:		[:side :dist :len |  "Check for window side adjust"		dist <= 2  ifTrue: [ptName _ side]].	ptName ifNil:		["Check for pane border adjust"		^ self spawnPaneFrameHandle: event].	#(topLeft bottomRight bottomLeft topRight) do:		[:corner |  "Check for window corner adjust"		(pt dist: (self bounds perform: corner)) < 20 ifTrue: [ptName _ corner]].	resizer _ NewHandleMorph new followHand: event hand		forEachPointDo:			[:p | localPt _ self pointFromWorld: p.			ptName = #top ifTrue: [newBounds _ self bounds withTop: localPt y].			ptName = #bottom ifTrue: [newBounds _ self bounds withBottom: localPt y].			ptName = #left ifTrue: [newBounds _ self bounds withLeft: localPt x].			ptName = #right ifTrue: [newBounds _ self bounds withRight: localPt x].			ptName = #topLeft ifTrue: [newBounds _ localPt corner: bounds bottomRight].			ptName = #bottomRight ifTrue: [newBounds _ bounds topLeft corner: localPt].			ptName = #bottomLeft ifTrue: [newBounds _ (bounds withBottom: localPt y) withLeft: localPt x].			ptName = #topRight ifTrue: [newBounds _ (bounds withTop: localPt y) withRight: localPt x].			newBounds extent > (60@40) ifTrue: [self bounds: newBounds]]		lastPointDo: [:lastPoint | ].	event hand world addMorph: resizer.	resizer startStepping! !!TextMorphForEditView methodsFor: 'all' stamp: 'di 11/10/1998 10:13'!flash	^ editView flash! !!Utilities class methodsFor: 'miscellaneous' stamp: 'di 11/10/1998 10:24'!emergencyCollapse	World ifNotNil: [^ self].	ScheduledControllers screenController emergencyCollapse! !!WorldMorph methodsFor: 'sensing' stamp: 'di 11/10/1998 11:41'!patchAt: patchRect without: stopMorph andNothingAbove: stopThere	"Return a complete rendering of this patch of the display screen	without stopMorph, and possibly without anything above it."	| c |	c _ FormCanvas extent: patchRect extent depth: Display depth.	c _ c copyOrigin: patchRect topLeft negated clipRect: (0@0 extent: patchRect extent).	(self bounds containsRect: patchRect) ifFalse:		["Need to fill area outside bounds with black."		c form fillColor: Color black].	(self bounds intersects: patchRect) ifFalse:		["Nothing within bounds to show."		^ c form].	c fillColor: color.  "Fill bounds with world color."	submorphs reverseDo:		[:m | m == stopMorph				ifTrue: [stopThere ifTrue: [^ c form]]				ifFalse: [(m fullDrawOn: c without: stopMorph andStopThere: stopThere)							ifTrue: [^ c form]]].  	hands reverseDo: [:h |		h submorphsReverseDo:		[:m | m == stopMorph			ifTrue: [stopThere ifTrue: [^ c form]]			ifFalse: [(m fullDrawOn: c without: stopMorph andStopThere: stopThere)						ifTrue: [^ c form]]]].  	^ c form! !