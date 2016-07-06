'From Squeak3.1alpha of 5 February 2001 [latest update: #3521] on 6 February 2001 at 2:33:56 pm'!"Change Set:		nav30Date:			4 February 2001Author:			Bob ArningMore changes for the InternalThreadNavigationMorph:- published copies will carry with them the thumnails required for proper display before the (next) project is loaded- when editing a thread, the original navigator disappears. It will reappear when you hit 'Okay' on the sorter.- the sorter now has a new project button (thumbnail of the current project)- the sorter is better behaved WRT width during sorting- new project requests now include a prompt for project nameHopefully fixed another (the last?? hope, hope) buglet in InfiniteFormChanged the text fields in the project query/details/renamer to show empty fields just like fields with dataRetract the navigator when leaving a project so it looks neaterWhen navigating to a new project from the Project sorter, open a thread navigator in the new project"!BookPageSorterMorph subclass: #ProjectSorterMorph	instanceVariableNames: 'sizeOfEachMorph '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Navigators'!TextMorph subclass: #ShowEmptyTextMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Experimental'!!ShowEmptyTextMorph commentStamp: 'RAA 2/6/2001 14:11' prior: 0!A slight modification on TextMorph to show empty fields just as one would fields with data: with a cursor and without the pink field!!EToyGenericDialogMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/6/2001 14:10'!genericTextFieldNamed: aString	| newField |	newField _ ShowEmptyTextMorph new		beAllFont: self myFont;		extent: 300@20;		contentsWrapped: ''.	namedFields at: aString put: newField.	^newField! !!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'RAA 2/6/2001 14:00'!infiniteFillRectangle: aRectangle fillStyle: aFillStyle	| additionalOffset rInPortTerms clippedPort targetTopLeft clipOffset ex |	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.	something better is needed, but not now"	additionalOffset _ 0@0.	ex _ aFillStyle form extent.	rInPortTerms _ aRectangle translateBy: origin.	clippedPort _ port clippedBy: rInPortTerms.	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.	clipOffset _ rInPortTerms topLeft - targetTopLeft.	additionalOffset _ (clipOffset \\ ex) - ex.	^aFillStyle		displayOnPort: clippedPort		offsetBy: additionalOffset! !!InfiniteForm methodsFor: 'displaying' stamp: 'RAA 2/6/2001 14:02'!displayOnPort: aPort at: offset	| targetBox patternBox savedMap top left |	self flag: #bob.	"this *may* not get called at the moment. I have been trying to figure out the right way for this to work and am using #displayOnPort:offsetBy: as my current offering - Bob"	(patternForm isKindOf: Form) ifFalse: [		"patternForm is a Pattern or Color; just use it as a mask for BitBlt"		^ aPort fill: aPort clipRect fillColor: patternForm rule: Form over].	"do it iteratively"	targetBox _ aPort clipRect.	patternBox _ patternForm boundingBox.	savedMap _ aPort colorMap.	aPort sourceForm: patternForm;		fillColor: nil;		combinationRule: Form paint;		sourceRect: (0@0 extent: patternBox extent);		colorMap: (patternForm colormapIfNeededForDepth: aPort destForm depth).	top _ (targetBox top truncateTo: patternBox height) "- (offset y \\ patternBox height)".	left _  (targetBox left truncateTo: patternBox width) "- (offset x \\ patternBox width)".	left to: (targetBox right - 1) by: patternBox width do:		[:x | top to: (targetBox bottom - 1) by: patternBox height do:			[:y | aPort destOrigin: x@y; copyBits]].	aPort colorMap: savedMap.! !!InfiniteForm methodsFor: 'displaying' stamp: 'RAA 2/6/2001 14:03'!displayOnPort: aPort offsetBy: offset	| targetBox patternBox savedMap top left |	"this version tries to get the form aligned where the user wants it and not just aligned with the cliprect"	(patternForm isKindOf: Form) ifFalse: [		"patternForm is a Pattern or Color; just use it as a mask for BitBlt"		^ aPort fill: aPort clipRect fillColor: patternForm rule: Form over].	"do it iteratively"	targetBox _ aPort clipRect.	patternBox _ patternForm boundingBox.	savedMap _ aPort colorMap.	aPort sourceForm: patternForm;		fillColor: nil;		combinationRule: Form paint;		sourceRect: (0@0 extent: patternBox extent);		colorMap: (patternForm colormapIfNeededForDepth: aPort destForm depth).	top _ (targetBox top truncateTo: patternBox height) + offset y.	left _  (targetBox left truncateTo: patternBox width) + offset x.	left to: (targetBox right - 1) by: patternBox width do:		[:x | top to: (targetBox bottom - 1) by: patternBox height do:			[:y | aPort destOrigin: x@y; copyBits]].	aPort colorMap: savedMap.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 16:12'!acceptSortedContentsFrom: aHolder	"Update my page list from the given page sorter."	| nameOfThisProject cachedData proj |	threadName isEmpty ifTrue: [threadName _ 'I need a name'].	threadName _ FillInTheBlank 		request: 'Name this thread.' 		initialAnswer: threadName.	threadName isEmptyOrNil ifTrue: [^self].	listOfPages _ OrderedCollection new.	aHolder submorphs doWithIndex: [:m :i |		(nameOfThisProject _ m valueOfProperty: #nameOfThisProject) ifNotNil: [			cachedData _ {nameOfThisProject}.			proj _ Project named: nameOfThisProject.			(proj isNil or: [proj thumbnail isNil]) ifFalse: [				cachedData _ cachedData, {proj thumbnail scaledToSize: self myThumbnailSize}.			].			listOfPages add: cachedData.		].	].	self class know: listOfPages as: threadName.	self removeAllMorphs; addButtons.	self world ifNil: [		self openInWorld; positionAppropriately.	].! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 16:08'!addButtons	| marginPt i sz data images pageNumber f m b1 b2 dot arrowWidth arrowCenter vertices arrowHeight nameMorph |	self changeNoLayout.	self hResizing: #rigid.	self vResizing: #rigid.	marginPt _ 4@4.	i _ self currentIndex.	sz _ self myThumbnailSize.	arrowWidth _ 14.	arrowHeight _ 17.	data _ {		{i - 1. 'Previous:'. #previousPage. #rightCenter. arrowWidth negated. 'Prev'}.		{i + 1. 'Next:'. #nextPage. #leftCenter. arrowWidth. 'Next'}	}.	images _ data collect: [ :tuple |		pageNumber _ tuple first.		(pageNumber between: 1 and: listOfPages size) ifTrue: [			f _ self 				makeThumbnailForPageNumber: pageNumber 				scaledToSize: sz 				default: tuple sixth.			f _ f deepCopy.		"we're going to mess it up"			arrowCenter _ f boundingBox perform: tuple fourth.			vertices _ {				arrowCenter - (0@arrowHeight).				arrowCenter + (0@arrowHeight).				arrowCenter + (tuple fifth @ 0).			}.			f getCanvas				drawPolygon: vertices 				color: Color orange 				borderWidth: 0 				borderColor: Color transparent.			m _ ImageMorph new image: f.			m setBalloonText: tuple second,' ',(listOfPages at: pageNumber) first.			m addMouseUpActionWith: (				MessageSend receiver: self selector: tuple third			).		] ifFalse: [			f _ (Form extent: sz depth: 16) fillColor: Color lightGray.			m _ ImageMorph new image: f.		].		m	].	b1 _ images first.	b2 _ images second.	dot _ EllipseMorph new extent: 16@16; color: Color orange lighter; borderWidth: 0.	self addMorph: (b1 position: self position + marginPt).	self addMorph: (b2 position: b1 topRight + (marginPt x @ 0)).	self extent: (b1 bottomRight max: b2 bottomRight) - self position + marginPt.	self addMorph: dot.	dot align: dot center with: b1 bounds rightCenter + ((marginPt x @ 0) // 2).	dot setBalloonText: threadName,'more commands'.	dot on: #mouseDown send: #moreCommands to: self.	self fullBounds.	self addMorph: (nameMorph _ SquishedNameMorph new).	nameMorph		target: self getSelector: #threadName setSelector: nil;		color: Color transparent;		width: self width;		height: 15;		align: nameMorph bottomLeft with: self bottomLeft.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 11:41'!editThisThread	| sorter |	sorter _ ProjectSorterMorph new.	sorter navigator: self listOfPages: listOfPages.	self currentWorld addMorphFront: sorter.	sorter align: sorter center with: self currentWorld center.	self delete.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 09:57'!insertNewProject	| newProj me |	[newProj _ Project newMorphicOn: nil.]		on: ProjectViewOpenNotification		do: [ :ex | ex resume: false].		EToyProjectDetailsMorph 		getFullInfoFor: newProj		ifValid: [			me _ CurrentProjectRefactoring currentProjectName.			listOfPages withIndexDo: [ :each :index |				each first = me ifTrue: [					listOfPages add: {newProj name} afterIndex: index.					^self switchToThread: threadName				].			].			listOfPages add: {newProj name} afterIndex: listOfPages size.			^self switchToThread: threadName		]		expandedFormat: false.! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 09:38'!makeThumbnailForPageNumber: pageNumber scaledToSize: sz default: aString	| cachedData proj tn label |	cachedData _ listOfPages at: pageNumber.	proj _ Project named: cachedData first.	(proj isNil or: [proj thumbnail isNil]) ifTrue: [		cachedData size >= 2 ifTrue: [^cachedData second].		tn _ Form extent: sz depth: 8.		tn fillColor: Color veryLightGray.		label _ (StringMorph contents: aString) imageForm.		label displayOn: tn at: tn center - (label extent // 2) rule: Form paint.		^tn	].	tn _ proj thumbnail  scaledToSize: sz.	cachedData size < 2 ifTrue: [		cachedData _ cachedData,#(0).		listOfPages at: pageNumber put: cachedData.	].	cachedData at: 2 put: tn.	^tn! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 16:16'!myThumbnailSize	^52@39! !!Project methodsFor: 'menu messages' stamp: 'RAA 2/6/2001 14:18'!enter: returningFlag revert: revertFlag saveForRevert: saveForRevert	"Install my ChangeSet, Transcript, and scheduled views as current globals. If returningFlag is true, we will return to the project from whence the current project was entered; don't change its previousProject link in this case.	If saveForRevert is true, save the ImageSegment of the project being left.	If revertFlag is true, make stubs for the world of the project being left.	If revertWithoutAsking is true in the project being left, then always revert."	| showZoom recorderOrNil old forceRevert response seg newProcess |	(world isKindOf: StringMorph) ifTrue: [		self inform: 'This project is not all here. I will try to load a complete version.'.		^self loadFromServer: true	"try to get a fresh copy"	].	self isCurrentProject ifTrue: [^ self].	"Check the guards"	guards ifNotNil:		[guards _ guards reject: [:obj | obj isNil].		guards do: [:obj | obj okayToEnterProject ifFalse: [^ self]]].	forceRevert _ false.	CurrentProject rawParameters 		ifNil: [revertFlag ifTrue: [^ self inform: 'nothing to revert to']]		ifNotNil: [saveForRevert ifFalse: [				forceRevert _ CurrentProject projectParameters 								at: #revertWithoutAsking ifAbsent: [false]]].	forceRevert not & revertFlag ifTrue: [		response _ SelectionMenu confirm: 'Are you sure you want to destroy this Project\ and revert to an older version?\\(From the parent project, click on this project''s thumbnail.)' withCRs			trueChoice: 'Revert to saved version' 			falseChoice: 'Cancel'.		response ifFalse: [^ self]].	revertFlag | forceRevert 		ifTrue: [seg _ CurrentProject projectParameters at: #revertToMe ifAbsent: [					^ self inform: 'nothing to revert to']]		ifFalse: [			CurrentProject finalExitActions.			CurrentProject makeThumbnail.			returningFlag == #specialReturn ifTrue: [				ProjectHistory forget: CurrentProject.		"this guy is irrelevant"				Project forget: CurrentProject.			] ifFalse: [				ProjectHistory remember: CurrentProject.			].		].	(revertFlag | saveForRevert | forceRevert) ifFalse: [		(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [			self storeToMakeRoom]].	Smalltalk isMorphic ifTrue: [Display bestGuessOfCurrentWorld triggerClosingScripts].	"Update the display depth and make a thumbnail of the current project"	CurrentProject displayDepth: Display depth.	old _ CurrentProject.		"for later"	"Show the project transition.	Note: The project zoom is run in the context of the old project,		so that eventual errors can be handled accordingly"	displayDepth == nil ifTrue: [displayDepth _ Display depth].	self installNewDisplay: Display extent depth: displayDepth.	(showZoom _ self showZoom) ifTrue: [		self displayZoom: CurrentProject parent ~~ self].	(world isMorph and: [world hasProperty: #letTheMusicPlay])		ifTrue: [world removeProperty: #letTheMusicPlay]		ifFalse: [Smalltalk at: #ScorePlayer ifPresent: [:playerClass | 					playerClass allSubInstancesDo: [:player | player pause]]].	returningFlag == #specialReturn ifTrue: [		old removeChangeSetIfPossible.	"keep this stuff from accumulating"		nextProject _ nil	] ifFalse: [		returningFlag			ifTrue: [nextProject _ CurrentProject]			ifFalse: [previousProject _ CurrentProject].	].	CurrentProject saveState.	CurrentProject isolationHead == self isolationHead ifFalse:		[self invokeFrom: CurrentProject].	CurrentProject _ self.	Smalltalk newChanges: changeSet.	TranscriptStream newTranscript: transcript.	Sensor flushKeyboard.	Smalltalk isMorphic ifTrue: [recorderOrNil _ Display pauseMorphicEventRecorder].	ProjectHistory remember: CurrentProject.	world isMorph		ifTrue:			[Display changeMorphicWorldTo: world.  "Signifies Morphic"			world install.			world transferRemoteServerFrom: old world.			"(revertFlag | saveForRevert | forceRevert) ifFalse: [				(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [					self storeSomeSegment]]."			recorderOrNil ifNotNil: [recorderOrNil resumeIn: world].			world triggerOpeningScripts]		ifFalse:			[Display changeMorphicWorldTo: nil.  "Signifies MVC"			Smalltalk at: #ScheduledControllers put: world].	saveForRevert ifTrue: [		Smalltalk garbageCollect.	"let go of pointers"		old storeSegment.		"result _" old world isInMemory 			ifTrue: ['Can''t seem to write the project.']			ifFalse: [old projectParameters at: #revertToMe put: 					old world xxxSegment clone].				'Project written.'].			"original is for coming back in and continuing."	revertFlag | forceRevert ifTrue: [		seg clone revert].	"non-cloned one is for reverting again later"	self removeParameter: #exportState.	"Complete the enter: by launching a new process"	world isMorph ifTrue: [		self finalEnterActions.		world repairEmbeddedWorlds.		Project spawnNewProcessAndTerminateOld: true	] ifFalse: [		SystemWindow clearTopWindow.	"break external ref to this project"		newProcess _ [				ScheduledControllers resetActiveController.	"in case of walkback in #restore"			showZoom ifFalse: [ScheduledControllers restore].			ScheduledControllers searchForActiveController		] fixTemps newProcess priority: Processor userSchedulingPriority.		newProcess resume.		"lose the current process and its referenced morphs"		Processor terminateActive.	]! !!Project methodsFor: 'menu messages' stamp: 'RAA 2/6/2001 14:21'!finalExitActions	| navigator |	world isMorph ifTrue: [		navigator _ world findA: ProjectNavigationMorph.		navigator ifNotNil: [navigator retractIfAppropriate].	].! !!ProjectNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/6/2001 14:16'!retractIfAppropriate	mouseInside _ false.	self positionVertically.! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 16:06'!addControls	| b r partsBinButton newButton |	b _ SimpleButtonMorph new 		target: self;		color: self myColor darker;		borderColor: Color black.	newButton _ ImageMorph new image: (World project thumbnail scaledToSize: 24@18).	newButton on: #mouseDown send: #insertNewProject: to: self.	newButton setBalloonText: 'Make a new Project'.	(partsBinButton _ UpdatingThreePhaseButtonMorph checkBox)		target: self;		actionSelector: #togglePartsBinStatus;		arguments: #();		getSelector: #getPartsBinStatus.	r _ AlignmentMorph newRow		color: Color transparent;		borderWidth: 0;		layoutInset: 0;		wrapCentering: #center;		cellPositioning: #topCenter;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		extent: 5@5;		addMorphBack: (self wrapperFor: (b fullCopy label: 'Okay';	actionSelector: #acceptSort));		addMorphBack: (self wrapperFor: (b fullCopy label: 'Cancel';	actionSelector: #delete));		addMorphBack: (self wrapperFor: (newButton));		addTransparentSpacerOfSize: 8 @ 0;		addMorphBack: (self wrapperFor: partsBinButton);		addMorphBack: (self wrapperFor: (StringMorph contents: 'Parts bin') lock).	self addMorphFront: r.! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/6/2001 14:24'!clickFromSorterEvent: evt morph: aMorph	(aMorph bounds containsPoint: evt cursorPoint) ifFalse: [^self].	evt isMouseUp ifFalse: [		evt shiftPressed ifFalse: [^evt hand grabMorph: aMorph].		^self	].	evt shiftPressed ifTrue: [		WorldState addDeferredUIMessage: [			InternalThreadNavigationMorph openThreadNamed: book threadName		].		(Project named: (aMorph valueOfProperty: #nameOfThisProject)) enter.	].! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 12:00'!initialize	super initialize.	self useRoundedCorners.	color _ self myColor.	borderWidth _ 0.	pageHolder 		useRoundedCorners;		borderWidth: 0;		color: (			self 				gridFormOrigin: 0@0 				grid: 16@16 				background: Color white 				line: Color blue muchLighter		)! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 15:55'!insertNewProject: evt	| newProj |	[newProj _ Project newMorphicOn: nil.]		on: ProjectViewOpenNotification		do: [ :ex | ex resume: false].		EToyProjectDetailsMorph 		getFullInfoFor: newProj		ifValid: [			evt hand attachMorph: (self sorterMorphForProjectNamed: newProj name)		]		expandedFormat: false.! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 15:40'!morphsForMyContentsFrom: listOfPages sizedTo: sz	| morphsForPageSorter |	'Assembling thumbnail images...'		displayProgressAt: self cursorPoint		from: 0 to: listOfPages size		during: [:bar |			morphsForPageSorter _ listOfPages withIndexCollect: [ :each :index | 				bar value: index.				self sorterMorphForProjectNamed: each first			].		].	^morphsForPageSorter! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 11:25'!myColor	^Color r: 0.365 g: 0.634 b: 0.729! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 16:20'!navigator: aThreadNavigator listOfPages: listOfPages	| morphsForPageSorter pixelsAvailable pixelsNeeded scale |	"a bit of fudging to try to outguess the layout mechanism and get best possible scale"	pixelsAvailable _ Display extent - 130.	pixelsAvailable _ pixelsAvailable x * pixelsAvailable y.	pixelsNeeded _ 100@75.	pixelsNeeded _ pixelsNeeded x * pixelsNeeded y  * listOfPages size.	scale _ (pixelsAvailable / pixelsNeeded min: 1) sqrt.	sizeOfEachMorph _ (100@75 * scale) rounded.	morphsForPageSorter _ self morphsForMyContentsFrom: listOfPages sizedTo: sizeOfEachMorph.	morphsForPageSorter _ morphsForPageSorter reject: [ :each | each isNil].	self changeExtent: Display extent.	self		book: aThreadNavigator 		morphsToSort: morphsForPageSorter.	pageHolder 		cursor: aThreadNavigator currentIndex;		fullBounds;		hResizing: #rigid..! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/4/2001 15:38'!sorterMorphForProjectNamed: projName	| pvm proj |	(proj _ Project named: projName) ifNil: [^nil].	pvm _ ProjectViewMorph on: proj.	pvm _ pvm imageForm scaledToSize: sizeOfEachMorph.	pvm _ pvm asMorph.	pvm setProperty: #nameOfThisProject toValue: projName.	pvm setBalloonText: projName.	pvm on: #mouseDown send: #clickFromSorterEvent:morph: to: self.	pvm on: #mouseUp send: #clickFromSorterEvent:morph: to: self.	^pvm! !!ShowEmptyTextMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/6/2001 14:09'!drawOn: aCanvas	self setDefaultContentsIfNil.	aCanvas paragraph: self paragraph bounds: bounds color: color.! !InternalThreadNavigationMorph removeSelector: #clickFromSorterEvent:morph:!InternalThreadNavigationMorph removeSelector: #makeThumbnailFor:!InternalThreadNavigationMorph removeSelector: #morphsForMyContentsSizedTo:!