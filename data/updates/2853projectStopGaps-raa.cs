'From Squeak2.9alpha of 17 July 2000 [latest update: #2900] on 26 October 2000 at 2:18:22 pm'!"Change Set:		projectStopGapsDate:			26 October 2000Author:			Bob ArningHandle the problem of trying to enter a project whose world was turned into a StringMorph by the project save logic. Attempt to load a complete version of the project to replace this stub."!!Project methodsFor: 'menu messages' stamp: 'RAA 10/26/2000 14:16'!enter: returningFlag revert: revertFlag saveForRevert: saveForRevert	"Install my ChangeSet, Transcript, and scheduled views as current globals. If returningFlag is true, we will return to the project from whence the current project was entered; don't change its previousProject link in this case.	If saveForRevert is true, save the ImageSegment of the project being left.	If revertFlag is true, make stubs for the world of the project being left.	If revertWithoutAsking is true in the project being left, then always revert."	| showZoom recorderOrNil old forceRevert response seg newProcess |	(world isKindOf: StringMorph) ifTrue: [		self inform: 'This project is not all here. I will try to load a complete version.'.		^self loadFromServer: true	"try to get a fresh copy"	].	self isCurrentProject ifTrue: [^ self].	"Check the guards"	guards ifNotNil:		[guards _ guards reject: [:obj | obj isNil].		guards do: [:obj | obj okayToEnterProject ifFalse: [^ self]]].	forceRevert _ false.	CurrentProject rawParameters 		ifNil: [revertFlag ifTrue: [^ self inform: 'nothing to revert to']]		ifNotNil: [saveForRevert ifFalse: [				forceRevert _ CurrentProject projectParameters 								at: #revertWithoutAsking ifAbsent: [false]]].	forceRevert not & revertFlag ifTrue: [		response _ SelectionMenu confirm: 'Are you sure you want to destroy this Project\ and revert to an older version?\\(From the parent project, click on this project''s thumbnail.)' withCRs			trueChoice: 'Revert to saved version' 			falseChoice: 'Cancel'.		response ifFalse: [^ self]].	revertFlag | forceRevert 		ifTrue: [seg _ CurrentProject projectParameters at: #revertToMe ifAbsent: [					^ self inform: 'nothing to revert to']]		ifFalse: [			CurrentProject makeThumbnail.			returningFlag == #specialReturn ifTrue: [				ProjectHistory forget: CurrentProject.		"this guy is irrelevant"			] ifFalse: [				ProjectHistory remember: CurrentProject.			].		].	(revertFlag | saveForRevert | forceRevert) ifFalse: [		(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [			self storeToMakeRoom]].	Smalltalk isMorphic ifTrue: [Display bestGuessOfCurrentWorld triggerClosingScripts].	"Update the display depth and make a thumbnail of the current project"	CurrentProject displayDepth: Display depth.	old _ CurrentProject.		"for later"	"Show the project transition.	Note: The project zoom is run in the context of the old project,		so that eventual errors can be handled accordingly"	displayDepth == nil ifTrue: [displayDepth _ Display depth].	self installNewDisplay: Display extent depth: displayDepth.	(showZoom _ self showZoom) ifTrue: [		self displayZoom: CurrentProject parent ~~ self].	(world isMorph and: [world hasProperty: #letTheMusicPlay])		ifTrue: [world removeProperty: #letTheMusicPlay]		ifFalse: [Smalltalk at: #ScorePlayer ifPresent: [:playerClass | 					playerClass allSubInstancesDo: [:player | player pause]]].	returningFlag == #specialReturn ifTrue: [		old removeChangeSetIfPossible.	"keep this stuff from accumulating"		nextProject _ nil	] ifFalse: [		returningFlag			ifTrue: [nextProject _ CurrentProject]			ifFalse: [previousProject _ CurrentProject].	].	CurrentProject saveState.	CurrentProject isolationHead == self isolationHead ifFalse:		[self invokeFrom: CurrentProject].	CurrentProject _ self.	Smalltalk newChanges: changeSet.	TranscriptStream newTranscript: transcript.	Sensor flushKeyboard.	Smalltalk isMorphic ifTrue: [recorderOrNil _ Display pauseMorphicEventRecorder].	ProjectHistory remember: CurrentProject.	world isMorph		ifTrue:			[Display changeMorphicWorldTo: world.  "Signifies Morphic"			world install.			"(revertFlag | saveForRevert | forceRevert) ifFalse: [				(Preferences valueOfFlag: #projectsSentToDisk) ifTrue: [					self storeSomeSegment]]."			recorderOrNil ifNotNil: [recorderOrNil resumeIn: world].			world triggerOpeningScripts]		ifFalse:			[Display changeMorphicWorldTo: nil.  "Signifies MVC"			Smalltalk at: #ScheduledControllers put: world].	saveForRevert ifTrue: [		Smalltalk garbageCollect.	"let go of pointers"		old storeSegment.		"result _" old world isInMemory 			ifTrue: ['Can''t seem to write the project.']			ifFalse: [old projectParameters at: #revertToMe put: 					old world xxxSegment clone].				'Project written.'].			"original is for coming back in and continuing."	revertFlag | forceRevert ifTrue: [		seg clone revert].	"non-cloned one is for reverting again later"	self removeParameter: #exportState.	"Complete the enter: by launching a new process"	world isMorph ifTrue: [		self finalEnterActions.		world repairEmbeddedWorlds.		Project spawnNewProcessAndTerminateOld: true	] ifFalse: [		SystemWindow clearTopWindow.	"break external ref to this project"		newProcess _ [				ScheduledControllers resetActiveController.	"in case of walkback in #restore"			showZoom ifFalse: [ScheduledControllers restore].			ScheduledControllers searchForActiveController		] fixTemps newProcess priority: Processor userSchedulingPriority.		newProcess resume.		"lose the current process and its referenced morphs"		Processor terminateActive.	]! !!Project methodsFor: 'file in/out' stamp: 'RAA 10/26/2000 14:12'!ensureChangeSetNameUnique	| myName |	myName _ self name.	Project allProjects do: [:pp | 		pp == self ifFalse: [			(pp name = myName and: [pp projectChangeSet ~~ changeSet]) ifTrue: [				(pp parameterAt: #loadingNewerVersion ifAbsent: [false]) ifTrue: [					pp projectParameters at: #loadingNewerVersion put: false.				] ifFalse: [					changeSet ifNil: [^ changeSet _ ChangeSet new].					^changeSet name: (ChangeSet uniqueNameLike: myName)				].			]		]	]! !!Project methodsFor: 'file in/out' stamp: 'RAA 10/26/2000 14:15'!loadFromServer: newerAutomatically	"If a newer version of me is on the server, load it."	| servers pair resp |	self assureIntegerVersion.	self isCurrentProject ifTrue: ["exit, then do the command"		^ self armsLengthCommand: #loadFromServer withDescription: 'Loading'	].	servers _ self tryToFindAServerWithMe ifNil: [^ nil].	pair _ self class mostRecent: self name onServer: servers first.	pair first ifNil: [^ self inform: 'can''t find file on server for ', self name].	self currentVersionNumber > pair second ifTrue: [		^ self inform: 'That server has an older version of the project.'].	version = (Project parseProjectFileName: pair first) second ifTrue: [		resp _ (PopUpMenu labels: 'Reload anyway\Cancel' withCRs) startUpWithCaption: 					'The only changes are the ones you made here.'.		resp ~= 1 ifTrue: [^ nil]	] ifFalse: [		newerAutomatically ifFalse: [			resp _ (PopUpMenu labels: 'Load it\Cancel' withCRs) startUpWithCaption: 						'A newer version exists on the server.'.			resp ~= 1 ifTrue: [^ nil]		].	].	"let's avoid renaming the loaded change set since it will be replacing ours"	self projectParameters at: #loadingNewerVersion put: true.	ComplexProgressIndicator new 		targetMorph: nil;		historyCategory: 'project loading';		withProgressDo: [			ProjectLoading				installRemoteNamed: pair first				from: servers first				named: self name				in: parentProject		]! !!Project methodsFor: 'file in/out' stamp: 'RAA 10/26/2000 13:47'!tryToFindAServerWithMe	| servers resp primaryServerDirectory |	urlList isEmptyOrNil ifTrue: [urlList _ parentProject urlList copy].	[(servers _ self serverList) isNil] whileTrue: [		resp _ (PopUpMenu labels: 'Try to find a server\Cancel' withCRs) startUpWithCaption: 					'This project thinks it has never been on a server'.		resp ~= 1 ifTrue: [^ nil].		(primaryServerDirectory _ self findAFolderToLoadProjectFrom) ifNil: [^nil].		self storeNewPrimaryURL: primaryServerDirectory realUrl, '/'.	].	^servers! !!Project class methodsFor: 'utilities' stamp: 'RAA 10/26/2000 11:58'!interruptName: labelString	"Create a Notifier on the active scheduling process with the given label."	| suspendingList projectProcess |	Smalltalk isMorphic ifFalse:		[^ ScheduledControllers interruptName: labelString].	World primaryHand ifNotNil: [World primaryHand releaseAllFoci].	projectProcess _ self uiProcess.	"we still need the accessor for a while"	(suspendingList _ projectProcess suspendingList) == nil		ifTrue: [projectProcess == Processor activeProcess					ifTrue: [projectProcess suspend]]		ifFalse: [suspendingList remove: projectProcess.				projectProcess offList].	Debugger openInterrupt: labelString onProcess: projectProcess! !