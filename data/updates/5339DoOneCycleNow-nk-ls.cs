'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5325] on 4 July 2003 at 4:56:16 pm'!"Change Set:		DoOneCycleNowFixAndComment-nk-lsDate:			4 July 2003Author:			Ned Konzv2: adds Lex Spoon's comments (from his May 6 change set called doOneCycleComm-ls) to doOneCycleNow and friends.This changes some senders of doOneCycleNow in FileList2 to use doOneCycle.You can see the problem caused by doOneCycleNow when using a laptop, since the mouse will start jerking around the screen."!!FileList2 class methodsFor: 'modal dialogs' stamp: 'nk 5/8/2003 19:02'!modalFileSelector	| window |	window _ self morphicViewFileSelector.	window openCenteredInWorld.	[window world notNil] whileTrue: [		window outermostWorldMorph doOneCycle.	].	^(window valueOfProperty: #fileListModel) getSelectedFile! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'nk 5/8/2003 19:02'!modalFileSelectorForSuffixes: aList	| window aFileList |	window _ self morphicViewFileSelectorForSuffixes: aList.	aFileList _ window valueOfProperty: #fileListModel.	window openCenteredInWorld.	[window world notNil] whileTrue: [		window outermostWorldMorph doOneCycle.	].	^aFileList getSelectedFile! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'nk 5/8/2003 19:02'!modalFolderSelector: aDir	| window fileModel |	window _ self morphicViewFolderSelector: aDir.	fileModel _ window model.	window openInWorld: self currentWorld extent: 300@400.	[window world notNil] whileTrue: [		window outermostWorldMorph doOneCycle.	].	^fileModel getSelectedDirectory withoutListWrapper! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'nk 5/8/2003 19:02'!modalFolderSelectorForProject: aProject"FileList2 modalFolderSelectorForProject: Project current"	| window fileModel w |	window _ FileList2 morphicViewProjectSaverFor: aProject.	fileModel _ window valueOfProperty: #FileList.	w _ self currentWorld.	window position: w topLeft + (w extent - window extent // 2).	w addMorphInLayer: window.	w startSteppingSubmorphsOf: window.	[window world notNil] whileTrue: [		window outermostWorldMorph doOneCycle.	].	^fileModel getSelectedDirectory withoutListWrapper! !!FileList2 class methodsFor: 'modal dialogs' stamp: 'nk 5/8/2003 19:02'!modalFolderSelectorForProjectLoad	| window fileModel w |	window _ self morphicViewProjectLoader2InWorld: self currentWorld reallyLoad: false.	fileModel _ window valueOfProperty: #FileList.	w _ self currentWorld.	window position: w topLeft + (w extent - window extent // 2).	window openInWorld: w.	[window world notNil] whileTrue: [		window outermostWorldMorph doOneCycle.	].	^fileModel getSelectedDirectory withoutListWrapper! !!PasteUpMorph methodsFor: 'interaction loop' stamp: 'ls 5/6/2003 16:51'!doOneCycleNow	"see the comment in doOneCycleNowFor:"	worldState doOneCycleNowFor: self.! !!PasteUpMorph methodsFor: 'world state' stamp: 'ls 5/6/2003 16:51'!doOneCycle	"see the comment in doOneCycleFor:"	worldState doOneCycleFor: self! !!WorldState methodsFor: 'update cycle' stamp: 'ls 5/6/2003 16:51'!doOneCycleFor: aWorld	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.This is a moderately private method; a better alternative is usually either to wait for events or to check the state of things from #step methods."	self interCyclePause: (Preferences higherPerformance ifTrue: [1] ifFalse: [MinCycleLapse]).	self doOneCycleNowFor: aWorld.! !!WorldState methodsFor: 'update cycle' stamp: 'ls 5/6/2003 16:48'!doOneCycleNowFor: aWorld	"Immediately do one cycle of the interaction loop.  This should not be called directly, but only via doOneCycleFor:"	| recognizing |	recognizing _ false.	self flag: #bob.		"need to consider remote hands in lower worlds"	"process user input events"	LastCycleTime _ Time millisecondClockValue.	self handsDo: [:h |		ActiveHand _ h.		h processEvents.		h isGenieRecognizing ifTrue: 			[recognizing _ h giveGenieChanceToEscape not].		ActiveHand _ nil	].	"the default is the primary hand"	ActiveHand _ self hands first.	"The gesture recognizer needs enough points to be accurate.	Therefore morph stepping is disabled while capturing points for the recognizer"	recognizing ifFalse: 		[aWorld runStepMethods.		"there are currently some variations here"		self displayWorldSafely: aWorld].! !