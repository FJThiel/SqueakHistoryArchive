'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #266] on 25 August 2004 at 4:17:54 pm'!"Change Set:		fixes-mirDate:			25 August 2004Author:			Michael RuegerFixes a problem with enabling to many flaps after switching locale and a release related problem with Object dependents."!!Project methodsFor: 'language' stamp: 'mir 8/25/2004 16:16'!updateLocaleDependents	"Set the project's natural language as indicated"	ActiveWorld allTileScriptingElements do: [:viewerOrScriptor |			viewerOrScriptor localeChanged].	Flaps disableGlobalFlaps: false.	Preferences eToyFriendly		ifTrue: [Flaps addAndEnableEToyFlaps]		ifFalse: [Flaps enableGlobalFlaps].	(Project current isFlapIDEnabled: 'Navigator' translated)		ifFalse: [Flaps enableDisableGlobalFlapWithID: 'Navigator' translated].	ParagraphEditor initializeTextEditorMenus.	Utilities emptyScrapsBook.	MenuIcons initializeTranslations.	LanguageEnvironment localeChanged.	#(PartsBin ParagraphEditor BitEditor FormEditor StandardSystemController) 		do: [ :key | Smalltalk at: key ifPresent: [ :class | class initialize ]].	"self setFlaps.	self setPaletteFor: aLanguageSymbol."! !!SystemDictionary methodsFor: 'squeakland' stamp: 'mir 8/25/2004 16:05'!makeSqueaklandReleasePhasePrepare	"Smalltalk makeSqueaklandReleasePhasePrepare"	Undeclared removeUnreferencedKeys.	StandardScriptingSystem initialize.	Preferences initialize.	"(Object classPool at: #DependentsFields) size > 1 ifTrue: [self error:'Still have dependents']."	Undeclared isEmpty ifFalse: [self error:'Please clean out Undeclared'].	"Dump all projects"	Project allSubInstancesDo:[:prj| prj == Project current ifFalse:[Project deletingProject: prj]].	"Set new look so we don't need older fonts later"	StandardScriptingSystem applyNewEToyLook.	Browser initialize.	ScriptingSystem deletePrivateGraphics.	ChangeSorter removeChangeSetsNamedSuchThat:		[:cs| cs name ~= ChangeSet current name].	ChangeSet current clear.	ChangeSet current name: 'Unnamed1'.	Smalltalk garbageCollect.	"Reinitialize DataStream; it may hold on to some zapped entitities"	DataStream initialize.	"Remove existing player references"	References keys do:[:k| References removeKey: k].	Smalltalk garbageCollect.	ScheduledControllers _ nil.	Smalltalk garbageCollect.! !!MultiByteFileStream methodsFor: 'private' stamp: 'mir 8/25/2004 17:27'!setConverterForCode	| current |	(SourceFiles at: 2)		ifNotNil: [self fullName = (SourceFiles at: 2) fullName ifTrue: [^ self]].	current _ self converter saveStateOf: self.	self position: 0.	self binary.	((self next: 3) = (ByteArray with: 16rEF with: 16rBB with: 16rBF)) ifTrue: [		self converter: UTF8TextConverter new	] ifFalse: [		self converter: MacRomanTextConverter new.	].	converter restoreStateOf: self with: current.	self text.! !!SystemDictionary methodsFor: 'squeakland' stamp: 'mir 8/25/2004 17:38'!makeSqueaklandReleasePhaseFinalSettings	"Smalltalk makeSqueaklandReleasePhaseFinalSettings"	| serverName serverURL serverDir updateServer highestUpdate newVersion |	ProjectLauncher splashMorph: (FileDirectory default readOnlyFileNamed: 'scripts\SqueaklandSplash.morph') fileInObjectAndCode.	"Dump all morphs so we don't hold onto anything"	World submorphsDo:[:m| m delete].	#(		(honorDesktopCmdKeys false)		(warnIfNoChangesFile false)		(warnIfNoSourcesFile false)		(showDirectionForSketches true)		(menuColorFromWorld false)		(unlimitedPaintArea true)		(useGlobalFlaps false)		(mvcProjectsAllowed false)		(projectViewsInWindows false)		(automaticKeyGeneration true)		(securityChecksEnabled true)		(showSecurityStatus false)		(startInUntrustedDirectory true)		(warnAboutInsecureContent false)		(promptForUpdateServer false)		(fastDragWindowForMorphic false)		(externalServerDefsOnly true)		(expandedFormat false)		(allowCelesteTell false)		(eToyFriendly true)		(eToyLoginEnabled true)		(magicHalos true)		(mouseOverHalos true)		(biggerHandles false)		(includeSoundControlInNavigator true)		(readDocumentAtStartup true)		(preserveTrash true)	) do:[:spec|		Preferences setPreference: spec first toValue: spec last].	"Workaround for bug"	Preferences enable: #readDocumentAtStartup.	World color: (Color r: 0.9 g: 0.9 b: 1.0).	"Clear all server entries"	ServerDirectory serverNames do: [:each | ServerDirectory removeServerNamed: each].	SystemVersion current resetHighestUpdate.	"Add the squeakalpha update stream"	serverName _ 'Squeakalpha'.	serverURL _ 'squeakalpha.org'.	serverDir _ serverURL , '/'.	updateServer _ ServerDirectory new.	updateServer		server: serverURL;		directory: 'updates/';		altUrl: serverDir;		user: 'sqland';		password: nil.	Utilities updateUrlLists addFirst: {serverName. {serverDir. }.}.	"Add the squeakland update stream"	serverName _ 'Squeakland'.	serverURL _ 'squeakland.org'.	serverDir _ serverURL , '/'.	updateServer _ ServerDirectory new.	updateServer		server: serverURL;		directory: 'public_html/updates/';		altUrl: serverDir.	Utilities updateUrlLists addFirst: {serverName. {serverDir. }.}.	highestUpdate _ SystemVersion current highestUpdate.	(self confirm: 'Reset highest update (' , highestUpdate printString , ')?')		ifTrue: [SystemVersion current highestUpdate: 0].	newVersion _ FillInTheBlank request: 'New version designation:' initialAnswer: 'Squeakland 3.8.' , highestUpdate printString. 	SystemVersion newVersion: newVersion.	(self confirm: self version , 'Is this the correct version designation?If not, choose no, and fix it.') ifFalse: [^ self].! !