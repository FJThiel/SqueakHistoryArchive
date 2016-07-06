'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 5 March 2004 at 7:28 pm'!"Change Set:		q08-securityEtc-mir
Date:			10 November 2003
Author:			Michael Rueger

Derived from Squeakland updates 
0183localSecurity.cs, 0184reportAndFix.cs, and 0187loadFix.cs

Integrated with Squeak 3.7a by Scott Wallace, 4 March 04; there were several collisions that needed fixing up.

This update consists of three parts:
- hides the progress morph until the publishing actually starts (after the user selected a directory)
- changes the project loading logic so sandboxing can independently be en/disabled for local and remote projects (default is local-off, remote-on)
- add a storeLog button to the notifier to allow users to explicitely write a log after an interrupt.

and from the reportAndFix update of Squeakland...

Changes the error report button, applies the timestamping to automatic debug logs as well (controlled by a Preference debugLogTimestamp), adds a workaround for safari messing up underscores.

Fixes a problem with loading a project containing projects."!!ComplexProgressIndicator methodsFor: 'as yet unclassified' stamp: 'mir 1/5/2004 11:45'!withProgressDo: aBlock

	| safetyFactor totals trialRect delta stageCompletedString targetOwner |

	Smalltalk isMorphic ifFalse: [^aBlock value].
	formerProject _ Project current.
	formerWorld _ World.
	formerProcess _ Processor activeProcess.
	targetMorph ifNil: [
		targetMorph _ ProgressTargetRequestNotification signal.
		targetOwner := targetMorph owner].
	targetMorph ifNil: [
		trialRect _ Rectangle center: Sensor cursorPoint extent: 80@80.
		delta _ trialRect amountToTranslateWithin: formerWorld bounds.
		trialRect _ trialRect translateBy: delta.
		translucentMorph _ TranslucentProgessMorph new
			opaqueBackgroundColor: Color white;
			bounds: trialRect;
			openInWorld: formerWorld.
	] ifNotNil: [
		translucentMorph _ TranslucentProgessMorph new
			setProperty: #morphicLayerNumber toValue: targetMorph morphicLayerNumber - 0.1;
			bounds: targetMorph boundsInWorld;
			openInWorld: targetMorph world.
	].
	stageCompleted _ 0.
	safetyFactor _ 1.1.	"better to guess high than low"
	translucentMorph setProperty: #progressStageNumber toValue: 1.
	translucentMorph hide.
	targetOwner ifNotNil: [targetOwner hide].
	totals _ self loadingHistoryDataForKey: 'total'.
	newRatio _ 1.0.
	estimate _ totals size < 2 ifTrue: [
		15000		"be a pessimist"
	] ifFalse: [
		(totals sum - totals max) / (totals size - 1 max: 1) * safetyFactor.
	].
	start _ Time millisecondClockValue.
	self forkProgressWatcher.

	[
		aBlock 
			on: ProgressInitiationException
			do: [ :ex | 
				ex sendNotificationsTo: [ :min :max :curr |
					"ignore this as it is inaccurate"
				].
			].
	] on: ProgressNotification do: [ :note |
		translucentMorph show.
		targetOwner ifNotNil: [targetOwner show].
		note extraParam ifNotNil:[self addProgressDecoration: note extraParam].
		stageCompletedString _ (note messageText findTokens: ' ') first.
		stageCompleted _ (stageCompletedString copyUpTo: $:) asNumber.
		cumulativeStageTime _ Time millisecondClockValue - start max: 1.
		prevData _ self loadingHistoryDataForKey: stageCompletedString.
		prevData isEmpty ifFalse: [
			newRatio _ (cumulativeStageTime / (prevData average max: 1)) asFloat.
		].
		self 
			loadingHistoryAt: stageCompletedString 
			add: cumulativeStageTime.
		translucentMorph 
			setProperty: #progressStageNumber 
			toValue: stageCompleted + 1.
		note resume.
	].

	stageCompleted _ 999.	"we may or may not get here"

! !!ContextPart methodsFor: 'debugger access' stamp: 'sw 3/4/2004 00:27'!errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; cr.
	strm cr.
	strm nextPutAll: 'VM: ';
		nextPutAll:  SmalltalkImage current platformName asString;
		nextPutAll: ' - ';
		nextPutAll: SmalltalkImage current asString;
		cr.
	strm nextPutAll: 'Image: ';
		nextPutAll:  SystemVersion current version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		cr.
	strm cr.
	SecurityManager default printStateOn: strm.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt _ 0.  startPos _ strm position.
	aContext _ self.
	[aContext notNil and: [(cnt _ cnt + 1) < 5]] whileTrue:
		[aContext printDetails: strm.	"variable values"
		strm cr.
		aContext _ aContext sender].

	strm cr; nextPutAll: '--- The full stack ---'; cr.
	aContext _ self.
	cnt _ 0.
	[aContext == nil] whileFalse:
		[cnt _ cnt + 1.
		cnt = 5 ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; cr].
		strm print: aContext; cr.  "just class>>selector"	

		strm position > (startPos+4000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 60 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext _ aContext sender].
! !!Debugger methodsFor: 'initialize' stamp: 'mir 11/10/2003 15:13'!preDebugButtonQuads

	^Preferences eToyFriendly
		ifTrue: [
	 #(('Store log'		storeLog 	blue 	'write a log of the encountered problem' )
		('Abandon'		abandon 	black 	'abandon this execution by closing this window')
		('Debug'		debug 		red 		'bring up a debugger'))]
		ifFalse: [
	 #(('Proceed'		proceed 	blue 	'continue execution' )
		('Abandon'		abandon 	black 	'abandon this execution by closing this window')
		('Debug'		debug 		red 		'bring up a debugger'))]
! !!Debugger methodsFor: 'notifier menu' stamp: 'mir 3/5/2004 19:26'!storeLog	| logFileName |	logFileName := Preferences debugLogTimestamp		ifTrue: ['SqueakDebug-' , Time totalSeconds printString , '.log']		ifFalse: ['SqueakDebug.log'].
	Smalltalk logError: labelString printString inContext: contextStackTop to: logFileName
! !!PreDebugWindow methodsFor: 'as yet unclassified' stamp: 'mir 11/10/2003 15:15'!storeLog
	model storeLog! !!Preferences class methodsFor: 'standard queries' stamp: 'mir 3/5/2004 19:22'!debugLogTimestamp	^ self		valueOfFlag: #debugLogTimestamp		ifAbsent: [false]! !!Preferences class methodsFor: 'standard queries' stamp: 'mir 11/10/2003 14:28'!standaloneSecurityChecksEnabled
	^ self
		valueOfFlag: #standaloneSecurityChecksEnabled
		ifAbsent: [false]! !!ProjectLoading class methodsFor: 'as yet unclassified' stamp: 'sw 3/4/2004 00:34'!openName: aFileName stream: preStream fromDirectory: aDirectoryOrNil withProjectView: existingView
	"Reconstitute a Morph from the selected file, presumed to be represent a Morph saved via the SmartRefStream mechanism, and open it in an appropriate Morphic world."
	
 	| morphOrList proj trusted localDir projStream archive mgr projectsToBeDeleted baseChangeSet enterRestricted |
	(preStream isNil or: [preStream size = 0]) ifTrue: [
		ProgressNotification  signal: '9999 about to enter project'.		"the hard part is over"
		^self inform: 
'It looks like a problem occurred while
getting this project. It may be temporary,
so you may want to try again,' translated
	].
	ProgressNotification signal: '2:fileSizeDetermined ',preStream size printString.
	preStream isZipArchive 
		ifTrue:[	archive _ ZipArchive new readFrom: preStream.
				projStream _ self projectStreamFromArchive: archive]
		ifFalse:[projStream _ preStream].
	trusted _ SecurityManager default positionToSecureContentsOf: projStream.
	trusted ifFalse:
		[enterRestricted := (preStream isTypeHTTP or: [aFileName isNil])
			ifTrue: [Preferences securityChecksEnabled]
			ifFalse: [Preferences standaloneSecurityChecksEnabled].
		enterRestricted 
			ifTrue: [SecurityManager default enterRestrictedMode
				ifFalse:
					[preStream close.
					^ self]]].

	localDir _ Project squeakletDirectory.
	aFileName ifNotNil: [
		(aDirectoryOrNil isNil or: [aDirectoryOrNil pathName ~= localDir pathName]) ifTrue: [
			localDir deleteFileNamed: aFileName.
			(localDir fileNamed: aFileName) 
				nextPutAll: preStream contents;
				close.
		].
	].
	morphOrList _ projStream asUnZippedStream.
	preStream sleep.		"if ftp, let the connection close"
	ProgressNotification  signal: '3:unzipped'.
	ResourceCollector current: ResourceCollector new.
	baseChangeSet _ ChangeSet current.
	self useTempChangeSet.		"named zzTemp"
	"The actual reading happens here"
	[morphOrList _ morphOrList fileInObjectAndCode] ensure: [
				ChangeSet  newChanges: baseChangeSet].
	mgr _ ResourceManager new initializeFrom: ResourceCollector current.
	mgr registerUnloadedResources.
	archive ifNotNil:[mgr preLoadFromArchive: archive cacheName: aFileName].
	(preStream respondsTo: #close) ifTrue:[preStream close].
	ResourceCollector current: nil.
	ProgressNotification  signal: '4:filedIn'.
	ProgressNotification  signal: '9999 about to enter project'.		"the hard part is over"
	(morphOrList isKindOf: ImageSegment) ifTrue: [
		proj _ morphOrList arrayOfRoots 
			detect: [:mm | mm isKindOf: Project] 
			ifNone: [^self inform: 'No project found in this file'].
		proj resourceManager: mgr.
		"proj versionFrom: preStream."
		proj lastDirectory: aDirectoryOrNil.
		CurrentProjectRefactoring currentBeParentTo: proj.
		projectsToBeDeleted _ OrderedCollection new.
		existingView ifNil: [
			Smalltalk isMorphic ifTrue: [
				proj createViewIfAppropriate.
			] ifFalse: [
				ChangeSorter allChangeSets add: proj changeSet.
				ProjectView openAndEnter: proj.
				"Note: in MVC we get no further than the above"
			].
		] ifNotNil: [
			(existingView project isKindOf: DiskProxy) ifFalse: [
				existingView project changeSet name: ChangeSet defaultName.
				projectsToBeDeleted add: existingView project.
			].
			(existingView owner isSystemWindow) ifTrue: [
				existingView owner model: proj
			].
			existingView project: proj.
		].
		ChangeSorter allChangeSets add: proj changeSet.
		Project current projectParameters 
			at: #deleteWhenEnteringNewProject 
			ifPresent: [ :ignored | 
				projectsToBeDeleted add: Project current.
				Project current removeParameter: #deleteWhenEnteringNewProject.
			].
		projectsToBeDeleted isEmpty ifFalse: [
			proj projectParameters 
				at: #projectsToBeDeleted 
				put: projectsToBeDeleted.
		].
	
		proj world ifNotNil:
			[(proj world valueOfProperty: #soundAdditions) ifNotNilDo:
				[:additions | SampledSound assimilateSoundsFrom: additions]].

		^ ProjectEntryNotification signal: proj
	].

	(morphOrList isKindOf: SqueakPage) ifTrue: [
		morphOrList _ morphOrList contentsMorph
	].
	(morphOrList isKindOf: PasteUpMorph) ifFalse:
		[^ self inform: 'This is not a PasteUpMorph or exported Project.' translated].
	(Project newMorphicOn: morphOrList) enter
! !!SecurityManager methodsFor: 'private' stamp: 'mir 11/10/2003 16:14'!printStateOn: stream
	"Print the current state of myself onto stream.
	Used to gather information in the debug log."

	stream
		nextPutAll: 'SecurityManager state:'; cr;
		nextPutAll: 'Restricted: '; nextPutAll: self isInRestrictedMode asString; cr;
		nextPutAll: 'FileAccess: '; nextPutAll: self hasFileAccess asString; cr;
		nextPutAll: 'SocketAccess: '; nextPutAll: self hasSocketAccess asString; cr;
		nextPutAll: 'Working Dir '; nextPutAll: FileDirectory default pathName asString; cr;
		nextPutAll: 'Trusted Dir '; nextPutAll: self secureUserDirectory asString; cr;
		nextPutAll: 'Untrusted Dir '; nextPutAll: self untrustedUserDirectory asString; cr;
		cr! !!Stream methodsFor: 'testing' stamp: 'mir 11/10/2003 18:22'!isTypeHTTP
	^false! !"Postscript:
"
Preferences setPreference: #standaloneSecurityChecksEnabled toValue: false.
!