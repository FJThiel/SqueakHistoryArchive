'From SqueakLight|II of 31 May 2008 [latest update: #7218] on 12 May 2009 at 6:15:06 pm'!"Change Set:		ProcustesEndDate:			19 November 2006Author:			Edgar J. De CleeneProcustesProcrustes means he who stretches. He goes by other names as Damastes and Polypemon. Procrustes has two beds. When travelers come by, Procrustes offers them a bed. If it is a tall traveler he offers a small bed, if it is a short traveler, he offers a long bed. Then Procrustes cuts the tall travelers feet to make him fit and he stretches the short travel to make him fit the long bed. Theseus manages to get rid of this bandit and puts an end to his stretching and molesting.As some sure miss Pocustes way, we are forced to use Monticello.And I trick for use some SAR kind of"!Object subclass: #PackageInfo	instanceVariableNames: 'packageName methodCategoryPrefix postscript preamble resources preambleOfRemoval window postscriptOfRemoval '	classVariableNames: ''	poolDictionaries: ''	category: 'PackageInfo-Base'!MCTool subclass: #PackageResources	instanceVariableNames: 'resources resourcesListIndex thisPackage'	classVariableNames: ''	poolDictionaries: ''	category: 'PackageInfo-Base'!!Object methodsFor: 'evaluating' stamp: 'edc 7/18/2005 10:51'!ancestors|  nonMetaClass  classList |	nonMetaClass := self theNonMetaClass.			classList := OrderedCollection new.		nonMetaClass allSuperclasses reverseDo: 		[:aClass | 		classList add: aClass name.		].	^ classList! !!Object methodsFor: 'evaluating' stamp: 'edc 7/18/2005 10:51'!othersClassList|classList metodosSospechosos | classList := Set new.metodosSospechosos := self  methodDict .metodosSospechosos isEmpty		ifFalse: [metodosSospechosos				collect: [:cm | cm literals						select: [:any | any isVariableBinding]						thenCollect: [:each | (Smalltalk at: each key ifAbsent:[])								ifNotNil: [  classList add: each key]]]].					metodosSospechosos := self class methodDict .metodosSospechosos isEmpty		ifFalse: [metodosSospechosos				collect: [:cm | cm literals						select: [:any | any isVariableBinding]						thenCollect: [:each | (Smalltalk at: each key ifAbsent:[])								ifNotNil: [classList add: each key]]]].					classList remove: self name  ifAbsent: [].					^classList					! !!MCCacheRepository methodsFor: 'as yet unclassified' stamp: 'edc 12/2/2006 09:26'!basicStoreVersion: aVersion	(aVersion isCacheable and: [self allFileNames includes: aVersion fileName])		ifFalse: [super basicStoreVersion: aVersion]! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!associate: tokens	| result |	result := Dictionary new.	tokens pairsDo: [:key :value | 					value isString ifFalse: [value := value collect: [:ea | self associate: ea]].					result at: key put: value].	^ result! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!extractDefinitionsFrom: member	| reader |	(MCSnapshotReader readerClassForFileNamed: member fileName)		ifNotNilDo: [:rc | reader := rc on: member contentStream text.					definitions addAll: reader definitions]! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'avi 1/19/2004 16:11'!extractDependencyFrom: zipMember	^ MCVersionDependency		package: (MCPackage named: (zipMember fileName copyAfterLast: $/))		info: (self extractInfoFrom: (self parseMember: zipMember fileName))! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'avi 9/17/2005 23:38'!extractInfoFrom: dict	^ self infoCache at: (dict at: #id) ifAbsentPut:		[MCVersionInfo			name: (dict at: #name ifAbsent: [''])			id: (UUID fromString: (dict at: #id))			message: (dict at: #message ifAbsent: [''])			date: ([Date fromString: (dict at: #date) ] on: Error do: [ :ex | ex return: nil ])			time: ([ Time fromString:(dict at: #time)] on: Error do: [ :ex | ex return: nil ])			author: (dict at: #author ifAbsent: [''])			ancestors: ((dict at: #ancestors) collect: [:ea | self extractInfoFrom: ea])			stepChildren: ((dict at: #stepChildren ifAbsent: [#()]) collect: [:ea | self extractInfoFrom: ea])]! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!infoCache	^ infoCache ifNil: [infoCache := Dictionary new]! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!loadDefinitions	definitions := OrderedCollection new.	(self zip memberNamed: 'snapshot.bin') ifNotNilDo:		[:m | [^ definitions := (DataStream on: m contentStream) next definitions]			on: Error do: [:fallThrough ]].	"otherwise"	(self zip membersMatching: 'snapshot/*')		do: [:m | self extractDefinitionsFrom: m].! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!loadDependencies	dependencies := (self zip membersMatching: 'dependencies/*') collect: [:m | self extractDependencyFrom: m].	dependencies := dependencies asArray.! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!loadPackage	| dict |	dict := self parseMember: 'package'.	package := MCPackage named: (dict at: #name)! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!loadVersionInfo	info := self extractInfoFrom: (self parseMember: 'version')! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'stephaneducasse 2/4/2006 20:47'!parseMember: fileName	| tokens |	tokens := (self scanner scanTokens: (self zip contentsOf: fileName)) first.	^ self associate: tokens! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'avi 1/22/2004 20:33'!scanner	^ MCScanner! !!MCMczReader methodsFor: 'as yet unclassified' stamp: 'boot 11/14/2006 08:45'!zip	zip ifNil:		[zip := ZipArchive new.		zip readFrom: stream.		self class zip: zip].	^ zip! !!MCMczReader class methodsFor: 'accessing' stamp: 'cwp 8/1/2003 14:59'!extension	^ 'mcz'! !!MCMczReader class methodsFor: 'accessing' stamp: 'boot 11/14/2006 08:43'!zip^ zip! !!MCMczReader class methodsFor: 'accessing' stamp: 'boot 11/14/2006 08:43'!zip: aZipArchive zip:= aZipArchive! !!MCMczReader class methodsFor: 'testing' stamp: 'avi 1/19/2004 14:48'!supportsDependencies	^ true! !!MCMczReader class methodsFor: 'testing' stamp: 'cwp 8/1/2003 12:19'!supportsVersions	^ true! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/7/2006 11:24'!basicNewChangeSet: newName	Smalltalk at: #ChangesOrganizer ifPresentAndInMemory: [ :cs | ^cs basicNewChangeSet: newName ].	(self changeSetNamed: newName) ifNotNil: [ self inform: 'Sorry that name is already used'. ^nil ].	^ChangeSet basicNewNamed: newName.! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/7/2006 11:22'!changeSetNamed: newName	Smalltalk at: #ChangesOrganizer ifPresentAndInMemory: [ :cs | ^cs changeSetNamed: newName ].	^ChangeSet allInstances detect: [ :cs | cs name = newName ] ifNone: [ nil ].! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/5/2006 07:26'!currentChangeSet	"I copy from SARInstaller currentChangeSet "		^[ ChangeSet current ]		on: MessageNotUnderstood		do: [ :ex | ex return: Smalltalk changes ]! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/5/2006 07:28'!errorNoSuchMember: aMemberName	(self confirm: 'No member named ', aMemberName, '. Do you want to stop loading?')		== true ifTrue: [ self error: 'aborted' ].! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/7/2006 11:26'!fileInMemberNamed: csName	"This is to be used from preamble/postscript code to file in zip members as ChangeSets."	| cs |	cs _ self zip memberNamed: csName.	cs ifNil: [ ^self errorNoSuchMember: csName ].	self  fileIntoChangeSetNamed: csName fromStream: cs contentStream text setConverterForCode.	! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/5/2006 07:28'!fileIntoChangeSetNamed: aString fromStream: aStream 	"We let the user confirm filing into an existing ChangeSet	or specify another ChangeSet name if	the name derived from the filename already exists.	Duplicated from SMSimpleInstaller.	Should be a class-side method."	^self withCurrentChangeSetNamed: aString		do: [ :cs | | newName |			newName := cs name.			aStream setConverterForCode.			aStream 				fileInAnnouncing: 'Loading ' , newName , ' into change set ''' , newName, ''''.			aStream close]! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/7/2006 11:25'!newChanges: aChangeSet		^[ ChangeSet newChanges: aChangeSet ]		on: MessageNotUnderstood		do: [ :ex | ex return: (Smalltalk newChanges: aChangeSet) ]! !!MCMczReader class methodsFor: 'sarMimic' stamp: 'edc 12/7/2006 11:21'!withCurrentChangeSetNamed: aString do: aOneArgumentBlock 	"Evaluate the one-argument block aOneArgumentBlock while the named change set is active.	We let the user confirm operating on an existing ChangeSet 	or specify another ChangeSet name if 	the name derived from the filename already exists. 	Duplicated from SMSimpleInstaller. 	Returns change set."	| changeSet newName oldChanges |	newName := aString.	changeSet := self changeSetNamed: newName.	changeSet ifNotNil: 			[newName := UIManager default 						request: 'ChangeSet already present, just confirm to overwrite or enter a new name:'						initialAnswer: newName.			newName isEmpty ifTrue: [self error: 'Cancelled by user'].			changeSet := self changeSetNamed: newName].	changeSet ifNil: [changeSet := self basicNewChangeSet: newName].	changeSet 		ifNil: [self error: 'User did not specify a valid ChangeSet name'].	oldChanges := self currentChangeSet.		[ self newChanges: changeSet.	aOneArgumentBlock value: changeSet] 			ensure: [ self newChanges: oldChanges].	^changeSet! !!MCMczWriter methodsFor: 'visiting' stamp: 'edc 12/2/2006 10:21'!writeResources: aVersion| local resources |resources := aVersion package resources.resources do: [:file| local :=  FileDirectory localNameFor: file.local := 'resources/',local.	(zip addFile: file as: local)		desiredCompressionMethod: ZipArchive compressionDeflated].	! !!MCMczWriter methodsFor: 'visiting' stamp: 'edc 12/7/2006 11:03'!writeSnapshot: aSnapshot	self addString: (self serializeDefinitions: aSnapshot definitions) at: 'snapshot/source.', self snapshotWriterClass extension.	self addString: (self serializeInBinary: aSnapshot) at: 'snapshot.bin'! !!MCMczWriter methodsFor: 'visiting' stamp: 'edc 12/2/2006 10:06'!writeVersion: aVersion	self writeFormat.	self writePackage: aVersion package.	self writeVersionInfo: aVersion info.	self writeDefinitions: aVersion.	aVersion dependencies do: [:ea | self writeVersionDependency: ea].	 aVersion package hasResources	 ifTrue:[self writeResources:aVersion ].! !!MCMczWriter class methodsFor: 'as yet unclassified' stamp: 'edc 12/2/2006 10:06'!fileOut: aVersion on: aStream	| inst |		inst := self on: aStream.	inst writeVersion: aVersion.			inst flush.	! !!MCPackage methodsFor: 'as yet unclassified' stamp: 'edc 12/2/2006 09:40'!hasResources^ self packageInfo hasResources! !!MCPackage methodsFor: 'as yet unclassified' stamp: 'boot 12/2/2006 07:30'!postscript^self packageInfo postscript! !!MCPackage methodsFor: 'as yet unclassified' stamp: 'boot 12/2/2006 07:28'!preamble^self packageInfo preamble! !!MCPackage methodsFor: 'as yet unclassified' stamp: 'edc 12/2/2006 09:29'!resources^ self packageInfo resources! !!MCPackage methodsFor: 'as yet unclassified' stamp: 'edc 11/12/2006 16:14'!snapshot	| packageInfo definitions categories |	packageInfo := self packageInfo.	definitions := OrderedCollection new.	categories := packageInfo systemCategories.	categories isEmpty ifFalse: [ definitions add: (MCOrganizationDefinition categories: categories) ].	packageInfo methods do: [:ea | definitions add: ea asMethodDefinition] displayingProgress: 'Snapshotting methods...'.	(packageInfo respondsTo: #overriddenMethods) ifTrue:		[packageInfo overriddenMethods			do: [:ea | definitions add:					(packageInfo changeRecordForOverriddenMethod: ea) asMethodDefinition]			displayingProgress: 'Searching for overrides...'].	packageInfo classes do: [:ea | definitions addAll: ea classDefinitions] displayingProgress: 'Snapshotting classes...'.	(packageInfo respondsTo: #hasPreamble) ifTrue: [		packageInfo hasPreamble ifTrue: [definitions addFirst: (MCPreambleDefinition from: packageInfo)].		packageInfo hasPostscript ifTrue: [definitions add: (MCPostscriptDefinition from: packageInfo)].		packageInfo hasPreambleOfRemoval ifTrue: [definitions add: (MCRemovalPreambleDefinition from: packageInfo)].		packageInfo hasPostscriptOfRemoval ifTrue: [definitions add: (MCRemovalPostscriptDefinition from: packageInfo)]]. 	^ MCSnapshot fromDefinitions: definitions! !!MCStWriter methodsFor: 'writing' stamp: 'edc 11/12/2006 07:02'!writeDefinitions: aCollection	"initStream is an ugly hack until we have proper init defs"	| noInitialize classList |	initStream := String new writeStream.classList := OrderedCollection new..classList := aCollection select:[:ea| (ea isKindOf: MCMethodDefinition) ].classList := classList select: [:any | any   isInitializer] .classList :=(SortedCollection sortBlock: [:c1 :c2| ((Smalltalk at: c1 className ) ancestors size) < ((Smalltalk at: c2 className ) ancestors size) ]) addAll: classList; yourself.noInitialize := aCollection copyWithoutAll: classList.	(MCDependencySorter sortItems: noInitialize)		do: [:ea | ea accept: self]		displayingProgress: 'Writing definitions...'.		classList do:[:ea| self writeMethodInitializer: ea].		stream nextPutAll: initStream contents.! !!MCVersion methodsFor: 'actions' stamp: 'edc 12/2/2006 09:46'!fileOutOn: aStream	self writerClass fileOut: self on: aStream.	! !!MCVersion methodsFor: 'actions' stamp: 'edc 12/8/2006 11:15'!load: v fromDirectory: d	MCMczReader loadVersionStream: v fromDirectory: d! !!MCVersionInspector methodsFor: 'as yet unclassified' stamp: 'edc 12/8/2006 16:01'!load| stream dir v |v := self version.dir :=  self repository directory.stream := dir readOnlyFileNamed: v info name,'.mcz'.	Cursor wait showWhile: [self version load: stream fromDirectory: dir]! !!MCFileRepositoryInspector methodsFor: 'as yet unclassified' stamp: 'edc 12/8/2006 11:20'!repository^ repository! !!MCVersionLoader methodsFor: 'loading' stamp: 'edc 12/5/2006 09:25'!load	| loader preamble postscript package |		self checkForModifications.	loader := MCPackageLoader new.	versions do: [:ea |		ea canOptimizeLoading			ifTrue: [ea patch applyTo: loader]			ifFalse: [loader updatePackage: ea package withSnapshot: ea snapshot]].		package := versions first package packageInfo .	postscript	 := (versions first snapshot definitions last) isKindOf: MCPostscriptDefinition .		postscript ifTrue:[ package postscript: (versions first snapshot definitions last) source].	preamble	 := (versions first snapshot definitions first) isKindOf: MCPreambleDefinition.		preamble ifTrue:[ package preamble: (versions first snapshot definitions first) source].			loader loadWithNameLike: versions first info name.	versions do: [:ea | ea workingCopy loaded: ea]! !!MCWorkingCopyBrowser methodsFor: 'actions' stamp: 'edc 12/2/2006 11:17'!addResourcesworkingCopy packageInfo addResources! !!MCWorkingCopyBrowser methodsFor: 'actions' stamp: 'edc 12/2/2006 09:20'!saveVersion	| repo |	self canSave ifFalse: [^self].	self checkForNewerVersions ifFalse: [^self].	repo _ self repository.	workingCopy newVersion ifNotNilDo:		[:v |		(MCVersionInspector new version: v) show.				Cursor wait showWhile: [repo storeVersion: v].		MCCacheRepository default cacheAllFileNamesDuring: 			[repo cacheAllFileNamesDuring: 				[v allAvailableDependenciesDo:					[:dep |					(repo includesVersionNamed: dep info name)						ifFalse: [repo storeVersion: dep]]]]]! !!MCWorkingCopyBrowser methodsFor: 'morphic ui' stamp: 'edc 12/2/2006 11:10'!editLoadScripts	| menu |	self hasWorkingCopy ifFalse: [^self].	menu := MenuMorph new defaultTarget: self.	menu add: 'edit preamble' selector: #editScript: argument: #preamble.	menu add: 'edit postscript' selector: #editScript: argument: #postscript.	menu add: 'edit preambleOfRemoval' selector: #editScript: argument: #preambleOfRemoval.	menu add: 'edit postscriptOfRemoval' selector: #editScript: argument: #postscriptOfRemoval.	menu add: 'add non code resources to this package' selector: #addResources argument: nil.	menu popUpInWorld.! !!MCWorkingCopyBrowser methodsFor: 'morphic ui' stamp: 'edc 11/12/2006 12:03'!editScript: scriptSymbol|  setter title |setter := (scriptSymbol ,':' asString) asSymbol.title := scriptSymbol asString, ' of the Package ', workingCopy package name.UIManager default		edit: (workingCopy packageInfo perform: scriptSymbol)		label: title		accept:[:aString| workingCopy packageInfo perform: setter with: aString]		! !!PackageInfo methodsFor: 'resources' stamp: 'edc 12/7/2006 08:23'!addResourcesresources ifNil:[	resources := OrderedCollection new].	window := PackageResources basicNew.window thisPackage:   self .window initialize.! !!PackageInfo methodsFor: 'resources' stamp: 'edc 12/7/2006 10:31'!moreResources| result full|[result := FileList2 modalFileSelector .	result isNil]whileFalse:[full := result directory fullNameFor: result name.		(resources add: full)]!]style[(13 11 4 123 23 1)f1b,f1,f1cred;,f1,f1cred;,f1! !!PackageInfo methodsFor: 'resources' stamp: 'edc 12/7/2006 08:29'!resources	"Answer the value of resources"	^ resources collect: [:ea| FileDirectory default localNameFor: ea]		! !!PackageInfo methodsFor: 'resources' stamp: 'boot 12/2/2006 07:58'!resources: anObject	"Set the value of resources"	resources := anObject! !!PackageInfo methodsFor: 'testing' stamp: 'edc 12/2/2006 10:05'!hasResources^ resources ~= nil! !!PackageInfo methodsFor: 'accessing' stamp: 'boot 12/2/2006 07:58'!methodCategoryPrefix: anObject	"Set the value of methodCategoryPrefix"	methodCategoryPrefix := anObject! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:25'!hasPostscript	"Answer the value of hasPostscript"	^ postscript notNil! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:29'!hasPostscriptOfRemoval	"Answer the value of hasPostscriptOfRemoval"	^ hasPostscriptOfRemoval notNil! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 10:54'!hasPostscriptOfRemoval: anObject	"Set the value of hasPostscriptOfRemoval"	hasPostscriptOfRemoval := anObject! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:28'!hasPreamble	"Answer the value of hasPreamble"	^ postscript notNil! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:28'!hasPreambleOfRemoval	"Answer the value of hasPreambleOfRemoval"	^ hasPreambleOfRemoval notNil! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 10:54'!hasPreambleOfRemoval: anObject	"Set the value of hasPreambleOfRemoval"	hasPreambleOfRemoval := anObject! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:04'!postscript	"Answer the string representing the postscript.  "	^postscript ifNotNil:[postscript isString ifTrue:[postscript] ifFalse:[postscript contents asString]]	ifNil:[^'"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one." ']! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 12:03'!postscript: aStringpostscript:= aString! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'mist 2/17/2005 23:14'!postscriptOfRemoval^ postscriptOfRemoval ifNil: [postscriptOfRemoval _ StringHolder new contents: '"below, add code to clean up after the unloading of this package"']! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'mist 2/18/2005 22:45'!postscriptOfRemoval: aStringpostscriptOfRemoval _ StringHolder new contents: aString! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:07'!preamble	"Answer the string representing the preamble"	^preamble ifNotNil:[preamble isString ifTrue:[preamble] ifFalse:[preamble contents asString]]	ifNil:[^self preambleTemplate]! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 12:03'!preamble: aStringpreamble:= aString! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'mist 2/17/2005 23:15'!preambleOfRemoval^ preambleOfRemoval ifNil: [preambleOfRemoval _ StringHolder new contents: '"below, add code to prepare for the unloading of this package"']! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'mist 2/19/2005 00:41'!preambleOfRemoval: aStringpreambleOfRemoval _ StringHolder new contents: aString! !!PackageInfo methodsFor: 'preamble/postscript' stamp: 'edc 11/12/2006 15:06'!preambleTemplate	"Answer a string that will form the default contents for a change set's preamble.	Just a first stab at what the content should be."	^ String streamContents: [:strm |		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."		strm tab;tab; nextPutAll: self name.		strm cr; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.		strm cr; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: Preferences defaultAuthorName.		strm cr; cr; nextPutAll: '<your descriptive text goes here>"']"PackageInfo preambleTemplate"! !!PackageResources methodsFor: 'accessing' stamp: 'edc 11/23/2006 11:38'!resources^self thisPackage resources! !!PackageResources methodsFor: 'accessing' stamp: 'edc 11/23/2006 10:47'!resourcesListIndex	"Answer the value of resourcesListIndex"	^ resourcesListIndex! !!PackageResources methodsFor: 'accessing' stamp: 'edc 11/23/2006 10:47'!resourcesListIndex: anObject	"Set the value of resourcesListIndex"	resourcesListIndex := anObject! !!PackageResources methodsFor: 'accessing' stamp: 'edc 11/23/2006 11:24'!thisPackage	"Answer the value of thisPackage"	^ thisPackage! !!PackageResources methodsFor: 'accessing' stamp: 'edc 11/23/2006 11:24'!thisPackage: anObject	"Set the value of thisPackage"	thisPackage := anObject! !!PackageResources methodsFor: 'initialize-release' stamp: 'edc 12/7/2006 07:30'!initialize| modal |modal := false.	Smalltalk at: #ToolBuilder ifPresent: [:tb | tb open: self. ^ self].	^self window openInWorldExtent: self defaultExtent; yourself! !!PackageResources methodsFor: 'initialize-release' stamp: 'edc 11/23/2006 10:41'!wantsOptionalButtons	"Sure, why not?"	^ true! !!PackageResources methodsFor: 'as yet unclassified' stamp: 'edc 12/7/2006 10:26'!buildWith: builder	"Create the ui for the browser"	| windowSpec listSpec buttonSpec panelSpec  |	self resourcesListIndex:0.	windowSpec := builder pluggableWindowSpec new.	windowSpec model: self.	windowSpec label: 'PackageInfoResources Browser'.	windowSpec children: OrderedCollection new.	listSpec := builder pluggableListSpec new.	listSpec 		model: self ;		list: #resources; 		getIndex: #resourcesListIndex; 		setIndex: 0; 		menu: #resourcesListMenu:; 		frame: (0@0 corner: 0.70@1.0).	windowSpec children add: listSpec.		panelSpec := builder pluggablePanelSpec new.	panelSpec frame: (0.7@0.1 corner: 0.95@0.95).	panelSpec children: OrderedCollection new.	windowSpec children addLast: panelSpec.		buttonSpec := builder pluggableButtonSpec new.		buttonSpec 			model: self thisPackage;			label: 'addFiles'; 			state: true; 			action: #moreResources;			frame: (0@0 corner: 0.95@0.95).		panelSpec children addLast: buttonSpec.				^builder build: windowSpec! !!PackageResources methodsFor: 'as yet unclassified' stamp: 'edc 11/23/2006 10:51'!resourcesListMenu: aMenuself halt! !Object subclass: #PackageInfo	instanceVariableNames: 'packageName methodCategoryPrefix preamble postscript resources postscriptOfRemoval preambleOfRemoval window'	classVariableNames: ''	poolDictionaries: ''	category: 'PackageInfo-Base'!!PackageInfo reorganize!('comparing' = hash)('dependencies' externalCallers externalClasses externalPackages externalRefsSelect:thenCollect: externalSubclasses externalUsers)('listing' classes classesAndMetaClasses coreMethods extensionClasses extensionMethods foreignClasses foreignSystemCategories methods overrideMethods selectors systemCategories)('resources' addResources moreResources resources resources:)('modifying' addCoreMethod: addExtensionMethod: addMethod: baseCategoryOfMethod: externalBehaviors externalTraits removeMethod:)('naming' categoryName externalName methodCategoryPrefix packageName packageName: systemCategoryPrefix)('registering' register)('testing' category:matches: coreCategoriesForClass: coreMethodsForClass: extensionCategoriesForClass: extensionMethodsForClass: extensionMethodsFromClasses: foreignExtensionCategoriesForClass: foreignExtensionMethodsForClass: hasResources includesClass: includesClassNamed: includesMethod:ofClass: includesMethodCategory:ofClass: includesMethodCategory:ofClassNamed: includesMethodReference: includesSystemCategory: isForeignClassExtension: isOverrideMethod: isYourClassExtension: outsideClasses referenceForMethod:ofClass:)('*monticello')('*omnibrowser')('accessing' methodCategoryPrefix:)('preamble/postscript' hasPostscript hasPostscriptOfRemoval hasPostscriptOfRemoval: hasPreamble hasPreambleOfRemoval hasPreambleOfRemoval: postscript postscript: postscriptOfRemoval postscriptOfRemoval: preamble preamble: preambleOfRemoval preambleOfRemoval: preambleTemplate)!