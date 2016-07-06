'From Squeak3.7beta of ''1 April 2004'' [latest update: #5948] on 12 June 2004 at 1:24:46 pm'!"Change Set:		FileListServicesForDirectories-nkDate:			12 June 2004Author:			Ned KonzAllows registered FileList service providers to add services for directories. These will be available from the volume list menu and from the directory pane.Also restores the FileList2 volume list menu."!!FileList methodsFor: 'file list menu' stamp: 'nk 6/12/2004 12:05'!itemsForDirectory: dir 	| services |	services := OrderedCollection new.	dir ifNotNil: [		services			addAll: (self class itemsForDirectory: dir).		services last useLineAfter: true. ].	services add: self serviceAddNewFile.	services add: self serviceAddNewDirectory.	^ services! !!FileList methodsFor: 'file list menu' stamp: 'nk 6/12/2004 12:06'!itemsForNoFile	| services |	services := OrderedCollection new.	services add: self serviceSortByName.	services add: self serviceSortBySize.	services add: (self serviceSortByDate useLineAfter: true).	services addAll: (self itemsForDirectory: (self isFileSelected ifFalse: [ self directory ] ifTrue: [])).	^ services		! !!FileList methodsFor: 'volume menu' stamp: 'nk 6/12/2004 12:07'!volumeMenu: aMenu	aMenu addList: {			{'recent...' translated.		#recentDirs}.			#-.			{'add server...' translated.		#askServerInfo}.			{'remove server...' translated.		#removeServer}.			#-.			{'delete directory...' translated.	#deleteDirectory}.			#-}.	aMenu		addServices: (self itemsForDirectory: self directory)		for: self		extraLines: #().	^aMenu.! !!FileList2 methodsFor: 'as yet unclassified' stamp: 'rww 12/13/2003 13:07'!morphicDirectoryTreePaneFiltered: aSymbol	^(SimpleHierarchicalListMorph 		on: self		list: aSymbol		selected: #currentDirectorySelected		changeSelected: #setSelectedDirectoryTo:		menu: #volumeMenu:		keystroke: nil)			autoDeselect: false;			enableDrag: false;			enableDrop: true;			yourself		! !!Object class methodsFor: 'file list services' stamp: 'nk 6/12/2004 11:41'!fileReaderServicesForDirectory: aFileDirectory	"Backstop"	^#()! !!Object class methodsFor: 'file list services' stamp: 'nk 6/12/2004 11:30'!fileReaderServicesForFile: fullName suffix: suffix	"Backstop"	^#()! !!FileList class methodsFor: 'file reader registration' stamp: 'nk 6/12/2004 11:42'!itemsForDirectory: aFileDirectory	"Answer a list of services appropriate when no file is selected."	| services |	services _ OrderedCollection new.	self registeredFileReaderClasses do: [:reader |		reader ifNotNil: [services addAll: (reader fileReaderServicesForDirectory: aFileDirectory) ]].	^ services! !PluggableFileList removeSelector: #itemsForNoFile!!FileList class reorganize!('instance creation' addButtonsAndFileListPanesTo:at:plus:forFileList: addVolumesAndPatternPanesTo:at:plus:forFileList: defaultButtonPaneHeight open openAsMorph openEditorOn:editString: openFileDirectly openMorphOn:editString: prototypicalToolWindow)('class initialization' initialize registerInFlapsRegistry unload)('file reader registration' allRegisteredServices detectService:ifNone: isReaderNamedRegistered: itemsForDirectory: itemsForFile: registerFileReader: registeredFileReaderClasses suffixOf: unregisterFileReader:)('window color' windowColorSpecification)!!Object class reorganize!('instance creation' categoryForUniclasses chooseUniqueClassName initialInstance initializedInstance instanceOfUniqueClass instanceOfUniqueClassWithInstVarString:andClassInstVarString: isUniClass newFrom: newUniqueClassInstVars:classInstVars: newUserInstance readCarefullyFrom: readFrom:)('documentation' howToModifyPrimitives whatIsAPrimitive)('private' releaseExternalSettings)('objects from disk' createFrom:size:version:)('plugin generation' ccg:emitLoadFor:from:on: ccg:generateCoerceToOopFrom:on: ccg:generateCoerceToValueFrom:on: ccg:prolog:expr:index: ccgCanConvertFrom: ccgDeclareCForVar:)('class initialization' flushDependents flushEvents initialize initializeDependentsFields reInitializeDependentsFields)('file list services' fileReaderServicesForDirectory: fileReaderServicesForFile:suffix:)!!FileList reorganize!('drag''n''drop' acceptDroppingMorph:event:inMorph: dragPassengerFor:inMorph: dragTransferTypeForMorph: dropDestinationDirectory:event: isDirectoryList: primitiveCopyFileNamed:to: wantsDroppedMorph:event:inMorph:)('file list' fileList fileListIndex fileListIndex: fileName readOnlyStream)('file list menu' dirAndFileName fileContentsMenu:shifted: fileListMenu: fileSelectedMenu: fullFileListMenu:shifted: itemsForAnyFile itemsForDirectory: itemsForFile: itemsForNoFile myServicesForFile:suffix: noFileSelectedMenu: offerAllFileOptions suffixOfSelectedFile)('file menu action' addNew:byEvaluating: addNewDirectory addNewFile compressFile deleteFile get getHex renameFile sortByDate sortByName sortBySize spawn:)('initialization' buttonSelectorsToSuppress directory: dynamicButtonServices labelString modelSleep modelWakeUp optionalButtonHeight optionalButtonRow optionalButtonSpecs optionalButtonView release setFileStream: universalButtonServices updateButtonRow)('menu messages' copyName perform:orSendTo:)('own services' serviceAddNewDirectory serviceAddNewFile serviceAllFileOptions serviceBroadcastUpdate serviceCompressFile serviceCopyName serviceDeleteFile serviceGet serviceGetHex serviceRenameFile serviceSortByDate serviceSortByName serviceSortBySize serviceViewContentsInWorkspace servicesFromSelectorSpecs: viewContentsInWorkspace)('server list' askServerInfo putUpdate: removeServer)('updating' update:)('volume list and pattern' deleteDirectory directory fileNameFormattedFrom:sizePad: listForPattern: pattern pattern: veryDeepFixupWith: volumeList volumeListIndex volumeListIndex:)('volume menu' volumeMenu:)('private' addPath: contents defaultContents entriesMatching: fileNameFromFormattedItem: folderString fullName isFileSelected listForPatterns: put: readContentsBrief: readContentsHex: readServerBrief recentDirs registeredFileReaderClasses resort: sortBlock sortingByDate sortingByName sortingBySize updateFileList)!