'From Squeak3.3alpha of 2 February 2002 [latest update: #4735] on 14 February 2002 at 5:40:07 pm'!"Change Set:		FileListFix-sdDate:			14 February 2002Author:			stephane ducassePublished to 3.3a as 4765FileListFix-sd.cs.Fix some problems with the registering file listThe protocol changed also.Now any tools have to implement on its class sidethe methods: 	services that returns all the services provided	├fileReaderServicesForFile: fullName suffix: suffix that given a full name file and its suffix returns appropriate services. Note that the services will be invoked passing the fullname of the selected file.Read preamble of 4684 for more information.Known problems:the registration of a tool (when loaded in the image)is done via the class initialize method. Some classes like GZipReadStream already implement such method hence it is impossible to define an initialize method that could be unloaded when the tools would be unloaded. This means that the modules system will have to provide some mechanism so that multiple class extensions can be executed at load timeThis is for example true for the following classes:GZipReadStream, Morph, SWikiPage and Wonderland"!Object subclass: #SimpleServiceEntry	instanceVariableNames: 'provider label selector useLineAfter description stateSelector argumentGetter'	classVariableNames: ''	module: #(Squeak Development FileList)!!SimpleServiceEntry commentStamp: 'sd' prior: 0!I represent a serviceprovider : the service providerlabel : to be display in a menuselector : to do the serviceuseLineAfterstateSelector : a secondary selector (to be able to query state of the provider for example)description : a description for balloon for example!!B3DScene class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:27'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = '3ds') | (suffix = '*') 		ifTrue: [Array with: self serviceOpen3DSFile]		ifFalse: [#()]! !!B3DScene class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:30'!serviceOpen3DSFile	^ SimpleServiceEntry 				provider: self 				label: 'Open 3DS file'				selector: #open3DSFile:				description: 'open 3DS file'! !!B3DScene class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:31'!services	^ Array with: self serviceOpen3DSFile ! !!CRRecognizer class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:28'!fileReaderServicesForFile: fullName suffix: suffix	^ (suffix = 'ggd')		ifTrue: [ {SimpleServiceEntry 						provider: self 						label: 'load Genie Gesture Dictionary'						selector: #loadCRDictionary:						description: 'load Genie Gesture Dictionary'}]		ifFalse: [suffix = 'gdp'					ifTrue: [ {SimpleServiceEntry 									provider: self 									label: 'load Genie Display Properties'										selector: #loadCRDisplayProperties: description: 'load Genie Display Properties'} ]					ifFalse: [#()]] ! !!CRRecognizer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:37'!serviceLoadGenieDisplay	^ SimpleServiceEntry 				provider: self 				label: 'load Genie Display Properties'					selector: #loadCRDisplayProperties: 				description: 'load Genie Display Properties'! !!CRRecognizer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:34'!serviceLoadGenieGesture	^ SimpleServiceEntry 				provider: self 				label: 'load Genie Gesture Dictionary'				selector: #loadCRDictionary:				description: 'load Genie Gesture Dictionary'! !!CRRecognizer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:37'!services	^ Array 		with: self serviceLoadGenieGesture		with: self serviceLoadGenieDisplay	! !!ChangeList class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:29'!fileReaderServicesForFile: fullName suffix: suffix	^(FileStream isSourceFileSuffix: suffix)		ifTrue: [self services]		ifFalse: [#()]! !!ChangeList class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:52'!serviceBrowseChangeFile	^ SimpleServiceEntry 				provider: self 				label: 'as change list'				selector: #browseChangesFile:					description: 'browse as change list'! !!ChangeList class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:45'!services	^ Array with: self serviceBrowseChangeFile! !!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:29'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'st') | (suffix = 'cs') | (suffix = '*')		ifTrue: [ self services]		ifFalse: [#()]! !!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:52'!serviceFileIntoNewChangeSet	^ SimpleServiceEntry 				provider: self 				label: 'in new change set'				selector: #fileIntoNewChangeSet:				description: 'file into a new change set'			! !!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:47'!services	^ Array with: self serviceFileIntoNewChangeSet	! !!DummyToolWorkingWithFileList class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:29'!fileReaderServicesForFile: fullName suffix: suffix	^ (suffix = 'kkk')		ifTrue: [ self services]		ifFalse: [#()] ! !!DummyToolWorkingWithFileList class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:46'!initialize	"self initialize"	FileList registerFileReader: self! !!DummyToolWorkingWithFileList class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:31'!serviceLoadAFilForDummyTool	^ SimpleServiceEntry 						provider: self 						label: 'menu label'						selector: #loadAFileForTheDummyTool:						description: 'Menu label for dummy tool' ! !!DummyToolWorkingWithFileList class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:32'!services 	^ Array with: self serviceLoadAFilForDummyTool! !!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'sd 2/6/2002 21:33'!fileReaderServicesForFile: fullName suffix: suffix	^(FileStream isSourceFileSuffix: suffix)		ifTrue: [ Array with: self serviceBrowseCode]		ifFalse: [#()]! !!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'sd 2/1/2002 21:40'!serviceBrowseCode	^ SimpleServiceEntry 				provider: self 				label: 'browse code'				selector: #browseFile:				description: 'browse code'! !!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'sd 2/1/2002 21:40'!services	^ Array with: self serviceBrowseCode! !!FileList methodsFor: 'initialization' stamp: 'sd 2/14/2002 17:34'!optionalButtonRow	| aRow aButton selectorsWithArgsSelector|	selectorsWithArgsSelector _ self selectorsWithArgsSelector. 	aRow _ AlignmentMorph newRow beSticky.	aRow color: Color transparent.	aRow clipSubmorphs: true.	aRow layoutInset: 5@1; cellInset: 6.	self optionalButtonSpecs do:			[:service |				aButton :=  PluggableButtonMorph								on: service provider								getState: nil								action: service selector.				(selectorsWithArgsSelector includes: service selector)					ifTrue: [aButton argsGettingBlock: [listIndex = 0 														ifTrue: [nil]														ifFalse: [Array with: self fileName]]].				aButton					color: Color transparent;					hResizing: #spaceFill;					vResizing: #spaceFill;					useRoundedCorners;					label: service label  asString;					askBeforeChanging: true;					onColor: Color transparent offColor: Color transparent.				aRow addMorphBack: aButton.				aButton setBalloonText: service description.				(service selector  == #sortBySize)					ifTrue:						[aRow addTransparentSpacerOfSize: (4@0)]].	^ aRow ! !!FileList methodsFor: 'initialization' stamp: 'sd 2/14/2002 17:13'!optionalButtonSpecs	"the status of the button is hardcoded since they is no way we can know 	for the moment based on the services"	| services |	services := OrderedCollection new.	services add: self serviceSortByName.	services add: self serviceSortByDate.	services add: self serviceSortBySize.	services addAll: (self class servicesFromSelectorSpecs: self selectorsWithArgsSelector).	services add: self serviceDeleteFile. 	^ services! !!FileList methodsFor: 'initialization' stamp: 'sd 2/14/2002 16:35'!optionalButtonView	| aView bHeight windowWidth offset previousView aButtonView wid services |	aView _ View new model: self.	bHeight _ self optionalButtonHeight.	windowWidth _ 120.	aView window: (0@0 extent: windowWidth@bHeight).	offset _ 0.	services _ self optionalButtonSpecs copyFrom: 1 to: 6.  "Too cramped for the seventh!!"	previousView _ nil.	services do: [:service |		aButtonView _ PluggableButtonView 						on: service provider						getState: (service extraSelector == #none 									ifTrue: [nil] 									ifFalse: [service extraSelector]) 						action: service selector.		service selector = services last selector			ifTrue:				[wid _ windowWidth - offset]			ifFalse:				[aButtonView borderWidthLeft: 0 right: 1 top: 0 bottom: 0.				wid _ (windowWidth // (services size)) - 2].		aButtonView			label: service label asParagraph;			window: (offset@0 extent: wid@bHeight).		offset _ offset + wid.		service selector = services first selector			ifTrue: [aView addSubView: aButtonView]			ifFalse: [aView addSubView: aButtonView toRightOf: previousView].		previousView _ aButtonView].	^aView! !!FileList methodsFor: 'initialization' stamp: 'sd 2/14/2002 17:12'!selectorsWithArgsSelector	"returns the list of the selector that requires an extra argument"	^ #(fileIn: fileIntoNewChangeSet: browseChangesFile:)! !!FileList methodsFor: 'file list' stamp: 'sd 2/14/2002 16:58'!fileName	^ fileName! !!FileList methodsFor: 'file menu action' stamp: 'sd 2/1/2002 20:02'!spawn: code	"Open a simple Edit window"	listIndex = 0 ifTrue: [^ self].	self class openEditorOn: (directory readOnlyFileNamed: fileName)				"read only just for initial look"			editString: code! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/1/2002 21:07'!fileContentsMenu: aMenu shifted: shifted	| shiftMenu |	self flag: #toScott. 	"I tried the following but I'm not expert in menu so I simply simplify the menu"	"| services |	services := OrderedCollection new.	services add: self serviceGet; add: self serviceGetHex.	services addAll: (self class servicesFromSelectorSpecs: #(fileIntoNewChangeSet: fileIn: browseChangesFile: browseFile:)).	aMenu 		addServices: services		for: self fullName		extraLines: #(). "	^ shifted 		ifFalse: [aMenu 			labels: 'find...(f)find again (g)set search string (h)do again (j)undo (z)copy (c)cut (x)paste (v)paste...do it (d)print it (p)inspect it (i)accept (s)cancel (l)more...' 		lines: #(3 5 9 13 17)		selections: #( find findAgain setSearchStringagain undocopySelection cut paste pasteRecentdoIt printIt inspectItaccept cancelshiftedYellowButtonActivity)]	ifTrue: [shiftMenu _ ParagraphEditor shiftedYellowButtonMenu.		aMenu 			labels: shiftMenu labelString 			lines: shiftMenu lineArray			selections: shiftMenu selections] ! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/1/2002 21:29'!fileSelectedMenu: aMenu	| firstItems secondItems thirdItems n1 n2 n3 services |	firstItems _ self itemsForFile: self fullName.	secondItems _ self itemsForAnyFile.	thirdItems _ self itemsForNoFile.	n1 _ firstItems size.	n2 _ n1 + secondItems size.	n3 _ n2 + thirdItems size.	services _ firstItems, secondItems, thirdItems, self serviceAllFileOptions.	^ aMenu 		addServices: services 		for: self fullName		extraLines: (Array with: n1 with: n2 with: n3)! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/1/2002 20:03'!fullFileListMenu: aMenu shifted: aBoolean	"Fill the menu with all possible items for the file list pane, regardless of selection."	| services |	aMenu title: 'all possible file operations'.	Smalltalk isMorphic ifTrue: [aMenu addStayUpItemSpecial].	services := self class servicesFromSelectorSpecs: 							#(#openImageInWindow: #importImage: #openAsBackground: 							#fromFileName: #openFromFile:							 fileIntoNewChangeSet: fileIn: browseChangesFile: 							playMidiFile: openAsMovie: openAsFlash: openTTFFile:							open3DSFile:  openTapeFromFile: openVRMLFile:							viewContents: saveContents: openOn: putUpdate:							removeLineFeeds: renderFile:							#loadCRDictionary: #loadCRDisplayProperties: ).	aMenu 		addServices: services 		for: self fullName		extraLines: #()	! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/1/2002 19:56'!itemsForAnyFile	"what's happen if the Zip stuff is not in the image?"		| services |	self flag: #possibleBug.	services := OrderedCollection new: 5.	services add: self serviceCopyName. 	services add: self serviceRenameFile. 	services add: self serviceDeleteFile.	services add: self serviceCompressFile.	(self class isReaderNamedRegistered: #ZipArchive)		ifTrue: [ services add: self serviceAddFileToZip].				^ services! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/6/2002 21:25'!itemsForFile: fullName	| services suffix|	suffix := (FileDirectory extensionFor: fullName) asLowercase.	services _ OrderedCollection new.	self registeredFileReaderClasses do: [:reader |		reader ifNotNil: [services addAll: (reader fileReaderServicesForFile: fullName suffix: suffix)]].	^services, (self myServicesForFile: fullName suffix: suffix).! !!FileList methodsFor: 'file list menu' stamp: 'sd 1/31/2002 12:08'!itemsForNoFile	| services |	services := OrderedCollection new: 6.	services add: self serviceSortByName.	services add: self serviceSortBySize.	services add: (self serviceSortByDate useLineAfter: true).	(self isFileSelected not and: [self class isReaderNamedRegistered: #FileContentsBrowser])			ifTrue:[ services add: (self serviceBrowseCodeFiles useLineAfter: true)].	services add: self serviceAddNewFile.	services add: self serviceAddNewDirectory.	^ services		! !!FileList methodsFor: 'file list menu' stamp: 'sd 2/6/2002 21:25'!myServicesForFile: fullName suffix: suffix	^(FileStream isSourceFileSuffix: suffix)		ifTrue: [ {self serviceBroadcastUpdate} ]		ifFalse: [ #() ]! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:10'!serviceAddFileToZip	^ SimpleServiceEntry 				provider: (Smalltalk at: #ZipArchive) 				label: 'file to zip'				selector: #addFileToZip:				description: 'add file to zip'! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:11'!serviceAddNewDirectory	^ SimpleServiceEntry 		provider: self 		label: 'new direction' 		selector: #addNewDirectory		description: 'add new directory' 	! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:11'!serviceAddNewFile	^ SimpleServiceEntry provider: self label: 'new file' selector: #addNewFile description: 'add new file'! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:12'!serviceAllFileOptions	^ {SimpleServiceEntry provider: self label: 'more...' selector: #offerAllFileOptions description: 'show all the options available'}! !!FileList methodsFor: 'own services' stamp: 'sd 2/1/2002 19:57'!serviceBroadcastUpdate	^ SimpleServiceEntry 				provider: self 				label: 'broadcast as update'				selector: #putUpdate:				description: 'broadcast file as update'! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:24'!serviceBrowseCodeFiles	self flag: #stef.	"Here we are breaking the registration mechanism by a direct reference to the fileContentsBrowser.	The problem is that service is waiting for a filename and here this specific vicous service is used 	when no file is selected. I think that we should change that"	^  SimpleServiceEntry 		provider: FileContentsBrowser		label: 'browse code files' 		selector: #selectAndBrowseFile:! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:16'!serviceCompressFile	^ SimpleServiceEntry provider: self label: 'compress' selector: #compressFile description: 'compress file'! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:16'!serviceCopyName	^ (SimpleServiceEntry provider: self label: 'copy name to clipboard' selector: #copyName description:'copy name to clipboard' )! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 21:17'!serviceDeleteFile	^ (SimpleServiceEntry provider: self label: 'delete' selector: #deleteFile)			description: 'delete the seleted item'! !!FileList methodsFor: 'own services' stamp: 'sd 2/1/2002 20:48'!serviceGet	^  (SimpleServiceEntry 			provider: self 			label: 'get file' 			selector: #get			description: 'get entire file')			! !!FileList methodsFor: 'own services' stamp: 'sd 2/1/2002 20:50'!serviceGetHex	^  (SimpleServiceEntry 			provider: self 			label: 'view as hex' 			selector: #getHex			description: 'view as hex')			! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:15'!serviceRenameFile	^ (SimpleServiceEntry provider: self label: 'rename' selector: #renameFile description: 'rename file')! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:15'!serviceSortByDate	^  (SimpleServiceEntry 			provider: self 			label: 'by date' 			selector: #sortByDate 			description: 'sort entries by date')		extraSelector: #sortingByDate			! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:14'!serviceSortByName	^ (SimpleServiceEntry 		provider: self label: 'by name' selector: #sortByName 		description: 'sort entries by name')		extraSelector: #sortingByName		! !!FileList methodsFor: 'own services' stamp: 'sd 1/31/2002 22:14'!serviceSortBySize	^  (SimpleServiceEntry 			provider: self 			label: 'by size' 			selector: #sortBySize			description: 'sort entries by size')			extraSelector: #sortingBySize! !!FileList class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:26'!initialize	"FileList initialize"	RecentDirs := OrderedCollection new.	(Smalltalk allClassesImplementing: #fileReaderServicesForFile:suffix:) do: 		[:providerMetaclass |			self registerFileReader: providerMetaclass soleInstance]! !!FileList class methodsFor: 'file reader registration' stamp: 'sd 2/1/2002 21:30'!allRegisteredServices	"self allRegisteredServices"	| col |	col := OrderedCollection new.	self registeredFileReaderClasses do: [:each | col addAll: (each services)].	^ col! !!FileList class methodsFor: 'file reader registration' stamp: 'sd 1/31/2002 21:42'!detectService: aBlock ifNone: anotherBlock	"self detectService: [:each | each selector = #fileIn:] ifNone: [nil]"	^ self allRegisteredServices			detect: aBlock			ifNone: anotherBlock! !!FileList class methodsFor: 'file reader registration' stamp: 'sd 1/31/2002 21:53'!servicesFromSelectorSpecs: symbolArray	"self servicesFromSelectorSpecs: #(fileIn: fileIntoNewChangeSet: browseChangesFile:)"	|res services col | 	col := OrderedCollection new.	services := FileList allRegisteredServices.	symbolArray  do: 		[:sel | 			res := services					detect: [:each | each selector = sel] ifNone: [nil].			res notNil					ifTrue: [col add: res]].	^ col! !!FileListTest methodsFor: 'private' stamp: 'sd 2/1/2002 23:04'!checkIsServiceIsFromDummyTool: service		^ (service instVarNamed: #provider) = DummyToolWorkingWithFileList	 	& service label = 'menu label'		& (service instVarNamed: #selector) = #loadAFileForTheDummyTool:! !!FileListTest methodsFor: 'test' stamp: 'sd 2/6/2002 21:26'!testService	"a stupid test to check that the class returns a service"	"(self selector: #testService) debug"		| service |	service := (DummyToolWorkingWithFileList fileReaderServicesForFile: 'abab.kkk' suffix: 'kkk') first.	self assert: (self checkIsServiceIsFromDummyTool: service).	service := (DummyToolWorkingWithFileList fileReaderServicesForFile: 'zkk.gz' suffix: 'gz').	self assert: service isEmpty! !!FileListTest methodsFor: 'test' stamp: 'sd 2/1/2002 23:04'!testServicesForFileEnding	"(self selector: #testServicesForFileEnding) debug"	self assert: (self checkIsServiceIsFromDummyTool: 						(FileList new itemsForFile: 'aaa.kkk') first).	self assert:  (FileList new itemsForFile: 'aaa.zkk') isEmpty! !!FileStream class methodsFor: 'file reader services' stamp: 'sd 2/6/2002 21:34'!fileReaderServicesForFile: fullName suffix: suffix	^(self isSourceFileSuffix: suffix)		ifTrue: [			{SimpleServiceEntry 				provider: self 				label: 'fileIn'				selector: #fileIn:.			SimpleServiceEntry 				provider: self 				label: 'remove line feeds'				selector: #removeLineFeeds:}]		ifFalse: [#()]! !!FileStream class methodsFor: 'file reader services' stamp: 'sd 2/1/2002 22:27'!serviceFileIn		^ SimpleServiceEntry 				provider: self 				label: 'fileIn'				selector: #fileIn:				description: 'file in'! !!FileStream class methodsFor: 'file reader services' stamp: 'sd 2/1/2002 22:28'!serviceRemoveLineFeeds	^ SimpleServiceEntry 				provider: self 				label: 'remove line feeds'				selector: #removeLineFeeds:					description: 'remove line feeds in file'! !!FileStream class methodsFor: 'file reader services' stamp: 'sd 2/1/2002 22:28'!services	^ Array 			with: self serviceRemoveLineFeeds			with: self serviceFileIn	! !!FlashMorphReader class methodsFor: 'read Flash file' stamp: 'sd 2/6/2002 21:35'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'swf') | (suffix = '*') 		ifTrue: [ self services]		ifFalse: [#()]! !!FlashMorphReader class methodsFor: 'read Flash file' stamp: 'sd 2/1/2002 22:08'!serviceOpenAsFlash	^ SimpleServiceEntry 				provider: self 				label: 'open as Flash'				selector: #openAsFlash:				description: 'open file as flash'! !!FlashMorphReader class methodsFor: 'read Flash file' stamp: 'sd 2/1/2002 22:09'!services	^ Array with: self serviceOpenAsFlash! !!Form class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:35'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'bmp') | (suffix = 'gif') | (suffix = 'jpg') | (suffix ='form') | (suffix = '*') | (suffix = 'png')		ifTrue: [ self services ]		ifFalse: [#()]! !!Form class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:42'!serviceImageAsBackground	^ SimpleServiceEntry 				provider: self 				label: 'open image as background'				selector: #openAsBackground:				description: 'open image as background'	! !!Form class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:42'!serviceImageImports	^	SimpleServiceEntry 				provider: self 				label: 'read image into ImageImports'				selector: #importImage:				description: 'read image into ImageImports'.			! !!Form class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:43'!serviceOpenImageInWindow	^ SimpleServiceEntry 				provider: self 				label: 'open image in a window'				selector: #openImageInWindow:				description: 'open graphic in a window'.! !!Form class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:43'!services	^ Array 		with: self serviceImageImports		with: self serviceOpenImageInWindow		with: self serviceImageAsBackground ! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:35'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'gz') | (suffix = '*') 		ifTrue: [ self services]		ifFalse: [#()]! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:16'!serviceDecompressToFile	^ SimpleServiceEntry 				provider: self 				label: 'decompress to file'				selector: #saveContents:				description: 'decompress to file'! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:15'!serviceViewDecompress	^ SimpleServiceEntry 				provider: self 				label: 'view decompressed'				selector: #viewContents:				description: 'view decompressed' ! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:16'!services	^ Array 		with: self serviceViewDecompress		with: self serviceDecompressToFile	! !!Morph class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:35'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'morph') | (suffix = 'morphs') | (suffix = 'sp') |(suffix = '*')		ifTrue: [			{SimpleServiceEntry 				provider: self 				label: 'load as morph'				selector: #fromFileName:				description: 'load as morph'}]		ifFalse: [#()]! !!Morph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:45'!serviceLoadMorphFromFile	^ SimpleServiceEntry 				provider: self 				label: 'load as morph'				selector: #fromFileName:				description: 'load as morph'! !!Morph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:45'!services	^ Array with: self serviceLoadMorphFromFile! !!ArchiveViewer class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:21'!fileReaderServicesForFile: fullName suffix: suffix 	^ (suffix = 'zip') | (suffix = 'ZIP') | (suffix = '*')		ifTrue: [			{SimpleServiceEntry				provider: self				label: 'open in zip viewer'				selector: #openOn: }]		ifFalse: [suffix = '*'				ifTrue: [					{SimpleServiceEntry						provider: self						label: 'add file to new zip'						selector: #addFileToNewZip: }]				ifFalse: [#()]]! !!ArchiveViewer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:17'!serviceAddToNewZip		^ SimpleServiceEntry					provider: self					label: 'add file to new zip'					selector: #addFileToNewZip: 					description: 'add file to new zip'									! !!ArchiveViewer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:15'!serviceOpenInZipViewer		^ SimpleServiceEntry					provider: self					label: 'open in zip viewer'					selector: #openOn: 					description: 'open in zip viewer'									! !!ArchiveViewer class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 21:18'!services		^ Array 		with: self serviceAddToNewZip		with: self serviceOpenInZipViewer								! !!BookMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:28'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'bo') | (suffix = '*') 		ifTrue: [ Array with: self serviceLoadAsBook]		ifFalse: [#()]! !!BookMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:33'!serviceLoadAsBook	^ SimpleServiceEntry 			provider: self 			label: 'load as book'			selector: #openFromFile:			description: 'open as bookmorph'! !!BookMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 21:33'!services	^ Array with: self serviceLoadAsBook! !!EventRecorderMorph class methodsFor: 'instance creation' stamp: 'sd 2/6/2002 21:31'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'tape') | (suffix = '*') 		ifTrue: [ self services]		ifFalse: [#()]! !!EventRecorderMorph class methodsFor: 'instance creation' stamp: 'sd 2/6/2002 21:31'!services	^{SimpleServiceEntry 			provider: self 			label: 'open for playback'			selector: #openTapeFromFile:.}! !!MoviePlayerMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:36'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'movie') | (suffix = '*')		ifTrue: [ self services]		ifFalse: [#()]! !!MoviePlayerMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:17'!serviceOpenAsMovie	^ SimpleServiceEntry 				provider: self 				label: 'open as movie'				selector: #openAsMovie:					description: 'open file as movie'! !!MoviePlayerMorph class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:18'!services	^ Array with: self serviceOpenAsMovie	! !!PluggableFileList methodsFor: 'file list menu' stamp: 'sd 2/1/2002 21:29'!fileSelectedMenu: aMenu	| firstItems secondItems thirdItems n1 n2 n3 services |	firstItems _ self itemsForFile: self fullName asLowercase.	secondItems _ self itemsForAnyFile.	thirdItems _ self itemsForNoFile.	n1 _ firstItems size.	n2 _ n1 + secondItems size.	n3 _ n2 + thirdItems size.	services _ firstItems, secondItems, thirdItems, 			(SimpleServiceEntry provider: self label: 'more...' selector: #offerAllFileOptions).	^ aMenu 		addServices: services 		for: self fullName		extraLines: (Array with: n1 with: n2 with: n3)! !!ProjectViewMorph class methodsFor: 'project window creation' stamp: 'sd 2/6/2002 21:36'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'extseg') | (suffix = 'project') | (suffix = 'pr') |	  (suffix = 'morph') | (suffix = 'morphs') | (suffix = 'sp') |(suffix = '*')		ifTrue: [			self services]		ifFalse: [#()]! !!ProjectViewMorph class methodsFor: 'project window creation' stamp: 'sd 2/1/2002 22:01'!serviceOpenProjectFromFile	^ SimpleServiceEntry 				provider: self 				label: 'load as project'				selector: #openFromFile:				description: 'open project from file'		! !!ProjectViewMorph class methodsFor: 'project window creation' stamp: 'sd 2/1/2002 22:01'!services	^ Array with: self serviceOpenProjectFromFile	! !!Scamper class methodsFor: 'instance creation' stamp: 'sd 2/6/2002 21:36'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'htm') | (suffix = 'html') 		ifTrue: [self services]		ifFalse: [#()]! !!Scamper class methodsFor: 'instance creation' stamp: 'sd 2/1/2002 22:19'!serviceOpenInWebBrowser	^ SimpleServiceEntry 			provider: self 			label: 'open in web browser'			selector: #openFile:			description: 'open in web browser'! !!Scamper class methodsFor: 'instance creation' stamp: 'sd 2/1/2002 22:20'!services	^ Array with: self serviceOpenInWebBrowser! !!ScorePlayerMorph class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:37'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'mid') | (suffix = '*') 		ifTrue: [ self services]		ifFalse: [#()]! !!ScorePlayerMorph class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:21'!servicePlayMidiFile	^ SimpleServiceEntry 				provider: self 				label: 'play midi file'				selector: #playMidiFile:				description: 'play midi file'! !!ScorePlayerMorph class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:21'!services	^ Array with: self servicePlayMidiFile	! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:03'!description	"may be used for balloon or other"	^ description! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:03'!description: aString	"may be used for balloon or other"	description := aString! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:10'!extraSelector	"normally should not be used directly"	^stateSelector! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:11'!extraSelector: aSymbol	stateSelector := aSymbol! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:38'!provider	^ provider! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:42'!provider: anObject label: aString selector: aSymbol	"basic initialization message"		provider := anObject.	label :=  aString.	selector :=  aSymbol.	stateSelector := #none.	description := ''! !!SimpleServiceEntry methodsFor: 'accessing' stamp: 'sd 1/31/2002 21:09'!selector	"normally should not be used directly"	^selector! !!SimpleServiceEntry methodsFor: 'performing service' stamp: 'sd 1/31/2002 22:06'!provider: anObject label: aString selector: aSymbol description: anotherString	"basic initialization message"		self provider: anObject label:  aString selector: aSymbol.	stateSelector := #none.	description := anotherString! !!SimpleServiceEntry methodsFor: 'extra' stamp: 'sd 1/31/2002 21:06'!performExtraFor: anObject	"carry out the extra service I provide"	"the stateSelector can be used to ask state of the provider to be reflected in button or other"	^stateSelector numArgs = 0		ifTrue: [provider perform: stateSelector]		ifFalse: [provider perform: stateSelector with: anObject]! !!SimpleServiceEntry methodsFor: 'extra' stamp: 'sd 1/31/2002 21:08'!requestExtraSelector	"send me this message to ask me to perform secondary service"	^#performExtraFor:! !!SimpleServiceEntry methodsFor: 'printing' stamp: 'sd 1/31/2002 21:38'!printOn: aStream	aStream nextPutAll: 'Service: ('.	self provider notNil		ifTrue: [aStream nextPutAll: provider printString].	aStream nextPutAll: ' --- '. 	self selector notNil		ifTrue: [aStream nextPutAll: selector asString]! !!SimpleServiceEntry class methodsFor: 'instance creation' stamp: 'sd 1/31/2002 22:05'!provider: anObject label: aString selector: aSymbol description: anotherString	^self new provider: anObject label: aString selector: aSymbol description: anotherString! !!SwikiPage class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:37'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = '*') ifTrue: [ self services]		ifFalse: [#()]! !!SwikiPage class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:13'!serviceGenerateHTML	^ SimpleServiceEntry 			provider: self 			label: 'generate HTML'			selector: #renderFile:			description: 'generate HTML'! !!SwikiPage class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:13'!services	^ Array with: self serviceGenerateHTML	! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/6/2002 21:37'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'fnt')		ifTrue: [			self services]		ifFalse: [#()]! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 19:32'!initialize	"self initialize"	FileList registerFileReader: self! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 19:31'!openTTFFile: fullName 	(TTFontReader parseFileNamed: fullName) asMorph open! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:02'!serviceOpenTrueTypeFont	^ SimpleServiceEntry 				provider: self 				label: 'open true type font'				selector: #openTTFFile:				description: 'open true type font'! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 22:03'!services	^ Array with: self serviceOpenTrueTypeFont! !!TTFontReader class methodsFor: 'class initialization' stamp: 'sd 2/1/2002 19:29'!unload	FileList unregisterFileReader: self ! !!Wonderland class methodsFor: 'fileIn/Out' stamp: 'sd 2/6/2002 21:37'!fileReaderServicesForFile: fullName suffix: suffix	^(suffix = 'wrl') | (suffix = '*') 		ifTrue: [ Array with: self serviceOpenInWonderland]		ifFalse: [(suffix = 'mdl') | (suffix = '*')					ifTrue: [ Array with: self serviceOpenModelInEditor]					ifFalse: [#()]]! !!Wonderland class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:10'!serviceOpenInWonderland	^ SimpleServiceEntry 			provider: self 			label: 'open in Wonderland'			selector: #openVRMLFile:			description: 'open in Wonderland' ! !!Wonderland class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:10'!serviceOpenModelInEditor		^ SimpleServiceEntry 		provider: self 		label: 'open model in editor'		selector: #openModelIntoAlice:		description: 'open model in editor'! !!Wonderland class methodsFor: 'fileIn/Out' stamp: 'sd 2/1/2002 22:11'!services	^ Array 		with: self serviceOpenModelInEditor		with: self serviceOpenInWonderland	! !Wonderland class removeSelector: #fileReaderServicesForSuffix:!TTFontReader initialize!SwikiPage class removeSelector: #fileReaderServicesForSuffix:!ScorePlayerMorph class removeSelector: #fileReaderServicesForSuffix:!Scamper class removeSelector: #fileReaderServicesForSuffix:!ProjectViewMorph class removeSelector: #fileReaderServicesForSuffix:!MoviePlayerMorph class removeSelector: #fileReaderServicesForSuffix:!EventRecorderMorph class removeSelector: #fileReaderServicesForSuffix:!BookMorph class removeSelector: #fileReaderServicesForSuffix:!ArchiveViewer class removeSelector: #fileReaderServicesForSuffix:!Morph class removeSelector: #fileReaderServicesForSuffix:!GZipReadStream class removeSelector: #fileReaderServicesForSuffix:!Form class removeSelector: #fileReaderServicesForSuffix:!FlashMorphReader class removeSelector: #fileReaderServicesForSuffix:!FileStream class removeSelector: #fileReaderServicesForSuffix:!FileList initialize!FileList removeSelector: #buttonsWithArgsSelector!FileList removeSelector: #fileNameArray!FileList removeSelector: #itemsForFileEnding:!FileList removeSelector: #myServicesForFileEnding:!FileList removeSelector: #selectedFile!FileList removeSelector: #selectorWithArgsSelector!FileContentsBrowser class removeSelector: #fileReaderServicesForSuffix:!DummyToolWorkingWithFileList class removeSelector: #fileReaderServicesForSuffix:!DummyToolWorkingWithFileList initialize!ChangeSorter class removeSelector: #fileReaderServicesForSuffix:!ChangeList class removeSelector: #fileReaderServicesForSuffix:!CRRecognizer class removeSelector: #fileReaderServicesForSuffix:!B3DScene class removeSelector: #fileReaderServicesForSuffix:!