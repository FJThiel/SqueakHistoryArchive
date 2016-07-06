'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 28 February 2001 update 3950] on 24 April 2001 at 5:10:45 pm'!"Change Set:		DblClickProjects-arDate:			24 April 2001Author:			Andreas RaabThe CS unifies the mechanisms for starting up with some project into the (now renamed) ProjectLauncher. The ProjectLauncher will load a project when double clicking on it. Works nicely on Windows but needs to be tested on Mac since I'm uncertain if the VM knows where to find the corresponding image file."!MIMEDocument subclass: #MIMELocalFileDocument	instanceVariableNames: 'contentStream '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Url'!!MIMELocalFileDocument commentStamp: 'ar 4/24/2001 16:27' prior: 0!For local files, we do not read the entire contents unless we absolutely have to.!Smalltalk renameClassNamed: #PluginLauncher as: #ProjectLauncher!!AutoStart class methodsFor: 'updating' stamp: 'ar 4/24/2001 15:59'!checkForUpdates	| availableUpdate |	World ifNotNil: [World install].	HTTPClient determineIfRunningInBrowser.	HTTPClient isRunningInBrowser		ifFalse: [^self processUpdates].	availableUpdate _ (AbstractLauncher extractParameters		at: 'UPDATE'		ifAbsent: [''] ) asInteger.	availableUpdate		ifNil: [^false].	^SystemVersion checkAndApplyUpdates: availableUpdate! !!AutoStart class methodsFor: 'updating' stamp: 'ar 4/24/2001 15:59'!processUpdates	"Process update files from a well-known update server.  This method is called at system startup time,   Only if the preference #updateFromServerAtStartup is true is the actual update processing undertaken automatically"	| choice |	(Preferences valueOfFlag: #updateFromServerAtStartup) ifTrue:		[choice _ (PopUpMenu labels: 'Yes, Update\No, Not now' withCRs)			startUpWithCaption: 'Shall I look for new code\updates on the server?' withCRs.		choice = 1 ifTrue: [Utilities updateFromServer]].	^false! !!FileDirectory methodsFor: 'file operations' stamp: 'ar 4/24/2001 16:31'!mimeTypesFor: fileName	"Return a list of MIME types applicable to the receiver. This default implementation uses the file name extension to figure out what we're looking at but specific subclasses may use other means of figuring out what the type of some file is. Some systems like the macintosh use meta data on the file to indicate data type"	| idx ext dot |	ext _ ''.	dot _ self class extensionDelimiter.	idx _ (self fullNameFor: fileName) findLast: [:ch| ch = dot].	idx = 0 ifFalse:[ext _ fileName copyFrom: idx+1 to: fileName size].	^StandardMIMEMappings at: ext asLowercase ifAbsent:[nil]! !!MIMELocalFileDocument methodsFor: 'accessing' stamp: 'ar 4/24/2001 16:28'!content	^content ifNil:[content _ contentStream contentsOfEntireFile].! !!MIMELocalFileDocument methodsFor: 'accessing' stamp: 'ar 4/24/2001 16:27'!contentStream	^contentStream ifNil:[super contentStream]! !!MIMELocalFileDocument methodsFor: 'accessing' stamp: 'ar 4/24/2001 16:27'!contentStream: aFileStream	contentStream _ aFileStream.	content _ nil.! !!MIMELocalFileDocument class methodsFor: 'instance creation' stamp: 'ar 4/24/2001 16:31'!contentType: aString contentStream: aStream	^(self contentType: aString content: nil) contentStream: aStream! !!ProjectLauncher methodsFor: 'running' stamp: 'ar 4/24/2001 17:09'!startUp	| scriptName loader isUrl |	World ifNotNil: [World install].	HTTPClient determineIfRunningInBrowser.	HTTPClient isRunningInBrowser ifTrue:[		self setupFromParameters.		scriptName _ self parameterAt: 'src'.		CodeLoader defaultBaseURL: (self parameterAt: 'Base').	] ifFalse:[		scriptName _ (Smalltalk getSystemAttribute: 2) ifNil:[''].		scriptName isEmpty ifFalse:[			"figure out if script name is a URL by itself"			isUrl _ (scriptName asLowercase beginsWith:'http://') or:[					(scriptName asLowercase beginsWith:'file://') or:[					(scriptName asLowercase beginsWith:'ftp://')]].			isUrl ifFalse:[scriptName _ 'file://',scriptName]].	].	scriptName isEmptyOrNil ifTrue:[^self].	self setupFlaps.	loader _ CodeLoader new.	loader loadSourceFiles: (Array with: scriptName).	(scriptName asLowercase endsWith: '.pr') 		ifTrue:[self installProjectFrom: loader]		ifFalse:[loader installSourceFiles].! !!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'ar 4/24/2001 16:38'!readDocumentFile	"No longer used. Everything is now done in ProjectLauncher."	StartupStamp _ '----STARTUP----', Time dateAndTimeNow printString, ' as ', Smalltalk imageName.! !!FileUrl methodsFor: 'downloading' stamp: 'ar 4/24/2001 16:32'!retrieveContents	| file pathString s dir type |		pathString _ self pathForFile.	path last size > 0 ifTrue: [		file _ [FileStream readOnlyFileNamed: pathString] 				on: FileDoesNotExistException do:[:ex| nil].		file ifNotNil: [			type _ file mimeTypes.			type ifNotNil:[type _ type first].			type ifNil:[MIMEDocument guessTypeFromName: self path last].			^MIMELocalFileDocument 				contentType: type				contentStream: file ] ].	"assume it's a directory..."	s _ WriteStream on: String new.	dir _ FileDirectory on: pathString.	(pathString endsWith: '/') ifFalse: [ pathString _ pathString, '/' ].	s nextPutAll: '<title>Directory Listing for ', pathString, '</title>'.	s nextPutAll: '<h1>Directory Listing for ', pathString, '</h1>'.	s nextPutAll: '<ul>'.	s cr.	dir entries do: [ :entry |		s nextPutAll: '<li><a href="'.		s nextPutAll: entry name.		s nextPutAll: '">'.		s nextPutAll: entry name.		s nextPutAll: '</a>'.		s cr. ].	s nextPutAll: '</ul>'.	^MIMEDocument  contentType: 'text/html'  content: s contents  url: ('file:', pathString)! !SystemDictionary removeSelector: #processUpdates!