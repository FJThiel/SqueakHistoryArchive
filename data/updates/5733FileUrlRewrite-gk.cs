'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 13 February 2004 at 12:03:38 am'!"Change Set:		FileUrlRewriteDate:			12 February 2004Author:			G�ran KrampeNote: This changeset may have side effects, we simply need to test drive it (Filelist with FTP etc).A problem that cropped up with Scamper was a fix Markus made in update 5414. Markus made FileUrl>>pathForFile and #pathForDirectory to prepend the pathName delimiter if the FileUrl is absolute.The refinement is to avoid prepending if the file url has a path that starts with a DOS style drive letter, since such paths are absolute per definition.This triggered a BIG make-over of FileUrl, where it was made much more proper by using 'file://' as the prefix instead of 'file:'. According to the RFCs a file URL can not have relative file paths, the isAbsolute etc still works but the result sending toText (which I didn't bother to change even though it has a strange name) will be proper and does not reflect isAbsolute.I have taken care of adding 'smarts' on how FileUrl creates itself given a non valid String representation - it tries to be quite helpful. This is partly based on what Mozilla does in this regard.Now it is IMHO much improved, seems to work, has a bunch of new tests (posted separately), and Scamper works fine from filelist on DOS drives."!Url subclass: #FileUrl	instanceVariableNames: 'path isAbsolute host '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Url'!!FileUrl commentStamp: 'gk 2/10/2004 10:56' prior: 0!This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txtHere is the relevant part of the RFC:3.10 FILES   The file URL scheme is used to designate files accessible on a   particular host computer. This scheme, unlike most other URL schemes,   does not designate a resource that is universally accessible over the   Internet.   A file URL takes the form:       file://<host>/<path>   where <host> is the fully qualified domain name of the system on   which the <path> is accessible, and <path> is a hierarchical   directory path of the form <directory>/<directory>/.../<name>.   For example, a VMS file     DISK$USER:[MY.NOTES]NOTE123456.TXT   might become     <URL:file://vms.host.edu/disk$user/my/notes/note12345.txt>   As a special case, <host> can be the string "localhost" or the empty   string; this is interpreted as `the machine from which the URL is   being interpreted'.   The file URL scheme is unusual in that it does not specify an   Internet protocol or access method for such files; as such, its   utility in network protocols between hosts is limited.From the above we can conclude that the RFC says that the <path> part never starts or ends with a slash and is always absolute. If the last name can be a directory instead of a file is not specified clearly.The path is stored as a SequenceableCollection of path parts.Notes regarding non RFC features in this class:- If the last path part is the empty string, then the FileUrl is referring to a directory. This is also shown when sent #toText with a trailing slash.- The FileUrl has an attribute isAbsolute which refers to if the path should be considered absolute or relative to the current directory. This distinction is not visible in the String representation of FileUrl, since the RFC does not have that.- Fragment is supported (kept for historical reasons)!!FileDirectory methodsFor: 'file name utilities' stamp: 'gk 2/10/2004 13:22'!asUrl	"Convert my path into a file:// type url - a FileUrl."		^FileUrl pathParts: (self pathParts copyWith: '')! !!FileDirectory methodsFor: 'file name utilities' stamp: 'gk 2/10/2004 13:23'!url	"Convert my path into a file:// type url String."		^self asUrl toText! !!FileStream methodsFor: 'file accessing' stamp: 'gk 2/10/2004 13:21'!asUrl	"Convert my path into a file:// type url - a FileUrl."		^FileUrl pathParts: (self directory pathParts copyWith: self localName)! !!FileStream methodsFor: 'file accessing' stamp: 'gk 2/10/2004 13:21'!url	"Convert my path into a file:// type url String."		^self asUrl toText! !!FileUrl methodsFor: 'printing' stamp: 'gk 2/10/2004 10:49'!toText	"Return the FileUrl according to RFC1738 plus supporting fragments:		'file://<host>/<path>#<fragment>'	Note that <host> being '' is equivalent to 'localhost'.	Note: The pathString can not start with a leading $/	to indicate an 'absolute' file path.	This is not according to RFC1738 where the path should have	no leading or trailing slashes, and always	be considered absolute relative to the filesystem."	^String streamContents: [:s |		s nextPutAll: self schemeName, '://'.		host ifNotNil: [s nextPutAll: host].		s nextPut: $/; nextPutAll: self pathString.		fragment ifNotNil: [ s nextPut: $#; nextPutAll: fragment encodeForHTTP ]]! !!FileUrl methodsFor: 'testing' stamp: 'gk 2/9/2004 20:32'!firstPartIsDriveLetter	"Return true if the first part of the path is a letter	followed by a $: like 'C:' "		| firstPart |	path isEmpty ifTrue: [^false].	firstPart _ path first.	^firstPart size = 2 and: [		firstPart first isLetter			and: [firstPart last = $:]]! !!FileUrl methodsFor: 'paths' stamp: 'gk 2/10/2004 00:19'!pathDirString	"Path to directory as url, using slash as delimiter.	Filename is left out."	^String streamContents: [ :s |		isAbsolute ifTrue: [ s nextPut: $/ ].		1 to: self path size - 1 do: [ :ii |			s nextPutAll: (path at: ii); nextPut: $/]]! !!FileUrl methodsFor: 'paths' stamp: 'gk 2/10/2004 00:19'!pathForDirectory	"Path using local file system's delimiter.  $\ or $:	DOS paths with drive letters should not	be prepended with a pathNameDelimiter even though	they are absolute. Filename is left out."	^String streamContents: [ :s |		(self isAbsolute and: [self firstPartIsDriveLetter not])			ifTrue: [ s nextPut: $/ ].		1 to: self path size - 1 do: [ :ii |			s nextPutAll: (path at: ii); nextPut: FileDirectory default pathNameDelimiter]]! !!FileUrl methodsFor: 'paths' stamp: 'gk 2/9/2004 20:24'!pathForFile	"Path using local file system's delimiter.  $\ or $:	DOS paths with drive letters should not	be prepended with a pathNameDelimiter even though	they are absolute."		| first |	^String streamContents: [ :s |		first _ self isAbsolute and: [self firstPartIsDriveLetter not].		self path do: [ :p |			first ifTrue: [ s nextPut: FileDirectory default pathNameDelimiter ].			first _ true.			s nextPutAll: p ] ]! !!FileUrl methodsFor: 'paths' stamp: 'gk 2/10/2004 10:22'!pathString	"Path as it appears in a URL with $/ as delimiter."		| first |	^String streamContents: [ :s |		"isAbsolute ifTrue:[ s nextPut: $/ ]."		first _ true.		self path do: [ :p |			first ifFalse: [ s nextPut: $/ ].			first _ false.			s nextPutAll: p encodeForHTTP ] ]! !!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 10:16'!host	"Return the host name, either 'localhost', '', or a fully qualified domain name."		^host ifNil: ['']! !!FileUrl methodsFor: 'accessing' stamp: 'gk 2/12/2004 16:22'!host: hostName	"Set the host name, either 'localhost', '', or a fully qualified domain name."		host _ hostName! !!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 10:50'!isAbsolute: aBoolean	isAbsolute _ aBoolean! !!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 00:15'!path	"Return an ordered collection of the path elements."		^path! !!FileUrl methodsFor: 'accessing' stamp: 'gk 2/10/2004 00:16'!path: anArray	"Set the collection of path elements."		path _ anArray! !!FileUrl methodsFor: 'downloading' stamp: 'gk 2/10/2004 13:06'!default	"Use the default local Squeak file directory."		| local |	local _ self class pathParts: (FileDirectory default pathParts), #('') isAbsolute: true.	self privateInitializeFromText: self pathString relativeTo: local.		"sets absolute also"! !!FileUrl methodsFor: 'downloading' stamp: 'gk 2/10/2004 00:50'!retrieveContents	| file pathString s type entries |	pathString _ self pathForFile.	file _ [FileStream readOnlyFileNamed: pathString] 			on: FileDoesNotExistException do:[:ex| ex return: nil].	file ifNotNil: [		type _ file mimeTypes.		type ifNotNil:[type _ type first].		type ifNil:[MIMEDocument guessTypeFromName: self path last].		^MIMELocalFileDocument 			contentType: type			contentStream: file].	"see if it's a directory..."	entries _ [(FileDirectory on: pathString) entries] 				on: InvalidDirectoryError do:[:ex| ex return: nil].	entries ifNil:[^nil].	s _ WriteStream on: String new.	(pathString endsWith: '/') ifFalse: [ pathString _ pathString, '/' ].	s nextPutAll: '<title>Directory Listing for ', pathString, '</title>'.	s nextPutAll: '<h1>Directory Listing for ', pathString, '</h1>'.	s nextPutAll: '<ul>'.	s cr.	entries do: [ :entry |		s nextPutAll: '<li><a href="'.		s nextPutAll: entry name.		s nextPutAll: '">'.		s nextPutAll: entry name.		s nextPutAll: '</a>'.		s cr. ].	s nextPutAll: '</ul>'.	^MIMEDocument  contentType: 'text/html'  content: s contents  url: ('file://', pathString)! !!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/10/2004 13:05'!host: aHostString pathParts: aCollection isAbsolute: aBoolean	host _ aHostString.	path _ aCollection.	isAbsolute _ aBoolean! !!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/12/2004 16:01'!initializeFromPathString: aPathString	"<aPathString> is a file path as a String.	We construct a path collection using various heuristics."	| pathString hasDriveLetter |	pathString _ aPathString.	pathString isEmpty ifTrue: [pathString _ '/'].	path _ (pathString findTokens: '/') collect: [:token | token unescapePercents].	"A path like 'C:' refers in practice to 'c:/'"	((pathString endsWith: '/') or:		[(hasDriveLetter _ self firstPartIsDriveLetter) and: [path size = 1]])			ifTrue: [path add: ''].	"Decide if we are absolute by checking for leading $/ or	beginning with drive letter. Smarts for other OSes?"	self isAbsolute: ((pathString beginsWith: '/')						or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])! !!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/10/2004 13:04'!pathParts: aCollection isAbsolute: aBoolean	^self host: nil pathParts: aCollection isAbsolute: aBoolean! !!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/12/2004 16:11'!privateInitializeFromText: aString	"Calculate host and path from a file URL in String format.	Some malformed formats are allowed and interpreted by guessing."	| schemeName pathString bare hasDriveLetter stream char i |	bare _ aString withBlanksTrimmed.	schemeName _ Url schemeNameForString: bare.	(schemeName isNil or: [schemeName ~= self schemeName])		ifTrue: [			host _ ''.			pathString _ bare]		ifFalse: [			"First remove schemeName and colon"			bare _ bare copyFrom: (schemeName size + 2) to: bare size.			"A proper file URL then has two slashes before host,			A malformed URL is interpreted as using syntax file:<path>."			(bare beginsWith: '//')				ifTrue: [i _ bare indexOf: $/ startingAt: 3.						i=0 ifTrue: [								host _ bare copyFrom: 3 to: bare size.								pathString _ '']							ifFalse: [								host _ bare copyFrom: 3 to: i-1.								pathString _ bare copyFrom: host size + 3 to: bare size]]				ifFalse: [host _ ''.						pathString _ bare]].	self initializeFromPathString: pathString! !!FileUrl methodsFor: 'private-initialization' stamp: 'gk 2/12/2004 16:29'!privateInitializeFromText: pathString relativeTo: aUrl	"<pathString> should be a filesystem path.	This url is adjusted to be aUrl + the path."	| bare newPath |	self host: aUrl host.	self initializeFromPathString: pathString.	self isAbsolute: aUrl isAbsolute.	newPath _ aUrl path copy.	newPath removeLast.	"empty string that says its a directory"	path do: [ :token |		((token ~= '..') and: [token ~= '.']) ifTrue: [ 			newPath addLast: token unescapePercents ].		token = '..' ifTrue: [ 			newPath isEmpty ifFalse: [ 				newPath last = '..' ifFalse: [ newPath removeLast ] ] ].		"token = '.' do nothing" ].	path _ newPath	! !!FileUrl methodsFor: 'classification' stamp: 'gk 2/10/2004 10:34'!scheme	^self class schemeName! !!FileUrl methodsFor: 'classification' stamp: 'gk 2/10/2004 10:34'!schemeName	^self class schemeName! !!FileUrl methodsFor: 'copying' stamp: 'gk 2/10/2004 09:52'!copy	"Be sure not to share the path with the copy"	^(self clone) path: path copy! !!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 12:16'!absoluteFromText: aString	"Method that can be called explicitly to create a FileUrl."	^self new privateInitializeFromText: aString! !!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:04'!host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean	"Create a FileUrl."	^self new host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:10'!pathParts: aCollectionOfPathParts	"Create a FileUrl."	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: true! !!FileUrl class methodsFor: 'instance creation' stamp: 'gk 2/10/2004 13:06'!pathParts: aCollectionOfPathParts isAbsolute: aBoolean	"Create a FileUrl."	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !!FileUrl class methodsFor: 'constants' stamp: 'gk 2/10/2004 10:33'!schemeName	^'file'! !!Project methodsFor: 'file in/out' stamp: 'gk 2/10/2004 13:27'!storeOnServerInnards	"Save to disk as an Export Segment.  Then put that file on the server I came from, as a new version.  Version is literal piece of file name.  Mime encoded and http encoded."	| resp newName primaryServerDirectory serverVersionPair localDirectory localVersionPair myVersionNumber warning maxNumber suppliedPassword oldResourceUrl |	self assureIntegerVersion.	"Find out what version"	primaryServerDirectory _ self primaryServerIfNil: [		(primaryServerDirectory _ self findAFolderToStoreProjectIn) ifNil: [^self].		oldResourceUrl _ self resourceUrl.		primaryServerDirectory == #localOnly ifTrue: [			self storeNewPrimaryURL: FileDirectory default url.			nil		] ifFalse: [			self storeNewPrimaryURL: primaryServerDirectory downloadUrl.			primaryServerDirectory		].	].	localDirectory _ self squeakletDirectory.	serverVersionPair _ self class mostRecent: self name onServer: primaryServerDirectory.	localVersionPair _ self class mostRecent: self name onServer: localDirectory.	maxNumber _ myVersionNumber _ self currentVersionNumber.	ProgressNotification signal: '2:versionsDetected'.	warning _ ''.	myVersionNumber < serverVersionPair second ifTrue: [		warning _ warning,'\There are newer version(s) on the server'.		maxNumber _ maxNumber max: serverVersionPair second.	].	myVersionNumber < localVersionPair second ifTrue: [		warning _ warning,'\There are newer version(s) in the local directory'.		maxNumber _ maxNumber max: localVersionPair second.	].	"8 Nov 2000 - only check on the first attempt to publish"	myVersionNumber = 0 ifTrue: [		warning isEmpty ifFalse: [			myVersionNumber = 0 ifTrue: [				warning _ warning,'\THIS PROJECT HAS NEVER BEEN SAVED'			].			warning _ 'WARNING', '\Project: ',self name,warning.			resp _ (PopUpMenu labels: 'Store anyway\Cancel' withCRs) startUpWithCaption: 				(warning, '\Please cancel, rename this project, and see what is there.') withCRs.				resp ~= 1 ifTrue: [^ nil]		].	].	version _ self bumpVersion: maxNumber.	oldResourceUrl		ifNotNil: [self resourceManager adjustToNewServer: self resourceUrl from: oldResourceUrl].	"write locally - now zipped automatically"	newName _ self versionedFileName.	lastSavedAtSeconds _ Time totalSeconds.	self exportSegmentFileName: newName directory: localDirectory.	(localDirectory readOnlyFileNamed: newName) setFileTypeToObject; close.	ProgressNotification signal: '4:localSaveComplete'.	"3 is deep in export logic"	primaryServerDirectory ifNotNil: [		suppliedPassword _ ''.		Preferences passwordsOnPublish ifTrue: [			suppliedPassword _ FillInTheBlank requestPassword: 'Project password'		].		[		primaryServerDirectory			writeProject: self			inFileNamed: newName			fromDirectory: localDirectory.		] on: ProjectPasswordNotification do: [ :ex |			ex resume: (suppliedPassword ifNil: [''])		].	].	ProgressNotification signal: '9999 save complete'.	"Later, store with same name on secondary servers.  Still can be race conditions.  All machines will go through the server list in the same order."	"2 to: servers size do: [:aServer | aServer putFile: local named: newName]."! !FileUrl class removeSelector: #host:path:isAbsolute:!!FileUrl class reorganize!('instance creation' absoluteFromText: host:pathParts:isAbsolute: pathParts: pathParts:isAbsolute:)('constants' schemeName)!FileUrl removeSelector: #asString!FileUrl removeSelector: #host:path:isAbsolute:!FileUrl removeSelector: #path:isAbsolute:!Url subclass: #FileUrl	instanceVariableNames: 'host path isAbsolute'	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Url'!!FileUrl reorganize!('printing' toText)('testing' firstPartIsDriveLetter)('paths' pathDirString pathForDirectory pathForFile pathString)('accessing' host host: isAbsolute isAbsolute: path path:)('downloading' default hasContents retrieveContents)('private-initialization' host:pathParts:isAbsolute: initializeFromPathString: pathParts:isAbsolute: privateInitializeFromText: privateInitializeFromText:relativeTo:)('classification' scheme schemeName)('copying' copy)!