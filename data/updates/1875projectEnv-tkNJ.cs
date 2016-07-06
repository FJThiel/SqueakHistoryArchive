'From Squeak2.8alpha of 13 January 2000 [latest update: #1873] on 25 February 2000 at 6:20:29 pm'!Model subclass: #Project	instanceVariableNames: 'world changeSet transcript parentProject previousProject displayDepth activeProcess exitFlag viewSize thumbnail nextProject guards projectParameters isolatedHead inForce classArray methodDictArray orgArray version urlList environment '	classVariableNames: 'CurrentProject UsingIsolation '	poolDictionaries: ''	category: 'System-Support'!!FileDirectory methodsFor: 'file operations' stamp: 'tk 2/25/2000 15:30'!putFile: file1 named: destinationFileName	"Copy the contents of the existing fileStream into the file destinationFileName in this directory.  fileStream can be anywhere in the fileSystem."	| file2 buffer |	file1 binary.	(file2 _ self newFileNamed: destinationFileName) ifNil: [^ false].	file2 binary.	buffer _ String new: 50000.	[file1 atEnd] whileFalse:		[file2 nextPutAll: (file1 nextInto: buffer)].	file1 close.	file2 close.	^ true! !!FileDirectory methodsFor: 'file name utilities' stamp: 'tk 2/25/2000 13:40'!realUrl	^ self url! !!FileDirectory methodsFor: 'file name utilities' stamp: 'tk 2/25/2000 13:40'!url	"Convert my path into a file:// type url.  Use slash instead of the local delimiter (:), and convert odd characters to %20 notation."	"If slash (/) is not the file system delimiter, encode slashes before converting."	| list |	list _ self pathParts.	^ String streamContents: [:strm |		strm nextPutAll: 'file:/'.		list do: [:each | strm nextPut: $/; nextPutAll: each encodeForHTTP].		strm nextPut: $/]! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 2/24/2000 13:47'!rootsIncludingPlayers	"Return a new roots array with more objects.  (Caller should store into rootArray.) Player (non-systemDefined) gets its class and metaclass put into the Roots array.  Then ask for the segment again."| extras havePresenter players morphs env |userRootCnt ifNil: [userRootCnt _ arrayOfRoots size].extras _ OrderedCollection new.arrayOfRoots do: [:root | 	(root isKindOf: Presenter) ifTrue: [havePresenter _ root].	(root isKindOf: PasteUpMorph) ifTrue: [			root isWorldMorph ifTrue: [havePresenter _ root presenter]].	(root isKindOf: Project) ifTrue: [havePresenter _ root world presenter]].havePresenter ifNotNil: [	havePresenter flushPlayerListCache.		"old and outside guys"	morphs _ IdentitySet new: 400.	havePresenter associatedMorph allMorphsAndBookPagesInto: morphs.	players _ (morphs select: [:m | m player ~~ nil] 				thenCollect: [:m | m player]) asArray.	players _ players select: [:ap | (arrayOfRoots includes: ap class) not		& (ap class isSystemDefined not)].	extras addAll: (players collect: [:each | each class]).	(env _ havePresenter world project environment) ifNil: [		extras addAll: (players collect: [:each | each class class])].	].extras isEmpty ifTrue: [^ nil].	"no change"env 	ifNil: ["old pre-environment"		havePresenter _ players _ morphs _ nil.		^ arrayOfRoots, extras]	"will contain multiples of some, but reduced later"	ifNotNil: [		(env includesKey: #Object) ifTrue: [self error: 'only look in local env, not up chain'].			"If get error, use a message other than includesKey:"		extras do: [:cls | 			(env includesKey: cls name) ifFalse: [				env declare: cls name from: Smalltalk]].		havePresenter _ players _ morphs _ env _ nil.		^ arrayOfRoots, extras	"still need in roots in case outside pointers"		]! !!Project commentStamp: 'tk 2/25/2000 15:51' prior: 0!A Project stores the state of a complete Squeak desktop, including the windows, and the currently active changeSet.  A project knows who its parent project is.  When you change projects, whether by entering or exiting, the screen state of the project being exited is saved in that project.A project is retained by its view in the parent world.  It is effectively named by the name of its changeSet, which can be changed either by renaming in a changeSorter, or by editing the label of its view from the parent project.As the site of major context switch, Projects are the locus of swapping between the old MVC and the new Morphic worlds.  The distinction is based on whether the variable 'world' contains a WorldMorph or a ControlManager.-----Projects may be stored on the disk in external format.  (Project named: 'xxx') exportSegment.Projects may be loaded from a server and stored back.  Storing on a server never overwrites.  It always makes a new version.A project remembers the url of where it lives (in urlList.  The list is length one, for now).The url may point to a local disk instead of a server.All projects that the user looks at, are cached in the Squeaklet folder.  Sorted by server.  The cache holds the most recent version only.For now, control of saving an reloading is from the thumbnail that owns the project.-----When you accept a method, the entire system feels the change, except projects that are "isolated".  In an "isolated" project, changes are revoked when you leave the project.  When you enter another project, that project's changes are installed.  (To make a project be isolated, choose "isolate changes of this project" from the "changes..." section of the screen menu.)  You can use an isolated project for making dangerous change to a system, and you can get out if it crashes.  A foreign application can have the separate environment it wants.  Also, you can freeze part of the system for a demo that you don't want to disturb.	An isolated project shares methods with all subprojects inside it, unless they are isolated themselves.   Each isolated project is the head of a tree of projects it shares all methods with.	You may 'assert' all changes ever made in the current project to take effect everywhere.  You are first informed if there are any direct method conflicts with other projects.  The conflicts are presented in a ChangeList Browser.	[Later: A project may be 'frozen'.  Asserts do not apply to it after that.  (Great for demos.)  You should be informed when an assert was blocked in a frozen project.]	Class definitions are global.  If you add an instance variable, it happens in all projects.  All versions of the methods are recompiled, in all projects.  If you remove an inst var that is in use in another isolated project, it will become an Undeclared global.  It is best not to remove an inst var when it is being used in another isolated project.   	Senders and Implementors do not see other versions of a method in other isolated projects.	When you ask for versions of a method, you will not get the history in other isolated projects.	Moving methods and classes between changeSets, and merging changeSets has no effect on which methods are in force.  But, when you look at a changeSet from a different isolated project, the methods will contain the 'wrong' code.  A changeSet is just a list of method names, and does not keep separate copies of any code.	When finer grained assertion is needed, use the method (aProject assertClass: aClass from: thisProject warn: warnConflicts).	How isolated changes work: The first time a class changes, store its MethodDictionary object.  Keep parallel arrays of associations to Classes and MethodDictionaries.  Traverse these and install them when you enter an "ioslated project".  When you leave, store this project's own MethodDictionaries there.	To do an assert, we must discover which methods changed here, and which changed only in the project we are asserting into.  There is one copy of the 'virgin' method dictionaries in the system.  It is always being temporarily stored by the currently inForce isolated project.isolatedHead - true for the "top project", and the origin of each "isolated project".  false or nil for any subproject that shares all methods with its parent project.inForce -  true if my methods are installed now.  false if I am dormant.classArray - list of associations to classes methodDictArray - the method dictionaries of those classes before we started changing methods.  They hang onto the original compiledMethods.  (If this project is dormant, it contains the method dictionaries of those classes as they will be here, in this project).orgArray - the class organizations of the classes in classArray.UsingIsolation (class variable) - true when more than one isolated project exists.  When false, only top project has (isolatedHead = true), and no project records any method dictionaries.  This is to save space when not using Isolated Projects.!!Project methodsFor: 'accessing' stamp: 'tk 2/24/2000 13:51'!environment	^ environment! !!Project methodsFor: 'accessing' stamp: 'tk 2/25/2000 16:00'!urlList: anArray	urlList _ anArray! !!Project methodsFor: 'file in/out' stamp: 'tk 2/25/2000 18:20'!loadFromServer	| servers pair resp thumbnailWindow pvm |	"If a newer version of me is on the server, load it."	(servers _ self serverList) isEmpty 		ifTrue: [^ self inform: 'This project thinks it has never been on a server'].	pair _ self class mostRecent: self name onServer: servers first.	pair first ifNil: [^ self inform: 'can''t find file on server'].	(Base64MimeConverter decodeInteger: version unescapePercents) > pair second ifTrue: [		^ self inform: 'That server has an older version of the project.'].	version = (pair first findTokens: '_.') second 		ifTrue: [resp _ (PopUpMenu labels: 'Reload anyway\Cancel' withCRs) startUpWithCaption: 					'The only changes are the ones you made here.'.				resp ~= 1 ifTrue: [^ nil]]		ifFalse: [resp _ (PopUpMenu labels: 'Load it\Cancel' withCRs) startUpWithCaption: 					'A newer version exists on the server.'.				resp ~= 1 ifTrue: [^ nil]].	"Find parent project, go there, zap old thumbnail"	parentProject == Project current ifFalse: [parentProject enter].	parentProject world submorphsDo: [:sub | 		(sub isKindOf: SystemWindow) ifTrue: [			(pvm _ sub findA: ProjectViewMorph) ifNotNil: [				pvm project == self ifTrue: [thumbnailWindow _ sub]]]].	thumbnailWindow delete.	thumbnailWindow owner ifNotNil: [^ self].	"user refused"	"replace with a new one"	ProjectViewMorph openFromFile: (servers first oldFileNamed: pair first).		"Later check rest of servers if fails.  Still have list here"! !!Project methodsFor: 'file in/out' stamp: 'tk 2/25/2000 14:51'!storeOnServer	| servers pair newVersion resp newName local folder |	"Save to disk as an Export Segment.  Then put that file on the server I came from, as a new version.  Version is literal piece of file name.  Mime encoded and http encoded."	"write locally"	self exportSegment.	"Find out what version"	(servers _ self serverList) isEmpty 		ifTrue: [folder _ PluggableFileList getFolderDialog openLabel:					 'Select a folder on a server:'.			folder ifNil: [^ self].			servers _ Array with: folder.			urlList _ Array with: folder realUrl.			pair _ Array with: nil with: -1]		ifFalse: [pair _ self class mostRecent: self name onServer: servers first].	(newVersion _ self newVersion: pair) ifNil: [^ self].	newName _ self name, '_', newVersion, '.pr'.	local _ FileStream oldFileNamed: self name, '.pr'.	resp _ servers first putFile: local named: newName retry: false.	resp ifFalse: [self inform: 'the primary server of this project seems to be down'.  ^ self].	version _ newVersion.	"committed"	"Later, store with same name on secondary servers.  Still can be race conditions.  All machines will go through the server list in the same order."	"2 to: servers size do: [:aServer | aServer putFile: local named: newName]."	"Rename disk file to be the final name"	local reset.	FileDirectory default rename: local localName toBe: newName.! !!Project methodsFor: 'object fileIn' stamp: 'tk 2/24/2000 13:48'!convertdwctppdaevtngpiicmovu0: varDict dwctppdaevtngpiicmovue0: smartRefStrm	"These variables are automatically stored into the new instance ('world' 'changeSet' 'transcript' 'parentProject' 'previousProject' 'displayDepth' 'activeProcess' 'exitFlag' 'viewSize' 'thumbnail' 'nextProject' 'guards' 'projectParameters' 'isolatedHead' 'inForce' 'classArray' 'methodDictArray' 'orgArray' 'version' 'urlList' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"New variables: ('environment' )  If a non-nil value is needed, please assign it."! !!ProjectViewMorph methodsFor: 'as yet unclassified' stamp: 'tk 2/25/2000 16:01'!mouseMove: evt	| menu selection |	(self containsPoint: evt cursorPoint)		ifTrue:			[self showBorderAs: Color red.			mouseDownTime				ifNil:					[mouseDownTime _ Time millisecondClockValue]				ifNotNil:					[((Time millisecondClockValue - mouseDownTime) > 1100)						ifTrue:							[menu _ CustomMenu new.							menu add: 'enter this project' action: #enter.							menu add: 'save (also saves a local copy)' action: #storeOnServer.							menu add: 'saveAs' action: #store.							menu add: 'see if server version is more recent' 									action: #loadFromServer.							selection _ (menu build preSelect: #enter) startUpCenteredWithCaption: ('Project Named', 								'"', project name, '"').							selection = #enter ifTrue: [^ self enter].							selection = #storeOnServer ifTrue: [^ project storeOnServer].							selection = #store ifTrue: [project urlList: nil.										^ project storeOnServer].							selection = #loadFromServer ifTrue: [^ project loadFromServer].							]]]		ifFalse:			[self showBorderAs: Color blue.			mouseDownTime _ nil]! !!ServerDirectory methodsFor: 'do ftp' stamp: 'tk 2/25/2000 15:30'!putFile: fileStream named: fileNameOnServer retry: aBool	"ar 11/24/1998 Do the usual putFile:named: operation but retry if some error occurs and aBool is set. Added due to having severe transmission problems on shell.webpage.com."	| resp |	type == #file ifTrue: [		^ (FileDirectory on: urlObject pathForDirectory)			putFile: fileStream named: fileNameOnServer].	[[resp _ self putFile: fileStream named: fileNameOnServer] 		ifError:[:err :rcvr| resp _ '5xx ',err]. "Report as error"	aBool and:[((resp isKindOf: String) and: [resp size > 0]) and:[resp first ~= $2]]] whileTrue:[		(self confirm:('Error storing ',fileNameOnServer,' on the server.\(',resp,',)\Retry operation?') withCRs) ifFalse:[^resp].	].	^resp! !