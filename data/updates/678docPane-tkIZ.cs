'From Squeak 2.3 of January 14, 1999 on 15 February 1999 at 5:23:04 pm'!Object subclass: #DocLibrary	instanceVariableNames: 'group lastUpdate lastUpdateName methodVersions '	classVariableNames: 'DocsCachePath DropBox External '	poolDictionaries: ''	category: 'Interface-DocPane'!Object subclass: #NetNameResolver	instanceVariableNames: ''	classVariableNames: 'DefaultHostName HaveNetwork LastContact ResolverBusy ResolverError ResolverReady ResolverSemaphore ResolverUninitialized '	poolDictionaries: ''	category: 'System-Network'!!DocLibrary commentStamp: 'tk 2/13/1999 12:34' prior: 0!Method and Class shared documentation.  Pane in browser.  url for each official version of each method. Each update server group have a prefix (i=internal, e=external).  Point.x;.738.sp  Pane holds a pasteupmorph with comments and examples.  	Must be very careful to give the right options for when to look for docs.  Could be annoying.  Look on disk.  If there, bring it in in background.  If not there, and network has been active this session, or within 15 mins, get from server (in background) and cache on disk.  	When get updates, check for latest version of all comments in the cache.  	Need quick registration of version of inst vars (less bulky and quick to check.)  If all inst var lists are the same as a certain release, mark it as that.  Each release (or update?) have an automatic known registration.	Get doc, Get all docs for this class. //  Net: When you ask, If net has been used, Always (always gets in background) // From disk:  When you ask, always (laptop users do the former).  	Security: Squeakers can write anything, including players.  Users can only add Morphic objects, not players.  (No new code)	Mech:  Users write file to a server with open drop box.  Our server in Alan's office (the librarian) grabs the files once every two minutes, and scans them.  Code must be same as before.  Saves a copy.  Writes on official directory on two outside servers.	Any combo of objects of existing classes that can crash the system, or deny service?  Should the librarian try all buttons first?  If it crashes, won't post it.	Need another machine to check if the librarian is up, and beep Ted.  Could check a time stamp on the main server.  Users could also tell if librarian is up.  Number of docs in the queue.	If we had mime really down, could have squeak email the page to the librarian.  What if the user does not know his pop server?  Use a standard one?  How keep spam out?-----[ ] set up folders, get reader going (no good interface yet)group		Name of group of servers (internal/external)lastUpdate	Number of last update we have.lastUpdateName		File name without number for checking against ChangeSets.methodVersions	Dictionary (class.method -> #(45 secs 120 secs 198 secs)) updates 	that this method  appeared in.  From my version, check backwards till find a doc file on server.  secs is (Time totalSeconds) of file on the server (by its directory) of last version I have.  so can tell if have most recent one.  (use one day slop for older ones)	point.x;.205.sp	rectangle.205.spNames of this form that are too long are run through a dictionary and given a serial number.  It is (first two letters of class name), (crc16 of first half), (crc16 of second half).205.sp.  	Can't store over a file in the drop box, so append random number to end of name.  Look at times to figure out which one is most recent when empty drop box.			localCachePath 	name of cache directory on local disk.  (Is shared between Squeaks on this machine, so might have things too new.)  In the form of a url 'file://disk/folder/'Algorithm for finding the doc file:  	Find my version	Find version of current def of method relative to me.	make file name.	look locally	check server, might have changed.When put new update, no extra work needed.When put a new version of the system, include External with methodVersions filled in.  If methods changed and not in a numbered update, must run a method to put them in the database.When get updates, add new entries as we read updates.Default method update number is 0.AA _ DocLibrary new initialize.AA scanFolder: 'file://Ted''s/Updates 328-/' from: 595.DocLibrary classPool at: #External put: AA.DocLibrary new setUp.[How use internal updates, but do documentation for external?  Disable feature of adding to table when get updates.  Point to UIUC external directory and scan the latest ext updates.]	When a docPane comes in, store property: #classAndMethod.  To put out, menu item "Broadcast Documentation" in PasteUpMorph that has the property.  DocLibrary puts out this morph.  Writes to drop box and local cache.	In codePane, on more menu, "Fetch Documentation" (if none, ask if want blank one).  Creates a new pasteUpMorph after verifying that it doesn't have one.	Later need preference and do fetch always and in the background.	Crude review process -- a method here that brings up each pane that is in drop box (10 at a time).  First just shows code and text, not bring in.  Then bring in.  And a way for me to store it in the official directory.  (Do as menu items in file list?)  And archives and deletes for drop box.  (I do manually twice a day?)	When write a file, take lastUpdateName and look for it in ChangeSet names.  When find, see if this method occurs in any newer changeSet.  If so, writing to an older version.  "The documentation will be attached to the version of this method in xxx.cs.  You have a newer version of that method in yyy.cs.  If what you are storing applies only to the newer version, please do not broadcast it!!  Wait until the new version is in an external update." Broadcast to all Squeak users \ Cancel.  (Otherwise "Make this documentation available to all Squeak users?")When fetch any updates, look for "latest.ix"  Has format:External   407   'aChangeSet.cs'376.ix'class method:' updateNumber'class method' updateNumber'class' updateNumber	Keep local copy of updates.list and read it for files not mentioned yet in latest.ix.�Warn the user if the method he is documenting is too new to be on the External updates server.�Correcting the database of method versions when new External Updates are released.�Create the file to put on the server with the database info for a new update.�Methods to help the reviewer (me) scan files.  It will show me all the code, all the doits in text, and all the text.�Allow documentation for classes, as opposed to methods. (written in file, in dict, just need interface)self scanUpdatesIn: (ServerDirectory serverNamed: 'UpdatesExtUIUC') realUrl, '/'.self updateMethodVersions.!!DocLibrary reorganize!('initialize' initialize setUp)('doc pane' assureCacheFolder cache:as: docNamesAt: docNamesAt:asOf: docObjectAt: fetchDocSel:class: haveNetwork openDocAt: saveDoc: saveDocCheck:)('approving docs' adminSetup)('database of updates' absorbAfter:from: scan:updateID: scan:updateID:writeOn: scanFolder:from: scanUpdatesIn: updateMethodVersions)!!DocLibrary methodsFor: 'initialize' stamp: 'tk 2/4/1999 12:29'!initialize	lastUpdate _ 0.	methodVersions _ Dictionary new.! !!DocLibrary methodsFor: 'initialize' stamp: 'tk 2/15/1999 14:49'!setUp	"set up the External version"	| email |	self initialize.	External _ self.	group _ 'Squeak Public Updates'.	lastUpdate _ 599.	lastUpdateName _ 'MTMcontainsPoint-ar.cs'.	DropBox _ ServerDirectory new.	DropBox server: 'squeak.cs.uiuc.edu'; directory: 'incoming'.	DropBox type: #ftp.	email _ nil.  "Celeste popUserName."	"If nil, we ask at drop time"	DropBox user: 'anonymous'; password: email.	DropBox moniker: 'Doc Pane DropBox'.		"later allow a second server"! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/13/1999 13:45'!assureCacheFolder	"Make sure there is a folder docPaneCache and a file: url for it in DocsCachePath.  In local folder or one level up.  User may wish to install a different path and folder name (as a url).  Could be a url to a local server."	| dir local |	DocsCachePath ifNil: [		dir _ FileDirectory default.		(dir includesKey: 'docPaneCache') ifTrue: [			DocsCachePath _ dir url, 'docPaneCache/']].	DocsCachePath ifNil: [		dir _ FileDirectory default containingDirectory.		DocsCachePath _ dir url, 'docPaneCache/'.		(dir includesKey: 'docPaneCache') ifFalse: [			^ dir createDirectory: 'docPaneCache']].	"create the folder"	local _ ServerDirectory new fullPath: DocsCachePath.	local exists ifFalse: [		DocsCachePath _ nil.	"we must be on a new disk"		self assureCacheFolder].! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/13/1999 14:03'!cache: strm as: fileName	"Save the file locally in case the network is not available."	| local |	local _ ServerDirectory new fullPath: DocsCachePath.	(local fileNamed: fileName) nextPutAll: strm contents; close.! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/5/1999 07:33'!docNamesAt: classAndMethod	"Return a list of fileNames to try for this method.  'Point x:' is form of classAndMethod."	| key verList fileNames |	key _ DocLibrary properStemFor: classAndMethod.	verList _ methodVersions at: key ifAbsent: [#()].	fileNames _ OrderedCollection new.	1 to: verList size by: 2 do: [:ind |		fileNames addFirst: key,'.',(verList at: ind) printString, '.sp'].	fileNames addLast: key,'.0.sp'.	^ fileNames! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/5/1999 07:33'!docNamesAt: classAndMethod asOf: currentUpdate	"Return a list of fileNames to try for this method.  'Point x:' is form of classAndMethod."	| key verList fileNames |	key _ DocLibrary properStemFor: classAndMethod.	verList _ methodVersions at: key ifAbsent: [#()].	fileNames _ OrderedCollection new.	1 to: verList size by: 2 do: [:ind |		(verList at: ind) <= currentUpdate ifTrue: [			fileNames addFirst: key,'.',(verList at: ind) printString, '.sp']].	fileNames addLast: key,'.0.sp'.	^ fileNames! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/15/1999 14:59'!docObjectAt: classAndMethod	"Return a morphic object that is the documentation pane for this method.  nil if none can be found.  Look on both the network and the disk."	| fileNames server aUrl strm local obj |	methodVersions size = 0 ifTrue: [self updateMethodVersions].	"first time"	fileNames _ self docNamesAt: classAndMethod.	self assureCacheFolder.	self haveNetwork ifTrue: [		"server _ (ServerDirectory groupNamed: group) clone."  "Note: directory ends with '/updates' which needs to be '/docpane', but altUrl end one level up"		server _ ServerDirectory groupNamed: group.			"later try multiple servers"		aUrl _ server altUrl, 'docpane/'.		fileNames do: [:aVersion | 			strm _ HTTPSocket httpGetNoError: aUrl,aVersion 				args: nil accept: 'application/octet-stream'.			strm class == RWBinaryOrTextStream ifTrue: [				self cache: strm as: aVersion.				strm reset.				obj _ strm fileInObjectAndCode asMorph.				(obj valueOfProperty: #classAndMethod) = classAndMethod ifFalse: [					self inform: 'suspicious object'.					obj setProperty: #classAndMethod toValue: classAndMethod].				^ obj].	"The pasteUpMorph itself"			"If file not there, error 404, just keep going"]].	local _ ServerDirectory new fullPath: DocsCachePath.	"check that it is really there -- let user respecify"	fileNames do: [:aVersion | 		(local includesKey: aVersion) ifTrue: [			strm _ local oldFileNamed: aVersion.			obj _ strm fileInObjectAndCode asMorph.			(obj valueOfProperty: #classAndMethod) = classAndMethod ifFalse: [				self inform: 'suspicious object'.				obj setProperty: #classAndMethod toValue: classAndMethod].			^ obj].	"The pasteUpMorph itself"		"If file not there, just keep looking"].	"Never been documented"	^ nil! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/14/1999 20:54'!fetchDocSel: aSelector class: className	"Look on servers to see if there is documentation pane for the selected message. Take into account the current update number.  If not, ask the user if she wants to create a blank one."	| key response docPane ext |	key _ className, ' ', aSelector.	(self openDocAt: key) ifNil: [		response _ (PopUpMenu labels: 'Create new page\Cancel' withCRs)				startUpWithCaption: 'No documentation exists for this method.\Would you like to write some?' withCRs.		response = 1 ifTrue: [			docPane _ PasteUpMorph new.			docPane color: Color white; borderWidth: 2; borderColor: Color green.			docPane setProperty: #classAndMethod toValue: key.			docPane setProperty: #initialExtent toValue: (ext _ 200@200).			docPane topLeft: (RealEstateAgent initialFrameFor: docPane) origin.			docPane extent: ext.			docPane addMorph: (TextMorph new topLeft: docPane topLeft + (10@10);					extent: docPane height - 10 @ 30).			Smalltalk currentWorld addMorph: docPane]].	"If found, openDocAt: put it on the screen"! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/15/1999 16:30'!haveNetwork		| hn ask |	(hn _ NetNameResolver haveNetwork) class == Symbol ifFalse: [^ hn].	hn == #expired ifTrue: [		ask _ self confirm: 'OK to connect to the internet?'.		ask ifFalse: [NetNameResolver haveNetwork: false].		^ ask].	^ false! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/9/1999 20:42'!openDocAt: classAndMethod	| docPane |	(docPane _ self docObjectAt: classAndMethod) ifNotNil: [		docPane setProperty: #initialExtent toValue: docPane bounds extent.		docPane topLeft: (RealEstateAgent initialFrameFor: docPane) origin.		Smalltalk currentWorld addMorph: docPane].	^ docPane! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/15/1999 17:10'!saveDoc: aMorph	"Broadcast this documentation to the Squeak community.  Associate it with the method it documents.  Send to a drop box, where it can be inspected before being posted on External servers."	| classAndMethod fName remoteFile |	classAndMethod _ aMorph valueOfProperty: #classAndMethod.	classAndMethod ifNil: [		^ self error: 'need to know the class and method'].	"later let user set it"	fName _ (self docNamesAt: classAndMethod) first.	DropBox user asLowercase = 'anonymous' ifTrue: [		fName _ fName, 1000 atRandom printString].	"trusted users store directly"	DropBox password.	"In case user has to type it.  Avoid timeout from server"	Cursor wait showWhile: [		remoteFile _ DropBox fileNamed: fName.		remoteFile fileOutClass: nil andObject: aMorph.		"remoteFile close"].! !!DocLibrary methodsFor: 'doc pane' stamp: 'tk 2/14/1999 20:40'!saveDocCheck: aMorph	"Make sure the document gets attached to the version of the code that the user was looking at.  Is there a version of this method in a changeSet beyond the updates we know about?  Works even when the user has internal update numbers and the documentation is for external updates (It always is)."	| classAndMethod parts selector class lastUp beyond ours docFor unNum ok key verList ext response |	classAndMethod _ aMorph valueOfProperty: #classAndMethod.	classAndMethod ifNil: [		^ self error: 'need to know the class and method'].	"later let user set it"	parts _ classAndMethod findTokens: ' .'.	selector _ parts last asSymbol.	class _ Smalltalk at: (parts first asSymbol) ifAbsent: [^ self saveDoc: aMorph].	parts size = 3 ifTrue: [class _ class class].	"Four indexes we are looking for:		docFor = highest numbered below lastUpdate that has method.		unNum = a higher unnumbered set that has method.		lastUp = lastUpdate we know about in methodVersions		beyond = any set about lastUp that has the method."	ChangeSorter gatherChangeSets doWithIndex: [:cs :ind | "youngest first"		(cs name includesSubString: lastUpdateName) ifTrue: [lastUp _ ind].		(cs atSelector: selector class: class) ~~ #none ifTrue: [			lastUp ifNotNil: [beyond _ ind. ours _ cs name]				ifNil: [cs name first isDigit ifTrue: [docFor _ ind] 						ifFalse: [unNum _ ind. ours _ cs name]]]].	"See if version the user sees is the version he is documenting"	ok _ beyond == nil.	unNum ifNotNil: [docFor ifNotNil: [ok _ docFor > unNum]						ifNil: [ok _ false]].  "old changeSets gone"	ok ifTrue: [^ self saveDoc: aMorph].	key _ DocLibrary properStemFor: classAndMethod.	verList _ (methodVersions at: key ifAbsent: [#()]), #(0 0).	ext _ verList first.	"external update number we will write to"	response _ (PopUpMenu labels: 'Cancel\Broadcast Page' withCRs)				startUpWithCaption: 'You are documenting a method in External Update ', ext asString, '.\There is a more recent version of that method in ' withCRs, ours, '.\If you are explaining the newer version, please Cancel.\Wait until that version appears in an External Update.' withCRs.	response = 2 ifTrue: [self saveDoc: aMorph].! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/13/1999 11:53'!absorbAfter: oldVersion from: fileName	"Read the .ix file and add to the methodVersions database.  See class comment."	| server aUrl strm newUpdate newName prevFile classAndMethod updateID key verList new |	server _ ServerDirectory groupNamed: group.		"later try multiple servers"	aUrl _ server altUrl, 'docpane/', fileName.	strm _ HTTPSocket httpGetNoError: aUrl		args: nil accept: 'application/octet-stream'.	strm class == RWBinaryOrTextStream ifFalse: [^ false].	(strm upTo: $ ) = 'External' ifFalse: [strm close. ^ false].	newUpdate _ Integer readFrom: strm.	newUpdate = oldVersion ifTrue: [strm close. ^ false].		"already have it" 	strm upTo: $'.	newName _ strm nextDelimited: $'.  strm upTo: Character cr.	prevFile _ strm upTo: Character cr.	"does this report on updates just after what I know?"	oldVersion = (prevFile splitInteger first) ifFalse: [		strm close. ^ prevFile].	"see earlier sucessor file"	[strm atEnd] whileFalse: [		strm upTo: $'.		classAndMethod _ strm nextDelimited: $'.  strm next.		updateID _ Integer readFrom: strm.		key _ DocLibrary properStemFor: classAndMethod.		verList _ methodVersions at: key ifAbsent: [#()].		(verList includes: updateID) ifFalse: [			new _ verList, (Array with: updateID with: -1 "file date seen").			methodVersions at: key put: new]].	strm close.	lastUpdate _ newUpdate.	lastUpdateName _ newName.	^ true! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/5/1999 08:07'!scan: updateStream updateID: updateID	"Scan this update file and remember the update numbers of the methods."	| changeList ee semi key verList new |	updateStream reset; readOnly.	Cursor read showWhile:		[changeList _ ChangeList new			scanFile: updateStream from: 0 to: updateStream size].	changeList list do: [:entry |		ee _ nil.		(entry beginsWith: 'method: ') ifTrue: [			(semi _ entry indexOf: $;) = 0 				ifTrue: [semi _ entry size]				ifFalse: [semi _ semi-1].			ee _ entry copyFrom: 9 to: semi].		(entry beginsWith: 'class comment for ') ifTrue: [			(semi _ entry indexOf: $;) = 0 				ifTrue: [semi _ entry size]				ifFalse: [semi _ semi-1].			ee _ entry copyFrom: 19 to: semi].	"comment for whole class"		ee ifNotNil: [			key _ DocLibrary properStemFor: ee.			Transcript show: key; cr.			verList _ methodVersions at: key ifAbsent: [#()].			(verList includes: updateID) ifFalse: [				new _ verList, (Array with: updateID with: -1 "file date seen").				methodVersions at: key put: new]].		].! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/13/1999 11:22'!scan: updateStream updateID: updateID writeOn: strm	"Scan this update file and remember the update numbers of the methods."	| changeList ee semi |	updateStream reset; readOnly.	Cursor read showWhile:		[changeList _ ChangeList new			scanFile: updateStream from: 0 to: updateStream size].	changeList list do: [:entry |		ee _ nil.		(entry beginsWith: 'method: ') ifTrue: [			(semi _ entry indexOf: $;) = 0 				ifTrue: [semi _ entry size]				ifFalse: [semi _ semi-1].			ee _ entry copyFrom: 9 to: semi].		(entry beginsWith: 'class comment for ') ifTrue: [			(semi _ entry indexOf: $;) = 0 				ifTrue: [semi _ entry size]				ifFalse: [semi _ semi-1].			ee _ entry copyFrom: 19 to: semi].	"comment for whole class"		ee ifNotNil: [			Transcript show: ee; cr.			strm cr; nextPutAll: ee surroundedBySingleQuotes; space;				nextPutAll: updateID asString].		].! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/11/1999 12:06'!scanFolder: directoryUrl from: updateID	"Scan all update files in the directory starting at updateID+1.  updates.list must be present to tell us the file names."	| updateList line num |	updateList _ (ServerFile new fullPath: directoryUrl,'updates.list') asStream.	[line _ updateList upTo: Character cr.	updateList atEnd] whileFalse: [		line first isDigit ifTrue: [			num _ line splitInteger first.			num > updateID ifTrue: [				self scan: (ServerFile new fullPath: directoryUrl,line) asStream					updateID: num]			]].	lastUpdate <= num ifTrue: [		lastUpdate _ num.		lastUpdateName _ line splitInteger last].! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/13/1999 11:25'!scanUpdatesIn: directoryUrl	"Scan all update files in the directory starting at lastUpdate+1.  Create a .ix file on my local hard disk.  updates.list must be present to tell us the file names."	| updateList line num temp out |	updateList _ (ServerFile new fullPath: directoryUrl,'updates.list') asStream.	temp _ WriteStream on: (String new: 2000).	[line _ updateList upTo: Character cr.	updateList atEnd] whileFalse: [		line first isDigit ifTrue: [			num _ line splitInteger first.			num > lastUpdate ifTrue: [				self scan: (ServerFile new fullPath: directoryUrl,line) asStream					updateID: num writeOn: temp]			]].	num >= lastUpdate ifTrue: [		out _ FileStream newFileNamed: 'to', num asString, '.ix'.		out nextPutAll: 'External ', num asString; space. 		line splitInteger last storeOn: out.	"quoted"		out cr; nextPutAll: lastUpdate asString, '.ix' "; cr".	"temp begins with cr"		out nextPutAll: temp contents; close.		self inform: 'Rename latest.ix to ', lastUpdate asString, 			'.ix on both external servers.Put to', num asString, '.ix on both and call it latest.ix'].	! !!DocLibrary methodsFor: 'database of updates' stamp: 'tk 2/15/1999 14:49'!updateMethodVersions	"See if any new updates have occurred, and put their methods into the database."	| indexFile list result |	self haveNetwork ifFalse: [^ self].	indexFile _ 'latest.ix'.	list _ OrderedCollection new.	[result _ self absorbAfter: lastUpdate from: indexFile.	"boolean if succeeded, or we are up to date, or server not available"	result class == String] whileTrue: [		"result is the prev file name"		list addFirst: indexFile.		indexFile _ result].	list do: [:aFile | self absorbAfter: lastUpdate from: aFile].		"should always work this time"! !!DocLibrary class methodsFor: 'as yet unclassified' stamp: 'tk 2/4/1999 15:58'!external	"The dictionary for the External Updates"	^ External! !!DocLibrary class methodsFor: 'as yet unclassified' stamp: 'tk 2/5/1999 08:11'!properStemFor: classAndMethod	"Put 'class method' into proper form as a file name.  Leave upper and lower case.  The fileName must be short enough and have proper characters for all platforms and servers."	| sz |	classAndMethod size > 23 ifTrue: ["too long"		sz _ classAndMethod size.		"input contains space and :, not . and ;"		^ (classAndMethod copyFrom: 1 to: 2), 			((classAndMethod copyFrom: 3 to: sz//2) crc16 printString),			((classAndMethod copyFrom: sz//2+1 to: sz) crc16 printString)		].	^ (classAndMethod copyReplaceAll: ' ' with: '.')		copyReplaceAll: ':' with: ';'! !!FileDirectory methodsFor: 'file name utilities' stamp: 'tk 2/13/1999 13:30'!url	"Convert my path into a file:// type url.  Use slash instead of the local delimiter (:), and convert odd characters to %32 notation."	"If slash (/) is not the file system delimiter, encode slashes before converting."	| list |	list _ self pathParts.	^ String streamContents: [:strm |		strm nextPutAll: 'file:/'.		list do: [:each | strm nextPut: $/; nextPutAll: each encodeForHTTP].		strm nextPut: $/]! !!HTTPSocket class methodsFor: 'get the page' stamp: 'tk 2/10/1999 12:27'!httpGetNoError: url args: args accept: mimeType	"Return the exact contents of a web file.  Do better error checking.  Asks for the given MIME type.  To fetch raw data, you can use the MIMI type 'application/octet-stream'.  If mimeType is nil, use 'text/html'.  The parsed header is saved. Use a proxy server if one has been registered."	| document data |	document _ self httpGetDocument: url  args: args  accept: mimeType.	(document isKindOf: String) ifTrue: [		"strings indicate errors"		^ document ].	data _ document content.	(data beginsWith: '<HTML><HEAD>
<TITLE>4') ifTrue: [		"an error message  404 File not found"		^ data copyFrom: 21 to: data size-16].		^ (RWBinaryOrTextStream with: data) reset! !!Morph methodsFor: 'accessing' stamp: 'tk 2/6/1999 22:43'!asMorph	^ self! !!Morph methodsFor: 'fileIn/out' stamp: 'tk 2/9/1999 20:18'!saveDocPane	DocLibrary external saveDocCheck: self! !!NetNameResolver commentStamp: 'tk 2/15/1999 14:24' prior: 0!This class implements TCP/IP style network name lookup and translation facilities.Attempt to keep track of whether there is a network available.HaveNetwork	true if last attempt to contact the network was successful.LastContact		Time of that contact (totalSeconds).haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).!!NetNameResolver class methodsFor: 'network initialization' stamp: 'tk 2/15/1999 14:35'!haveNetwork	"Our best estimate of whether a network is available.  Caller will want to ask user if we should try this time."	HaveNetwork ifFalse: [^ false].	Time totalSeconds - LastContact > 1800 "30 min" ifTrue: [^ #expired].	^ true	"are current"! !!NetNameResolver class methodsFor: 'network initialization' stamp: 'tk 2/15/1999 14:47'!haveNetwork: boolean	"Allow user to say we don't want to try to start a connection.  Not enforced.  Only for caller's information when he asks."	HaveNetwork _ boolean.	LastContact _ Time totalSeconds.! !!NetNameResolver class methodsFor: 'network initialization' stamp: 'tk 2/15/1999 14:28'!initializeNetworkIfFail: errorBlock	"Initialize the network drivers and record the semaphore to be used by the resolver. Do nothing if the network is already initialized. Evaluate the given block if network initialization fails."	"NetNameResolver initializeNetworkIfFail: [self error: 'network initialization failed']"	| semaIndex result |	self primNameResolverStatus = ResolverUninitialized		ifFalse: [			LastContact _ Time totalSeconds.  HaveNetwork _ true.			^ self].  "network is already initialized"	LastContact _ Time totalSeconds.  HaveNetwork _ false.	"in case abort"	ResolverSemaphore _ Semaphore new.	semaIndex _ Smalltalk registerExternalObject: ResolverSemaphore.	Utilities informUser:'Initializing the network drivers; this maytake up to 30 seconds and can''t be interrupted'		during: [result _ self primInitializeNetwork: semaIndex].	"result is nil if network initialization failed, self if it succeeds"	result ifNil: [errorBlock value]		ifNotNil: [HaveNetwork _ true].! !!Password methodsFor: 'accessing' stamp: 'tk 2/10/1999 14:06'!passwordFor: serverDir	"Returned the password from one of many sources.  OK if send in a nil arg."	| sp msg |	cache ifNotNil: [^ cache].	sequence ifNotNil: [		(sp _ self serverPasswords) ifNotNil: [			sequence <= sp size ifTrue: [^ sp at: sequence]]].	msg _ (serverDir isKindOf: ServerDirectory)		ifTrue: [serverDir moniker]		ifFalse: ['this directory'].	(serverDir user = 'anonymous') & (serverDir type == #ftp) ifTrue: [			^ cache _ FillInTheBlank request: 'Please let this anonymous ftp\server know your email address.\This is the polite thing to do.' withCRs			initialAnswer: 'yourName@company.com'].	^ cache _ FillInTheBlank request: 'Password for ', msg, ':'.		"Diff between empty string and abort?"! !!PasteUpMorph methodsFor: 'menu & halo' stamp: 'tk 2/10/1999 13:51'!addPlayfieldMenuItems: menu hand: aHandMorph	| subMenu prefix hasPalette |	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'save on file...' action: #saveOnFile.	subMenu add: 'save as SqueakPage at url...' action: #saveOnURL.	(self valueOfProperty: #classAndMethod) ifNotNil: [		subMenu add: 'broadcast as documentation' action: #saveDocPane].	subMenu add: 'navigate to...' action: #navigateTo.	subMenu add: 'round up strays' action: #roundUpStrays.	subMenu addLine.	#(	(autoLineLayout			'auto layout'			toggleAutoLineLayout)		(resizeToFit				'resize-to-fit'			toggleResizeToFit)		(indicateCursor			'showing cursor'		toggleIndicateCursor)		(isPartsBin				'being a parts bin'		toggleIsPartsBin)		(wantsMouseOverHalos	'mouse-over halos'		toggleMouseOverHalos))	do:		[:triplet |			prefix _ (self perform: triplet first) ifTrue: ['stop '] ifFalse: ['start '].			subMenu add:  (prefix, triplet second) action: triplet third].	prefix _ (self hasProperty: #automaticPhraseExpansion) ifTrue: ['stop '] ifFalse: ['start '].	subMenu add: (prefix, 'phrase expansion') action: #toggleAutomaticPhraseExpansion.	prefix _ (self hasProperty: #alwaysShowThumbnail) ifTrue: ['stop '] ifFalse: ['start '].	subMenu add: (prefix, 'showing thumbnails') action: #toggleAlwaysShowThumbnail.	(self hasProperty: #alwaysShowThumbnail) ifTrue:		[subMenu add: 'set thumbnail height...' action: #setThumbnailHeight].	(hasPalette _ self standardPalette ~~ nil) ifTrue:		[prefix _ self automaticViewing ifTrue: ['stop '] ifFalse: ['start '].		subMenu add: (prefix, 'automatic viewing') action: #toggleAutomaticViewing].	subMenu addLine.	hasPalette ifTrue: [subMenu add: 'clear palette area' action: #clearPaletteArea].	(self resizeToFit & self indicateCursor & self autoLineLayout) ifFalse:		[subMenu add: 'behave like a Holder' action: #behaveLikeHolder].	self backgroundSketch ifNotNil: [subMenu add: 'delete background painting' action: #deleteBackgroundPainting].	presenter ifNil:		[subMenu add: 'make detachable' action: #makeDetachable].	subMenu addLine.	subMenu add: 'use standard texture' action: #setStandardTexture.	subMenu add: 'make graph paper...' action: #makeGraphPaper.	subMenu addTitle: 'Playfield options'.	menu add: 'playfield options...' subMenu: subMenu.! !!ServerDirectory methodsFor: 'updates' stamp: 'tk 2/4/1999 11:40'!putUpdate: fileStrm	"Put this file out as an Update on the servers of my group.  Each version of the system has its own set of update files.  'updates.list' holds the master list.  Each update is a fileIn whose name begins with a number.  See Utilities class readServerUpdatesThrough:saveLocally:updateImage:."	| myServers updateStrm sequence newName myName response local restOfText seq |"	(ScheduledControllers scheduledControllers detect: [:each |		each model == Transcript] ifNone: [nil]) ifNil: [			^ self inform: 'Please open a Transcript window, and then start putting out this update again.']."	local _ fileStrm localName.	(self checkNames: (Array with: local)) ifFalse: [^ nil].	"allowed characters"	myName _ group ifNil: [self moniker] ifNotNil: [group key].	response _ (PopUpMenu labels: 'Install update\Cancel update' withCRs)		startUpWithCaption: 'Do you really want to broadcast the file ', local, 			'\to every Squeak user who updates from ' withCRs, myName, '?'.	response = 1 ifFalse: [^ nil].	"abort"	(myServers _ self checkServers) size = 0 ifTrue: [^ self].	updateStrm _ myServers first getFileNamed: 'updates.list'.	"get last number and add 1"	sequence _ Utilities lastUpdateNum: updateStrm.	seq _ (sequence+1) printString.	seq size = 1 ifTrue: [seq _ '00', seq].	seq size = 2 ifTrue: [seq _ '0', seq].	newName _ seq, local.	restOfText _ Utilities position: updateStrm 	"sets the postion!!!!"			atVersion: (Smalltalk at: #EToySystem) version.	restOfText size > 0 ifTrue: [		response _ (PopUpMenu labels: 'Make update for my older version\Cancel update' withCRs)			startUpWithCaption: 'This system, ', (Smalltalk at: #EToySystem) version,				' is not the latest version'.		response = 1 ifFalse: [^ nil].	"abort"		].	"append name to updates"	(updateStrm skip: -1; next) == Character cr ifFalse: [		updateStrm nextPut: Character cr].	updateStrm nextPutAll: newName; nextPut: Character cr; nextPutAll: restOfText.	myServers do: [:aServer |		fileStrm reset.	"reopen"		aServer putFile: fileStrm named: newName retry: true.		updateStrm reset.		aServer putFile: updateStrm named: 'updates.list' retry: true.		Transcript cr; show: 'Update succeeded on server ', aServer moniker].			Transcript cr; show: 'Be sure to test your new update!!'; cr.	"rename the file locally (may fail)"	fileStrm directory rename: local toBe: newName.!]style[(19 225 65 2075)f1b,f1,f1LUtilities class readServerUpdatesThrough:saveLocally:updateImage:;,f1! !!ServerDirectory methodsFor: 'accessing' stamp: 'tk 2/14/1999 20:44'!password	passwordHolder ifNil: [passwordHolder _ Password new].	^ passwordHolder passwordFor: self	"may ask the user"! !!ServerDirectory methodsFor: 'accessing' stamp: 'tk 2/14/1999 21:44'!type: aSymbol	type _ aSymbol! !!ServerDirectory methodsFor: 'file directory' stamp: 'tk 2/13/1999 13:58'!deleteFileNamed: fullName	"Detete a remote file.  fullName is directory path, and does include name of the server.  Or it can just be a fileName."	| file so rr |	file _ self as: ServerFile.	(fullName includes: self pathNameDelimiter)		ifTrue: [file fullPath: fullName]		"sets server, directory(path), fileName"		ifFalse: [file fileName: fullName].	"JUST a single NAME, rest is here"			"Mac files that include / in name, must encode it as %2F "	file type == #file ifTrue: [		^ (FileDirectory forFileName: (file fileNameRelativeTo: self)) 			deleteFileNamed: file fileName].		so _ file openNoDataFTP.	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'DELE ', file fileName.	(rr _ so responseOK) == true ifFalse: [^ rr].	""	so sendCommand: 'QUIT'.	(rr _ so responseOK) == true ifFalse: [^ rr].	"221"	so destroy.	"Always OK to destroy"! !!ServerDirectory methodsFor: 'file directory' stamp: 'tk 2/13/1999 14:00'!exists	"It is difficult to tell if a directory exists.  This is ugly, but it works for writable directories.  (tested only for file://)"	| probe |	self entries size > 0 ifTrue: [^ true].	(probe _ self newFileNamed: 'withNoName23') ifNil: [^ false].	probe close.	probe directory deleteFileNamed: probe localName.	^ true! !!SqueakPage methodsFor: 'accessing' stamp: 'tk 2/5/1999 16:47'!asMorph	^ self fetchContents! !!StringHolder methodsFor: 'message list menu' stamp: 'tk 2/9/1999 20:56'!fetchDocPane	"Look on servers to see if there is documentation pane for the selected message. Take into account the current update number.  If not, ask the user if she wants to create one."	DocLibrary external fetchDocSel: self selectedMessageName 		class: self selectedClassName! !!Browser methodsFor: 'message functions' stamp: 'tk 2/9/1999 20:53'!messageListMenu: aMenu shifted: shifted	^ shifted ifFalse: [aMenu labels:'browse full (b)fileOutprintOutsenders of... (n)implementors of... (m)method inheritanceversions (v)inst var refs...inst var defs...class var refs...class variablesclass refs (N)removemore...'	lines: #(3 7 12)	selections:		#(browseMethodFull fileOutMessage printOutMessage		browseSendersOfMessages browseMessages methodHierarchy browseVersions		browseInstVarRefs browseInstVarDefs browseClassVarRefs 			browseClassVariables browseClassRefs		removeMessage shiftedYellowButtonActivity )]	ifTrue: [aMenu labels: 'browse class hierarchybrowse classbrowse methodimplementors of sent messageschange sets with this methodinspect instancesinspect subinstancesremove from this browserannotations in this browserrevert to previous versionremove from current change setrevert and forgetfetch documentationmore...' 	lines: #(5 7 9 12)	selections: #(classHierarchy browseClass 		buildMessageBrowser browseAllMessages findMethodInChangeSets 		inspectInstances inspectSubInstances		removeMessageFromBrowser chooseAnnotations		revertToPreviousVersion 		removeFromCurrentChanges revertAndForget		fetchDocPane		unshiftedYellowButtonActivity)]! !!Utilities class methodsFor: 'fetching updates' stamp: 'tk 2/13/1999 13:00'!readServerUpdatesThrough: maxNumber saveLocally: saveLocally updateImage: updateImage	"Scan the update server(s) for unassimilated updates. If maxNumber is not nil, it represents the highest-numbered update to load.  This makes it possible to update only up to a particular point.   If saveLocally is true, then save local copies of the update files on disc.  If updateImage is true, then absorb the updates into the current image.A file on the server called updates.list has the names of the last N update files.  We look backwards for the first one we do not have, and start there""* To add a new update:  Name it starting with a new two-digit code.  * Do not use %, /, *, space, or more than one period in the name of an update file.* The update name does not need to have any relation to the version name.* Figure out which versions of the system the update makes sense for.* Add the name of the file to each version's category below.* Put this file and the update file on all of the servers.** To make a new version of the system:  Pick a name for it (no restrictions)* Put # and exactly that name on a new line at the end of this file.* During the release process, fill in exactly that name in the dialog box.* Put this file on the server.""Utilities readServerUpdatesThrough: 828 saveLocally: false updateImage: true""Utilities readServerUpdatesThrough: 828 saveLocally: true updateImage: true"	| doc urls failed loaded str |	Utilities chooseUpdateList ifFalse: [^ self].	"ask the user which kind of updates"	Cursor wait showWhile: [(Smalltalk includesKey: #EToySystem)		ifTrue: [ScriptingSystem guessDOLProxy].	urls _ self newUpdatesOn: 		(Utilities serverUrls collect: [:url | url, 'updates/']) throughNumber: maxNumber.	loaded _ 0.	failed _ nil.	urls do: [:this |		failed ifNil:			[doc _ HTTPSocket httpGet: this accept: 'application/octet-stream'.			doc class == String				ifTrue: [failed _ this]	"an error loading"				ifFalse:					[doc reset; text.					doc peek asciiValue = 4	"pure object file"						ifTrue: [failed _ this]	"Must be fileIn, not pure object file"						ifFalse:							["(this endsWith: '.html') ifTrue: [doc _ doc asHtml]."								"HTML source code not supported here yet"							updateImage ifTrue:									[ChangeSorter newChangesFromStream: doc									named: (this findTokens: '/') last].							saveLocally ifTrue:								[self saveUpdate: doc onFile: (this findTokens: '/') last].	"if wanted"							loaded _ loaded + 1]]]]].	str _ loaded printString ,' new update files processed.'.	failed ifNotNil: [str _ str, '\Could not load ' withCRs, 		(urls size - loaded) printString ,' update files.',		'\Starting with "' withCRs, failed, '".'].	failed ifNil: [DocLibrary external ifNotNil: [			DocLibrary external updateMethodVersions]].	self inform: str.! !DocLibrary removeSelector: #absorbVersionsFrom:!DocLibrary removeSelector: #checkFolder!DocLibrary removeSelector: #scan:!DocLibrary removeSelector: #findNewUpdates!"Postscript:Establish a database for methods that have Documentation panes."DocLibrary new setUp.NetNameResolver haveNetwork: true.	"initialization.  just got updates"!