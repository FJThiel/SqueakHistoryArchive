'From Squeak 2.2 of Sept 23, 1998 on 15 October 1998 at 9:49:48 am'!!ServerDirectory methodsFor: 'do ftp' stamp: 'tk 10/12/1998 08:58'!openFTP	"Open a connection to the directory and server I hold.  Return a FTPSocket.  It has opened passive, and has a dataPort number assigned to a data FTPSocket.  But the data connection is not open.  When you are done, be sure to tell the socket to QUIT, and then destroy it."	| so resp portInfo list dataPort dd rr serverIP |	Socket initializeNetwork.	FTPSocket retry: [serverIP _ NetNameResolver addressForName: server timeout: 20.				serverIP ~~ nil] 		asking: 'Trouble resolving server name.  Keep trying?'		ifGiveUp: [^ 'Could not resolve the server named: ', server].	so _ FTPSocket new.	so portNum: 21.	so connectTo: serverIP port: 21.  "21 is for the control connection"	so waitForConnectionUntil: FTPSocket standardDeadline.	Transcript cr; show: server; cr.	(rr _ so lookFor: '220 ') == true ifFalse: [^ rr].	"220 para1 Microsoft FTP Service"	so sendCommand: 'USER ', user.	(rr _ so lookFor: '331 ') == true ifFalse: [^ rr].	"331 Password required"	so sendCommand: 'PASS ', self password.	(rr _ so lookFor: '230 ') == true ifFalse: [^ rr].	"230 User logged in"	so sendCommand: 'CWD ', directory.	(rr _ so lookFor: '250 ') == true ifFalse: [^ rr].	"250 CWD successful"	"Need to ask for name of directory to make sure?"	so sendCommand: 'TYPE L 8'.	(rr _ so lookFor: '200 ') == true ifFalse: [^ rr].	"200 Type set to L"	so sendCommand: 'PASV'.	resp _ (so getResponseUpTo: FTPSocket crLf) first.		"Tells which port on server to use for data"	Transcript show: resp; cr.	(resp beginsWith: '227 ') ifFalse: [ "Check for Entering Passive Mode"		so sendCommand: 'QUIT'.		so destroy.		^ self error: 'can''t get into passive mode'].	portInfo _ (resp findTokens: '()') at: 2.	list _ portInfo findTokens: ','.	dataPort _ (list at: 5) asNumber * 256 + (list at: 6) asNumber.	dd _ FTPSocket new.	dd portNum: dataPort.	so dataSocket: dd.	"save it, not opened yet"	^ so! !!StandardFileStream methodsFor: 'read, write, position' stamp: 'tk 10/15/1998 09:30'!padToEndWith: aChar	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."	| pad |	self atEnd ifTrue: [^ self].	pad _ self isBinary 		ifTrue: [aChar asCharacter asciiValue]	"ok for char or number"		ifFalse: [aChar asCharacter].	self nextPutAll: (buffer1 class new: ((self size - self position) min: 20000) 							withAll: pad).! !!SwikiAction methodsFor: 'save and restore' stamp: 'mjg 10/9/1998 13:05'!restore: nameOfSwiki	"Read all files in the directory 'nameOfSwiki'.  Reconstruct theurl map."	| map page folder dir rep templateFolder |	map _ self class mapClass new.	self map: map.	self formatter: (HTMLformatter new initialize).	self formatter specialCharacter: $*.	self name: nameOfSwiki.	templateFolder _ 'swiki'.	self source: templateFolder,(ServerAction pathSeparator).	map action: self.	map pages: (Dictionary new).	map directory: nameOfSwiki. "This is where the pages are."	folder _ (ServerAction serverDirectory), nameOfSwiki.	dir _ FileDirectory on: folder.	dir fileNames do: [:fName |		rep _ fName detect: [:char | char isDigit not] ifNone: [$3].		rep isDigit ifTrue: ["all are digits"			page _ self class pageClass new.			page fromFileNamed: folder,				(ServerAction pathSeparator),fName action: self.			(page time isNil) ifTrue: [page time: Time now].			map at: page name put: page]].	PWS link: nameOfSwiki to: self.! !!CachedSwikiAction methodsFor: 'URL processing' stamp: 'mjg 10/13/1998 12:29'!inputFrom: request	"Take user's input and respond with a searchresult or store the edit"	| coreRef page theText |	coreRef _ request message size < 2		ifTrue: ['1']		ifFalse: [request message at: 2].	coreRef = 'searchresult' ifTrue: [		"If contains search string, do search"		request reply: PWS crlf,			(HTMLformatter evalEmbedded: (self fileContents: source, 'results.html')				with: (urlmap searchFor: (					request fields at: 'searchFor' ifAbsent: ['nothing']))).		^ #return].	(theText _ request fields at: 'text' ifAbsent: [nil]) ifNotNil: [		"It's a response from an edit, so store the page"		page _ urlmap atID: coreRef.		page user: request peerName.  "Address is machine, user only if logged in"		 page pageStatus = #new ifTrue: [page pageStatus: #standard].		page _ urlmap			storeID: coreRef			text: theText withSqueakLineEndings			from: request peerName.		self generate: (urlmap atID: coreRef) from: request.		self generateRecent.		^ self].	"return self means do serve the edited page afterwards"	request fields keys do: [:aTag |		(aTag beginsWith: 'text-') ifTrue: [			urlmap				storeID: coreRef				text: (request fields at: aTag) withSqueakLineEndings				insertAt: (aTag copyFrom: 6 to: aTag size).	"string"		self generate: (urlmap atID: coreRef) from: request.		self generateRecent.			^ self]].		"oops, a new kind!!"	Transcript show: 'Unknown data from client. '; show: request fieldsprintString; cr.! !!URLmap methodsFor: 'accessing' stamp: 'JZH 10/13/1998 22:56'!atID: id	"Return page of a given key."	| idString |	idString _ id isInteger ifTrue: [id printString] ifFalse: [id].	^ pages detect: [:page | page coreID = idString] ifNone: [selfatID: '1']! !!URLmap methodsFor: 'accessing' stamp: 'mjg 10/14/1998 14:38'!pageURL: aPage	(aPage pageStatus = #new)		ifTrue:			[(action isKindOf: CachedSwikiAction)			ifFalse: [^'<u>',(aPage name),'</u><a href="', 				(action pageURL: aPage),'.edit">?</a>']			ifTrue: [^'<u>',(aPage name),'</u><a href="',				(action pwsURL),(action name),'.',(aPage coreID),'.edit">?</a>']				"Underlines new but not yet edited pages"]		ifFalse:			[^'<a href="', (action pageURL: aPage),'">',(aPage name),'</a>']"Logically, there should be editPageUrl in SwikiAction as well. After all,we might want to define other URL schemes." ! !!URLmap methodsFor: 'creating' stamp: 'mjg 10/9/1998 13:06'!newpage: label from: peer	| newpage newfile |	newfile _ ((pages inject: 0 into: [:max :p |		p coreID asNumber > max ifTrue: [p coreID asNumber]				ifFalse: [max]]) + 1) printString.	newpage _ action class pageClass new.	self at: label put: newpage.	newpage address: peer.	newpage date: (Date today).	newpage time: (Time now).	newpage coreID: newfile.	newpage name: label.	newpage file: ((ServerAction serverDirectory),		directory, (ServerAction pathSeparator), newfile).	newpage pageStatus: #new.	newpage text: 'Describe ',label,' here'.	newpage map: self.	newpage url: (action name),'.',newfile.	^newpage! !