'From Squeak3.3alpha of 30 January 2002 [latest update: #4744] on 13 February 2002 at 3:16:28 pm'!"Change Set:		xTPfixes-hgDate:			12 February 2002Author:			Henrik GedenrydThe http/ftp fixes include a correct Squeak version in the user agent, and a class variable to turn Transcript output on or off (off by default).Better ServerDirectory protocol for multi-action sessions (more than one ServerDirectory action within a single ftp session).Also corrects the use of proxies in several places.There is also a feature for bypassing proxies when loading via HTTP."!SimpleClientSocket subclass: #HTTPSocket	instanceVariableNames: 'headerTokens headers responseCode '	classVariableNames: 'HTTPBlabEmail HTTPPort HTTPProxyExceptions HTTPProxyPort HTTPProxyServer LogToTranscript ParamDelimiters '	module: #(Squeak Network Library Protocol)!!HTTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/11/2002 13:55'!getResponseUpTo: markerString	"Keep reading until the marker is seen.  Return three parts: header, marker, beginningOfData.  Fails if no marker in first 2000 chars." 	| buf response bytesRead tester mm tries |	buf _ String new: 2000.	response _ WriteStream on: buf.	tester _ 1. mm _ 1.	tries _ 3.	[tester _ tester - markerString size + 1 max: 1.  "rewind a little, in case the marker crosses a read boundary"	tester to: response position do: [:tt |		(buf at: tt) = (markerString at: mm) ifTrue: [mm _ mm + 1] ifFalse: [mm _ 1].			"Not totally correct for markers like xx0xx"		mm > markerString size ifTrue: ["got it"			^ Array with: (buf copyFrom: 1 to: tt+1-mm)				with: markerString				with: (buf copyFrom: tt+1 to: response position)]].	 tester _ 1 max: response position.	"OK if mm in the middle"	 (response position < buf size) & (self isConnected | self dataAvailable) 			& ((tries _ tries - 1) >= 0)] whileTrue: [		(self waitForDataUntil: (Socket deadlineSecs: 5)) ifFalse: [			Transcript show: ' <response was late> '].		bytesRead _ self primSocket: socketHandle receiveDataInto: buf 			startingAt: response position + 1 count: buf size - response position.		"response position+1 to: response position+bytesRead do: [:ii | 			response nextPut: (buf at: ii)].	totally redundant, but needed to advance position!!"		response instVarAt: 2 "position" put: 			(response position + bytesRead)].	"horrible, but fast"	^ Array with: response contents		with: ''		with: ''		"Marker not found and connection closed"! !!HTTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/11/2002 20:13'!getRestOfBuffer: beginning	"We don't know the length.  Keep going until connection is closed.  Part of it has already been received.  Response is of type text, not binary."	| buf response bytesRead |	response _ RWBinaryOrTextStream on: (String new: 2000).	response nextPutAll: beginning.	buf _ String new: 2000.	[self isConnected | self dataAvailable] 	whileTrue: [		(self waitForDataUntil: (Socket deadlineSecs: 5)) ifFalse: [	 		Transcript show: 'data was slow'; cr].		bytesRead _ self primSocket: socketHandle receiveDataInto: buf 				startingAt: 1 count: buf size. 		bytesRead > 0 ifTrue: [  			response nextPutAll: (buf copyFrom: 1 to: bytesRead)] ].	self logToTranscript ifTrue: [		Transcript cr; show: 'data byte count: ', response position printString].	response reset.	"position: 0."	^ response! !!HTTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/11/2002 19:47'!logToTranscript	^LogToTranscript == true! !!FTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/11/2002 19:47'!getResponseUpTo: markerString		| response |	response _ super getResponseUpTo: markerString.	self logToTranscript ifTrue: [		Transcript show: response first; cr].	^response! !!FTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/4/2002 10:18'!lookSoftlyFor: beginning        "Get the response from the server.  Return true the string in beginning is at the front of what came back.  Don't kill the socket if we fail.  Users wants to try another password."	| resp what all |	(readAhead ~~ nil and: [readAhead size > 0])		ifTrue: [resp _ readAhead removeFirst]  "response already came in"		ifFalse: [			all _ self getResponseUpTo: CrLf.			resp _ all at: 1.       "150 Opening binary mode data conn"			readAhead _ (all at: 3) findBetweenSubStrs: (Array with: CrLf)].	resp size > 0 		ifTrue: [			resp first isDigit ifFalse: [ ^self lookFor: beginning ].				"we're in the middle of a line, not the end." #XXX. "this should be fixed..."			(resp at: 4) == $- ifTrue: [^ self lookFor: beginning]. "is a comment"			(resp beginsWith: beginning) ifTrue: [^ true].  "exactly what we wanted"			]		ifFalse: [resp _ '[timeout]'].	what _ self sayWhat:  resp.	what = 2 ifTrue: [self halt].	^ resp! !!FTPSocket methodsFor: 'as yet unclassified' stamp: 'hg 2/11/2002 19:48'!sendCommand: commandString	self logToTranscript ifTrue: [		(commandString beginsWith: 'PASS')			ifFalse: [Transcript show: commandString; cr]			ifTrue: [Transcript show: 'PASS ***'; cr]].	super sendCommand: commandString.! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/12/2002 11:39'!httpGet: url args: args accept: mimeType	^self httpGet: url args: args accept: mimeType request: ''! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/12/2002 11:37'!httpGet: url args: args accept: mimeType request: requestString	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIMI type 'application/octet-stream'."	| document |	document _ self httpGetDocument: url  args: args  accept: mimeType request: requestString.	(document isKindOf: String) ifTrue: [		"strings indicate errors"		^ document ].	^ (RWBinaryOrTextStream with: document content) reset! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/13/2002 14:23'!httpGetDocument: url args: args accept: mimeType request: requestString	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. An extra requestString may be submitted and must end with crlf.  The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIME type 'application/octet-stream'."	| serverName serverAddr port sock header length bare page list firstData aStream index connectToHost connectToPort type newUrl |	Socket initializeNetwork.	bare _ (url asLowercase beginsWith: 'http://') 		ifTrue: [url copyFrom: 8 to: url size]		ifFalse: [url].	bare _ bare copyUpTo: $#.  "remove fragment, if specified"	serverName _ bare copyUpTo: $/.	page _ bare copyFrom: serverName size + 1 to: bare size.	(serverName includes: $:) 		ifTrue: [ index _ serverName indexOf: $:.			port _ (serverName copyFrom: index+1 to: serverName size) asNumber.			serverName _ serverName copyFrom: 1 to: index-1. ]		ifFalse: [ port _ self defaultPort ].	page size = 0 ifTrue: [page _ '/'].	"add arguments"	args ifNotNil: [page _ page, (self argString: args) ].	(self shouldUseProxy: serverName)		ifFalse: [ 			connectToHost _ serverName.			connectToPort _ port ]		ifTrue:  [			page _ 'http://', serverName, ':', port printString, page.		"put back together"			connectToHost _ HTTPProxyServer.			connectToPort _ HTTPProxyPort].		self flag: #XXX.  "this doesn't make sense if a user isn't available for questioning...  -ls"	self retry: [serverAddr _ NetNameResolver addressForName: connectToHost timeout: 20.				serverAddr ~~ nil] 		asking: 'Trouble resolving server name "' , connectToHost , '".  Keep trying?'		ifGiveUp: [Socket deadServer: connectToHost.				^ 'Could not resolve the server named: ', connectToHost].3 timesRepeat: [	sock _ HTTPSocket new.	sock connectTo: serverAddr port: connectToPort.	(sock waitForConnectionUntil: (self deadlineSecs: 30)) ifFalse: [		Socket deadServer: connectToHost.  sock destroy.		^ 'Server ',connectToHost,' is not responding'].	"Transcript cr;show: url; cr.	Transcript show: page; cr."	sock sendCommand: 'GET ', page, ' HTTP/1.0', CrLf, 		(mimeType ifNotNil: ['ACCEPT: ', mimeType, CrLf] ifNil: ['']),		'ACCEPT: text/html', CrLf,	"Always accept plain text"		HTTPBlabEmail,	"may be empty"		requestString,	"extra user request. Authorization"		self userAgentString, CrLf,		'Host: ', serverName, ':', port printString, CrLf.	"blank line automatically added"	list _ sock getResponseUpTo: CrLf, CrLf ignoring: (String with: CR).	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	"Transcript show: page; cr; show: header; cr."	firstData _ list at: 3.	header isEmpty 		ifTrue: [aStream _ 'server aborted early']		ifFalse: [			"dig out some headers"			sock header: header.			length _ sock getHeader: 'content-length'.			length ifNotNil: [ length _ length asNumber ].			type _ sock getHeader: 'content-type'.			sock responseCode first = $3 ifTrue: [				newUrl _ sock getHeader: 'location'.				newUrl ifNotNil: [ 					Transcript show: 'redirecting to ', newUrl; cr.					sock destroy.					newUrl _ self expandUrl: newUrl ip: serverAddr port: connectToPort.					^self httpGetDocument: newUrl args: args  accept: mimeType] ].			aStream _ sock getRestOfBuffer: firstData totalLength: length.			"a 400-series error"			sock responseCode first = $4 ifTrue: [^ header, aStream contents].			].	sock destroy.	"Always OK to destroy!!"	aStream class ~~ String ifTrue: [ 		^ MIMEDocument contentType: type content: aStream contents url: url].	aStream = 'server aborted early' ifFalse: [		]	].{'HTTPSocket class>>httpGetDocument:args:accept:request:'. aStream. url} inspect.	^'some other bad thing happened!!'! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/13/2002 14:18'!httpPostMultipart: url args: argsDict accept: mimeType request: requestString	" do multipart/form-data encoding rather than x-www-urlencoded "	" by Bolot Kerimbaev, 1998 "	" this version is a memory hog: puts the whole file in memory "	"bolot 12/14/2000 18:28 -- minor fixes to make it comply with RFC 1867"	| serverName serverAddr s header length bare page list firstData aStream port argsStream specifiedServer type newUrl mimeBorder fieldValue |	Socket initializeNetwork.	"parse url"	bare _ (url asLowercase beginsWith: 'http://') 		ifTrue: [url copyFrom: 8 to: url size]		ifFalse: [url].	serverName _ bare copyUpTo: $/.	specifiedServer _ serverName.	(serverName includes: $:) ifFalse: [ port _ self defaultPort ] ifTrue: [		port _ (serverName copyFrom: (serverName indexOf: $:) + 1 to: serverName size) asNumber.		serverName _ serverName copyUpTo: $:.	].	page _ bare copyFrom: (bare indexOf: $/) to: bare size.	page size = 0 ifTrue: [page _ '/'].	(self shouldUseProxy: serverName) ifTrue: [ 		page _ 'http://', serverName, ':', port printString, page.		"put back together"		serverName _ HTTPProxyServer.		port _ HTTPProxyPort].	mimeBorder _ '----squeak-georgia-tech-', Time millisecondClockValue printString, '-csl-cool-stuff-----'.	"encode the arguments dictionary"	argsStream _ WriteStream on: String new.	argsDict associationsDo: [:assoc |		assoc value do: [ :value |		"print the boundary"		argsStream nextPutAll: '--', mimeBorder, CrLf.		" check if it's a non-text field "		argsStream nextPutAll: 'Content-disposition: multipart/form-data; name="', assoc key, '"'.		(value isKindOf: MIMEDocument)			ifFalse: [fieldValue _ value]			ifTrue: [argsStream nextPutAll: ' filename="', value url pathForFile, '"', CrLf, 'Content-Type: ', value contentType.				fieldValue _ (value content					ifNil: [(FileStream fileNamed: value url pathForFile) contentsOfEntireFile]					ifNotNil: [value content]) asString]." Transcript show: 'field=', key, '; value=', fieldValue; cr. "		argsStream nextPutAll: CrLf, CrLf, fieldValue, CrLf.	]].	argsStream nextPutAll: '--', mimeBorder, '--'.  	"make the request"		self retry: [serverAddr _ NetNameResolver addressForName: serverName timeout: 20.				serverAddr ~~ nil] 		asking: 'Trouble resolving server name "' , serverName , '".  Keep trying?'		ifGiveUp: [^ 'Could not resolve the server named: ', serverName].	s _ HTTPSocket new.	s connectTo: serverAddr port: port.	s waitForConnectionUntil: self standardDeadline.	Transcript cr; show: serverName, ':', port asString; cr.	s sendCommand: 'POST ', page, ' HTTP/1.1', CrLf, 		(mimeType ifNotNil: ['ACCEPT: ', mimeType, CrLf] ifNil: ['']),		'ACCEPT: text/html', CrLf,	"Always accept plain text"		HTTPBlabEmail,	"may be empty"		requestString,	"extra user request. Authorization"		self userAgentString, CrLf,		'Content-type: multipart/form-data; boundary=', mimeBorder, CrLf,		'Content-length: ', argsStream contents size printString, CrLf,		'Host: ', specifiedServer, CrLf.  "blank line automatically added"	s sendCommand: argsStream contents.	"get the header of the reply"	list _ s getResponseUpTo: CrLf, CrLf.	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	"Transcript show: page; cr; show: argsStream contents; cr; show: header; cr."	firstData _ list at: 3.	"dig out some headers"	s header: header.	length _ s getHeader: 'content-length'.	length ifNotNil: [ length _ length asNumber ].	type _ s getHeader: 'content-type'.	s responseCode first = $3 ifTrue: [		"redirected - don't re-post automatically"		"for now, just do a GET, without discriminating between 301/302 codes"		newUrl _ s getHeader: 'location'.		newUrl ifNotNil: [			(newUrl beginsWith: 'http://')				ifFalse: [					(newUrl beginsWith: '/')						ifTrue: [newUrl _ (bare copyUpTo: $/), newUrl]						ifFalse: [newUrl _ url, newUrl. self flag: #todo							"should do a relative URL"]				].			Transcript show: 'redirecting to: ', newUrl; cr.			s destroy.			^self httpGetDocument: newUrl			"for some codes, may do:			^self httpPostMultipart: newUrl args: argsDict  accept: mimeType request: requestString"] ].	aStream _ s getRestOfBuffer: firstData totalLength: length.	s responseCode = '401' ifTrue: [^ header, aStream contents].	s destroy.	"Always OK to destroy!!"	^ MIMEDocument contentType: type  content: aStream contents url: url! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/13/2002 14:19'!httpPostToSuperSwiki: url args: argsDict accept: mimeType request: requestString	| serverName serverAddr s header length bare page list firstData aStream port specifiedServer type mimeBorder contentsData |	Socket initializeNetwork.	"parse url"	bare _ (url asLowercase beginsWith: 'http://') 		ifTrue: [url copyFrom: 8 to: url size]		ifFalse: [url].	serverName _ bare copyUpTo: $/.	specifiedServer _ serverName.	(serverName includes: $:) ifFalse: [ port _ self defaultPort ] ifTrue: [		port _ (serverName copyFrom: (serverName indexOf: $:) + 1 to: serverName size) asNumber.		serverName _ serverName copyUpTo: $:.	].	page _ bare copyFrom: (bare indexOf: $/ ifAbsent: [^'error']) to: bare size.	page size = 0 ifTrue: [page _ '/'].		(self shouldUseProxy: serverName) ifTrue: [ 		page _ 'http://', serverName, ':', port printString, page.		"put back together"		serverName _ HTTPProxyServer.		port _ HTTPProxyPort].	mimeBorder _ '---------SuperSwiki',Time millisecondClockValue printString,'-----'.	contentsData _ String streamContents: [ :strm |		strm nextPutAll: mimeBorder, CrLf.		argsDict associationsDo: [:assoc |			assoc value do: [ :value |				strm					nextPutAll: 'Content-disposition: form-data; name="', assoc key, '"';					nextPutAll: CrLf;					nextPutAll: CrLf;					nextPutAll: value;					nextPutAll: CrLf;					nextPutAll: CrLf;					nextPutAll: mimeBorder;					nextPutAll: CrLf.			]		].	].  	"make the request"		self retry: [serverAddr _ NetNameResolver addressForName: serverName timeout: 20.				serverAddr ~~ nil] 		asking: 'Trouble resolving server name "' , serverName , '".  Keep trying?'		ifGiveUp: [^ 'Could not resolve the server named: ', serverName].	s _ HTTPSocket new.	s connectTo: serverAddr port: port.	s waitForConnectionUntil: self standardDeadline.	s sendCommand: 'POST ', page, ' HTTP/1.1', CrLf, 		(mimeType ifNotNil: ['ACCEPT: ', mimeType, CrLf] ifNil: ['']),		'ACCEPT: text/html', CrLf,	"Always accept plain text"		HTTPBlabEmail,	"may be empty"		requestString,	"extra user request. Authorization"		self userAgentString, CrLf,		'Content-type: multipart/form-data; boundary=', mimeBorder, CrLf,		'Content-length: ', contentsData size printString, CrLf,		'Host: ', specifiedServer, CrLf.  "blank line automatically added"	s sendCommand: contentsData.	list _ s getResponseUpTo: CrLf, CrLf.	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	firstData _ list at: 3.	header isEmpty ifTrue: [		s destroy.		^'no response'	].	s header: header.	length _ s getHeader: 'content-length'.	length ifNotNil: [ length _ length asNumber ].	type _ s getHeader: 'content-type'.	aStream _ s getRestOfBuffer: firstData totalLength: length.	s responseCode = '401' ifTrue: [^ header, aStream contents].	s destroy.	"Always OK to destroy!!"	^ MIMEDocument contentType: type  content: aStream contents url: url! !!HTTPSocket class methodsFor: 'get the page' stamp: 'hg 2/13/2002 14:19'!httpPut: contents to: url user: user passwd: passwd	"Upload the contents of the stream to a file on the server"	| bare serverName specifiedServer port page serverAddr authorization s list header firstData length aStream command |	Socket initializeNetwork. 	"parse url"	bare _ (url asLowercase beginsWith: 'http://') 		ifTrue: [url copyFrom: 8 to: url size]		ifFalse: [url].	serverName _ bare copyUpTo: $/.	specifiedServer _ serverName.	(serverName includes: $:) ifFalse: [ port _ self defaultPort ] ifTrue: [		port _ (serverName copyFrom: (serverName indexOf: $:) + 1 				to: serverName size) asNumber.		serverName _ serverName copyUpTo: $:.	].	page _ bare copyFrom: (bare indexOf: $/) to: bare size.	page size = 0 ifTrue: [page _ '/'].	(self shouldUseProxy: serverName) ifTrue: [ 		page _ 'http://', serverName, ':', port printString, page.		"put back together"		serverName _ HTTPProxyServer.		port _ HTTPProxyPort].  	"make the request"		self retry: [serverAddr _ NetNameResolver addressForName: serverName timeout: 20.				serverAddr ~~ nil] 		asking: 'Trouble resolving server name "' , serverName , '".  Keep trying?'		ifGiveUp: [^ 'Could not resolve the server named: ', serverName].	authorization _ (Base64MimeConverter mimeEncode: (user , ':' , passwd) readStream) contents.	s _ HTTPSocket new.	s connectTo: serverAddr port: port.	s waitForConnectionUntil: self standardDeadline.	Transcript cr; show: url; cr.	command _ 		'PUT ', page, ' HTTP/1.0', CrLf, 		self userAgentString, CrLf,		'Host: ', specifiedServer, CrLf, 		'ACCEPT: */*', CrLf,		'Authorization: Basic ' , authorization , CrLf , 		'Content-length: ', contents size printString, CrLf , CrLf , 		contents.	s sendCommand: command.	"get the header of the reply"	list _ s getResponseUpTo: CrLf, CrLf ignoring: (String with: CR).	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	"Transcript show: page; cr; show: argsStream contents; cr; show: header; cr."	firstData _ list at: 3.	"dig out some headers"	s header: header.	length _ s getHeader: 'content-length'.	length ifNotNil: [ length _ length asNumber ].	aStream _ s getRestOfBuffer: firstData totalLength: length.	s destroy.	"Always OK to destroy!!"	^ header, aStream contents! !!HTTPSocket class methodsFor: 'utilities' stamp: 'hg 2/11/2002 11:32'!httpPostDocument: url  args: argsDict accept: mimeType request: requestString	"like httpGET, except it does a POST instead of a GET.  POST allows data to be uploaded"	| s header length page list firstData aStream type newUrl httpUrl argString |	Socket initializeNetwork.	httpUrl _ Url absoluteFromText: url.	page _ httpUrl fullPath.	"add arguments"	argString _ argsDict		ifNotNil: [argString _ self argString: argsDict]		ifNil: [''].	page _ page, argString.	s _ HTTPSocket new. 	s _ self initHTTPSocket: httpUrl wait: (self deadlineSecs: 30) ifError: [:errorString | ^errorString].	Transcript cr; show: url; cr.	s sendCommand: 'POST ', page, ' HTTP/1.0', CrLf, 		(mimeType ifNotNil: ['ACCEPT: ', mimeType, CrLf] ifNil: ['']),		'ACCEPT: text/html', CrLf,	"Always accept plain text"		HTTPBlabEmail,	"may be empty"		requestString,	"extra user request. Authorization"		self userAgentString, CrLf,		'Content-type: application/x-www-form-urlencoded', CrLf,		'Content-length: ', argString size printString, CrLf,		'Host: ', httpUrl authority, CrLf.  "blank line automatically added"	s sendCommand: argString.	"get the header of the reply"	list _ s getResponseUpTo: CrLf, CrLf ignoring: (String with: CR).	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	"Transcript show: page; cr; show: argsStream contents; cr; show: header; cr."	firstData _ list at: 3.	"dig out some headers"	s header: header.	length _ s getHeader: 'content-length'.	length ifNotNil: [ length _ length asNumber ].	type _ s getHeader: 'content-type'.	s responseCode first = $3 ifTrue: [		newUrl _ s getHeader: 'location'.		newUrl ifNotNil: [			Transcript show: 'Response: ' , s responseCode.			Transcript show: ' redirecting to: ', newUrl; cr.			s destroy.			"^self httpPostDocument: newUrl  args: argsDict  accept: mimeType"			^self httpGetDocument: newUrl accept: mimeType ] ].	aStream _ s getRestOfBuffer: firstData totalLength: length.	s responseCode = '401' ifTrue: [^ header, aStream contents].	s destroy.	"Always OK to destroy!!"	^ MIMEDocument contentType: type  content: aStream contents url: url! !!HTTPSocket class methodsFor: 'utilities' stamp: 'hg 2/11/2002 11:31'!userAgentString 	"self userAgentString"	^'User-Agent: ',		SystemVersion current version, '-', 		SystemVersion current highestUpdate printString! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/5/2002 16:50'!fileExists: fileName	"Does the file exist on this server directory?  fileName must be simple with no / or references to other directories."	| stream |	self isTypeFile ifTrue: [^ self fileNames includes: fileName].	self isTypeHTTP ifTrue: [		stream _ self readOnlyFileNamed: fileName.		^stream contents notEmpty].	"ftp"	^ self entries anySatisfy: [:entry | entry name = fileName]! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/4/2002 09:42'!getDirectory	"Return a stream with a listing of the current server directory.  (Later -- Use a proxy server if one has been registered.)"	| so dd resp rr |	so _ self openFTP.	"Open passive.  Do everything up to RETR or STOR"	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'LIST'.	dd _ so dataSocket.	dd connectTo: so remoteAddress port: dd portNum.	dd waitForConnectionUntil: FTPSocket standardDeadline.	"Transcript show: 'getting directory LIST'; cr."	"Transcript show: 'retrieve from port ', dd portNum printString; cr."	resp _ dd getAllDataWhileWatching: so.	"Later use the length to pre-allocate the buffer"	(resp == #error:) ifTrue: [socket _ nil.  ^ resp].	dd close.	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"150 Opening binary conn on foo (3113 bytes)"	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"226 Transfer complete."	socket ifNil: [		so sendCommand: 'QUIT'.		(rr _ so responseOK) == true ifFalse: [^ rr].	"221"		so destroy].	"Always OK to destroy"	dd destroy.	^ resp	"RWStream with just the data"! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/11/2002 20:35'!getFileList	"Return a stream with a list of files in the current server directory.  (Later -- Use a proxy server if one has been registered.)"	| so dd resp rr |	so _ self openFTP.	"Open passive.  Do everything up to RETR or STOR"	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'NLST'.	dd _ so dataSocket.	dd connectTo: so remoteAddress port: dd portNum.	dd waitForConnectionUntil: FTPSocket standardDeadline.	"Transcript show: 'getting file list NLST'; cr."	"Transcript show: 'retrieve from port ', dd portNum printString; cr."	resp _ dd getAllDataWhileWatching: so.	"Later use the length to pre-allocate the buffer"	(resp == #error:) ifTrue: [socket _ nil.  ^ resp].	dd close.	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"150 Opening binary conn on foo (3113 bytes)"	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"226 Transfer complete."	socket ifNil: [		so sendCommand: 'QUIT'.		(rr _ so responseOK) == true ifFalse: [^ rr].	"221"		so destroy].	"Always OK to destroy"	dd destroy.	^ resp	"RWStream with just the data"! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/11/2002 20:36'!getFileNamed: fileNameOnServer	"Just FTP a file from a server.  Return a stream.  (Later -- Use a proxy server if one has been registered.)"	| so dd resp rr |	so _ self openFTP.	"Open passive.  Do everything up to RETR or STOR"	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'RETR ', fileNameOnServer.	dd _ so dataSocket.	dd connectTo: so remoteAddress port: dd portNum.	dd waitForConnectionUntil: FTPSocket standardDeadline.	"Transcript show: 'retrieving file ', fileNameOnServer; cr."	"Transcript show: 'retrieve from port ', dd portNum printString; space;		show: fileNameOnServer; cr."	resp _ dd getAllDataWhileWatching: so.	"Later use the length to pre-allocate the buffer"	(resp == #error:) ifTrue: [socket _ nil.  ^ resp].	dd close.	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"150 Opening binary conn on foo (3113 bytes)"	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"226 Transfer complete."	socket ifNil: ["normally leave connection open.  Don't quit"		so sendCommand: 'QUIT'.		(rr _ so responseOK) == true ifFalse: [^ rr].	"221"		so destroy].	"Always OK to destroy"	dd destroy.	^ resp	"a RWBinaryOrTextStream"! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/12/2002 11:44'!getFileNamed: fileNameOnServer into: dataStream		^self getFileNamed: fileNameOnServer into: dataStream 		httpRequest: 'Pragma: no-cache', String crlf! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/12/2002 11:40'!getFileNamed: fileNameOnServer into: dataStream httpRequest: requestString	"Just FTP a file from a server.  Return a stream.  (Later -- Use a proxy server if one has been registered.)"	| so dd resp rr |	self isTypeFile ifTrue: [		dataStream nextPutAll: 			(resp _ FileStream oldFileNamed: server,(self serverDelimiter asString), 				self bareDirectory, (self serverDelimiter asString),				fileNameOnServer) contentsOfEntireFile.		dataStream dataIsValid.		^ resp].	self isTypeHTTP ifTrue: [		resp _ HTTPSocket httpGet: (self fullNameFor: fileNameOnServer) 				args: nil accept: 'application/octet-stream' request: requestString.		resp class == String ifTrue: [^ dataStream].	"error, no data"		dataStream copyFrom: resp.		dataStream dataIsValid.		^ dataStream].	so _ self openFTP.	"Open passive.  Do everything up to RETR or STOR"	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'RETR ', fileNameOnServer.	dd _ so dataSocket.	dd connectTo: so remoteAddress port: dd portNum.	dd waitForConnectionUntil: FTPSocket standardDeadline.	Transcript show: 'retrieving file ', fileNameOnServer; cr.	"Transcript show: 'retrieve from port ', dd portNum printString; cr."	resp _ dd getDataTo: dataStream whileWatching: so.		"Later use the length to pre-allocate the buffer"	(resp == #error:) ifTrue: [socket _ nil.  ^ resp].	dd close.	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"150 Opening binary conn on foo (3113 bytes)"	(rr _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"226 Transfer complete."	socket ifNil: ["normally leave connection open.  Don't quit"		so sendCommand: 'QUIT'.		(rr _ so responseOK) == true ifFalse: [^ rr].	"221"		so destroy].	"Always OK to destroy"	dd destroy.	dataStream dataIsValid.	^ resp	"String with just the data"! !!ServerDirectory methodsFor: 'up/download' stamp: 'hg 2/11/2002 20:35'!putFile: fileStream named: fileNameOnServer	"Just FTP a local fileStream to the server.  (Later -- Use a proxy server if one has been registered.)"	| so dd resp rr |	so _ self openFTP.	"Open passive.  Do everything up to RETR or STOR"	so class == String ifTrue: ["error, was reported" ^ so].	so sendCommand: 'STOR ', fileNameOnServer.	dd _ so dataSocket.	dd connectTo: so remoteAddress port: dd portNum.	dd waitForConnectionUntil: FTPSocket standardDeadline.	"Transcript show: 'storing file ', fileNameOnServer; cr."	"Transcript show: 'store via port ', dd portNum printString; cr."	dd sendData: fileStream contentsOfEntireFile.	dd close.	(rr _ so responseOK) == true ifFalse: [		(rr beginsWith: '426 ') ifTrue: [socket _ nil.  ^ rr, ' (Server may be full.)'].			"Connection closed, transfer aborted"		socket _ nil.  ^ rr].	"150 Opening BINARY mode data connection"	(resp _ so responseOK) == true ifFalse: [		socket _ nil.  ^ rr].	"226 Transfer complete."	socket ifNil: [		so sendCommand: 'QUIT'.		(rr _ so responseOK) == true ifFalse: [^ rr].	"221"		so destroy].	"Always OK to destroy"	dd destroy.	^ resp	"226 Transfer complete."! !!ServerDirectory methodsFor: 'dis/connect' stamp: 'hg 2/11/2002 21:30'!openNoDataFTP	"Open a connection to the directory and server I hold.  Return a FTPSocket.  No dataPort is opened.  When you are all done, be sure to tell the socket to QUIT, and then destroy it."	| so rr serverIP what |	Socket initializeNetwork.	socket ifNotNil: [socket isValid 			ifTrue: [^ socket]	"already open"			ifFalse: [socket _ nil]].	Cursor wait showWhile: [		FTPSocket retry: [serverIP _ NetNameResolver addressForName: server timeout: 20.					serverIP ~~ nil] 			asking: 'Trouble resolving server name "' , server , '".  Keep trying?'			ifGiveUp: [^ 'Could not resolve the server named: ', server].		so _ FTPSocket new.		so portNum: 21.		so connectTo: serverIP port: 21.  "21 is for the control connection"		so waitForConnectionUntil: FTPSocket standardDeadline.		].	"Transcript cr; show: 'ftp: ', server; cr."	(rr _ so lookFor: '220 ') == true ifFalse: [^ rr].	"220 para1 Microsoft FTP Service"	[	"repeat both USER and PASS since some servers require it"		so sendCommand: 'USER ', user.		(rr _ so lookFor: '331 ') == true ifFalse: [^ rr].	"331 Password required"		so sendCommand: 'PASS ', self password.		"will ask user, if needed"		 (rr _ so lookSoftlyFor: '230 ') == true	] 	"230 User logged in"		whileFalse: [			rr first == $5 ifFalse: [^ rr].	"timeout"			passwordHolder _ nil.			what _ (PopUpMenu labels: 'enter password\give up' withCRs) 				startUpWithCaption: 'Would you like to try another password?'.			what = 1 ifFalse: [so destroy.  ^ rr]].	(rr _ self changeWorkingDirectory: so) == true ifFalse: [^rr].	"Need to ask for name of directory to make sure?"	"socket _ so".	"If user wants to keep connnection open, he must store socket"	^ so! !!ServerDirectory methodsFor: 'dis/connect' stamp: 'hg 2/11/2002 20:15'!quit	"break the connection"	| rr so |	(so _ socket) ifNil: [^ self].	"already done"	socket _ nil.	so isValid ifFalse: [^ self].	so sendCommand: 'QUIT'.	(rr _ so responseOK) == true ifFalse: [^ rr].	"221"	so logToTranscript ifTrue: [		Transcript cr; show: 'ftp closing: ', server].	so destroy.	"Always OK to destroy"! !!ServerDirectory methodsFor: 'file directory' stamp: 'hg 2/3/2002 17:25'!assureExistence	"Make sure the current directory exists. If necessary, create all parts inbetween"		self exists ifFalse: [		self isRoot ifFalse: [			self containingDirectory assurePathExists: self localName]]! !!ServerDirectory methodsFor: 'file directory' stamp: 'hg 2/8/2002 17:44'!directoryNamed: localFileName 	"Return a copy of me pointing at this directory below me"	| new newPath newAltUrl |	new _ self copy.	urlObject		ifNotNil: [new urlObject path: new urlObject path copy.			new urlObject path removeLast; addLast: localFileName; addLast: ''.			^ new].	"sbw.  When working from an FTP server, the first time we access	a subdirectory the <directory> variable is empty.  In that case we	cannot begin with a leading path delimiter since that leads us to	the wrong place."	newPath _ directory isEmpty				ifTrue: [localFileName]				ifFalse: [directory , self pathNameDelimiter asString , localFileName].	self altUrl ifNotNil: [		newAltUrl _ self altUrl, self pathNameDelimiter asString , localFileName].	new directory: newPath; altUrl: newAltUrl.	^ new! !!ServerDirectory methodsFor: 'file directory' stamp: 'hg 2/11/2002 21:30'!exists	"It is difficult to tell if a directory exists.  This is ugly, but it works for writable directories.  http: will fall back on ftp for this"	| probe success |	success _ false.	self isTypeFile ifTrue: [		self entries size > 0 ifTrue: [^ true].		probe _ self newFileNamed: 'withNoName23'. 		probe ifNotNil: [			probe close.			probe directory deleteFileNamed: probe localName].		^success _ probe notNil].	[self isAwake		ifFalse: [			self openNoDataFTP ifNotNilDo: [:sock |				  sock sendCommand: 'QUIT'; destroy].	"Always OK to destroy"			success _ true]		ifTrue: [success _ (self changeWorkingDirectory: socket) == true]	 ] on: Error do: [:ex | ].	^success! !!ServerDirectory methodsFor: 'file directory' stamp: 'hg 2/8/2002 00:04'!fileNames	"Return a collection of names for the files (but not directories) in this directory."	"(ServerDirectory serverNamed: 'UIUCArchive') fileNames"	^ self entries select: [:entry | (entry at: 4) not]		thenCollect: [:entry | entry first]! !!ServerDirectory methodsFor: 'file directory' stamp: 'hg 2/8/2002 17:39'!fullNameFor: aFileName	"Convention: 	If it is an absolute path, directory stored with a leading slash, and url has no user@.	If relative path, directory stored with no leading slash, and url begins user@.	Should we include ftp:// on the front?"	urlObject ifNotNil: [^ urlObject pathString, aFileName].	(aFileName includes: self pathNameDelimiter)		ifTrue: [^ aFileName].	self isTypeHTTP ifTrue: [		^ self downloadUrl, aFileName].	directory isEmpty ifTrue: [^ server, 		self pathNameDelimiter asString, aFileName].	^ (directory first == $/ ifTrue: [''] ifFalse: [user,'@']), 		server, self slashDirectory, 		self pathNameDelimiter asString, aFileName! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 21:31'!changeWorkingDirectory: sock	"use this to work in different remote directories in the same session.  Ie. when multiple instances of my class use the same session (socket)"	| rr |	directory isEmpty ifFalse: [		sock sendCommand: 'CWD ', directory.		(rr _ sock lookFor: '250 ') == true ifFalse: [^ rr].	"250 CWD successful"	].	^true! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 18:32'!directoryNamed: localFileName usingSameSession: aBoolean	| new |	new _ self directoryNamed: localFileName.	aBoolean ifTrue: [new useSocket: socket].	^new! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 21:31'!ensureCorrectWorkingDirectory	"useful if multiple directories share one socket/session"	self isAwake ifFalse: [^self].	"no need"	(self changeWorkingDirectory: socket) == true ifFalse: [		self error: 'Couldn''t change working directory.'.].	^true! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 18:21'!isAwake	^socket notNil! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 18:32'!useSocket: aSocket	socket _ aSocket! !!ServerDirectory methodsFor: 'multi-action sessions' stamp: 'hg 2/11/2002 18:35'!wakeUp	"Start a multi-action session: Open for FTP and keep the connection open"	| so |	(so _ self openNoDataFTP) class == String ifTrue: [^ so].	^ socket _ so! !!ServerDirectory reorganize!('accessing' acceptsUploads: altUrl altUrl: bareDirectory copy directory directory: directoryObject downloadUrl fullPath: isTypeFTP isTypeFile isTypeHTTP loaderUrl loaderUrl: moniker moniker: password password: passwordSequence passwordSequence: printOn: realUrl server server: slashDirectory type: typeForPrefs typeWithDefault url url: urlObject urlObject: user user:)('testing' acceptsUploads isProjectSwiki isRoot isSearchable)('up/download' fileExists: getDirectory getFileList getFileNamed: getFileNamed:into: getFileNamed:into:httpRequest: getOnlyBuffer:from: putFile:named: putFile:named:retry: putFileSavingOldVersion:named:)('dis/connect' openFTP openNoDataFTP quit)('file directory' asServerFileNamed: assureExistence assurePathExists: containingDirectory createDirectory: deleteDirectory: deleteFileNamed: directoryNamed: directoryNames entries exists fileAndDirectoryNames fileNamed: fileNames fullNameFor: getOnly:from: includesKey: localName localNameFor: localPathExists: matchingEntries: newFileNamed: oldFileNamed: oldFileOrNoneNamed: on: pathName pathNameDelimiter pathParts readOnlyFileNamed: rename:toBe: serverDelimiter splitName:to: streamOnBeginningOf:)('multi-action sessions' changeWorkingDirectory: directoryNamed:usingSameSession: ensureCorrectWorkingDirectory isAwake reset sleep useSocket: wakeUp)('server groups' closeGroup convertGroupName groupName groupName: openGroup serversInGroup)('initialize' fromUser)('squeaklets' directoryWrapperClass moveAllButYoungest:in:to: upLoadProject:members:retry: upLoadProject:named:resourceUrl:retry: updateProjectInfoFor:)('file-in/out' storeServerEntryOn:)('updates' checkNames: checkServersWithPrefix:andParseListInto: copyUpdatesNumbered:toVersion: exportUpdatesExcept: outOfDate: putUpdate: putUpdateMulti:fromDirectory: updateInstallVersion:)('school support' eToyUserList eToyUserList: eToyUserListUrl eToyUserListUrl: eToyUserName: hasEToyUserList)!HTTPSocket class removeSelector: #initHTTPSocketFromServerName:port:wait:ifError:!