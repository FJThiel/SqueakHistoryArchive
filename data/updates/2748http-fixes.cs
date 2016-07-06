'From Squeak2.9alpha of 25 July 2000 [latest update: #2776] on 29 September 2000 at 2:31:31 pm'!"Change Set:		http-fixesDate:			29 September 2000Author:			Michael RuegerFixes a recently introduced problem with http get where in the get the hostname was passed and not just the full path."!!HTTPSocket class methodsFor: 'get the page' stamp: 'mir 9/29/2000 14:31'!httpGetDocument: url args: args accept: mimeType request: requestString	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. An extra requestString may be submitted and must end with crlf.  The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIME type 'application/octet-stream'."	| httpUrl page sock list header firstData aStream length type newUrl command |	Socket initializeNetwork.	httpUrl _ Url absoluteFromText: url.	page _ httpUrl fullPath.	"add arguments"	args ifNotNil: [page _ page, (self argString: args) ].3 timesRepeat: [	sock _ self initHTTPSocket: httpUrl ifError: [:errorString | ^errorString].	command _ 'GET ', page, ' HTTP/1.0', CrLf, 		(mimeType ifNotNil: ['ACCEPT: ', mimeType, CrLf] ifNil: ['ACCEPT: */* ']),		" 'ACCEPT: text/html', CrLf, "	"Always accept plain text"		HTTPBlabEmail,	"may be empty"		requestString,	"extra user request. Authorization"		'User-Agent: Squeak 1.31', CrLf ,		'Host: ', httpUrl authority.	httpUrl port ifNotNil: [		command _ command , ':', httpUrl port printString].	command _ command , CrLf.	"blank line automatically added"	"Transcript cr; cr; show: command; cr."	sock sendCommand: command.	list _ sock getResponseUpTo: CrLf, CrLf ignoring: (String with: CR).	"list = header, CrLf, CrLf, beginningOfData"	header _ list at: 1.	"Transcript show: page; cr; show: header; cr."	firstData _ list at: 3.	header isEmpty 		ifTrue: [aStream _ 'server aborted early']		ifFalse: [			"dig out some headers"			sock header: header.			length _ sock getHeader: 'content-length'.			length ifNotNil: [ length _ length asNumber ].			type _ sock getHeader: 'content-type'.			sock responseCode first = $3 ifTrue: [				newUrl _ sock getHeader: 'location'.				newUrl ifNotNil: [ 					Transcript show: 'redirecting to ', newUrl; cr.					sock destroy.					^self httpGetDocument: httpUrl authority , '/' , newUrl  args: args  accept: mimeType ] ].			aStream _ sock getRestOfBuffer: firstData totalLength: length.			sock responseCode first > $3				ifTrue: [^ header, aStream contents].			].	sock destroy.	"Always OK to destroy!!"	aStream class ~~ String ifTrue: [ 		^ MIMEDocument contentType: type content: aStream contents url: url].	^aStream"	aStream = 'server aborted early' ifFalse: [		self halt.		^aStream]"	].! !!HierarchicalUrl methodsFor: 'printing' stamp: 'mir 9/29/2000 14:16'!fullPath	| ans |	ans _ WriteStream on: String new.	path do: [ :pathElem |		ans nextPut: $/.		ans nextPutAll: pathElem encodeForHTTP. ].	self query isNil ifFalse: [ 		ans nextPut: $?.		ans nextPutAll: self query. ].	self fragment isNil ifFalse: [		ans nextPut: $#.		ans nextPutAll: self fragment encodeForHTTP. ].		^ans contents! !