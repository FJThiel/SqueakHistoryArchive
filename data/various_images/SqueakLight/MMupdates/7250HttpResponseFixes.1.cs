'From MinimalMorphic of 8 December 2006 [latest update: #7258] on 11 December 2009 at 6:53:16 am'!!HttpResponse commentStamp: '<historical>' prior: 0!kom/4.12 (Comanche/4.12)bolot 2/20/2002 13:55- cookies support- defaultChunkSize delegates to Kom5PreferencesHttpResponse (bolot 4/2/2001 18:52)Comment from kom46:I am a response to an HttpRequest.  I can formulate an HTTP response and send it out over a socket.  An HttpAdapter will accept an HttpRequest, dispatch a method call to an HttpPlug (which will result in a stream or an error), and then formulat an instance of me to deliver the response to the client.!!HttpResponse methodsFor: 'accessing-cookies' stamp: 'svp 4/15/2003 01:08'!addCookies: newCookies		self cookies addAll: newCookies.! !!HttpResponse methodsFor: 'accessing-cookies' stamp: 'DGD 7/20/2001 00:25'!cookies^ cookies ifNil:[cookies := OrderedCollection new]! !!HttpResponse methodsFor: 'accessing-cookies' stamp: 'DGD 7/20/2001 00:30'!setCookieName: nameString value: valueString expiresDate: expiresDate expiresTime: expiresTime path: pathString domain: domainString secure: secureBoolean 	"set a cookie in the Response"	| temp |	temp := String new writeStream.	"NAME=VALUE"	temp nextPutAll: nameString;		 nextPut: $=;		 nextPutAll: valueString.	"; expires=Wdy, DD-Mon-YYYY HH:MM:SS GMT"	(expiresDate notNil and:[ expiresTime notNil])		ifTrue: [temp nextPutAll: '; expires=';				 nextPutAll: expiresDate weekday;				 nextPutAll: ', '.			expiresDate printOn: temp format: #(1 2 3 $- 2 2 2 ).			temp nextPutAll: ' ';				 nextPutAll: expiresTime print24;				 nextPutAll: ' GMT'].	"; path=PATH"	pathString notNil		ifTrue: [temp nextPutAll: '; path=';				 nextPutAll: pathString].	"; domain=DOMAIN"	domainString notNil		ifTrue: [temp nextPutAll: '; domain=';				 nextPutAll: domainString].	"; secure"	(secureBoolean notNil			and: [secureBoolean])		ifTrue: [temp nextPutAll: '; secure'].	""	self cookies add: temp contents! !!HttpResponse methodsFor: 'accessing-cookies' stamp: 'DGD 7/20/2001 00:24'!setCookieName: nameString value: valueString path: pathString 	"set a cookie in the Response"	self		setCookieName: nameString		value: valueString		expiresDate: nil		expiresTime: nil		path: pathString		domain: nil		secure: nil! !!HttpResponse methodsFor: 'comanche processing' stamp: 'bolot 1/11/2001 17:51'!asHttpPartialResponseBlock: aBlock	^(self as: HttpPartialResponse)		producerBlock: aBlock;		yourself! !!HttpResponse methodsFor: 'comanche processing'!asHttpResponseTo: aRequest	^self! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 5/16/2003 23:54'!contentLength	^contentLength! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/16/1999 11:54'!contentType	^contentType! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/16/1999 11:54'!contentType: aMimeType	contentType := aMimeType! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/16/1999 11:53'!contents	^contents! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 5/16/2003 23:54'!contents: aStream	contents := aStream.	contentLength := aStream size.! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 11/17/1999 11:08'!fieldAt: aString	^self fields at: aString! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 11/17/1999 11:08'!fieldAt: aString ifAbsent: absBlock	^self fields at: aString ifAbsent: absBlock! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 11/17/1999 11:08'!fieldAt: aString ifAbsentPut: absBlock	^self fields at: aString ifAbsentPut: absBlock! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 11/17/1999 11:07'!fieldAt: aString put: aValue	self fields at: aString put: aValue! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 4/22/2003 00:12'!fields	^fields ifNil: [fields := Dictionary new]! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 12/16/1999 11:33'!httpVersion	^'HTTP/1.1'! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 4/15/2003 01:00'!isPersistent	^(self fieldAt: 'Connection') = 'Keep-Alive'! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 5/7/2003 04:13'!responseChunkSize	^self class responseChunkSize! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/16/1999 11:53'!status	^status! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/16/1999 11:53'!status: aSymbol	status := aSymbol! !!HttpResponse methodsFor: 'accessing' stamp: 'svp 11/18/1999 13:03'!statusCode	^(StatusCodes at: self status) key.! !!HttpResponse methodsFor: 'accessing' stamp: 'SVP 8/26/1999 10:41'!statusCodeAndReason	| tmp |	tmp := StatusCodes at: self status.	^tmp key, ' ', tmp value.! !!HttpResponse methodsFor: 'defaults' stamp: 'SVP 8/12/1999 14:08'!defaultContentType	^MIMEDocument contentTypeHtml! !!HttpResponse methodsFor: 'initialize-release' stamp: 'SVP 8/26/1999 08:06'!destroy	self contents close.! !!HttpResponse methodsFor: 'authentication' stamp: 'bolot 3/11/2002 14:22'!hashPassword: aPassword	^HttpRequest hashPassword: aPassword! !!HttpResponse methodsFor: 'authentication' stamp: 'bolot 3/11/2002 14:22'!secretWord	^HttpRequest secretWord! !!HttpResponse methodsFor: 'authentication' stamp: 'bolot 3/11/2002 14:12'!setUsername: aUsername	"save the username in a cookie"	self		setCookieName: #ComancheUsername		value: aUsername		path: '/'.	self flag: #todo.	"allow for multiple usernames"! !!HttpResponse methodsFor: 'authentication' stamp: 'bolot 3/11/2002 17:30'!setUsername: aUsername password: aPassword	"save the username/password in cookies"	| pwHash |	self setUsername: aUsername.	pwHash := self hashPassword: aPassword.	self		setCookieName: #ComanchePassword		value: pwHash asString		path: '/'.	self flag: #todo.	"allow for multiple usernames"! !!HttpResponse methodsFor: 'responding' stamp: 'svp 5/16/2003 23:56'!pvtWriteContentLengthOn: aStream	self contentLength ifNotNilDo:		[ :length |		aStream			nextPutAll: 'Content-length: ';			nextPutAll: length asString;			nextPutAll: String crlf]! !!HttpResponse methodsFor: 'responding' stamp: 'bolot 12/16/2000 09:27'!pvtWriteContentTypeOn: aStream	aStream		nextPutAll: 'Content-type: ';		nextPutAll: self contentType;		nextPutAll: String crlf! !!HttpResponse methodsFor: 'responding' stamp: 'svp 5/7/2003 04:13'!pvtWriteContentsOn: aStream	| chunkSize cnts |	chunkSize := self responseChunkSize.	cnts := self contents.	[cnts atEnd] whileFalse: 		[aStream nextPutAll: (cnts next: chunkSize); flush]! !!HttpResponse methodsFor: 'responding' stamp: 'svp 4/15/2003 01:10'!pvtWriteCookiesOn: aStream 	self cookies		do: [:cookie | aStream nextPutAll: 'Set-Cookie: ';				 nextPutAll: cookie;				 nextPutAll: String crlf]! !!HttpResponse methodsFor: 'responding' stamp: 'svp 4/22/2003 00:19'!pvtWriteFieldsOn: aStream	fields isEmptyOrNil ifTrue: [ ^self ].	fields keysAndValuesDo: [ :k :v |		aStream			nextPutAll: k;			nextPutAll: ': ';			nextPutAll: v;			nextPutAll: String crlf	]! !!HttpResponse methodsFor: 'responding' stamp: 'bolot 12/16/2000 09:29'!pvtWriteStatusOn: aStream	aStream		nextPutAll: self httpVersion;		nextPut: $ ;		nextPutAll: self statusCodeAndReason;		nextPutAll: String crlf! !!HttpResponse methodsFor: 'responding' stamp: 'svp 5/15/2003 12:50'!writeHeadersOn: aStream	| tmp |	"Header"	tmp := WriteStream on: ''.	self pvtWriteStatusOn: tmp.	self pvtWriteFieldsOn: tmp.	self pvtWriteCookiesOn: tmp.	self pvtWriteContentTypeOn: tmp.	self pvtWriteContentLengthOn: tmp.	tmp nextPutAll: String crlf.	aStream nextPutAll: tmp contents; flush.! !!HttpResponse methodsFor: 'responding' stamp: 'svp 4/22/2003 01:13'!writeOn: aStream	self writeHeadersOn: aStream.	self pvtWriteContentsOn: aStream.! !!HttpResponse class methodsFor: 'bindings' stamp: 'svp 4/15/2003 15:06'!current	^#'HttpResponse-Current' binding! !!HttpResponse class methodsFor: 'bindings' stamp: 'svp 4/15/2003 15:07'!current: anHttpResponse	#'HttpResponse-Current' binding: anHttpResponse! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/12/2003 16:10'!fromFileStream: aStream	^self basicNew		status: #ok;		"fieldAt: 'Expires' put: 'Thu, 15 Apr 2010 20:00:00 GMT';"		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: aStream mimeType;		contents: aStream;		yourself	! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/13/2003 17:09'!fromMIMEDocument: aMIMEDoc	| content |	content := aMIMEDoc content.	content isString ifTrue:		[content := ReadStream on: content].	^self basicNew		status: #ok;		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: aMIMEDoc mimeType;		contents: content;		yourself	! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/12/2003 16:10'!fromStream: aStream	^self basicNew		status: #ok;		"fieldAt: 'Expires' put: 'Thu, 15 Apr 2010 20:00:00 GMT';"		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: aStream mimeType;		contents: aStream;		yourself	! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/13/2003 17:11'!fromStream: aStream contentType: mimeTypeString	^self basicNew		status: #ok;		"fieldAt: 'Expires' put: 'Thu, 15 Apr 2010 20:00:00 GMT';"		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: mimeTypeString;		contents: aStream;		yourself	! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/12/2003 16:10'!fromString: aString	^self basicNew		status: #ok;		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: MIMEDocument contentTypeHtml;		contents: (ReadStream on: aString);		yourself! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/13/2003 17:13'!fromString: aString contentType: contentTypeString	^self basicNew		status: #ok;		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: contentTypeString;		contents: (ReadStream on: aString);		yourself! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/12/2003 16:10'!fromUrl: aUrl	^self basicNew		status: #tempMoved;		fieldAt: 'Location' put: aUrl asString;		contentType: MIMEDocument contentTypeHtml;		contents: (ReadStream on: '<head><title>Object moved</title></head><body><h1>Object Moved</h1>This object may be found <a HREF="', aUrl asString,'">here</a>.</body>');		yourself! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/12/2003 16:28'!new	^self basicNew		status: #ok;		"fieldAt: 'Cache-Control' put: 'max-age=86400';"		contentType: MIMEDocument contentTypeHtml;		contents: (ReadStream on: 'This is a simple HttpResponse');		yourself! !!HttpResponse class methodsFor: 'instance creation' stamp: 'gk 10/22/2003 16:10'!redirectTo: url	"Create a redirection response."	^(HttpResponse			status: #tempMoved			contents: 'Temporarily moved to: <A HREF="', url, '">', url, '</A>')		fieldAt: 'Location' put: url;		fieldAt: 'URI' put: url; yourself! !!HttpResponse class methodsFor: 'instance creation' stamp: 'svp 5/16/2003 23:08'!status: aSymbol contents: aString	^self basicNew		status: aSymbol;		contentType: MIMEDocument contentTypeHtml;		contents: (ReadStream on: aString);		yourself! !!HttpResponse class methodsFor: 'initialization' stamp: 'SVP 8/16/1999 16:43'!initialize	"  HttpResponse initialize  "	self initializeStatusCodes.! !!HttpResponse class methodsFor: 'initialization' stamp: 'gc 10/22/2007 23:37'!initializeStatusCodes	"  HttpResponse initializeStatusCodes  "	StatusCodes := IdentityDictionary new		"1xx: Meta status codes"		at: #continue				put: ('100' -> 'Continue');		at: #switchingProtocols	put: ('101' -> 'Switching Protocols');		at: #processing			put: ('102' -> 'Processing'); "WebDav RFC: 2518"		"2xx: Success status codes"		at: #ok								put: ('200' -> 'OK');		at: #created							put: ('201' -> 'Created');		at: #accepted							put: ('202' -> 'Accepted');		at: #nonAuthoritativeInformation 	put: ('203' -> 'Non-Authoritative Information');		at: #noContent						put: ('204' -> 'No Content');		at: #resetContent	 					put: ('205' -> 'Reset Content');		at: #partialContent					put: ('206' -> 'Partial Content');		at: #multiStatus						put: ('207' -> 'Multi-Status'); "WebDav RFC: 2518"		"3xx: Redirection status codes"		at: #multipleChoices		put: ('300' -> 'Multiple Choices');		at: #permMoved			put: ('301' -> 'Moved Permanently'); "kept for compatibility"		at: #permanentlyMoved	put: ('301' -> 'Moved Permanently');		at: #found					put: ('302' -> 'Found');		at: #seeOther				put: ('303' -> 'See Other');										at: #notModified			put: ('304' -> 'Not Modified');		at: #useProxy				put: ('305' -> 'Use Proxy');		at: #temporaryRedirect	put: ('307' -> 'Temporary Redirect');		"4xx: Client-Side Error status code"		at: #badRequest				put: ('400' -> 'Bad Request');		at: #unauthorized				put: ('401' -> 'Unauthorized');		at: #payment					put: ('402' -> 'Payment Required');		at: #forbidden				put: ('403' -> 'Forbidden');		at: #notFound					put: ('404' -> 'Not Found');		at: #notAllowed				put: ('405' -> 'Method Not Allowed');		at: #notAcceptable			put: ('406' -> 'Not Acceptable');		at: #proxyAuthRequired		put: ('407' -> 'Proxy Authentication Required');		at: #requestTimeout			put: ('408' -> 'Request Timeout');		at: #conflict					put: ('409' -> 'Conflict');		at: #gone						put: ('410' -> 'Gone');		at: #lenghtRequired			put: ('411' -> 'Lenght Required');		at: #preconditionFailed		put: ('412' -> 'Precondition Failed');		at: #entityTooLarge			put: ('413' -> 'Request Entity Too Large');		at: #uriTooLong				put: ('414' -> 'Request-URI Too Long');		at: #unsupportedMediaType	put: ('415' -> 'Unsupported Media Type');		at: #rangeNotSatisfiable	put: ('416' -> 'Requested Range Not Satisfiable');		at: #expectationFailed		put: ('417' -> 'Expectation Failed');		at: #unprocessableEntity	put: ('422' -> 'Unprocessable Entity'); "WebDav RFC: 2518"		at: #locked					put: ('423' -> 'Locked'); "WebDav RFC: 2518"		at: #failedDependency		put: ('424' -> 'Failed Dependency'); "WebDav RFC: 2518"		"5xx: Server-Side Error status codes"		at: #serverError				put: ('500' -> 'Internal Server Error');		at: #notImplemented			put: ('501' -> 'Not Implemented');		at: #badGateway				put: ('502' -> 'Bad Gateway');		at: #serviceUnavailable		put: ('503' -> 'Service Unavailable');		at: #gatewayTimeout			put: ('504' -> 'Gateway Timeout');		at: #versionNotSupported	put: ('505' -> 'HTTP Version Not Supported');		at: #InsufficientStorage	put: ('507' -> 'Insufficient Storage'); 	"WebDav RFC: 2518" 		yourself.	"Initialize an inverse mapping to lookup a symbol based on the numeric code"	StatusSymbols := Dictionary new.	StatusCodes associationsDo: 		[ :assn |		StatusSymbols at: assn value key asNumber put: assn key].! !!HttpResponse class methodsFor: 'preferences' stamp: 'svp 5/7/2003 04:13'!responseChunkSize	"This is the maximum number of bytes of the content stream that are written 	to the output stream during each iteration of the writing loop."	^#'HttpResponse-responseChunkSize' binding ifNil: [131072]! !!HttpResponse class methodsFor: 'preferences' stamp: 'svp 5/7/2003 04:10'!responseChunkSize: anInteger	^#'HttpResponse-responseChunkSize' binding: anInteger! !!HttpResponse class methodsFor: 'accessing' stamp: 'svp 4/15/2003 09:20'!statusCodeFor: aSymbol	^(StatusCodes at: aSymbol) key! !!HttpResponse class methodsFor: 'accessing' stamp: 'svp 4/15/2003 09:21'!statusDescriptionFor: aSymbol	^(StatusCodes at: aSymbol) value! !!HttpResponse class methodsFor: 'accessing' stamp: 'svp 10/28/2003 13:14'!statusSymbolFor: aCode	^StatusSymbols at: aCode asNumber ifAbsent: [#serverError]! !HttpResponse initialize!