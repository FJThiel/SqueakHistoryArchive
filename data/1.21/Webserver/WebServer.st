"WebServer.st
Early version of my Squeak Web Server. Image roll forward and undo are not yet implemented.

	TITLE			WebServer.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			0.1.1
	IMAGE VERSION	1.21
	PREREQUISITES	miscChanges.st
	DATE			August 19, 1997"!

"Some utility methods for existing classes"!

!Collection methodsFor: 'Web Server' stamp: 'go 8/14/97 13:06'!
asLink
	"Return a HTML list of my contents."

	| stream |
	stream := WriteStream on: ''.
	stream nextPutAll: '<ul>'.
	self do: [ :item | stream nextPutAll: '<li>'; nextPutAll: item asLink ].
	stream nextPutAll: '</ul>'.
	^stream contents! !

!Object class methodsFor: 'HTML Reply' stamp: 'go 8/18/97 15:48'!
asHtml: aRequest
	"Return a HTML page for me."

	aRequest
		title: (self asString);
		reply: '<dl><dt><b>Superclass</b></dt><dd>';
		reply: self superclass asLink;
		reply: '<dt><b>Subclasses</b></dt><dd>';
		reply: (self subclasses asSortedCollection: [:a :b | a name < b name ]) asLink;
		reply: '<dt><b>Comment</b></dt><dd>';
		reply: self comment asLink;
		reply: '</dl>'
! !

!Object methodsFor: 'Web Server' stamp: 'go 8/14/97 13:00'!
asLink: nameString message: aSelector
	"Return HTML code for a link to me using the given name and message selector."

	| stream |
	stream := WriteStream on: ''.
	stream
		nextPutAll: '<a href="';
		nextPutAll: (WebRequest linkIdFor: self);
		nextPut: $.;
		nextPutAll: aSelector;
		nextPutAll: '">';
		nextPutAll: nameString;
		nextPutAll: '</a>'.
	^stream contents! !

!Object methodsFor: 'Web Server' stamp: 'go 8/14/97 13:14'!
asLink
	"Return HTML code for a standard link to me."

	^(self respondsTo: #asHtml:) 
		ifTrue: [ self asLink: self asString message: 'asHtml' ]
		ifFalse: [ self asString ]! !

!String methodsFor: 'Web Server' stamp: 'go 8/14/97 13:06'!
asLink
	"No fancy link building here."

	^self! !

!Socket methodsFor: 'queries' stamp: 'go 8/18/97 11:12'!
peerName
	"Return the name of the host I'm connected to."

	^NetNameResolver nameForAddress: (self primSocketRemoteAddress: socketHandle) timeout: 60! !

TextConstants at: #CrLfCrLf put: (String with: Character cr with: Character linefeed with: Character cr with: Character linefeed)!
TextConstants at: #HttpHeader put: 'HTTP/1.0 200 OK', (String with: Character cr with: Character linefeed), 'Content-Type: text/html', (TextConstants at: #CrLfCrLf)!
TextConstants at: #HttpAuthorize put: ('HTTP/1.0 401 Unauthorized', (String with: Character cr with: Character linefeed), 'WWW-Authenticate: Basic realm="Squeak"', (TextConstants at: #CrLfCrLf), '<html><h1>Unauthorized</h1>').!

"The web server proper."!

Object subclass: #WebRequest
	instanceVariableNames: 'peerName userId message fields connection log '
	classVariableNames: 'LinkResolver ObjectNames ServerActive UserMap '
	poolDictionaries: 'TextConstants '
	category: 'Web Server'!

!WebRequest methodsFor: 'Processing' stamp: 'go 8/13/97 14:56'!
decodeFields: aString
  "Convert the form fields in aString to a query dictionary."

  | query dict i key value |

  query := aString findTokens: '&'.
  dict := Dictionary new.
  query do: [ :tag |
    i := tag indexOf: $=.
    key := tag copyFrom: 1 to: i - 1.
    value := i < tag size ifTrue: [ self unEscape: (tag copyFrom: i + 1 to: tag size) ] ifFalse: [ nil ].
    (dict includesKey: key)
     ifFalse: [ dict at: key put: value ]
     ifTrue: [
      ((dict at: key) isKindOf: String) 
		ifTrue: [ dict at: key put: (OrderedCollection with: (dict at: key)) ].
      (dict at: key) add: value
     ]
  ].
  ^dict! !

!WebRequest methodsFor: 'Processing' stamp: 'go 8/18/97 13:42'!
getReply
  "Generate the reply."

	| rec msg |
	peerName = 'proxy.tuwien.ac.at' ifTrue: [ ^self noProxy ].
	message = #('robots' 'txt') ifTrue: [ ^self robots ].
	rec := self objectFromString: (message at: 1).
	msg := ((message at: 2), ':') asSymbol.
	'HTML Reply' = ((rec class whichClassIncludesSelector: msg)
			whichCategoryIncludesSelector: msg) 
		ifFalse: [ self error: 'Illegal message.' ].
	rec perform: msg with: self.
	self reply: '<hr><address>Erzeugt vom <a href="http://ftp.tuwien.ac.at/~go/Squeak/webserver.html">Squeak WebServer</a> am ';
		reply: (Date today asString);
		reply: ' um ';
		reply: (Time now asString);
		reply: '.</address></body></html>'! !

!WebRequest methodsFor: 'Processing' stamp: 'go 8/18/97 14:24'!
noAuth
	"Raise a noAuth error."

	self
		log: '*** noAuth';
		reply: HttpAuthorize;
		error: 'noAuth'! !

!WebRequest methodsFor: 'Processing' stamp: 'go 8/19/97 10:39'!
noProxy
	"Return a page saying we don't want proxy access."

	self
		log: '*** No Proxy';
		reply: 'HTTP/1.0 403 Forbidden';
		reply: CrLfCrLf;
		reply: '</h1>Kein Zugriff über den Proxy Server !!</h1>'! !

!WebRequest methodsFor: 'Processing' stamp: 'go 8/18/97 13:39'!
robots
	"Return a text specifying our robots policy."

	self log: 'Robot Policy'.
	self reply: 'HTTP/1.0 200 OK
Content-Type: text/plain

User-agent: Scooter
Disallow:

User-agent: *
Disallow: /
'! !

!WebRequest methodsFor: 'Processing' stamp: 'go 8/13/97 14:51'!
unEscape: aString
  "Convert escape sequences to their proper characters."

	| rs ws c |
	rs := ReadStream on: aString.
	ws := WriteStream on: ''.
	[ rs atEnd ] whileFalse: [
		c := rs next.
		ws nextPut: 
			(c = $+ ifTrue: [ $  ] ifFalse: [
				c = $% 
					ifTrue: [ (Integer readFrom: rs base: 16) asCharacter ]
					ifFalse: [ c ]
				]).
		].
	^ws contents copyWithout: Character cr! !


!WebRequest methodsFor: 'Initializing' stamp: 'go 8/18/97 16:17'!
initializeFrom: aSocket
  "Initialize me from aSocket."

	| request header idx |
	(log := WriteStream with: Time totalSeconds asString) tab.
	connection := aSocket.
	request := self readRequest.
	header := request first substrings.
	((idx := header indexOf: 'Authorization:') ~= 0 and: [ (header at: idx + 1) = 'Basic' ])
		ifTrue: [ userId := header at: idx + 2].
	peerName := connection peerName.
	log
		nextPutAll: peerName asString; tab;
		nextPutAll: userId asString; tab;
		nextPutAll: (header at: 2); tab;
		nextPutAll: request last asString; tab.
	message := (self unEscape: (header at: 2)) findTokens: '/.'.
	request last notNil ifTrue: [ fields := self decodeFields: request last ]
! !

!WebRequest methodsFor: 'Initializing' stamp: 'go 8/18/97 16:17'!
readRequest
	"Read the request and return an array of header and query."

	| idx request header query length |
	request := ''.
	[	request := request, connection getResponse.
		(idx := request findString: CrLfCrLf startingAt: 1) = 0 ] whileTrue.
	header := request copyFrom: 1 to: idx - 1.
	(request beginsWith: 'POST') ifTrue: [
		(length := request asUppercase findString: 'CONTENT-LENGTH:' startingAt: 1) = 0 
			ifTrue: [ self error: '* noLength' ].
		length := (request copyFrom: length + 15 to: 
		  (request indexOf: Character cr startingAt: length ifAbsent: [])) 
			withBlanksTrimmed asNumber + idx + 3.
		[ request size < length ] whileTrue: [ request := request, connection getResponse ].
		query := (request copyFrom: idx + 3 to: request size) withBlanksTrimmed ]
	ifFalse: [
		(idx := header indexOf: $?) ~= 0 ifTrue: [
			query := header copyFrom: idx + 1 to: header size.
			header := header copyFrom: 1 to: idx - 1 ]].
	^{ header. query }! !


!WebRequest methodsFor: 'Queries' stamp: 'go 8/14/97 13:29'!
objectFromString: aString
  "Return the object denoted by aString."

	^LinkResolver
		at: (aString first isDigit ifTrue: [ aString asNumber ] ifFalse: [ aString ]) 
		ifAbsent: [ self error: 'Object with ID ', aString, ' does not exist.' ]! !


!WebRequest methodsFor: 'Accessing' stamp: 'go 8/12/97 17:00'!
fields

   "Return the value of the instance variable 'fields'."
   ^fields! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/18/97 11:29'!
log
   "Return the value of the instance variable 'log'."
   ^log! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/18/97 11:17'!
log: aString
	"Add to the log."

	log nextPutAll: aString! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/12/97 17:01'!
message

   "Return the value of the instance variable 'message'."
   ^message! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/12/97 17:02'!
objectsInField: fieldName
  "Extract the array of objects denoted by 'fieldName', return an empty array if there are none."

  | sel |
  sel := fields at: fieldName ifAbsent: [ ^#() ].
  (sel isKindOf: Array) ifFalse: [ sel := { sel } ].
  ^sel collect: [ :s | self objectFromString: s ]! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/18/97 11:29'!
peerName
  "Return the requesting ipAddress"

  ^peerName! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/13/97 15:18'!
reply: aString
	"Send back part of the reply."

	connection sendData: aString! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/18/97 11:20'!
title: aString
	"Write a standard header and set the document title. Also used for logging purposes."

	self
		log: aString;
		reply: HttpHeader;
		reply: '<html><head><title>';
		reply: aString;
		reply: '</title></head><body bgcolor=white><table width="100%"><td><hr><h1>';
		reply: aString;
		reply: '</h1><hr><td align=right><img src="http://ftp.tuwien.ac.at/~go/SqueakLogo.gif" alt="Squeak Logo"></table>'.
! !

!WebRequest methodsFor: 'Accessing' stamp: 'go 8/18/97 13:48'!
user
	"Return the requesting user.
	 For security reasons raise a 'forbidden' error if no peerName is present."

	peerName isNil ifTrue: [ self error: 'forbidden' ].
	^UserMap at: userId ifAbsent: [ self noAuth ]! !


!WebRequest methodsFor: 'HTML Composition' stamp: 'go 8/18/97 16:41'!
htmlSearchSelection
	"Return a search selection form. If I have received fields select them in the new form."

	| str theField |
	str := '<form action="Server.htmlSearch" method=post>
<select name="coll">
<option value="1">Classname
<option value="2">Comment
</select><select name="how">
<option value="is">is
<option value="starts">begins with
<option value="includes">contains
</select>
<input name="what" size=20> <input type=submit value="Suchen">' copy.

	fields notNil ifTrue: [
		theField := fields at: 'coll'.
		str := str copyReplaceTokens: theField with: theField, ' selected'.
		theField := '"', (fields at: 'how'), '"'.
		str := str copyReplaceTokens: theField with: theField, ' selected'.
		(theField := fields at: 'what') size > 0 ifTrue: [
			str := str copyReplaceTokens: '"what"' with: '"what" value="', theField, '" ']].
	^str
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WebRequest class
	instanceVariableNames: ''!

!WebRequest class methodsFor: 'Configuring' stamp: 'go 8/13/97 18:12'!
link: anObject as: aString
	"Establish the link mapping from aString to anObject and vice versa."

	LinkResolver isNil ifTrue: [
		LinkResolver := Dictionary new.
		ObjectNames := IdentityDictionary new ].
	LinkResolver at: aString put: anObject.
	ObjectNames at: anObject put: aString
! !

!WebRequest class methodsFor: 'Configuring' stamp: 'go 8/14/97 11:02'!
linkIdFor: anObject
	"Return the linkId for anObject. Create one if necessary."

	| newId |
	^(ObjectNames at: anObject ifAbsent: [
		[	newId := SmallInteger maxVal atRandom.
			ObjectNames includesKey: newId ] whileTrue.
		LinkResolver at: newId put: anObject.
		ObjectNames at: anObject put: newId ]) asString! !

!WebRequest class methodsFor: 'Configuring' stamp: 'go 8/13/97 15:59'!
mapFrom: aKey to: aPerson
	"Establish a mapping from a RFC 1421 key to a usern."

	UserMap isNil ifTrue: [ UserMap := Dictionary new ].
	aPerson isNil ifTrue: [ UserMap removeKey: aKey ] ifFalse: [ UserMap at: aKey put: aPerson ]
! !

!WebRequest class methodsFor: 'Configuring' stamp: 'go 8/13/97 18:06'!
mapName: nameString password: pwdString to: aPerson
	"Insert/remove the encoding per RFC1421 of the username:password combination 
	 into/from the UserMap."

	| clear code clearSize idx map |
	clear := (nameString, ':', pwdString) asByteArray.
	clearSize := clear size.
	[ clear size \\ 3 ~= 0 ] whileTrue: [ clear := clear, #(0) ].
	idx := 1.
	map := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'.
	code := WriteStream on: ''.
	[ idx < clear size ] whileTrue: [ code 
		nextPut: (map at: (clear at: idx) // 4 + 1);
		nextPut: (map at: (clear at: idx) \\ 4 * 16 + ((clear at: idx + 1) // 16) + 1);
   		nextPut: (map at: (clear at: idx + 1) \\ 16 * 4 + ((clear at: idx + 2) // 64) + 1);
   		nextPut: (map at: (clear at: idx + 2) \\ 64 + 1).
		idx := idx + 3 ].
	code := code contents.
	idx := code size.
	clear size - clearSize timesRepeat: [ code at: idx put: $=. idx := idx - 1].
	self mapFrom: code to: aPerson! !

!WebRequest class methodsFor: 'Configuring' stamp: 'go 8/18/97 10:30'!
purgeLinkIds
	"Purge the automatically generated linkIds."
	"WebRequest purgeLinkIds"

	| stale |
	stale := ReadWriteStream on: #().
	LinkResolver associationsDo: [ :ass | ass key isInteger ifTrue: [ stale nextPut: ass ]].
	stale reset; do: [ :ass |
		LinkResolver removeKey: ass key.
		ObjectNames removeKey: ass value ]! !


!WebRequest class methodsFor: 'Serving' stamp: 'go 8/18/97 10:31'!
doNightlyJobs
	"This is just a wrapper so we don't have to restart the server loop when we add/remove jobs."

	[	self purgeLinkIds.
		"some other jobs here" 
	] ifError: [ :msg :rec | ^'*** ', rec asString, ': ', msg asString ].
	^'Nightly Jobs done.'! !

!WebRequest class methodsFor: 'Serving' stamp: 'go 8/18/97 15:59'!
serve: aSocket
	"Create an instance and initialize it from aSocket."

	| inst |
	inst := self new.
	[ inst initializeFrom: aSocket; getReply ] ifError: [ :msg :rec |
		msg = 'noAuth' ifFalse: [
			inst log: '*** ', rec asString, ': ', msg.
			[ inst reply: (msg = 'forbidden' 
				ifTrue: [ 'HTTP/1.0 403 Forbidden' ] 
				ifFalse: [ 'HTTP/1.0 400 Bad Request' ]), CrLfCrLf, msg ] ifError: [ ]
		]
	].
	aSocket closeAndDestroy.
	^inst log contents! !

!WebRequest class methodsFor: 'Serving' stamp: 'go 8/18/97 15:15'!
serveOnPort: portNumber loggingTo: fileName
	"This is the HTTP server loop. Periodically flush the logfile. Invoke dayly (nightly) jobs."

	| connection log clock |
	Socket initializeNetwork: 0.
	log := FileStream newFileNamed: fileName.
	clock := Date today.
	(connection := Socket new) listenOn: portNumber.
	ServerActive := true.
	[	(connection waitForConnectionUntil: Socket standardDeadline)
		ifTrue: [
			log nextPutAll: (self serve: connection); cr.
			(connection := Socket new) listenOn: portNumber ]
		ifFalse: [
			clock < Date today ifTrue: [
				clock := Date today addDays: 1.
				log nextPutAll: (self doNightlyJobs); cr; flush
			].
			log flush
		].
		ServerActive
	] whileTrue.
	connection destroy.
	log close
! !

!WebRequest class methodsFor: 'Serving' stamp: 'go 8/13/97 13:58'!
stopServer
	"Stop the server."

	ServerActive := false! !

WebRequest link: WebRequest as: 'Server'!
WebRequest link: Object as: 'Object'!

!WebRequest class methodsFor: 'HTML Reply' stamp: 'go 8/18/97 16:37'!
demo: aRequest
	"A few demo pages."

	aRequest
		title: 'Welcome to Squeak';
		reply: (Object asLink: 'Browse the classes' message: 'asHtml');
		reply: '<p>';
		reply: (self asLink: 'Search classes' message: 'htmlSearch');
		reply: '<p>';
		reply: (self asLink: 'Stop the Server' message: 'stop')! !

!WebRequest class methodsFor: 'HTML Reply' stamp: 'go 8/18/97 15:34'!
htmlSearch: aRequest
	"Perform the search."

	| coll how what msg |
	(aRequest fields isNil or: [(what := aRequest fields at: 'what') size = 0 ]) ifTrue: [
		aRequest title: 'Squeak Suchspezifikation'.
		aRequest reply: aRequest htmlSearchSelection.
		^self ].
	aRequest title: 'Squeak Suchergebnis'.
	what := what asLowercase.
	coll := self searchOption: (aRequest fields at: 'coll') asNumber.
	msg := (coll at: 3) asSymbol.
	coll := (aRequest objectFromString: (coll at: 1)) perform: (coll at: 2) asSymbol.
	how  := aRequest fields at: 'how'.
	how = 'is' ifTrue: [ 
		coll := coll select: [ :obj | (obj perform: msg) asString asLowercase = what ]].
	how = 'starts' ifTrue: [ 
		coll := coll select: [ :obj | (obj perform: msg) asString asLowercase beginsWith: what ]].
	how = 'includes' ifTrue: [ 
		coll := coll select: [ :obj |
			((obj perform: msg) asString asLowercase findString: what startingAt: 1) ~= 0 ]].
	aRequest reply: coll asLink; reply: '<hr>'; reply: aRequest htmlSearchSelection! !

!WebRequest class methodsFor: 'HTML Reply' stamp: 'go 8/18/97 14:24'!
stop: aRequest
	"Stop the server."

	aRequest user = true ifFalse: [ aRequest noAuth ].
	ServerActive := false.
	aRequest title: 'Server stopped'! !


!WebRequest class methodsFor: 'HTML Composition' stamp: 'go 8/18/97 16:51'!
searchOption: id
  "Return an array that specifies the search option denoted by 'id'' '"

  ^#(
   ('Object' 'withAllSubclasses' 'name')
   ('Object' 'withAllSubclasses' 'comment') ) at: id! !

WebRequest mapName: 'manager' password: 'squeak' to: true!
