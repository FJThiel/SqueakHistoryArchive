'From Squeak3.9alpha of 4 July 2005 [latest update: #6686] on 6 September 2005 at 4:00 pm'!!HTTPSocket class methodsFor: 'get the page' stamp: 'md 9/6/2005 15:59'!httpGet: url args: args accept: mimeType	^self httpGet: url args: args accept: mimeType request: ''! !!HTTPSocket class methodsFor: 'get the page' stamp: 'md 9/6/2005 15:59'!httpGet: url args: args accept: mimeType request: requestString	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIME type 'application/octet-stream'."	| document |	document _ self httpGetDocument: url  args: args  accept: mimeType request: requestString.	(document isString) ifTrue: [		"strings indicate errors"		^ document ].	^ (RWBinaryOrTextStream with: document content) reset! !!HTTPSocket class methodsFor: 'get the page' stamp: 'md 9/6/2005 15:58'!httpGet: url args: args user: user passwd: passwd	| authorization |	authorization _ (Base64MimeConverter mimeEncode: (user , ':' , passwd) readStream) contents.	^self 		httpGet: url args: args accept: '*/*' 		request: 'Authorization: Basic ' , authorization , CrLf! !!HTTPSocket class reorganize!('class initialization' blabEmail: initialize)('get the page' httpFileIn: httpFileInNewChangeSet: httpGetDocument:args:accept:request: httpGet: httpGet:accept: httpGet:args:accept: httpGet:args:accept:request: httpGetDocument: httpGetDocument:accept: httpGetDocument:args: httpGetDocument:args:accept: httpGetNoError:args:accept: httpGet:args:user:passwd: httpGif: httpJpeg: httpPost:args:accept: httpPostDocument:args: httpPostDocument:args:accept: httpPostDocument:args:accept:request: httpPostMultipart:args:accept:request: httpPostToSuperSwiki:args:accept:request: httpPost:args:user:passwd: httpPut:to:user:passwd: httpShowChunk: httpShowGif: httpShowJpeg: httpShowPage:)('proxy settings' addHTTPProxyPreferences addProxyException: checkHTTPProxyPreferences fetchExternalSettingsIn: httpProxyExceptions httpProxyPort httpProxyPort: httpProxyServer httpProxyServer: proxySettingsFileName proxyTestingComment proxyUser:password: removeProxyException: stopUsingProxyServer useProxyServerNamed:port: useProxyServerNamed:port:proxyUser:password:)('utilities' argString: argStringUnencoded: expandUrl:ip:port: initHTTPSocket:ifError: initHTTPSocket:wait:ifError: retry:asking:ifGiveUp: shouldUseProxy: showImage:named: userAgentString)('magic numbers' defaultPort)!