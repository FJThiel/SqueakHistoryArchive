'From Squeak 2.2beta of Sept 11, 1998 on 16 September 1998 at 3:36:40 pm'!!SimpleClientSocket methodsFor: 'all' stamp: 'jm 9/16/1998 14:37'!waitForDataQueryingUserEvery: seconds	"Wait for data to arrive, asking the user periodically if they wish to keep waiting. If they don't wish to keep waiting, destroy the socket and raise an error."	| gotData |	gotData _ false.	[gotData]		whileFalse: [			gotData _ self waitForDataUntil: (Socket deadlineSecs: seconds).			gotData ifFalse: [				self isConnected ifFalse: [					self destroy.					self error: 'server closed connection'].				(self confirm: 'server not responding; keep trying?')					ifFalse: [						self destroy.						self error: 'no response from server']]].! !!HTTPSocket methodsFor: 'all' stamp: 'jm 9/16/1998 14:21'!getRestOfBuffer: beginning totalLength: length	"Reel in a string of a fixed length.  Part of it has already been received.  Close the connection after all chars are received.  We do not strip out linefeed chars.  tk 6/16/97 22:32" 	"if length is nil, read until connection close"	| buf response bytesRead |	buf _ String new: (length ifNil: [ 2000 ]).	response _ RWBinaryOrTextStream on: buf.	response nextPutAll: beginning.	buf _ String new: (length ifNil: [ 2000 ]).	[(length isNil ifTrue: [ true ] ifFalse: [response position < length]) 	 & (self dataAvailable | self isConnected)] 	whileTrue: [		[self isConnected and: [(self waitForDataUntil: (Socket deadlineSecs: 5)) not]] whileTrue: [	 		Transcript show: 'data was slow'; cr].		bytesRead _ self primSocket: socketHandle receiveDataInto: buf startingAt: 1 count: (length isNil ifTrue: [ buf size ] ifFalse: [ length - response size ]). 		bytesRead > 0 ifTrue: [  			response nextPutAll: (buf copyFrom: 1 to: bytesRead)] ].	Transcript cr; show: 'data byte count: ', response position printString.	Transcript cr; show: ((self isConnected) ifTrue: ['Over length by: ', bytesRead printString] 		ifFalse: ['Socket closed']). 	"response text.	is already a text stream"	response reset.	"position: 0."	^ response! !