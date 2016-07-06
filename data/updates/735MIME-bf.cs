'From Squeak 2.3 of January 14, 1999 on 15 March 1999 at 4:50:41 pm'!"Change Set:		MIME-bfDate:			27 November 1998Author:			Bert FreudenbergSome Utilities for decoding MIME messages.	* LimitingLineStreamWrapper: treat a boundary string (or other per-line conditions) as end of stream. Useful for decoding multipart nessages.	* QuotedPrintableMimeConverter: decode MIME contents as defined in RFC2045	* RFC2047MimeConverter: decode MIME header as defined in RFC2047"!Object subclass: #LimitingLineStreamWrapper	instanceVariableNames: 'stream line limitingBlock position '	classVariableNames: ''	poolDictionaries: 'TextConstants '	category: 'Collections-Streams'!Object subclass: #MimeConverter	instanceVariableNames: 'dataStream mimeStream '	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Streams'!MimeConverter subclass: #Base64MimeConverter	instanceVariableNames: 'data '	classVariableNames: 'FromCharTable ToCharTable '	poolDictionaries: ''	category: 'Collections-Streams'!MimeConverter subclass: #QuotedPrintableMimeConverter	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Streams'!QuotedPrintableMimeConverter subclass: #RFC2047MimeConverter	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Streams'!!LimitingLineStreamWrapper commentStamp: '<historical>' prior: 0!I'm a wrapper for a stream optimized for line-by-line access using #nextLine. My instances can be nested.I read one line ahead. Reading terminates when the stream ends, or if the limitingBlock evaluated with the line answers true. To skip the delimiting line for further reading use #skipThisLine.Character-based reading (#next) is permitted, too. Send #updatePosition when switching from line-based reading.See examples at the class side.--bf 2/19/1999 12:52!!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:25'!delimiter: aString	"Set limitBlock to check for a delimiting string. Be unlimiting if nil"	self limitingBlock: (aString caseOf: {		[nil] -> [[:aLine | false]].		[''] -> [[:aLine | aLine size = 0]]	} otherwise: [[:aLine | aLine beginsWith: aString]])! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/13/1998 13:08'!lastLineRead	"Return line last read. At stream end, this is the boundary line or nil"	^ line! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 19:16'!limitingBlock: aBlock	"The limitingBlock is evaluated with a line to check if this line terminates the stream"	limitingBlock _ aBlock fixTemps.	self updatePosition! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 2/19/1999 11:45'!linesUpToEnd	| elements ln |	elements _ OrderedCollection new.	[(ln _ self nextLine) isNil] whileFalse: [ 		elements add: ln].	^elements! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:37'!next	"Provide character-based access"	position isNil ifTrue: [^nil].	position < line size ifTrue: [^line at: (position _ position + 1)].	line _ stream nextLine.	self updatePosition.	^ Character cr! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:09'!nextLine	| thisLine |	self atEnd ifTrue: [^nil].	thisLine _ line.	line _ stream nextLine.	^thisLine! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/13/1998 13:04'!peekLine	self atEnd ifTrue: [^nil].	^ line! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 16:53'!skipThisLine	line _ stream nextLine.	self updatePosition.! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 2/19/1999 11:47'!upToEnd	| ln |	^String streamContents: [:strm |		[(ln _ self nextLine) isNil] whileFalse: [ 			strm nextPutAll: ln; cr]]! !!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:37'!updatePosition	"Call this before doing character-based access"	position _ self atEnd ifFalse: [0]! !!LimitingLineStreamWrapper methodsFor: 'testing' stamp: 'bf 11/13/1998 16:55'!atEnd	^line isNil or: [limitingBlock value: line]! !!LimitingLineStreamWrapper methodsFor: 'stream protocol' stamp: 'bf 11/13/1998 17:00'!close	^stream close! !!LimitingLineStreamWrapper methodsFor: 'printing' stamp: 'bf 11/24/1998 13:39'!printOn: aStream	super printOn: aStream.	aStream nextPutAll: ' on '.	stream printOn: aStream! !!LimitingLineStreamWrapper methodsFor: 'private' stamp: 'bf 11/24/1998 14:30'!setStream: aStream delimiter: aString	stream _ aStream.	line _ stream nextLine.	self delimiter: aString.	"sets position"! !!LimitingLineStreamWrapper class methodsFor: 'instance creation' stamp: 'bf 11/24/1998 14:31'!on: aStream delimiter: aString	^self new setStream: aStream delimiter: aString! !!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 11:48'!example1	"LimitingLineStreamWrapper example1"	"Separate chunks of text delimited by a special string"	| inStream msgStream messages |	inStream _ self exampleStream.	msgStream _ LimitingLineStreamWrapper on: inStream delimiter: 'From '.	messages _ OrderedCollection new.	[inStream atEnd] whileFalse: [		msgStream skipThisLine.		messages add: msgStream upToEnd].	^messages			! !!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 12:46'!example2	"LimitingLineStreamWrapper example2"	"Demo nesting wrappers - get header lines from some messages"	| inStream msgStream headers headerStream |	inStream _ self exampleStream.	msgStream _ LimitingLineStreamWrapper on: inStream delimiter: 'From '.	headers _ OrderedCollection new.	[inStream atEnd] whileFalse: [		msgStream skipThisLine. "Skip From"		headerStream _ LimitingLineStreamWrapper on: msgStream delimiter: ''.		headers add: headerStream linesUpToEnd.		[msgStream nextLine isNil] whileFalse. "Skip Body"	].	^headers			! !!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 12:44'!exampleStream	^ReadStream on:'From me@somewhereFrom: meTo: youSubject: TestTestFrom you@elsewhereFrom: youTo: meSubject: Re: testokay'! !!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:55'!dataStream	^dataStream! !!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:51'!dataStream: anObject	dataStream _ anObject! !!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:53'!mimeStream	^mimeStream! !!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:51'!mimeStream: anObject	mimeStream _ anObject! !!MimeConverter methodsFor: 'conversion' stamp: 'bf 11/12/1998 13:30'!mimeDecode	"Do conversion reading from mimeStream writing to dataStream"	self subclassResponsibility! !!MimeConverter methodsFor: 'conversion' stamp: 'bf 11/12/1998 13:31'!mimeEncode	"Do conversion reading from dataStream writing to mimeStream"	self subclassResponsibility! !!PositionableStream methodsFor: 'accessing' stamp: 'bf 11/24/1998 13:35'!nextLine	"Answer next line (may be empty), or nil if at end"	self atEnd ifTrue: [^nil].	^self upTo: Character cr! !!QuotedPrintableMimeConverter commentStamp: '<historical>' prior: 0!I do quoted printable MIME decoding as specified in RFC 2045 "MIME Part One: Format of Internet Message Bodies".Short version of RFC2045, Sect. 6.7:	(1) Any octet, except a CR or LF that is part of a CRLF line break of the canonical (standard) form of the data being encoded, may be represented by an "=" followed by a two digit hexadecimal representation of the octet's value. [...]	(2) Octets with decimal values of 33 through 60 inclusive, and 62 through 126, inclusive, MAY be represented as the US-ASCII characters which correspond to those octets [...].	(3) Octets with values of 9 and 32 MAY be represented as US-ASCII TAB (HT) and SPACE characters, respectively, but MUST NOT be so represented at the end of an encoded line.  [...]	(4) A line break in a text body, represented as a CRLF sequence in the text canonical form, must be represented by a (RFC 822) line break, which is also a CRLF sequence, in the Quoted-Printable encoding.  [...]	(5) The Quoted-Printable encoding REQUIRES that encoded lines be no more than 76 characters long.  If longer lines are to be encoded with the Quoted-Printable encoding, "soft" line breaks must be used.  An equal sign as the last character on a encoded line indicates such a non-significant ("soft") line break in the encoded text.--bf 11/27/1998 16:50!!QuotedPrintableMimeConverter methodsFor: 'conversion' stamp: 'bf 11/24/1998 20:33'!mimeDecode	"Do conversion reading from mimeStream writing to dataStream"	| line s c1 v1 c2 v2 |	[(line _ mimeStream nextLine) isNil] whileFalse: [		line _ line withoutTrailingBlanks.		line size = 0			ifTrue: [dataStream cr]			ifFalse: [				s _ ReadStream on: line.				[dataStream nextPutAll: (s upTo: $=).				s atEnd] whileFalse: [					c1 _ s next. v1 _ c1 digitValue.					((v1 between: 0 and: 15) and: [s atEnd not])						ifFalse: [dataStream nextPut: $=; nextPut: c1]						ifTrue: [c2 _ s next. v2 _ c2 digitValue.							(v2 between: 0 and: 15)								ifFalse: [dataStream nextPut: $=; nextPut: c1; nextPut: c2]								ifTrue: [dataStream nextPut: (Character value: v1 * 16 + v2)]]].				line last = $= ifFalse: [dataStream cr]]].	^ dataStream! !!RFC2047MimeConverter commentStamp: '<historical>' prior: 0!I do quoted printable MIME decoding as specified in RFC 2047 ""MIME Part Three: Message Header Extensions for Non-ASCII Text". See String>>decodeMimeHeader!!RFC2047MimeConverter methodsFor: 'conversion' stamp: 'bf 11/27/1998 17:04'!mimeDecode	"Do conversion reading from mimeStream writing to dataStream. See String>>decodeMimeHeader"	| c |	[mimeStream atEnd] whileFalse: [		c _ mimeStream next.		c = $=			ifTrue: [c _ Character value: mimeStream next digitValue * 16				+ mimeStream next digitValue]			ifFalse: [c = $_ ifTrue: [c _ $ ]].		dataStream nextPut: c].	^ dataStream! !!String methodsFor: 'converting' stamp: 'bf 11/24/1998 19:58'!withoutTrailingBlanks	"Return a copy of the receiver from which trailing blanks have been trimmed."	| last |	last _ self findLast: [:c | c isSeparator not].	last = 0 ifTrue: [^ ''].  "no non-separator character"	^ self copyFrom: 1 to: last	" ' abc  d   ' withoutTrailingBlanks"! !!String methodsFor: 'internet' stamp: 'bf 11/30/1998 16:09'!decodeMimeHeader	"See RFC 2047, MIME Part Three: Message Header Extension for Non-ASCII Text.	Text containing non-ASCII characters is encoded by the sequence		=?character-set?encoding?encoded-text?=	Encoding is Q (quoted printable) or B (Base64), handled by Base64MimeConverter / RFC2047MimeConverter. The character-set (usually iso-8859-1) is ignored"	| input output temp decoder encoding pos | 	input _ ReadStream on: self.	output _ WriteStream on: String new.	[output nextPutAll: (input upTo: $=).	"ASCII Text"	input atEnd] whileFalse: [		(temp _ input next) = $?			ifFalse: [output nextPut: $=; nextPut: temp]			ifTrue: [				input skipTo: $?. "Skip charset"				encoding _ (input upTo: $?) asUppercase.				temp _ input upTo: $?.				input next.	"Skip final ="				decoder _ encoding = 'B'					ifTrue: [Base64MimeConverter new]					ifFalse: [RFC2047MimeConverter new].				decoder					mimeStream: (ReadStream on: temp);					dataStream: output;					mimeDecode.				pos _ input position.				input skipSeparators.	"Delete spaces if followed by ="				input peek = $= ifFalse: [input position: pos]]].	^output contents! !Base64MimeConverter removeSelector: #mimeStream:!Base64MimeConverter removeSelector: #dataStream!Base64MimeConverter removeSelector: #mimeStream!Base64MimeConverter removeSelector: #dataStream:!