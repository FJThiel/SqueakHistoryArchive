"      NAME	CrLfFileStream.st
       AUTHOR	raab@isg.cs.uni-magdeburg.de (Andreas Raab)
       URL	(none)
       FUNCTION	Detect and process line end conventions in file streams
       KEYWORDS	files, stream,  line end convention, cross-platform
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.1
       DATE	28-Mar-98

SUMMARY

Provides class CrLfFileStream for automaticallydetecting line end conventions on external files.  You can install it permanently by changingFileStream class>>concreteStream as follows:FileStream class>>concreteStream        'Who should we really direct class queries to?  '        ^CrLfFileStream

				Andreas Raab
"!
'From Squeak 1.31 of Feb 4, 1998 on 28 March 1998 at 7:29:03 pm'!
StandardFileStream subclass: #CrLfFileStream
	instanceVariableNames: 'lineEndConvention '
	classVariableNames: 'Cr CrLf Lf LineEndDefault LineEndStrings LookAheadCount '
	poolDictionaries: ''
	category: 'System-Files'!

!CrLfFileStream methodsFor: 'open/close' stamp: 'ar 3/21/98 03:14'!
open: aFileName forWrite: writeMode 
	"Open the receiver.  If writeMode is true, allow write, else access will be 
	read-only. "
	| result |
	result _ super open: aFileName forWrite: writeMode.
	result ifNotNil: [
		writeMode
			ifTrue:[lineEndConvention _ LineEndDefault]
			ifFalse:[self detectLineEndConvention]].
	^ result! !


!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:16'!
ascii
	super ascii.
	self detectLineEndConvention! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:16'!
binary
	super binary.
	lineEndConvention _ nil! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 2/5/98 19:53'!
detectLineEndConvention
	"Detect the line end convention used in this stream. The result may be either #cr, #lf or #crlf."
	| char numRead pos |
	self isBinary ifTrue: [^ self error: 'Line end conventions are not used on binary streams'].
	lineEndConvention _ nil.
	"Default if nothing else found"
	numRead _ 0.
	pos _ self position.
	[self atEnd not and: [numRead < LookAheadCount]]
		whileTrue: 
			[char _ self next.
			char = Lf
				ifTrue: 
					[self position: pos.
					^ lineEndConvention _ #lf].
			char = Cr
				ifTrue: 
					[self peek = Lf
						ifTrue: [lineEndConvention _ #crlf]
						ifFalse: [lineEndConvention _ #cr].
					self position: pos.
					^ lineEndConvention].
			numRead _ numRead + 1].
	self position: pos.
	^ lineEndConvention ifNil:[lineEndConvention := LineEndDefault]! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:17'!
next
	| char |
	char _ super next.
	lineEndConvention ifNil: [^ char].
	(LineEndStrings at: lineEndConvention) first = char
		ifTrue: [lineEndConvention = #crlf
				ifTrue: [self peek = Character lf
						ifTrue: 
							[super next.
							^ Cr]
						ifFalse: [^ char]]
				ifFalse: [^ Cr]]
		ifFalse: [^ char]
! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:17'!
next: n 
	| string |
	string _ super next: n.
	lineEndConvention ifNil: [^ string].
	lineEndConvention == #crlf
		ifTrue: ["Special case for last character"
			(string last = Cr and: [self peek = Lf])
				ifTrue: [self next]].
	^ self convertStringToCr: string! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
nextPut: char 
	(lineEndConvention notNil and: [char = Cr])
		ifTrue: [super nextPutAll: (LineEndStrings at: lineEndConvention)]
		ifFalse: [super nextPut: char].
	^ char! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
nextPutAll: aString 
	super nextPutAll: (self convertStringFromCr: aString).
	^ aString
! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 3/28/98 15:39'!
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next _ self next.
	(lineEndConvention notNil and:[next = Character cr]) ifTrue:[
		self position: self position - (LineEndStrings at: lineEndConvention) size.
	] ifFalse:[
		self position: self position - 1.
	].
	^ next! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 1/20/98 16:18'!
verbatim: aString 
	super verbatim: (self convertStringFromCr: aString).
	^ aString! !


!CrLfFileStream methodsFor: 'private' stamp: 'ar 1/20/98 16:21'!
convertStringFromCr: aString 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Cr with: Lf].
	"lineEndConvention == #crlf"
	inStream _ ReadStream on: aString.
	outStream _ WriteStream on: (String new: aString size).
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: [outStream nextPutAll: CrLf]].
	^ outStream contents! !

!CrLfFileStream methodsFor: 'private' stamp: 'ar 1/20/98 16:21'!
convertStringToCr: aString 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Lf with: Cr].
	"lineEndConvention == #crlf"
	inStream _ ReadStream on: aString.
	outStream _ WriteStream on: (String new: aString size).
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: 
					[outStream nextPut: Cr.
					inStream peek = Lf ifTrue: [inStream next]]].
	^ outStream contents! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

CrLfFileStream class
	instanceVariableNames: ''!

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToCR
	"CrLfFileStream defaultToCR"
	LineEndDefault := #cr.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToCRLF
	"CrLfFileStream defaultToCRLF"
	LineEndDefault := #crlf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:10'!
defaultToLF
	"CrLfFileStream defaultToLF"
	LineEndDefault := #lf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 1/20/98 16:13'!
guessDefaultLineEndConvention
	"Lets try to guess the line end convention from what we know about the path name delimiter from FileDirectory."
	FileDirectory pathNameDelimiter = $: ifTrue:[^self defaultToCR].
	FileDirectory pathNameDelimiter = $/ ifTrue:[^self defaultToLF].
	FileDirectory pathNameDelimiter = $\ ifTrue:[^self defaultToCRLF].
	"in case we don't know"
	^self defaultToCR! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 3/28/98 19:28'!
initialize

	"CrLfFileStream initialize"
	Cr := Character cr.
	Lf := Character value: 10.
	CrLf := String with: Cr with: Lf.
	LineEndStrings := Dictionary new.
	LineEndStrings at: #cr put: (String with: Cr).
	LineEndStrings at: #lf put: (String with: Lf).
	LineEndStrings at: #crlf put: (String with: Cr with: Lf).
	LookAheadCount := 2048.
	self defaultToCR.! !


CrLfFileStream initialize!

