'From Squeak 1.2 of June 29, 1997 on 10 July 1997 at 1:31:26 pm'!StandardFileStream subclass: #CrLfFileStream
	instanceVariableNames: 'lineEndConvention '
	classVariableNames: 'Cr CrLf Lf LineEndDefault LineEndStrings LookAheadCount '
	poolDictionaries: ''
	category: 'System-Files'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'ar 7/10/97 10:28'!
lf
	^self value: 10! !


!CrLfFileStream methodsFor: 'open/close' stamp: 'ar 7/10/97 11:03'!
open: aFileName forWrite: writeMode 
	"Open the receiver.  If writeMode is true, allow write, else access will be read-only."
	| result |
	result := super open: aFileName forWrite: writeMode.
	result ifNotNil:[self detectLineEndConvention].
	^result! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 10:45'!
ascii
	super ascii.
	self detectLineEndConvention.! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 10:44'!
binary
	super binary.
	lineEndConvention := nil.! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:02'!
detectLineEndConvention
	"Detect the line end convention used in this stream.
	The result may be either #cr, #lf or #crlf."
	| char numRead pos |
	self isBinary ifTrue:[^self error:'Line end conventions are not used on binary streams'].
	lineEndConvention := LineEndDefault. "Default if nothing else found"
	numRead := 0.
	pos := self position.
	[self atEnd not and:[numRead < LookAheadCount]] whileTrue:[
		char := self next.
		char = Lf ifTrue:[
			self position: pos.
			^lineEndConvention := #lf].
		char = Cr ifTrue:[
			self peek = Lf
				ifTrue:[lineEndConvention := #crlf]
				ifFalse:[lineEndConvention := #cr].
			self position: pos.
			^lineEndConvention].
		numRead := numRead + 1.
	].
	self position: pos.
	^lineEndConvention! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:02'!
next
	| char |
	char := super next.
	lineEndConvention ifNil:[^char].
	(LineEndStrings at: lineEndConvention) first = char ifFalse:[^char]
	ifTrue:[
		lineEndConvention = #crlf ifFalse:[^Cr]
		ifTrue:[
			self peek = Character lf 
				ifTrue:[super next. ^Cr]
				ifFalse:[^char]]].
! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:02'!
next: n
	| string |
	string := super next: n.
	lineEndConvention ifNil:[^string].
	lineEndConvention == #crlf ifTrue:[
		"Special case for last character"
		(string last = Cr and:[self peek = Lf]) ifTrue:[self next]].
	^self convertStringToCr: string! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:13'!
nextPut: char

	(lineEndConvention notNil and:[char = Cr]) 
		ifTrue:[super nextPutAll: (LineEndStrings at: lineEndConvention)]
		ifFalse:[super nextPut: char].
	^char! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:13'!
nextPutAll: aString

	super nextPutAll: (self convertStringFromCr: aString).
	^aString! !

!CrLfFileStream methodsFor: 'access' stamp: 'ar 7/10/97 13:13'!
verbatim: aString

	super verbatim: (self convertStringFromCr: aString).
	^aString! !

!CrLfFileStream methodsFor: 'private' stamp: 'ar 7/10/97 13:30'!
convertStringFromCr: aString
	| inStream outStream |
	lineEndConvention ifNil:[^aString].
	lineEndConvention == #cr ifTrue:[^aString].
	lineEndConvention == #lf ifTrue:[^aString copy replaceAll: Cr with: Lf].
	"lineEndConvention == #crlf"	
	inStream := ReadStream on: aString.
	outStream := WriteStream on: (String new: aString size).
	[inStream atEnd] whileFalse:[
		outStream nextPutAll: (inStream upTo: Cr).
		(inStream atEnd not or:[aString last = Cr]) ifTrue:[
			outStream nextPutAll: CrLf]].
	^outStream contents! !

!CrLfFileStream methodsFor: 'private' stamp: 'ar 7/10/97 13:30'!
convertStringToCr: aString
	| inStream outStream |
	lineEndConvention ifNil:[^aString].
	lineEndConvention == #cr ifTrue:[^aString].
	lineEndConvention == #lf ifTrue:[^aString copy replaceAll: Lf with: Cr].
	"lineEndConvention == #crlf"	
	inStream := ReadStream on: aString.
	outStream := WriteStream on: (String new: aString size).
	[inStream atEnd] whileFalse:[
		outStream nextPutAll: (inStream upTo: Cr).
		(inStream atEnd not or:[aString last = Cr]) ifTrue:[
			outStream nextPut: Cr.
			inStream peek = Lf ifTrue:[inStream next]]].
	^outStream contents! !


!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 7/10/97 13:16'!
defaultToCR
	"CrLfFileStream defaultToCR"
	LineEndDefault := #cr.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 7/10/97 13:16'!
defaultToCRLF
	"CrLfFileStream defaultToCRLF"
	LineEndDefault := #crlf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 7/10/97 13:16'!
defaultToLF
	"CrLfFileStream defaultToLF"
	LineEndDefault := #lf.! !

!CrLfFileStream class methodsFor: 'class initialization' stamp: 'ar 7/10/97 13:16'!
initialize
	"CrLfFileStream initialize"
	Cr := Character cr.
	Lf := Character lf.
	CrLf := String with: Cr with: Lf.
	LineEndStrings := Dictionary new.
	LineEndStrings at: #cr put: (String with: Character cr).
	LineEndStrings at: #lf put: (String with: Character lf).
	LineEndStrings at: #crlf put: (String with: Character cr with: Character lf).
	LookAheadCount := 2048.

	self defaultToCR.! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 7/10/97 11:12'!
replaceAll: oldObject with: newObject
	"Replace all occurences of oldObject with newObject"
	| index |
	index := self indexOf: oldObject startingAt: 1 ifAbsent:[0].
	[index = 0] whileFalse:[
		self at: index put: newObject.
		index := self indexOf: oldObject startingAt: index+1 ifAbsent:[0]]! !


CrLfFileStream initialize!
