"	This Squeak format file contains the Squeak implementation of portable functions commonly needed in applications but implemented differently in each dialect.

	******
	* $Name:  $
	* $Id: PortFunc.st 1.1 1999-10-11 06:26:18-05 Harmon Alpha $
	* $Log: PortFunc.st $
	* Revision 1.1  1999-10-11 06:26:18-05  Harmon
	* Initial revision
	*
	******!

'From Squeak 2.5 of August 6, 1999 on 28 September 1999 at 7:15:39 am'!
Object subclass: #PortableFunctions
	instanceVariableNames: ''
	classVariableNames: 'PortableFunctionsInstance '
	poolDictionaries: ''
	category: 'Application-Portability'!
PortableFunctions subclass: #SqueakPortableFunctions
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Application-Portability'!
SqueakPortableFunctions class
	instanceVariableNames: ''!

!Object methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
portFunc
	"Answer a <PortableFunctions>, the singleton instance of this dialect's portable functions subclass.

Definition: <PortableFunctions factory>
Return Values

	<PortableFunctions>		state
"		#PortFunc. "Protocol: <PortableFunctions factory>."
	^(Smalltalk at: #'PortableFunctions')
		current
! !


!PortableFunctions commentStamp: 'RAH 9/28/1999 07:06' prior: 0!
This is the starting point for the Portable Functions Protocols documentation.

1. Portable Functions Protocols

	The Portable Functions protocols consists of the PortableFunctions root class.

	The graphs below shows the conformance relationships between the protocols defined in this section.

		     <Object>
			|
		--------------------
		|				|
	<PortableFunctions>	|
			 <PortableFunctions factory>

1.1 Protocol: <PortableFunctions>

Conforms To
	<Object>

Description
	Class PortableFunctions is an abstract superclass that defines the behavior for functions commonly needed in applications but implemented differently in each dialect such as notifying the user, checking if a file exists, etc.  The function behavior is implemented in the dialect specific subclass.
	The Object>>#portFunc method answers the singleton instance of this dialect's portable functions subclass.  To notify the user the following would be used in a Float method:

	self portFunc promptInform: 'You are hungry.'


1.1.2 Protocol: <PortableFunctions factory>

Conforms To
	<Object>

Description
	This protocol implements the behavior of creating and accessing the singleton instance of this dialect's portable functions subclass.

Class Variables:

	PortableFunctionsInstance	<PortableFunctionsInstance>	The singleton instance of this dialect's portable functions subclass.
!

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
currentDateTimeString
	"Answer a <readableString>, the current date and time.  Example:

	nil portFunc currentDateTimeString

Definition: <PortableFunctions>
Return Values
	<readableString>		new
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility

! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
dialectNameVersionString
	"Answer a <readableString>, the name and version of this Smalltalk dialect.  Example:

	nil portFunc dialectNameVersionString

Definition: <PortableFunctions>
Return Values
	<readableString>		new
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
fileExists: locatorString
	"Answer a <boolean>, true if the file named locatorString (file drive, path, filename, and extension) exits, else false.  Example:

	nil portFunc fileExists: 'C:\MyDir\Junk.txt'

Definition: <PortableFunctions>
Parameters
	locatorString		<readableString>	uncaptured
Return Values
	<boolean>	unspecified
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
fileLocatorFromPath: pathString fileName: fileString extension: extensionString
	"Answer a <readableString>, the file locator constructed from the file drive and path, filename (without extension), and extension.  Example:

	nil portFunc fileLocatorFromPath: 'C:\MyDir'
		fileName: 'Junk'
		extension: 'txt'.

Definition: <PortableFunctions>
Parameters
	pathString		<readableString>	uncaptured
	fileString		<readableString>	uncaptured
	extensionString	<readableString>	uncaptured
Return Values
	<readableString>		new
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
fileLocatorSplit: fileLocatorString to: pathFileExtBlock
	"Split the file locator, fileLocatorString, in to the file drive and path, filename (without extension), and extension.  The fileLocatorString must be of the form:

	<dirPath><delimiter><localName>,

where <dirPath><delimiter> is optional.  The <dirPath> part may contain delimiters.  Example:

	| flPath flName flExt |
	nil portFunc fileLocatorSplit: 'C:\MyDir\Junk.txt'
		to: [ :flPathString :flNameString :flExtString |
			flPath := flPathString.
			flName := flNameString.
			flExt := flExtString.
		].

Definition: <PortableFunctions>
Parameters
	fileLocatorString	<readableString>		uncaptured
	pathFileExtBlock		<valuable>			uncaptured
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptError: messageString
	"Put up an error prompt with messageString.  Example:

	nil portFunc promptError: 'You are hungry.'

Definition: <PortableFunctions>
Parameters
	messageString	<readableString>	uncaptured
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptFatal: messageString
	"Put up a fatal prompt with messageString.  Example:

	nil portFunc promptFatal: 'You are hungry.'

Definition: <PortableFunctions>
Parameters
	messageString	<readableString>	uncaptured
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptInform: messageString
	"Put up a inform prompt with messageString.  Example:

	nil portFunc promptInform: 'You are hungry.'

Definition: <PortableFunctions>
Parameters
	messageString	<readableString>	uncaptured
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptPath: filelocPathString fileName: filelocNameString
extension: filelocExtString fileType: fileTypeObject title: titleString
	"Answer a <readableString> the file locator prompted from the user or an empty string if canceled with the initial file drive and path, filelocPathString, filename (without extension), filelocNameString, extension, filelocExtString, dialect specific file type, fileTypeObject, and title, titleString.  Example:

	nil portFunc promptPath: C:\MyDir'
		fileName: 'Junk'
		extension: 'txt'
		fileType: '*.txt'
		title: 'Open new program: '. 

Definition: <PortableFunctions>
Parameters
	filelocPathString	<readableString>	uncaptured
	filelocNameString	<readableString>	uncaptured
	filelocExtString		<readableString>	uncaptured
	fileTypeObject		<Object>			uncaptured
	titleString			<readableString>	uncaptured
Return Values
	<readableString>		new
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptWarn: messageString
	"Put up a warn prompt with messageString.  Example:

	nil portFunc promptWarn: 'You are hungry.'

Definition: <PortableFunctions>
Parameters
	messageString	<readableString>	uncaptured
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:22'!
promptYesNo:  messageString
	"Answer a <boolean>, true or false the user response to the message prompt with messageString.  Example:

	nil portFunc promptYesNo: 'Are you hungry?'

Definition: <PortableFunctions>
Parameters
	messageString	<readableString>	uncaptured
Return Values
	<boolean>		unspecified
"		#PortFunc. "Protocol: <PortableFunctions>."
	^self subclassResponsibility
! !

!PortableFunctions methodsFor: 'initializing' stamp: 'RAH 9/24/1999 06:22'!
initialize
	"Private - Initialize the instance.

Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	"None currently required."
! !


!PortableFunctions class methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:22'!
portableFunctionsInstance: aPortableFunctionsInstance
	"Set PortableFunctionsInstance to a <PortableFunctions>, the singleton instance of this dialect's portable functions subclass.

Definition: <PortableFunctions factory>
Parameters
	aPortableFunctionsInstance	<PortableFunctions>	captured
"		#PortFunc. "Protocol: <PortableFunctions factory>."

	PortableFunctionsInstance := aPortableFunctionsInstance.
! !

!PortableFunctions class methodsFor: 'instance creation' stamp: 'RAH 9/24/1999 06:22'!
current
	"Answer a <PortableFunctions>, the singleton instance of this dialect's portable functions subclass.

Definition: <PortableFunctions factory>
Return Values
	<PortableFunctions>		state
"		#PortFunc. "Protocol: <PortableFunctions factory>."

	^PortableFunctionsInstance
! !


!PositionableStream methodsFor: 'accessing' stamp: 'RAH 9/23/1999 10:13'!
nextLine
	"Answer a <readableString>, the next line (without line termination) in the receiver's element stream.  If at end or the answer is only separators return an empty String.

Return Values
	<readableString>		new
"	| c lineString |
		#PortFunc.
	self atEnd ifTrue: [		"Chg SQ"
		^String new
	].

	lineString := String new.
	
	"DOS/Windows on x86		CrLf
		 Unix					  Lf
		 Mac						Cr"
	[	self atEnd or: [c := self peek.
		(c = Character cr or: [c = Character lf])]
	] whileFalse: [
		lineString := lineString, (String with: self next)
	].
	(c = Character cr or: [c = Character lf]) ifTrue: [
		self next.
		(c = Character cr and: [self peek = Character lf]) ifTrue: [
			self next.
	]	].

	^lineString! !

!PositionableStream methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:19'!
nextToken
	"Answer a <readableString>, the next token in the receiver's element stream, delimited by elements which answer true to #isSeparator.  Answer an empty String if there are no more token in the receiver.

Return Values
	<readableString>		new
"	| token |
		#PortFunc.
	self atEnd ifTrue: [		"Chg SQ"
		^String new
	].
	[self peek isSeparator] whileTrue: [self next].
"	startPos := self position."
	token := String new.
	[self peek isSeparator] whileFalse: [
		token := token, (String with: self next)
	].
"	^self contents
		copyFrom: startPos
		to: self position

	self position: startPos.
	^self next: (self position - startPos + 1).
"
	^token! !

!PositionableStream methodsFor: 'accessing' stamp: 'RAH 9/24/1999 06:20'!
peekAllFor: aCollection
	"Answer a <boolean>, true if the next series of objects to be accessed in the receiver stream equal the objects in aCollection, else answer false.  Only advance the stream position if the answer is true.

Parameters
	aCollection	<collection>		uncaptured
Return Values
	<boolean>	unspecified
"	| startPosition |
		#PortFunc.
	startPosition := self position.
	aCollection do: [ :element |
		self atEnd ifTrue: [
			self position: startPosition.
			^false
		].
		element = self next ifFalse: [
			self position: startPosition.
			^false
	]   ].
	^true! !

!PositionableStream methodsFor: 'positioning' stamp: 'RAH 9/24/1999 06:21'!
skipLineTermination
	"Skip the following text line termination,
	Note: Dialect specific.
"		#PortFunc.
	self atEnd ifFalse: [	"Skip line termination."
		self peek = Character cr ifTrue: [
			self next.
			self atEnd ifFalse: [
				self peek = Character lf ifTrue: [
					self next.
	]	]	]	].! !


!SqueakPortableFunctions commentStamp: 'RAH 9/28/1999 07:11' prior: 0!
The PortableFunctions class comment is the starting point for the Portable Functions Protocols documentation.

Description
	Class SqueakPortableFunctions is a concrete subclass of PortableFunctions that implements the <PortableFunctions> behavior of functions commonly needed in applications but implemented differently in each dialect such as notifying the user, checking if a file exists, etc.

Note:

	It must initialize the class variable PortableFunctionsInstance with he singleton instance of itself.

	This also has toCr and toCrLf to fix line termination to be cr before installing code, or cr-lf (Windows norm) before checking into RCS.
!

!SqueakPortableFunctions methodsFor: 'constants' stamp: 'RAH 9/28/1999 07:03'!
currentDateTimeString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	#todoPortFunc. "Use DateTime class"
	^(Time dateAndTimeNow at: 1) printString,	"Chg SQ"
		' ', (Time dateAndTimeNow at: 2) printString
! !

!SqueakPortableFunctions methodsFor: 'constants' stamp: 'RAH 9/24/1999 06:28'!
dialectNameVersionString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	^Smalltalk version copy		"Chg SQ"
! !

!SqueakPortableFunctions methodsFor: 'file locator manipulation' stamp: 'RAH 9/24/1999 06:29'!
fileExists: locatorString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	^FileDirectory new fileExists: locatorString	"Chg SQ"! !

!SqueakPortableFunctions methodsFor: 'file locator manipulation' stamp: 'RAH 9/24/1999 06:30'!
fileLocatorFromPath: pathString fileName: fileString extension: extensionString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	#todoPortFunc. "chg to handle empty parms."
	^(FileDirectory on: pathString)	"Chg SQ"
		fullNameFor: (fileString,
			(String with: FileDirectory extensionDelimiter), extensionString).
! !

!SqueakPortableFunctions methodsFor: 'file locator manipulation' stamp: 'RAH 9/24/1999 06:32'!
fileLocatorSplit: fileLocatorString to: pathFileExtBlock
"Definition: <PortableFunctions>
"	| flPathString flNameString flExtString ndx |
		#PortFunc. "Protocol: <PortableFunctions>."
	FileDirectory	"Chg SQ"
		splitName: fileLocatorString
		to: [ :flPath :flNameExt |
			flPathString := flPath.
			flNameString := flNameExt.
		].
	ndx := flNameString indexOf: FileDirectory extensionDelimiter.
	ndx > 0 ifTrue: [
		flExtString := flNameString
			copyFrom: (ndx + 1)
			to: flNameString size.
		flNameString := flNameString
			copyFrom: 1
			to: (ndx - 1).
	] ifFalse: [
		flExtString := String new.
	].
	^pathFileExtBlock
		value: flPathString
		value: flNameString
		value: flExtString! !

!SqueakPortableFunctions methodsFor: 'initializing' stamp: 'RAH 9/24/1999 06:33'!
initialize
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	super initialize! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:34'!
promptError: messageString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	PopUpMenu notify: messageString.	"Chg SQ"
! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:34'!
promptFatal: messageString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	PopUpMenu notify: messageString.	"Chg SQ"
! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:35'!
promptInform: messageString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	PopUpMenu notify: messageString.	"Chg SQ"
! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:36'!
promptPath: filelocPathString
fileName: filelocNameString
extension: filelocExtString
fileType: fileTypeObject
title: titleString
"Definition: <PortableFunctions>
"	| fileLocatorString |
		#PortFunc. "Protocol: <PortableFunctions>."
#todoPortFunc. "Use a file requester built using FileList."
	fileLocatorString := FillInTheBlank	"Chg SQ"
		request: titleString
		initialAnswer: (filelocPathString, '\', filelocNameString, '.',
						filelocExtString).
	fileLocatorString isEmpty ifTrue: [
		^String new
	].
	^fileLocatorString
! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:36'!
promptWarn: messageString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	PopUpMenu notify: messageString.	"Chg SQ"
! !

!SqueakPortableFunctions methodsFor: 'prompting user' stamp: 'RAH 9/24/1999 06:37'!
promptYesNo:  messageString
"Definition: <PortableFunctions>
"		#PortFunc. "Protocol: <PortableFunctions>."
	^(SelectionMenu confirm: messageString) = true	"Chg SQ"! !

!SqueakPortableFunctions class methodsFor: 'initializing' stamp: 'RAH 9/27/1999 16:34'!
initialize
	"Class initialization.  Example:

		SqueakPortableFunctions initialize

Definition: <PortableFunctions factory>
"		#PortFunc. "Protocol: <PortableFunctions factory>."
	"Initialize the default portable functions instance."
	super portableFunctionsInstance: (self new
		initialize;
		yourself)
! !

!SqueakPortableFunctions class methodsFor: 'file converting' stamp: 'RAH 9/28/1999 07:04'!
toCr: pathFilename
	"Fix line termination to be only cr (Squeak internal norm)."
	| aDir filename inStream max path tmpFilename outStream |
		#PortFunc.
	FileDirectory
		splitName: pathFilename
		to: [ :pathString :filenameString |
			path := pathString.
			filename := filenameString.
	].
	tmpFilename := 'AJnkRAH.tmp'.

	aDir := FileDirectory on: path.
	inStream	:= aDir oldFileNamed: filename.
	(aDir fileExists: tmpFilename) ifTrue: [
		aDir deleteFileNamed: tmpFilename.
	].
	outStream	:= aDir newFileNamed: tmpFilename.
	max := inStream size.

	('Converting ', pathFilename, ' to Cr')
		displayProgressAt: Sensor cursorPoint
		from: 0 to: max
		during: [ :bar |
			[inStream atEnd] whileFalse: [
				bar value: inStream position.
				outStream
					nextPutAll: inStream nextLine; 
					cr.
		]	].

	inStream close.
	outStream close.
	aDir deleteFileNamed: filename.
	aDir
		rename: tmpFilename
		toBe: filename.
! !

!SqueakPortableFunctions class methodsFor: 'file converting' stamp: 'RAH 9/28/1999 07:04'!
toCrLf: pathFilename
	"Fix line termination to be cr-lf (Windows norm)."
	| aDir filename inStream max path tmpFilename outStream |
		#PortFunc.
	FileDirectory
		splitName: pathFilename
		to: [ :pathString :filenameString |
			path := pathString.
			filename := filenameString.
	].
	tmpFilename := 'AJnkRAH.tmp'.

	aDir := FileDirectory on: path.
	inStream	:= aDir oldFileNamed: filename.
	(aDir fileExists: tmpFilename) ifTrue: [
		aDir deleteFileNamed: tmpFilename.
	].
	outStream	:= aDir newFileNamed: tmpFilename.
	max := inStream size.

	('Converting ', pathFilename, ' to CrLf')
		displayProgressAt: Sensor cursorPoint
		from: 0 to: max
		during: [ :bar |
			[inStream atEnd] whileFalse: [
				bar value: inStream position.
		outStream
			nextPutAll: inStream nextLine; 
			nextPut: Character cr;
			nextPut: Character lf.
		]	].

	inStream close.
	outStream close.
	aDir deleteFileNamed: filename.
	aDir
		rename: tmpFilename
		toBe: filename.
! !


" Postinstall script.:"
	"Initialize classes:"
	(Array with: PortableFunctions
	) do: [ :rootClass |
		((OrderedCollection with: rootClass)
			addAll: rootClass allSubclasses;
			yourself
		) do: [ :aClass |
			(aClass includesSelector: #'initialize') ifTrue: [
				aClass initialize
	]	]	].!
