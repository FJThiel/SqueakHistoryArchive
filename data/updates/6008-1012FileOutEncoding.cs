'From Squeak3.7-m17n of 30 June 2004 [latest update: #6] on 9 July 2004 at 6:44:55 am'!"Change Set:		FileOutEncodingDate:			6 July 2004Author:			Yoshiki Ohshima"!!AbstractString methodsFor: 'converting' stamp: 'yo 7/5/2004 16:48'!findSelector	"Dan's code for hunting down selectors with keyword parts; while this doesn't give a true parse, in most cases it does what we want, in where it doesn't, we're none the worse for it."	| sel possibleParens level n |	sel _ self withBlanksTrimmed.	(sel includes: $:) ifTrue:		[sel _ sel copyReplaceAll: ':' with: ': '.	"for the style (aa max:bb) with no space"		possibleParens _ sel findTokens: Character separators.		sel _ self class streamContents:			[:s | level _ 0.			possibleParens do:				[:token |				(level = 0 and: [token endsWith: ':'])					ifTrue: [s nextPutAll: token]					ifFalse: [(n _ token occurrencesOf: $( ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $[ ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $] ) > 0 ifTrue: [level _ level - n].							(n _ token occurrencesOf: $) ) > 0 ifTrue: [level _ level - n]]]]].	sel isEmpty ifTrue: [^ nil].	sel isOctetString ifTrue: [sel _ sel asOctetString].	Symbol hasInterned: sel ifTrue:		[:aSymbol | ^ aSymbol].	^ nil! !!AbstractString methodsFor: 'converting' stamp: 'yo 7/5/2004 16:43'!withBlanksTrimmed	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."	| first result |	first _ self findFirst: [:c | c isSeparator not].	first = 0 ifTrue: [^ ''].  "no non-separator character"	result _  self		copyFrom: first		to: (self findLast: [:c | c isSeparator not]).	result isOctetString ifTrue: [^ result asOctetString] ifFalse: [^ result].	" ' abc  d   ' withBlanksTrimmed"! !!AbstractString methodsFor: 'converting' stamp: 'tak 4/25/2004 12:57'!withSeparatorsCompacted	"replace each sequences of whitespace by a single space character"	"' test ' withSeparatorsCompacted = ' test '"	"' test test' withSeparatorsCompacted = ' test test'"	"'test test		' withSeparatorsCompacted = 'test test '"	| out in next isSeparator |	self isEmpty ifTrue: [^ self].	out _ WriteStream on: (String new: self size).	in _ self readStream.	isSeparator _ [:char | char asciiValue < 256				and: [CSSeparators includes: char]].	[in atEnd] whileFalse: [		next _ in next.		(isSeparator value: next)			ifTrue: [				out nextPut: $ .				[in atEnd or:					[next _ in next.					(isSeparator value: next)						ifTrue: [false]						ifFalse: [out nextPut: next. true]]] whileFalse]			ifFalse: [out nextPut: next]].	^ out contents! !!ChangeSet methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:16'!fileOut	"File out the receiver, to a file whose name is a function of the  	change-set name and either of the date & time or chosen to have a  	unique numeric tag, depending on the preference  	'changeSetVersionNumbers'"	| slips nameToUse internalStream |	self checkForConversionMethods.	ChangeSet promptForDefaultChangeSetDirectoryIfNecessary.	nameToUse := Preferences changeSetVersionNumbers				ifTrue: [self defaultChangeSetDirectory nextNameFor: self name extension: FileStream cs]				ifFalse: [self name , FileDirectory dot , Utilities dateTimeSuffix, FileDirectory dot , FileStream cs].	(Preferences warningForMacOSFileNameLength			and: [nameToUse size > 30])		ifTrue: [nameToUse := FillInTheBlank						request: (nameToUse , '\has ' , nameToUse size asString , ' letters - too long for Mac OS.\Suggested replacement is:') withCRs						initialAnswer: (nameToUse contractTo: 30).			nameToUse = ''				ifTrue: [^ self]].	Cursor write showWhile: [			internalStream _ WriteStream on: (String new: 10000).			internalStream header; timeStamp.			self fileOutPreambleOn: internalStream.			self fileOutOn: internalStream.			self fileOutPostscriptOn: internalStream.			internalStream trailer.			FileStream writeSourceCodeFrom: internalStream baseName: (nameToUse copyFrom: 1 to: nameToUse size - 3) isSt: false useHtml: false.	].	Preferences checkForSlips		ifFalse: [^ self].	slips := self checkForSlips.	(slips size > 0			and: [(PopUpMenu withCaption: 'Methods in this fileOut have haltsor references to the Transcriptor other ''slips'' in them.Would you like to browse them?' chooseFrom: 'Ignore\Browse slips')					= 2])		ifTrue: [self systemNavigation browseMessageList: slips name: 'Possible slips in ' , name]! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:16'!fileOutCategory: catName asHtml: useHtml	"FileOut the named category, possibly in Html format."	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	internalStream header; timeStamp.	self fileOutCategory: catName on: internalStream moveSource: false toFile: 0.	internalStream trailer.	FileStream writeSourceCodeFrom: internalStream baseName: (self name , '-' , catName) isSt: true useHtml: useHtml.! !!ClassDescription methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:16'!fileOutMethod: selector asHtml: useHtml	"Write source code of a single method on a file in .st or .html format"	| internalStream |	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].	internalStream _ WriteStream on: (String new: 1000).	internalStream header; timeStamp.	self printMethodChunk: selector withPreamble: true		on: internalStream moveSource: false toFile: 0.	FileStream writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '')) isSt: true useHtml: useHtml.! !!Class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:16'!fileOutAsHtml: useHtml	"File a description of the receiver onto a new file whose base name is the name of the receiver."	| internalStream |	internalStream _ WriteStream on: (String new: 100).	internalStream header; timeStamp.	self sharedPools size > 0 ifTrue: [		self shouldFileOutPools			ifTrue: [self fileOutSharedPoolsOn: internalStream]].	self fileOutOn: internalStream moveSource: false toFile: 0.	internalStream trailer.	FileStream writeSourceCodeFrom: internalStream baseName: self name isSt: true useHtml: useHtml.! !!Class class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:16'!fileOutPool: aString	"file out the global pool named aString"	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	self new fileOutPool: (self environment at: aString asSymbol) onFileStream: internalStream.	FileStream writeSourceCodeFrom: internalStream baseName: aString isSt: true useHtml: false.! !!CodeHolder methodsFor: 'message list menu' stamp: 'yo 7/5/2004 11:36'!messageListKey: aChar from: view	"Respond to a Command key.  I am a model with a code pane, and I also	have a listView that has a list of methods.  The view knows how to get	the list and selection."	| sel class |	aChar == $D ifTrue: [^ self toggleDiffing].	sel _ self selectedMessageName.	aChar == $m ifTrue:  "These next two put up a type in if no message selected"		[^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: self systemNavigation].	aChar == $n ifTrue: 		[^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: self systemNavigation].	"The following require a class selection"	(class _ self selectedClassOrMetaClass) ifNil: [^ self arrowKey: aChar from: view].	aChar == $b ifTrue: [^ Browser fullOnClass: class selector: sel].	aChar == $N ifTrue: [^ self browseClassRefs].	aChar == $i ifTrue: [^ self methodHierarchy].	aChar == $h ifTrue: [^ self classHierarchy].	aChar == $p ifTrue: [^ self browseFullProtocol].	"The following require a method selection"	sel ifNotNil: 		[aChar == $o ifTrue: [^ self fileOutMessage].		aChar == $c ifTrue: [^ self copySelector].		aChar == $v ifTrue: [^ self browseVersions].		aChar == $O ifTrue: [^ self openSingleMessageBrowser].		aChar == $x ifTrue: [^ self removeMessage].		aChar == $d ifTrue: [^ self removeMessageFromBrowser].		(aChar == $C and: [self canShowMultipleMessageCategories])			ifTrue: [^ self showHomeCategory]].	^ self arrowKey: aChar from: view! !!ChangeList methodsFor: 'menu actions' stamp: 'yo 7/5/2004 20:16'!fileOutSelections 	| fileName internalStream |	fileName _ FillInTheBlank request: 'Enter the base of file name' initialAnswer: 'Filename'.	internalStream _ WriteStream on: (String new: 1000).	internalStream header; timeStamp.	listSelections with: changeList do: 		[:selected :item | selected ifTrue: [item fileOutOn: internalStream]].	FileStream writeSourceCodeFrom: internalStream baseName: fileName isSt: true useHtml: false.! !!ChangeList class methodsFor: 'public access' stamp: 'yo 7/6/2004 19:42'!browseStream: changesFile	"Opens a changeList on a fileStream"	| changeList charCount |	changesFile readOnly.	charCount _ changesFile size.	charCount > 1000000 ifTrue:		[(self confirm: 'The file ', changesFile name , 'is really long (' , charCount printString , ' characters).Would you prefer to view only the last million characters?')			ifTrue: [charCount _ 1000000]].	"changesFile setEncoderForSourceCodeNamed: changesFile name."	Cursor read showWhile:		[changeList _ self new			scanFile: changesFile from: changesFile size-charCount to: changesFile size].	changesFile close.	self open: changeList name: changesFile localName , ' log' multiSelect: true! !!ChangeSorter class methodsFor: 'adding' stamp: 'yo 7/6/2004 20:51'!newChangesFromStream: aStream named: aName	"File in the code from the stream into a new change set whose	name is derived from aName. Leave the 'current change set'	unchanged. Return the new change set or nil on failure."	| oldChanges newName newSet newStream |	oldChanges _ ChangeSet current.	PreviousSet _ oldChanges name. 		"so a Bumper update can find it"	newName _ aName sansPeriodSuffix.	newSet _ self basicNewChangeSet: newName.	[newSet ifNotNil:		[		(aStream respondsTo: #converter:) ifFalse: [			newStream _ MultiByteBinaryOrTextStream with: (aStream contentsOfEntireFile).			newStream reset.		] ifTrue: [			newStream _ aStream.		].		newStream converter: MacRomanTextConverter new.		"newStream setEncoderForSourceCodeNamed: aName."		ChangeSet  newChanges: newSet.		newStream fileInAnnouncing: 'Loading ', newName, '...'.		Transcript cr; show: 'File ', aName, ' successfully filed in to change set ', newName].	aStream close] ensure: [			ChangeSet  newChanges: oldChanges].	^ newSet! !!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:45'!fileIntoNewChangeSet: fullName	"File in all of the contents of the currently selected file, if any, into a new change set." 	| fn ff |	fullName ifNil: [^ Beeper beep].	ff _ FileStream readOnlyFileNamed: (fn _ GZipReadStream uncompressedFileName: fullName).	self newChangesFromStream: ff named: (FileDirectory localNameFor: fn)! !!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 15:52'!fileReaderServicesForFile: fullName suffix: suffix	^ (FileStream isSourceFileSuffix: suffix)		ifTrue: [ self services]		ifFalse: [#()]! !!FileContentsBrowser methodsFor: 'fileIn/fileOut' stamp: 'yo 7/5/2004 20:48'!fileIntoNewChangeSet	| p ff |	(p _ self selectedPackage) ifNil: [^ Beeper beep].	ff _ FileStream readOnlyFileNamed: p fullPackageName.	ChangeSorter newChangesFromStream: ff named: p packageName! !!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'yo 7/5/2004 22:32'!browseCompressedCodeStream: aStandardFileStream 	"Browse the selected file in fileIn format."	| zipped unzipped |	zipped _ GZipReadStream on: aStandardFileStream.	unzipped _ MultiByteBinaryOrTextStream with: zipped contents asString.	unzipped reset.	self browseStream: unzipped named: aStandardFileStream name.! !!FileContentsBrowser class methodsFor: 'instance creation' stamp: 'yo 7/6/2004 20:51'!browseStream: aStream named: aString	| package organizer packageDict browser |	Cursor wait showWhile: [		packageDict _ Dictionary new.		organizer _ SystemOrganizer defaultList: Array new.		(aStream respondsTo: #converter:) ifTrue: [			aStream converter: MacRomanTextConverter new.			"aStream setEncoderForSourceCodeNamed: aString."		].		package _ (FilePackage new fullName: aString; fileInFrom: aStream).		packageDict 			at: package packageName 			put: package.		organizer 			classifyAll: package classes keys 			under: package packageName.		(browser := self new)			systemOrganizer: organizer;			packages: packageDict].	self		openBrowserView: browser createViews		label: 'File Contents Browser'.! !!FileList methodsFor: 'file list menu' stamp: 'yo 7/5/2004 20:17'!fileContentsMenu: aMenu shifted: shifted	"Construct aMenu to have items appropriate for the file browser's code pane, given the shift state provided"	| shiftMenu services maybeLine extraLines |	shifted ifTrue:		[shiftMenu _ ParagraphEditor shiftedYellowButtonMenu.		^ aMenu 			labels: shiftMenu labelString 			lines: shiftMenu lineArray			selections: shiftMenu selections].	fileName ifNotNil:		[services _ OrderedCollection new.		(#(briefHex briefFile needToGetBriefHex needToGetBrief) includes: brevityState) ifTrue:			[services add: self serviceGet].		(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse:			[services add: self serviceGetHex].		(#(needToGetShiftJIS needToGetEUCJP needToGetCNGB needToGetEUCKR needToGetUTF8) includes: brevityState) ifFalse:			[services add: self serviceGetEncodedText].		maybeLine _ services size.		(FileStream sourceFileSuffixes includes: self suffixOfSelectedFile) ifTrue:			[services addAll:				(self servicesFromSelectorSpecs:					#(fileIntoNewChangeSet: fileIn: browseChangesFile: browseFile:))].		extraLines _ OrderedCollection new.		maybeLine > 0 ifTrue: [extraLines add: maybeLine].		services size > maybeLine ifTrue: [extraLines add: services size].		aMenu 			addServices: services			for: self fullName			extraLines: extraLines].	aMenu addList: {			{'find...(f)' translated.		#find}.			{'find again (g)' translated.		#findAgain}.			{'set search string (h)' translated.	#setSearchString}.			#-.			{'do again (j)' translated.		#again}.			{'undo (z)' translated.			#undo}.			#-.			{'copy (c)' translated.			#copySelection}.			{'cut (x)' translated.			#cut}.			{'paste (v)' translated.		#paste}.			{'paste...' translated.			#pasteRecent}.			#-.			{'do it (d)' translated.		#doIt}.			{'print it (p)' translated.		#printIt}.			{'inspect it (i)' translated.		#inspectIt}.			{'fileIn selection (G)' translated.	#fileItIn}.			#-.			{'accept (s)' translated.		#accept}.			{'cancel (l)' translated.		#cancel}.			#-.			{'more...' translated.			#shiftedYellowButtonActivity}}.	^ aMenu! !!FileList methodsFor: 'private' stamp: 'yo 7/5/2004 19:41'!contents	"Answer the contents of the file, reading it first if needed."	"Possible brevityState values:		FileList,		fullFile, briefFile, needToGetFull, needToGetBrief,		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"	(listIndex = 0) | (brevityState == #FileList) ifTrue: [^ self defaultContents].  "no file selected"	brevityState == #fullFile ifTrue: [^ contents].	brevityState == #fullHex ifTrue: [^ contents].	brevityState == #briefFile ifTrue: [^ contents].	brevityState == #briefHex ifTrue: [^ contents].	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false].	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true].	brevityState == #needToGetFull ifTrue: [^ self readContentsBrief: false].	brevityState == #needToGetBrief ifTrue: [^ self readContentsBrief: true].  "default"	(TextConverter allEncodingNames includes: brevityState) 		ifTrue: [ ^self readContentsAsEncoding: brevityState].	self halt: 'unknown state ' , brevityState printString! !!FileList methodsFor: 'private' stamp: 'yo 7/6/2004 20:52'!defaultEncoderFor: aFileName	"This method just illustrates the stupidest possible implementation of encoder selection."	| l |	l _ aFileName asLowercase."	((l endsWith: FileStream multiCs) or: [		l endsWith: FileStream multiSt]) ifTrue: [		^ UTF8TextConverter new.	]."	((l endsWith: FileStream cs) or: [		l endsWith: FileStream st]) ifTrue: [		^ MacRomanTextConverter new.	].	^ Latin1TextConverter new.	! !!FileList methodsFor: 'private' stamp: 'yo 7/5/2004 19:43'!readContentsBrief: brevityFlag	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."	| f fileSize first5000 |	brevityFlag ifTrue: [		directory isRemoteDirectory ifTrue: [^ self readServerBrief]].	f := directory oldFileOrNoneNamed: self fullName.	f converter: (self defaultEncoderFor: self fullName).	f ifNil: [^ 'For some reason, this file cannot be read' translated].	(brevityFlag not or: [(fileSize := f size) <= 100000]) ifTrue:		[contents := f contentsOfEntireFile.		brevityState := #fullFile.   "don't change till actually read"		^ contents].	"if brevityFlag is true, don't display long files when first selected"	first5000 := f next: 5000.	f close.	contents := 'File ''{1}'' is {2} bytes long.You may use the ''get'' command to read the entire file.Here are the first 5000 characters...------------------------------------------{3}------------------------------------------... end of the first 5000 characters.' translated format: {fileName. fileSize. first5000}.	brevityState := #briefFile.   "don't change till actually read"	^ contents.! !!HTTPSocket class methodsFor: 'get the page' stamp: 'yo 7/5/2004 20:51'!httpGet: url args: args accept: mimeType request: requestString	"Return the exact contents of a web object. Asks for the given MIME type. If mimeType is nil, use 'text/html'. The parsed header is saved. Use a proxy server if one has been registered.  tk 7/23/97 17:12"	"Note: To fetch raw data, you can use the MIME type 'application/octet-stream'."	| document |	document _ self httpGetDocument: url  args: args  accept: mimeType request: requestString.	(document isKindOf: String) ifTrue: [		"strings indicate errors"		^ document ].	^ (RWBinaryOrTextStream with: document content) reset! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'yo 7/5/2004 16:38'!selectedSymbol	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"	| aString |	self hasCaret ifTrue: [^ nil].	aString _ self selection string.	aString isOctetString ifTrue: [aString _ aString asOctetString].	aString _ aString copyWithoutAll:		{Character space.  Character cr.  Character tab}.	aString size == 0 ifTrue: [^ nil].	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].	^ nil! !!FileStream methodsFor: 'fileIn/Out' stamp: 'yo 7/6/2004 20:52'!fileIn	"Guarantee that the receiver is readOnly before fileIn for efficiency and	to eliminate remote sharing conflicts."	self readOnly."	((self localName asLowercase endsWith: (FileStream multiSt)) or: [		self localName asLowercase endsWith: (FileStream multiCs)]) ifTrue: [			self converter: UTF8TextConverter new.	]."	self fileInAnnouncing: 'Loading ', self localName! !!FileStream class methodsFor: 'file reader services' stamp: 'yo 7/6/2004 19:45'!fileIn: fullName	"File in the entire contents of the file specified by the name provided"	| ff |	fullName ifNil: [^ Beeper beep].	ff _ self readOnlyFileNamed: (GZipReadStream uncompressedFileName: fullName).	ff converter: MacRomanTextConverter new.	"ff setEncoderForSourceCodeNamed: fullName."	ff fileIn.! !!FileStream class methodsFor: 'file reader services' stamp: 'yo 7/8/2004 06:30'!writeUTF8PreambleOn: aStream	aStream nextPutAll: '"<utf-8>"| me |ctx _ thisContext.[ctx == nil or:[ctx receiver isKindOf: FileStream]] whileFalse:[ctx _ ctx sender].ctx ifNotNil:[me _ ctx receiver].me ifNotNil: [me converter: UTF8TextConverter new.]!!'! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'yo 7/6/2004 20:53'!fileIn: fullFileName	"FileIn the contents of a gzipped file"	| zipped unzipped |	zipped _ self on: (FileStream readOnlyFileNamed: fullFileName).	unzipped _ MultiByteBinaryOrTextStream with: (zipped contents asString).	unzipped reset.	unzipped converter: MacRomanTextConverter new.	"unzipped setEncoderForSourceCodeNamed: fullFileName."	unzipped fileIn.! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 21:32'!fileIntoNewChangeSet: fullFileName	"FileIn the contents of a gzipped file"	| zipped unzipped cs |	cs _ Smalltalk at: #ChangeSorter ifAbsent: [ ^self ].	zipped _ self on: (FileStream readOnlyFileNamed: fullFileName).	unzipped _ MultiByteBinaryOrTextStream with: zipped contents asString.	unzipped reset.	cs newChangesFromStream: unzipped named: (FileDirectory localNameFor: fullFileName)! !!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 21:10'!fileReaderServicesForFile: fullName suffix: suffix 	| services |	(suffix = 'gz') | (suffix = '*')		ifFalse: [^ #()].	services _ OrderedCollection new.	(suffix = '*') | (fullName asLowercase endsWith: '.cs.gz') | (fullName asLowercase endsWith: '.mcs.gz')		ifTrue: [services add: self serviceFileIn.			(Smalltalk includesKey: #ChangeSorter)				ifTrue: [services add: self serviceFileIntoNewChangeSet]].	services addAll: self services.	^ services! !!MultiByteBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'yo 7/8/2004 10:13'!fileIn	super fileIn.! !!MultiByteBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'yo 7/5/2004 18:02'!guessConverter	(self originalContents includesSubString: (String with: (Character value: 16r1B) with: (Character value: 16r24))) ifTrue: [		^ CompoundTextConverter new.	] ifFalse: [		^ UTF8TextConverter new.	].! !!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'yo 7/5/2004 20:21'!fileOut	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	self fileOutOn: internalStream.	self needsInitialize ifTrue:[		internalStream cr; nextChunkPut: self name,' initialize'.	].	FileStream writeSourceCodeFrom: internalStream baseName: self name isSt: true useHtml: false.! !!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'yo 7/5/2004 20:21'!fileOutCategory: categoryName	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	self fileOutMethods: (self organization listAtCategoryNamed: categoryName)			on: internalStream.	FileStream writeSourceCodeFrom: internalStream baseName: (self name, '-', categoryName) isSt: true useHtml: false.! !!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'yo 7/5/2004 20:21'!fileOutMethod: selector	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	self fileOutMethods: (Array with: selector) on: internalStream.	FileStream writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '')) isSt: true useHtml: false.! !!SARInstaller methodsFor: 'client services' stamp: 'yo 7/5/2004 15:56'!fileInMemberNamedAsUTF8: csName	"This is to be used from preamble/postscript code to file in zip members as ChangeSets."	| cs stream |	cs _ self memberNamed: csName.	cs ifNil: [ ^self errorNoSuchMember: csName ].	stream _ cs contentStream.	stream ascii.	stream converter: UTF8TextConverter new.	self class fileIntoChangeSetNamed: csName fromStream: stream.	self installed: cs.! !!SARInstaller methodsFor: 'client services' stamp: 'yo 7/6/2004 20:54'!installMember: memberOrName	| memberName extension isGraphic stream member |	member _ self memberNamed: memberOrName.	member ifNil: [ ^false ].	memberName _ member fileName.	extension _ (FileDirectory extensionFor: memberName) asLowercase.	Smalltalk at: #CRDictionary ifPresent: [ :crDictionary |		(extension = crDictionary fileNameSuffix) ifTrue: [  self fileInGenieDictionaryNamed: memberName. ^true ] ].	extension caseOf: {		[ Project projectExtension ] -> [ self fileInProjectNamed: memberName createView: true ].		[ FileStream st ] -> [ self fileInPackageNamed: memberName ].		[ FileStream cs ] -> [  self fileInMemberNamed: memberName  ]."		[ FileStream multiSt ] -> [  self fileInMemberNamedAsUTF8: memberName  ].		[ FileStream multiCs ] -> [  self fileInMemberNamedAsUTF8: memberName  ]."		[ 'mc' ] -> [ self fileInMonticelloPackageNamed: memberName ].		[ 'mcv' ] -> [ self fileInMonticelloVersionNamed: memberName ].		[ 'mcz' ] -> [ self fileInMonticelloZipVersionNamed: memberName ].		[ 'morph' ] -> [ self fileInMorphsNamed: member addToWorld: true ].		[ 'ttf' ] -> [ self fileInTrueTypeFontNamed: memberName ].	} otherwise: [		('t*xt' match: extension) ifTrue: [ self openTextFile: memberName ]			ifFalse: [ stream _ member contentStream.		isGraphic _ ImageReadWriter understandsImageFormat: stream.		stream reset.		isGraphic			ifTrue: [ self openGraphicsFile: member ]			ifFalse: [ "now what?" ^false ]]	].	^true! !!SMDefaultInstaller methodsFor: 'private' stamp: 'yo 7/7/2004 06:17'!fileIn	"Installing in the standard installer is simply filing in.	Both .st and .cs files will file into a ChangeSet of their own.	We let the user confirm filing into an existing ChangeSet	or specify another ChangeSet name if	the name derived from the filename already exists."		| fileStream |	((unpackedFileName endsWith: (FileDirectory dot, FileStream st))		or: [unpackedFileName endsWith: (FileDirectory dot, FileStream cs)])		ifTrue:[			fileStream _ dir readOnlyFileNamed: unpackedFileName.			fileStream converter: MacRomanTextConverter new.			self fileIntoChangeSetNamed: (fileStream localName sansPeriodSuffix)				fromStream: fileStream.			^ self]."	((unpackedFileName endsWith: (FileDirectory dot, FileStream multiSt))		or: [unpackedFileName endsWith: (FileDirectory dot, FileStream multiCs)])		ifTrue:[			fileStream _ dir readOnlyFileNamed: unpackedFileName.			fileStream converter: UTF8TextConverter new.			self fileIntoChangeSetNamed: (fileStream localName sansPeriodSuffix)				fromStream: fileStream.			^ self]."	self error: 'Filename should end with a proper extension'.! !!SMDefaultInstaller class methodsFor: 'testing' stamp: 'yo 7/5/2004 20:21'!canInstall: aPackage	"Answer if this class can install/upgrade the package.	This installer handles .st, .cs, .st.gz and .cs.gz files."	| fileName |	fileName _ aPackage downloadFileName.	fileName ifNil: [^false].	fileName _ fileName asLowercase.	^ FileStream sourceFileSuffixes anySatisfy: [:each | 		(fileName endsWith: (FileDirectory dot, each)) or: [			fileName endsWith: (FileDirectory dot, each, '.gz')].	].! !!SMLoader class methodsFor: 'private-publishing' stamp: 'yo 7/5/2004 20:21'!publish	| pi versionNo packagedFileName packageFile sd initialPackagedFileName |	pi _ PackageInfo named: 'SM-Loader'.	versionNo := FillInTheBlank request: 'Version number for this file?'.	pi fileOut.	initialPackagedFileName := pi externalName,'.st'.	packagedFileName _ pi externalName, '.', versionNo asString, (FileDirectory dot, FileStream cs).	FileDirectory default rename: initialPackagedFileName toBe: packagedFileName.	GZipWriteStream compressFile: packagedFileName.	packageFile := FileDirectory default readOnlyFileNamed: packagedFileName.	sd := ServerDirectory new.	sd		user: 'dvf';		server: 'modules.squeakfoundation.org';		password: (FillInTheBlank request: 'password?');		directory: 'Packages/';		putFile: packageFile named: packagedFileName! !!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:22'!fileOut  "SystemOrganization fileOut"	| internalStream |	internalStream _ WriteStream on: (String new: 30000).	internalStream nextPutAll: 'SystemOrganization changeFromCategorySpecs: #('; cr;		print: SystemOrganization;  "ends with a cr"		nextPutAll: ')!!'; cr.	FileStream writeSourceCodeFrom: internalStream baseName: (FileDirectory default nextNameFor: 'SystemOrganization' extension: 'st') isSt: true useHtml: false! !!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'yo 7/5/2004 20:22'!fileOutCategory: category asHtml: useHtml	"FileOut all the classes in the named system category."	| internalStream |	internalStream _ WriteStream on: (String new: 1000).	self fileOutCategory: category on: internalStream initializing: true.	FileStream writeSourceCodeFrom: internalStream baseName: category isSt: true useHtml: useHtml.! !!TextConverter class methodsFor: 'utilities' stamp: 'yo 7/5/2004 19:41'!allEncodingNames	"TextConverter allEncodingNames"	| encodingNames |	encodingNames _ Set new.	self allSubclasses		do: [:each | 			| names | 			names _ each encodingNames.			names notEmpty				ifTrue: [encodingNames add: names first asSymbol]].	^encodingNames! !TextConverter class removeSelector: #allEncodingStates!MultiByteFileStream class removeSelector: #codeExtensions!MultiByteFileStream class removeSelector: #cs!MultiByteFileStream class removeSelector: #multiCs!MultiByteFileStream class removeSelector: #multiSt!MultiByteFileStream class removeSelector: #sourceFileSuffixes!MultiByteFileStream class removeSelector: #st!MultiByteFileStream class removeSelector: #writeSourceCodeFrom:BaseName:!MultiByteFileStream class removeSelector: #writeSourceCodeFrom:baseName:!MultiByteFileStream class removeSelector: #writeSourceCodeFrom:baseName:asHtml:!MultiByteFileStream class removeSelector: #writeSourceCodeFrom:baseName:isSt:useHtml:!MultiByteFileStream class removeSelector: #writeSourceCodeFrom:baseName:useHtml:!MultiByteFileStream removeSelector: #setEncoderForSourceCode!MultiByteFileStream removeSelector: #setEncoderForSourceCodeNamed:!!MultiByteFileStream reorganize!('access' accepts: ascii binary converter converter: fileInEncodingName: lineEndConvention: wantsLineEndConversion:)('public' next next: nextDelimited: nextMatchAll: nextPut: nextPutAll: peek peekFor: skipSeparators skipSeparatorsAndPeekNext upTo: upToEnd)('crlf private' bareNext convertStringFromCr: convertStringToCr: detectLineEndConvention doConversion next:innerFor: wantsLineEndConversion)('private basic' basicNext basicNext: basicNext:into: basicNextInto: basicNextPut: basicNextPutAll: basicPeek basicPosition basicPosition: basicReadInto:startingAt:count: basicSetToEnd basicSkip: basicUpTo: basicVerbatim:)('open/close' open:forWrite: reset)('remnant' filterFor:)('private')!HtmlFileStream removeSelector: #copyPreamble:from:!FileStream class removeSelector: #writeUTF8PreambleTo:!FileStream removeSelector: #cs!FileStream removeSelector: #multiCs!FileStream removeSelector: #multiSt!FileStream removeSelector: #sourceFileSuffixes!FileStream removeSelector: #st!FileStream removeSelector: #writeSourceCodeFrom:baseName:isSt:useHtml:!PositionableStream removeSelector: #copyMethodWithPreamble:from:!PositionableStream removeSelector: #copyPreamble:from:!Class removeSelector: #fileOutAsHtml2:!