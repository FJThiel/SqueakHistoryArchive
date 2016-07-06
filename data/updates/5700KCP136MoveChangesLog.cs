'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5623] on 16 January 2004 at 3:54:47 pm'!"Change Set:		KCP-0116-MovingChangesLogDate:			16 January 2004Author:			Nathanael Schaerli & Stephane DucasseMoved changes file logging from SystemDictionary into SmalltalkImage.(added missing method timestamps -dew)"!!ClassDescription methodsFor: 'method dictionary' stamp: 'NS 1/16/2004 15:41'!removeSelector: selector 	| priorMethod priorProtocol | 	"Remove the message whose selector is given from the method 	dictionary of the receiver, if it is there. Answer nil otherwise."	(self methodDict includesKey: selector) ifFalse: [^ nil].	priorMethod _ self compiledMethodAt: selector.	priorProtocol := self whichCategoryIncludesSelector: selector.	ChangeSet current removeSelector: selector class: self		priorMethod: priorMethod		lastMethodInfo: {priorMethod sourcePointer.						priorProtocol}.	super removeSelector: selector.	self organization removeElement: selector.	self acceptsLoggingOfCompilation ifTrue:		[SmalltalkImage current logChange: self name , ' removeSelector: #' , selector].	SystemChangeNotifier uniqueInstance 			methodRemoved: selector inProtocol: priorProtocol fromClass: self! !!CompiledMethod methodsFor: 'source code management' stamp: 'NS 1/16/2004 15:39'!putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock	"Store the source code for the receiver on an external file.	If no sources are available, i.e., SourceFile is nil, then store	temp names for decompilation at the end of the method.	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,	in each case, storing a 4-byte source code pointer at the method end."	| file remoteString  st80str |	(SourceFiles == nil or: [(file _ SourceFiles at: fileIndex) == nil]) ifTrue:		[^ self become: (self copyWithTempNames: methodNode tempNames)].	SmalltalkImage current assureStartupStampLogged.	file setToEnd.	preambleBlock value: file.  "Write the preamble"	(methodNode isKindOf: DialectMethodNode)		ifTrue:		["This source was parsed from an alternate syntax.		We must convert to ST80 before logging it."		st80str _ (DialectStream dialect: #ST80 contents: [:strm | methodNode printOn: strm])						asString.		remoteString _ RemoteString newString: st80str						onFileNumber: fileIndex toFile: file]		ifFalse:		[remoteString _ RemoteString newString: sourceStr						onFileNumber: fileIndex toFile: file].	file nextChunkPut: ' '.	InMidstOfFileinNotification signal ifFalse: [file flush].	self checkOKToAdd: sourceStr size at: remoteString position.	self setSourcePosition: remoteString position inFile: fileIndex! !!Compiler class methodsFor: 'evaluating' stamp: 'NS 1/16/2004 15:41'!evaluate: textOrString for: anObject notifying: aController logged: logFlag	"Compile and execute the argument, textOrString with respect to the class 	of anObject. If a compilation error occurs, notify aController. If both 	compilation and execution are successful then, if logFlag is true, log 	(write) the text onto a system changes file so that it can be replayed if 	necessary."	| val |	val _ self new				evaluate: textOrString				in: nil				to: anObject				notifying: aController				ifFail: [^nil].	logFlag ifTrue: [Smalltalk logChange: textOrString].	^val! !!ParagraphEditor methodsFor: 'do-its' stamp: 'NS 1/16/2004 15:41'!evaluateSelection	"Treat the current selection as an expression; evaluate it and return the result"	| result rcvr ctxt |	self lineSelectAndEmptyCheck: [^ ''].	(model respondsTo: #doItReceiver) 		ifTrue: [FakeClassPool adopt: model selectedClass.  "Include model pool vars if any"				rcvr _ model doItReceiver.				ctxt _ model doItContext]		ifFalse: [rcvr _ ctxt _ nil].	result _ [		rcvr class evaluatorClass new 			evaluate: self selectionAsStream			in: ctxt			to: rcvr			notifying: self			ifFail: [FakeClassPool adopt: nil. ^ #failedDoit]	] 		on: OutOfScopeNotification 		do: [ :ex | ex resume: true].	FakeClassPool adopt: nil.	SmalltalkImage current logChange: self selection string.	^ result! !!PositionableStream methodsFor: 'fileIn/Out' stamp: 'NS 1/16/2004 15:41'!fileInAnnouncing: announcement 	"This is special for reading expressions from text that has been formatted 	with exclamation delimitors. The expressions are read and passed to the 	Compiler. Answer the result of compilation.  Put up a progress report with     the given announcement as the title."	| val chunk |	announcement 		displayProgressAt: Sensor cursorPoint		from: 0		to: self size		during: 			[:bar | 			[self atEnd] whileFalse: 					[bar value: self position.					self skipSeparators.										[val := (self peekFor: $!!) 								ifTrue: [(Compiler evaluate: self nextChunk logged: false) scanFrom: self]								ifFalse: 									[chunk := self nextChunk.									self checkForPreamble: chunk.									Compiler evaluate: chunk logged: true]] 							on: InMidstOfFileinNotification							do: [:ex | ex resume: true].					self skipStyleChunk].			self close].	"Note:  The main purpose of this banner is to flush the changes file."	SmalltalkImage current logChange: '----End fileIn of ' , self name , '----'.	self flag: #ThisMethodShouldNotBeThere.	"sd"	self systemNavigation allBehaviorsDo: 			[:cl | 			cl				removeSelectorSimply: #DoIt;				removeSelectorSimply: #DoItIn:].	^val! !!PositionableStream methodsFor: '*Project-SAR-fileIn' stamp: 'NS 1/16/2004 15:42'!fileInFor: client announcing: announcement	"This is special for reading expressions from text that has been formatted 	with exclamation delimitors. The expressions are read and passed to the 	Compiler. Answer the result of compilation.  Put up a progress report with     the given announcement as the title.	Does NOT handle preambles or postscripts specially."	| val chunk |	announcement displayProgressAt: Sensor cursorPoint		from: 0 to: self size		during:		[:bar |		[self atEnd]			whileFalse: 				[bar value: self position.				self skipSeparators.				[ val _ (self peekFor: $!!) ifTrue: [						(Compiler evaluate: self nextChunk for: client logged: false) scanFrom: self					] ifFalse: [						chunk _ self nextChunk.						self checkForPreamble: chunk.						Compiler evaluate: chunk for: client logged: true ].				] on: InMidstOfFileinNotification				  do: [ :ex | ex resume: true].				self atEnd ifFalse: [ self skipStyleChunk ]].		self close].	"Note:  The main purpose of this banner is to flush the changes file."	SmalltalkImage current logChange: '----End fileIn of ' , self name , '----'.	Smalltalk systemNavigation allBehaviorsDo: [ :cl | 		cl removeSelectorSimply: #DoIt; removeSelectorSimply: #DoItIn:	].	^ val! !!SMSqueakMap methodsFor: 'private' stamp: 'NS 1/16/2004 15:42'!noteInstalled: aPackage version: version	"The package was just successfully installed.	<aPackage> can be either an instance of SMCard or a String encoding a UUID.	We record the fact in our Dictionary of installed packages	and log a 'do it' to mark this in the changelog.	The doit helps keeping track of the packages when	recovering changes etc - not a perfect solution but should help.	The map used is the default map.	The id of the package is the key and the value is an OrderedCollection	of Arrays with the version, the point in time and the current installCounter."	| time installs name id |	aPackage isString		ifTrue: [name _ '<unknown package name>'. id _ UUID fromString: aPackage]		ifFalse: [name _ aPackage name. id _ aPackage id].	installedPackages ifNil: [installedPackages _ Dictionary new].	time _ Time totalSeconds.	self countInstall.	installs _ installedPackages at: id ifAbsent: [				installedPackages at: id put: OrderedCollection new].	installs add:		(Array with: version				with: time				with: installCounter).	SmalltalkImage current logChange: '"Installed ', name, ' version ', version, '".(Smalltalk at: #SMSqueakMap ifAbsent: []) ifNotNil:[	SMSqueakMap noteInstalledPackage: ', id asString storeString, ' version: ', version storeString, ' atSeconds: ', time asString, ' number: ', installCounter asString, ']'! !!SystemDictionary methodsFor: 'class names' stamp: 'NS 1/16/2004 15:44'!forgetClass: aClass logged: aBool 	"Delete the class, aClass, from the system, but log the removal neither to the current change set nor to the changes log.	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."	aBool 		ifTrue: 			[aClass wantsChangeSetLogging 				ifTrue: [ChangeSet current noteRemovalOf: aClass].			aClass acceptsLoggingOfCompilation 				ifTrue: [SmalltalkImage current logChange: 'Smalltalk removeClassNamed: #' , aClass name]].	SystemOrganization removeElement: aClass name.	self removeFromStartUpList: aClass.	self removeFromShutDownList: aClass.	self removeKey: aClass name ifAbsent: [].	self flushClassNameCache! !!SystemDictionary methodsFor: 'sources, change log' stamp: 'NS 1/16/2004 15:37'!assureStartupStampLogged	"If there is a startup stamp not yet actually logged to disk, do it now."	self deprecated: 'Use SmalltalkImage current assureStartupStampLogged'.	SmalltalkImage current assureStartupStampLogged.	"	StartupStamp ifNil: [^ self].	(SourceFiles isNil or: [(changesFile _ SourceFiles at: 2) == nil]) ifTrue: [^ self].	changesFile isReadOnly ifTrue:[^self].	changesFile setToEnd; cr; cr.	changesFile nextChunkPut: StartupStamp asString; cr.	StartupStamp _ nil.	self forceChangesToDisk."! !!SystemDictionary methodsFor: 'sources, change log' stamp: 'NS 1/16/2004 15:38'!logChange: aStringOrText 	"Write the argument, aString, onto the changes file."		self deprecated: 'Use SmalltalkImage current logChange:'.	SmalltalkImage current logChange: aStringOrText.		"	| aString changesFile |	(SourceFiles isNil or: [(SourceFiles at: 2) == nil]) ifTrue: [^ self].	self assureStartupStampLogged.	aStringOrText isText		ifTrue: [aString _ aStringOrText string]		ifFalse: [aString _ aStringOrText].	(aString isMemberOf: String)		ifFalse: [self error: 'can''t log this change'].	(aString findFirst: [:char | char isSeparator not]) = 0		ifTrue: [^ self].	(changesFile _ SourceFiles at: 2).	changesFile isReadOnly ifTrue:[^self].	changesFile setToEnd; cr; cr.	changesFile nextChunkPut: aString.	self forceChangesToDisk."! !!SystemDictionary methodsFor: 'toDeprecate' stamp: 'NS 1/16/2004 15:43'!saveAsEmbeddedImage	"Save the current state of the system as an embedded image"	self deprecated: 'Use SmalltalkImage current saveAsEmbeddedImage'.	SmalltalkImage current saveAsEmbeddedImage.! !!SystemDictionary methodsFor: 'toDeprecate' stamp: 'NS 1/16/2004 15:40'!snapshot: save andQuit: quit embedded: embeddedFlag	"Mark the changes file and close all files. If save is true, save the current state of this Smalltalk in the image file. If quit is true, then exit to the outer shell. The latter part of this method runs when resuming a previously saved image. The resume logic checks for a document file to process when starting up."	self deprecated: 'Use SmalltalkImage current snapshot: save andQuit: quit embedded: embeddedFlag'.	SmalltalkImage current snapshot: save andQuit: quit embedded: embeddedFlag! !