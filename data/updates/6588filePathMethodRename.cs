'From Squeak3.8-Nihongo of 25 February 2005 [latest update: #2] on 24 February 2005 at 6:45:39 pm'!"Change Set:		filePathMethodRenameDate:			24 February 2005Author:			Yoshiki OhshimaRename systemPath to vmPath."!Object subclass: #FilePath	instanceVariableNames: 'squeakPathName systemPathName vmPathName converter'	classVariableNames: ''	poolDictionaries: ''	category: 'Multilingual-BaseClasses'!!FilePath methodsFor: 'file in/out' stamp: 'yo 2/24/2005 18:43'!copySystemToVm	(self class instVarNames includes: 'systemPathName') ifTrue: [		vmPathName _ self instVarNamed: 'systemPathName'.	].! !FilePath allInstancesDo: [:e | e copySystemToVm].!!AbstractString methodsFor: 'converting' stamp: 'yo 2/24/2005 18:33'!asFileName	"Answer a String made up from the receiver that is an acceptable file 	name."	| string checkedString |	string _ (FilePath pathName: self) asVmPathName.	checkedString _ FileDirectory checkName: string fixErrors: true.	^ (FilePath pathName: checkedString isEncoded: true) asSqueakPathName.! !!AbstractString methodsFor: 'converting' stamp: 'yo 2/24/2005 18:33'!asVmPathName	^ (FilePath pathName: self) asVmPathName.! !!FileDirectory methodsFor: 'testing' stamp: 'yo 2/24/2005 18:34'!exists"Answer whether the directory exists"	| result |	result _ self primLookupEntryIn: pathName asVmPathName index: 1.	^ result ~= #badDirectoryPath! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:33'!createDirectory: localFileName	"Create a directory with the given name in this directory. Fail if the name is bad or if a file or directory with that name already exists." 	self primCreateDirectory: (self fullNameFor: localFileName) asVmPathName! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:33'!deleteDirectory: localDirName	"Delete the directory with the given name in this directory. Fail if the path is bad or if a directory by that name does not exist." 	self primDeleteDirectory: (self fullNameFor: localDirName) asVmPathName.! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:34'!deleteFileNamed: localFileName ifAbsent: failBlock	"Delete the file of the given name if it exists, else evaluate failBlock.	If the first deletion attempt fails do a GC to force finalization of any lost references. ar 3/21/98 17:53"	| fullName |	fullName _ self fullNameFor: localFileName.	(StandardFileStream 		retryWithGC:[self primDeleteFileNamed: (self fullNameFor: localFileName) asVmPathName]		until:[:result| result notNil]		forFileNamed: fullName) == nil			ifTrue: [^failBlock value].! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:34'!getMacFileTypeAndCreator: fileName 	| results typeString creatorString |	"get the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. Does nothing on other platforms (where the underlying primitive is a noop)."	"FileDirectory default getMacFileNamed: 'foo'"	typeString _ ByteArray new: 4 withAll: ($? asInteger).	creatorString _ ByteArray new: 4 withAll: ($? asInteger).	[self primGetMacFileNamed: (self fullNameFor: fileName) asVmPathName		type: typeString		creator: creatorString.] ensure: 		[typeString _ typeString asString. 		creatorString _ creatorString asString].	results _ Array with: typeString convertFromSystemString with: creatorString convertFromSystemString.	^results! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:34'!rename: oldFileName toBe: newFileName	| selection oldName newName |	"Rename the file of the given name to the new name. Fail if there is no file of the old name or if there is an existing file with the new name."	"Modified for retry after GC ar 3/21/98 18:09"	oldName _ self fullNameFor: oldFileName.	newName _ self fullNameFor: newFileName.	(StandardFileStream 		retryWithGC:[self primRename: oldName asVmPathName to: newName asVmPathName]		until:[:result| result notNil]		forFileNamed: oldName) ~~ nil ifTrue:[^self].	(self fileExists: oldFileName) ifFalse:[		^self error:'Attempt to rename a non-existent file'.	].	(self fileExists: newFileName) ifTrue:[		selection _ (PopUpMenu labels:'delete old versioncancel')				startUpWithCaption: 'Trying to rename a file to be', newFileName , 'and it already exists.'.		selection = 1 ifTrue:			[self deleteFileNamed: newFileName.			^ self rename: oldFileName toBe: newFileName]].	^self error:'Failed to rename file'.! !!FileDirectory methodsFor: 'file operations' stamp: 'yo 2/24/2005 18:34'!setMacFileNamed: fileName type: typeString creator: creatorString	"Set the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. Does nothing on other platforms (where the underlying primitive is a noop)."	"FileDirectory default setMacFileNamed: 'foo' type: 'TEXT' creator: 'ttxt'" 	self primSetMacFileNamed: (self fullNameFor: fileName) asVmPathName		type: typeString convertToSystemString		creator: creatorString convertToSystemString.! !!FileDirectory methodsFor: 'private' stamp: 'yo 2/24/2005 18:34'!directoryContentsFor: fullPath	"Return a collection of directory entries for the files and directories in the directory with the given path. See primLookupEntryIn:index: for further details."	"FileDirectory default directoryContentsFor: ''"	| entries index done entryArray f |	entries _ OrderedCollection new: 200.	index _ 1.	done _ false.	f _ fullPath asVmPathName.	[done] whileFalse: [		entryArray _ self primLookupEntryIn: f index: index.		#badDirectoryPath = entryArray ifTrue: [			^(InvalidDirectoryError pathName: pathName asSqueakPathName) signal].		entryArray == nil			ifTrue: [done _ true]			ifFalse: [entries addLast: (DirectoryEntry fromArray: entryArray)].		index _ index + 1].	^ entries asArray collect: [:s | s convertFromSystemName].! !!FilePath methodsFor: 'file in/out' stamp: 'yo 2/24/2005 18:41'!convertToCurrentVersion: varDict refStream: smartRefStrm	"If we're reading in an old version with a system path instance variable, convert it to a vm path."	varDict at: 'systemPathName' ifPresent: [ :x | 		vmPathName _ x.	].	^super convertToCurrentVersion: varDict refStream: smartRefStrm.! !!FilePath methodsFor: 'conversion' stamp: 'yo 2/24/2005 18:45'!asVmPathName	^ vmPathName.! !!FilePath methodsFor: 'conversion' stamp: 'yo 2/24/2005 18:45'!coverter: aTextConverter	converter class ~= aTextConverter class ifTrue: [		converter _ aTextConverter.		vmPathName _ squeakPathName convertToWithConverter: converter	].! !!FilePath methodsFor: 'conversion' stamp: 'yo 2/24/2005 18:45'!pathName: p isEncoded: isEncoded	converter _ LanguageEnvironment defaultFileNameConverter.	isEncoded ifTrue: [		squeakPathName _ p convertFromWithConverter: converter.		vmPathName _ p.	] ifFalse: [		squeakPathName _ p isOctetString ifTrue: [p asOctetString] ifFalse: [p].		vmPathName _ squeakPathName convertToWithConverter: converter.	].! !!FilePath class methodsFor: 'as yet unclassified' stamp: 'yo 2/24/2005 18:38'!classVersion	^ 1.! !!MultiString methodsFor: 'converting' stamp: 'yo 2/24/2005 18:34'!asFileName	"Answer a String made up from the receiver that is an acceptable file 	name."	| string checkedString |	string _ FileDirectory checkName: self fixErrors: true.	checkedString _ (FilePath pathName: string) asVmPathName.	^ (FilePath pathName: checkedString isEncoded: true) asSqueakPathName.! !!SmalltalkImage methodsFor: 'image, changes names' stamp: 'yo 2/24/2005 18:34'!imageName: newName	"Set the the full path name for the current image.  All further snapshots will use this."	| encoded |	encoded _ (FilePath pathName: newName isEncoded: false) asVmPathName.	self primImageName: encoded.! !!StandardFileStream methodsFor: 'open/close' stamp: 'yo 2/24/2005 18:34'!open: fileName forWrite: writeMode 	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."	"Changed to do a GC and retry before failing ar 3/21/98 17:25"	| f |	f _ fileName asVmPathName.	fileID _ StandardFileStream retryWithGC:[self primOpen: f writable: writeMode] 					until:[:id| id notNil] 					forFileNamed: fileName.	fileID ifNil: [^ nil].  "allows sender to detect failure"	self register.	name _ fileName.	rwmode _ writeMode.	buffer1 _ String new: 1.! !!ZipArchiveMember methodsFor: 'accessing' stamp: 'yo 2/24/2005 18:34'!centralDirectoryHeaderSize	| systemFileName systemFileComment systemCdExtraField |	systemFileName _ fileName asVmPathName.	systemFileComment _ fileComment convertToSystemString.	systemCdExtraField _ cdExtraField.	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size! !!ZipArchiveMember methodsFor: 'private-writing' stamp: 'yo 2/24/2005 18:34'!refreshLocalFileHeaderTo: aStream	"Re-writes my local header to the given stream.	To be called after writing the data stream.	Assumes that fileName and localExtraField sizes didn't change since last written."	| here systemFileName |	here _ aStream position.	systemFileName _ fileName asVmPathName.	aStream position: writeLocalHeaderRelativeOffset.	aStream nextPutAll: LocalFileHeaderSignature.	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.	aStream nextLittleEndianNumber: 2 put: bitFlag.	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.	aStream nextLittleEndianNumber: 4 put: crc32.	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).	aStream nextLittleEndianNumber: 4 put: uncompressedSize.	aStream nextLittleEndianNumber: 2 put: systemFileName size.	aStream nextLittleEndianNumber: 2 put: localExtraField size.	aStream position: here.! !!ZipArchiveMember methodsFor: 'private-writing' stamp: 'yo 2/24/2005 18:34'!writeCentralDirectoryFileHeaderTo: aStream	"C2 v3 V4 v5 V2"	| systemFileName systemFileComment systemCdExtraField |	systemFileName _ fileName asVmPathName.	systemFileComment _ fileComment convertToSystemString.	systemCdExtraField _ cdExtraField.	aStream nextPutAll: CentralDirectoryFileHeaderSignature.	aStream nextLittleEndianNumber: 1 put: versionMadeBy.	aStream nextLittleEndianNumber: 1 put: fileAttributeFormat.	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.	aStream nextLittleEndianNumber: 2 put: bitFlag.	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.	"These next 3 should have been updated during the write of the data"	aStream nextLittleEndianNumber: 4 put: crc32.	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).	aStream nextLittleEndianNumber: 4 put: uncompressedSize.	aStream nextLittleEndianNumber: 2 put: systemFileName size.	aStream nextLittleEndianNumber: 2 put: systemCdExtraField size.	aStream nextLittleEndianNumber: 2 put: systemFileComment size.	aStream nextLittleEndianNumber: 2 put: 0.		"diskNumberStart"	aStream nextLittleEndianNumber: 2 put: internalFileAttributes.	aStream nextLittleEndianNumber: 4 put: externalFileAttributes.	aStream nextLittleEndianNumber: 4 put: writeLocalHeaderRelativeOffset.	aStream nextPutAll: systemFileName asByteArray.	aStream nextPutAll: systemCdExtraField asByteArray.	aStream nextPutAll: systemFileComment asByteArray.! !!ZipArchiveMember methodsFor: 'private-writing' stamp: 'yo 2/24/2005 18:34'!writeLocalFileHeaderTo: aStream	"Write my local header to a file handle.	Stores the offset to the start of the header in my	writeLocalHeaderRelativeOffset member."	| systemFileName |	systemFileName _ fileName asVmPathName.	aStream nextPutAll: LocalFileHeaderSignature.	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.	aStream nextLittleEndianNumber: 2 put: bitFlag.	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.	aStream nextLittleEndianNumber: 4 put: crc32.	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).	aStream nextLittleEndianNumber: 4 put: uncompressedSize.	aStream nextLittleEndianNumber: 2 put: systemFileName size.	aStream nextLittleEndianNumber: 2 put: localExtraField size.	aStream nextPutAll: systemFileName asByteArray.	aStream nextPutAll: localExtraField asByteArray.! !FilePath removeSelector: #asSystemPathName!Object subclass: #FilePath	instanceVariableNames: 'squeakPathName vmPathName converter'	classVariableNames: ''	poolDictionaries: ''	category: 'Multilingual-BaseClasses'!!FilePath reorganize!('file in/out' convertToCurrentVersion:refStream: copySystemToVm)('conversion' asSqueakPathName asVmPathName coverter: pathName pathName:isEncoded: printOn:)!AbstractString removeSelector: #asSystemPathName!