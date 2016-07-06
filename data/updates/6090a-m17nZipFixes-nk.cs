'From Squeakland 3.8.5976 of 19 August 2004 [latest update: #243] on 21 August 2004 at 4:16:25 pm'!!ArchiveViewer class methodsFor: 'fileIn/Out' stamp: 'nk 8/21/2004 16:01'!fileReaderServicesForFile: fullName suffix: suffix 	|  services |	services _ OrderedCollection new.	services add: self serviceAddToNewZip.	({'zip'.'sar'.'pr'. 'mcz'. '*'} includes: suffix)		ifTrue: [services add: self serviceOpenInZipViewer.				services add: self serviceExtractAll].	^ services! !!ByteArray methodsFor: 'zip archive' stamp: 'nk 8/21/2004 15:23'!lastIndexOfPKSignature: aSignature	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"	| a b c d |	a _ aSignature first.	b _ aSignature second.	c _ aSignature third.	d _ aSignature fourth.	(self size - 3) to: 1 by: -1 do: [ :i |		(((self at: i) = a)			and: [ ((self at: i + 1) = b)				and: [ ((self at: i + 2) = c)					and: [ ((self at: i + 3) = d) ]]])						ifTrue: [ ^i ]	].	^0! !!ReadWriteStream methodsFor: 'testing' stamp: 'nk 8/21/2004 15:47'!isZipArchive	"Determine if this appears to be a valid Zip archive"	| sig |	self binary.	sig _ self next: 4.	self position: self position - 4. "rewind"	^ZipArchive validSignatures includes: sig! !!ZipArchive methodsFor: 'private' stamp: 'nk 8/21/2004 15:22'!readSignatureFrom: stream	"Returns next signature from given stream, leaves stream positioned afterwards."	| signatureData | 	signatureData _ ByteArray new: 4.	stream next: 4 into: signatureData.	({ CentralDirectoryFileHeaderSignature . LocalFileHeaderSignature . EndOfCentralDirectorySignature }		includes: signatureData)			ifFalse: [ ^self error: 'bad signature ', signatureData asString asHex, ' at position ', (stream position - 4) asString ].	^signatureData! !!ZipArchive class methodsFor: 'constants' stamp: 'nk 8/21/2004 15:19'!findEndOfCentralDirectoryFrom: stream	"Seek in the given stream to the end, then read backwards until we find the	signature of the central directory record. Leave the file positioned right	before the signature.	Answers the file position of the EOCD, or 0 if not found."	| data fileLength seekOffset pos maxOffset |	stream setToEnd.	fileLength _ stream position.	"If the file length is less than 18 for the EOCD length plus 4 for the signature, we have a problem"	fileLength < 22 ifTrue: [^ self error: 'file is too short'].		seekOffset _ 0.	pos _ 0.	data _ ByteArray new: 4100.	maxOffset _ 40960 min: fileLength.	"limit search range to 40K"	[		seekOffset _ (seekOffset + 4096) min: fileLength.		stream position: fileLength - seekOffset.		data _ stream next: (4100 min: seekOffset) into: data startingAt: 1.		pos _ data lastIndexOfPKSignature: EndOfCentralDirectorySignature.		pos = 0 and: [seekOffset < maxOffset]	] whileTrue.	^ pos > 0		ifTrue: [ | newPos | stream position: (newPos _ (stream position + pos - seekOffset - 1)). newPos]		ifFalse: [0]! !!ZipFileConstants class methodsFor: 'pool initialization' stamp: 'nk 8/21/2004 15:50'!initialize	"ZipFileConstants initialize"	FaMsdos		:= 0.	FaUnix 		:= 3.	DeflatingCompressionNormal		:= 0.	DeflatingCompressionMaximum	:= 2.	DeflatingCompressionFast		:= 4.	DeflatingCompressionSuperFast	:= 6.	CompressionStored				:= 0.	CompressionDeflated				:= 8.	CompressionLevelNone			:= 0.	CompressionLevelDefault			:= 6.	IfaTextFile						:= 1.	IfaBinaryFile					:= 0.	DataDescriptorLength 				:= 12.	"Unix permission bits"	DefaultDirectoryPermissions		:= 8r040755.	DefaultFilePermissions			:= 8r0100666.	DirectoryAttrib 					:= 8r040000.	FileAttrib 						:= 8r0100000.	CentralDirectoryFileHeaderSignature _ 		(ByteArray with: 16r50 with: 16r4B with: 16r01 with: 16r02).	LocalFileHeaderSignature _		(ByteArray with: 16r50 with: 16r4B with: 16r03 with: 16r04).	EndOfCentralDirectorySignature _		(ByteArray with: 16r50 with: 16r4B with: 16r05 with: 16r06).! !ZipFileConstants initialize!