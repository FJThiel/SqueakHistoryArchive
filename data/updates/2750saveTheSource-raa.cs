'From Squeak2.9alpha of 17 July 2000 [latest update: #2778] on 30 September 2000 at 8:55:03 pm'!"Change Set:		saveTheSourceDate:			30 September 2000Author:			Bob Arning- restore code that was lost for a while to preserve the source for CompiledMethods exported in an ImageSegment where the sourcePointer had been set. Otherwise, the receiving system may get real confused as to the source for that method."!!ImageSegment methodsFor: 'read/write segment' stamp: 'RAA 9/30/2000 20:53'!writeForExportWithSources: fName inDirectory: aDirectory	"Write the segment on the disk with all info needed to reconstruct it in a new image.  For export.  Out pointers are encoded as normal objects on the disk.  Append the source code of any classes in roots.  Target system will quickly transfer the sources to its changes file."	"this is the old version which I restored until I solve the gzip problem"	| fileStream temp tempFileName zipper allClassesInRoots classesToWriteEntirely methodsWithSource |	state = #activeCopy ifFalse: [self error: 'wrong state'].	(fName includes: $.) ifFalse: [		^ self inform: 'Please use ''.pr'' or ''.extSeg'' at the end of the file name'.].	temp _ endMarker.	endMarker _ nil.	tempFileName _ aDirectory nextNameFor: 'SqProject' extension: 'temp'.	zipper _ [		ProgressNotification signal: '3:uncompressedSaveComplete'.		(aDirectory oldFileNamed: tempFileName) compressFile.	"makes xxx.gz"		aDirectory 			rename: (tempFileName, FileDirectory dot, 'gz')			toBe: fName.		aDirectory			deleteFileNamed: tempFileName			ifAbsent: []	].	fileStream _ aDirectory newFileNamed: tempFileName.	fileStream fileOutClass: nil andObject: self.		"remember extra structures.  Note class names."	endMarker _ temp.	"append sources"	allClassesInRoots _ arrayOfRoots select: [:cls | cls isKindOf: Behavior].	classesToWriteEntirely _ allClassesInRoots select: [ :cls | cls theNonMetaClass isSystemDefined].	methodsWithSource _ OrderedCollection new.	allClassesInRoots do: [ :cls |		(classesToWriteEntirely includes: cls) ifFalse: [			cls selectorsAndMethodsDo: [ :sel :meth |				meth sourcePointer = 0 ifFalse: [methodsWithSource add: {cls. sel. meth}].			].		].	].	(classesToWriteEntirely isEmpty and: [methodsWithSource isEmpty]) ifTrue: [zipper value. ^ self].	fileStream reopen; setToEnd.	fileStream nextPutAll: '\\!!ImageSegment new!!\\' withCRs.	methodsWithSource do: [ :each |		fileStream nextPut: $!!.	"try to pacify ImageSegment>>scanFrom:"		fileStream nextChunkPut: 'RenamedClassSourceReader formerClassName: ',				each first name printString,' methodsFor: ',				(each first organization categoryOfElement: each second) asString printString,				' stamp: ',(Utilities timeStampForMethod: each third) printString; cr.		fileStream nextChunkPut: (each third getSourceFor: each second in: each first) asString.		fileStream nextChunkPut: ' '; cr.	].	classesToWriteEntirely do: [:cls | 		cls isMeta ifFalse: [fileStream nextPutAll: 						(cls name, ' category: ''', cls category, '''.!!'); cr; cr].		cls organization			putCommentOnFile: fileStream			numbered: 0			moveSource: false			forClass: cls.	"does nothing if metaclass"		cls organization categories do: 			[:heading |			cls fileOutCategory: heading				on: fileStream				moveSource: false				toFile: 0]].	"no class initialization -- it came in as a real object"	fileStream close.	zipper value.! !