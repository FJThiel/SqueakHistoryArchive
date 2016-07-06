'From Squeak3.1alpha of 4 February 2001 [latest update: #3691] on 22 February 2001 at 10:46:12 am'!"Change Set:		SpaceLog-arDate:			21 February 2001Author:			Andreas Raab(Republished 16 Mar because it was left out of the 3.1 update stream.)Write a log about space usage when exporting a project to find out more about where the memory hogs are."Preferences addPreference: #debugPrintSpaceLog category: #debug default: false balloonHelp:'When true, write a log about space usage in image segments during publishing'.!!ImageSegment methodsFor: 'read/write segment' stamp: 'ar 2/22/2001 10:45'!writeForExportWithSources: fName inDirectory: aDirectory changeSet: aChangeSetOrNil	"Write the segment on the disk with all info needed to reconstruct it in a new image.  For export.  Out pointers are encoded as normal objects on the disk.  Append the source code of any classes in roots.  Target system will quickly transfer the sources to its changes file."	"An experimental version to fileout a changeSet first so that a project can contain its own classes"	| fileStream temp tempFileName zipper allClassesInRoots classesToWriteEntirely methodsWithSource |	state = #activeCopy ifFalse: [self error: 'wrong state'].	(fName includes: $.) ifFalse: [		^ self inform: 'Please use ''.pr'' or ''.extSeg'' at the end of the file name'.].	temp _ endMarker.	endMarker _ nil.	tempFileName _ aDirectory nextNameFor: 'SqProject' extension: 'temp'.	zipper _ [		Preferences debugPrintSpaceLog ifTrue:[			fileStream _ aDirectory newFileNamed: 				(fName copyFrom: 1 to: (fName lastIndexOf: $.)), 'space'.			self printSpaceAnalysisOn: fileStream.			fileStream close].		ProgressNotification signal: '3:uncompressedSaveComplete'.		(aDirectory oldFileNamed: tempFileName) compressFile.	"makes xxx.gz"		aDirectory 			rename: (tempFileName, FileDirectory dot, 'gz')			toBe: fName.		aDirectory			deleteFileNamed: tempFileName			ifAbsent: []	].	fileStream _ aDirectory newFileNamed: tempFileName.	fileStream fileOutChangeSet: aChangeSetOrNil andObject: self.		"remember extra structures.  Note class names."	endMarker _ temp.	"append sources"	allClassesInRoots _ arrayOfRoots select: [:cls | cls isKindOf: Behavior].	classesToWriteEntirely _ allClassesInRoots select: [ :cls | cls theNonMetaClass isSystemDefined].	methodsWithSource _ OrderedCollection new.	allClassesInRoots do: [ :cls |		(classesToWriteEntirely includes: cls) ifFalse: [			cls selectorsAndMethodsDo: [ :sel :meth |				meth sourcePointer = 0 ifFalse: [methodsWithSource add: {cls. sel. meth}].			].		].	].	(classesToWriteEntirely isEmpty and: [methodsWithSource isEmpty]) ifTrue: [zipper value. ^ self].	fileStream reopen; setToEnd.	fileStream nextPutAll: '\\!!ImageSegment new!!\\' withCRs.	methodsWithSource do: [ :each |		fileStream nextPut: $!!.	"try to pacify ImageSegment>>scanFrom:"		fileStream nextChunkPut: 'RenamedClassSourceReader formerClassName: ',				each first name printString,' methodsFor: ',				(each first organization categoryOfElement: each second) asString printString,				' stamp: ',(Utilities timeStampForMethod: each third) printString; cr.		fileStream nextChunkPut: (each third getSourceFor: each second in: each first) asString.		fileStream nextChunkPut: ' '; cr.	].	classesToWriteEntirely do: [:cls | 		cls isMeta ifFalse: [fileStream nextPutAll: 						(cls name, ' category: ''', cls category, '''.!!'); cr; cr].		cls organization			putCommentOnFile: fileStream			numbered: 0			moveSource: false			forClass: cls.	"does nothing if metaclass"		cls organization categories do: 			[:heading |			cls fileOutCategory: heading				on: fileStream				moveSource: false				toFile: 0]].	"no class initialization -- it came in as a real object"	fileStream close.	zipper value.! !!ImageSegment methodsFor: 'compact classes' stamp: 'ar 2/21/2001 19:26'!compactClassesArray	| ccIndexes ind ccArray hdrBits |	"A copy of the real compactClassesArray, but with only the classes actually used in the segment.  Slow, but OK for export."	ccIndexes _ Set new.	ind _ 2. 	"skip version word, first object"	"go past extra header words"	(hdrBits _ (segment atPin: ind) bitAnd: 3) = 1 ifTrue: [ind _ ind+1].	hdrBits = 0 ifTrue: [ind _ ind+2].	[ccIndexes add: (self compactIndexAt: ind).	"0 if has class field"	 ind _ self objectAfter: ind.	 ind > segment size] whileFalse.	ccArray _ Smalltalk compactClassesArray clone.	1 to: ccArray size do: [:ii | "only the ones we use"		(ccIndexes includes: ii) ifFalse: [ccArray at: ii put: nil]].	^ ccArray! !!ImageSegment methodsFor: 'statistics' stamp: 'ar 2/21/2001 18:44'!classNameAt: index	| ccIndex |	ccIndex _ self compactIndexAt: index.	ccIndex = 0 ifFalse:[^(Smalltalk compactClassesArray at: ccIndex) name].	ccIndex _ segment at: index-1.	(ccIndex bitAnd: 16r80000000) = 0 ifTrue:[		"within segment; likely a user object"		^#UserObject].	ccIndex _ (ccIndex bitAnd: 16r7FFFFFFF) bitShift: -2.	^(outPointers at: ccIndex) name! !!ImageSegment methodsFor: 'statistics' stamp: 'ar 2/21/2001 19:19'!doSpaceAnalysis	"Capture statistics about the IS and print the number of instances per class and space usage"	| index sz word hdrBits cc instCount instSpace |	state == #activeCopy ifFalse:[self errorWrongState].	instCount _ IdentityDictionary new.	instSpace _ IdentityDictionary new.	index _ 2. 	"skip version word, first object"	"go past extra header words"	hdrBits _ (segment at: index) bitAnd: 3.	hdrBits = 1 ifTrue: [index _ index+1].	hdrBits = 0 ifTrue: [index _ index+2].	[index > segment size] whileFalse:[		hdrBits _ (word _ segment at: index) bitAnd: 3.		hdrBits = 2 ifTrue:[sz _ word bitAnd: 16rFFFFFFFC].		hdrBits = 0 ifTrue:[sz _ ((segment at: index-2) bitAnd: 16rFFFFFFFC) + 8].		hdrBits = 1 ifTrue:[sz _ (word bitAnd: "SizeMask" 252) + 4].		hdrBits = 3 ifTrue:[sz _ word bitAnd: "SizeMask" 252].		hdrBits = 2 			ifTrue:[cc _ #freeChunk]			ifFalse:[cc _ self classNameAt: index].		instCount at: cc put: (instCount at: cc ifAbsent:[0]) + 1.		instSpace at: cc put: (instSpace at: cc ifAbsent:[0]) + sz.		index _ self objectAfter: index].	^{instCount. instSpace}! !!ImageSegment methodsFor: 'statistics' stamp: 'ar 2/21/2001 19:22'!printSpaceAnalysisOn: aStream	"Capture statistics about the IS and print the number of instances per class and space usage"	| instCount instSpace sorted sum1 sum2 |	instCount _ self doSpaceAnalysis.	instSpace _ instCount last.	instCount _ instCount first.	sorted _ SortedCollection sortBlock:[:a1 :a2| a1 value >= a2 value].	instSpace associationsDo:[:a| sorted add: a].	sorted do:[:assoc|		aStream cr; nextPutAll: assoc key; tab.		aStream print: (instCount at: assoc key); nextPutAll:' instances '.		aStream print: assoc value; nextPutAll: ' bytes '.	].	sum1 _ instCount inject: 0 into:[:sum :n| sum + n].	sum2 _ instSpace inject: 0 into:[:sum :n| sum + n].	aStream cr; cr.	aStream print: sum1; nextPutAll:' instances '.	aStream print: sum2; nextPutAll: ' bytes '.! !