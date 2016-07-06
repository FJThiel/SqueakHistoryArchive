'From Squeak2.6 of 11 October 1999 [latest update: #1722] on 17 December 1999 at 11:24:45 am'!!ImageSegment commentStamp: 'tk 12/16/1999 23:05' prior: 0!I represent a segment of Squeak address space.  I am created from an array of root objects.  After storing, my segment contains a binary encoding of every object accessible from my roots but not otherwise accessible from anywhere else in the system.  My segment contains outward pointers that are indices into my table of outPointers.	When a segment is written out onto a file, it goes in a folder called <image name>_segs.  If your image is called "Squeak2.6.image", the folder "Squeak2.6_segs" must accompany the image whenever your move, copy, or rename it.	Whenever a Class is in arrayOfRoots, its class (aClass class) must also be in the arrayOfRoots.	There are two kinds of image segments.  Normal image segments are a piece of a specific Squeak image, and can only be read back into that image.  The image holds the array of outPointers that are necessary to turn the bits in the file into objects.	To put out a normal segment that holds a Project (not the current project), execute (Project named: 'xxx') storeSegment.	The second kind of image segment is an Export Segment.  It can be read into a different Squeak image.  To create one:(ImageSegment new copyFromRootsForExport: (Array with: Baz with: Baz class))		writeForExport: 'myFile.extSeg'.To read into another image:  Select 'myFile.extSeg' in a FileList, Menu more..., fileIn.  It will install its classes automatically.  If you need to see the roots array, it is temporarily stored in (SmartRefStream scannedObject).arrayOfRoots	The objects that head the tree we will trace.segment			The WordArray of raw bits of all objects in the tree.outPointers		Oops of all objects outside the segment pointed to from inside.state			(see below)segmentName	Its basic name.  Often the name of a Project.fileName		The local name of the file.  'Foo-23.seg'endMarker		An object located in memory somewhere after a segment that has just been brought in.  To enumerate the objects in the segment, start at the segment and go to this object.An ImageSegment may exist in several states...#activeCopyarrayOfRoots, segment, and outPointers are all as they were created by copyFromRoots:.  The segment has been created in memory, but nothing else has changed about the Squeak system.#activeEach of the original roots has been transmuted into an ImageSegmentRootStub that refers back to this image segment.  The original objects in the segment will all be garbageCollected.#onFileThe segment has been written out to a file and replaced by a file pointer.  Only the array of outPointers remains in the image.  To get this far:(ImageSegment new copyFromRoots: (Array with: Baz with: Baz class))		writeToFile: 'myFile.seg'.#inactiveIn this state, the rootsArray is set, but the segment is invalid.#onFileWithSymbolsThe segment has been written out to a file, along with the text of all the symbols in the outPointers array, and replaced by a file pointer.  This reduces the size of the outPointers array, and also allows the system to reclaim any symbols that are not referred to from elsewhere in the image.  The specific format used is that of a literal array as follows:	#(symbol1 symbol2 # symbol3 symbol4 'symbolWithSpaces' # symbol5).In this case, the original outPointers array was 8 long, but the compacted table of outPointers retains only two entries.  These get inserted in place of the #'s in the array of symbols after it is read back in.  Symbols with embedded spaces or other strange characters are written as strings, and converted back to symbols when read back in.  The symbol # is never written out.	NOTE: All IdentitySets or dictionaries must be rehashed when being read back from this format.  The symbols are effectively internal.  (No, not if read back into same image.  If a different image, then use #imported.  -tk)#importedThe segment is on an external file or just read in from one.  The segment and outPointers are meant to be read into a foreign image.   (It is in SmartRefStream format.)  In this form the segment can be read from a URL, and installed.  A copy of the original array of root objects is constructed, with former outPointers bound to existing objects in the host system.  	(Any Class inside the segment MUST be in the arrayOfRoots.  This is so its association can be inserted into Smalltalk.  The class's metaclass must be in roots also.  Methods that are in outPointers because blocks point at them, are found and added to the roots.)	All IdentitySets and dictionaries are rehashed when being read back from exported segments.) To discover why only som eof the objects in a project are being written out, try this (***Destructive Test***).  This breaks lots of backpointers in the target project, and puts up an array of suspicious objects, a list of the classes of the outPointers, and a debugger."Close any transcripts in the target project"World currentHand objectToPaste ifNotNil: [	self inform: 'Hand is holding a Morph in its paste buffer:\' withCRs,		World currentHand objectToPaste printString].PV _ Project named: 'xxxx'.(IS _ ImageSegment new) findRogueRootsImSeg: 	(Array with: PV world presenter with: PV world).IS findOwnersOutPtrs.	"Optionally: write a file with owner chains""Quit and DO NOT save"!!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/17/1999 11:19'!copyFromRoots: aRootArray sizeHint: segSizeHint	"Copy a tree of objects into a WordArray segment.  The copied objects in the segment are not in the normal Squeak space.  If this method yields a very small segment, it is because objects just below the roots are pointed at from the outside.  (See findRogueRootsImSeg: for a *destructive* diagnostic of who is pointing in.)"	| segmentWordArray outPointerArray segSize rootSet |	aRootArray ifNil: [self errorWrongState].	rootSet _ IdentitySet new: 150.	rootSet addAll: aRootArray.	arrayOfRoots _ rootSet asArray.	rootSet _ nil.	"be clean"	outPointers _ nil.	"may have used this instance before"	segSize _ segSizeHint > 0 ifTrue: [segSizeHint *3 //2] ifFalse: [50000].	["Guess a reasonable segment size"	segmentWordArray _ WordArray new: segSize.	endMarker _ 'End' clone.	"for enumeration of objects"	[outPointerArray _ Array new: segSize // 20] ifError: [		state _ #tooBig.  ^ self].	(self storeSegmentFor: arrayOfRoots					into: segmentWordArray					outPointers: outPointerArray) == nil]		whileTrue:			["Double the segment size and try again"			segmentWordArray _ outPointerArray _ nil.			segSize _ segSize * 2].	segment _ segmentWordArray.	outPointers _ outPointerArray.	state _ #activeCopy.! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/17/1999 00:03'!localName	| segs ind sep |	"Return the current file name for this segment, a local name in the segments directory."	fileName ifNil: [^ nil].	"^ fileName"		"The following is for backward compatibility.  Remove this part after June 2000.	Check if the fileName is a full path, and make it local.  Regardless of current or previous file system delimiter."	segs _ self class folder copyLast: 4.  "_segs"	ind _ 1.	[ind _ fileName findString: segs startingAt: ind+1 caseSensitive: false.		ind = 0 ifTrue: [^ fileName].		sep _ fileName at: ind + (segs size).		sep isAlphaNumeric ] whileTrue.		"sep is letter or digit, not a separator"	^ fileName _ fileName copyFrom: ind+(segs size)+1 "delimiter" to: fileName size! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/16/1999 22:30'!readFromFile	"Read in a simple segment.  Use folder of this image, even if remembered as previous location of this image"	| ff realName |	realName _ self class folder, FileDirectory slash, self localName.	ff _ FileStream oldFileNamed: realName.	segment _ ff nextInto: (WordArray new: ff size//4).	ff close.	state _ #active! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/16/1999 22:32'!writeForExport: shortName	"Write the segment on the disk with all info needed to reconstruct it in a new image.  For export.  Out pointers are encoded as normal objects on the disk."	| fileStream temp fName |	state = #activeCopy ifFalse: [self error: 'wrong state'].	temp _ endMarker.	endMarker _ nil.	(shortName endsWith: '.extSeg')		ifTrue: [fName _ shortName]		ifFalse: [fName _ shortName , '.extSeg'].	fileStream _ FileStream newFileNamed: fName.	fileStream fileOutClass: nil andObject: self.		"remember extra structures.  Note class names."	endMarker _ temp.! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/17/1999 00:13'!writeToFile	state = #active ifFalse: [self error: 'wrong state'. ^ self].	Cursor write showWhile: [		segmentName ifNil: [			segmentName _ (FileDirectory localNameFor: fileName) sansPeriodSuffix].			"OK that still has number on end.  This is an unusual case"		fileName _ self class uniqueFileNameFor: segmentName.	"local name"		(self class segmentDirectory newFileNamed: fileName) nextPutAll: segment; close.		segment _ nil.		endMarker _ nil.		state _ #onFile].! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/17/1999 00:14'!writeToFileWithSymbols	| symbols nonSymbols pound |	state = #extracted ifFalse: [self error: 'wrong state'].	segmentName ifNil: [		segmentName _ (FileDirectory localNameFor: fileName) sansPeriodSuffix].		"OK that still has number on end.  This is an unusual case"	fileName _ self class uniqueFileNameFor: segmentName.	symbols _ OrderedCollection new.	nonSymbols _ OrderedCollection new.	pound _ '#' asSymbol.	outPointers do:		[:s | 		((s isMemberOf: Symbol) and: [s isLiteral and: [s ~~ pound]])			ifTrue: [symbols addLast: s]			ifFalse: [symbols addLast: pound.  nonSymbols addLast: s]].	(self class segmentDirectory newFileNamed: fileName)		store: symbols asArray; cr;		nextPutAll: segment; close.	outPointers _ nonSymbols asArray.	state _ #onFileWithSymbols! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 12/16/1999 23:44'!reclaimObsoleteSegmentFiles  "ImageSegment reclaimObsoleteSegmentFiles"	"Delete segment files that can't be used after this image is saved.	Note that this is never necessary -- it just saves file space."	| aFileName segDir segFiles folderName byName exists |	folderName _ FileDirectory default class localNameFor: self folder.	(FileDirectory default includesKey: folderName) ifFalse: [		^ self "don't create if absent"].	segDir _ self segmentDirectory.	segFiles _ (segDir fileNames select: [:fn | fn endsWith: '.seg']) asSet.	exists _ segFiles copy.	segFiles isEmpty ifTrue: [^ self].	byName _ Set new.	"Remove (save) every file owned by a segment in memory"	ImageSegment allInstancesDo: [:is | 		(aFileName _ is localName) ifNotNil: [			segFiles remove: aFileName ifAbsent: [].			(exists includes: aFileName) ifFalse: [				Transcript cr; show: 'Segment file not found: ', aFileName].			byName add: is segmentName]].	"Of the segments we have seen, delete unclaimed the files."	segFiles do: [:fName | 		"Delete other file versions with same project name as one known to us"		(byName includes: (fName sansPeriodSuffix stemAndNumericSuffix first))			ifTrue: [segDir deleteFileNamed: fName]].! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 12/16/1999 22:33'!uniqueFileNameFor: segName	"Choose a unique file name for the segment with this name."	| segDir fileName listOfFiles |	segDir _ self segmentDirectory.	listOfFiles _ segDir fileNames.	BiggestFileNumber ifNil: [BiggestFileNumber _ 1].	BiggestFileNumber > 99 ifTrue: [BiggestFileNumber _ 1].	"wrap"	[fileName _ segName, BiggestFileNumber printString, '.seg'.	 (listOfFiles includes: fileName)] whileTrue: [		BiggestFileNumber _ BiggestFileNumber + 1].	"force a unique file name"	^ fileName! !!Project methodsFor: 'file in/out' stamp: 'tk 12/17/1999 11:19'!exportSegment	"Store my project out on the disk as an *exported* ImageSegment.  All outPointers in a form that can be resolved in the target image.  Name it <project name>.extSeg.  What do we do about subProjects, especially if they are out as local image segments?  Force them to come in?"| is response str ans revertSeg |world == World ifTrue: [^ false]. 	"self inform: 'Can''t send the current world out'."world isMorph ifFalse: [	self projectParameters at: #isMVC put: true.	^ false].	"Only Morphic projects for now"world ifNil: [^ false].  world presenter ifNil: [^ false].Utilities emptyScrapsBook.World currentHand objectToPaste ifNotNil: [	response _ (PopUpMenu labels: 'Delete\Keep' withCRs)		startUpWithCaption: 'Hand is holding a Morph in its paste buffer:\' withCRs,			World currentHand objectToPaste printString.	response = 1 ifTrue: [World currentHand clearPasteBuffer]]."Just export me, not my previous version"revertSeg _ self projectParameters at: #revertToMe ifAbsent: [nil].self projectParameters removeKey: #revertToMe ifAbsent: [].	is _ ImageSegment new copyFromRootsForExport: 	(Array with: self).	"world, and all Players"is state = #tooBig ifTrue: [^ false].str _ ''.is segment size < 3000 ifTrue: [	str _ 'Segment is only ', is segment size printString, ' long.'].(is outPointers detect: [:out | out isMorph] ifNone: [nil]) ifNotNil: [	str _ str, '\Morphs are pointed at from the outside.' withCRs].(is outPointers includes: world) ifTrue: [	str _ str, '\Project''s own world is not in the segment.' withCRs].str isEmpty ifFalse: [	ans _ (PopUpMenu labels: 'Do not write fileWrite file anyway') startUpWithCaption: str.	ans = 1 ifTrue: [		revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].		^ false]].is writeForExport: self name.revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].^ true! !!Project methodsFor: 'file in/out' stamp: 'tk 12/17/1999 11:06'!storeSegment	"Store my project out on the disk as an ImageSegment.  Keep the outPointers in memory.  Name it <project name>.seg.  *** Caller must be holding (Project alInstances) to keep subprojects from going out. ***"| is response sizeHint |world == World ifTrue: [^ false]. 	"self inform: 'Can''t send the current world out'."world isInMemory ifFalse: [^ false].  "already done"world isMorph ifFalse: [	self projectParameters at: #isMVC put: true.	^ false].	"Only Morphic projects for now"world ifNil: [^ false].  world presenter ifNil: [^ false].Utilities emptyScrapsBook.World currentHand objectToPaste ifNotNil: [	response _ (PopUpMenu labels: 'Delete\Keep' withCRs)		startUpWithCaption: 'Hand is holding a Morph in its paste buffer:\' withCRs,			World currentHand objectToPaste printString.	response = 1 ifTrue: [World currentHand clearPasteBuffer]].sizeHint _ self projectParameters at: #segmentSize ifAbsent: [0].is _ ImageSegment new copyFromRootsLocalFileFor: 			(Array with: world presenter with: world)	"world, and all Players"		 sizeHint: sizeHint.is state = #tooBig ifTrue: [^ false].is segment size < 2000 ifTrue: ["debugging" 	Transcript show: self name, ' not enough objects for a Segment.'; cr.	^ false].self projectParameters at: #segmentSize put: is segment size.is extract; writeToFile: self name.^ true! !!SequenceableCollection methodsFor: 'copying' stamp: 'tk 12/16/1999 22:05'!copyLast: num	"Return the last num elements of the collection"	^ self copyFrom: (self size - num + 1) to: self size! !ImageSegment removeSelector: #makeFileNameLocal!ImageSegment removeSelector: #saveRevertionInfo!ImageSegment class removeSelector: #reclaimObsoleteSegmentFiles2!Project removeSelector: #possiblyRevert!