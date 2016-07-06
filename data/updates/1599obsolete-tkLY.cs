'From Squeak2.6 of 11 October 1999 [latest update: #1596] on 9 November 1999 at 9:54:57 am'!"Change Set:		obsolete-tkLYDate:			9 November 1999Author:			Ted KaehlerPuts up an error if the user is trying to export an object or export image segment that has an instance of an obsolete class in it.Further refinement to deleting old image segments on the disk."!!DiskProxy methodsFor: 'as yet unclassified' stamp: 'tk 11/4/1999 19:28'!global: globalNameSymbol selector: selectorSymbol args: argArray	"Initialize self as a DiskProxy constructor with the given	globalNameSymbol, selectorSymbol, and argument Array.	I will internalize by looking up the global object name in the	SystemDictionary (Smalltalk) and sending it this message with	these arguments."	(globalNameSymbol beginsWith: 'AnObsolete') ifTrue: [		self error: 'Trying to write out, ', globalNameSymbol].	globalObjectName _ globalNameSymbol asSymbol.	constructorSelector _ selectorSymbol asSymbol.	constructorArgs _ argArray.! !!ImageSegment class methodsFor: 'fileIn/Out' stamp: 'tk 11/7/1999 17:31'!reclaimObsoleteSegmentFiles  "ImageSegment reclaimObsoleteSegmentFiles"	"Delete segment files that can't be used after this image is saved.	Note that this is never necessary -- it just saves file space."	| segDir segFiles folderName byName exists |	folderName _ FileDirectory default class localNameFor: self folder.	(FileDirectory default includesKey: folderName) ifFalse: [^ self "don't create if absent"].	segDir _ self segmentDirectory.	segFiles _ (segDir fileNames select: [:fn | fn endsWith: '.seg']) asSet.	exists _ segFiles copy.	segFiles isEmpty ifTrue: [^ self].	byName _ Set new.	"Remove (save) every file owned by a segment in memory"	ImageSegment allInstancesDo: [:is | 		is localName ifNotNil: [			segFiles remove: is localName ifAbsent: [].			(exists includes: is localName) ifFalse: [				Transcript cr; show: 'Segment file not found: ', is localName].			byName add: is segmentName]].	"Of the segments we have seen, delete unclaimed the files."	segFiles do: [:fName | 		"Delete other file versions with same project name as one known to us"		(byName includes: (fName sansPeriodSuffix stemAndNumericSuffix first))			ifTrue: [segDir deleteFileNamed: fName]].! !!SmartRefStream methodsFor: 'read write' stamp: 'tk 11/5/1999 09:45'!instVarInfo: anObject	"Return the object to write on the outgoing file that contains the structure of each class we are about to write out.  Must be an Array whose first element is 'class structure'.  Its second element is a Dictionary of pairs of the form #Rectangle -> #(<classVersion> 'origin' 'corner').  "	"Make a pass through the objects, not writing, but recording the classes.  Construct a database of their inst vars and any version info (classVersion)."	| dummy refs cls newSupers |	structures _ Dictionary new.	superclasses _ Dictionary new.	dummy _ ReferenceStream on: (DummyStream on: nil).		"Write to a fake Stream, not a file"	"Collect all objects"	dummy rootObject: anObject.	"inform him about the root"	dummy nextPut: anObject.	refs _ dummy references.	self uniClassInstVarsRefs: dummy.	"catalog the extra objects in UniClass inst vars"	objCount _ refs size.		"for progress bar"		"Note that Dictionary must not change its implementation!!  If it does, how do we read this reading information?"	(refs includesKey: #AnImageSegment) 		ifFalse: [			refs keysDo: [:each | 				cls _ each class.				cls isObsolete ifTrue: [self error: 'Trying to write ', cls name].				cls class == Metaclass ifFalse: [					structures at: cls name put: false]]]		ifTrue: [self recordImageSegment: refs].	"Save work by only computing inst vars once for each class"	newSupers _ Set new.	structures keysDo: [:nm | 		cls _ Smalltalk at: nm.		cls allSuperclasses do: [:aSuper |			structures at: aSuper name ifAbsent: [newSupers add: aSuper name]]].			"Don't modify structures during iteration"	newSupers do: [:nm | structures at: nm put: 3].	"Get all superclasses into list"	structures keysDo: [:nm | "Nothing added to classes during loop"		cls _ Smalltalk at: nm.		structures at: nm put: 			((Array with: cls classVersion), (cls allInstVarNames)).		superclasses at: nm ifAbsent: [				superclasses at: nm put: cls superclass name]].	self saveClassInstVars.	"of UniClassses"	^ (Array with: 'class structure' with: structures with: 'superclasses' with: superclasses)! !!SmartRefStream methodsFor: 'read write' stamp: 'tk 11/5/1999 09:45'!recordImageSegment: refs	"Besides the objects being written out, record the structure of instances inside the image segment we are writing out."	| cls |	refs keysDo: [:each | 		cls _ each class.		cls isObsolete ifTrue: [self error: 'Trying to write ', cls name].		cls class == Metaclass ifFalse: [			structures at: cls name put: false.			(each isKindOf: ImageSegment) ifTrue: [				each outPointers do: [:out |					(out isKindOf: Class) ifTrue: [						structures at: out theNonMetaClass name put: false]]].			(each isKindOf: Association) ifTrue: [].			]]! !