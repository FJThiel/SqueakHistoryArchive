'From Squeak3.2 of 22 February 2002 [latest update: #4952] on 22 October 2002 at 9:42:56 am'!"Change Set:		Project-dep-tkDate:			21 October 2002Author:			Ted KaehlerFor a long time, there has been a bug in the way Projects are written.  If objects in the projet used 'old fashioned dependents', the dependents link was not preserved.  Here is a fix.	Ordinarily, object that have dependents are instances of a subclass of Model.  But, class Object still provides the 'Global Dependents' mechanism, and some people still use it.  When both the model and its global dependent are in a project that is being saved, we remember them, so we can hook them up again when the project is read in."!!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 10/21/2002 15:31'!copySmartRootsExport: rootArray 	"Use SmartRefStream to find the object.  Make them all roots.  Create the segment in memory.  Project should be in first five objects in rootArray."	| newRoots list segSize symbolHolder dummy replacements naughtyBlocks goodToGo allClasses sizeHint proj |	Smalltalk forgetDoIts.	symbolHolder _ Symbol allInstances.	"Hold onto Symbols with strong pointers, 		so they will be in outPointers"	dummy _ ReferenceStream on: (DummyStream on: nil).		"Write to a fake Stream, not a file"	"Collect all objects"	dummy insideASegment: true.	"So Uniclasses will be traced"	dummy rootObject: rootArray.	"inform him about the root"	dummy nextPut: rootArray.	(proj _dummy project) ifNotNil: [self dependentsSave: dummy].	allClasses _ SmartRefStream new uniClassInstVarsRefs: dummy.		"catalog the extra objects in UniClass inst vars.  Put into dummy"	allClasses do: [:cls | 		dummy references at: cls class put: false.	"put Player5 class in roots"		dummy blockers removeKey: cls class ifAbsent: []].	"refs _ dummy references."	arrayOfRoots _ self smartFillRoots: dummy.	"guaranteed none repeat"	self savePlayerReferences: dummy references.	"for shared References table"	replacements _ dummy blockers.	dummy project "recompute it" ifNil: [self error: 'lost the project!!'].	dummy project class == DiskProxy ifTrue: [self error: 'saving the wrong project'].	dummy _ nil.	"force GC?"	naughtyBlocks _ arrayOfRoots select: [ :each |		(each isKindOf: ContextPart) and: [each hasInstVarRef]	].	"since the caller switched ActiveWorld, put the real one back temporarily"	naughtyBlocks isEmpty ifFalse: [		World becomeActiveDuring: [			goodToGo _ PopUpMenu				confirm: 'Some block(s) which reference instance variables are included in this segment. These may fail whenthe segment is loaded if the class has been reshaped.What would you like to do?' 				trueChoice: 'keep going' 				falseChoice: 'stop and take a look'.			goodToGo ifFalse: [				naughtyBlocks inspect.				self error: 'Here are the bad blocks'].		].	].	"Creation of the segment happens here"	"try using one-quarter of memory min: four megs to publish (will get bumped later)"	sizeHint _ (Smalltalk garbageCollect // 4 // 4) min: 1024*1024.	self copyFromRoots: arrayOfRoots sizeHint: sizeHint areUnique: true.	segSize _ segment size.	[(newRoots _ self rootsIncludingBlockMethods) == nil] whileFalse: [		arrayOfRoots _ newRoots.		self copyFromRoots: arrayOfRoots sizeHint: segSize areUnique: true].		"with methods pointed at from outside"	[(newRoots _ self rootsIncludingBlocks) == nil] whileFalse: [		arrayOfRoots _ newRoots.		self copyFromRoots: arrayOfRoots sizeHint: segSize areUnique: true].		"with methods, blocks from outPointers"	list _ self compactClassesArray.	outPointers _ outPointers, ((list select: [:cls | cls ~~ nil]), (Array with: 1717 with: list)).	1 to: outPointers size do: [:ii | 		(outPointers at: ii) class == BlockContext ifTrue: [outPointers at: ii put: nil].		(outPointers at: ii) class == MethodContext ifTrue: [outPointers at: ii put: nil].		"substitute new object in outPointers"		(replacements includesKey: (outPointers at: ii)) ifTrue: [			outPointers at: ii put: (replacements at: (outPointers at: ii))]].	proj ifNotNil: [self dependentsCancel: proj].	symbolHolder.! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 10/21/2002 16:18'!dependentsCancel: aProject	"Erase the place we temporarily held the dependents of things in this project.  So we don't carry them around forever."	aProject projectParameters removeKey: #GlobalDependentsInProject.! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 10/21/2002 16:17'!dependentsRestore: aProject	"Retrieve the list of dependents from the exporting system, hook them up, and erase the place we stored them."	| dict |	dict _ aProject projectParameterAt: #GlobalDependentsInProject.	dict ifNil: [^ self].	dict associationsDo: [:assoc |		assoc value do: [:dd | assoc key addDependent: dd]].	self dependentsCancel: aProject.! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 10/21/2002 16:25'!dependentsSave: dummy	"Object that have dependents are supposed to be instances of subclasses of Model.  But, class Objects still provides 'Global Dependents', and some people still use them.  When both the model and the dependent are in a project that is being saved, remember them, so we can hook them up when this project is loaded in."	| dict proj list |	proj _ dummy project.	dict _ Dictionary new.	DependentsFields associationsDo: [:assoc |		(dummy references includesKey: assoc key) ifTrue: [			list _ assoc value select: [:dd | dummy references includesKey: dd].			list size > 0 ifTrue: [dict at: assoc key put: list]]].	dict size > 0 ifTrue: [		proj projectParameterAt: #GlobalDependentsInProject put: dict].! !!ImageSegment methodsFor: 'fileIn/Out' stamp: 'tk 10/21/2002 15:34'!comeFullyUpOnReload: smartRefStream	"fix up the objects in the segment that changed size.  An object in the segment is the wrong size for the modern version of the class.  Construct a fake class that is the old size.  Replace the modern class with the old one in outPointers.  Load the segment.  Traverse the instances, making new instances by copying fields, and running conversion messages.  Keep the new instances.  Bulk forward become the old to the new.  Let go of the fake objects and classes.	After the install (below), arrayOfRoots is filled in.  Globalize new classes.  Caller may want to do some special install on certain objects in arrayOfRoots. 	May want to write the segment out to disk in its new form."	| mapFakeClassesToReal ccFixups receiverClasses rootsToUnhiberhate myProject |	self flag: #bobconv.		RecentlyRenamedClasses _ nil.		"in case old data hanging around"	mapFakeClassesToReal _ smartRefStream reshapedClassesIn: outPointers.		"Dictionary of just the ones that change shape.  Substitute them in outPointers."	ccFixups _ self remapCompactClasses: mapFakeClassesToReal 				refStrm: smartRefStream.	ccFixups ifFalse: [^ self error: 'A class in the file is not compatible'].	endMarker _ segment nextObject. 	"for enumeration of objects"	endMarker == 0 ifTrue: [endMarker _ 'End' clone].	arrayOfRoots _ self loadSegmentFrom: segment outPointers: outPointers.		"Can't use install.  Not ready for rehashSets"	mapFakeClassesToReal isEmpty ifFalse: [		self reshapeClasses: mapFakeClassesToReal refStream: smartRefStream	].	receiverClasses _ self restoreEndianness.		"rehash sets"	smartRefStream checkFatalReshape: receiverClasses.	"Classes in this segment."	arrayOfRoots do: [:importedObject | 		importedObject class class == Metaclass ifTrue: [self declare: importedObject]].	arrayOfRoots do: [:importedObject | 		(importedObject isKindOf: CompiledMethod) ifTrue: [			importedObject sourcePointer > 0 ifTrue: [importedObject zapSourcePointer]].		(importedObject isKindOf: Project) ifTrue: [			myProject _ importedObject.			importedObject ensureChangeSetNameUnique.			Project addingProject: importedObject.			importedObject restoreReferences.			self dependentsRestore: importedObject.			ScriptEditorMorph writingUniversalTiles: 				((importedObject projectPreferenceAt: #universalTiles) ifNil: [false])]].		rootsToUnhiberhate _ arrayOfRoots select: [:importedObject | 		importedObject respondsTo: #unhibernate	"ScriptEditors and ViewerFlapTabs"	].	myProject ifNotNil: [		myProject world setProperty: #thingsToUnhibernate toValue: rootsToUnhiberhate	].	mapFakeClassesToReal isEmpty ifFalse: [		mapFakeClassesToReal keys do: [:aFake | 			aFake indexIfCompact > 0 ifTrue: [aFake becomeUncompact].			aFake removeFromSystemUnlogged].		SystemOrganization removeEmptyCategories].	"^ self"! !!ImageSegment methodsFor: 'fileIn/Out' stamp: 'tk 10/21/2002 14:40'!storeDataOn: aDataStream	"Don't wrote the array of Roots.  Also remember the structures of the classes of objects inside the segment."	| tempRoots tempOutP list |	state = #activeCopy ifFalse: [self error: 'wrong state'].		"real state is activeCopy, but we changed it will be right when coming in"	tempRoots _ arrayOfRoots.	tempOutP _ outPointers.	outPointers _ outPointers clone.	self prepareToBeSaved.	arrayOfRoots _ nil.	state _ #imported.	super storeDataOn: aDataStream.		"record my inst vars"	arrayOfRoots _ tempRoots.	outPointers _ tempOutP.	state _ #activeCopy.	aDataStream references at: #AnImageSegment put: false.	"the false is meaningless"		"This key in refs is the flag that there is an ImageSegment in this file."	"Find the receivers of blocks in the segment.  Need to get the structure of their classes into structures.  Put the receivers into references."	(aDataStream byteStream isKindOf: DummyStream) ifTrue: [		list _ Set new.		arrayOfRoots do: [:ea | 			(ea class == BlockContext) | (ea class == MethodContext) ifTrue: [ 				list add: ea receiver class ]].		aDataStream references at: #BlockReceiverClasses put: list].! !!Model methodsFor: 'copying' stamp: 'tk 10/21/2002 12:59'!veryDeepFixupWith: deepCopier 	"See if the dependents are being copied also.  If so, point at the new copies.  (The dependent has self as its model.)	Dependents handled in class Object, when the model is not a Model, are fixed up in Object veryDeepCopy."	| originalDependents refs newDependent |	super veryDeepFixupWith: deepCopier.	originalDependents _ dependents.	originalDependents ifNil: [		^self.		].	dependents _ nil.	refs _ deepCopier references.	originalDependents		do: [:originalDependent | 			newDependent _ refs						at: originalDependent						ifAbsent: [].			newDependent				ifNotNil: [self addDependent: newDependent]]!]style[(29 206 19 395)f1b,f1,f1LObject veryDeepCopy;,f1! !