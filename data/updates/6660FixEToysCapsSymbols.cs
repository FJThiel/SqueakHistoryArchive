'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6643] on 12 April 2005 at 5:22:21 pm'!"Change Set:		FixEToysCapsSymbolsDate:			12 April 2005Author:			Andreas RaabFixes the problem with MultiSymbol capitalization for eToy projects."!!ImageSegment methodsFor: 'fileIn/Out' stamp: 'ar 4/12/2005 12:25'!comeFullyUpOnReload: smartRefStream
	"fix up the objects in the segment that changed size.  An
object in the segment is the wrong size for the modern version of the
class.  Construct a fake class that is the old size.  Replace the
modern class with the old one in outPointers.  Load the segment.
Traverse the instances, making new instances by copying fields, and
running conversion messages.  Keep the new instances.  Bulk forward
become the old to the new.  Let go of the fake objects and classes.
	After the install (below), arrayOfRoots is filled in.
Globalize new classes.  Caller may want to do some special install on
certain objects in arrayOfRoots.
	May want to write the segment out to disk in its new form."

	| mapFakeClassesToReal ccFixups receiverClasses
rootsToUnhiberhate myProject existing |

	RecentlyRenamedClasses _ nil.		"in case old data
hanging around"
	mapFakeClassesToReal _ smartRefStream reshapedClassesIn: outPointers.
		"Dictionary of just the ones that change shape.
Substitute them in outPointers."
	ccFixups _ self remapCompactClasses: mapFakeClassesToReal
				refStrm: smartRefStream.
	ccFixups ifFalse: [^ self error: 'A class in the file is not
compatible'].
	endMarker _ segment nextObject. 	"for enumeration of objects"
	endMarker == 0 ifTrue: [endMarker _ 'End' clone].	self fixCapitalizationOfSymbols.
	arrayOfRoots _ self loadSegmentFrom: segment outPointers: outPointers.
		"Can't use install.  Not ready for rehashSets"
	mapFakeClassesToReal isEmpty ifFalse: [
		self reshapeClasses: mapFakeClassesToReal refStream:
smartRefStream
	].
	"When a Project is stored, arrayOfRoots has all objects in
the project, except those in outPointers"
	arrayOfRoots do: [:importedObject |
		(importedObject isKindOf: MultiString) ifTrue: [
			importedObject mutateJISX0208StringToUnicode.
			importedObject class = MultiSymbol ifTrue: [
				"self halt."
				Symbol hasInterned: 
importedObject asString ifTrue: [:multiSymbol |
					multiSymbol == importedObject
ifFalse: [
						importedObject
becomeForward: multiSymbol.
					].
				].
			].
		].
		(importedObject isKindOf: TTCFontSet) ifTrue: [
			existing _ TTCFontSet familyName:
importedObject familyName
						pointSize:
importedObject pointSize.	"supplies default"
			existing == importedObject ifFalse:
[importedObject becomeForward: existing].
		].
	].
	"Smalltalk garbageCollect.   MultiSymbol rehash.  These take
time and are not urgent, so don't to them.  In the normal case, no
bad MultiSymbols will be found."

	receiverClasses _ self restoreEndianness.		"rehash sets"
	smartRefStream checkFatalReshape: receiverClasses.

	"Classes in this segment."
	arrayOfRoots do: [:importedObject |
		importedObject class class == Metaclass ifTrue: [self
declare: importedObject]].
	arrayOfRoots do: [:importedObject |
		(importedObject isKindOf: CompiledMethod) ifTrue: [
			importedObject sourcePointer > 0 ifTrue:
[importedObject zapSourcePointer]].
		(importedObject isKindOf: Project) ifTrue: [
			myProject _ importedObject.
			importedObject ensureChangeSetNameUnique.
			Project addingProject: importedObject.
			importedObject restoreReferences.
			self dependentsRestore: importedObject.
			ScriptEditorMorph writingUniversalTiles:
				((importedObject projectPreferenceAt:
#universalTiles) ifNil: [false])]].

	rootsToUnhiberhate _ arrayOfRoots select: [:importedObject |
		importedObject respondsTo: #unhibernate
	"ScriptEditors and ViewerFlapTabs"
	].
	myProject ifNotNil: [
		myProject world setProperty: #thingsToUnhibernate
toValue: rootsToUnhiberhate
	].

	mapFakeClassesToReal isEmpty ifFalse: [
		mapFakeClassesToReal keys do: [:aFake |
			aFake indexIfCompact > 0 ifTrue: [aFake
becomeUncompact].
			aFake removeFromSystemUnlogged].
		SystemOrganization removeEmptyCategories].
	"^ self"
! !!ImageSegment methodsFor: 'fileIn/Out' stamp: 'ar 4/12/2005 12:27'!fixCapitalizationOfSymbols	"MultiString>>capitalized was not implemented 
correctly. 	Fix eventual accessors and mutators here."	| sym ms |	1 to: outPointers size do:[:i|		sym := outPointers at: i.		(sym class == MultiSymbol and:[sym size > 3]) ifTrue:[			((sym beginsWith: 'get')				and:[(sym at: 4) asInteger < 256				and:[(sym at: 4) isLowercase]]) ifTrue:[					ms _ sym asString.					ms at: 4 put: (ms at: 4) asUppercase.					ms _ ms asSymbol.					sym becomeForward: ms.			].			((sym beginsWith: 'set')				and:[(sym at: 4) asInteger < 256				and:[(sym at: 4) isLowercase				and:[sym last = $:				and:[(sym occurrencesOf: $:) = 1]]]]) ifTrue:[					ms _ sym asString.					ms at: 4 put: (ms at: 4) asUppercase.					ms _ ms asSymbol.					sym becomeForward: ms.				].			outPointers at: i put: sym.		].	].! !!MultiSymbol methodsFor: 'private' stamp: 'ar 4/12/2005 14:12'!fixUponLoad: aProject seg: anImageSegment	"We are in an old project that is being loaded from disk. 	Fix up conventions that have changed."	| ms |
	"Yoshiki did not put MultiSymbols into outPointers in older 
images!!	When all old images are gone, remove this method."	ms _ Symbol intern: self asString.	self == ms ifFalse: [		"For a project from older m17n image, this is necessary."		self becomeForward: ms.		aProject projectParameters at: #MultiSymbolInWrongPlace put: true	].	"MultiString>>capitalized was not implemented 
correctly. 	Fix eventual accessors and mutators here."	((self beginsWith: 'get')		and:[(self at: 4) asInteger < 256		and:[(self at: 4) isLowercase]]) ifTrue:[			ms _ self asString.			ms at: 4 put: (ms at: 4) asUppercase.			ms _ ms asSymbol.			self becomeForward: ms.			aProject projectParameters at: #MultiSymbolInWrongPlace put: true.		].	((self beginsWith: 'set')		and:[(self at: 4) asInteger < 256		and:[(self at: 4) isLowercase		and:[self last = $:		and:[(self occurrencesOf: $:) = 1]]]]) ifTrue:[			ms _ self asString.			ms at: 4 put: (ms at: 4) asUppercase.			ms _ ms asSymbol.			self becomeForward: ms.			aProject projectParameters at: #MultiSymbolInWrongPlace put: true.		].	^ super fixUponLoad: aProject seg: anImageSegment	"me, 
not the label"! !!MultiSymbol reorganize!('accessing' at: at:put: byteAt: byteAt:put: byteSize species wordAt: wordAt:put:)('testing' isMultiByteString)('private' fixUponLoad:seg: pvtAt:put: string:)!