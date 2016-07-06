'From Vancouver1.0 of 29 September 2004 [latest update: #350] on 6
December 2004 at 10:41:24 am'!
"Change Set:		ProjectLoad-tk
Date:			2 December 2004
Author:			Ted Kaehler

Clean up the code that load a Project from disk or a server.
Improved class comments in Project and ImageSegment.  Speed of
loading is only modestly better.

Provides a new way for objects in a Project to adjust themselves when
they enter a different version of the system.  After a project is
loaded, every object is sent fixUponLoad: aProject refStream:
smartRefStrm.

Just to review:
	comeFullyUpOnReload: smartRefStream		Used to
re-discover an object that already exists in this image, such as a
resource, global variable, Character, or Symbol.  (sent to objects in
outPointers)
	convertToCurrentVersion: varDict refStream: smartRefStrm
		fill in fields that have been added to a class since
the object was stored.  Used to set the extra inst var to a default
value.  Or, return a new object of a different class.  (sent to
objects that changed instance variables)
	fixUponLoad: aProject refStream: smartRefStrm
	change the object due to conventions that have changed on the
project level.  (sent to all objects in the incoming project)
"!


!ImageSegment commentStamp: 'tk 12/2/2004 12:33' prior: 0!
I represent a segment of Squeak address space.  I am created from an
array of root objects.  After storing, my segment contains a binary
encoding of every object accessible from my roots but not otherwise
accessible from anywhere else in the system.  My segment contains
outward pointers that are indices into my table of outPointers.
	The main use of ImageSegments is to store Projects.  A dummy
version of SmartRefStream traverses the Project.  Everything it finds
is classified as either an object that is owned by the project (only
pointed to inside the project), or an object outside the project that
is pointed to from inside the project.  The objects that are
completely owned by the project are compressed into pure binary form
in an ImageSegment.  The outside objects are put in the 'outPointers'
array.  The entire ImageSegment (binary part plus outPointers) is
encoded in a SmartRefStream, and saved on the disk.  (aProject
exportSegmentWithChangeSet:fileName:directory:) calls (anImageSegment
writeForExportWithSources:inDirectory:changeSet:).
	Note that every object inside the project is put into the
segment's arrayOfRoots.  This is because a dummy SmartRefStream to
scan the project, in order to make intelligent decisions about what
belongs in the project.
	See Project's class comment for what messages are sent to
objects as they are unpacked in a new image.

---- Older Details ------

	The primary kind of image segment is an Export Segment.  It
can be saved on a server and read into a completely different Squeak
image.
Old way to create one:
(ImageSegment new copyFromRootsForExport: (Array with: Baz with: Baz class))
		writeForExport: 'myFile.extSeg'.
Old way to create one for a project:
	(Project named: 'Play With Me - 3') exportSegment.
To read it into another image:  Select 'myFile.extSeg' in a FileList,
Menu 'load as project'.  It will install its classes automatically.
If you need to see the roots array, it is temporarily stored in
(SmartRefStream scannedObject).

Most of 'states' of an ImageSegment are not used to export a project,
and have been abandoned.

	When a segment is written out onto a file, it goes in a
folder called <image name>_segs.  If your image is called
"Squeak2.6.image", the folder "Squeak2.6_segs" must accompany the
image whenever your move, copy, or rename it.
	Whenever a Class is in arrayOfRoots, its class (aClass class)
must also be in the arrayOfRoots.
	There are two kinds of image segments.  Normal image segments
are a piece of a specific Squeak image, and can only be read back
into that image.  The image holds the array of outPointers that are
necessary to turn the bits in the file into objects.
	To put out a normal segment that holds a Project (not the
current project), execute (Project named: 'xxx') storeSegment.


arrayOfRoots	The objects that head the tree we will trace.
segment			The WordArray of raw bits of all objects in the tree.
outPointers		Oops of all objects outside the segment
pointed to from inside.
state			(see below)
segmentName	Its basic name.  Often the name of a Project.
fileName		The local name of the file.  'Foo-23.seg'
endMarker		An object located in memory somewhere after a
segment that has
		just been brought in.  To enumerate the objects in
the segment, start at
		the segment and go to this object.
userRootCnt		number of roots submitted by caller.  Extras
are added in preparation for saving.

state that an ImageSegment may exist in...

#activeCopy			(has been copied, with the intent to
become active)
arrayOfRoots, segment, and outPointers have been created by
copyFromRoots:.  The tree of objects has been encoded in the segment,
but those objects are still present in the Squeak system.

#active				(segment is actively holding objects)
The segment is now the only holder of tree of objects.  Each of the
original roots has been transmuted into an ImageSegmentRootStub that
refers back to this image segment.  The original objects in the
segment will all be garbageCollected.

#onFile
The segment has been written out to a file and replaced by a file
pointer.  Only ImageSegmentRootStubs and the array of outPointers
remains in the image.  To get this far:
(ImageSegment new copyFromRoots: (Array with: Baz with: Baz class))
		writeToFile: 'myFile.seg'.

#inactive
The segment has been brought back into memory and turned back into
objects.  rootsArray is set, but the segment is invalid.

#onFileWithSymbols
The segment has been written out to a file, along with the text of
all the symbols in the outPointers array, and replaced by a file
pointer.  This reduces the size of the outPointers array, and also
allows the system to reclaim any symbols that are not referred to
from elsewhere in the image.  The specific format used is that of a
literal array as follows:
	#(symbol1 symbol2 # symbol3 symbol4 'symbolWithSpaces' # symbol5).
In this case, the original outPointers array was 8 long, but the
compacted table of outPointers retains only two entries.  These get
inserted in place of the #'s in the array of symbols after it is read
back in.  Symbols with embedded spaces or other strange characters
are written as strings, and converted back to symbols when read back
in.  The symbol # is never written out.
	NOTE: All IdentitySets or dictionaries must be rehashed when
being read back from this format.  The symbols are effectively
internal.  (No, not if read back into same image.  If a different
image, then use #imported.  -tk)

#imported
The segment is on an external file or just read in from one.  The
segment and outPointers are meant to be read into a foreign image.
In this form, the image segment can be read from a URL, and
installed.  A copy of the original array of root objects is
constructed, with former outPointers bound to existing objects in the
host system.
	(Any Class inside the segment MUST be in the arrayOfRoots.
This is so its association can be inserted into Smalltalk.  The
class's metaclass must be in roots also.  Methods that are in
outPointers because blocks point at them, were found and added to the
roots.
	All IdentitySets and dictionaries are rehashed when being
read back from exported segments.)


To discover why only some of the objects in a project are being
written out, try this (***Destructive Test***).  This breaks lots of
backpointers in the target project, and puts up an array of
suspicious objects, a list of the classes of the outPointers, and a
debugger.
"Close any transcripts in the target project"
World currentHand objectToPaste ifNotNil: [
	self inform: 'Hand is holding a Morph in its paste buffer:\' withCRs,
		World currentHand objectToPaste printString].
PV _ Project named: 'xxxx'.
(IS _ ImageSegment new) findRogueRootsImSeg:
	(Array with: PV world presenter with: PV world).
IS findOwnersOutPtrs.	"Optionally: write a file with owner chains"
"Quit and DO NOT save"

When an export image segment is brought into an image, it is like an
image starting up.  Certain startUp messages need to be run.  These
are byte and word reversals for nonPointer data that comes from a
machine of the opposite endianness.  #startUpProc passes over all
objects in the segment, and:
	The first time an instance of class X is encountered, (msg _
X startUpFrom: anImageSegment) is sent.  If msg is nil, the usual
case, it means that instances of X do not need special work.  X is
included in the IdentitySet, noStartUpNeeded.  If msg is not nil,
store it in the dictionary, startUps (aClass -> aMessage).
	When a later instance of X is encountered, if X is in
noStartUpNeeded, do nothing.  If X is in startUps, send the message
to the instance.  Typically this is a message like #swapShortObjects.
	Every class that implements #startUp, should see if it needs
a parallel implementation of #startUpFrom:.  !




!Project commentStamp: 'tk 12/2/2004 12:38' prior: 0!
A Project stores the state of a complete Squeak desktop, including
the windows, and the currently active changeSet.  A project knows who
its parent project is.  When you change projects, whether by entering
or exiting, the screen state of the project being exited is saved in
that project.

A project is retained by its view in the parent world.  It is
effectively named by the name of its changeSet, which can be changed
either by renaming in a changeSorter, or by editing the label of its
view from the parent project.

As the site of major context switch, Projects are the locus of
swapping between the old MVC and the new Morphic worlds.  The
distinction is based on whether the variable 'world' contains a
WorldMorph or a ControlManager.

Saving and Loading
Projects may be stored on the disk in external format.  (Project
named: 'xxx') exportSegment, or choose 'store project on file...'.
Projects may be loaded from a server and stored back.  Storing on a
server never overwrites;  it always makes a new version.  A project
remembers the url of where it lives in urlList.  The list is length
one, for now.  The url may point to a local disk instead of a server.
All projects that the user looks at, are cached in the Squeaklet
folder.  Sorted by server.  The cache holds the most recent version
only.

When a project is loaded into Squeak, its objects are converted to
the current version.  There are three levels of conversion.  First,
each object is converted from raw bits to an object in its old
format.  Then it is sent some or all of these messages:
	comeFullyUpOnReload: smartRefStream  		Used to
re-discover an object that already exists in this image, such as a
resource, global variable, Character, or Symbol.  (sent to objects in
outPointers)
	convertToCurrentVersion: varDict refStream: smartRefStrm
		fill in fields that have been added to a class since
the object was stored.  Used to set the extra inst var to a default
value.  Or, return a new object of a different class.  (sent to
objects that changed instance variables)
	fixUponLoad: aProject refStream: smartRefStrm
	change the object due to conventions that have changed on the
project level.  (sent to all objects in the incoming project)

Here is the calling sequence for storing out a Project:
Project saveAs
Project storeOnServer
Project storeOnServerWithProgressInfo
Project storeOnServerInnards
Project exportSegmentFileName:directory:
Project exportSegmentWithChangeSet:fileName:directory:
ImageSegment writeForExportWithSources:inDirectory:changeSet:
---------
Isolation (not used any more)
When you accept a method, the entire system feels the change, except
projects that are "isolated".  In an isolated project, all new global
variables (including new classes) arestored in the project-local
environment, and all changes to preexisting classes are revoked when
you leave the project.  When you enter another project, that
project's changes are invoked.  Invocation and revocation are handled
efficiently by swapping pointers.  To make a project be isolated,
choose 'isolate changes of this project' from the 'changes...'
section of the screen menu.  You can use an isolated project for
making dangerous change to a system, and you can get out if it
crashes.  A foreign application can have the separate environment it
wants.  Also, you can freeze part of the system for a demo that you
don't want to disturb.  An isolated project shares methods with all
subprojects inside it, unless they are isolated themselves.   Each
isolated project is the head of a tree of projects with which it
shares all methods.

You may 'assert' all changes ever made in the current project to take
effect above this project.  This amounts to exporting all the globals
in the current environment, and zapping the revocation lists to that
the current state of the world will remain in force upon exit from
this project.

[Later: A project may be 'frozen'.  Asserts do not apply to it after
that.  (Great for demos.)  You should be informed when an assert was
blocked in a frozen project.]

Class definitions are layered by the isolation mechanism.  You are
only allowed to change the shape of a class in projects that lie
within its isolation scope.  All versions of the methods are
recompiled, in all projects.  If you remove an inst var that is in
use in an isolated project, it will become an Undeclared global.  It
is best not to remove an inst var when it is being used in another
isolated project. [If we recompile them all, why can't we diagnose
the problem before allowing the change??]

Senders and Implementors do not see versions of a method in isolated
projects.  [again, we might want to make this possible at a cost].
When you ask for versions of a method, you will not get the history
in other isolated projects.

Moving methods and classes between changeSets, and merging changeSets
has no effect on which methods are in force.  But, when you look at a
changeSet from a different isolated project, the methods will contain
code that is not in force.  A changeSet is just a list of method
names, and does not keep separate copies of any code.

When finer grained assertion is needed, use the method (aProject
assertClass: aClass from: thisProject warn: warnConflicts).

How isolated changes work: The first time a class changes, store its
MethodDictionary object.  Keep parallel arrays of associations to
Classes and MethodDictionaries.  Traverse these and install them when
you enter an "ioslated project".  When you leave, store this
project's own MethodDictionaries there.
	To do an assert, we must discover which methods changed here,
and which changed only in the project we are asserting into.  There
is one copy of the 'virgin' method dictionaries in the system.  It is
always being temporarily stored by the currently inForce isolated
project.

isolatedHead - true for the top project, and for each isolated
project.  false or nil for any subproject that shares all methods
with its parent project.

inForce -  true if my methods are installed now.  false if I am
dormant. [is this equivalent to self == Project Current?]

classArray - list of associations to classes

methodDictArray - the method dictionaries of those classes before we
started changing methods.  They hang onto the original
compiledMethods.  (If this project is dormant, it contains the method
dictionaries of those classes as they will be here, in this project).

orgArray - the class organizations of the classes in classArray.

UsingIsolation (class variable) - No longer used.

When you want to save a project in export format from within that
very project, it gets tricky.  We set two flags in parentProject,
exit to it, and let parentProject write the project.
ProjectViewMorph in parentProject checks in its step method, does the
store, clears the flags, and reenters the subProject.

!


!Object methodsFor: 'objects from disk' stamp: 'tk 11/29/2004 15:04'!
fixUponLoad: aProject seg: anImageSegment
	"change the object due to conventions that have changed on
the project level.  (sent to all objects in the incoming project).
Specific classes should reimplement this."! !


!ChangeSet methodsFor: 'converting' stamp: 'tk 11/26/2004 05:56'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	"major change - 4/4/2000"
	| newish |
	varDict at: 'classChanges' ifPresent: [ :x |
		newish _ self convertApril2000: varDict using: smartRefStrm.
		newish == self ifFalse: [^ newish].
		].
	^super convertToCurrentVersion: varDict refStream: smartRefStrm.

! !


!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/1/2004 16:26'!
smartFillRoots: dummy
	| refs known ours ww blockers |
	"Put all traced objects into my arrayOfRoots.  Remove some
that want to be in outPointers.  Return blockers, an
IdentityDictionary of objects to replace in outPointers."

	blockers _ dummy blockers.
	known _ (refs _ dummy references) size.
	refs fasterKeys do: [:obj | "copy keys to be OK with removing items"
		(obj class == Symbol) ifTrue: [refs removeKey: obj.
known _ known-1].
		(obj class == MultiSymbol) ifTrue: [refs removeKey:
obj.  known _ known-1].
		(obj class == PasteUpMorph) ifTrue: [
			obj isWorldMorph & (obj owner == nil) ifTrue: [
				obj == dummy project world ifFalse: [
					refs removeKey: obj.  known _ known-1.
					blockers at: obj put:
						(StringMorph
contents: 'The worldMorph of a different world')]]].
					"Make a ProjectViewMorph here"
		"obj class == Project ifTrue: [Transcript show: obj; cr]."
		(blockers includesKey: obj) ifTrue: [
			refs removeKey: obj ifAbsent: [known _
known+1].  known _ known-1].
		].
	ours _ dummy project world.
	refs keysDo: [:obj |
			obj isMorph ifTrue: [
				ww _ obj world.
				(ww == ours) | (ww == nil) ifFalse: [
					refs removeKey: obj.  known _ known-1.
					blockers at: obj put:
(StringMorph contents:
								obj
printString, ' from another world')]]].
	"keep original roots on the front of the list"
	(dummy rootObject) do: [:rr | refs removeKey: rr ifAbsent: []].
	^ dummy rootObject, refs fasterKeys asArray.

! !

!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 12/2/2004 12:41'!
writeForExportWithSources: fName inDirectory: aDirectory changeSet:
aChangeSetOrNil
	"Write the segment on the disk with all info needed to
reconstruct it in a new image.  For export.  Out pointers are encoded
as normal objects on the disk.  Append the source code of any classes
in roots.  Target system will quickly transfer the sources to its
changes file."
	"Files out a changeSet first, so that a project can contain
classes that are unique to the project."

	| fileStream temp tempFileName zipper allClassesInRoots
classesToWriteEntirely methodsWithSource |
	state = #activeCopy ifFalse: [self error: 'wrong state'].
	(fName includes: $.) ifFalse: [
		^ self inform: 'Please use ''.pr'' or ''.extSeg'' at
the end of the file name'.].
	temp _ endMarker.
	endMarker _ nil.
	tempFileName _ aDirectory nextNameFor: 'SqProject' extension: 'temp'.
	zipper _ [
		Preferences debugPrintSpaceLog ifTrue:[
			fileStream _ aDirectory newFileNamed:
				(fName copyFrom: 1 to: (fName
lastIndexOf: $.)), 'space'.
			self printSpaceAnalysisOn: fileStream.
			fileStream close].
		ProgressNotification signal: '3:uncompressedSaveComplete'.
		(aDirectory oldFileNamed: tempFileName) compressFile.
	"makes xxx.gz"
		aDirectory
			rename: (tempFileName, FileDirectory dot, 'gz')
			toBe: fName.
		aDirectory
			deleteFileNamed: tempFileName
			ifAbsent: []
	].
	fileStream _ aDirectory newFileNamed: tempFileName.
	fileStream fileOutChangeSet: aChangeSetOrNil andObject: self.
		"remember extra structures.  Note class names."
	endMarker _ temp.

	"append sources"
	allClassesInRoots _ arrayOfRoots select: [:cls | cls
isKindOf: Behavior].
	classesToWriteEntirely _ allClassesInRoots select: [ :cls |
cls theNonMetaClass isSystemDefined].
	methodsWithSource _ OrderedCollection new.
	allClassesInRoots do: [ :cls |
		(classesToWriteEntirely includes: cls) ifFalse: [
			cls selectorsAndMethodsDo: [ :sel :meth |
				meth sourcePointer = 0 ifFalse:
[methodsWithSource add: {cls. sel. meth}].
			].
		].
	].
	(classesToWriteEntirely isEmpty and: [methodsWithSource
isEmpty]) ifTrue: [zipper value. ^ self].

	fileStream reopen; setToEnd.
	fileStream nextPutAll: '\\!!ImageSegment new!!\\' withCRs.
	methodsWithSource do: [ :each |
		fileStream nextPut: $!!.	"try to pacify
ImageSegment>>scanFrom:"
		fileStream nextChunkPut: 'RenamedClassSourceReader
formerClassName: ',
				each first name printString,' methodsFor: ',
				(each first organization
categoryOfElement: each second) asString printString,
				' stamp: ',(Utilities
timeStampForMethod: each third) printString; cr.
		fileStream nextChunkPut: (each third getSourceFor:
each second in: each first) asString.
		fileStream nextChunkPut: ' '; cr.
	].
	classesToWriteEntirely do: [:cls |
		cls isMeta ifFalse: [fileStream nextPutAll:
						(cls name, '
category: ''', cls category, '''.!!'); cr; cr].
		cls organization
			putCommentOnFile: fileStream
			numbered: 0
			moveSource: false
			forClass: cls.	"does nothing if metaclass"
		cls organization categories do:
			[:heading |
			cls fileOutCategory: heading
				on: fileStream
				moveSource: false
				toFile: 0]].
	"no class initialization -- it came in as a real object"
	fileStream close.
	zipper value.
! !

!ImageSegment methodsFor: 'fileIn/Out' stamp: 'tk 12/6/2004 09:56'!
comeFullyUpOnReload: smartRefStream
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
	endMarker == 0 ifTrue: [endMarker _ 'End' clone].
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
				MultiSymbol hasInternedALoadedSymbol:
importedObject ifTrue: [:multiSymbol |
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
! !


!Morph methodsFor: 'objects from disk' stamp: 'tk 11/26/2004 06:02'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	(varDict at: #ClassName) == #DropShadowMorph ifTrue: [
		varDict at: #ClassName put: #Morph.	"so we don't
repeat this"
		^ self convertNovember2000DropShadow: varDict using:
smartRefStrm
			"always returns a new object of a different class"
	].
	varDict at: 'costumee' ifPresent: [ :x |
		self convertAugust1998: varDict using: smartRefStrm].
		"never returns a different object"

	"5/18/2000"
	varDict at: 'openToDragNDrop' ifPresent: [ :x | self
enableDragNDrop: x ].
	^super convertToCurrentVersion: varDict refStream: smartRefStrm.


! !


!AlignmentMorph methodsFor: 'objects from disk' stamp: 'tk 11/26/2004 05:51'!
convertToCurrentVersion: varDict refStream: smartRefStrm

	| newish |
	newish _ super convertToCurrentVersion: varDict refStream:
smartRefStrm.

	"major change - much of AlignmentMorph is now implemented
more generally in Morph"
	varDict at: 'hResizing' ifPresent: [ :x |
		^ newish convertOldAlignmentsNov2000: varDict using:
smartRefStrm].
	^ newish
! !


!MultiSymbol methodsFor: 'objects from disk' stamp: 'tk 12/6/2004 10:36'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk.
Fix up conventions that have changed."
	| ms |

	"Yoshiki did not put MultiSymbols into outPointers in older
images!!  When all old images are gone, remove this method."
	ms _ MultiSymbol intern: self asString.
	self == ms ifFalse: ["For a project from older m17n image,
this is necessary."
				self becomeForward: ms.
				aProject projectParameters at:
#MultiSymbolInWrongPlace put: true].

	^ super fixUponLoad: aProject seg: anImageSegment	"me,
not the label"
! !


!PasteUpMorph methodsFor: 'objects from disk' stamp: 'tk 11/29/2004 17:31'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk.
Fix up conventions that have changed."

	self isWorldMorph ifTrue: [
			(self valueOfProperty: #soundAdditions) ifNotNilDo:
				[:additions | SampledSound
assimilateSoundsFrom: additions]].

	^ super fixUponLoad: aProject seg: anImageSegment! !


!Project methodsFor: 'file in/out' stamp: 'tk 12/2/2004 12:27'!
exportSegmentWithChangeSet: aChangeSetOrNil fileName: aFileName
directory: aDirectory
	"Store my project out on the disk as an *exported*
ImageSegment.  All outPointers will be in a form that can be resolved
in the target image.  Name it <project name>.extSeg.  Whatdo we do
about subProjects, especially if they are out as local image
segments?  Force them to come in?
	Player classes are included automatically."

	| is str ans revertSeg roots holder collector fd mgr stacks |

	"Files out a changeSet first, so that a project can contain
its own classes"
world isMorph ifFalse: [
	self projectParameters at: #isMVC put: true.
	^ false].	"Only Morphic projects for now"
world ifNil: [^ false].  world presenter ifNil: [^ false].

Utilities emptyScrapsBook.
world currentHand pasteBuffer: nil.	  "don't write the paste buffer."
world currentHand mouseOverHandler initialize.	  "forget about any
references here"
	"Display checkCurrentHandForObjectToPaste."
Command initialize.
world clearCommandHistory.
world fullReleaseCachedState; releaseViewers.
world cleanseStepList.
world localFlapTabs size = world flapTabs size ifFalse: [
	self error: 'Still holding onto Global flaps'].
world releaseSqueakPages.
ScriptEditorMorph writingUniversalTiles: (self projectParameterAt:
#universalTiles ifAbsent: [false]).
holder _ Project allProjects.	"force them in to outPointers, where
DiskProxys are made"

"Just export me, not my previous version"
revertSeg _ self projectParameters at: #revertToMe ifAbsent: [nil].
self projectParameters removeKey: #revertToMe ifAbsent: [].

roots _ OrderedCollection new.
roots add: self; add: world; add: transcript; add: changeSet; add: thumbnail.
roots add: world activeHand.

	"; addAll: classList; addAll: (classList collect: [:cls | cls class])"

roots _ roots reject: [ :x | x isNil].	"early saves may not have
active hand or thumbnail"

	fd _ aDirectory directoryNamed: self resourceDirectoryName.
	fd assureExistence.
	"Clean up resource references before writing out"
	mgr _ self resourceManager.
	self resourceManager: nil.
	ResourceCollector current: ResourceCollector new.
	ResourceCollector current localDirectory: fd.
	ResourceCollector current baseUrl: self resourceUrl.
	ResourceCollector current initializeFrom: mgr.
	ProgressNotification signal: '2:findingResources' extra:
'(collecting resources...)'.
	"Must activate old world because this is run at #armsLength.
	Otherwise references to ActiveWorld, ActiveHand, or ActiveEvent
	will not be captured correctly if referenced from blocks or user code."
	world becomeActiveDuring:[
		is _ ImageSegment new copySmartRootsExport: roots asArray.
		"old way was (is _ ImageSegment new
copyFromRootsForExport: roots asArray)"
	].
	self resourceManager: mgr.
	collector _ ResourceCollector current.
	ResourceCollector current: nil.
	ProgressNotification signal: '2:foundResources' extra: ''.
	is state = #tooBig ifTrue: [
		collector replaceAll.
		^ false].

str _ ''.
"considered legal to save a project that has never been entered"
(is outPointers includes: world) ifTrue: [
	str _ str, '\Project''s own world is not in the segment.' withCRs].
str isEmpty ifFalse: [
	ans _ (PopUpMenu labels: 'Do not write file
Write file anyway
Debug') startUpWithCaption: str.
	ans = 1 ifTrue: [
		revertSeg ifNotNil: [projectParameters at:
#revertToMe put: revertSeg].
		collector replaceAll.
		^ false].
	ans = 3 ifTrue: [
		collector replaceAll.
		self halt: 'Segment not written']].
	stacks _ is findStacks.

	is
		writeForExportWithSources: aFileName
		inDirectory: fd
		changeSet: aChangeSetOrNil.
	SecurityManager default signFile: aFileName directory: fd.
	"Compress all files and update check sums"
	collector forgetObsolete.
	self storeResourceList: collector in: fd.
	self storeHtmlPageIn: fd.
	self writeStackText: stacks in: fd registerIn: collector.
	"local proj.005.myStack.t"
	self compressFilesIn: fd to: aFileName in: aDirectory
resources: collector.
			"also deletes the resource directory"
	"Now update everything that we know about"
	mgr updateResourcesFrom: collector.

revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].
holder.

collector replaceAll.

world flapTabs do: [:ft |
		(ft respondsTo: #unhibernate) ifTrue: [ft unhibernate]].
is arrayOfRoots do: [:obj |
	obj class == ScriptEditorMorph ifTrue: [obj unhibernate]].
^ true
! !


!ProjectLoading class methodsFor: 'as yet unclassified' stamp: 'tk
12/6/2004 10:37'!
openName: aFileName stream: preStream fromDirectory: aDirectoryOrNil
withProjectView: existingView
	"Reconstitute a Morph from the selected file, presumed to be
represent a Morph saved via the SmartRefStream mechanism, and open it
in an appropriate Morphic world."

   	| morphOrList proj trusted localDir projStream archive mgr
projectsToBeDeleted baseChangeSet enterRestricted substituteFont
numberOfFontSubstitutes exceptions |
	(preStream isNil or: [preStream size = 0]) ifTrue: [
		ProgressNotification  signal: '9999 about to enter
project'.		"the hard part is over"
		^self inform:
'It looks like a problem occurred while
getting this project. It may be temporary,
so you may want to try again,' translated
	].
	ProgressNotification signal: '2:fileSizeDetermined
',preStream size printString.
	preStream isZipArchive
		ifTrue:[	archive _ ZipArchive new readFrom: preStream.
				projStream _ self
projectStreamFromArchive: archive]
		ifFalse:[projStream _ preStream].
	trusted _ SecurityManager default positionToSecureContentsOf:
projStream.
	trusted ifFalse:
		[enterRestricted := (preStream isTypeHTTP or:
[aFileName isNil])
			ifTrue: [Preferences securityChecksEnabled]
			ifFalse: [Preferences standaloneSecurityChecksEnabled].
		enterRestricted
			ifTrue: [SecurityManager default enterRestrictedMode
				ifFalse:
					[preStream close.
					^ self]]].

	localDir _ Project squeakletDirectory.
	aFileName ifNotNil: [
		(aDirectoryOrNil isNil or: [aDirectoryOrNil pathName
~= localDir pathName]) ifTrue: [
			localDir deleteFileNamed: aFileName.
			(localDir fileNamed: aFileName) binary
				nextPutAll: preStream contents;
				close.
		].
	].
	morphOrList _ projStream asUnZippedStream.
	preStream sleep.		"if ftp, let the connection close"
	ProgressNotification  signal: '3:unzipped'.
	ResourceCollector current: ResourceCollector new.
	baseChangeSet _ ChangeSet current.
	self useTempChangeSet.		"named zzTemp"
	"The actual reading happens here"
	substituteFont := Preferences standardEToysFont copy.
	numberOfFontSubstitutes := 0.
	exceptions := Set new.
	[[morphOrList _ morphOrList fileInObjectAndCode]
		on: FontSubstitutionDuringLoading do: [ :ex |
				exceptions add: ex.
				numberOfFontSubstitutes :=
numberOfFontSubstitutes + 1.
				ex resume: substituteFont ]]
			ensure: [ ChangeSet  newChanges: baseChangeSet].
	mgr _ ResourceManager new initializeFrom: ResourceCollector current.
	mgr fixJISX0208Resource.
	mgr registerUnloadedResources.
	archive ifNotNil:[mgr preLoadFromArchive: archive cacheName:
aFileName].
	(preStream respondsTo: #close) ifTrue:[preStream close].
	ResourceCollector current: nil.
	ProgressNotification  signal: '4:filedIn'.
	ProgressNotification  signal: '9999 about to enter project'.
		"the hard part is over"
	(morphOrList isKindOf: ImageSegment) ifTrue: [
		proj _ morphOrList arrayOfRoots
			detect: [:mm | mm isKindOf: Project]
			ifNone: [^self inform: 'No project found in
this file'].
		proj projectParameters at: #substitutedFont put: (
			numberOfFontSubstitutes > 0
				ifTrue: [substituteFont]
				ifFalse: [#none]).
		proj projectParameters at: #MultiSymbolInWrongPlace put: false.
			"Yoshiki did not put MultiSymbols into
outPointers in older images!!"
		morphOrList arrayOfRoots do: [:obj |
			obj fixUponLoad: proj seg: morphOrList "imageSegment"].
		(proj projectParameters at: #MultiSymbolInWrongPlace) ifTrue: [
			morphOrList arrayOfRoots do: [:obj | (obj
isKindOf: Set) ifTrue: [obj rehash]]].

		proj resourceManager: mgr.
		"proj versionFrom: preStream."
		proj lastDirectory: aDirectoryOrNil.
		CurrentProjectRefactoring currentBeParentTo: proj.
		projectsToBeDeleted _ OrderedCollection new.
		existingView ifNil: [
			Smalltalk isMorphic ifTrue: [
				proj createViewIfAppropriate.
			] ifFalse: [
				ChangeSorter allChangeSets add: proj changeSet.
				ProjectView openAndEnter: proj.
				"Note: in MVC we get no further than the above"
			].
		] ifNotNil: [
			(existingView project isKindOf: DiskProxy) ifFalse: [
				existingView project changeSet name: 
ChangeSet defaultName.
				projectsToBeDeleted add: existingView project.
			].
			(existingView owner isSystemWindow) ifTrue: [
				existingView owner model: proj
			].
			existingView project: proj.
		].
		ChangeSorter allChangeSets add: proj changeSet.
		Project current projectParameters
			at: #deleteWhenEnteringNewProject
			ifPresent: [ :ignored |
				projectsToBeDeleted add: Project current.
				Project current removeParameter:
#deleteWhenEnteringNewProject.
			].
		projectsToBeDeleted isEmpty ifFalse: [
			proj projectParameters
				at: #projectsToBeDeleted
				put: projectsToBeDeleted.
		].
		^ ProjectEntryNotification signal: proj
	].

	(morphOrList isKindOf: SqueakPage) ifTrue: [
		morphOrList _ morphOrList contentsMorph
	].
	(morphOrList isKindOf: PasteUpMorph) ifFalse:
		[^ self inform: 'This is not a PasteUpMorph or
exported Project.' translated].
	(Project newMorphicOn: morphOrList) enter
! !


!ScriptEditorMorph methodsFor: 'objects from disk' stamp: 'tk
11/29/2004 17:27'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk.
Fix up conventions that have changed."

	(aProject projectParameters at: #substitutedFont ifAbsent: [#none])
		 ~~ #none ifTrue: [ self setProperty:
#needsLayoutFixed toValue: true ].

	^ super fixUponLoad: aProject seg: anImageSegment! !


!SmartRefStream methodsFor: 'import image segment' stamp: 'tk
11/26/2004 05:53'!
applyConversionMethodsTo: objectIn className: className varMap: varMap
	"Modify the object's instance vars to have the proper values
for its new shape.  Mostly, fill in defaut values of new inst vars.
Can substitute an object of a different class.  (Beware: if
substituted, varMap will not be correct when the new object is asked
to convert.)"
	| anObject prevObject |

	self flag: #bobconv.

	anObject _ objectIn.
	[
		prevObject _ anObject.
		anObject _ anObject convertToCurrentVersion: varMap
refStream: self.
		prevObject == anObject
	] whileFalse.
	^anObject
! !


!StringMorph methodsFor: 'objects from disk' stamp: 'tk 11/29/2004 16:52'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk.
Fix up conventions that have changed."

	| substituteFont |
	substituteFont _ aProject projectParameters at:
#substitutedFont ifAbsent: [#none].
	(substituteFont ~~ #none and: [self font == substituteFont])
			ifTrue: [ self fitContents ].

	^ super fixUponLoad: aProject seg: anImageSegment! !


!TTCFontDescription class methodsFor: 'as yet unclassified' stamp:
'tk 12/6/2004 09:05'!
descriptionNamed: descriptionName at: index

	| array |
	(array _  self descriptionNamed: descriptionName) ifNil: [^ nil].
	^ array at: index.
! !


!TTCFontSet methodsFor: 'objects from disk' stamp: 'tk 12/4/2004 14:51'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  Write a
reference to a known FontSet in the other system instead."

	"a path to me"
	dp _ DiskProxy global: #TTCFontSet selector: #familyName:pointSize:
			args: {self familyName. self pointSize}.
	refStrm replace: self with: dp.
	^ dp.
! !


!TextMorph methodsFor: 'objects from disk' stamp: 'tk 11/29/2004 16:54'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk.
Fix up conventions that have changed."

	| substituteFont |
	substituteFont _ aProject projectParameters at:
#substitutedFont ifAbsent: [#none].
	(substituteFont ~~ #none and: [self textStyle fontArray
includes: substituteFont])
			ifTrue: [ self fit ].

	^ super fixUponLoad: aProject seg: anImageSegment! !


!StringMorph reorganize!
('accessing' contents contents: contentsClipped: fitContents font
font:emphasis: fontName:size: fontToUse getCharacters
handsWithMeForKeyboardFocus interimContents: measureContents
minimumWidth setWidth: text userString valueFromContents)
('drawing' drawOn: lookTranslucent)
('editing' acceptContents acceptValue: cancelEdits doneWithEdits
launchMiniEditor: lostFocusWithoutAccepting
wantsKeyboardFocusOnShiftClick)
('event handling' handlesMouseDown: hasFocus mouseDown:
wouldAcceptKeyboardFocus)
('font' emphasis:)
('halos and balloon help' addOptionalHandlesTo:box: boundsForBalloon)
('initialization' defaultColor initWithContents:font:emphasis: initialize)
('layout' fullBounds)
('menu' addCustomMenuItems:hand: changeEmphasis changeFont changeString)
('parts bin' initializeToStandAlone)
('printing' balloonTextForClassAndMethodString
balloonTextForLexiconString balloonTextForMethodString font:
fullPrintOn: printOn:)
('objects from disk' fixUponLoad:seg:)
('*connectors-layout' minHeight)
('*connectors-testing' isStringMorph)
!


!ScriptEditorMorph reorganize!
('access' associatedPlayer morph myMorph scriptInstantiation scriptedPlayer)
('buttons' addDestroyButtonTo: addDismissButtonTo: addYesNoToHand
chooseFrequency chooseTrigger destroyScript dismiss
editMethodDescription hasParameter install installWithNewLiteral
makeIsolatedCodePane numberOfParameters playerScripted replaceRow1
scriptName scriptTitle scriptee showSourceInScriptor tryMe
updateStatus updateStatusMorph:)
('caching' releaseCachedState resetHandWithTile)
('copying' veryDeepFixupWith: veryDeepInner:)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event:
assureParameterTilesValid buttonRowForEditor indexOfMorphAbove:
prepareToUndoDropOf: removeSpaces repelsMorph:event: trackDropZones
tryMeButton wantsDroppedMorph:event: willingToBeDiscarded)
('e-toy support' adaptToWorld: isCandidateForAutomaticViewing
isTileEditor localeChanged objectViewed
replaceReferencesToSlot:inPlayer:with:)
('event handling' handlesMouseOver: handlesMouseOverDragging:
mouseEnter: mouseEnterDragging: mouseLeave: mouseLeaveDragging:)
('frequency' setFrequencyTo: typeInFrequency)
('initialization' defaultBorderWidth defaultColor initialize phrase:
playerScripted: scriptContainer setDefaultBorderCharacteristics
setFillStyle setMorph: setMorph:scriptName: updateHeader
updateToPlayer:)
('layout')
('menu' autoFitOnOff autoFitString fixLayout)
('menus' addCustomMenuItems:hand: wantsHaloHandleWithSelector:inHalo:)
('other' addParameter allTileRowStructures becomeTextuallyCoded
ceaseHavingAParameter codeString extent: handUserButtonDownTile
handUserButtonUpTile handUserParameterTile handUserRandomTile
handUserTileForSelf hasScriptInvoking:ofPlayer:
hasScriptReferencing:ofPlayer: hibernate insertUniversalTiles
insertUniversalTilesForClass:selector: isEmpty isTextuallyCoded
methodNodeMorph methodString modernize offerScriptorMenu
openScriptedObjectsViewer recompileScript recreateScript
reinsertSavedTiles: renameScript renameScriptTo: revealPlayer
revertScriptVersion setScriptNameTo: setTimeStamp showArrows
storeCodeOn:indent: tearOfButtonToFireScript tileRows timeStamp
toggleWhetherShowingTiles unhibernate userScriptObject)
('save & revert' revertToTileVersion saveScriptVersion savedTileVersionsCount)
('scripting' bringUpToDate isTileScriptingElement)
('submorphs-accessing')
('submorphs-add/remove' dismissViaHalo)
('testing' changeParameterTypeTo: parameterVariableObject
setParameterType: stepTime typeForParameter)
('textually-coded scripts' showingMethodPane showingMethodPane:)
('tiles from method' fromExistingMethod:forPlayer:)
('objects from disk' fixUponLoad:seg:)
('private' addNewRow insertTileRow:after: removeEmptyRows
rowInsertionIndexFor: scriptEdited)
('*customevents-other' explainStatusAlternatives)
('*customevents-buttons' actuallyDestroyScript)
('menu commands' nameForParameter)
!

Project removeSelector:
#exportSegmentWithCatagories:classes:fileName:directory:!


Object removeSelector: #fixUponLoad:refStream:!