'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6643] on 10 April 2005 at 8:49:17 pm'!"Change Set:		BinaryStorageFixesDate:			10 April 2005Author:			Andreas RaabSome last touches to make loading strings from binary files work correctly."!!Class methodsFor: 'fileIn/Out' stamp: 'ar 4/10/2005 20:27'!objectForDataStream: refStrm	| |	"I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."	refStrm insideASegment		ifFalse: ["Normal use"			^ DiskProxy global: self theNonMetaClass name selector: #withClassVersion:				args: {self classVersion}]		ifTrue: ["recording objects to go into an ImageSegment"			self isSystemDefined ifFalse: [^ self].		"do trace Player classes"			(refStrm rootObject includes: self) ifTrue: [^ self].				"is in roots, intensionally write out, ^ self"						"A normal class.  remove it from references.  Do not trace."			refStrm references removeKey: self ifAbsent: []. 	"already there"			^ nil]! !!Class methodsFor: 'fileIn/Out' stamp: 'ar 4/10/2005 20:27'!withClassVersion: aVersion	aVersion = self classVersion ifTrue:[^self].	^self error: 'Invalid class version'! !!DataStream methodsFor: 'write and read' stamp: 'ar 4/10/2005 20:31'!next	"Answer the next object in the stream."	| type selector anObject isARefType pos internalObject |	type _ byteStream next.	type ifNil: [pos _ byteStream position.	"absolute!!!!"		byteStream close.	"clean up"		byteStream position = 0 			ifTrue: [self error: 'The file did not exist in this directory'] 			ifFalse: [self error: 'Unexpected end of object file'].		pos.	"so can see it in debugger"		^ nil].	type = 0 ifTrue: [pos _ byteStream position.	"absolute!!!!"		byteStream close.	"clean up"		self error: 'Expected start of object, but found 0'.		^ nil].	isARefType _ self noteCurrentReference: type.	selector _ #(readNil readTrue readFalse readInteger	"<-4"			readStringOld readSymbol readByteArray		"<-7"			readArray readInstance readReference readBitmap	"<-11"			readClass readUser readFloat readRectangle readShortInst 	"<-16"			readString readWordArray readWordArrayForSegment 	"<-19"			readWordLike readMethod "<-21") at: type.	selector == 0 ifTrue: [pos _ byteStream position.	"absolute!!!!"			byteStream close. 			self error: 'file is more recent than this system'. ^ nil].	anObject _ self perform: selector. "A method that recursively		calls next (readArray, readInstance, objectAt:) must save &		restore the current reference position."	isARefType ifTrue: [self beginReference: anObject].		"After reading the externalObject, internalize it.		 #readReference is a special case. Either:		   (1) We actually have to read the object, recursively calling			   next, which internalizes the object.		   (2) We just read a reference to an object already read and			   thus already interalized.		 Either way, we must not re-internalize the object here."	selector == #readReference ifTrue: [^ anObject].	internalObject _ anObject comeFullyUpOnReload: self.	internalObject == String ifTrue:[		"This is a hack to figure out if we're loading a String class 		that really should be a ByteString. Note that these days this		will no longer be necessary since we use #withClassVersion:		for constructing the global thus using a different classVersion		will perfectly do the trick."		((anObject isKindOf: DiskProxy) 			and:[anObject globalObjectName == #String			and:[anObject constructorSelector == #yourself]]) ifTrue:[				internalObject := ByteString]].	^ self maybeBeginReference: internalObject! !!ImageSegment methodsFor: 'compact classes' stamp: 'ar 4/10/2005 19:55'!cc: ind new: inTheSeg current: inTheImage fake: fakeCls refStrm: smartRefStream	"Sort out all the cases and decide what to do.  Every Fake class is uncompacted before having insts converted.  As the segment is installed, instances of reshaped compact classes will have the wrong class.  Trouble cases:	1) Existing class is compact in the segment and not compact here.  Make that compact, (error if that slot is used), load the segment.  If an class was just filed in, it is an existing class as far as we are concerned.	2) A compact class has a different shape.  We created a Fake class.  Load the segment, with instances in the seg having the Wrong Class!!!!  Find the bad instancees, and copy them over to being the real class.	3) An existing class is not compact in the segment, but is in the image.  Just let the new instance be uncompact.  That is OK, and never reaches this code.	A class that is a root in this segment cannot be compact.  That is not allowed."	(inTheImage == nil) & (fakeCls == nil) ifTrue: ["case 1 and empty slot" 		inTheSeg becomeCompactSimplyAt: ind.  ^ true].		(inTheImage == inTheSeg) & (fakeCls == nil) ifTrue: ["everything matches" 		^ true].	inTheImage ifNil: ["reshaped and is an empty slot"		fakeCls becomeCompactSimplyAt: ind.  ^ true].		"comeFullyUpOnReload: will clean up"	(inTheSeg == String and:[inTheImage == ByteString]) ifTrue:[		"ar 4/10/2005: Workaround after renaming String to ByteString"		^true	].	"Is the image class really the class we are expecting?  inTheSeg came in as a DiskProxy, and was mapped if it was renamed!!"	inTheImage == inTheSeg ifFalse: [		self inform: 'The incoming class ', inTheSeg name, ' wants compact class \location ', ind printString, ', but that is occupied by ', inTheImage name, '.  \This file cannot be read into this system.  The author of the file \should make the class uncompact and create the file again.' withCRs.		^ false].	"Instances of fakeCls think they are compact, and thus will say they are instances of the class inTheImage, which is a different shape.  Just allow this to happen.  Collect them and remap them as soon as the segment is installed."	^ true! !!SmartRefStream methodsFor: 'conversion' stamp: 'ar 4/10/2005 15:44'!abstractStringx0	^ String! !!SmartRefStream methodsFor: 'import image segment' stamp: 'ar 4/10/2005 20:42'!mapClass: newClass origName: originalName	"See if instances changed shape.  If so, make a fake class for the old shape and return it.  Remember the original class name."	| newName oldInstVars fakeClass |	newClass isMeta ifTrue: [^ newClass].	newName _ newClass name.	(steady includes: newClass) & (newName == originalName) ifTrue: [^ newClass].		"instances in the segment have the right shape"	oldInstVars _ structures at: originalName ifAbsent: [			self error: 'class is not in structures list'].	"Missing in object file"	(newName == #ByteString and:[originalName == #String]) ifTrue:[^newClass].	fakeClass _ Object subclass: ('Fake37', originalName) asSymbol		instanceVariableNames: oldInstVars allButFirst		classVariableNames: ''		poolDictionaries: ''		category: 'Obsolete'.	ChangeSet current removeClassChanges: fakeClass name.	"reduce clutter"	^ fakeClass! !