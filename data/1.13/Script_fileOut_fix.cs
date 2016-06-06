"HyperSqueak Buttons whose methods had not been edited lost their
button-ness when saved on the disk and brought back in.  This fixes it.  It
also ensures that all of an object's scripts are saved at fileOut.
--Ted"!


!Obj methodsFor: 'scripts'!
assureWorkingsReflectScripts
	"Exploratory!!  Enumerate the selectors belonging to the receiver,
and then make sure that the receiver's workings hold objects to represent
them.  6/17/96 sw
	6/22/96 sw: remove any existing script that is no longer
represented by a method
	7/13/96 sw: use newUserInstance"

	| sels itsName scriptObject existingScripts |
	(self class == self class officialClass) ifTrue: [^ self].
		"Methods of officialCLasses do not appear in workings"
	sels _ self class selectors array copyWithoutAll: #(DoIt DoItIn:).
	sels _ sels copyWithout: nil.
	existingScripts  _ self workingsBackToFront select:
		[:anObj | anObj isKindOf: Script] thenCollect: [:anObj |
anObj name].

	sels do:
		[:aSel | (self workingsDictionary includesKey: (itsName _
aSel asString))
			ifTrue:
				[existingScripts remove: itsName]
			ifFalse:
				[scriptObject _ Script newUserInstance.
				self addToWorkings: scriptObject atKey:
itsName.
				scriptObject contents: (self class
sourceCodeAt: aSel)]].
	existingScripts do: "Now only has old maids"
		[:aKey | self removeFromWorkings: (self workingsAtKey:
aKey)]! !
!Obj methodsFor: 'disk I/O'!
storeDataOn: byteStream
	"Write myself out on the ReferenceStream.  If I am a sole instance
of a unique User class, use my generic class instead.  Don't write the
parent pointer, objectContainedIn.  8/9/96 tk"
	"See SqueakView.saveToDisk for who to write out a file of objects.
See IncomingObjects.aComment for how to read the file back in again."
	| cntInstVars cntIndexedVars class |

	self assureWorkingsReflectScripts.
	class _ (self ioType = #User) ifTrue: [self class superclass]
				ifFalse: [self class].
	cntInstVars _ self class instSize.
	cntIndexedVars _ self basicSize.
	cntIndexedVars > 0 ifTrue: [
		self error: 'not prepared to read variable Obj instances'].
	byteStream
		beginInstance: class
			"Must be a standard subclass of Obj, i.e. Folder"
		size: cntInstVars "+ cntIndexedVars".
	1 to: cntInstVars do: [:i |
		(#(2 7 9) includes: i) "objectContainedIn, currentCostume,
canvas"
			ifTrue: [i = 9
				ifTrue: [byteStream nextPut: nil] "don't
write the canvas"
				ifFalse: [byteStream nextPut:
					(self pathOrObj: (self instVarAt:
i))]]
			ifFalse: [byteStream nextPut: (self instVarAt: i)]].
	"1 to: cntIndexedVars do:
		[:i | byteStream nextPut: (self basicAt: i)]."
	(self parameters includesKey: #PictureName) ifTrue: [
		IncomingObjects recent animation: #PictureName in: self].
	(self parameters includesKey: #BackgroundPaintingName) ifTrue: [
		IncomingObjects recent animation: #BackgroundPaintingName
in: self].
! !

