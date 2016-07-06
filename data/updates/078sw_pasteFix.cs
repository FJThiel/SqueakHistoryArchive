'From Squeak 2.0 of May 22, 1998 on 31 May 1998 at 12:46:31 am'!"Change Set:		sw_pasteFixDate:			31 May 1998Author:			Scott WallaceFixes bug in paste-morph feature that dropped user into debugger when trying to paste an object that had been previously named."!!Morph methodsFor: 'apr98 additions' stamp: 'sw 5/30/1998 17:47'!usableDuplicate	^ self usableDuplicateIn: self world! !!Morph methodsFor: 'apr98 additions' stamp: 'sw 5/30/1998 17:45'!usableDuplicateIn: aWorld	|  aName usedNames newPlayer newMorph |	newMorph _ self fullCopy.	costumee ifNotNil:		[newPlayer _ costumee duplicatedPlayerForCostume: newMorph.			"nb newPlayer has had his costume set to newMorph in the above-called method"		self isFlexMorph ifTrue: [newMorph renderedMorph costumee: newPlayer]. "???"		newMorph actorState: (costumee actorState shallowCopy initializeFor: newPlayer)].	(aName _ self knownName) == nil ifTrue:		[costumee ~~ nil ifTrue: [aName _ newMorph innocuousName]].  "Force a difference here"	aName ~~ nil ifTrue:		[usedNames _ aWorld allKnownNames copyWith: aName.		newMorph setNameTo: (Utilities keyLike: aName satisfying: [:f | (usedNames includes: f) not])].	newMorph justDuplicatedFrom: self.	newMorph removeProperty: #partsDonor.	newMorph privateOwner: nil.	(newPlayer ~~ nil and: [newMorph renderedMorph eventHandler ~~ nil]) ifTrue:		[newPlayer assureEventHandlerRepresentsStatus].	newPlayer ifNotNil: [aWorld flushPlayerListCache].	^ newMorph! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/30/1998 17:46'!duplicateMorph	| newMorph |	newMorph _ argument usableDuplicateIn: self world.	self grabMorphFromMenu: newMorph.	newMorph costumee ifNotNil: [newMorph costumee startRunning].	" -- End of presently active code -- "	self flag: #noteToDan.  "The following code was formerly in duplicateMorph, and may need to be reincorporated somewhere:	oldModel _ argument findA: MorphicModel.	oldModel ifNotNil:		[oldModel model duplicate: (new findA: MorphicModel) from: oldModel]."	self flag: #noteToTed.  "the following corrsponds to the hook you had in for getting script tiles straightened out:	newMorph justDuplicatedFrom: argument.		We depend on nameInModel working, and hand having grabbed already (old tck note)"! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/30/1998 17:46'!objectToPaste	"It may need to be sent #startRunning by the client"	^ PasteBuffer usableDuplicateIn: self world! !!HandMorph methodsFor: 'meta menu' stamp: 'sw 5/30/1998 17:45'!pasteMorph	| aPastee |	PasteBuffer ifNil: [^ self inform: 'Nothing to paste.'].	self attachMorph: (aPastee _ PasteBuffer usableDuplicateIn: self world).	aPastee costumee ifNotNil: [aPastee costumee startRunning]! !!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'sw 5/30/1998 17:46'!rootForGrabOf: aMorph	"If open to drag-n-drop, allow submorph to be extracted. If parts bin, copy the submorph."	| root |	root _ aMorph.	[root = self] whileFalse:		[root owner == self ifTrue:			[self isPartsBin				ifTrue:					[^ root usableDuplicateIn: self world].			self openToDragNDrop					ifTrue: [^ root]].		root _ root owner].	^ super rootForGrabOf: aMorph! !