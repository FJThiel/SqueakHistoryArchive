'From Squeak 2.4b of April 23, 1999 on 2 August 1999 at 4:26:27 pm'!"Change Set:		GlobalRefsFix-arDate:			2 August 1999Author:			Andreas RaabFixes the problem that recompiled classes are not found by using #allCallsOn:. Also recompiles all methods in the system to make sure all old stuff is being fixed."!Object subclass: #ClassBuilder	instanceVariableNames: 'environ classMap associationMap instVarMap progress maxClassIndex currentClassIndex '	classVariableNames: 'QuietMode '	poolDictionaries: ''	category: 'Kernel-Classes'!!ClassBuilder methodsFor: 'initialize' stamp: 'ar 8/2/1999 15:59'!initialize	environ _ Smalltalk.	instVarMap _ IdentityDictionary new.	associationMap _ WeakValueDictionary new.! !!ClassBuilder methodsFor: 'class definition' stamp: 'ar 8/2/1999 16:02'!name: className inEnvironment: env subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category	"Define a new class in the given environment"	| oldClass newClass organization instVars classVars force |	environ _ env.	instVars _ Scanner new scanFieldNames: instVarString.	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].	"Validate the proposed name"	(self validateClassName: className) ifFalse:[^nil].	oldClass _ env at: className ifAbsent:[nil].	oldClass isBehavior 		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"	"Run validation checks so we know that we have a good chance for recompilation"	(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].	(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].	(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].	(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil].	"Create a template for the new class (will return oldClass when there is no change)"	newClass _ self 		newSubclassOf: newSuper 		type: type 		instanceVariables: instVars		from: oldClass.	newClass == nil ifTrue:[^nil]. "Some error"	newClass setName: className.	"Export the new class into the environment.	Note: It is absolutely necessary to remove any outdated association first,	since these are used to indicate the method class and any older class	with sends to super might go nuts over the changed class."	(environ at: className ifAbsent:[nil]) == newClass ifFalse:[		self recordObsoleteAssociation: 			(Smalltalk associationAt: className ifAbsent:[nil]).		environ removeKey: className ifAbsent:[].		environ declare: className from: Undeclared.		environ at: newClass name put: newClass.	]. "So far, so good."	"Install the class variables and pool dictionaries... "	force _ (newClass declare: classVarString) | (newClass sharing: poolString).	"... classify ..."	organization _ environ ifNotNil:[environ organization].	organization classify: newClass name under: category asSymbol.	"... record any new class ..."	oldClass == nil ifTrue:[Smalltalk changes addClass: newClass].	"... and recompile."	newClass _ self recompile: force from: oldClass to: newClass mutate: false.	^newClass! !!ClassBuilder methodsFor: 'class definition' stamp: 'ar 8/2/1999 16:00'!reshapeClass: oldClass toSuper: newSuper	"Reshape the given old class to the new super class"	| fmt newClass newMeta newSuperMeta oldMeta instVars |	oldClass becomeUncompact.	"Compute the new format of the class"	instVars _ instVarMap at: oldClass name ifAbsent:[oldClass instVarNames].	fmt _ self computeFormat: oldClass typeOfClass				instSize: instVars size				forSuper: newSuper				ccIndex: 0."Known to be 0 since we uncompacted oldClass first"	fmt == nil ifTrue:[^nil].	oldClass isMeta ifFalse:["Create a new meta class"		oldMeta _ oldClass class.		newMeta _ oldMeta clone.		newSuperMeta _ newSuper ifNil:[Class] ifNotNil:[newSuper class].		newMeta 			superclass: newSuperMeta			methodDictionary: MethodDictionary new			format: (self computeFormat: oldMeta typeOfClass 							instSize: oldMeta instVarNames size 							forSuper: newSuperMeta							ccIndex: 0);			setInstVarNames: oldMeta instVarNames;			organization: oldMeta organization.	].	newClass _ newMeta == nil		ifTrue:[oldClass clone]		ifFalse:[newMeta adoptInstance: oldClass from: oldMeta].	newClass		superclass: newSuper		methodDictionary: MethodDictionary new		format: fmt;		setInstVarNames: instVars;		organization: oldClass organization.	"Note: This is a possibly dangerous state. As long as the 	meta class is not recompiled we should not send any 	messages to the non-meta class."	oldClass isMeta ifFalse:[		"Keep the old association to the class around in case we need it"		self recordObsoleteAssociation: (Smalltalk associationAt: oldClass name).		"Note: It is deadly important to remove the Smalltalk association name->class		before exporting the class -- sends to super are encoded by the ST association		and exporting the old class by simply doing 			Smalltalk at: newClass name put: newClass		would result in a (possibly fatal) redefinition of the method class.		Unfortunately, exporting newClass might be problem too. Since it is		not yet recompiled sending messages to it during the recompilation		might be fatal. This, however, should not happen as long as the		new class is not referenced from recompilation by means of a message		send -- e.g., the old association is still kept around all direct refs		to the global are still okay. Trouble would start if in the recompilation		something like 'Smalltalk at: #Array' occurs and we were just about		to recompile Array."		environ removeKey: oldClass name ifAbsent:[].		environ at: newClass name put: newClass.		"Recompile the meta class"		oldMeta hasMethods 			ifTrue:[newMeta compileAllFrom: oldMeta].		"Fix up meta class structure"		oldMeta superclass removeSubclass: oldMeta.		newMeta superclass addSubclass: newMeta.		"And record the change so we can fix global refs later"		self recordClass: oldMeta replacedBy: newMeta].	"Recompile the class"	oldClass hasMethods 		ifTrue:[newClass compileAllFrom: oldClass].	"Fix up the class hierarchy"	oldClass superclass removeSubclass: oldClass.	newClass superclass addSubclass: newClass.	"Adopt all the instances of the old class"	oldClass autoMutateInstances		ifTrue:[newClass updateInstancesFrom: oldClass].	"And record the change"	self recordClass: oldClass replacedBy: newClass.	^newClass! !!ClassBuilder methodsFor: 'private' stamp: 'ar 8/2/1999 16:26'!fixGlobalReferences	"Fix all the references to globals which are now outdated.	Care must be taken that we do not accidentally 'fix' dangerous stuff."	| oldClasses newClasses oldRefs newRefs |	instVarMap _ nil.	(classMap contains:[:any| any notNil and:[any isObsolete]])		ifFalse:[^self]. "Great - no references pending"	any _ nil. "Grrrr. Must clean up the temp of the block above"	Smalltalk garbageCollect. "Try to get rid of as many refs as we can"	"Collect the old and the new refs"	oldClasses _ OrderedCollection new.	newClasses _ OrderedCollection new.	classMap keysAndValuesDo:[:new :old|		old == nil ifFalse:[			newClasses add: new.			oldClasses add: old]].	oldClasses isEmpty ifTrue:[^self]. "GC cleaned up the rest"	"Now fix all the known dangerous pointers to old classes by creating	copies of those still needed. Dangerous pointers should come only	from obsolete subclasses (where the superclass must be preserved)."	self fixObsoleteReferencesTo: oldClasses.	"Setup the references for #become.	This includes mapping oldClasses to newClasses as well as any	users of the old associations."	oldRefs _ WriteStream on: Array new.	newRefs _ WriteStream on: Array new.	"The classes first"	oldRefs nextPutAll: oldClasses.	newRefs nextPutAll: newClasses.	"And the remaining used globals"	associationMap keysAndValuesDo:[:name :assoc|		assoc == nil ifFalse:["Still in use"			oldRefs nextPut: assoc.			newRefs nextPut: (Smalltalk associationAt: name).		].	].	oldRefs _ oldRefs contents.	newRefs _ newRefs contents.	"After this has been done fix the remaining references"	progress == nil ifFalse:[progress value: 'Fixing references to globals'].	"Forward all old refs to the new ones"	oldRefs elementsForwardIdentityTo: newRefs.	"Done"! !!ClassBuilder methodsFor: 'private' stamp: 'ar 8/2/1999 16:26'!recordObsoleteAssociation: anAssociation	"Record an obsolete association to a Smalltalk global"	(anAssociation == nil or:[associationMap includesKey: anAssociation key])		ifFalse:[associationMap at: anAssociation key put: anAssociation].! !"Postscript:Recompile all classes to fix eventual references"Utilities informUser:'Fixing all references to globals.This will take a while...' during:[	Smalltalk allClassesDo:[:cls| 		Transcript cr; show:'Recompiling ', cls name,' ...'.		cls compileAll].].!