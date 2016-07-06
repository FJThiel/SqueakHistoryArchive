'From Squeak3.3alpha of 15 February 2002 [latest update: #4818] on 3 May 2002 at 4:02:23 pm'!"Change Set:		Module-removes-tkDate:			3 May 2002Author:			Ted KaehlerMakes it easier to remove all classes from a Module, prior to destroying it.  Likewise for all submodules.Allow an error to look up the stack for a certain method selector."!!ContextPart methodsFor: 'accessing' stamp: 'tk 5/3/2002 15:57'!deepWithinSelector:  aSelector	"Return the nearest context above me that is running aSelector.  nil if none"	| ctx |	ctx _ self.	[(ctx _ ctx sender) == nil] whileFalse:			[ctx selector == aSelector ifTrue: [^ ctx]].	^ nil! !!Module methodsFor: 'module composition' stamp: 'tk 5/2/2002 14:47'!allSubmodulesDo: aBlock	"Traverse my subtree.  May send a module to the block more than once."	^ self neighborModuleRefs do: [:ref | 		ref refersToSubmodule & ref isModuleResolved ifTrue: [			ref module allSubmodulesDo: aBlock.			aBlock value: ref module]]! !!Module methodsFor: 'compatibility' stamp: 'tk 5/2/2002 14:51'!removeAllMyClassesFromSystem	"Delete all of my classes from the system.  Log the removal neither to the current change set nor to the changes log"	self allClassesDo: [:aClass | self removeClassFromSystem: aClass logged: false].! !