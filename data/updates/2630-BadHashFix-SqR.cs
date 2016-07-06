'From Squeak2.8 of 30 June 2000 [latest update: #2348] on 3 August 2000 at 1:51:27 pm'!"Change Set:		BadHashFixDate:			3 August 2000Author:			Andres Valloud & Luciano NotarfrancescoRemoves Symbol>>hash.This is done since now symbols may be equal to strings and viceversa. When they were equal, though, their hashes were not and that's not good.Fixing this brought up some issues with the Interpreter. When the C code is generated, some methods are inlined. Such inlined methods have local variable names that mask global variable names. There is a check for this (CCodeGenerator>>prepareMethods). The methods dictionary had symbol keys, but it was queried for strings. Since the hashes for equal variable names were different, those overlapping names were never reported as errors by the CCodeGenerator. We changed the names of the local variables by prefixing them with 'local'.Also reimplements String>>hash. The previous implementations could provide only 14 bits of hash, and this was just at its best. It did not spread hash values enough either, so just about 12 bits were being used by all the Strings in the system. The new implementation of hash for Strings provides an excellent hash mentioned by Mark van Gulik, and it is now used in more places through the Collection hierarchy (check ByteArray>>hash and SequenceableCollection>>hash).Also reimplements Fraction>>hash so that it does not answer LargeIntegers. A small method called hashMultiply was added to SmallInteger to keep things clean. A convenient method was also added to Set class to rehash all sets except MethodDictionaries (takes too long and they are not affected by #hash).Finally, do not edit or fileOut this changeset!! It contains hand made postscripts and initialization code that will rehash all Sets and Dictionaries in your system.A quick check to see if this changeset did its work properly is to do the following:	* Try to get the Set class by typing 'Set' in a workspace and then doing a print-it	* Open a workspaceIf that works, you are in good shape. SqR & len, 8/4/2000 14:41"!"Interpreter fixes"!!Interpreter methodsFor: 'contexts' stamp: 'SqR 8/2/2000 16:47'!argumentCountOfBlock: blockPointer	| localArgCount |	localArgCount _ self fetchPointer: BlockArgumentCountIndex							ofObject: blockPointer.	(self isIntegerObject: localArgCount)		ifTrue: [ ^ self integerValueOf: localArgCount ]		ifFalse: [ self primitiveFail. ^0 ].! !!Interpreter methodsFor: 'message sending' stamp: 'SqR 8/2/2000 16:53'!internalBytecodeActivateNewMethod	| methodHeader newContext tempCount localArgCount needsLarge |	self inline: true.	methodHeader _ self headerOf: newMethod.	needsLarge _ methodHeader bitAnd: LargeContextBit.	(needsLarge = 0 and: [freeContexts ~= NilContext])		ifTrue: [newContext _ freeContexts.				freeContexts _ self fetchPointer: 0 ofObject: newContext]		ifFalse: ["Slower call for large contexts or empty free list"				self externalizeIPandSP.				newContext _ self allocateOrRecycleContext: needsLarge.				self internalizeIPandSP].	tempCount _ (methodHeader >> 19) bitAnd: 16r3F.	"Assume: newContext will be recorded as a root if necessary by the	 call to newActiveContext: below, so we can use unchecked stores."	self storePointerUnchecked: SenderIndex	ofObject: newContext		withValue: activeContext.	self storeWord: InstructionPointerIndex	ofObject: newContext		withValue: (self integerObjectOf:			(((LiteralStart + (self literalCountOfHeader: methodHeader)) * 4) + 1)).	self storeWord: StackPointerIndex			ofObject: newContext		withValue: (self integerObjectOf: tempCount).	self storePointerUnchecked: MethodIndex ofObject: newContext		withValue: newMethod.	"Copy the reciever and arguments..."	localArgCount _ argumentCount.	0 to: localArgCount do:		[:i | self storePointerUnchecked: ReceiverIndex+i ofObject: newContext			withValue: (self internalStackValue: localArgCount-i)].	"clear remaining temps to nil in case it has been recycled"	methodHeader _ nilObj.  "methodHeader here used just as faster (register?) temp"	localArgCount+1 to: tempCount do:		[:i | self storePointerUnchecked: ReceiverIndex+i ofObject: newContext			withValue: methodHeader].	self internalPop: localArgCount + 1.	reclaimableContextCount _ reclaimableContextCount + 1.	self internalNewActiveContext: newContext.! !!Interpreter methodsFor: 'message sending' stamp: 'SqR 8/2/2000 16:52'!internalExecuteNewMethod	| localPrimIndex |	self inline: true.	localPrimIndex _ primitiveIndex.	localPrimIndex > 0		ifTrue: [(localPrimIndex > 255 and: [localPrimIndex < 520])				ifTrue: ["Internal return instvars"						localPrimIndex >= 264						ifTrue:						[^ self internalPop: 1 thenPush:								(self fetchPointer: localPrimIndex-264										ofObject: self internalStackTop)]						ifFalse:						["Internal return constants"						localPrimIndex = 256 ifTrue: [^ nil "^ self"].						localPrimIndex = 257 ifTrue: [^ self internalPop: 1 thenPush: trueObj].						localPrimIndex = 258 ifTrue: [^ self internalPop: 1 thenPush: falseObj].						localPrimIndex = 259 ifTrue: [^ self internalPop: 1 thenPush: nilObj].						^ self internalPop: 1 thenPush: (self integerObjectOf: localPrimIndex-261)]]				ifFalse: 	[self externalizeIPandSP.						self primitiveResponse.						self internalizeIPandSP.						successFlag ifTrue: [^ nil]]].	"if not primitive, or primitive failed, activate the method"	self internalActivateNewMethod.	"check for possible interrupts at each real send"	self internalQuickCheckForInterrupts.! !!Interpreter methodsFor: 'method lookup cache' stamp: 'SqR 8/2/2000 16:50'!addToMethodCacheSel: selector class: class method: meth primIndex: localPrimIndex	"Add the given entry to the method cache.	The policy is as follows:		Look for an empty entry anywhere in the reprobe chain.		If found, install the new entry there.		If not found, then install the new entry at the first probe position			and delete the entries in the rest of the reprobe chain.		This has two useful purposes:			If there is active contention over the first slot, the second				or third will likely be free for reentry after ejection.			Also, flushing is good when reprobe chains are getting full."	"ar 3/10/2000: Store class in lkupClass so that a primitive response 	can rewrite the mcache entry. Currently this is only used by	primitiveExternalCall (see comment there) to allow fast failure for 	any external primitive that is not found."	| probe hash |	self inline: false.	lkupClass _ class.	hash _ selector bitXor: class.  "drop low-order zeros from addresses"	0 to: CacheProbeMax-1 do:		[:p | probe _ (hash >> p) bitAnd: MethodCacheMask.		(methodCache at: probe + MethodCacheSelector) = 0 ifTrue:				["Found an empty entry -- use it"				methodCache at: probe + MethodCacheSelector put: selector.				methodCache at: probe + MethodCacheClass put: class.				methodCache at: probe + MethodCacheMethod put: meth.				methodCache at: probe + MethodCachePrim put: localPrimIndex.				^ nil]].	"OK, we failed to find an entry -- install at the first slot..."	probe _ hash bitAnd: MethodCacheMask.  "first probe"	methodCache at: probe + MethodCacheSelector put: selector.	methodCache at: probe + MethodCacheClass put: class.	methodCache at: probe + MethodCacheMethod put: meth.	methodCache at: probe + MethodCachePrim put: localPrimIndex.	"...and zap the following entries"	1 to: CacheProbeMax-1 do:		[:p | probe _ (hash >> p) bitAnd: MethodCacheMask.		methodCache at: probe + MethodCacheSelector put: 0].! !!Interpreter methodsFor: 'method lookup cache' stamp: 'len 8/2/2000 16:39'!rewriteMethodCacheSel: selector class: class primIndex: localPrimIndex 	"Rewrite an existing entry in the method cache with a new primitive   	index."	| probe hash |	self inline: false.	hash _ selector bitXor: class.	0 to: CacheProbeMax - 1 do: 		[:p | 		probe _ hash >> p bitAnd: MethodCacheMask.		((methodCache at: probe + MethodCacheSelector)			= selector and: [(methodCache at: probe + MethodCacheClass)				= class])			ifTrue: 				[methodCache at: probe + MethodCachePrim put: localPrimIndex.				^ nil]]! !"Utility methods"!!SmallInteger methodsFor: 'bit manipulation' stamp: 'SqR 8/3/2000 13:29'!hashMultiply	| low |	low _ self bitAnd: 16383.	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384))			bitAnd: 16r0FFFFFFF! !!Set class methodsFor: 'initialization' stamp: 'SqR 8/3/2000 13:19'!quickRehashAllSets  "Set rehashAllSets"	| insts |	self withAllSubclassesDo:		[:c |			insts _ c allInstances.			(insts isEmpty or: [c = MethodDictionary]) ifFalse:			['Rehashing instances of ' , c name				displayProgressAt: Sensor cursorPoint				from: 1 to: insts size				during: [:bar | 1 to: insts size do: [:x | bar value: x. (insts at: x) rehash]]			]		]! !!Set class methodsFor: 'initialization' stamp: 'SqR 8/3/2000 13:18'!rehashAllSets  "Set rehashAllSets"	| insts |	self withAllSubclassesDo:		[:c |			insts _ c allInstances.			insts isEmpty ifFalse:			['Rehashing instances of ' , c name				displayProgressAt: Sensor cursorPoint				from: 1 to: insts size				during: [:bar | 1 to: insts size do: [:x | bar value: x. (insts at: x) rehash]]			]		]! !"Fraction hash does not answer large integers"!!Fraction methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:33'!hash	"Hash is reimplemented because = is implemented."	^numerator hash bitXor: denominator hash! !Set quickRehashAllSets "No need to rehash MethodDictionaries"!"Better default hash for SequenceableCollection"!!SequenceableCollection methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:39'!hash	| hash |	hash _ self species hash.	1 to: self size do: [:i | hash _ (hash + (self at: i) hash) hashMultiply].	^hash! !Set quickRehashAllSets "No need to rehash MethodDictionaries"!"Remove Symbol hash and String hash. No need to rehash MethodDictionaries"!Symbol removeSelector: #hash. Symbol slowRehash. Set quickRehashAllSets!String removeSelector: #hash. Symbol slowRehash. Set quickRehashAllSets!"Better ByteArray hash"!!ByteArray methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:30'!hash	| hash |	hash _ 0.	1 to: self size do: [:i | hash _ (hash + (self at: i)) hashMultiply].	^hash! !Set quickRehashAllSets "No need to rehash MethodDictionaries"!"Better default hash for Collection"!!Collection methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:36'!hash	"Answer an integer hash value for the receiver such that,	  -- the hash value of an unchanged object is constant over time, and	  -- two equal objects have equal hash values"	| hash |	hash _ self species hash.	self size <= 10 ifTrue:		[self do: [:elem | hash _ hash bitXor: elem hash]].	^hash bitXor: self size hash! !Set quickRehashAllSets "No need to rehash MethodDictionaries"!"Fraction hash does not answer large integers"!!Fraction methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:33'!hash	"Hash is reimplemented because = is implemented."	^numerator hash bitXor: denominator hash! !Set quickRehashAllSets "No need to rehash MethodDictionaries"!TilePadMorph initialize!ScriptingSystem initializeTypeColors!