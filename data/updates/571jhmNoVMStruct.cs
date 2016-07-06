'From Squeak 2.3 beta of Nov 25, 1998 on 13 January 1999 at 12:28:19 pm'!"Change Set:		jhmNoVMStructDate:			12 January 1999Author:			John MaloneyUndo the experimental modification of the Smalltalk-to-C translatorput most VM global variables in a struct. This caused difficulties forthe Jitter and resulted in worse code on Intel machines.Also updates the 'readme' file and adds code to ensure that the serialports are closed when exiting Squeak."!!CCodeGenerator methodsFor: 'C code generator' stamp: 'ikp 12/7/97 20:54'!emitCCodeOn: aStream doInlining: inlineFlag doAssertions: assertionFlag	"Emit C code for all methods in the code base onto the given stream. All inlined method calls should already have been expanded."	| verbose |	"method preparation"	verbose _ false.	self prepareMethods.	verbose ifTrue: [		self printUnboundCallWarnings.		self printUnboundVariableReferenceWarnings.		Transcript cr.	].	assertionFlag ifFalse: [ self removeAssertions ].	self doInlining: inlineFlag.	"code generation"	methods _ methods asSortedCollection: [ :m1 :m2 | m1 selector < m2 selector ].	self emitCHeaderOn: aStream.	self emitCVariablesOn: aStream.	self emitCFunctionPrototypesOn: aStream.'Writing Translated Code...'displayProgressAt: Sensor cursorPointfrom: 0 to: methods sizeduring: [:bar |	methods doWithIndex: [ :m :i | bar value: i.		m emitCCodeOn: aStream generator: self.]].! !!CCodeGenerator methodsFor: 'C code generator'!emitCVariablesOn: aStream	"Store the global variable declarations on the given stream."	aStream nextPutAll: '/*** Variables ***/'; cr.	variables asSortedCollection do: [ :var |		(variableDeclarations includesKey: var) ifTrue: [			aStream nextPutAll: (variableDeclarations at: var), ';'; cr.		] ifFalse: [			"default variable declaration"			aStream nextPutAll: 'int ', var, ';'; cr.		].	].	aStream cr.! !!ObjectMemory methodsFor: 'gc -- compaction' stamp: 'jm 1/13/1999 10:55'!fwdTableInit: blkSize	"Set the limits for a table of two- or three-word forwarding blocks above the last used oop. The pointer fwdTableNext moves up to fwdTableLast. Used for compaction of memory and become-ing objects. Returns the number of forwarding blocks available."	self inline: false.	"set endOfMemory to just after a minimum-sized free block"	self setSizeOfFree: freeBlock to: BaseHeaderSize.	endOfMemory _ freeBlock + BaseHeaderSize.	"make a fake free chunk at endOfMemory for use as a sentinal in memory scans"	self setSizeOfFree: endOfMemory to: BaseHeaderSize.	"use all memory free between freeBlock and memoryLimit for forwarding table"	"Note: Forward blocks must be quadword aligned."	fwdTableNext _ (endOfMemory + BaseHeaderSize + 7) bitAnd: 16rFFFFFFF8.	fwdTableLast _ memoryLimit - blkSize.  "last forwarding table entry"	"return the number of forwarding blocks available"	^ (fwdTableLast - fwdTableNext) // blkSize  "round down"! !!ObjectMemory methodsFor: 'become' stamp: 'jm 1/13/1999 10:56'!prepareForwardingTableForBecoming: array1 with: array2 twoWay: twoWayFlag	"Ensure that there are enough forwarding blocks to accomodate this become, then prepare forwarding blocks for the pointer swap. Return true if successful."	"Details: Doing a GC might generate enough space for forwarding blocks if we're short. However, this is an uncommon enough case that it is better handled by primitive fail code at the Smalltalk level."	| entriesNeeded entriesAvailable fieldOffset oop1 oop2 fwdBlock fwdBlkSize |	entriesNeeded _ (self lastPointerOf: array1) // 4.  "need enough entries for all oops"	twoWayFlag		ifTrue: ["Double the number of blocks for two-way become"				entriesNeeded _ entriesNeeded * 2.				fwdBlkSize _ 8  "Note: Forward blocks must be quadword aligned."]		ifFalse: ["One-way become needs backPointers in fwd blocks."				fwdBlkSize _ 16  "Note: Forward blocks must be quadword aligned."].	entriesAvailable _ self fwdTableInit: fwdBlkSize.	entriesAvailable < entriesNeeded ifTrue: 		[self initializeMemoryFirstFree: freeBlock.  "re-initialize the free block"		^ false].	fieldOffset _ self lastPointerOf: array1.	[fieldOffset >= BaseHeaderSize] whileTrue: 		[oop1 _ self longAt: array1 + fieldOffset.		oop2 _ self longAt: array2 + fieldOffset.		fwdBlock _ self fwdBlockGet: fwdBlkSize.		self initForwardBlock: fwdBlock mapping: oop1 to: oop2 withBackPtr: twoWayFlag not.		twoWayFlag ifTrue:			["Second block maps oop2 back to oop1 for two-way become"			fwdBlock _ self fwdBlockGet: fwdBlkSize.			self initForwardBlock: fwdBlock mapping: oop2 to: oop1 withBackPtr: twoWayFlag not].		fieldOffset _ fieldOffset - 4].	^ true! !CCodeGenerator removeSelector: #useStructNamed:forAllGlobalsExcept:!