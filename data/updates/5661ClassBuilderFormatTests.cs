'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5623] on 4 January 2004 at 9:06:39 pm'!"Change Set:		ClassBuilderFormatTestsDate:			4 January 2004Author:			stephane ducassejust renamed the category to follow Test-Ca.this is this version that should be harvested"!TestCase subclass: #ClassBuilderFormatTests	instanceVariableNames: 'baseClass subClass'	classVariableNames: ''	poolDictionaries: ''	category: 'Tests-Kernel-Classes'!!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:21'!testByteVariableSubclass	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object variableByteSubclass: self baseClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self assert: (subClass isBytes).	subClass removeFromSystem.	"pointer classes"	self should:[self makeIVarsSubclassOf: baseClass] raise: Error.	self should:[self makeVariableSubclassOf: baseClass] raise: Error.	self should:[self makeWeakSubclassOf: baseClass] raise: Error.	"bit classes"	self shouldnt:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self assert: (subClass isBytes).	subClass removeFromSystem.	self should:[self makeWordVariableSubclassOf: baseClass] raise: Error.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:20'!testSubclass	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object subclass: self baseClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self deny: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"pointer classes"	self shouldnt:[self makeIVarsSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self deny: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeVariableSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert:(subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeWeakSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert:(subClass isVariable).	self assert:(subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"bit classes"	self shouldnt:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self assert: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeWordVariableSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:21'!testSubclassWithInstanceVariables	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object subclass: self baseClassName		instanceVariableNames: 'var1 var2'		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self deny: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"pointer classes"	self shouldnt:[self makeIVarsSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self deny: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeVariableSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeWeakSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self assert: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"bit classes"	self should:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self should:[self makeWordVariableSubclassOf: baseClass] raise: Error.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:20'!testVariableSubclass	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object variableSubclass: self baseClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	"pointer classes"	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeIVarsSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeVariableSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeWeakSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self assert: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"bit classes"	self should:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self should:[self makeWordVariableSubclassOf: baseClass] raise: Error.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:20'!testWeakSubclass	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object weakSubclass: self baseClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	"pointer classes"	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self assert: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeIVarsSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self assert: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeVariableSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	self shouldnt:[self makeWeakSubclassOf: baseClass] raise: Error.	self assert: (subClass isPointers).	self assert: (subClass isVariable).	self assert: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"bit classes"	self should:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self should:[self makeWordVariableSubclassOf: baseClass] raise: Error.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'testing' stamp: 'ar 1/4/2004 20:20'!testWordVariableSubclass	"Ensure that the invariants for superclass/subclass format are preserved"	baseClass := Object variableWordSubclass: self baseClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.	[	self shouldnt:[self makeNormalSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	"pointer classes"	self should:[self makeIVarsSubclassOf: baseClass] raise: Error.	self should:[self makeVariableSubclassOf: baseClass] raise: Error.	self should:[self makeWeakSubclassOf: baseClass] raise: Error.	"bit classes"	self should:[self makeByteVariableSubclassOf: baseClass] raise: Error.	self shouldnt:[self makeWordVariableSubclassOf: baseClass] raise: Error.	self deny: (subClass isPointers).	self assert: (subClass isVariable).	self deny: (subClass isWeak).	self deny: (subClass isBytes).	subClass removeFromSystem.	] ensure:[self cleanup].! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!baseClassName	^#DummyClassBuilderFormatTestSuperClass! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!cleanup	subClass ifNotNil:[subClass removeFromSystem].	baseClass ifNotNil:[baseClass removeFromSystem].! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!makeByteVariableSubclassOf: aClass	subClass := aClass variableByteSubclass: self subClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!makeIVarsSubclassOf: aClass	subClass := aClass subclass: self subClassName		instanceVariableNames: 'var3 var4'		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!makeNormalSubclassOf: aClass	subClass := aClass subclass: self subClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:15'!makeVariableSubclassOf: aClass	subClass := aClass variableSubclass: self subClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'.! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:16'!makeWeakSubclassOf: aClass	subClass := aClass weakSubclass: self subClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:16'!makeWordVariableSubclassOf: aClass	subClass := aClass variableWordSubclass: self subClassName		instanceVariableNames: ''		classVariableNames: ''		poolDictionaries: ''		category: 'Kernel-Tests-ClassBuilder'! !!ClassBuilderFormatTests methodsFor: 'utilities' stamp: 'ar 1/4/2004 20:16'!subClassName	^#DummyClassBuilderFormatTestSubClass! !Smalltalk removeClassNamed: #AnObsoleteDummyClassBuilderFormatTestSubClass!