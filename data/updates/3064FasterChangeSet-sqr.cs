'From Squeak2.9alpha of 13 June 2000 [latest update: #2988] on 26 November 2000 at 9:52:13 am'!"Change Set:		252FasterChangeSet-sqrDate:			14 November 2000Author:			Andres ValloudGreatly improves class order determination for changeset fileout"!ChangeSet class	instanceVariableNames: ''!!ChangeSet class methodsFor: 'fileIn/Out' stamp: 'SqR 11/14/2000 11:36'!doWeFileOut: aClass given: aSet cache: cache	| aClassAllSuperclasses aClassSoleInstanceAllSuperclasses |	aClassAllSuperclasses _ cache at: aClass		ifAbsent: [cache at: aClass put: aClass allSuperclasses asArray].	(aSet includesAnyOf: aClassAllSuperclasses) ifTrue: [^false].	aClass isMeta ifFalse: [^true].	(aSet includes: aClass soleInstance) ifTrue: [^false].	aClassSoleInstanceAllSuperclasses _ cache at: aClass soleInstance		ifAbsent: [cache at: aClass soleInstance put: aClass soleInstance allSuperclasses asArray].	(aSet includesAnyOf: aClassSoleInstanceAllSuperclasses) ifTrue: [^false].	^true! !!ChangeSet class methodsFor: 'fileIn/Out' stamp: 'SqR 11/14/2000 11:37'!superclassOrder: classes	"Arrange the classes in the collection, classes, in superclass order so the 	classes can be properly filed in. Do it in sets instead of ordered collections.	SqR 4/12/2000 22:04"	| all list aClass inclusionSet aClassIndex cache |	list _ classes copy. "list is indexable"	inclusionSet _ list asSet. cache _ Dictionary new.	all _ OrderedCollection new: list size.	list size timesRepeat:		[			aClassIndex _ list findFirst: [:one | one isNil not and: 				[self doWeFileOut: one given: inclusionSet cache: cache]].			aClass _ list at: aClassIndex.			all addLast: aClass.			inclusionSet remove: aClass.			list at: aClassIndex put: nil		].	^all! !