'From Squeak3.4 of 1 March 2003 [latest update: #5170] on 4 April 2003 at 6:07:27 pm'!"Change Set:		KCP-0040-Date:			4 April 2003Author:			stephane ducasse and alexandre bergelMove return out of the conditional branches of certain methods of behavior"!!Behavior methodsFor: 'accessing class hierarchy'!allSuperclasses	"Answer an OrderedCollection of the receiver's and the receiver's  	ancestor's superclasses. The first element is the receiver's immediate  	superclass, followed by its superclass; the last element is Object."	| temp |	^ superclass == nil		ifTrue: [ OrderedCollection new]		ifFalse: [temp _ superclass allSuperclasses.			temp addFirst: superclass.			temp]! !!Behavior methodsFor: 'accessing method dictionary'!allSelectors	"Answer a Set of all the message selectors that instances of the receiver  	can understand."	"Point allSelectors"	| temp |	^ superclass == nil		ifTrue: [self selectors]		ifFalse: [temp _ superclass allSelectors.			temp addAll: self selectors.			temp]! !!Behavior methodsFor: 'testing class hierarchy'!kindOfSubclass	"Answer a String that is the keyword that describes the receiver's kind 	of subclass, either a regular subclass, a variableSubclass, a  	variableByteSubclass, a variableWordSubclass, or a weakSubclass."	self isWeak		ifTrue: [^ ' weakSubclass: '].	^ self isVariable		ifTrue: [self isBits				ifTrue: [self isBytes						ifTrue: [ ' variableByteSubclass: ']						ifFalse: [ ' variableWordSubclass: ']]				ifFalse: [ ' variableSubclass: ']]		ifFalse: [ ' subclass: ']! !!Class methodsFor: 'pool variables'!allSharedPools	"Answer a Set of the pools the receiver shares, including those defined  	in the superclasses of the receiver."	| aSet | 	^ superclass == nil		ifTrue: [self sharedPools copy]		ifFalse: [aSet _ superclass allSharedPools.			aSet addAll: self sharedPools.			aSet]! !