'From Squeak3.7beta of ''1 April 2004'' [latest update: #5969] on 3 July 2004 at 5:57:01 pm'!Set subclass: #KeyedSet	instanceVariableNames: 'keyBlock'	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Unordered'!!KeyedSet commentStamp: '<historical>' prior: 0!Like Set except a key of every element is used for hashing and searching instead of the element itself.  keyBlock gets the key of an element.!KeyedSet subclass: #KeyedIdentitySet	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Collections-Unordered'!!KeyedSet methodsFor: 'adding' stamp: 'ajh 12/4/2001 05:35'!add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: (keyBlock value: newObject).
	(array at: index) ifNotNil: [^ self errorKeyAlreadyExists: (array at: index)].
	self atNewIndex: index put: newObject.
	^ newObject! !!KeyedSet methodsFor: 'adding' stamp: 'ajh 12/4/2001 05:27'!addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements"

	(aCollection respondsTo: #associationsDo:)
		ifTrue: [aCollection associationsDo: [:ass | self add: ass]]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection! !!KeyedSet methodsFor: 'adding' stamp: 'ajh 6/3/2002 10:11'!member: newObject
	"Include newObject as one of the receiver's elements, if already exists just return it"

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index _ self findElementOrNil: (keyBlock value: newObject).
	(array at: index) ifNotNil: [^ array at: index].
	self atNewIndex: index put: newObject.
	^ newObject! !!KeyedSet methodsFor: 'private' stamp: 'ajh 3/29/2001 19:04'!errorKeyNotFound

	self error: 'key not found'! !!KeyedSet methodsFor: 'private' stamp: 'ajh 9/5/2000 03:44'!fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"
	| length oldIndex newIndex element |
	oldIndex _ index.
	length _ array size.
	[oldIndex = length
			ifTrue: [oldIndex _  1]
			ifFalse: [oldIndex _  oldIndex + 1].
	(element _ self keyAt: oldIndex) == nil]
		whileFalse: 
			[newIndex _ self findElementOrNil: (keyBlock value: element).
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]! !!KeyedSet methodsFor: 'private' stamp: 'ajh 9/7/2001 11:56'!init: n
	super init: n.
	keyBlock _ [:element | element key].
! !!KeyedSet methodsFor: 'private' stamp: 'ajh 9/5/2000 03:46'!noCheckAdd: anObject
	array at: (self findElementOrNil: (keyBlock value: anObject)) put: anObject.
	tally _ tally + 1! !!KeyedSet methodsFor: 'private' stamp: 'ajh 12/13/2001 00:17'!rehash
	| newSelf |
	newSelf _ self species new: self size.
	newSelf keyBlock: keyBlock.
	self do: [:each | newSelf noCheckAdd: each].
	array _ newSelf array! !!KeyedSet methodsFor: 'private' stamp: 'ajh 9/5/2000 03:55'!scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 9/5/2000 03:57'!at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound]! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 10/6/2000 20:28'!at: key ifAbsent: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| obj |
	obj _ array at: (self findElementOrNil: key).
	obj ifNil: [^ aBlock value].
	^ obj! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 12/10/2000 15:42'!at: key ifAbsentPut: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	add the result of evaluating aBlock to self"

	^ self at: key ifAbsent: [self add: aBlock value]! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 9/5/2000 03:58'!at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v _ self at: key ifAbsent: [^ nil].
	^ aBlock value: v
! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 7/3/2004 17:55'!keys

	| keys |
	keys _ Set new.
	self keysDo: [:key | keys add: key].
	^ keys! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 7/3/2004 17:54'!keysDo: block

	self do: [:item | block value: (keyBlock value: item)]! !!KeyedSet methodsFor: 'accessing' stamp: 'ajh 5/11/2002 13:28'!keysSorted

	| keys |
	keys _ SortedCollection new.
	self do: [:item | keys add: (keyBlock value: item)].
	^ keys! !!KeyedSet methodsFor: 'initialize' stamp: 'ajh 9/5/2000 03:36'!keyBlock: oneArgBlock
	"When evaluated return the key of the argument which will be an element of the set"

	keyBlock _ oneArgBlock! !!KeyedSet methodsFor: 'removing' stamp: 'ajh 9/5/2000 03:47'!remove: oldObject ifAbsent: aBlock

	| index |
	index _ self findElementOrNil: (keyBlock value: oldObject).
	(array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject! !!KeyedSet methodsFor: 'removing' stamp: 'ajh 3/29/2001 19:03'!removeKey: key 

	^ self removeKey: key ifAbsent: [self errorKeyNotFound]! !!KeyedSet methodsFor: 'removing' stamp: 'ajh 3/29/2001 19:03'!removeKey: key ifAbsent: aBlock

	| index obj |
	index _ self findElementOrNil: key.
	(obj _ array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally _ tally - 1.
	self fixCollisionsFrom: index.
	^ obj! !!KeyedSet methodsFor: 'testing' stamp: 'ajh 9/5/2000 03:45'!includes: anObject 
	^ (array at: (self findElementOrNil: (keyBlock value: anObject))) ~~ nil! !!KeyedSet methodsFor: 'testing' stamp: 'ajh 3/29/2001 23:56'!includesKey: key

	^ (array at: (self findElementOrNil: key)) ~~ nil! !!KeyedSet methodsFor: 'copying' stamp: 'ajh 9/5/2000 03:56'!copy
	^super copy postCopyBlocks! !!KeyedSet methodsFor: 'copying' stamp: 'ajh 9/5/2000 03:56'!postCopyBlocks
	keyBlock _ keyBlock copy.
	"Fix temps in case we're referring to outside stuff"
	keyBlock fixTemps.! !!KeyedIdentitySet methodsFor: 'private' stamp: 'ajh 12/10/2000 20:24'!scanFor: anObject
	"Same as super except change = to ==, and hash to identityHash"

	| element start finish |
	start _ (anObject identityHash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [(keyBlock value: element) == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [(keyBlock value: element) == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !!KeyedSet class methodsFor: 'instance creation' stamp: 'ajh 10/23/2000 23:16'!keyBlock: oneArgBlock
	"Create a new KeySet whose way to access an element's key is by executing oneArgBlock on the element"

	^ self new keyBlock: oneArgBlock! !