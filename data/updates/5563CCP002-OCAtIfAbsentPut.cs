'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5548] on 16 November 2003 at 4:38:13 pm'!"Change Set:		OCAtIfAbsentPutDate:			14 November 2003Author:			Marcus DenkerThis changeset adds at:ifAbsentPut: to OrderedCollectionThe comment:Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)This is used by the ClosureCompiler (IRBuilder)"!!OrderedCollection methodsFor: 'adding' stamp: 'ajh 5/22/2003 12:03'!at: index ifAbsentPut: block	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"	| v |	index <= self size ifTrue: [		^ (v _ self at: index)			ifNotNil: [v]			ifNil: [self at: index put: block value]	].	[self size < index] whileTrue: [self add: nil].	^ self at: index put: block value! !