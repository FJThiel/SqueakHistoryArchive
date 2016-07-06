'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 10:58:53 pm'!
SequenceableCollection subclass: #ReindexedCollection
	instanceVariableNames: 'sequence interval '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'NumericCollectionsApp'!

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence from: start 
	"Create a reindexedCollection on aSequence from start to the end of 
	aSequence "
	^ self
		on: aSequence
		from: start
		to: aSequence size
		by: 1!
]style[(4 9 7 5 3 81 15 9 9 5 7 9 13)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence from: start by: step 
	"Create a reindexedCollection on aSequence start to the end of aSequence 
	if step is positive, else  
	from start to the beginning of the sequence if step is negative."
	^ self
		on: aSequence
		from: start
		to: (step > 0
				ifTrue: [aSequence size]
				ifFalse: [1])
		by: step!
]style[(4 9 7 5 5 4 3 169 15 9 9 5 8 4 18 9 31 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence from: start to: stop 
	"Create a reindexedCollection on aSequence from start to stop by 1 (or -1 
	if start is greater than  
	stop)"
	^ self
		on: aSequence
		from: start
		to: stop
		by: (start <= stop
				ifTrue: [1]
				ifFalse: [-1])!
]style[(4 9 7 5 5 4 3 110 15 9 9 5 7 4 8 5 4 4 35)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence from: start to: stop by: step 
	"Create a reindexedCollection on aSequence from start to stop by step"
	^ self new
		initialize: aSequence
		from: start
		to: stop
		by: step!
]style[(4 9 7 5 5 4 5 4 3 70 27 9 9 5 7 4 7 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence to: stop 
	"Create a reindexedCollection on aSequence from 1 to stop by 1"
	^ self
		on: aSequence
		from: 1
		to: stop
		by: 1!
]style[(4 9 5 4 3 63 15 9 17 4 8)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence to: stop by: step 
	"Create a reindexedCollection on aSequence from 1 to stop (if step is 
	positive) or the end to stop (if  
	step is negative). Note: if step is not 1 or -1, there is a chance that the 
	index specified by stop may  
	not be in the interval."
	^ self
		on: aSequence
		from: (step > 0
				ifTrue: [1]
				ifFalse: [aSequence size])
		to: stop
		by: step!
]style[(4 9 5 4 5 4 3 241 15 9 10 4 35 9 14 4 7 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection class methodsFor: 'instance creation' stamp: 'TAG 10/13/1998 22:56'!
on: aSequence with: anInterval 
	"Create a reindexedCollection on aSequence"
	^ self new initialize: aSequence with: anInterval!
]style[(4 9 7 10 3 43 25 9 7 10)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !


!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:54'!
from: start 
	"Create aReindexedCollection from the receiver"
	^ self
		from: start
		to: self size
		by: 1!
]style[(6 5 3 47 17 5 24)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:54'!
from: start by: step 
	"Create aReindexedCollection from the receiver"
	^ self
		from: start
		to: (step > 1
				ifTrue: [self size]
				ifFalse: [1])
		by: step!
]style[(6 5 5 4 3 47 17 5 8 4 53 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:54'!
from: start to: stop 
	"Create aReindexedCollection from the receiver"
	^ self
		from: start
		to: stop
		by: (start <= stop
				ifTrue: [1]
				ifFalse: [-1])!
]style[(6 5 5 4 3 47 17 5 7 4 8 5 4 4 35)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:54'!
from: start to: stop by: step 
	"Create aReindexedCollection from the receiver"
	^ ReindexedCollection
		on: self
		from: start
		to: stop
		by: step!
]style[(6 5 5 4 5 4 3 47 43 5 7 4 7 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:55'!
to: stop 
	"Create aReindexedCollection from the receiver"
	^ self
		from: 1
		to: stop
		by: 1!
]style[(4 4 3 47 25 4 8)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!SequenceableCollection methodsFor: 'converting - reindexed' stamp: 'TAG 10/13/1998 22:55'!
to: stop by: step 
	"Create aReindexedCollection from the receiver"
	^ self
		from: (step > 0
				ifTrue: [1]
				ifFalse: [self size])
		to: stop
		by: step!
]style[(4 4 5 4 3 47 18 4 53 4 7 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !


!ReindexedCollection commentStamp: '<historical>' prior: 0!
ReindexedCollection is a wrapper around a sequenceable collection that remaps the indices with in linear algorithm.  The elements in the ReindexedCollection are elements in the sequenceable collection at some start to some stop at some step.  See class side examples.

Instance Variables:
	sequence	<SequenceableCollection>	the sequence that will be reindexed.
	interval	<Interval>	the object that describes indicies of interest in the sequence.

!

!ReindexedCollection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:55'!
at: index 
	"Answer the value of an indexable field in the sequence instance 
	variable. "
	^ sequence at: (interval at: index)!
]style[(4 5 3 78 31 5 1)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1! !

!ReindexedCollection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:55'!
at: index put: value 
	"Store the argument value in the indexable field of the sequence 
	instance variable indicated by index.  
	Answer the value that was stored."
	^ sequence at: (interval at: index)
		put: value!
]style[(4 5 6 5 3 142 31 5 9 5)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:55'!
size
	"Answer how many elements the receiver contains."
	^ interval size!
]style[(4 2 49 17)f1b,f1,f1cblue;,f1! !

!ReindexedCollection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:55'!
slide
	"slide by 1"
	self slide: 1!
]style[(5 2 12 15)f1b,f1,f1cblue;,f1! !

!ReindexedCollection methodsFor: 'accessing' stamp: 'TAG 10/13/1998 22:55'!
slide: anIncrement 
	"given an increment, adjust the reindex map by sliding it that far"
	interval _ interval + anIncrement!
]style[(7 11 3 67 24 11)f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection methodsFor: 'adding' stamp: 'TAG 10/13/1998 22:55'!
add: anObject 
	self shouldNotImplement!
]style[(5 8 26)f1b,f1cgreen;b,f1! !

!ReindexedCollection methodsFor: 'initialize' stamp: 'TAG 10/13/1998 22:55'!
initialize: aSequence from: start to: stop by: step 
	sequence _ aSequence.
	interval _ Interval
				from: start
				to: stop
				by: step!
]style[(12 9 7 5 5 4 5 4 14 9 33 5 9 4 9 4)f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection methodsFor: 'initialize' stamp: 'TAG 10/13/1998 22:55'!
initialize: aSequence with: anInterval 
	sequence _ aSequence.
	interval _ anInterval!
]style[(12 9 7 10 14 9 14 10)f1b,f1cgreen;b,f1b,f1cgreen;b,f1,f1cblue;,f1,f1cblue;! !

!ReindexedCollection methodsFor: 'private' stamp: 'TAG 10/13/1998 22:56'!
species
	"Answer the preferred class for reconstructing the receiver,  
	that is, the sequence."
	^ sequence species!
]style[(7 2 87 20)f1b,f1,f1cblue;,f1! !



