'From Squeak3.6beta of ''4 July 2003'' [latest update: #5395] on 19 August 2003 at 12:20:20 am'!"Change Set:		NamedProcessDate:			19 August 2003Author:			Stephen PairI'd like to suggest including Stephen's NamedProcess package into the base image.  It's a tiny change, is really useful when looking at processes and shouldn't affect anyone who doesn't choose to use it.I just took the SAR from SqueakMap, put in proper method categories, and made it into one CS"[Link subclass: #Process	instanceVariableNames: 'suspendedContext priority myList errorHandler name '	classVariableNames: ''	poolDictionaries: ''	category: 'Kernel-Processes'] on: Warning do: [ :warning | warning resume ].!!BlockContext methodsFor: 'scheduling' stamp: 'svp 6/23/2003 10:59'!forkAt: priority named: name
	"Create and schedule a Process running the code in the receiver at the
	given priority and having the given name. Answer the newly created 
	process."

	| forkedProcess |
	forkedProcess := self newProcess.
	forkedProcess priority: priority.
	forkedProcess name: name.
	^ forkedProcess resume! !!BlockContext methodsFor: 'scheduling' stamp: 'svp 6/23/2003 10:59'!forkNamed: aString
	"Create and schedule a Process running the code in the receiver and
	having the given name."

	^ self newProcess name: aString; resume! !!Process methodsFor: 'accessing' stamp: 'svp 12/5/2002 14:42'!name

	^name ifNil: [ self hash asString forceTo: 5 paddingStartWith: $ ]! !!Process methodsFor: 'accessing' stamp: 'svp 12/5/2002 14:42'!name: aString

	name _ aString! !!Process methodsFor: 'printing' stamp: 'svp 12/5/2002 14:45'!browserPrintStringWith: anObject 	| stream |	stream _ WriteStream				on: (String new: 100).	stream nextPut: $(.	priority printOn: stream.	self isSuspended		ifTrue: [stream nextPut: $s].	stream nextPutAll: ') '.	stream nextPutAll: self name.	stream nextPut: $:.	stream space.	stream nextPutAll: anObject asString.	^ stream contents! !