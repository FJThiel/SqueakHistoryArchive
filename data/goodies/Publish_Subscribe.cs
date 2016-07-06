"      NAME	Publish-Subscribe       AUTHOR	peter.smet@flinders.edu.au (PJS)       URL	(none)       FUNCTION	Event system       KEYWORDS	trigger events       ST-VERSIONS	Squeak       PREREQUISITES	(none)       CONFLICTS	(none known)       DISTRIBUTION	world       VERSION	0.7       DATE	20-Jun-99SUMMARYevent system with weak references				PJS"!
"      NAME	Publish-Subscribe       AUTHOR	peter.smet@flinders.edu.au
       URL	http://       FUNCTION	Event Messaging System
       ST-VERSIONS	Squeak       PREREQUISITES	(none)
       CONFLICTS	(none known)       DISTRIBUTION	world       VERSION	
       DATE	20-Jun-99SUMMARYPostOffice is an event dispatching system. 
Eg. when: #thisEvent send: #aSelector to: aSubscriber.
'default' is a class instance variable that represents the sole instance of PostOffice. ie PostOffice is a Singleton. PostOffice should be transparent - dependencies with events can
be set up as in the test examples, using the methods in Object.
In essence, the entire class is private.
The only class side method of importance is default - all other methods are for testing and are discardable. 
PostOffice uses weak references to handle all publishers of, and subscribers to, events. No explicit removal is required.
				PJS"!

'From Squeak 2.4b of April 23, 1999 on 21 June 1999 at 5:13:31 am'!
Object subclass: #PostOffice
	instanceVariableNames: 'publishers accessLock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Publish-Subscribe'!
PostOffice class
	instanceVariableNames: 'default testProcess '!
Object subclass: #RecursionLock
	instanceVariableNames: 'mutex owner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Semaphore-Recursive'!

!Object methodsFor: 'dependents access' stamp: 'pjs 6/14/1999 17:10'!
subscribers
	"Answer a collection of objects that are 'dependent' on the receiver;  
	 that is, all objects that should be notified if the receiver changes."

	^ self postOffice subscribersTo: self ! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 2/26/1999 16:23'!
postOffice
	^PostOffice default! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 16:23'!
trigger: eventSymbol 
	self trigger: eventSymbol withAll: #()! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 16:24'!
trigger: eventSymbol with: argument 
	self trigger: eventSymbol withAll: (Array with: argument)! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 16:24'!
trigger: eventSymbol withAll: argumentArray 
	self postOffice incoming: (Message selector: eventSymbol arguments: argumentArray)
		from: self! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/16/1999 13:55'!
unsubscribe
	self postOffice removeSubscriber: self! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/16/1999 13:56'!
unsubscribe: eventSymbol
	self postOffice removeSubscriber: self forEvent: eventSymbol! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 16:25'!
when: eventSymbol send: aSelector to: anObject 
	self
		when: eventSymbol
		send: aSelector
		to: anObject
		withAll: #()! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 16:25'!
when: eventSymbol send: aSelector to: anObject with: anArgument 
	self
		when: eventSymbol
		send: aSelector
		to: anObject
		withAll: (Array with: anArgument)! !

!Object methodsFor: 'publish-subscribe' stamp: 'pjs 4/27/1999 21:10'!
when: eventSymbol send: aSelector to: anObject withAll: argArray 

	self postOffice
		when: eventSymbol
		send: (Message selector: aSelector arguments: argArray)
		to: anObject
		for: self! !


!Dictionary methodsFor: 'accessing' stamp: 'crl 3/1/1999 13:00'!
associations
	"Answer my associations."

	| associations |

	associations _ OrderedCollection new.
	self keysDo: [:key | associations add: (key -> (self at: key))].
	^associations! !

!Dictionary methodsFor: 'enumerating' stamp: 'crl 2/26/1999 15:53'!
keysAndValuesDo: aBlockClosure
	"Evaluate aBlockClosure with each of my key/value pairs."

	self associationsDo: [:association | aBlockClosure value: association key value: association value]! !


!Message methodsFor: 'Publish-Subscribe' stamp: 'pjs 4/27/1999 12:25'!
arguments: anArray
	"Answer the arguments of the receiver."

	args := anArray! !

!Message methodsFor: 'Publish-Subscribe' stamp: 'pjs 4/27/1999 21:34'!
equals: aMessage 
   | same |
   same := self selector = aMessage selector.
   ^same and: [self arguments = aMessage arguments]! !

!Message methodsFor: 'Publish-Subscribe' stamp: 'pjs 4/29/1999 10:25'!
passArgumentsTo: aMessage

	self arguments isEmpty ifTrue: [^self].
     self arguments size ~= aMessage arguments size ifTrue: [^ self].
	aMessage arguments: self arguments! !


!PostOffice commentStamp: 'pjs 6/21/1999 05:02' prior: 0!
PostOffice is an event dispatching system. 

"default" is a class instance variable that represents the sole
instance of PostOffice. ie PostOffice is a Singleton.
PostOffice should be transparent - dependencies with events can
be set up as in the test examples, using the methods in Object.
In essence, the entire class is private.

The only class side method of importance is default - all other
methods are for testing and are discardable. 
PostOffice uses weak references to handle all publishers of, and
subscribers to, events. No explicit removal is required.
It is vital that PostOffice is synchronized with the garbage
collector. Beware of changing any code inside protected: blocks.
Early explicit ^ returns from blocks wreaks havoc with
semaphores that do not get signalled when they should.

This class uses a recursive Semaphore so that one
incoming message can trigger another etc. In other words, the
PostOffice #incoming: method can invoke itself without getting
deadlocked (see testNested for an example).

To test if things are working correctly evaluate 
PostOffice testAll

Known bugs:
Putting an inspector onto the PostOffice interferes with
garbage collection. It is safer to evaluate
PostOffice default publishers keys   
or
PostOffice default publishers size

User interupts can leave Semaphores in inconsistent states.
Hopefully this will be fixed when Squeak gets an official
exception handling framework.

Please report any improvements, comments, bugs etc to:
peter.smet@flinders.edu.au



!

!PostOffice reorganize!
('helper' at: at:at: at:at:at: protected: subscribersDo:)
('finalization' finalizeValues)
('query' publishers subscribersTo:)
('removing' removeSubscriber: removeSubscriber:forEvent:)
('initialize-release' initialize)
('messaging' addMessage:to: addPublisher: incoming:from: when:send:to:for:)
!


!PostOffice methodsFor: 'helper' stamp: 'pjs 5/7/1999 16:21'!
at: aPublisher 
	"create key if absent"
	^self publishers at: aPublisher ifAbsent: [self addPublisher: aPublisher]! !

!PostOffice methodsFor: 'helper' stamp: 'pjs 4/29/1999 10:47'!
at: aPublisher at: aSubscriber 
	| dict |
	dict _ self at: aPublisher.
	^ dict at: aSubscriber ifAbsent: [dict at: aSubscriber put: Dictionary new]! !

!PostOffice methodsFor: 'helper' stamp: 'pjs 4/29/1999 10:47'!
at: aPublisher at: aSubscriber at: anEvent 
	| dict |
	dict _ self at: aPublisher at: aSubscriber.
	^ dict at: anEvent ifAbsent: [dict at: anEvent put: Set new]! !

!PostOffice methodsFor: 'helper' stamp: 'pjs 5/30/1999 20:55'!
protected: aBlock 
	"Execute aBlock protected by the accessLock"
	^ accessLock critical: aBlock.
! !

!PostOffice methodsFor: 'helper' stamp: 'pjs 6/11/1999 22:36'!
subscribersDo: aBlock 
	self protected: [publishers do: aBlock]! !

!PostOffice methodsFor: 'finalization' stamp: 'pjs 6/12/1999 23:18'!
finalizeValues
	self
		protected: 
			[
			self publishers do: [:subscribers | subscribers finalizeValues].
			self publishers finalizeValues]! !

!PostOffice methodsFor: 'query' stamp: 'pjs 5/30/1999 00:49'!
publishers
	^ publishers! !

!PostOffice methodsFor: 'query' stamp: 'pjs 6/14/1999 10:11'!
subscribersTo: aPublisher 
	^ self protected: [(self publishers at: aPublisher ifAbsent: [Dictionary new]) keys]! !

!PostOffice methodsFor: 'removing' stamp: 'pjs 5/30/1999 01:08'!
removeSubscriber: aSubscriber 
	self subscribersDo: [:subscribers | subscribers removeKey: aSubscriber ifAbsent: []]! !

!PostOffice methodsFor: 'removing' stamp: 'pjs 6/17/1999 06:11'!
removeSubscriber: aSubscriber forEvent: eventSymbol 
	| dict |
	self
		subscribersDo: 
			[:subscribers | 
			dict _ subscribers at: aSubscriber ifAbsent: [Dictionary new].
			dict removeKey: eventSymbol ifAbsent: []]! !

!PostOffice methodsFor: 'initialize-release' stamp: 'pjs 6/17/1999 06:33'!
initialize
	publishers _ WeakKeyDictionary new.
	accessLock _ RecursionLock forMutualExclusion! !

!PostOffice methodsFor: 'messaging' stamp: 'pjs 6/12/1999 21:10'!
addMessage: aMessage to: aSet 
	(aSet contains: [:msg | msg equals: aMessage])
		ifFalse: [aSet add: aMessage]! !

!PostOffice methodsFor: 'messaging' stamp: 'pjs 5/21/1999 17:03'!
addPublisher: anObject 

	^ self publishers at: anObject put: (WeakKeyDictionary new)! !

!PostOffice methodsFor: 'messaging' stamp: 'pjs 6/21/1999 04:13'!
incoming: aMessage from: aPublisher 
	| subscribers msgList |
	self
		protected: 
			[subscribers _ self publishers at: aPublisher ifAbsent: [Dictionary new].
			subscribers keysAndValuesDo: [:sub :msgs | sub
					ifNotNil: 
						[
						msgList _ msgs at: aMessage selector ifAbsent: [#()].
						msgList do: 
							[:msg | 
							aMessage passArgumentsTo: msg.
							msg sentTo: sub]]]]! !

!PostOffice methodsFor: 'messaging' stamp: 'pjs 6/12/1999 21:50'!
when: eventSymbol send: aMessage to: aSubscriber for: aPublisher 
	self
		protected: 
			[
			self addMessage: aMessage to: (self
					at: aPublisher
					at: aSubscriber
					at: eventSymbol).
			]! !


!PostOffice class reorganize!
('initialize-release' initialize)
('testing' reset start stop test1 test3 test4 testAdd testAll testBasicAdd testNested testProcess testRemove testSubs)
('conversion' convertDependents)
('instance creation' default new)
!


!PostOffice class methodsFor: 'initialize-release' stamp: 'pjs 6/21/1999 01:31'!
initialize
	self default.
	! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/1/1999 09:45'!
reset
     WeakArray removeWeakDependent: self default.
     default := nil.
! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/12/1999 22:26'!
start
	testProcess _ [[true]
				whileTrue: 
					[Processor yield.
					self testAdd.
					Processor yield.
					self test4.
					Processor yield.
					self test3.
					Processor yield.
					self testRemove.
					Processor yield.
					self testNested.
                        self testNested.     
					Processor yield]] newProcess.
	testProcess priority: Processor userSchedulingPriority.
	testProcess resume! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 3/7/1999 20:36'!
stop
	testProcess terminate.! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 5/23/1999 22:28'!
test1
| p q |
	p _ Bag new.
	q _ Bag new.
	p
		when: #splotch
		send: #add:
		to: q
		with: 'z'.
	p
		when: #splatch
		send: #remove:
		to: q
		with: 'r'.
	p
		when: #splotch
		send: #add:
		to: q
		with: 'a'.
	p trigger: #splotch.
	p trigger: #splotch with: 'z'.
	p trigger: #splotch withAll: 'abc'.
	p trigger: #splotch with: 'r'.
	p trigger: #splotch.
	q trigger: #splotch.
	p trigger: #splatch with: 'z'.
	q trigger: #unknown withAll: 'jeremy'.
	q unsubscribe: #addme.
	q unsubscribe! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 5/23/1999 22:31'!
test3
	| a b c s |
	a _ Date today.
	b _ Time now.
	c _ Bag new.
	s
		when: #added
		send: #size
		to: c.
	c add: 66.
	Bag new
		when: #greasemonkey
		send: #asString
		to: b.
	Set new
		when: #greasemonkey
		send: #asString
		to: s.
	b trigger: #greasemonkey.
	s trigger: #added.
	s trigger: #creole.
	c unsubscribe: #mistake.
	c unsubscribe: #added.
	a unsubscribe! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/12/1999 22:03'!
test4
	| g |
	Transcript
		when: #transcript
		send: #even
		to: 17.
	7
		when: #wow
		send: #odd
		to: 17.
	500
		timesRepeat: 
			[g _ Bag new.
			g
				when: #pink
				send: #even
				to: 5
				with: 'hat'.
			Bag new
				when: #pink
				send: #even
				to: 5
				with: 'hat'].
	Bag new trigger: #pink.
	g trigger: #pink.
	Bag new trigger: #oh.
	g unsubscribe: #transcript.
	Transcript trigger: #transcript.
	Transcript trigger: #unknown.
	Transcript unsubscribe.
	Transcript unsubscribe: #size.
	Transcript trigger: #transcript with: '56'.
	8 trigger: #wow.
	7 trigger: #wow.
	3 trigger: #hats.
	3 trigger: #hoho withAll: 'dfffffffffffffffffffffffffff'.
	17 unsubscribe.
	17 unsubscribe: #wow.
	8 trigger: #wow.
	7 trigger: #wow! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 5/21/1999 21:23'!
testAdd
	8
		when: #this
		send: #isEmpty
		to: Bag new
		with: #parameter.
	Bag new
		when: #this
		send: #even
		to: 2.

	7
		when: #this
		send: #even
		to: 5
		with: #parameter.
	7
		when: #this
		send: #odd
		to: 2
		with: #parameter.
	7 trigger: #this withAll: 'harry'.
	7 trigger: #that.
	8 trigger: #this.
	Bag new trigger: #nothing! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/12/1999 23:45'!
testAll
	self testBasicAdd.
	self testNested.
	self test3.
	self test4.
	self test1.
	self testAdd.
	self testNested.
	self testRemove.
    self testSubs.! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/12/1999 21:34'!
testBasicAdd

	| z |
	Bag new
		when: #this
		send: #even
		to: 2.
	z _ Bag new.
	z
		when: #this
		send: #even
		to: 2.
	Bag new trigger: #this. 
	z trigger: #this.
	z _ nil.! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 5/28/1999 00:57'!
testNested
	8
		when: #this
		send: #trigger:
		to: 4
         with: #this.

	4 when: #this 
       send: #trigger: 
       to: 2
       with: #this.

     2
		when: #this
		send: #even
		to: 5
		with: #parameter.
	7
		when: #this
		send: #odd
		to: 2
		with: #parameter.

	8 trigger: #this! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 3/7/1999 20:31'!
testProcess
	^testProcess! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 5/21/1999 21:24'!
testRemove
	2 unsubscribe: #notexist.
	2 unsubscribe: #even.
	2 unsubscribe.
	Transcript unsubscribe: #that.
	Transcript unsubscribe.
     Bag new unsubscribe.! !

!PostOffice class methodsFor: 'testing' stamp: 'pjs 6/12/1999 23:15'!
testSubs
	| x y z |
	x _ Bag new.
	y _ Bag new.
     z := Bag new.
	Transcript 
		when: #splotch
		send: #add:
		to: x
		with: 'a'.
	Transcript
		when: #splatch
		send: #remove:
		to: z
		with: 'a'.
	Transcript
		when: #splotch
		send: #add:
		to: y
		with: 'bb'.
 x := y := z := nil.
   ! !

!PostOffice class methodsFor: 'conversion' stamp: 'pjs 6/15/1999 15:08'!
convertDependents
	DependentsFields keysAndValuesDo: [:publisher :subscribers | subscribers do: [:eachsub | publisher
				when: #changed
				send: #update:
				to: eachsub
				with: publisher]].
DependentsFields := IdentityDictionary new.! !

!PostOffice class methodsFor: 'instance creation' stamp: 'pjs 6/15/1999 02:09'!
default
	default ifNil: [default _ WeakArray addWeakDependent: self basicNew initialize].
     
	^ default! !

!PostOffice class methodsFor: 'instance creation' stamp: 'pjs 6/1/1999 09:50'!
new

	^self error: 'PostOffice can only have one instance'
! !


!RecursionLock methodsFor: 'as yet unclassified' stamp: 'pjs 6/14/1999 10:09'!
critical: aBlock 
	| activeProcess result |
	activeProcess _ Processor activeProcess.
	^ activeProcess == owner
		ifTrue: [aBlock value]
		ifFalse: [mutex
				critical: 
					[owner _ activeProcess.
					result := aBlock value.
					owner _ nil.
                        result]]! !

!RecursionLock methodsFor: 'as yet unclassified' stamp: 'pjs 5/30/1999 20:44'!
initialize
 mutex := Semaphore forMutualExclusion! !


!RecursionLock class methodsFor: 'as yet unclassified' stamp: 'pjs 6/14/1999 10:12'!
forMutualExclusion
    ^self new initialize! !


!WeakArray class methodsFor: 'accessing' stamp: 'pjs 4/29/1999 12:13'!
addWeakDependent: anObject 
	| finished index weakDependent |
	self isFinalizationSupported ifFalse: [^ self].
	FinalizationLock
		critical: 
			[finished _ false.
			index _ 0.
			
			[index _ index + 1.
			finished not and: [index <= FinalizationDependents size]]
				whileTrue: 
					[weakDependent _ FinalizationDependents at: index.
					weakDependent isNil
						ifTrue: 
							[FinalizationDependents at: index put: anObject.
							finished _ true]].
			finished
				ifFalse: 
					["Grow linearly"
					FinalizationDependents _ FinalizationDependents , (WeakArray new: 10).
					FinalizationDependents at: index put: anObject]]
		ifError: [:msg :rcvr | rcvr error: msg].
    ^anObject! !


PostOffice initialize!

