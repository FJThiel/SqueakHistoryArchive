'From Squeak 1.2 of June 29, 1997 on 7 August 1997 at 12:59:18 am'!Object subclass: #Action
	instanceVariableNames: 'arguments '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events'!
Action subclass: #BlockAction
	instanceVariableNames: 'block '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events'!
Object subclass: #EventManager
	instanceVariableNames: 'actionLists '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events'!
EventManager subclass: #Animal
	instanceVariableNames: 'name isDead '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events Testing'!
Animal subclass: #Dog
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events Testing'!
Object subclass: #EventTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events Testing'!
Animal subclass: #Human
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events Testing'!
Action subclass: #MessageAction
	instanceVariableNames: 'selector receiver '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VS-Events'!
EventManager class
	instanceVariableNames: 'eventsTriggered '!

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
canTriggerEvent: event 
	^ self class canTriggerEvent: event ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/5/97 00:48'!
removeAllActionsWithReceiver: object 
	self actionLists do: [:actionList | 
		actionList removeAll: (actionList select: [:action | action receiver = object])] ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
triggerEvent: event 
	^ self triggerEvent: event withArguments: #() ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
triggerEvent: event with: arg 
	^ self triggerEvent: event withArguments: (Array with: arg) ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
triggerEvent: event with: arg1 with: arg2 
	^ self triggerEvent: event withArguments: (Array with: arg1 with: arg2) ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
triggerEvent: event withArguments: array 
	(self canTriggerEvent: event)
		ifFalse: [self error: 'cannot trigger that event'].
	(self actionListForEvent: event)
		do: [:action | action processWith: array] ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
when: event do: block 
	self
		when: event
		do: block
		withArguments: #() ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:37'!
when: event do: block withArguments: array 
	self when: event processAction: (BlockAction block: block arguments: array) ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
when: event send: message to: object 
	^ self
		when: event
		send: message
		to: object
		withArguments: #() ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
when: event send: message to: object with: arg 
	^ self
		when: event
		send: message
		to: object
		withArguments: (Array with: arg) ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
when: event send: message to: object with: arg1 with: arg2 
	^ self
		when: event
		send: message
		to: object
		withArguments: (Array with: arg1 with: arg2) ! !

!Object methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
when: event send: message to: object withArguments: array 
	(self canTriggerEvent: event)
		ifFalse: [self error: 'cannot trigger that event'].
	self when: event processAction: (MessageAction
			receiver: object
			selector: message
			arguments: array) ! !

!Object methodsFor: 'vs events - private' stamp: 'taj 8/5/97 00:48'!
actionListForEvent: symbol 
	^ self actionLists 
	 	at: symbol 
		ifAbsent: [self actionLists at: symbol put: OrderedCollection new] ! !

!Object methodsFor: 'vs events - private' stamp: 'taj 8/3/97 04:36'!
actionLists
	self subclassResponsibility ! !

!Object methodsFor: 'vs events - private' stamp: 'taj 8/3/97 04:37'!
when: event processAction: action 
	| actionList |
	actionList _ self actionListForEvent: event.
	(actionList includes: action)
		ifFalse: [actionList add: action] ! !


!Action methodsFor: 'all' stamp: 'taj 8/3/97 04:21'!
arguments
	^ arguments ! !

!Action methodsFor: 'all' stamp: 'taj 8/3/97 04:21'!
arguments: array 
	arguments _ array ! !

!Action methodsFor: 'all' stamp: 'taj 8/3/97 04:22'!
processWith: array 
	self subclassResponsibility ! !

!Action methodsFor: 'all' stamp: 'taj 8/3/97 04:22'!
receiver
	self subclassResponsibility ! !


!BlockAction methodsFor: 'all' stamp: 'taj 8/3/97 04:26'!
block
	^ block ! !

!BlockAction methodsFor: 'all' stamp: 'taj 8/3/97 04:27'!
block: b 
	block _ b ! !

!BlockAction methodsFor: 'all' stamp: 'taj 8/3/97 04:29'!
processWith: array 
	block valueWithArguments: (array isEmpty
			ifTrue: [self arguments]
			ifFalse: [array]) ! !

!BlockAction methodsFor: 'all' stamp: 'taj 8/3/97 04:30'!
receiver
	^ self block receiver ! !


!EventManager methodsFor: 'object overrides' stamp: 'taj 8/3/97 04:32'!
actionLists
	^ actionLists ! !

!EventManager methodsFor: 'object overrides' stamp: 'taj 8/3/97 04:44'!
release
	super release.
	actionLists _ nil ! !

!EventManager methodsFor: 'private' stamp: 'taj 8/3/97 04:32'!
initialize
	actionLists _ Dictionary new ! !


!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:23'!
die
	isDead _ true.
	self report: 'dies'.
	self triggerEvent: #killed: with: self ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:39'!
ignore: anAnimal 
	anAnimal removeAllActionsWithReceiver: self ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:24'!
initialize
	super initialize.
	isDead _ false ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:24'!
isDead
	^ isDead ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:24'!
name
	^ name ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:24'!
name: aString 
	name _ aString ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:25'!
report: aString 
	Transcript show: self name , ' ' , aString;
	 cr ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:25'!
say: aString 
	self report: 'says ''' , aString , '''' ! !

!Animal methodsFor: 'all' stamp: 'taj 8/3/97 04:25'!
watch: anAnimal 
	anAnimal when: #killed: do: [:another | self report: 'notices ' , another name , '''s death'] ! !


!Dog methodsFor: 'all' stamp: 'taj 8/3/97 04:31'!
performCommand: aString 
	self isDead
		ifFalse: 
			[self report: 'completes command: ''' , aString , ''''.
			self triggerEvent: #performedCommand: with: self] ! !

!Dog methodsFor: 'all' stamp: 'taj 8/3/97 04:31'!
watch: aHuman 
	super watch: aHuman.
	aHuman when: #killed: do: 
		[:other | 
		self report: 'licks ' , other name.
		self ignore: other].
	aHuman
		when: #commandGiven:
		send: #performCommand:
		to: self ! !


!Human methodsFor: 'all' stamp: 'taj 8/3/97 04:34'!
giveCommand: aString 
	self report: 'commands ''' , aString , ''''.
	self triggerEvent: #commandGiven: with: aString ! !

!Human methodsFor: 'all' stamp: 'taj 8/3/97 04:35'!
watch: aDog 
	super watch: aDog.
	aDog when: #killed: do: 
		[:another | 
		self say: 'alas poor ' , another name , ', i knew him well'.
		self ignore: another].
	aDog when: #performedCommand: do: [:d | self say: 'good ' , d name , '!!'] ! !


!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:35'!
= another 
	(another isKindOf: MessageAction)
		ifFalse: [^ false].
	self receiver ~= another receiver ifTrue: [^ false].
	self selector ~= another selector ifTrue: [^ false].
	^ true ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:35'!
hash
	^ self receiver hash xor: self selector hash ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:35'!
processWith: array 
	| message |
	array isEmpty
		ifTrue: [message _ Message selector: self selector arguments: self arguments]
		ifFalse: [message _ Message selector: self selector arguments: array].
	message sentTo: self receiver ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:36'!
receiver
	^ receiver ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:36'!
receiver: object 
	receiver _ object ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:36'!
selector
	^ selector ! !

!MessageAction methodsFor: 'all' stamp: 'taj 8/3/97 04:36'!
selector: symbol 
	selector _ symbol ! !


!Object class methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
canTriggerEvent: event 
	^ self eventsTriggered includes: event ! !

!Object class methodsFor: 'vs events' stamp: 'taj 8/3/97 04:38'!
eventsTriggered
	^ #() ! !


!BlockAction class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:30'!
block: block arguments: arguments 
	| i |
	i _ self new.
	i block: block.
	i arguments: arguments.
	^ i ! !


!EventManager class methodsFor: 'object overrides' stamp: 'taj 8/3/97 04:33'!
eventsTriggered
	eventsTriggered isNil ifTrue: [self initializeEventsTriggered].
	^ eventsTriggered ! !

!EventManager class methodsFor: 'object overrides' stamp: 'taj 8/3/97 04:33'!
initialize
	super initialize.
	self initializeEventsTriggered ! !

!EventManager class methodsFor: 'object overrides' stamp: 'taj 8/3/97 04:33'!
new
	^ super new initialize ! !

!EventManager class methodsFor: 'private' stamp: 'taj 8/3/97 04:32'!
constructEventsTriggered
	"initializeEventsTriggered must be called every time this method is 
	updated "
	^ Set with: #changed ! !

!EventManager class methodsFor: 'public' stamp: 'taj 8/3/97 04:33'!
initializeEventsTriggered
	"must be called every time the constructEventsTriggered method is 
	updated "
	eventsTriggered _ self constructEventsTriggered ! !


!Animal class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:26'!
constructEventsTriggered
	^ super constructEventsTriggered add: #killed:;
	 yourself ! !

!Animal class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:26'!
named: aString 
	^ self new name: aString;
	 yourself ! !


!Dog class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:31'!
constructEventsTriggered
	^ super constructEventsTriggered add: #performedCommand:;
	 yourself ! !


!EventTester class methodsFor: 'tests' stamp: 'taj 8/3/97 04:34'!
test1
	"run through this method with the debugger, and watch the transcript"
	| human fido java | 
	human _ Human named: 'Fernando'.
	fido _ Dog named: 'Fido'.
	java _ Dog named: 'Java'.
	human watch: fido;
	 watch: java.
	fido watch: human.
	java watch: human.
	human giveCommand: 'sit'.
	java die.
	human giveCommand: 'beg'! !


!Human class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:35'!
constructEventsTriggered
	^ super constructEventsTriggered add: #commandGiven:;
	 yourself ! !


!MessageAction class methodsFor: 'as yet unclassified' stamp: 'taj 8/3/97 04:36'!
receiver: receiver selector: selector arguments: arguments 
	| i |
	i _ self new.
	i receiver: receiver.
	i selector: selector.
	i arguments: arguments.
	^ i ! !


EventManager initialize!
