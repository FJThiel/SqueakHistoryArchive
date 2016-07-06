"      NAME	NameSpace-early code
       AUTHOR	pno@whitestein.com (Peter Novak)
       URL	(none)
       FUNCTION	implementation of concepts of name spaces to Squeak2.4b.
       KEYWORDS	namespaces environments subsystems
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(some bugs maybe)
       DISTRIBUTION	world
       VERSION	0.9
       DATE	05-Aug-99

SUMMARY

It may serve as inspiration for other experienced smalltalkers in their approach to implement real good name spaces to Squeak.

				Peter Novak
"!
'From Squeak 2.4b of April 23, 1999 on 22 July 1999 at 2:39:47 pm'!
Dictionary subclass: #NameSpace
	instanceVariableNames: 'superNameSpaces subNameSpaces nsName pools removed'
	classVariableNames: 'CachedClassNames'
	poolDictionaries: ''
	category: 'NameSpaces'!
OrderedCollection subclass: #NameSpaceCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'NameSpaces'!
NameSpace subclass: #SystemDictionary
	instanceVariableNames: ''
	classVariableNames: 'CachedClassNames LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SpecialSelectors StartUpList SystemChanges '
	poolDictionaries: ''
	category: 'System-Support'!
ClassDescription subclass: #Metaclass
	instanceVariableNames: 'thisClass nameSpace '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!
Workspace subclass: #EnvironmentalWorkspace
	instanceVariableNames: 'viewNS '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-Support'!
Browser subclass: #EnvironmentalBrowser
	instanceVariableNames: 'nameSpaceList nameSpaceIndex heldNameSpace '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-Browser'!
Model subclass: #Project
	instanceVariableNames: 'world changeSet transcript parentProject previousProject displayDepth activeProcess exitFlag viewSize thumbnail nextProject guards projectParameters nameSpace '
	classVariableNames: 'CurrentProject '
	poolDictionaries: ''
	category: 'Interface-Projects'!

!Project methodsFor: 'name space' stamp: 'nowo 8/4/1999 09:33'!
nameSpace
  (nameSpace isNil) ifTrue: [self nameSpace: Smalltalk].
  ^ nameSpace.! !

!Project methodsFor: 'name space' stamp: 'nowo 8/4/1999 09:29'!
nameSpace: aNameSpace
  (aNameSpace isKindOf: NameSpace) ifFalse: [self error: 'argument has to be a NameSpace'].
  nameSpace _ aNameSpace.! !

!Project methodsFor: 'initialization' stamp: 'nowo 8/4/1999 09:33'!
setProjectHolder: aProject

	world _ ControlManager new.
	changeSet _ ChangeSet new initialize.
	transcript _ TranscriptStream new.
	displayDepth _ Display depth.
	parentProject _ aProject.
     self nameSpace: aProject nameSpace.! !

!Metaclass methodsFor: 'name spaces' stamp: 'nowo 7/28/1999 16:32'!
nameSpace
  ^ nameSpace! !

!Metaclass methodsFor: 'name spaces' stamp: 'nowo 7/28/1999 16:33'!
nameSpace: aNameSpace
  (aNameSpace isKindOf: NameSpace) ifFalse: [self error: 'argument has to be a NameSpace'].
  nameSpace _ aNameSpace.! !

!Class methodsFor: 'initialize-release' stamp: 'nowo 8/3/1999 17:41'!
removeFromSystemUnlogged
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"

	NameSpace current removeClassFromSystemUnlogged: self.
	self obsolete! !


!Class methodsFor: 'initialize-release' stamp: 'nowo 8/3/1999 17:42'!
removeFromSystem
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."

	NameSpace current removeClassFromSystem: self.
	self obsolete! !

!Class methodsFor: 'name space' stamp: 'nowo 7/28/1999 16:32'!
nameSpace
  ^ self class nameSpace.! !

!Class methodsFor: 'name space' stamp: 'nowo 7/28/1999 16:33'!
nameSpace: aNameSpace
  self class nameSpace: aNameSpace.! !

Smalltalk do: [:each | (each isKindOf: Class) ifTrue: [each nameSpace: Smalltalk]].!

!SystemDictionary methodsFor: 'testing' stamp: 'nowo 7/26/1999 11:24'!
isSmalltalk
  ^ true.! !

!NameSpace class methodsFor: 'as yet unclassified' stamp: 'nowo 6/30/1999 15:11'!
new

  ^super new initialize.! !

!NameSpace class methodsFor: 'as yet unclassified' stamp: 'nowo 7/30/1999 16:35'!
newWithPools: aSet
  super new.
  self pools: aSet.! !


!NameSpace class methodsFor: 'setting' stamp: 'nowo 7/19/1999 12:10'!
current

  ^ Project current nameSpace.! !

!NameSpace class methodsFor: 'setting' stamp: 'nowo 7/19/1999 12:10'!
currentNameSpace

  ^ NameSpace current.! !


!NameSpace class methodsFor: 'setting' stamp: 'nowo 7/19/1999 12:09'!
setTo: aNameSpace 
	Project current nameSpace: aNameSpace.
! !

!NameSpace methodsFor: 'pools' stamp: 'nowo 8/3/1999 16:47'!
addPool: aPool
  pools add: aPool.! !

!NameSpace methodsFor: 'pools' stamp: 'nowo 7/30/1999 16:33'!
pools
  ^ pools.! !

!NameSpace methodsFor: 'pools' stamp: 'nowo 7/30/1999 16:34'!
pools: aSet
  (aSet isKindOf: Set) ifFalse: [self error: 'argument has to be an IdentitySet'].
  pools _ aSet.! !


!NameSpace methodsFor: 'testing' stamp: 'nowo 7/1/1999 17:12'!
isSmalltalk

  ^ false.! !

!NameSpace methodsFor: 'resolving' stamp: 'nowo 7/1/1999 17:11'!
doesNotUnderstand: aMessage 

	^ self at: aMessage selector
		ifAbsent: 
			[((aMessage selector at: 1) isUppercase and: [self confirm: 'Global variable ', aMessage selector, ' not yet defined in ' , self fullName , '. Define as sub-environment?'])
				ifTrue: 
					[self createSubNameSpace: aMessage selector asSymbol.
					^ self at: aMessage selector ifAbsent: [self error: 'problem in doesNotUnderStand']].
			^ super doesNotUnderstand: aMessage]! !

!NameSpace methodsFor: 'removing' stamp: 'nowo 7/2/1999 11:56'!
removeSubNameSpace: aNameSpace 
  self removeSubNameSpace: aNameSpace ifAbsent: [self error: 'no such subNameSpace'].! !

!NameSpace methodsFor: 'removing' stamp: 'nowo 7/2/1999 11:17'!
removeSubNameSpace: aNameSpace ifAbsent: aBlock

  ((self subNameSpaces) includes: aNameSpace) 
         ifFalse: [^ aBlock value].  

  self privateRemoveSubNameSpace: aNameSpace ifAbsent: aBlock.  
  aNameSpace privateRemoveSuperNameSpace: self ifAbsent: aBlock.
    ! !

!NameSpace methodsFor: 'removing' stamp: 'nowo 7/2/1999 11:56'!
removeSuperNameSpace: aNameSpace 
  self removeSuperNameSpace: aNameSpace ifAbsent: [self error: 'no such superNameSpace'].! !

!NameSpace methodsFor: 'removing' stamp: 'nowo 7/1/1999 17:26'!
removeSuperNameSpace: aNameSpace ifAbsent: aBlock

   aNameSpace removeSubNameSpace: self ifAbsent: aBlock.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/19/1999 12:40'!
name: aString
  (aString isKindOf: String) ifFalse: [self error: 'argument is not a String'].
  nsName _ aString asSymbol.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/1/1999 14:59'!
privateAddSubNameSpace: aNameSpace

  subNameSpaces add: aNameSpace.
  aNameSpace privateAddSuperNameSpace: self.
  self privateAt: aNameSpace name put: aNameSpace.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/1/1999 15:02'!
privateAddSuperNameSpace: aNameSpace

  superNameSpaces add: aNameSpace.
! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/7/1999 10:17'!
privateAssociationAt: aSymbol ifAbsent: aBlock

  ^ super associationAt: aSymbol ifAbsent: aBlock.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 6/30/1999 15:46'!
privateAt: aSymbol put: aObject

  (aSymbol isKindOf: Symbol) ifFalse: [self error: 'argument is not a symbol'].
  self add: aSymbol -> aObject.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 6/30/1999 16:00'!
privateRemoveKey: aSymbol

  (aSymbol isKindOf: Symbol) ifFalse: [self error: 'argument is not a Symbol'].

  self removeKey: aSymbol. ! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/2/1999 11:35'!
privateRemoveSubNameSpace: aNameSpace ifAbsent: aBlock

  self privateSubNameSpaces: (subNameSpaces select: [:one | (one == aNameSpace) not]).
  
  

  ! !

!NameSpace methodsFor: 'private' stamp: 'nowo 7/2/1999 11:35'!
privateRemoveSuperNameSpace: aNameSpace ifAbsent: aBlock

   self privateSuperNameSpaces: (superNameSpaces select: [:one | (one == aNameSpace) not]).

  ! !

!NameSpace methodsFor: 'private' stamp: 'nowo 6/30/1999 17:12'!
privateSubNameSpaces: aNameSpaceCollection
  subNameSpaces _ aNameSpaceCollection.! !

!NameSpace methodsFor: 'private' stamp: 'nowo 6/30/1999 17:12'!
privateSuperNameSpaces: aNameSpaceCollection
  superNameSpaces _ aNameSpaceCollection.! !

!NameSpace methodsFor: 'printing' stamp: 'nowo 7/6/1999 17:04'!
printOn: aStream

	aStream nextPutAll: 'NameSpace'.
	(self isSmalltalk)
		ifTrue: [ aStream nextPutAll: ' (all the globals for ', self name,')' ].
! !

!NameSpace methodsFor: 'initialize' stamp: 'nowo 7/30/1999 16:22'!
initialize

  removed _ Set new.
  pools _ OrderedCollection new.
  self add: (#Undeclared -> Dictionary new).
  self privateSuperNameSpaces: NameSpaceCollection new; 
       privateSubNameSpaces: NameSpaceCollection new.
  (self isSmalltalk) ifFalse: [
  Smalltalk UndefinedObject class
		name: #UndefinedObject
		inEnvironment: self
		subclassOf: Smalltalk UndefinedObject
		instanceVariableNames: ''
		variable: false
		words: true
		pointers: true
		weak: false
		classVariableNames: ''
		poolDictionaries: ''
		category: 'UndefinedObject'
		comment: nil
		changed: false ]
    ifTrue: [Smalltalk UndefinedObject class compile: 'new
   ^ super new.' classified: 'xyz' withStamp: 'nowo 30/07/99' notifying: nil.].  

   "self UndefinedObject class compile: 'new
   ^ super new.' classified: 'xyz' withStamp: 'nowo 30/07/99' notifying: nil."

   self UndefinedObject nameSpace: self.
   self at: #nil put: self UndefinedObject new.

! !

!NameSpace methodsFor: 'creating' stamp: 'nowo 6/30/1999 15:11'!
createSubNameSpace: aSymbol

  | sub |
  (aSymbol isKindOf: Symbol) ifFalse: [self error: 'argument is not a Symbol'].
  sub _ NameSpace new name: aSymbol.
  ^ self addSubNameSpace: sub.
  ! !

!NameSpace methodsFor: 'converting' stamp: 'nowo 7/1/1999 11:51'!
asSystemDictionary
  | spc |
  spc _ SystemDictionary new.
  self associationsDo: [:one | spc add: one].
  ^ spc.! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 7/28/1999 17:45'!
allNamesOf: anObject 
	| dict key |
    dict _ Dictionary new.
	NameSpace allInstances do: 
		[:one | 
		key _ one keyAtValue: anObject ifAbsent: [].
		key notNil ifTrue: [dict add: key asSymbol -> one]]! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 7/28/1999 17:47'!
allOccurenciesOf: aSymbol 
	| dict val |
	dict _ Dictionary new.
	NameSpace allInstances do: 
		[:one | 
		val _ one at: aSymbol ifAbsent: [].
		val notNil ifTrue: [dict add: val asSymbol -> one]]! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 7/28/1999 17:47'!
allOccurenciesOf: aSymbol 
	| dict val |
	dict _ Dictionary new.
	NameSpace allInstances do: 
		[:one | 
		val _ one at: aSymbol ifAbsent: [].
		val notNil ifTrue: [dict add: val asSymbol -> one]]! !


!NameSpace methodsFor: 'collecting' stamp: 'nowo 6/30/1999 17:10'!
allSubNameSpaces
  ^ self collectSubNameSpacesTo: NameSpaceCollection new done: IdentitySet new.
! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 6/30/1999 17:10'!
allSuperNameSpaces
  
  ^ self collectSuperNameSpacesTo: NameSpaceCollection new done: IdentitySet new.! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 6/30/1999 17:11'!
collectSubNameSpacesTo: aNameSpaceCollection done: aSet

  self subNameSpaces do: [:one | 
           (aSet includes: one) 
                    ifFalse: [aNameSpaceCollection add: one.
                               aSet add: one.
                               one collectSubNameSpacesTo: aNameSpaceCollection done: aSet]].
  ^ aNameSpaceCollection.
   ! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 6/30/1999 17:11'!
collectSuperNameSpacesTo: aNameSpaceCollection done: aSet

  self superNameSpaces do: [:one | 
           (aSet includes: one) 
                    ifFalse: [aNameSpaceCollection add: one.
                               aSet add: one.
                               one collectSuperNameSpacesTo: aNameSpaceCollection done: aSet]].
  ^ aNameSpaceCollection.
   ! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 6/30/1999 11:06'!
inheritsFrom: aNameSpace
  ^ self allSuperNameSpaces includes: aNameSpace.! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 8/3/1999 18:24'!
collectSubNameSpacesAndPoolsTo: anOrderedCollection done: aSet

  self collectSubNameSpacesTo: anOrderedCollection done: aSet.
  self pools do: [:one | 
           (aSet includes: one) 
                    ifFalse: [anOrderedCollection add: one.
                               aSet add: one.
                               (one isKindOf: NameSpace) ifTrue: [one collectSuperNameSpacesAndPoolsTo: anOrderedCollection done: aSet]]].
  ^ anOrderedCollection.
   ! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 8/3/1999 18:24'!
collectSuperNameSpacesAndPoolsTo: anOrderedCollection done: aSet

  self collectSuperNameSpacesTo: anOrderedCollection done: aSet.
  self pools do: [:one | 
           (aSet includes: one) 
                    ifFalse: [anOrderedCollection add: one.
                               aSet add: one.
                               (one isKindOf: NameSpace) ifTrue: [one collectSuperNameSpacesAndPoolsTo: anOrderedCollection done: aSet]]].
  ^ anOrderedCollection.
   ! !

!NameSpace methodsFor: 'adding' stamp: 'nowo 7/2/1999 11:12'!
addSubNameSpace: aNameSpace
  "check the correctness of inheritance and if it's okay than call privateAddSubNameSpace"

  (aNameSpace isKindOf: NameSpace)
         ifFalse: [self error: 'argument is not a NameSpace'].

  (self inheritsFrom: aNameSpace) 
         ifTrue: [self error: aNameSpace name, ': inheritance rules violation'].

  
  (self includesKey: aNameSpace name)
         ifTrue: [ 
          (self confirm: (aNameSpace name), ' already defined in ', self name,'. Redefine it?')
              ifTrue: [
                   self privateAddSubNameSpace: aNameSpace.]]
         ifFalse: [
           self privateAddSubNameSpace: aNameSpace].
  ^ aNameSpace.! !

!NameSpace methodsFor: 'adding' stamp: 'nowo 7/2/1999 11:12'!
addSuperNameSpace: aNameSpace
  "check the correctness of inheritance and if it's okay than call privateAddSubNameSpace"
  
  aNameSpace addSubNameSpace: self.
  ^ aNameSpace.! !

Smalltalk initialize.!
Smalltalk name: 'Smalltalk'.!

!NameSpace methodsFor: 'masquerading' stamp: 'nowo 7/30/1999 17:02'!
hide: aSymbol 
  self hide: aSymbol ifPresent: [self error: aSymbol, ' is member of this NameSpace'].! !

!NameSpace methodsFor: 'masquerading' stamp: 'nowo 7/30/1999 16:58'!
hide: aSymbol ifPresent: aBlock
  ((super at: aSymbol ifAbsent: [nil]) notNil) ifTrue: [^ aBlock value].
   self removed add: aSymbol.! !

!NameSpace methodsFor: 'masquerading' stamp: 'nowo 7/30/1999 16:58'!
removed
  ^ removed.! !

!NameSpace methodsFor: 'masquerading' stamp: 'nowo 7/30/1999 16:59'!
removed: aSet
  (aSet isKindOf: Set) ifFalse: [self error: 'argument has to be a Set'].
  removed _ aSet.! !

!NameSpace methodsFor: 'masquerading' stamp: 'nowo 7/30/1999 17:00'!
show: aSymbol
   self removed remove: aSymbol ifAbsent: [nil].! !


!NameSpace methodsFor: 'access' stamp: 'nowo 7/6/1999 17:04'!
at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."
     (self removed includes: aKey) ifTrue: [self show: aKey].
	(self includesKey: aKey) ifFalse: 
		[self declare: aKey from: Undeclared.
		self flushClassNameCache].
	super at: aKey put: anObject.
	^ anObject
! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/6/1999 17:04'!
fullName

  (self isSmalltalk) ifTrue: [^ self name].
  ^ ((self superNameSpaces) at: 1) fullName,' ', self name.! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/19/1999 11:23'!
name
  ^ nsName! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/6/1999 17:04'!
path
  
  (self isSmalltalk) ifTrue: [^ Array with: self].
   ^ (((self superNameSpaces) at: 1) path), (Array with: self).! !

!NameSpace methodsFor: 'access' stamp: 'nowo 6/30/1999 12:24'!
path: anArray
  "anArray is an array of symbols"
 
  (anArray isKindOf: Array) ifFalse: [self error: 'argument not an array'].
  (anArray size > 0) ifFalse: [^ self].
  
  ((anArray at: 1) == #Smalltalk) ifTrue: [Smalltalk path: (anArray copyFrom: 2 to: anArray size)].

  ^ ((self subNameSpace: anArray) at: 1) path: (anArray copyFrom: 2 to: anArray size).   ! !

!NameSpace methodsFor: 'access' stamp: 'nowo 6/30/1999 12:27'!
subNameSpace: aSymbol

  ^ (self subNameSpaces) 
          detect: [:one | one name == aSymbol] 
          ifNone: [self error: 'no subNameSpace with name ', aSymbol asString].! !

!NameSpace methodsFor: 'access' stamp: 'nowo 6/30/1999 15:08'!
subNameSpaces
  ^ subNameSpaces! !

!NameSpace methodsFor: 'access' stamp: 'nowo 6/30/1999 12:30'!
superNameSpace: aSymbol

  ^ (self superNameSpaces) 
          detect: [:one | one name == aSymbol] 
          ifNone: [self error: 'no superNameSpace with name ', aSymbol asString].! !

!NameSpace methodsFor: 'access' stamp: 'nowo 6/29/1999 17:12'!
superNameSpaces
  ^ superNameSpaces! !

!NameSpace methodsFor: 'access' stamp: 'nowo 8/4/1999 08:33'!
supersFindAssociationAt: aSymbol ifAbsent: aBlock 
	(self collectSuperNameSpacesAndPoolsTo: OrderedCollection new done: IdentitySet new)
		do: [:each | (each includesKey: aSymbol)
				ifTrue: [^ each associationAt: aSymbol ifAbsent: aBlock]].
	^ aBlock value! !


!NameSpace methodsFor: 'access' stamp: 'nowo 7/2/1999 11:12'!
undeclared
  ^ self at: #Undeclared ifAbsent: [self error: 'no undeclared object'].! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/22/1999 11:59'!
associationAt: aSymbol 

   ^ self associationAt: aSymbol ifAbsent: [self errorKeyNotFound].
! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/22/1999 11:59'!
associationAt: aSymbol ifAbsent: aBlock
   (self removed includes: aSymbol) ifTrue: [^ aBlock value].
   (aSymbol == #Super) ifTrue: [^ (#Super -> self superNameSpaces)].
   ^ self privateAssociationAt: aSymbol asSymbol ifAbsent: [self supersFindAssociationAt: aSymbol ifAbsent: aBlock].
! !

!NameSpace methodsFor: 'access' stamp: 'nowo 8/4/1999 08:30'!
at: aSymbol ifAbsent: aBlock 
	(self removed includes: aSymbol)
		ifTrue: [^ aBlock value].
	aSymbol == #Super ifTrue: [^ self superNameSpaces].
	(self includesKey: aSymbol)
		ifTrue: [^ super at: aSymbol ifAbsent: aBlock]
		ifFalse: [(self collectSuperNameSpacesAndPoolsTo: OrderedCollection new done: IdentitySet new)
				do: [:each | (each includesKey: aSymbol)
						ifTrue: [^ each at: aSymbol ifAbsent: [self error: 'strange error occured']]]].
	^ aBlock value! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/23/1999 13:50'!
pathFor: aSymbol 
	(self includesKey: aSymbol)
		ifTrue: [^ self]
		ifFalse: [self isSmalltalk
				ifTrue: [^ nil]
				ifFalse: [^ superNameSpaces pathFor: aSymbol]].
! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/29/1999 08:11'!
findNameSpaceOf: aSymbol ifAbsent: aBlock
  (self includesKey: aSymbol) ifTrue: [^ self].
  self allSuperNameSpaces do: [:one | (one includesKey: aSymbol) ifTrue: [^ one]].
  ^ aBlock value.! !

!NameSpace methodsFor: 'access' stamp: 'nowo 7/29/1999 09:11'!
addClass: aClass 
	(aClass isKindOf: Class)
		ifFalse: [self error: 'argument has to be a class'].
	self add: aClass name -> aClass.
	(self at: aClass name)
		compileAllFrom: aClass.
          aClass nameSpace: self.! !

!NameSpace methodsFor: 'collecting' stamp: 'nowo 7/26/1999 15:15'!
allBrothers
	| brothers |
	brothers _ IdentitySet new.
	self superNameSpaces 
             do: [:each | 
                   each subNameSpaces do: [:one | 
                        one == self ifFalse: [brothers add: one]]].
	^ brothers! !
!NameSpace methodsFor: 'import' stamp: 'nowo 7/28/1999 17:51'!
createAliasOf: anObject under: aSymbol

  self at: aSymbol put: anObject.! !

!NameSpace methodsFor: 'import' stamp: 'nowo 7/28/1999 17:51'!
importSymbol: aSymbol from: aNameSpace 
    self add: 
       (aNameSpace associationAt: aSymbol 
                       ifAbsent: 
             [self error: 'no object associated with ' , aSymbol , ' in ' , aNameSpace fullName])! !

!NameSpace methodsFor: 'nil' stamp: 'nowo 7/30/1999 16:20'!
doItContext
  ^ super at: #DoItContext ifAbsent: [self error: 'DoItContext key in missing in NameSpace'].! !

!NameSpace methodsFor: 'nil' stamp: 'nowo 7/30/1999 16:20'!
doItRequestor
  ^ self doItContext.! !

!SystemDictionary methodsFor: 'nil' stamp: 'nowo 7/30/1999 16:21'!
doItContext
  ^ nil.! !

!SystemDictionary methodsFor: 'nil' stamp: 'nowo 7/30/1999 16:21'!
doItReceiver
  ^ nil.! !

NameSpace setTo: Smalltalk.!

!NameSpaceCollection methodsFor: 'resolving' stamp: 'nowo 7/26/1999 16:13'!
doesNotUnderstand: aMessage 
	| sel col |
	((sel _ aMessage selector) at: 1) isUppercase ifFalse: [^ super doesNotUnderstand: aMessage].
	aMessage selector == #Super
		ifTrue: 
			[col _ IdentitySet new.
			self do: [:each | col addAll: each superNameSpaces].
			^ NameSpaceCollection newFrom: col ].
	^ self symbolAt: sel ifAbsent: [^ super doesNotUnderstand: aMessage]! !

!NameSpaceCollection methodsFor: 'access' stamp: 'nowo 7/6/1999 11:05'!
at: aSymbol ifAbsent: aBlock

  ^ self symbolAt: aSymbol ifAbsent: aBlock.! !

!NameSpaceCollection methodsFor: 'access' stamp: 'nowo 7/1/1999 15:19'!
symbolAt: aSymbol ifAbsent: aBlock

  | obj |
 
  self do: [:one | (obj _ one at: aSymbol ifAbsent: [nil]) notNil ifTrue: [^ obj]].
  ^ aBlock value.! !

!NameSpaceCollection methodsFor: 'access' stamp: 'nowo 7/23/1999 13:54'!
pathFor: aSymbol 

	self do: [:one | (one at: aSymbol ifAbsent: [nil]) notNil ifTrue: [^ one pathFor: aSymbol]].! !

!NameSpaceCollection methodsFor: 'testing' stamp: 'nowo 6/30/1999 17:09'!
includes: anObject
  	"Answer whether anObject is one of the receiver's elements."

	self do: [:each | anObject == each ifTrue: [^true]].
	^false! !

!NameSpaceCollection methodsFor: 'adding' stamp: 'nowo 7/26/1999 16:08'!
add: anObject 
	(anObject isKindOf: NameSpace)
		ifFalse: [self error: 'argument has to be a NameSpace'].
	(self includes: anObject)
		ifFalse: [super add: anObject]! !


!Parser methodsFor: 'error correction' stamp: 'nowo 7/19/1999 12:19'!
correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction fullSearch: tryHard 
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| alternatives aStream choice correctSelector userSelection lines firstLine |
	"If we can't ask the user, assume that the keyword will be defined later"
	self interactive ifFalse: [ ^ proposedKeyword asSymbol ].

     "nowo's update" 
     ((proposedKeyword asSymbol) at: 1) isUppercase ifTrue: [^ proposedKeyword asSymbol].

	userSelection _ requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.
	requestor select.
	alternatives _ tryHard
		ifFalse: [ Symbol possibleSelectorsFor: proposedKeyword ]
		ifTrue: [ Symbol morePossibleSelectorsFor: proposedKeyword ].

	aStream _ WriteStream on: (String new: 200).
	aStream nextPutAll: (proposedKeyword contractTo: 35); cr.
	firstLine _ 1.
 	alternatives do:
		[:sel | aStream nextPutAll: (sel contractTo: 35); nextPut: Character cr].
	aStream nextPutAll: 'cancel'.
	lines _ Array with: firstLine with: (alternatives size + firstLine).
	tryHard ifFalse:
		[aStream cr; nextPutAll: 'try harder'.
		lines _ lines copyWith: (alternatives size + firstLine + 1)].
	
	choice _ (PopUpMenu labels: aStream contents lines: lines)
		startUpWithCaption: 
'Unknown selector, please 
confirm, correct, or cancel'.

	tryHard not & (choice > lines last) ifTrue:
		[^ self correctSelector: proposedKeyword wordIntervals: spots
				exprInterval: expInt ifAbort: abortAction fullSearch: true ]. 

	(choice = 0) | (choice > (lines at: 2))
		ifTrue: [ ^ abortAction value ].

	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	choice = 1 ifTrue: [ ^ proposedKeyword asSymbol ].
	correctSelector _ alternatives at: choice - 1.
	self substituteSelector: correctSelector keywords wordIntervals: spots.
	((proposedKeyword last ~~ $:) and: [correctSelector last == $:]) ifTrue: [
		^ abortAction value].
	^ correctSelector.
! !

!Class methodsFor: 'compiling' stamp: 'nowo 7/29/1999 18:30'!
scopeHas: varName ifTrue: assocBlock 
	"Look up the first argument, varName, in the context of the receiver. If it is there,
	pass the association to the second argument, assocBlock, and answer true.
	Else answer false.
	: Allow key in shared pools to be a string for HyperSqueak"

	| assoc |
	assoc _ self classPool associationAt: varName ifAbsent: [].
	assoc == nil
		ifFalse: 
			[assocBlock value: assoc.
			^true].
	self sharedPools do: 
		[:pool | 
		varName = #Textual ifTrue: [self halt].
		assoc _ pool associationAt: varName ifAbsent: [
			pool associationAt: varName asString ifAbsent: []].
		assoc == nil
			ifFalse: 
				[assocBlock value: assoc.
				^true]].
             
          assoc _ self nameSpace associationAt: varName ifAbsent: [].
			assoc == nil
				ifFalse: 
					[assocBlock value: assoc.
					^true].	

	self nameSpace pools do: 
		[:pool | 
		assoc _ pool associationAt: varName ifAbsent: [
			pool associationAt: varName asString ifAbsent: []].
		assoc == nil
			ifFalse: 
				[assocBlock value: assoc.
				^true]].

          superclass == nil
     		ifTrue: 
			[^false].
	^superclass scopeHas: varName ifTrue: assocBlock! !


!Encoder methodsFor: 'results' stamp: 'nowo 7/19/1999 12:21'!
associationFor: aClass

	| name |
	name _ NameSpace current keyAtIdentityValue: aClass ifAbsent: [^Association new value: aClass].
	^NameSpace current associationAt: name! !

!Parser methodsFor: 'error correction' stamp: 'nowo 7/19/1999 12:20'!
declareGlobal: name
	| sym |
	sym _ name asSymbol.
	NameSpace current at: sym put: nil.
	^ encoder global: (NameSpace current associationAt: sym) name: sym! !

!NameSpace methodsFor: 'class names'!
classNamed: className 
	"className is either a class name or a class name followed by ' class'.
	Answer the class or metaclass it names.
	8/91 sw chgd so returns nil if class not found, to correct failures in Change Sorter across class renames"
	| meta baseName baseClass length |
	length _ className size.
	(length > 6 and: 
			[(className copyFrom: length - 5 to: length) = ' class'])
		ifTrue: 
			[meta _ true.
			baseName _ className copyFrom: 1 to: length - 6]
		ifFalse: 
			[meta _ false.
			baseName _ className].
	baseClass _ self at: baseName asSymbol ifAbsent: [nil].
	baseClass isNil ifTrue: [^ nil].
	meta
		ifTrue: [^baseClass class]
		ifFalse: [^baseClass]! !

!NameSpace methodsFor: 'class names' stamp: 'nowo 7/26/1999 17:29'!
classes
	"Answer a SortedCollection of all class names."
	| names |
    names _ OrderedCollection new.
		self do: 
			[:cl | (cl isKindOf: Class) ifTrue: [names add: cl name]].
	^ names.! !
  
!NameSpace methodsFor: 'class names' stamp: 'nowo 7/26/1999 17:29'!
classNames
	"Answer a SortedCollection of all class names."
	| names |
	CachedClassNames == nil ifTrue:
		[names _ OrderedCollection new: self size.
             names addAll: self classes.
             (self allSuperNameSpaces) do: [: each | names addAll: each classes].
		CachedClassNames _ names asSortedCollection].
	^ CachedClassNames! !

!NameSpace methodsFor: 'class names'!
flushClassNameCache
	"This is an implementation efficiency: the collection of class names is 
	saved as a class variable and recomputed whenever the collection is 
	needed but has been previously flushed (set to nil).  Last touched sw 8/91"
	"Smalltalk flushClassNameCache"

	CachedClassNames _ nil! !

!NameSpace methodsFor: 'class names'!
hasClassNamed: aString
	"Answer whether there is a class of the given name, but don't intern aString if it's not alrady interned.  4/29/96 sw"

	Symbol hasInterned: aString ifTrue: 
		[:aSymbol | ^ (self at: aSymbol ifAbsent: [nil]) isKindOf: Class].
	^ false! !

!NameSpace methodsFor: 'class names' stamp: 'di 2/3/1999 22:21'!
removeClassFromSystem: aClass
	"Delete the class, aClass, from the system."

	aClass wantsChangeSetLogging ifTrue:
		[SystemChanges noteRemovalOf: aClass].
	aClass acceptsLoggingOfCompilation ifTrue:
		[Smalltalk logChange:  'Smalltalk removeClassNamed: #', aClass name].
	self removeClassFromSystemUnlogged: aClass
! !

!NameSpace methodsFor: 'class names' stamp: 'di 4/19/1999 10:29'!
removeClassFromSystemUnlogged: aClass
	"Delete the class, aClass, from the system, but log the removal neither to the current change set nor to the changes log"

	SystemOrganization removeElement: aClass name.
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: [].
	self flushClassNameCache
! !

!NameSpace methodsFor: 'class names' stamp: 'sw 9/5/97 18:30'!
removeClassNamed: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	(oldClass _ self at: aName asSymbol ifAbsent: [nil]) == nil
		ifTrue:
			[Transcript cr; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.
			^ self].

	oldClass removeFromSystem! !

!NameSpace methodsFor: 'class names' stamp: 'di 2/3/1999 22:33'!
renameClass: aClass as: newName 
	"Rename the class, aClass, to have the title newName."
	| oldref i |
	SystemOrganization classify: newName under: aClass category.
	SystemOrganization removeElement: aClass name.
	SystemChanges renameClass: aClass as: newName.
	oldref _ self associationAt: aClass name.
	self removeKey: aClass name.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	(Array with: StartUpList with: ShutDownList) do:
		[:list |  i _ list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache! !

!NameSpace methodsFor: 'class names' stamp: 'sw 10/28/96'!
renameClassNamed: oldName as: newName
	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "

	| oldClass |
	(oldClass _ self at: oldName asSymbol ifAbsent: [nil]) == nil
		ifTrue:
			[Transcript cr; show: 'Class-rename for ', oldName, ' ignored because ', oldName, ' does not exist.'.
			^ self].

	oldClass rename: newName! !

!Browser methodsFor: 'class functions' stamp: 'nowo 7/26/1999 17:39'!
defineClass: defString notifying: aController 
	"The receiver's textual content is a request to define a new class. The 
	source code is defString. If any errors occur in compilation, notify 
	aController."
	| oldClass class newClassName |
	oldClass _ self selectedClassOrMetaClass.
	newClassName _ (defString findTokens: Character separators) third copyWithout: $#.
	((oldClass isNil or: [oldClass name asString ~= newClassName])
		and: [NameSpace current includesKey: newClassName asSymbol]) ifTrue:
			["Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [^ false]].
	oldClass ifNil: [oldClass _ Object].
	class _ oldClass subclassDefinerClass
				evaluate: defString
				notifying: aController
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: 
			[self changed: #classList.
			self classListIndex: 
				(self classList indexOf: 
					((class isKindOf: Metaclass)
						ifTrue: [class soleInstance name]
						ifFalse: [class name])).
			self clearUserEditFlag; editClass.
			^true]
		ifFalse: [^false]! !

!Browser methodsFor: 'system category functions' stamp: 'nowo 7/26/1999 17:39'!
findClass
	"Search for a class by name.  Modified so that if only 1 class matches the user-supplied string then the pop-up menu is bypassed"
	| pattern foundClass classNames index |

	self okToChange ifFalse: [^ self classNotFound].
	pattern _ FillInTheBlank request: 'Class name or fragment?'.
	pattern isEmpty ifTrue: [^ self classNotFound].
     NameSpace current flushClassNameCache.
	classNames _ NameSpace current classNames asArray select: 
				[:n | n includesSubstring: pattern caseSensitive: false].
	classNames isEmpty ifTrue: [^ self classNotFound].
	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[(PopUpMenu labelArray: classNames lines: #()) startUp].
	index = 0 ifTrue: [^ self classNotFound].
	foundClass _ NameSpace current at: (classNames at: index).
 	self systemCategoryListIndex: (self systemCategoryList indexOf: foundClass category).
	self classListIndex: (self classList indexOf: foundClass name). 
! !

!Browser methodsFor: 'class list' stamp: 'nowo 7/26/1999 17:38'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ RecentClasses select: [:n | Smalltalk includesKey: n].
	recentList size == 0 ifTrue: [^ self beep].
	className := (SelectionMenu selections: recentList) startUp.
	className == nil ifTrue: [^ self].
	class := NameSpace current at: className.
	self systemCategoryListIndex: (self systemCategoryList indexOf: class category).
	self classListIndex: (self classList indexOf: class name)! !

!Browser methodsFor: 'class functions' stamp: 'nowo 7/26/1999 17:37'!
renameClass
	| oldName newName obs |
	classListIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	oldName _ self selectedClass name.
	newName _ (self request: 'Please type new class name'
						initialAnswer: oldName) asSymbol.
	newName = oldName ifTrue: [^ self].
	(NameSpace current includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	self selectedClass rename: newName.
	self changed: #classList.
	self classListIndex: ((systemOrganizer listAtCategoryNamed: self selectedSystemCategoryName) indexOf: newName).
	obs _ Smalltalk allCallsOn: (NameSpace current associationAt: newName).
	obs isEmpty ifFalse:
		[Smalltalk browseMessageList: obs
			name: 'Obsolete References to ' , oldName
			autoSelect: oldName].
! !

!Browser methodsFor: 'class list' stamp: 'nowo 7/26/1999 17:36'!
selectedClass
	"Answer the class that is currently selected. Answer nil if no selection 
	exists."

	| name |
	(name _ self selectedClassName) ifNil: [^ nil].
	^ NameSpace current at: name! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:53'!
weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have weak indexable pointer variables."
	self isBits 
		ifTrue: 
			[^self error: 
				'cannot make a pointer subclass of a class with non-pointer fields'].
	^self class name: t 
		inEnvironment: NameSpace current
		subclassOf: self 
		instanceVariableNames: f
		variable: true 
		words: true 
		pointers: true
		weak: true
		classVariableNames: d 
		poolDictionaries: s 
		category: cat 
		comment: nil
		changed: false! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:52'!
variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable word-sized nonpointer variables."

	self instSize > 0 
		ifTrue: [^self error: 
					'cannot make a word subclass of a class with named fields'].
	self isBytes
		ifTrue: [^self error: 'cannot make a word subclass of a class with byte fields'].
	(self isVariable and: [self isPointers])
		ifTrue: [^self error: 
					'cannot make a word subclass of a class with pointer fields'].
	^self class name: t 
		inEnvironment: NameSpace current
		subclassOf: self 
		instanceVariableNames: f
		variable: true 
		words: true 
		pointers: false
		weak: false
		classVariableNames: d 
		poolDictionaries: s 
		category: cat 
		comment: nil
		changed: false! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:52'!
variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable pointer variables."

	self isBits 
		ifTrue: 
			[^self error: 
				'cannot make a pointer subclass of a class with non-pointer fields'].
	^self class name: t 
		inEnvironment: NameSpace current
		subclassOf: self 
		instanceVariableNames: f
		variable: true 
		words: true 
		pointers: true
		weak: false
		classVariableNames: d 
		poolDictionaries: s 
		category: cat 
		comment: nil
		changed: false! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:52'!
variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable byte-sized nonpointer variables."

	self instSize > 0 
		ifTrue: [^self error: 'cannot make a byte subclass of a class with named fields'].
	(self isVariable and: [self isWords])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with word fields'].
	(self isVariable and: [self isPointers])
		ifTrue: [^self error: 
					'cannot make a byte subclass of a class with pointer fields'].
	^self class name: t 
		inEnvironment: NameSpace current
		subclassOf: self 
		instanceVariableNames: f
		variable: true 
		words: false 
		pointers: false
		weak: false
		classVariableNames: d 
		poolDictionaries: s 
		category: cat 
		comment: nil
		changed: false! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:52'!
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."

	self isVariable
		ifTrue: 
			[self isPointers 
				ifTrue: [^self
							variableSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: s
							category: cat].
			self isBytes 
				ifTrue: [^self
							variableByteSubclass: t
							instanceVariableNames: f
							classVariableNames: d
							poolDictionaries: s
							category: cat].
			^self
				variableWordSubclass: t
				instanceVariableNames: f
				classVariableNames: d
				poolDictionaries: s
				category: cat].
	^self class
		name: t
		inEnvironment: NameSpace current
		subclassOf: self
		instanceVariableNames: f
		variable: false
		words: true
		pointers: true
		weak: false
		classVariableNames: d
		poolDictionaries: s
		category: cat
		comment: nil
		changed: false! !

!Class methodsFor: 'class name' stamp: 'nowo 7/26/1999 17:51'!
rename: aString 
	"The new name of the receiver is the argument, aString."

	| newName |
	newName _ aString asSymbol.
	(NameSpace current includesKey: newName)
		ifTrue: [^self error: newName , ' already exists'].
	(Undeclared includesKey: newName)
		ifTrue: [^ SelectionMenu notify: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.'].
	NameSpace current renameClass: self as: newName.
	name _ newName.
	self comment: self comment.
! !

!Class methodsFor: 'subclass creation' stamp: 'nowo 7/26/1999 17:50'!
newSubclass
	| i className |
	i _ 1.
	[className _ (self name , i printString) asSymbol.
	 NameSpace current includesKey: className]
		whileTrue: [i _ i + 1].

	^ self subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'UserObjects'

"Point newSubclass new"! !

!Class methodsFor: 'instance variables' stamp: 'nowo 7/26/1999 17:49'!
removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."

	| newInstVarString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newInstVarString _ ''.
	(self instVarNames copyWithout: aString) do: 
		[:varName | newInstVarString _ newInstVarString , ' ' , varName].
	superclass class
		name: self name
		inEnvironment: NameSpace current
		subclassOf: superclass
		instanceVariableNames: newInstVarString
		variable: self isVariable
		words: self isWords
		pointers: self isPointers
		weak: self isWeak
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category
		comment: nil
		changed: false! !

!Class methodsFor: 'instance variables' stamp: 'nowo 7/26/1999 17:50'!
addInstVarName: aString
	"Add the argument, aString, as one of the receiver's instance variables."

	superclass class
		name: self name
		inEnvironment: NameSpace current
		subclassOf: superclass
		instanceVariableNames: self instanceVariablesString , aString
		variable: self isVariable
		words: self isWords
		pointers: self isPointers
		weak: self isWeak
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category
		comment: nil
		changed: false! !


SystemDictionary removeSelector: #removeClassNamed:!
SystemDictionary removeSelector: #classNamed:!
SystemDictionary removeSelector: #removeClassFromSystemUnlogged:!
SystemDictionary removeSelector: #hasClassNamed:!
SystemDictionary removeSelector: #renameClassNamed:as:!
SystemDictionary removeSelector: #classNames!
SystemDictionary removeSelector: #removeClassFromSystem:!
SystemDictionary removeSelector: #flushClassNameCache!
SystemDictionary removeSelector: #renameClass:as:!
SystemDictionary removeSelector: #at:put:!

MouseMenuController subclass: #StandardSystemController
	instanceVariableNames: 'status nameSpace'
	classVariableNames: 'HBorderCursor ScheduledBlueButtonMenu ScheduledBlueButtonMessages VBorderCursor '
	poolDictionaries: ''
	category: 'Interface-Support'!

!StandardSystemController methodsFor: 'scheduling' stamp: 'nowo 7/29/1999 16:41'!
open
	"Create an area on the screen in which the receiver's scheduled view can 
	be displayed. Make it the active view."

	view resizeInitially.
	status _ #open.
	ScheduledControllers scheduleActive: self.
     self nameSpace: NameSpace current.! !

!StandardSystemController methodsFor: 'name space' stamp: 'nowo 7/29/1999 16:39'!
nameSpace
  ^ nameSpace! !

!StandardSystemController methodsFor: 'name space' stamp: 'nowo 7/29/1999 16:40'!
nameSpace: aNameSpace
  (aNameSpace isKindOf: NameSpace) ifFalse: [self error: 'argument has to be a NameSpace'].
  nameSpace _ aNameSpace.! !

!NameSpaceCollection methodsFor: 'printing' stamp: 'vikies 7/25/1999 09:39'!
printOn: aStream 
	"Print names of elements (NameSpaces)."
	aStream nextPut:$(.
	self do: [:element | aStream nextPutAll:(element name asString). aStream space].
	aStream nextPut: $)! !

!NameSpace methodsFor: 'printing' stamp: 'vikik 7/27/1999 14:16'!
printOn: aStream
"Print describtion of receiver (NameSpace) on aStream"
	aStream nextPutAll: 'NameSpace: ',(self name asString).
	(self isSmalltalk)	ifTrue: [ aStream nextPutAll: ' (root)' ].
	aStream space;cr;tab;nextPutAll:'super:'.
	superNameSpaces printOn:aStream.
	aStream cr;tab;nextPutAll:'sub:'.
	subNameSpaces printOn:aStream.
	aStream cr.
	
! !

!SystemDictionary methodsFor: 'dictionary access' stamp: 'vikies 7/25/1999 09:50'!
printOn: aStream
	super printOn:aStream.	"Act as a NameSpace"

	"aStream nextPutAll: 'a SystemDictionary'.
	(self == Smalltalk)
		ifTrue: [ aStream nextPutAll: ' (all the globals)' ]."
! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikik 7/29/1999 09:20'!
addSubCmd
	|aName|
	aName_ FillInTheBlank request:('name of new subNameSpace of ',(self topView controller nameSpace name asString),':').
	self topView controller nameSpace createSubNameSpace: aName asSymbol.
	self updateTree
! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikies 7/28/1999 10:22'!
onGetNameSpace
	|text start end|
	text_NameSpace current printString asText.
	start_text findString: $: startingAt: 1.
	end_text findString: ' ' startingAt: (start+2).	
	^(text makeBoldFrom:(start+1) to: end)
	! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikik 7/29/1999 09:19'!
projectMenu: aMenu
	^ aMenu 
		labels: #( 'set it'
					'inspect it'
					'add new subNameSpace'
					'update'
					'show siblings')
		lines: #(2)
		selections: #(setNameSpace inspectIt addSubCmd updateTree showSiblings).! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikik 7/29/1999 15:17'!
setNameSpace
	"get a selection from view's ctrl (a string), resolve it to 
	NameSpace & set it"
	| newName |
	newName _ viewNS controller selection asString asSymbol.
	self topView controller nameSpace: (self topView controller nameSpace perform: newName).
	NameSpace setTo: self topView controller nameSpace.
	self updateAfterNSchange.
	! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikik 7/27/1999 14:19'!
showSiblings
	| theirNames aString|
	theirNames _ (NameSpace current allBrothers collect: [:each | each name]) asString.
	aString_(self onGetNameSpace), (NameSpace current name asString),' siblings are that in ', theirNames asString.
	viewNS controller changeText: aString asText.! !

!EnvironmentalWorkspace methodsFor: 'actions' stamp: 'vikies 7/28/1999 10:29'!
updateTree
	self changed:#onGetNameSpace! !


!EnvironmentalWorkspace methodsFor: 'private' stamp: 'vikik 7/29/1999 15:00'!
addExtraViewsOn: model in: rect to: topView


	viewNS _ PluggableTextView on: self 
			text: #onGetNameSpace accept: nil
			readSelection: #contentsSelection menu: #projectMenu:. "#codePaneMenu:shifted:."
	viewNS window: rect.
	topView addSubView: viewNS.! !

!EnvironmentalWorkspace methodsFor: 'private' stamp: 'vikik 7/27/1999 16:06'!
updateAfterNSchange
	self topView relabel:('Workspace in ',NameSpace current fullName).
	^self changed:#onGetNameSpace! !


!EnvironmentalWorkspace methodsFor: 'initialize-release' stamp: 'vikik 7/29/1999 15:15'!
openLabel: aString  andTerminate: terminateBoolean 
	"Create a standard system view of the model, me, a StringHolder and open it.; do not terminate the active process if in mvc"
	| topView codeView |
	World ifNotNil: [^ self openAsMorphLabel: aString].
	topView _ (StandardSystemView new) model: self.
	topView borderWidth: 1.
	topView label: (aString,' in ',NameSpace current fullName).
	topView minimumSize: 150 @ 70.
	self addExtraViewsOn: self in: (0@0 extent: 150@20) to: topView.
	codeView _ PluggableTextView on: self 
			text: #contents accept: #acceptContents:
			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.
	codeView window: (0@20 extent: 150@100).
	topView addSubView: codeView.
	"self contents size > 0 ifTrue: [
			codeView hasUnacceptedEdits: true].  Is it already saved or not??"

	terminateBoolean
		ifTrue:
			[topView controller open]
		ifFalse:
			[topView controller openNoTerminate].
	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

EnvironmentalWorkspace class
	instanceVariableNames: ''!

!EnvironmentalWorkspace class methodsFor: 'instance creation' stamp: 'vikik 7/29/1999 15:03'!
open 
	self openLabel:'Workspace' ! !

!EnvironmentalWorkspace class methodsFor: 'instance creation' stamp: 'vikik 7/27/1999 16:31'!
openLabel: aString

	^self new openLabel: aString andTerminate:true! !

!ScreenController methodsFor: 'menu messages' stamp: 'vikik 7/27/1999 17:30'!
openEnvWorkspace
	"create an environmental workspace"
	EnvironmentalWorkspace open! !



!EnvironmentalBrowser methodsFor: 'inform actions' stamp: 'vikies 7/28/1999 10:44'!
getInformText
	|text|
	text_NameSpace current fullName asText allBold.
	^(('Current name space is: ' asText),text).! !


!EnvironmentalBrowser methodsFor: 'initialize-release' stamp: 'vikies 7/28/1999 09:40'!
defaultBackgroundColor
	^ Color r: 0.8 g: 0.8 b: 0.5! !

!EnvironmentalBrowser methodsFor: 'initialize-release' stamp: 'vikik 7/29/1999 06:47'!
openEditString: aString
	"Create a pluggable version of all the views for a Browser, including views and controllers."
	| systemCategoryListView classListView 
	messageCategoryListView messageListView browserCodeView topView switchView enviroView informView|

	World ifNotNil: [^ self openAsMorphEditing: aString].
	Sensor leftShiftDown ifTrue: [^ self openAsMorphEditing: aString "testing"].

	topView _ (StandardSystemView new) model: self.
	topView borderWidth: 1.
		"label and minSize taken care of by caller"

	enviroView _PluggableListView on: self
		list: #nameSpaceList
		selected: #selectedNameSpaceIndex
		changeSelected: #changeSelected:
		menu: #nameSpaceMenu:
		keystroke: nil.
	enviroView window: (0 @ 0 extent: 50 @ 70).
	enviroView menuTitleSelector: nil.
	topView addSubView: enviroView.

	systemCategoryListView _ PluggableListView on: self
		list: #systemCategoryList
		selected: #systemCategoryListIndex
		changeSelected: #systemCategoryListIndex:
		menu: #systemCategoryMenu:.
	systemCategoryListView window: (0 @ 0 extent: 50 @ 70).
	topView addSubView: systemCategoryListView toRightOf: enviroView.

	classListView _ PluggableListView on: self
		list: #classList
		selected: #classListIndex
		changeSelected: #classListIndex:
		menu: #classListMenu:
		keystroke: #classListKey:from:.
	classListView window: (0 @ 0 extent: 50 @ 62).
	topView addSubView: classListView toRightOf: systemCategoryListView.

	switchView _ self buildInstanceClassSwitchView.
	switchView borderWidth: 1.
	topView addSubView: switchView below: classListView.

	messageCategoryListView _ PluggableListView on: self
		list: #messageCategoryList
		selected: #messageCategoryListIndex
		changeSelected: #messageCategoryListIndex:
		menu: #messageCategoryMenu:.
	messageCategoryListView window: (0 @ 0 extent: 50 @ 70).
	topView addSubView: messageCategoryListView toRightOf: classListView.

	messageListView _ PluggableListView on: self
		list: #messageList
		selected: #messageListIndex
		changeSelected: #messageListIndex:
		menu: #messageListMenu:shifted:
		keystroke: #messageListKey:from:.
	messageListView window: (0 @ 0 extent: 50 @ 70).
	messageListView menuTitleSelector: #messageListSelectorTitle.
	topView addSubView: messageListView toRightOf: messageCategoryListView.

	browserCodeView _ PluggableTextView on: self 
			text: #contents accept: #contents:notifying:
			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.
	browserCodeView window: (0@0 extent: 250@110).
	topView addSubView: browserCodeView below: enviroView.

	informView _ PluggableTextView on: self 
			text: #getInformText accept: nil
			readSelection: nil menu: nil.
	informView window: (0@0 extent: 250@10).
	topView addSubView: informView above: enviroView.

	aString ifNotNil: [browserCodeView editString: aString.
			browserCodeView hasUnacceptedEdits: true].
	^ topView
! !

!EnvironmentalBrowser methodsFor: 'initialize-release' stamp: 'vikik 7/29/1999 06:37'!
systemOrganizer: aSystemOrganizer 
	self buildNameSpaceList.
	"NameSpace addDependent:self."
	^super systemOrganizer: aSystemOrganizer! !


!EnvironmentalBrowser methodsFor: 'private' stamp: 'vikies 7/28/1999 13:09'!
buildNameSpaceList
	"build list of namespace names (textOrString) ordered by hierarchy (tabs in front of names)"
	| nameSpace brothers |
	nameSpaceList _ OrderedCollection new.

	nameSpace _ NameSpace current.
	heldNameSpace_nameSpace.
	nameSpaceList addAll: (nameSpace superNameSpaces collectToOrdCollection: [:each | each name asString]).
	nameSpaceList size=0 
		ifTrue:[	nameSpaceList add: (( nameSpace name asString) asText allBold,' (root)' ) ]
		ifFalse:[nameSpaceList add: (('	' , nameSpace name asString) asText allBold)].
	nameSpaceIndex_ nameSpaceList size.
	nameSpaceList addAll: (nameSpace subNameSpaces collectToOrdCollection: [:each | '		',(each name asString)]).
	brothers_nameSpace allBrothers select:[:each| each name~=(nameSpace name)].
	nameSpaceList addAll:
		 (brothers collect:	[:each| '	' , each name asString]).
	^nameSpaceList
! !

!EnvironmentalBrowser methodsFor: 'private' stamp: 'vikies 7/28/1999 14:11'!
resolveSelected:anIndex
	"resolve NameSpace from aText in nameSpaceList"
	|text start|
	(anIndex = 0) ifTrue: [text _ heldNameSpace name]
                     ifFalse: [text_(nameSpaceList at:anIndex)].
	text_text asString.
	start_text skipDelimiters:(Character tab asString) startingAt:1.
	text_text copyFrom:start to:(text size).
	start_text findDelimiters:(Character space asString) startingAt:1.
	text size<start ifFalse:[text_text copyFrom:1 to:start].
	^heldNameSpace perform:(text asSymbol)! !

!EnvironmentalBrowser methodsFor: 'private' stamp: 'vikik 7/29/1999 07:07'!
setChange
	heldNameSpace_NameSpace current.
	self buildNameSpaceList.
	self updateCmd! !


!EnvironmentalBrowser methodsFor: 'updating' stamp: 'vikik 7/29/1999 06:59'!
clickUpdate
			((NameSpace current)~~heldNameSpace) ifTrue:[NameSpace setTo: (Compiler evaluate:('NameSpace current ',(heldNameSpace fullName)))]
		! !

!EnvironmentalBrowser methodsFor: 'updating' stamp: 'vikies 7/28/1999 12:44'!
update: aParam
	(aParam=#onNameSpaceSwitch) ifTrue:[self updateCmd].! !


!EnvironmentalBrowser methodsFor: 'enviro menu commands' stamp: 'vikik 7/29/1999 07:09'!
setItCmd
	NameSpace setTo:(self resolveSelected: nameSpaceIndex).
	self setChange! !

!EnvironmentalBrowser methodsFor: 'enviro menu commands' stamp: 'vikies 7/28/1999 12:43'!
updateCmd
	self buildNameSpaceList.
	self changed:#nameSpaceList.
	self changed:#selectedNameSpaceIndex.
	self changed:#getInformText.! !

!EnvironmentalBrowser methodsFor: 'enviro menu commands' stamp: 'vikik 7/29/1999 08:50'!
createSubCmd
	|aName|
	aName_ FillInTheBlank request:('name of new subNameSpace of ',(heldNameSpace name asString),':').
	heldNameSpace createSubNameSpace: aName asSymbol.
	self updateCmd! !

!EnvironmentalBrowser methodsFor: 'dependents' stamp: 'vikies 7/28/1999 12:35'!
removeDependent: anObject
	(anObject==(self topView)) ifTrue:[ NameSpace removeDependent:self]. "closing workspace"
	^super removeDependent: anObject! !


!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'vikik 7/29/1999 06:51'!
changeSelected:	anIndex
	
	nameSpaceIndex_anIndex.
	self setItCmd
	! !

!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'vikik 7/29/1999 06:51'!
nameSpaceList

	NameSpace current == heldNameSpace ifFalse:[^self buildNameSpaceList].
	^nameSpaceList! !

!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'nowo 8/4/1999 17:40'!
nameSpaceMenu:aMenu
	^ aMenu 
		labels: #( 
					'create subNameSpace'
					'add subNameSpace'
					'add superNameSpace'
					'update'
                        'hierarchy'
					)
		lines: #(3)
		selections: #( createSubCmd addSubCmd addSuperCmd updateCmd nsHierarchy).! !

!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'vikik 7/29/1999 06:51'!
selectedNameSpaceIndex

	^nameSpaceIndex! !


!EnvironmentalBrowser methodsFor: 'metaclass' stamp: 'vikik 7/29/1999 06:57'!
classMessagesIndicated
	self clickUpdate.
	^super classMessagesIndicated.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

EnvironmentalBrowser class
	instanceVariableNames: ''!

!EnvironmentalBrowser class methodsFor: 'instance creation' stamp: 'vikies 7/28/1999 09:38'!
openBrowser
	"Create and schedule a BrowserView with label 'System Browser'. The 
	view consists of five subviews, starting with the list view of system 
	categories of SystemOrganization. The initial text view part is empty."

	Browser openBrowserView: (self new openEditString: nil)
			label: 'NameSpace Browser'
! !

!NameSpaceCollection methodsFor: 'enumerating' stamp: 'vikies 7/28/1999 11:55'!
collectToOrdCollection: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into an OrderedCollection. Answer the new 
	collection. Override superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection _ OrderedCollection new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection! !


!ScreenController methodsFor: 'menu messages' stamp: 'vikies 7/28/1999 14:14'!
openEnvBrowser
	EnvironmentalBrowser openBrowser! !

!ScreenController methodsFor: 'nested menus' stamp: 'vikies 7/28/1999 14:13'!
openMenu
	^ SelectionMenu labelList:
		#(	'keep this menu up'

			'browser'
			'workspace'
			'name space browser'
			'name space workspace'
			'file list'
			'transcript'
			'selector finder'

			'simple change sorter'
			'dual change sorter'

			'project (mvc)'
			'project (morphic)'
			'project (construction)'
			)
		lines: #(1 8 10)
		selections: #(durableOpenMenu
openBrowser openWorkspace openEnvBrowser openEnvWorkspace openFileList openTranscript openSelectorBrowser
openSimpleChangeSorter openChangeManager
openProject  openMorphicProject  openConstructionProject )
"
ScreenController  new openMenu startUp
"! !

!EnvironmentalBrowser methodsFor: 'enviro menu commands' stamp: 'vikik 7/29/1999 11:11'!
addSuperCmd
	| all reply |
	all _ SortedCollection new.
	all _ (NameSpace allInstances collect: [:each | each fullName]) asSortedCollection.
	all addAll: (SystemDictionary allInstances collect: [:each | each fullName]) asSortedCollection.
	reply _ (SelectionMenu labelList: all selections: all) startUp.
	heldNameSpace addSuperNameSpace:(Compiler evaluate:reply).
	self updateCmd! !

!EnvironmentalBrowser methodsFor: 'enviro menu commands' stamp: 'vikik 7/29/1999 11:11'!
addSubCmd
	| all reply |
	all _ SortedCollection new.
	all _ (NameSpace allInstances collect: [:each | each fullName]) asSortedCollection.
	all addAll: (SystemDictionary allInstances collect: [:each | each fullName]) asSortedCollection.
	reply _ (SelectionMenu labelList: all selections: all) startUp.
	heldNameSpace addSubNameSpace:(Compiler evaluate:reply).
	self updateCmd! !

!NameSpace methodsFor: 'encoding' stamp: 'nowo 7/29/1999 18:27'!
checkVariable: aSymbol withAssociation: anAssociation
	(self includesKey: aSymbol)
		ifFalse: [(self findNameSpaceOf: aSymbol ifAbsent: []) isNil
				ifFalse: [(self confirm: aSymbol , ' was found in one of the superNameSpaces. Create new association?')
						ifTrue: 
							[self at: aSymbol put: nil.
							^ self associationAt: aSymbol ifAbsent: [self error: 'bad thing happened']]]].
  ^ anAssociation.! !

!StringHolder methodsFor: 'evaluation' stamp: 'nowo 7/30/1999 13:06'!
doItReceiver
	"Answer the object that should be informed of the result of evaluating a 
	text selection."

	^ NameSpace current UndefinedObject new.! !

!Metaclass methodsFor: 'class hierarchy' stamp: 'nowo 8/3/1999 18:08'!
name: newName inEnvironment: environ subclassOf: sup instanceVariableNames: instVarString variable: v words: w pointers: p weak: beWeak classVariableNames: classVarString poolDictionaries: poolString category: categoryName comment: commentString changed: changed 
	"This is the standard initialization message for creating a new Metaclass. 
	Answer an instance of me from the information provided in the 
	arguments. Create an error notification if the name does not begin with 
	an uppercase letter or if a class of the same name already exists.
	1/22/96 sw: don't ever do addClass, always do changeClass"

	| wasPresent oldClass newClass invalidFields invalidMethods |
	newName first isUppercase
		ifFalse: 
			[self error: 'Class names must be capitalized'.
			^false].
	(wasPresent _ environ includesKey: newName)
		ifTrue: 
			[oldClass _ environ at: newName.
			(oldClass isKindOf: Behavior)
				ifFalse: 
					[self error: newName , ' already exists!!  Proceed will store over it'.
					wasPresent _ false.
					oldClass _ self newNamed: newName].
			(oldClass checkForInstVarsOK: instVarString)
				ifFalse: [^ false]]
		ifFalse: [oldClass _ self newNamed: newName.
				Smalltalk flushClassNameCache].
	newClass _ oldClass copy.
	invalidFields _ changed | (newClass
					subclassOf: sup
					oldClass: oldClass
					instanceVariableNames: instVarString
					variable: v
					words: w
					pointers: p
					weak: beWeak
					ifBad: [^false]).
	invalidFields not & (oldClass instSize = newClass instSize)
		ifTrue: [newClass _ oldClass].
	invalidMethods _ invalidFields | (newClass declare: classVarString) | 
		(newClass sharing: poolString).
	commentString == nil ifFalse: [newClass comment: commentString].
	(environ includesKey: newName)
		ifFalse: [environ declare: newName from: Undeclared].
	environ at: newName put: newClass.
	SystemOrganization classify: newClass name under: categoryName asSymbol.
	newClass
		validateFrom: oldClass
		in: environ
		instanceVariableNames: invalidFields
		methods: invalidMethods
		wasPresent: wasPresent.
     newClass nameSpace: environ.
	"update subclass lists"
	newClass superclass removeSubclass: oldClass.
	newClass superclass addSubclass: newClass.
	"Update Changes"
	Smalltalk changes changeClass: newClass.
	^ newClass! !

!Parser methodsFor: 'expression types' stamp: 'nowo 8/4/1999 11:13'!
assignment: varNode 
	" var '_' expression => AssignmentNode."
	| loc |
	(varNode key isKindOf: Association)
		ifTrue: [varNode key: (NameSpace current checkVariable: varNode key key withAssociation: varNode key)
				code: varNode code].
	(loc _ varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0 ifTrue: [^ self notify: 'Cannot store into' at: loc].
	varNode nowHasDef.
	self advance.
	self expression ifFalse: [^ self expected: 'Expression'].
	parseNode _ AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder.
	^ true! !

!NameSpace methodsFor: 'fileOut' stamp: 'nowo 8/4/1999 11:56'!
fileOut
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| fileStream first classes |
	fileStream _ FileStream newFileNamed: (self name) , '.st'.

     first _ true.
     classes _ OrderedCollection new.
	self do: [:each | (each isKindOf: Class) ifTrue: [classes add: each]]. 
     classes do: 
		[:class | 
		first
			ifTrue: [first _ false]
			ifFalse: [fileStream cr; nextPut: Character newPage; cr].
		class
			fileOutOn: fileStream
			moveSource: false
			toFile: 0].
	fileStream close.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'nowo 8/4/1999 14:27'!
allClasses  "Smalltalk allClasses"
	^ self classNames collect: [:name | NameSpace current at: name]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'nowo 8/4/1999 14:27'!
allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system."

	(self classNames collect: [:name | NameSpace current at: name]) do: aBlock! !

!ScreenController methodsFor: 'menu messages' stamp: 'nowo 8/4/1999 15:46'!
changeNameSpace
	| all reply temp|

	all _ SortedCollection new.
	all _ (NameSpace allInstances collect: [:each | each fullName]) asSortedCollection.
	all addAll: (SystemDictionary allInstances collect: [:each | each fullName]) asSortedCollection.
	reply _ (SelectionMenu labelList: all selections: all) startUp.
     temp _ (Compiler evaluate:reply).
	(temp isKindOf: NameSpace) ifTrue: [NameSpace setTo: temp].
! !

!ScreenController methodsFor: 'nested menus' stamp: 'nowo 8/4/1999 15:45'!
projectScreenMenu
	"Answer the project screen menu."

	^ SelectionMenu labelList:
		#(	'keep this menu up'

			'previous project'
			'jump to project...'
		     'change name space'
			'restore display'

			'open...'
			'windows...'
			'changes...'
			'help...'
			'do...'

			'save'
			'save as...'
			'save and quit'
			'quit')
		lines: #(1 4 9)
		selections: #(durableScreenMenu
returnToPreviousProject jumpToProject changeNameSpace restoreDisplay
presentOpenMenu presentWindowMenu presentChangesMenu presentHelpMenu commonRequests
snapshot saveAs snapshotAndQuit quit )
"
ScreenController new projectScreenMenu startUp
"! !

!NameSpace methodsFor: 'printing' stamp: 'nowo 8/4/1999 17:16'!
printHierarchy
  	| aStream index |
	index _ 0.
	aStream _ WriteStream on: (String new: 16).
	self path do: 
		[:nspace | 
		aStream crtab: index.
		index _ index + 1.
		aStream nextPutAll: nspace name.
		aStream space.
		aStream nextPutAll: ((nspace allBrothers size > 0) ifTrue: ['...'] ifFalse: [''])].
	aStream cr.
	^ aStream contents.! !

!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'nowo 8/4/1999 17:51'!
contents
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."
	| comment theClass |
	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	editSelection == #newClass 
		ifTrue: [^ Class template: self selectedSystemCategoryName].
	editSelection == #editClass 
		ifTrue: [^ self selectedClassOrMetaClass definition].
	editSelection == #editComment 
		ifTrue: [(theClass _ self selectedClass) ifNil: [^ ''].
				comment _ theClass comment.
				comment size = 0
				ifTrue: [ ^ 'This class has not yet been commented.']
				ifFalse: [ ^ comment]].
	editSelection == #hierarchy 
		ifTrue: [^ self selectedClassOrMetaClass printHierarchy].
     editSelection == #nsHierarchy
          ifTrue: [^ heldNameSpace printHierarchy].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [^ self selectedClassOrMetaClass sourceCodeTemplate].
	editSelection == #editMessage
		ifTrue: [^ self selectedMessage].
	editSelection == #byteCodes ifTrue: [
		^ (self selectedClassOrMetaClass compiledMethodAt: 
				self selectedMessageName) symbolic asText].

	self error: 'Browser internal error: unknown edit selection.'! !

!EnvironmentalBrowser methodsFor: 'enviro actions' stamp: 'nowo 8/4/1999 17:50'!
nsHierarchy
	"Display the inheritance hierarchy of the receiver's selected class."
	self okToChange ifFalse: [^ self].
	self messageCategoryListIndex: 0.
	editSelection := #nsHierarchy.
	self changed: #editComment.
	self contentsChanged.
	^ self! !


