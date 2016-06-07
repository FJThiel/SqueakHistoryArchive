'From Squeak3:7 of ''4 September 2004'' [latest update: #5989] on 16 December 2007 at 10:36:49 am'!
Object subclass: #SystemProgressMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3.9Compat'!

!SystemProgressMorph commentStamp: '<historical>' prior: 0!
Dummy!
Symbol subclass: #ByteSymbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3.9Compat'!

!ByteSymbol commentStamp: '<historical>' prior: 0!
Dummy!
String subclass: #ByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: '3.9Compat'!

!ByteString commentStamp: '<historical>' prior: 0!
Dummy!

!Object methodsFor: 'accessing' stamp: 'kph 12/16/2007 08:05'!
ifNil: nilBlock ifNotNilDo: aBlock 
	"Evaluate aBlock with the receiver as its argument."

	^ aBlock value: self! !

!Object methodsFor: 'accessing' stamp: 'kph 12/16/2007 08:07'!
ifNotNilDo: aBlock ifNil: nilBlock
	"Evaluate aBlock with the receiver as its argument."

	^ aBlock value: self
! !


!PackageOrganizer class methodsFor: 'as yet unclassified' stamp: 'kph 12/16/2007 10:25'!
default

^ self new! !


!UndefinedObject methodsFor: 'testing' stamp: 'kph 12/16/2007 08:06'!
ifNil: nilBlock ifNotNilDo: ifNotNilBlock
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value! !

!UndefinedObject methodsFor: 'testing' stamp: 'kph 12/16/2007 08:07'!
ifNotNilDo: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am nil, so evaluate the block nilBlock"

	^ nilBlock value! !

!ReadWriteStream methodsFor: 'accessing' stamp: 'kph 2/26/2009 05:53'!
setConverterForCode


	"LPF: dummy method for compatability with 3.8+ packages"! !

!Class methodsFor: 'fileIn/Out' stamp: 'ar 4/10/2005 20:27'!withClassVersion: aVersion	aVersion = self classVersion ifTrue:[^self].	^self error: 'Invalid class version'! !