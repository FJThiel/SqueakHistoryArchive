"ProtoBrowserFix.st
In 1.2 the ´spawn protocol´ function of the SystemBrowser is broken since String lost a couple of compatibility methods. Also remove the unused instvar ´classDictionary´.

	TITLE			ProtoBrowserFix.st
	AUTHOR			Georg Gollmann (gollmann@edvz.tuwien.ac.at)
	VERSION			1.1
	IMAGE VERSION	1.2
	PREREQUISITES	none
	DATE			July 3, 1997"!
	
'From Squeak 1.2 of June 29, 1997 on 3 July 1997 at 12:42:52 pm'!

!ProtocolBrowser methodsFor: 'private'!
parse: messageString toClassAndSelector: csBlock
        "Decode strings of the form <className> [class] <selectorName>."
        | tuple cl |
        tuple _ messageString findTokens: ' 	'.
        cl _ tuple at: 2.
        cl _ (cl copyFrom: 2 to: cl size - 1) asSymbol.
        cl _ Smalltalk at: cl.
        self selectedClass: cl.
        self setSelector: tuple first.
        tuple size = 2
                ifTrue: [^ csBlock value: cl value: tuple first asSymbol]
                ifFalse: [^ csBlock value: cl class value: tuple first asSymbol]! !
!ProtocolBrowser methodsFor: 'private'!
onSubProtocolOf: aClass 
	"Create the protocol browser for the class, a Class."

	| defClass aSet aCollection |
	messageList := OrderedCollection new.
	selectedClass := aClass.
	aSet := Set new.
	aCollection := aClass withAllSuperclasses.
	aCollection remove: Object.
	aCollection do: [:each | aSet addAll: each selectors].
	aSet asSortedCollection do: [ :selector | 
		defClass := aClass whichClassIncludesSelector: selector.
		self selectorList add: selector, '		(' , defClass name , ')'.
	]! !
!ProtocolBrowser methodsFor: 'private'!
on: aClass 
	"Create the protocol browser for the class, a Class."

	| defClass |
	self selectorList: OrderedCollection new.
	aClass allSelectors asSortedCollection do: [ :selector | 
		defClass := aClass whichClassIncludesSelector: selector.
		self selectorList add: selector, '     (' , defClass name , ')'.
	]! !
ProtocolBrowser removeSelector: #classDictionary:!
ProtocolBrowser removeSelector: #classDictionary!
ProtocolBrowser removeInstVarName: 'classDictionary'!
ProtocolBrowser removeInstVarName: 'baseClass'!
ProtocolBrowser removeClassVarName: 'TextMenu'!
