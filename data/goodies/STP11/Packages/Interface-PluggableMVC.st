"Interface-PluggableMVC.st
Here's a simple pluggable text view/controller. 
The outline browser application is a good example of its use.
I put it in a separate class category in the hope that the 'powers that be' would move the pluggable list there in the next release as well.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   changesForPluggableText.st
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 25, 1996"!


StringHolderController subclass: #PluggableTextController
	instanceVariableNames: 'aspect menuSelector changeSelector initialSelection '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-PluggableMVC'!

!PluggableTextController methodsFor: 'accessing'!
aspect
	"Answer the receiver's 'aspect'."

	aspect isNil
		ifTrue: [aspect := view aspect].
	^aspect!
changeSelector
	"Answer the receiver's 'changeSelector'."

	changeSelector isNil
		ifTrue: [changeSelector := view changeSelector].
	^changeSelector!
initialSelection
	"Answer the receiver's 'initialSelection'."

	initialSelection isNil
		ifTrue: [initialSelection := view initialSelection].
	^initialSelection!
menuSelector
	"Answer the receiver's 'menuSelector'."

	menuSelector isNil
		ifTrue: [menuSelector := view menuSelector].
	^menuSelector! !

!PluggableTextController methodsFor: 'private'!
initializeYellowButtonMenu

	| yMenu |
	model isNil
		ifTrue: [^self].
	yMenu := model perform: self menuSelector.
	self yellowButtonMenu: yMenu
		yellowButtonMessages: yMenu selections! !

StringHolderView subclass: #PluggableTextView
	instanceVariableNames: 'aspect changeSelector menuSelector initialSelection '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-PluggableMVC'!

!PluggableTextView methodsFor: 'initialize-release'!
initialize
	"Initialize the receiver with the given model"

	model isNil
		ifFalse: [super initialize]!
initializeWith: theModel
	"Initialize the receiver with the given model"

	model := theModel.
	self initialize.
	model := nil.
	self model: theModel! !

!PluggableTextView methodsFor: 'accessing'!
aspect
	"Answer the receiver's 'aspect'."

	^aspect!
aspect: anObject
	"Set the receiver's instance variable 'aspect' to be anObject."

	aspect := anObject!
changeSelector
	"Answer the receiver's 'changeSelector'."

	^changeSelector!
changeSelector: anObject
	"Set the receiver's instance variable 'changeSelector' to be anObject."

	changeSelector := anObject!
initialSelection
	"Answer the receiver's 'initialSelection'."

	^initialSelection!
initialSelection: anObject
	"Set the receiver's instance variable 'initialSelection' to be anObject."

	initialSelection := anObject!
menuSelector
	"Answer the receiver's 'menuSelector'."

	^menuSelector!
menuSelector: anObject
	"Set the receiver's instance variable 'menuSelector' to be anObject."

	menuSelector := anObject!
setModel: anObject
	"Set the receiver's instance variable 'model' to be anObject."

	model := anObject! !

!PluggableTextView methodsFor: 'updating'!
update: aSymbol
	"Refer to the comment in View|update:."

	aSymbol == aspect
		ifTrue: [^self updateDisplayContents].
	aSymbol == #wantToChange
		ifTrue: [^self promptForCancel].
	aSymbol == #flash
		ifTrue: [^controller flash].
	! !

!PluggableTextView methodsFor: 'model access'!
getModelText
	"Answer the model's text contents."
 
	aspect isNil
		ifTrue: [^''].
	^model perform: aspect! !

!PluggableTextView methodsFor: 'controller access'!
defaultControllerClass 
	"Refer to the comment in View|defaultControllerClass."

	^PluggableTextController! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

PluggableTextView class
	instanceVariableNames: ''!

!PluggableTextView class methodsFor: 'instance creation'!
on: model
	"Answer an instance of the receiver with the given model."

	| inst |
	inst := self new.
	inst initializeWith: model.
	^inst!
on: model aspect: aspect change: change menu: menu
	"Answer an instance of the receiver with the given attributes."

	| inst |
	inst := self new.
	inst aspect: aspect.
	inst changeSelector: change.
	inst menuSelector: menu.
	inst initializeWith: model.
	inst controller initializeYellowButtonMenu.
	^inst! !
