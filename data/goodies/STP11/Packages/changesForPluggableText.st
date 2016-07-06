"changesForPluggableText.st
These are the simple changes necessary to make pluggable text views work.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   Object-testing.st (IMPORTANT)
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 25, 1996"!


!MouseMenuController methodsFor: 'menu messages'!
unshiftedYellowButtonActivity
	"Put up the regular yellow-button menu and take action as appropriate.  1/24/96 sw"
	"Added support for SelectionMenus (which return symbols) 1996.11.24--stp"

	| index  |
	yellowButtonMenu ~~ nil
		ifTrue: 
			[index _ yellowButtonMenu startUp.
			((index ~= 0) and: [index notNil])
				ifTrue: [self menuMessageReceiver performMenuMessage:
						(index isSymbol
							ifTrue: [index]
							ifFalse: [yellowButtonMessages at: index])]]
		ifFalse:
			[super controlActivity]! !

!PluggableListView methodsFor: 'model access'!
changeModelSelection: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setSelectionSelector ~~ nil
		ifTrue: [item _ (anInteger = 0 ifTrue: [nil] ifFalse: [items at: anInteger]).
			(item = (model perform: getSelectionSelector))
				ifTrue: [model perform: setSelectionSelector with: nil]
				ifFalse: [model perform: setSelectionSelector with: item]]! !

!PositionableStream methodsFor: 'accessing'!
upToAll: aCollection
	"Answer a subcollection from the current access position to the 
	occurrence (if any, not inclusive) of aCollection in the receiver. If 
	aCollection is not in the receiver, answer the entire rest of the receiver."
	"(ReadStream on: 'abcdefghijklmnop') upToAll: 'lmn' "
	"(ReadStream on: 'abcldefglmhijklmnop') upToAll: 'lmn' "
	"(ReadStream on: 'abcldefglmhijklmnop') upToAll: 'lqn' "

	| stream writer ch |
	stream := WriteStream on: String new.
		[stream nextPutAll: (self upTo: aCollection first).
		self atEnd
			ifTrue: [^stream contents].
		writer := WriteStream on: String new.
		writer nextPut: aCollection first.
		aCollection size - 1 timesRepeat:
			[ch := self next.
			ch isNil
				ifTrue: [stream nextPutAll: writer contents.
					^stream contents].
			writer nextPut: ch].
		writer contents = aCollection
			ifTrue: [^stream contents]
			ifFalse: [stream nextPutAll: writer contents].
		true] whileTrue! !

!SelectionMenu class methodsFor: 'instance creation'!
labels: aString lines: linesArray selectors: selectionsArray
	"Answer an instance of me whose items are in aString, with lines drawn 
	after each item indexed by anArray. Record the given array of selections
	corresponding to the items in labelsArray."

	^ (self labels: aString lines: linesArray) selections: selectionsArray! !

!StringHolderView methodsFor: 'updating'!
updateDisplayContents
	"Make the text that is displayed be the contents of the receiver's model."

	self editString: self getModelText.
	self displayView! !

!StringHolderView methodsFor: 'model access'!
getModelText
	"Get the model's text contents."
 
	^model contents!

model: aLockedModel 
	"Refer to the comment in View|model:."
 
	super model: aLockedModel.
	self editString: self getModelText! !
