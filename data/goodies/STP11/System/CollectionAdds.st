"CollectionAdds.st
Several useful collection extensions--second, third, etc., with:with:with:with:with:, etc.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    December 7, 1994"!

!SequenceableCollection methodsFor: 'accessing'!
fifth
	"Answer the fifth element of the receiver.  Provide an error 
	notification if the receiver contains no elements."

	^self at: 5!

fourth
	"Answer the fourth element of the receiver.  Provide an error 
	notification if the receiver contains no elements."

	^self at: 4!

second
	"Answer the second element of the receiver.  Provide an error 
	notification if the receiver contains no elements."

	^self at: 2!

third
	"Answer the third element of the receiver.  Provide an error 
	notification if the receiver contains no elements."

	^self at: 3! !

!Collection methodsFor: 'enumerating'!
sum
	"Answer the numerical sum of the elements in the receiver."

	^self inject: 0 into: [ :subTotal :next | subTotal + next]! !


!ArrayedCollection class methodsFor: 'instance creation'!
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifth with: sixth
	"Answer a new instance of ArrayedCollection, containing the four 
	arguments as the elements."

	| newCollection |
	newCollection := self new: 6.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifth.
	newCollection at: 6 put: sixth.
	^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifth with: sixth with: seventh
	"Answer a new instance of ArrayedCollection, containing the four 
	arguments as the elements."

	| newCollection |
	newCollection := self new: 7.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifth.
	newCollection at: 6 put: sixth.
	newCollection at: 7 put: seventh.
	^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifth with: sixth with: seventh with: eighth
	"Answer a new instance of ArrayedCollection, containing the four 
	arguments as the elements."

	| newCollection |
	newCollection := self new: 8.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifth.
	newCollection at: 6 put: sixth.
	newCollection at: 7 put: seventh.
	newCollection at: 8 put: eighth.
	^newCollection! !

!SequenceableCollection methodsFor: 'accessing'!
prevIndexOfSubCollection: aSubCollection startingAt: anIndex
	"Answer the previous index of anElement within the receiver between startIndex and stopIndex
	 working backwards through the receiver.  If the receiver does not contain anElement, answer nil"
	"'12345678901234567890' prevIndexOfSubCollection: '234' startingAt: 11"

	| subSize firstElement index |
	subSize := aSubCollection size.
	subSize = 0 ifTrue: [^0].
	firstElement := aSubCollection at: 1.
	subSize = 1
		ifTrue: [^self prevIndexOf: firstElement from: anIndex to: 0].
	anIndex to: 1 by: -1 do:
		[:i |
		(self at: i) = firstElement
			ifTrue: [index := 2.
				[(self at: i + index - 1) = (aSubCollection at: index)]
					whileTrue:
						[index = subSize ifTrue: [^i].
						index := index + 1]]].
	^0! !

