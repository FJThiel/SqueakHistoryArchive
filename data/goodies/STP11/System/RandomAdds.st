"RandomAdds.st
A few new messages for generating random numbers in limited ranges, or selecting from collections at random.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    December 7, 1994"!

!Random methodsFor: 'accessing'!
nextIntFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high - low) + low) truncated!

nextFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high - low) + low)! !

!Color class methodsFor: 'instance creation'!
random
	"Answer a ColorValue with random RGB intensity (between 0 and 1)."

	| cv sr sg sb r g b rand |
	rand := Random new.
	^self random: rand!

random: rand
	"Answer a ColorValue with random RGB intensity (between 0 and 1)."

	^self red: rand next green: rand next blue: rand next! !

!SequenceableCollection methodsFor: 'accessing'!
atRandom
	"Answer a random element of the receiver."

	^self at: (1 + (Random new next * (self size)) truncated)!

atRandom: aRandom
	"Answer a random element of the receiver."

	^self at: (1 + (aRandom next * (self size)) truncated)! !
