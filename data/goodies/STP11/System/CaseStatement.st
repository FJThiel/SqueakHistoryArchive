"CaseStatement.st
Adds a neat shorthand for immediate dictionaries and a case statement; a useful example, 
but don't use it too often.
Example:
	(	(#a -> [Transcript cr; show: 'a'; cr]), 
		(#b -> [Transcript cr; show: 'b'; cr]), 
		(#c -> [Transcript cr; show: 'c'; cr])
	) case: #b

Note that there is a similar structure already in Squeak, but that it uses curly-braces as special characters, and can only have blocks as keys/values. I prefer this method.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    April 14, 1987"!
	
!Association methodsFor: 'copying'!
, anAssociation

 	"Answer a Dictionary with of the receiver and the argument as its elements."
	"(#a -> 1), (#b -> 2)"
	| dict |	dict := Dictionary new.
	dict add: self.
	dict add: anAssociation.	^dict! !

!Dictionary methodsFor: 'copying'!

, anAssociation 	"Add the argument to the receiver."
	"(#a -> 1), (#b -> 2), (#c -> 3)"

	self add: anAssociation.	^self! !

!Dictionary methodsFor: 'enumerating'!

case: aKey
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))	case: #b"

	^self case: aKey otherwise: []!

case: aKey otherwise: otherwiseBlock
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))
	case: #b otherwise: [Transcript cr; show: 'other'; cr]"

	^(self at: aKey ifAbsent: otherwiseBlock) value! !
