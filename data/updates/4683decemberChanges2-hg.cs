'From Squeak3.2alpha of 1 November 2001 [latest update: #4599] on 2 January 2002 at 5:23:16 pm'!!Module methodsFor: 'name lookup schemes' stamp: 'hg 12/16/2001 18:40'!definesName: varName ifTrue: assocBlock	^ self definesName: varName usingScheme: self defaultBindingScheme ifTrue: assocBlock! !