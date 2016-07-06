'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #308] on 4 September 2004 at 7:17:25 pm'!!Morph methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 9/4/2004 11:47'!understandsBorderVocabulary	"Replace the 'isKindOf: BorderedMorph' so that (for instance) Connectors can have their border vocabulary visible in viewers."	^false! !!BorderedMorph methodsFor: '*flexiblevocabularies-scripting' stamp: 'nk 9/4/2004 11:47'!understandsBorderVocabulary	"Replace the 'isKindOf: BorderedMorph' so that (for instance) Connectors can have their border vocabulary visible in viewers."	^true! !!Player methodsFor: '*flexibleVocabularies-costume' stamp: 'nk 9/4/2004 11:48'!hasAnyBorderedCostumes	"Answer true if any costumes of the receiver are BorderedMorph descendents"	self costumesDo:		[:cost | (cost understandsBorderVocabulary) ifTrue: [^ true]].	^ false! !