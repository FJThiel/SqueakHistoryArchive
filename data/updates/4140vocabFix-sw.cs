'From Squeak3.1alpha of 28 February 2001 [latest update: #4139] on 5 June 2001 at 3:15:08 pm'!"Change Set:		vocabFix-swDate:			5 June 2001Author:			Scott WallaceFixes an error and an oversight in update 4133."!!TileMorph methodsFor: 'initialization' stamp: 'sw 6/5/2001 15:04'!rawVocabulary: aVocabulary	"Set the receiver's vocabulary, without side effects."	vocabularySymbol _ (aVocabulary isKindOf: Symbol)		ifTrue:			[aVocabulary]		ifFalse:			[aVocabulary vocabularyName]! !!Vocabulary class methodsFor: 'class initialization' stamp: 'sw 6/5/2001 15:08'!initializeStandardVocabularies	"Initialize a few standard vocabularies and place them in the AllVocabularies list."	AllVocabularies _ OrderedCollection new.	AllMethodInterfaces _ IdentityDictionary new.	self addVocabulary: EToyVocabulary new.	self addVocabulary: self newPublicVocabulary.	self addVocabulary: FullVocabulary new.	self addVocabulary: self newQuadVocabulary.	self addKiswahiliVocabulary.	self addGermanVocabulary.	self wonderlandVocabulary.  "creates it and adds it"	"self addVocabulary: self newNumberVocabulary."	"self addVocabulary: self newTestVocabulary."	"Vocabulary initialize"! !"Postscript:"Vocabulary initialize.Vocabulary cleanseVocabularyStructures.!