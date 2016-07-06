'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 30 August 2003 at 3:36:36 pm'!"Change Set:		ProjectDefaultLanguage-dgdDate:			30 August 2003Author:			Diego Gomez Deck <DiegoGomezDeck@ConsultAr.com>Make the new created morphic project inheritance the naturalLanguage from the current project.The expression: 'self setNaturalLanguageTo: CurrentProject naturalLanguage.' was added in Project>>initMorphic"!!Project methodsFor: 'initialization' stamp: 'dgd 8/30/2003 15:33'!initMorphic	"Written so that Morphic can still be removed.  Note that #initialize is never actually called for a morphic project -- see the senders of this method."	Smalltalk verifyMorphicAvailability ifFalse: [^ nil].	changeSet := ChangeSet new.	transcript := TranscriptStream new.	displayDepth := Display depth.	parentProject := CurrentProject.	isolatedHead := false.	world := PasteUpMorph newWorldForProject: self.	self setNaturalLanguageTo: CurrentProject naturalLanguage.	self initializeProjectPreferences "Do this *after* a world is installed so that the project will be recognized as a morphic one."! !