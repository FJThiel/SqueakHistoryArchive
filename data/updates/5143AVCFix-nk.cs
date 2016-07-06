'From Squeak3.2 of 11 July 2002 [latest update: #4956] on 19 August 2002 at 11:50:12 am'!"Change Set:		AVCFix-nkDate:			19 August 2002Author:			Ned KonzSome change sets that include definitionsof additionsToViewerCategories would not load correctly.The problem is that a spurious method UndefinedObject>>additionsToViewerCategories gets compiled sometimes, and then breaks later code.This CS is a quick patch for this problem."!!EToyVocabulary methodsFor: 'initialization' stamp: 'nk 8/19/2002 11:30'!morphClassesDeclaringViewerAdditions	"Answer a list of actual morph classes implementing #additionsToViewerCategories"	| survivors |	survivors _ OrderedCollection new.	(Smalltalk allImplementorsOf: #additionsToViewerCategories) do: [ :aMarker |		(aMarker actualClass isMeta and: [ (aMarker actualClass soleInstance isKindOf: Morph class)]) ifTrue: [			survivors add: aMarker actualClass soleInstance		]	].	^ survivors"EToyVocabulary basicNew morphClassesDeclaringViewerAdditions"! !