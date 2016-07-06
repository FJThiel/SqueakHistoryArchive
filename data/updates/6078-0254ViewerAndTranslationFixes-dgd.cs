'From Squeak3.8alpha of ''17 July 2004'' [latest update: #5976] on 16 August 2004 at 10:10:29 pm'!"Change Set:		ViewerAndTranslationFixes-dgdDate:			16 August 2004Author:			Diego Gomez DeckFix some errors related to the translation and viewer opening:- The category, in CategoryViewer, was selected but the actions didn't appears.  Fix in CategoryViewer>>initializeFor:categoryChoice:- The initial set of opened categories was broken, see StandardViewer>>likelyCategoryToShow."!!CategoryViewer methodsFor: 'initialization' stamp: 'dgd 8/16/2004 21:51'!initializeFor: aPlayer categoryChoice: aChoice	"Initialize the receiver to be associated with the player and category specified"	self listDirection: #topToBottom;		hResizing: #spaceFill;		vResizing: #spaceFill;		borderWidth: 1;		beSticky.	self color: Color green muchLighter muchLighter.	scriptedPlayer _ aPlayer.	self addHeaderMorph.	self chooseCategoryWhoseTranslatedWordingIs: aChoice! !!StandardViewer methodsFor: 'categories' stamp: 'dgd 8/16/2004 22:06'!likelyCategoryToShow
	"Choose a category to show based on what's already showing and on some predefined heuristics"

	| possible all aCat currVocab |
	all := (scriptedPlayer categoriesForViewer: self) asOrderedCollection.
	possible := all copy.
	currVocab := self currentVocabulary.
	self categoryMorphs do: 
			[:m | 
			aCat := currVocab categoryWhoseTranslatedWordingIs: m currentCategory.
			aCat ifNotNil: [possible remove: aCat wording ifAbsent: []]].
	(currVocab isKindOf: EToyVocabulary) 
		ifTrue: 
			["hateful!!"

			((possible includes: ScriptingSystem nameForInstanceVariablesCategory translated) 
				and: [scriptedPlayer hasUserDefinedSlots]) ifTrue: [^ ScriptingSystem nameForInstanceVariablesCategory].
			((possible includes: ScriptingSystem nameForScriptsCategory translated) and: [scriptedPlayer hasUserDefinedScripts]) 
				ifTrue: [^ ScriptingSystem nameForScriptsCategory]].
	{#basic translated} 
		do: [:preferred | (possible includes: preferred) ifTrue: [^preferred]].
	((scriptedPlayer isKindOf: Player) 
		and: [scriptedPlayer hasOnlySketchCostumes]) 
			ifTrue: [(possible includes: #tests translated) ifTrue: [^#tests translated]].
	{#'color & border' translated. #tests translated. #color translated. #flagging translated. #comparing translated.} 
		do: [:preferred | (possible includes: preferred) ifTrue: [^preferred]].
	^possible isEmpty ifFalse: [possible first] ifTrue: [all first]! !