'From Squeak2.9alpha of 16 June 2000 [latest update: #2977] on 9 November 2000 at 5:09:19 pm'!"Change Set:		MthFinder2-tkDate:			9 November 2000Author:			Ted KaehlerFix up the Method Finder to properly insert the results when a ProjectNavigationMorph is present (in front of it).Make Command-G continue a complex search inside a BookMorph."!!MethodFinder class methodsFor: 'as yet unclassified' stamp: 'tk 11/9/2000 10:09'!methodFor: dataAndAnswers	"Return a Squeak expression that computes these answers.  (This method is called by the comment in the bottom pane of a MethodFinder.  Do not delete this method.)"	| result selFinder |	result _ (self new) load: dataAndAnswers; findMessage.	Smalltalk isMorphic ifTrue: [		selFinder _ (World submorphThat: [:mm | mm class == SystemWindow and: [				mm model isKindOf: SelectorBrowser]] ifNone: [^ result]) model.		selFinder searchResult: result].	^result! !!TextMorphEditor methodsFor: 'mvc compatibility' stamp: 'tk 11/9/2000 17:05'!againOrSame: bool	| bk keys |	(bk _ morph ownerThatIsA: BookMorph) ifNotNil: [		(keys _ bk valueOfProperty: #searchKey ifAbsent: [nil]) ifNotNil: [			keys size > 0 ifTrue: [bk findText: keys]]].	super againOrSame: bool.	(morph respondsTo: #editView) ifTrue: [		morph editView selectionInterval: self selectionInterval].! !