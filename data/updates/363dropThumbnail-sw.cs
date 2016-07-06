'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 12:47:20 pm'!"Change Set:		dropThumbnail-swDate:			13 October 1998Author:			Scott WallaceWhen a MorphThumbnail is dropped on a pasteup that is not showing thumbnails, what gets dropped will be the original representee, if all is well"!!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'sw 10/8/1998 16:18'!morphToDropFrom: aMorph	| itsSelector aScriptor adjustment anEditor actualObject aUserScript ownersChoice aNail representee |	owner ifNotNil:		[(ownersChoice _ owner substituteForMorph: aMorph beingDroppedOn: self)			ifNotNil:	[^ ownersChoice]].	self alwaysShowThumbnail ifTrue:		[aNail _ aMorph representativeNoTallerThan: self maxHeightToAvoidThumbnailing thumbnailHeight: self heightForThumbnails.		aNail == aMorph ifFalse:			[aNail position: (self primaryHand position - ((self primaryHand targetOffset - self primaryHand formerPosition) * (aNail extent / aMorph extent)) rounded)].		^ aNail].	((aMorph isKindOf: MorphThumbnail) and: [(representee _ aMorph morphRepresented) owner == nil])		ifTrue:			[representee position: (self primaryHand position - ((self primaryHand targetOffset - self primaryHand formerPosition) * (representee extent / aMorph extent)) rounded).			^ representee].	self expandPhrasesToScripts ifFalse: [^ aMorph].	(aMorph hasProperty: #newAnonymousScript) ifTrue: [^ self emptyAnonymousScriptorFrom: aMorph].	(aMorph isKindOf: PhraseTileMorph) ifFalse: [^ aMorph].	(actualObject _ aMorph actualObject) ifNil: [^ aMorph].	actualObject assureUniClass.	aScriptor _ (itsSelector _ aMorph userScriptSelector) size > 0		ifTrue:			[actualObject isFlagshipForClass				ifFalse:					["We can set the status for our instantiation of this script, but cannot allow script editing"					anEditor _ actualObject scriptEvaluatorFor: itsSelector phrase: aMorph.					adjustment _ 50 @ 40.					anEditor]				ifTrue:					["old note: ambiguous case: if there's a script editor on the world, drop down a button, else drop down the script editor"					aUserScript _ actualObject class userScriptForPlayer: actualObject selector: itsSelector.					aUserScript isTextuallyCoded						ifTrue: [^ self scriptorForTextualScript: itsSelector ofPlayer: actualObject].					((anEditor _ actualObject scriptEditorFor: itsSelector) isInWorld and:							[anEditor owner == self])						ifFalse:							[adjustment _ 50 @ 30.							anEditor]						ifTrue:							[adjustment _ 60 @ 20.							actualObject anonymousScriptEditorFor: aMorph]]]		ifFalse:   "It's a system-defined selector; construct an anonymous scriptor around it"			[adjustment _ 60 @ 20.			actualObject anonymousScriptEditorFor: aMorph].	aScriptor position: (self primaryHand position - adjustment).	^ aScriptor! !