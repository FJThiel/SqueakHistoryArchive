'From Squeak3.1alpha of 4 February 2001 [latest update: #3741] on 27 February 2001 at 8:20:50 am'!"Change Set:		ScriptBugs2-tkDate:			26 February 2001Author:			Ted KaehlerMake the method header line of a scriptor disappear (universal tiles), since it is already in the top of the scriptor.When tiles for a user script like (Ellipse script1) are dropped into ANY pasteUpMorph, expand them into a scriptor.  Ignore the setting of #automaticPhraseExpansion in this case.  The tiles must have come from the Viewer."!!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'tk 2/26/2001 15:03'!morphToDropFrom: aMorph	"Given a morph being carried by the hand, which the hand is about to drop, answer the actual morph to be deposited.  Normally this would be just the morph itself, but several unusual cases arise, which this method is designed to service."	| aNail representee handy posBlock tempPos |	handy _ self primaryHand.	posBlock _ [:z | 			tempPos _ handy position - (handy targetOffset - aMorph formerPosition * (z extent / aMorph extent)) rounded.			self pointFromWorld: tempPos].	self alwaysShowThumbnail		ifTrue: [aNail _ aMorph						representativeNoTallerThan: self maxHeightToAvoidThumbnailing						norWiderThan: self maximumThumbnailWidth						thumbnailHeight: self heightForThumbnails.			aNail == aMorph				ifFalse: [aMorph formerPosition: aMorph position.						aNail position: (posBlock value: aNail)].			^ aNail].	((aMorph isKindOf: MorphThumbnail)			and: [(representee _ aMorph morphRepresented) owner == nil])		ifTrue: [representee				position: (posBlock value: representee).			^ representee].	self showingListView ifTrue:		[^ aMorph listViewLineForFieldList: (self valueOfProperty: #fieldListSelectors)].	(aMorph hasProperty: #newPermanentScript)		ifTrue: [^ aMorph asEmptyPermanentScriptor].	((aMorph isKindOf: PhraseTileMorph) or: [aMorph isKindOf: SyntaxMorph])		ifFalse: [^ aMorph].	aMorph userScriptSelector isEmptyOrNil ifTrue: ["non-user"		self automaticPhraseExpansion ifFalse: [^ aMorph]].	^ aMorph morphToDropInPasteUp: self! !!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 2/27/2001 08:15'!insertUniversalTilesForClass: aClass selector: aSelector	"Add a submorph which holds the universal-tiles script for the given class and selector"	|  source tree syn widget header |	source _ aClass sourceCodeAt: aSelector.    	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	aSelector numArgs = 0 ifTrue: [		"remove method header line"		(header _ syn findA: SelectorNode) ifNotNil: [header delete]].	widget _ syn inAScrollPane.	widget color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true;		setProperty: #maxAutoFitSize toValue: 300@200.	self addMorphBack: widget.	widget extent: (self width - 10 @ 150)."get the default size and then make resizable"	self fullBounds.	widget 		hResizing: #spaceFill;		vResizing: #spaceFill;		removeProperty: #maxAutoFitSize.	self 		hResizing: #rigid;		vResizing: #rigid.! !!Text methodsFor: 'attributes' stamp: 'tk 2/27/2001 08:20'!askIfAddStyle: priorMethod req: requestor	"Ask the user if we have a complex style (i.e. bold) for the first time"	| tell answ old |	(Preferences browseWithPrettyPrint and: [Preferences colorWhenPrettyPrinting])		ifTrue: [self couldDeriveFromPrettyPrinting ifTrue: [^ self asString]].	self runs coalesce.	self unembellished ifTrue: [^ self asString].	priorMethod ifNotNil: [old _ priorMethod getSourceFromFile].	(old == nil or: [old unembellished])		ifTrue:			[tell _ 'This method contains style for the first time (e.g. bold or colored text).Do you really want to save the style info?'.			answ _ (PopUpMenu labels: 'Save method with styleSave method simply')						startUpWithCaption: tell.			answ = 2 ifTrue: [^ self asString]]! !