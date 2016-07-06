'From Squeak2.9alpha of 12 June 2000 [latest update: #2665] on 21 September 2000 at 12:36:26 pm'!"Change Set:		Three QuickeesDate:			21 September 2000Author:			Dan IngallsSpeeds up findWindow.Speeds up allMorphsNotInPartsBinsDo.Introduces a way to reclaim a lot of space in images with many changeSets:	ChangeSet allInstancesDo: [:cs | cs zapHistory].I got 8MB back out of a 30MB image this way."!!ChangeSet methodsFor: 'initialize-release' stamp: 'di 9/21/2000 12:33'!zapHistory 	"Much stronger than trimHistory, but it should still leave the changeSet in good shape.	Must not be done on revertable changeSets		ChangeSet allInstancesDo: [:cs | cs zapHistory]."	revertable ifTrue: [^ self].  "No can do"	changeRecords do: [:chgRecord | chgRecord trimHistory]! !!ClassChangeRecord methodsFor: 'initialization' stamp: 'di 9/21/2000 12:34'!zapHistory	"Drop all recorded information not needed to simply keep track of what has been changed.	Saves a lot of space."	methodChanges do: [:r | r noteNewMethod: nil].  "Drop all refes to old methods"	thisOrganization _ nil.	priorOrganization _ nil.	thisComment _ nil.	priorComment _ nil.	thisMD _ nil.	priorMD _ nil.! !!Morph methodsFor: 'submorphs-accessing' stamp: 'di 9/21/2000 11:52'!allMorphsNotInPartsBinsDo: aBlock	"Evaluate the given block for all morphs in this composite morph (including the receiver) other than those that are, or reside in, a parts bin." 	self isPartsBin ifTrue: [^ self].	submorphs size > 0 ifTrue:		[submorphs do: [:m | m allMorphsNotInPartsBinsDo: aBlock]].	aBlock value: self.! !!HandMorph methodsFor: 'world menu commands' stamp: 'di 9/21/2000 12:20'!findWindow	"Present a menu of window titles, and activate the one that gets chosen.	Collapsed windows appear below line, expand if chosen."	| menu expanded collapsed nakedMorphs |	self flag: #bob.		"which world??"	menu _ MenuMorph new.	expanded _ SystemWindow windowsIn: self world satisfying: [:w | w isCollapsed not].	collapsed _ SystemWindow windowsIn: self world satisfying: [:w | w isCollapsed].	nakedMorphs _ self world submorphsSatisfying:		[:m | ((m isKindOf: SystemWindow) not and: [(m isKindOf: StickySketchMorph) not]) and:			[(m isKindOf: FlapTab) not]].	(expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty)) ifTrue: [^ self beep].	(expanded asSortedCollection: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) do:		[:w | menu add: w label target: w action: #activateAndForceLabelToShow.			w model canDiscardEdits ifFalse: [menu lastItem color: Color red]].	(expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty)) ifFalse: [menu addLine].	(collapsed asSortedCollection: [:w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label]) do: 		[:w | menu add: w label target: w action: #collapseOrExpand.		w model canDiscardEdits ifFalse: [menu lastItem color: Color red]].	nakedMorphs isEmpty ifFalse: [menu addLine].	(nakedMorphs asSortedCollection: [:w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name]) do:		[:w | menu add: w class name target: w action: #comeToFrontAndAddHalo].	menu addTitle: 'find window'.		menu popUpEvent: lastEvent! !