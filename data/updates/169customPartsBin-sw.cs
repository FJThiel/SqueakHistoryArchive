'From Squeak 2.1 of June 30, 1998 on 5 August 1998 at 6:24:16 pm'!"Change Set:		customPartsBin-swDate:			5 August 1998Author:			Scott WallaceLets the user designate any parts bin to be the standard custom one, which will be launched from the open menu on demand in a morphic world."!Object subclass: #StandardScriptingSystem	instanceVariableNames: ''	classVariableNames: 'FormDictionary HelpStrings PartsBin SystemSlotDictionary '	poolDictionaries: ''	category: 'Morphic-Scripting'!!BookMorph methodsFor: 'menu' stamp: 'sw 8/5/1998 00:23'!addBookMenuItemsTo: aMenu hand: aHandMorph	| controlsShowing subMenu |	subMenu _ MenuMorph new defaultTarget: self.	subMenu add: 'previous page' action: #previousPage.	subMenu add: 'next page' action: #nextPage.	subMenu add: 'insert a page' action: #insertPage.	subMenu add: 'delete this page' action: #deletePage.	controlsShowing _ self hasSubmorphWithProperty: #pageControl.	controlsShowing		ifTrue: [subMenu add: 'hide page controls' action: #hidePageControls]		ifFalse: [subMenu add: 'show page controls' action: #showPageControls].	subMenu add: 'sort pages' action: #sortPages:.	subMenu add: 'save as new-page prototype' action: #setNewPagePrototype.	newPagePrototype ifNotNil:		[subMenu add: 'clear new-page prototype' action: #clearNewPagePrototype].	subMenu add: 'make this be "My Parts Bin"' action: #saveAsMyPartsBin.	(aHandMorph classOfPasteBuffer isKindOf: PasteUpMorph class) ifTrue:		[subMenu add: 'paste book page'	action: #pasteBookPage].	aMenu add: 'Book...' subMenu: subMenu! !!BookMorph methodsFor: 'menu' stamp: 'sw 8/5/1998 00:36'!saveAsMyPartsBin	| aBin |	aBin _ self veryDeepCopy.	aBin pages do:		[:aPage | 			aPage isPartsBin: true.			aPage openToDragNDrop: false.			aPage submorphs do:				[:m | m setProperty: #partsDonor toValue: true.				m suspendEventHandler]].	ScriptingSystem setPartsBinFrom: aBin! !!Presenter methodsFor: 'palette & parts bin' stamp: 'sw 8/5/1998 00:16'!customPagesForPartsBin	| aPage |	^ #(BookMorph WebBookMorph) collect:		[:sym |			aPage _ self newPageForStandardPartsBin.			aPage addMorphBack: (Smalltalk at: sym) authoringPrototype.			aPage addMorphBack: Morph new previousPageButton markAsPartsDonor.			aPage addMorphBack: Morph new nextPageButton markAsPartsDonor.			aPage fixLayout.			aPage]! !!Presenter methodsFor: 'palette & parts bin' stamp: 'sw 8/5/1998 00:13'!launchMyPartsBin	associatedMorph primaryHand attachMorph: ScriptingSystem partsBin ! !!StandardScriptingSystem methodsFor: 'parts bin' stamp: 'sw 8/5/1998 00:10'!initializePartsBin	"ScriptingSystem initializePartsBin"	PartsBin _ Presenter new newStandardPartsBin! !!StandardScriptingSystem methodsFor: 'parts bin' stamp: 'sw 8/5/1998 00:09'!partsBin	PartsBin ifNil: [self initializePartsBin].	^ PartsBin veryDeepCopy! !!StandardScriptingSystem methodsFor: 'parts bin' stamp: 'sw 8/5/1998 00:36'!setPartsBinFrom: aMorph	PartsBin _ aMorph! !