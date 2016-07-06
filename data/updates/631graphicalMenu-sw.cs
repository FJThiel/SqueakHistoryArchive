'From Squeak 2.3 of January 14, 1999 on 1 February 1999 at 3:27:02 pm'!"Change Set:		graphicalMenu-swDate:			1 February 1999Author:			Scott WallaceProvides a graphics chooser for Morphic.(Alan has pointed out that a large chooser in which you see dozens or even hundreds of possible choices all at once is usually preferable for graphics, and presumably such a facility will be available in due course.  Meanwhile...)This chooser shows one grapheme at a time; it has two sets of buttons for moving among choices: one kind races lickety-split through the choices; the other kind advances by one on each click.The new facility is immediately put to use in the following ways:(1)  Every SketchMorph and every ImageMorph can now 'change its graphic' to any other graphic known to the system.  Look in the red-dot menu for 'choose new graphic' -- it will bring up a graphicalMenu that you can use to select a new graphic.(2)  The 'view image imports' command (in the 'help' menu) now puts up a graphicalChooser, which permits you to see all the graphics in the 'imports' library, together with their names, and a menu that lets you rename any graphic or remove any one from the library.(3)  Similarly, the internal graphics library, accessible via 'ScriptingSystem inspectFormDictionary' (in the 'do' menu), is now viewable and changeable.(4)  Graphical tabs are supportable in the TabSorter (however, you'll need a forthcoming update I haven't yet published before you can get your hands on graphical tabs.)"!AlignmentMorph subclass: #GraphicalMenu	instanceVariableNames: 'target currentIndex formChoices formDisplayMorph coexistWithOriginal '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Menus'!GraphicalMenu subclass: #GraphicalDictionaryMenu	instanceVariableNames: 'baseDictionary entryNames '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Menus'!!Dictionary methodsFor: 'user interface' stamp: 'sw 12/24/1998 11:35'!inspectFormsWithLabel: aLabel	"Open a Form Dictionary inspector on the receiver, with the given label.  "	| viewClass aList aGraphicalMenu |	Smalltalk isMorphic ifTrue:		[aList _ self collect: [:f | f].		aList size == 0 ifTrue: [^ self inform: 'Empty!!'].		aGraphicalMenu _ GraphicalDictionaryMenu new initializeFor: nil fromDictionary: self.		^ World primaryHand attachMorph: (aGraphicalMenu wrappedInWindowWithTitle: aLabel)].	viewClass _ PluggableTextView.	Smalltalk at: #FormInspectView		ifPresent: [:formInspectView | viewClass _ formInspectView].	^ DictionaryInspector openOn: self withEvalPane: true		withLabel: aLabel		valueViewClass: viewClass! !!GraphicalDictionaryMenu class reorganize!('example' example)!!GraphicalDictionaryMenu class methodsFor: 'example' stamp: 'sw 12/24/1998 12:11'!example	"GraphicalDictionaryMenu example"	| aDict |	aDict _ Dictionary new.	#('ColorTilesOff' 'ColorTilesOn' 'Controls') do:		[:aString | aDict at: aString put: (ScriptingSystem formAtKey: aString)].	aDict inspectFormsWithLabel: 'Testing One Two Three'! !!Morph methodsFor: 'menus' stamp: 'sw 12/1/1998 20:57'!addAddHandMenuItemsForHalo: aMenu hand: aHandMorph	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."	| unlockables |	aMenu addLine.	aMenu add: 'copy to paste buffer' action: #copyToPasteBuffer.	self player ifNotNil:		[aMenu add: 'make another instance of me' action: #makeNewPlayerInstance].	aMenu addLine.	"aMenu add: 'open viewer' action: #openViewerForArgument.	(self isKindOf: MorphThumbnail) ifFalse:		[aMenu add: 'make alias' action: #makeAliasForArgument]."	aMenu add: 'change costume...' action: #chooseNewCostumeForArgument.	self colorSettable ifTrue:		[aMenu add: 'change color...' action: #changeColor].	aHandMorph potentialEmbeddingTargets size > 1 ifTrue:		[aMenu add: 'embed...' action: #placeArgumentIn].	self isLocked		ifFalse:			[aMenu add: 'lock' action: #lockMorph]		ifTrue:			[aMenu add: 'unlock' action: #unlockMorph].  "probably not possible -- wouldn't get halo"	unlockables _ self submorphs select:		[:m | m isLocked].	unlockables size == 1 ifTrue:		[aMenu add: 'unlock "', unlockables first externalName, '"' action: #unlockContents].	unlockables size > 1 ifTrue:		[aMenu add: 'unlock all contents' action: #unlockContents.		aMenu add: 'unlock...' action: #unlockOneSubpart].	"aMenu add: 'make mouse-sensitive' action: #makeMouseSensitive."	(owner == nil or: [self == owner submorphs last]) ifFalse:		[aMenu add: 'send to back' action: #goBehind]! !!Morph methodsFor: 'menus' stamp: 'sw 12/17/1998 12:09'!chooseNewGraphic	"Used by any morph that can be represented by a graphic"	self chooseNewGraphicCoexisting: false! !!Morph methodsFor: 'menus' stamp: 'sw 12/22/1998 18:21'!chooseNewGraphicCoexisting: aBoolean	"Used by any morph that can be represented by a graphic"	| reasonableForms replacee aGraphicalMenu myGraphic |	reasonableForms _ (SketchMorph allInstances collect: [:m | m form]) asOrderedCollection.	reasonableForms addAll: (Smalltalk imageImports collect: [:f | f]).	reasonableForms _ reasonableForms asSet asOrderedCollection.	(reasonableForms includes: (myGraphic _ self form))		ifTrue:			[reasonableForms remove: myGraphic].	reasonableForms addFirst: myGraphic.	aGraphicalMenu _ GraphicalMenu new initializeFor: self withForms: reasonableForms coexist: aBoolean.	aBoolean		ifFalse:			[replacee _ self topRendererOrSelf.			replacee owner replaceSubmorph: replacee by: aGraphicalMenu]		ifTrue:			[self primaryHand attachMorph: aGraphicalMenu]! !!Morph methodsFor: 'e-toy support' stamp: 'sw 12/24/1998 11:34'!wrappedInWindowWithTitle: aTitle	| aWindow |	aWindow _ (SystemWindow labelled: aTitle) model: MorphicModel new.	aWindow addMorph: self frame: (0@0 extent: 1@1).	aWindow extent: self extent.	^ aWindow! !!GraphicalMenu reorganize!('all' cancel downArrowHit initializeFor:withForms:coexist: okay upArrowHit updateThumbnail)!!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/4/1998 23:49'!cancel	coexistWithOriginal		ifTrue:			[self delete]		ifFalse:			[owner replaceSubmorph: self topRendererOrSelf by: target]! !!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/1/1998 17:56'!downArrowHit	currentIndex _ currentIndex - 1.	(currentIndex < 1) ifTrue:  [currentIndex _ formChoices size].	self updateThumbnail	! !!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/8/1998 09:29'!initializeFor: aTarget withForms: formList coexist: aBoolean	"World primaryHand attachMorph: (GraphicalMenu new initializeFor: Form allInstances)"	| buttons b anIndex buttonCage imageWrapper |	target _ aTarget.	coexistWithOriginal _ aBoolean.	color _ Color white.	borderColor _ Color blue darker.	borderWidth _ 1.	formChoices _ formList.	currentIndex _ 1.	self hResizing: #shrinkWrap; vResizing: #shrinkWrap.	b _ SimpleButtonMorph new target: self; borderColor: Color black.	buttons _ AlignmentMorph newRow.	buttons borderWidth: 0; inset: 0.	buttons hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	buttons centering: #topLeft.	buttonCage _ AlignmentMorph newColumn.	buttonCage hResizing: #shrinkWrap; vResizing: #spaceFill.	buttonCage addTransparentSpacerOfSize: (0 @ 10).	buttons addMorphBack: (b fullCopy label: 'Prev';	actionSelector: #downArrowHit; actWhen: #buttonDown).	buttons addTransparentSpacerOfSize: (5@0).	buttons addMorphBack: (b fullCopy label: 'Next';	actionSelector: #upArrowHit; actWhen: #buttonDown).	buttons addTransparentSpacerOfSize: (5@0).	buttons addUpDownArrowsFor: self.	buttons submorphs last color: Color white.	buttonCage addMorphBack: buttons.	buttonCage addTransparentSpacerOfSize: (0 @ 12).	buttons _ AlignmentMorph newRow.	buttons addMorphBack: (b fullCopy label: 'OK';	actionSelector: #okay).	buttons addTransparentSpacerOfSize: (5@0).	buttons addMorphBack: (b fullCopy label: 'Cancel';	actionSelector: #cancel).	buttonCage addMorphBack: buttons.	buttonCage addTransparentSpacerOfSize: (0 @ 10).	self addMorphFront: buttonCage.	imageWrapper _ Morph new color: Color transparent; extent: 102 @ 82.	imageWrapper addMorphBack: (formDisplayMorph _ ImageMorph new extent: 100 @ 100).	self addMorphBack: imageWrapper.	target ifNotNil: [(anIndex _ formList indexOf: target form ifAbsent: [nil]) ifNotNil:		[currentIndex _ anIndex]].	self updateThumbnail! !!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/10/1998 14:21'!okay	target ifNotNil: [target newForm: (formChoices at: currentIndex)].	coexistWithOriginal		ifTrue:			[self delete]		ifFalse:			[owner replaceSubmorph: self topRendererOrSelf by: target]! !!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/1/1998 17:54'!upArrowHit	currentIndex _ currentIndex + 1.	(currentIndex > formChoices size) ifTrue: [currentIndex _ 1].	self updateThumbnail	! !!GraphicalMenu methodsFor: 'all' stamp: 'sw 12/2/1998 22:05'!updateThumbnail	| f scaleY scaleX maxWidth stdHeight |	maxWidth _ 100.	stdHeight _ 80.	f _ formChoices at: currentIndex.	scaleY _ stdHeight / f height.  "keep height invariant"	scaleY _ scaleY min: 1.	scaleX _ ((f width * scaleY) <= maxWidth)		ifTrue:			[scaleY]		ifFalse:			[maxWidth / f width].	formDisplayMorph image: (f magnify: f boundingBox by: (scaleX @ scaleY) smoothing: 2).	formDisplayMorph layoutChanged! !!GraphicalDictionaryMenu commentStamp: '<historical>' prior: 0!A morph that allows you to view, rename, and remove elements from a dictionary whose keys are strings and whose values are forms.!!GraphicalDictionaryMenu reorganize!('all' baseDictionary: initializeFor:fromDictionary: nameOfGraphic removeEntry renameEntry renameGraphicTo: showMenu)!!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 11:59'!baseDictionary: aDictionary	baseDictionary _ aDictionary.	entryNames _ aDictionary keys asSortedArray.	formChoices _ entryNames collect: [:n | aDictionary at: n].	currentIndex _ 1! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/27/1998 23:06'!initializeFor: aTarget fromDictionary: aDictionary	|  b buttons buttonCage imageWrapper anIndex |	self baseDictionary: aDictionary.	target _ aTarget.	coexistWithOriginal _ true.	color _ Color white.	borderColor _ Color blue darker.	borderWidth _ 1.	self hResizing: #shrinkWrap; vResizing: #shrinkWrap.	b _ SimpleButtonMorph new target: self; borderColor: Color black.	buttons _ AlignmentMorph newRow.	buttons borderWidth: 0; inset: 0.	buttons hResizing: #shrinkWrap; vResizing: #shrinkWrap; extent: 5@5.	buttons centering: #topLeft.	buttonCage _ AlignmentMorph newColumn.	buttonCage hResizing: #shrinkWrap; vResizing: #spaceFill.	buttonCage addTransparentSpacerOfSize: (0 @ 10).	buttons addMorphBack: (b fullCopy label: 'Prev';	actionSelector: #downArrowHit; actWhen: #buttonDown).	buttons addTransparentSpacerOfSize: (5@0).	buttons addMorphBack: (b fullCopy label: 'Next';	actionSelector: #upArrowHit; actWhen: #buttonDown).	buttons addTransparentSpacerOfSize: (5@0).	buttons addUpDownArrowsFor: self.	buttonCage addMorphBack: buttons.	buttonCage addTransparentSpacerOfSize: (0 @ 12).	buttons _ AlignmentMorph newRow.	buttons addTransparentSpacerOfSize: (20@0).	buttons addMorphBack: (b fullCopy label: '<>'; actWhen: #buttonDown; actionSelector: #showMenu).	buttons addTransparentSpacerOfSize: (20@0).	buttonCage addMorphBack: buttons.	buttonCage addTransparentSpacerOfSize: (0 @ 10).	buttonCage addMorphBack: (UpdatingStringMorph new contents: ' '; target: self; putSelector: #renameGraphicTo:; getSelector: #nameOfGraphic; useStringFormat).	buttonCage addTransparentSpacerOfSize: (0 @ 10).	self addMorphFront: buttonCage.	imageWrapper _ Morph new color: Color transparent; extent: 102 @ 82.	imageWrapper addMorphBack: (formDisplayMorph _ ImageMorph new extent: 100 @ 100).	self addMorphBack: imageWrapper.	target ifNotNil: [(anIndex _ formChoices indexOf: target form ifAbsent: [nil]) ifNotNil:		[currentIndex _ anIndex]].	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 11:25'!nameOfGraphic	^ entryNames at: currentIndex! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 12:15'!removeEntry	baseDictionary removeKey: (entryNames at: currentIndex).	self baseDictionary: baseDictionary.	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 12:15'!renameEntry	| reply curr |	reply _ FillInTheBlankMorph  request: 'New key? '		initialAnswer: (curr _ entryNames at: currentIndex)		centerAt: self center.	(reply size = 0 or: [reply = curr]) ifTrue: [^ self beep].	(baseDictionary includesKey: reply) ifTrue:		[^ self inform: 'sorry that conflicts withthe name of anotherentry in this dictionary'].	baseDictionary at: reply put: (baseDictionary at: curr).	baseDictionary removeKey: curr.	self baseDictionary: baseDictionary.	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 12:24'!renameGraphicTo: newName	| curr |	curr _ entryNames at: currentIndex.	(newName size = 0 or: [newName = curr]) ifTrue: [^ self beep].	(baseDictionary includesKey: newName) ifTrue:		[^ self inform: 'sorry that conflicts withthe name of anotherentry in this dictionary'].	baseDictionary at: newName put: (baseDictionary at: curr).	baseDictionary removeKey: curr.	self baseDictionary: baseDictionary.	currentIndex _ entryNames indexOf: newName.	self updateThumbnail! !!GraphicalDictionaryMenu methodsFor: 'all' stamp: 'sw 12/24/1998 11:45'!showMenu	| aMenu sel |	aMenu _ MVCMenuMorph new.	aMenu add: 'remove' action:	#removeEntry.	aMenu add: 'rename' action: #renameEntry.	sel _ aMenu invokeAt: self primaryHand position in: self world.	sel ifNotNil: [self perform: sel]! !!ImageMorph methodsFor: 'menu commands' stamp: 'sw 12/17/1998 12:06'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'choose new graphic...' target: self action: #chooseNewGraphic.	aCustomMenu add: 'read from file' action: #readFromFile.	aCustomMenu add: 'grab from screen' action: #grabFromScreen.! !!ImageMorph methodsFor: 'other' stamp: 'sw 12/17/1998 12:11'!newForm: aForm	self image: aForm! !!SketchMorph methodsFor: 'menu' stamp: 'sw 12/22/1998 18:17'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'choose new graphic...' target: self action: #chooseNewGraphic.	self addPaintingItemsTo: aCustomMenu hand: aHandMorph! !!SketchMorph methodsFor: 'other' stamp: 'sw 12/1/1998 18:16'!newForm: aForm	self originalForm: aForm.	self layoutChanged! !!StandardScriptingSystem methodsFor: 'form dictionary' stamp: 'sw 12/7/1998 16:47'!standardForms	"ScriptingSystem standardForms"	^ FormDictionary collect: [:f | f]! !HandMorph removeSelector: #chooseNewFormForSketchMorph!"Postscript:"Morph organization removeEmptyCategories.TabbedPalette organization renameCategory: 'parts ^ controls tabs' toBe: 'parts & controls tabs'.ScriptingSystem saveForm: (Form extent: 24@29 depth: 16	fromArray: #( 0 0 0 0 1157645568 1157645568 1157645568 0 0 0 0 0 0 0 17664 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 0 0 0 0 1157645568 1157645568 1157649568 1619025024 1155548416 1157645568 0 0 0 0 0 17664 1157645568 1157645568 1484814368 2015393824 1684030720 1157645568 1157627904 0 0 0 0 17664 1157645568 1157645568 1551923232 2015393824 1749042432 1157645568 1157645568 0 0 0 0 1157645568 887563503 686637281 1287678016 2015391776 1354777856 1157645568 1157645568 0 0 0 0 1157640425 81724639 81730771 1157645568 1155548416 1157645568 1157645568 1157645568 0 0 0 17664 1157637327 81724639 81727705 1157645568 1157645568 1157645568 1157645568 1157645568 0 0 0 17664 1157643491 215680223 81734891 1157645568 1157645568 1157645568 1157645568 1157645568 0 0 0 1157645568 1157645568 1088500967 954549504 1157645568 1157645568 1157645568 1157645568 0 0 0 0 1157645568 1157642496 559948128 956318976 1157645568 1157645568 1157645568 1157645568 0 0 0 0 1157645568 956301792 31457760 96483584 1157645568 1157645568 1157645568 1157645568 0 0 0 0 1157645568 692060640 31457760 31469856 1157645568 1157645568 1157645568 1157645568 0 0 0 0 1157645568 1023414688 31457760 295714048 1157645568 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 1157645568 1157645568 824193312 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157627904 0 1157645568 1157645568 1228950944 1228948736 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 0 1157645568 1157649952 1939895200 1939886560 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157627904 1157645568 1157653184 1939895200 1939890880 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157627904 1157645568 1157652064 1939895200 1939889760 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1231050208 1231045888 1157645568 1157645568 1157645568 1157627904 1157645568 1157645568 1157645568 1157645568 17664 1157645568 1157645568 1157645568 1157645568 1157645568 0 0 0 17664 1157645568 1157645568 0 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 0 0 1157645568 1157645568 0 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 0 0 1157645568 1157645568 0 17664 1157645568 1157645568 1157645568 1157627904 0 0 0 0 1157645568 1157627904 0 0 1157645568 1157645568 1157645568 1157645568 0 0 0 17664 1157645568 1157627904 0 0 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 1157645568 1157645568 0 0 0 17664 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 0 0 0 0 17664 1157645568 1157645568 1157645568 1157645568 1157645568 1157645568 1157627904 0 0 0 0 0 0 1157645568 1157645568 1157645568 1157645568 1157627904 0 0)	offset: 0@0) atKey: 'PaintTab'.ScriptingSystem saveForm: ((Form extent: 14@15 depth: 16	fromArray: #( 1 0 0 0 0 0 0 65537 0 0 0 0 65537 0 65537 65537 65537 65537 65537 65537 65536 65537 1 65537 65537 65537 1 65536 65537 0 0 0 0 1 65536 65537 0 65537 65537 65537 65537 65536 65537 0 65537 65537 65537 1 65536 65537 0 0 0 0 0 65537 65537 0 65537 65537 65537 0 65537 65537 0 1 65537 65537 65536 65537 65537 0 0 0 0 0 65537 65537 0 1 65537 65537 65537 65537 65537 0 65537 65537 65537 65536 65537 65537 1 65537 65537 65537 65537 65537 1 1 65537 65537 65537 65537 65536)	offset: 0@0)) atKey: 'Menu'.ScriptingSystem initStandardScriptInfo.ScriptingSystem initializeHelpStrings.Preferences enable: #automaticViewerPlacement.Preferences deletePreference: #useNewViewers.ScriptingSystem resetStandardPartsBin.Preferences initializeHelpMessages.!