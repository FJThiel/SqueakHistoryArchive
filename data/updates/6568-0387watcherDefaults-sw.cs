'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #386] on 12 January 2005 at 12:35:56 pm'!"Change Set:		watcherDefaults-swDate:			12 January 2005Author:			Scott WallaceRestores former watcher feel:.* Wordings 'simple watcher' and 'detailed watcher' (as opposed to 'unlabeled watcher' and 'labeled watcher') restored.*  The dark blue color restored.*  The preference that governs whether the drop of a getter produces a watcher or not, dropProducesWatcher, is set to false by the postscript of the update.*  Bug fix (Mantis Squeakland bug 824):  a wacher can again be dropped in a 'test' area, whereupon it will expand into the appropriate relation as usual.*  Also fixes long-standing bug (dating back to at least 2001) that mishandled the coercion of typed getters for variables of any SymbolList type)*  Also fixes bug that arrived with update 0383smartTileMorph5-tak, in which the code for a Scriptor was not getting recompiled when a relational tile (such as = or <=) had its relation changed by direct manipulation, either by operating one of its carets or by choosing a new value from a pop-up obtained by clicking on the tile."!!BooleanScriptEditor methodsFor: 'dropping/grabbing' stamp: 'sw 1/12/2005 09:13'!wantsDroppedMorph: aMorph event: evt 	"Answer whether the receiver would be interested in accepting the morph"	(submorphs detect: [:m | m isAlignmentMorph] ifNone: [nil]) 		ifNotNil: [^false].	^(aMorph isKindOf: PhraseTileMorph orOf: WatcherWrapper) 		and: [(#(#Command #Unknown) includes: aMorph resultType capitalized) not]! !!DataType methodsFor: 'tiles' stamp: 'sw 1/12/2005 08:35'!addWatcherItemsToMenu: aMenu forGetter: aGetter	"Add watcher items to the menu if appropriate, provided the getter is not an odd-ball one for which a watcher makes no sense"	(Vocabulary gettersForbiddenFromWatchers includes: aGetter) ifFalse:		[aMenu add: 'simple watcher' translated selector: #tearOffUnlabeledWatcherFor: argument: aGetter.		aMenu add: 'detailed watcher' translated selector: #tearOffFancyWatcherFor: argument: aGetter.		aMenu addLine]! !!Player methodsFor: 'slots-user' stamp: 'sw 1/12/2005 08:57'!fancyWatcherFor: aGetter	"Anser a labeled readout for viewing a value textuallyi"	| aWatcher aColor aLine itsName aSelector aLabel |	aWatcher _ self unlabeledWatcherFor: aGetter.	aColor _ Color r: 0.387 g: 0.581 b: 1.0.	aLine _ WatcherWrapper newRow.	aLine player: self variableName: (aSelector _ Utilities inherentSelectorForGetter: aGetter).	itsName _ aWatcher externalName.	aWatcher setNameTo: 'readout'.	aLine addMorphFront: (self tileReferringToSelf				borderWidth: 0; layoutInset: 4@0;				typeColor: aColor; 				color: aColor; bePossessive).	aLabel _ StringMorph contents: aSelector, ' = ' font: ScriptingSystem fontForTiles.	aLabel setProperty: #watcherLabel toValue: true.	aLine addMorphBack: aLabel.	aLine addMorphBack: aWatcher.	aLine setNameTo: itsName.	^ aLine! !!SymbolListType methodsFor: 'tiles' stamp: 'sw 1/12/2005 10:13'!affordsCoercionToBoolean	"Answer true if a tile of this data type, when dropped into a pane that demands a boolean, could plausibly be expanded into a comparison (of the form  frog < toad   or frog = toad) to provide a boolean expression"	"Formerly this had been disabled (9/27/01) but from today's perspective I don't see any reason to disable it..."	^ true! !!TileMorph methodsFor: 'accessing' stamp: 'sw 1/12/2005 12:35'!value: anObject	"Set the receiver's 'value'.  For a literal tile, this is the literal itself; for operator tiles it is the operator.  Recompile any enclosing script."	type == #literal		ifTrue: [self literal: anObject]		ifFalse: [self setOperatorAndUseArrows: anObject asString].	self scriptEdited.	self layoutChanged! !!WatcherWrapper methodsFor: 'initialization' stamp: 'sw 1/12/2005 09:01'!player: aPlayer variableName: aVariableName	"Set up my initial state"	| aColor |	aColor _ Color r: 0.387 g: 0.581 b: 1.0.	player _ aPlayer.	variableName _ aVariableName.	self		listDirection: #leftToRight;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap; 		color: aColor;		layoutInset: -1;		borderWidth: 1;		borderColor: aColor darker;		listCentering: #center.	self reconstituteName! !!WatcherWrapper methodsFor: 'accessing' stamp: 'sw 1/12/2005 09:33'!associatedPlayer	"Answer the player with which I'm associated"	^ player! !!WatcherWrapper methodsFor: 'accessing' stamp: 'sw 1/12/2005 09:32'!tileRows	"Answer, for the benefit of d&d scripting, a structure appropriate for dropping nto a script"	^ Array with: (Array with: self getterTilesForDrop)! !"Postscript:"Preferences disable: #dropProducesWatcher.!