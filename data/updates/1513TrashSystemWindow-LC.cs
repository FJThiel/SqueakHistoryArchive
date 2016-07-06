'From Squeak 2.5 of August 6, 1999 [latest update: #1499] on 1 October 1999 at 4:04:50 pm'!"Change Set:		TrashSystemWindowDate:			28 September 1999Author:			Leandro CanigliaFix: Since ObjectExplorer doesn't handle keystroke events, #exploreFor: cannot use the default keystroke selector. The error becomes apparent when MorphicWrappers are installed.Fix: When allowSysWindowEmbedding is set to true and a System Window goes to the trash, an error occurs. The error becomes apparent when MorphicWrappers are installed.Change: Sometimes Morph|allMorphs is used instead of Morph|allMorphsDo:. The difference is that while allMorphs creates a new collection, allMorphsDo: enumerates each submorph without creating spurious collections."!!Morph methodsFor: 'submorphs-accessing' stamp: 'LC 9/28/1999 19:12'!findDeepSubmorphThat: block1 ifAbsent: block2 	self		allMorphsDo: [:m | (block1 value: m)				== true ifTrue: [^ m]].	^ block2 value! !!Morph methodsFor: 'scripting' stamp: 'LC 9/28/1999 21:57'!makeAllTilesColored	self allMorphsDo: 		[:m | m restoreTypeColor]! !!Morph methodsFor: 'scripting' stamp: 'LC 9/28/1999 21:57'!makeAllTilesGreen	self allMorphsDo: 		[:m | m useUniformTileColor]! !!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'di 10/1/1999 16:01'!explorerFor: anObject	| window listMorph |	rootObject _ anObject.	(window _ ObjectExplorerWindow labelled: 'Explorer')		model: self;		color: Color red.	window addMorph: (listMorph _ SimpleHierarchicalListMorph 			on: self			list: #getList			selected: #getCurrentSelection			changeSelected: #noteNewSelection:			menu: #genericMenu:			keystroke: nil)		frame: (0@0 corner: 1@0.8).	window addMorph: ((PluggableTextMorph on: self text: #trash accept: #trash:				readSelection: #contentsSelection menu: #codePaneMenu:shifted:)					askBeforeDiscardingEdits: false)		frame: (0@0.8 corner: 1@1).	listMorph autoDeselect: false.     ^ window! !!SystemWindow methodsFor: 'top window' stamp: 'LC 9/28/1999 19:04'!extantSketchEditor	"If my world has an extant SketchEditorMorph associated with anything  	in this window, return that SketchEditor, else return nil"	| w sketchEditor pasteUp |	(w _ self world) isNil ifTrue: [^ nil].	(sketchEditor _ w sketchEditorOrNil) isNil ifTrue: [^ nil].	(pasteUp _ sketchEditor enclosingPasteUpMorph) isNil ifTrue: [^ nil].	self findDeepSubmorphThat: [:m | m = pasteUp]		ifAbsent: [^ nil].	^ sketchEditor! !