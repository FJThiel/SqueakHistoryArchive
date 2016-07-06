'From Squeak3.3alpha of 11 January 2002 [latest update: #4664] on 11 March 2002 at 4:11:47 pm'!!BookMorph methodsFor: 'other' stamp: 'tk 3/11/2002 12:05'!releaseCachedState	"Release the cached state of all my pages."	super releaseCachedState.	self removeProperty: #allText.	"the cache for text search"	pages do: [:page | 		page == currentPage ifFalse: [page fullReleaseCachedState]].! !!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 3/11/2002 12:12'!hibernate	"Possibly delete the tiles, but only if using universal tiles."	| tw |	Preferences universalTiles ifFalse: [^ self].	(tw _ self findA: TwoWayScrollPane) == nil		ifFalse: [self setProperty: #sizeAtHibernate toValue: self extent "+ tw xScrollerHeight".				submorphs size > 1 ifTrue: [tw delete]].! !!ScriptEditorMorph methodsFor: 'other' stamp: 'tk 3/11/2002 16:11'!insertUniversalTilesForClass: aClass selector: aSelector	"Add a submorph which holds the universal-tiles script for the given class and selector"	| source tree syn widget header |	source _ aClass sourceCodeAt: aSelector ifAbsent: [		Transcript cr; show: aClass name, 'could not find selector ', aSelector.		^ self delete].    	tree _ Compiler new 		parse: source 		in: aClass 		notifying: nil.	(syn _ tree asMorphicSyntaxUsing: SyntaxMorph)		parsedInClass: aClass.	aSelector numArgs = 0 ifTrue: [		"remove method header line"		(header _ syn findA: SelectorNode) ifNotNil: [header delete]].	syn removeReturnNode.		"if ^ self at end, remove it"	widget _ syn inAScrollPane.	widget hResizing: #spaceFill;		vResizing: #spaceFill;		color: Color transparent;		setProperty: #hideUnneededScrollbars toValue: true.	self addMorphBack: widget.	(self hasProperty: #autoFitContents) ifFalse:		[self valueOfProperty: #sizeAtHibernate ifPresentDo:			[:oldExtent | self extent: oldExtent]].	syn finalAppearanceTweaks.! !!String methodsFor: 'comparing' stamp: 'tk 3/8/2001 16:49'!howManyMatch: string 	"Count the number of characters that match up in self and aString."	| count shorterLength |		count  _  0 .	shorterLength  _  ((self size ) min: (string size ) ) .	(1 to: shorterLength  do: [:index |		 (((self at: index ) = (string at: index )  ) ifTrue: [count  _  (count + 1 ) .			]   ).		]   ).	^  count 		! !