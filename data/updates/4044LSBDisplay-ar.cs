'From Squeak3.1alpha of 28 February 2001 [latest update: #4043] on 17 May 2001 at 9:04:41 pm'!"Change Set:		LSBDisplay-arDate:			17 May 2001Author:			Andreas RaabMake little endian formats available when choosing a new display depth. Also makes changing the display depth a little nicer by providing checkmarks."!!DisplayScreen methodsFor: 'other' stamp: 'ar 5/17/2001 21:02'!supportedDisplayDepths	"Return all pixel depths supported on the current host platform."	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]! !!DisplayScreen methodsFor: 'private' stamp: 'ar 5/17/2001 21:03'!findAnyDisplayDepthIfNone: aBlock	"Return any display depth that is supported on this system.	If there is none, evaluate aBlock."	#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) do:[:bpp|		(self supportsDisplayDepth: bpp) ifTrue:[^bpp].	].	^aBlock value! !!TheWorldMenu methodsFor: 'as yet unclassified' stamp: 'ar 5/17/2001 21:01'!setDisplayDepth	"Let the user choose a new depth for the display. "	| result oldDepth allDepths allLabels menu hasBoth |	oldDepth _ Display nativeDepth.	allDepths _ #(1 -1 2 -2 4 -4 8 -8 16 -16 32 -32) select: [:d | Display supportsDisplayDepth: d].	hasBoth _ (allDepths anySatisfy:[:d| d > 0]) and:[allDepths anySatisfy:[:d| d < 0]].	allLabels _ allDepths collect:[:d|		String streamContents:[:s|			s nextPutAll: (d = oldDepth ifTrue:['<on>'] ifFalse:['<off>']).			s print: d abs.			hasBoth ifTrue:[s nextPutAll: (d > 0 ifTrue:['  (big endian)'] ifFalse:['  (little endian)'])].		]].	menu _ SelectionMenu labels: allLabels selections: allDepths.	result _ menu startUpWithCaption: 'Choose a display depth'.	result ifNotNil: [Display newDepth: result].	oldDepth _ oldDepth abs.	(Smalltalk isMorphic and: [(Display depth < 4) ~= (oldDepth < 4)])		ifTrue:			["Repaint windows since they look better all white in depth < 4"			(SystemWindow windowsIn: myWorld satisfying: [:w | true]) do:				[:w |				oldDepth < 4					ifTrue: [w restoreDefaultPaneColor]					ifFalse: [w updatePaneColors]]]! !