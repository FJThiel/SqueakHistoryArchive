'From Squeak2.9alpha of 17 July 2000 [latest update: #2542] on 30 August 2000 at 7:13:49 pm'!"Change Set:		pvmTweaksDate:			30 August 2000Author:			Bob Arning- a few appearance tweaks for Alan"!!ProjectViewMorph methodsFor: 'drawing' stamp: 'RAA 8/30/2000 19:10'!colorAroundName	^Color gray: 0.8! !!ProjectViewMorph methodsFor: 'drawing' stamp: 'RAA 8/30/2000 19:09'!drawOn: aCanvas	| projectName nameForm rectForName font |	self ensureImageReady.	super drawOn: aCanvas.	font _ self fontForName.	projectName _ self safeProjectName.	nameForm _ (StringMorph contents: projectName font: font) imageForm.	nameForm _ nameForm scaledToSize: (self extent - (4@2) min: nameForm extent).	rectForName _ self bottomLeft + 			(self width - nameForm width // 2 @ (nameForm height + 2) negated)				extent: nameForm extent.	rectForName topLeft eightNeighbors do: [ :pt |		aCanvas			stencil: nameForm 			at: pt			color: self colorAroundName.	].	aCanvas		stencil: nameForm 		at: rectForName topLeft 		color: Color black.	! !!ProjectViewMorph methodsFor: 'drawing' stamp: 'RAA 8/30/2000 19:11'!fontForName	| pickem |	pickem _ 3.	pickem = 1 ifTrue: [		^(((TextStyle named: #Helvetica) ifNil: [TextStyle default]) fontOfSize: 13) emphasized: 1.	].	pickem = 2 ifTrue: [		^(((TextStyle named: #Palatino) ifNil: [TextStyle default]) fontOfSize: 12) emphasized: 1.	].	^((TextStyle default) fontAt: 1) emphasized: 1! !