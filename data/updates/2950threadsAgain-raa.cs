'From Squeak2.9alpha of 17 July 2000 [latest update: #2997] on 11 November 2000 at 3:04:34 pm'!"Change Set:		threadsAgainDate:			11 November 2000Author:			Bob Arning<your descriptive text goes here>"!!InternalThreadNavigationMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 11/11/2000 15:01'!openThreadNamed: nameOfThread	| coll exists nav |	coll _ self knownThreads at: nameOfThread ifAbsent: [^self].	exists _ true.	nav _ World 		submorphThat: [ :each | (each isKindOf: self) and: [each threadName = nameOfThread]]		ifNone: [exists _ false. self new].	nav		listOfPages: coll;		threadName: nameOfThread;		removeAllMorphs;		addButtons;		openInWorld.	exists ifFalse: [		nav align: nav fullBounds bottomRight with: nav world bottomRight.	]! !