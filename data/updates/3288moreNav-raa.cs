'From Squeak2.9alpha of 17 July 2000 [latest update: #3354] on 1 February 2001 at 3:34:32 pm'!"Change Set:		moreNavDate:			1 February 2001Author:			Bob ArningMore fixups for the InternalThreadNavigationMorph- do not create a pvm of new projects- act on mouseDown for grabbing from sorter"!!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/1/2001 15:18'!clickFromSorterEvent: evt morph: aMorph	(aMorph bounds containsPoint: evt cursorPoint) ifFalse: [^self].	evt isMouseUp ifFalse: [		evt shiftPressed ifFalse: [^evt hand grabMorph: aMorph].		^self	].	evt shiftPressed ifTrue: [		(Project named: (aMorph valueOfProperty: #nameOfThisProject)) enter.	].! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/1/2001 11:08'!insertNewProject	| newProj me |	[newProj _ Project newMorphicOn: nil.]		on: ProjectViewOpenNotification		do: [ :ex | ex resume: false].		me _ CurrentProjectRefactoring currentProjectName.	listOfPages withIndexDo: [ :each :index |		each first = me ifTrue: [			listOfPages add: {newProj name} afterIndex: index.			^self switchToThread: threadName		].	].	listOfPages add: {newProj name} afterIndex: listOfPages size.	^self switchToThread: threadName! !!InternalThreadNavigationMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/1/2001 15:16'!morphsForMyContentsSizedTo: sz	| allMorphicProjects morphsForPageSorter proj pvm |	allMorphicProjects _ Project allMorphicProjects.	'Assembling thumbnail images...'		displayProgressAt: self cursorPoint		from: 0 to: listOfPages size		during: [:bar |			morphsForPageSorter _ listOfPages withIndexCollect: [ :each :index | 				bar value: index.				(proj _ Project named: each first in: allMorphicProjects) ifNil: [					nil				] ifNotNil: [					pvm _ ProjectViewMorph on: proj.					pvm _ pvm imageForm scaledToSize: sz.					pvm _ pvm asMorph.					pvm setProperty: #nameOfThisProject toValue: each first.					pvm setBalloonText: each first.					pvm on: #mouseDown send: #clickFromSorterEvent:morph: to: self.					pvm on: #mouseUp send: #clickFromSorterEvent:morph: to: self.					pvm				]			].		].	^morphsForPageSorter! !InternalThreadNavigationMorph removeSelector: #bookOfAllProjects!InternalThreadNavigationMorph removeSelector: #editThisThreadOLD!