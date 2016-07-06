'From Squeak 2.5 of August 6, 1999 on 18 August 1999 at 9:52:48 am'!"Change Set:		delProjectChgSet-swDate:			18 August 1999Author:			Scott WallaceIf, upon deleting a project, it is found that the moribund project's change set is empty and is not the current change set of any other project, then zap the change set at the same time."!!ChangeSet methodsFor: 'testing' stamp: 'sw 8/17/1999 19:00'!projectsBelongedTo	"Answer a list of all the projects for which the receiver is the current change set"	^ Project allSubInstances select: [:proj |		proj projectChangeSet == self]! !!ChangeSorter class methodsFor: 'as yet unclassified' stamp: 'sw 8/18/1999 09:44'!removeChangeSet: aChangeSet	"Remove the given changeSet.  Caller must assure that it's cool to do this"	AllChangeSets remove: aChangeSet ifAbsent: [].	aChangeSet wither! !!Project methodsFor: 'release' stamp: 'sw 8/18/1999 09:49'!okToChange	| ok hasSubProjects itsName |	hasSubProjects _ world isMorph		ifTrue: [(world submorphs select:						[:m | (m isKindOf: SystemWindow)								and: [m model isKindOf: Project]]) size > 0]		ifFalse: [(world controllerWhoseModelSatisfies:						[:m | m isKindOf: Project]) notNil].	hasSubProjects ifTrue:		[PopUpMenu notify: 'The project ', self name printString, 'contains sub-projects.  You must remove theseexplicitly before removing their parent.'.		^ false].	ok _ (world isMorph not and: [world scheduledControllers size <= 1]) or:			[self confirm:'Really delete the project', self name printString, 'and all its windows?'].	ok ifFalse: [^ false].	"about to delete this project; clear previous links to it from other Projects:"	Project allProjects do: [:p | p deletingProject: self].	ProjectViewMorph allSubInstancesDo: [:p | p deletingProject: self].	world isMorph  "special release for wonderlands"		ifTrue: [world submorphs do:						[:m | (m isKindOf: WonderlandCameraMorph)								and: [m getWonderland release]]].	(changeSet isEmpty and: [(changeSet projectsBelongedTo copyWithout: self) isEmpty])		ifTrue:			[itsName _ changeSet name.			ChangeSorter removeChangeSet: changeSet.			Transcript cr; show: 'project change set ', itsName, ' deleted.'].	^ true! !