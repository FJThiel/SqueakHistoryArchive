'From Squeak2.9alpha of 17 July 2000 [latest update: #3019] on 16 November 2000 at 1:17:49 pm'!"Change Set:		allProjectsDate:			16 November 2000Author:			Bob Arning- modify Project to keep a list of all instances so that enumeration via #allInstances or #allSubInstances (and the #garbageCollect required to make these accurate) will not be necessary. This will also enable us to implement other models of deciding whether to keep a Project in the image rather than depending on there being a ProjectViewMorph to serve as the anchor reference.If you find any anomalies in the enumeration of projects, please let me know."!Model subclass: #Project	instanceVariableNames: 'world changeSet transcript parentProject previousProject displayDepth viewSize thumbnail nextProject guards projectParameters isolatedHead inForce version urlList environment lastDirectory lastSavedAtSeconds '	classVariableNames: 'AllProjects CurrentProject GoalFreePercent GoalNotMoreThan UIProcess '	poolDictionaries: ''	category: 'System-Support'!!ChangeSet methodsFor: 'testing' stamp: 'RAA 11/13/2000 17:15'!correspondingProject	"If the receiver is the current change set for any project, answer it, else answer nil"	^Project allProjects 		detect: [ :proj |			proj projectChangeSet == self		]		ifNone: [nil]! !!ImageSegment methodsFor: 'fileIn/Out' stamp: 'RAA 11/16/2000 12:09'!comeFullyUpOnReload: smartRefStream	"fix up the objects in the segment that changed size.  An object in the segment is the wrong size for the modern version of the class.  Construct a fake class that is the old size.  Replace the modern class with the old one in outPointers.  Load the segment.  Traverse the instances, making new instances by copying fields, and running conversion messages.  Keep the new instances.  Bulk forward become the old to the new.  Let go of the fake objects and classes.	After the install (below), arrayOfRoots is filled in.  Globalize new classes.  Caller may want to do some special install on certain objects in arrayOfRoots. 	May want to write the segment out to disk in its new form."	| mapFakeClassesToReal fakes goods bads perfect ccFixups real insts receiverClasses |	mapFakeClassesToReal _ smartRefStream reshapedClassesIn: outPointers.		"Dictionary of just the ones that change shape.  Substitute them in outPointers."	ccFixups _ self remapCompactClasses: mapFakeClassesToReal 				refStrm: smartRefStream.	ccFixups ifFalse: [^ self error: 'A class in the file is not compatible'].	endMarker _ segment nextObject. 	"for enumeration of objects"	endMarker == 0 ifTrue: [endMarker _ 'End' clone].	arrayOfRoots _ self loadSegmentFrom: segment outPointers: outPointers.		"Can't use install.  Not ready for rehashSets"	mapFakeClassesToReal isEmpty ifFalse: [		fakes _ mapFakeClassesToReal keys.		goods _ OrderedCollection new.		bads _ OrderedCollection new.		fakes do: [:aFakeClass | 			real _ mapFakeClassesToReal at: aFakeClass.			(real indexIfCompact > 0) "and there is a fake class"				ifFalse: ["normal case"					aFakeClass allInstancesDo: [:misShapen | 						perfect _ smartRefStream convert: misShapen to: real.						(bads includes: misShapen) ifFalse: [							bads add: misShapen.							goods add: perfect]]]				ifTrue: ["instances have the wrong class.  Fix them before anyone notices."					insts _ OrderedCollection new.					self allObjectsDo: [:obj | obj class == real ifTrue: [insts add: obj]].					insts do: [:misShapen | 						perfect _ smartRefStream convert: misShapen to: real.						(bads includes: misShapen) ifFalse: [							bads add: misShapen.							goods add: perfect]]]].		bads size > 0 ifTrue: [			bads asArray elementsForwardIdentityTo: goods asArray]].	receiverClasses _ self restoreEndianness.		"rehash sets"	smartRefStream checkFatalReshape: receiverClasses.	"Classes in this segment."	arrayOfRoots do: [:aRoot | 		(aRoot isKindOf: Project) ifTrue: [			aRoot ensureChangeSetNameUnique.			Project addingProject: aRoot.		].		aRoot class class == Metaclass ifTrue: [ self declare: aRoot]	].	mapFakeClassesToReal isEmpty ifFalse: [		fakes do: [:aFake | 			aFake indexIfCompact > 0 ifTrue: [aFake becomeUncompact].			aFake removeFromSystemUnlogged].		SystemOrganization removeEmptyCategories].	"^ self"! !!Project methodsFor: 'accessing' stamp: 'RAA 11/16/2000 12:19'!findProjectView: projectDescription	| pName dpName proj |	"In this world, find the morph that holds onto the project described by projectDescription.  projectDescription can be a project, or the name of a project.  The project may be represented by a DiskProxy.  The holder morph may be at any depth in the world.	Need to fix this if Projects have subclasses, or if a class other than ProjectViewMorph can officially hold onto a project.  (Buttons, links, etc)	If parent is an MVC world, return the ProjectController."	self flag: #bob.		"read the comment"	pName _ (projectDescription isKindOf: String) 		ifTrue: [projectDescription]		ifFalse: [projectDescription name].	self isMorphic 		ifTrue: [world allMorphsDo: [:pvm |				pvm class == ProjectViewMorph ifTrue: [					(pvm project class == Project and: 						[pvm project name = pName]) ifTrue: [^ pvm].					pvm project class == DiskProxy ifTrue: [ 						dpName _ pvm project constructorArgs first.						dpName _ (dpName findTokens: '/') last.						dpName _ (Project parseProjectFileName: dpName) first.						dpName = pName ifTrue: [^ pvm]]]]]		ifFalse: [world scheduledControllers do: [:cont |				(cont isKindOf: ProjectController) ifTrue: [					((proj _ cont model) class == Project and: 						[proj name = pName]) ifTrue: [^ cont view].					proj class == DiskProxy ifTrue: [ 						dpName _ proj constructorArgs first.						dpName _ (dpName findTokens: '/') last.						dpName _ (Project parseProjectFileName: dpName) first.						dpName = pName ifTrue: [^ cont view]]]]			].	^ nil! !!Project methodsFor: 'release' stamp: 'RAA 11/13/2000 17:06'!deletingProject: aProject	"Clear my previousProject link if it points at the given Project, which is being deleted."	self flag: #bob.		"zapping projects"	previousProject == aProject		ifTrue: [previousProject _ nil].	nextProject == aProject		ifTrue:	[nextProject _ nil]! !!Project methodsFor: 'release' stamp: 'RAA 11/16/2000 13:02'!okToChange	"Answer whether the window in which the project is housed can be dismissed -- which is destructive. We never clobber a project without confirmation"	| ok is list |	self subProjects size  >0 ifTrue:		[PopUpMenu notify: 'The project ', self name printString, 'contains sub-projects.  You must remove theseexplicitly before removing their parent.'.		^ false].	ok _ world isMorph not and: [world scheduledControllers size <= 1].	ok ifFalse: [self isMorphic ifTrue: [		self parent == CurrentProject 			ifFalse: [^true]]].  "view from elsewhere.  just delete it."	ok _ (self confirm:'Really delete the project', self name printString, 'and all its windows?').			ok ifFalse: [^ false].	world isMorph 		ifTrue: [world submorphs do:   "special release for wonderlands"						[:m | (m isKindOf: WonderlandCameraMorph)								and: [m getWonderland release]].			"Remove Player classes and metaclasses owned by project"			is _ ImageSegment new arrayOfRoots: (Array with: self).			(list _ is rootsIncludingPlayers) ifNotNil: [				list do: [:playerCls | 					(playerCls respondsTo: #isMeta) ifTrue: [						playerCls isMeta ifFalse: [							playerCls removeFromSystemUnlogged]]]]].	self removeChangeSetIfPossible.	"do this last since it will render project inaccessible to #allProjects and their ilk"	Project deletingProject: self.	^ true! !!Project class methodsFor: 'instance creation' stamp: 'RAA 11/16/2000 12:07'!new	| new |	new _ super new.	new setProjectHolder: CurrentProject.	self addingProject: new.	^new! !!Project class methodsFor: 'instance creation' stamp: 'RAA 11/16/2000 12:07'!newMorphic	| new |	"ProjectView open: Project newMorphic"	new _ self basicNew.	self addingProject: new.	new initMorphic.	^new! !!Project class methodsFor: 'instance creation' stamp: 'RAA 11/16/2000 12:08'!newMorphicOn: aPasteUpOrNil	| newProject |	newProject _ self basicNew initMorphic.	self addingProject: newProject.	aPasteUpOrNil ifNotNil: [newProject installPasteUpAsWorld: aPasteUpOrNil].	newProject createViewIfAppropriate.	^newProject! !!Project class methodsFor: 'utilities' stamp: 'RAA 11/16/2000 12:04'!addingProject: newProject	(self allProjects includes: newProject) ifTrue: [^self].	AllProjects _ self allProjects copyWith: newProject.! !!Project class methodsFor: 'utilities' stamp: 'RAA 11/13/2000 17:14'!allProjects	^AllProjects ifNil: [		Smalltalk garbageCollect.		AllProjects _ self allSubInstances select: [:p | p name notNil].	].! !!Project class methodsFor: 'utilities' stamp: 'RAA 11/13/2000 17:26'!deletingProject: outgoingProject	ImageSegment allSubInstancesDo: [:seg |		seg ifOutPointer: outgoingProject thenAllObjectsDo: [:obj |			(obj isKindOf: ProjectViewMorph) ifTrue: [				obj deletingProject: outgoingProject.  obj abandon].			obj class == Project ifTrue: [obj deletingProject: outgoingProject]]].	Project allProjects do: [:p | p deletingProject: outgoingProject].	"ones that are in"	ProjectViewMorph allSubInstancesDo: [:p | 		p deletingProject: outgoingProject.		p project == outgoingProject ifTrue: [p abandon]].	AllProjects _ self allProjects copyWithout: outgoingProject.! !!ProjectViewMorph methodsFor: 'events' stamp: 'RAA 11/13/2000 17:06'!deletingProject: aProject	"My project is being deleted.  Delete me as well."	self flag: #bob.		"zapping projects"	project == aProject ifTrue: [self delete].! !!SystemDictionary methodsFor: 'shrinking' stamp: 'RAA 11/13/2000 17:17'!discardMVC   "Smalltalk discardMVC"	| keepers |	self flag: #bob.		"zapping projects"	Smalltalk isMorphic ifFalse:		[PopUpMenu notify: 'You must be in a Morphic project to discard MVC.'.		^ self].	"Check that there are no MVC Projects"	(Project allProjects allSatisfy: [ :proj | proj isMorphic]) ifFalse:		[(self confirm: 'Would you like a chance to remove yourMVC projects in an orderly manner?')					ifTrue: [^ self].		(self confirm: 'If you wish, I can remove all MVC projects,make this project be the top project, and placeall orphaned sub-projects of MVC parents here.Would you like be to do thisand proceed to discard all MVC classes?')					ifTrue: [self zapMVCprojects]					ifFalse: [^ self]].	Smalltalk reclaimDependents.	"Remove old Paragraph classes and View classes."	(ChangeSet superclassOrder: Paragraph withAllSubclasses asArray) reverseDo: 		[:c | c removeFromSystem].	(ChangeSet superclassOrder: View withAllSubclasses asArray) reverseDo: 		[:c | c removeFromSystem].	"Get rid of ParagraphEditor's ScrollController dependence"	#(markerDelta viewDelta scrollAmount scrollBar computeMarkerRegion) do:			[:sel | ParagraphEditor removeSelector: sel].	ParagraphEditor compile: 'updateMarker'.	ParagraphEditor superclass: MouseMenuController .	"Get rid of all Controller classes not needed by ParagraphEditor and ScreenController"	keepers _ TextMorphEditor withAllSuperclasses copyWith: ScreenController.	(ChangeSet superclassOrder: Controller withAllSubclasses asArray) reverseDo: 		[:c | (keepers includes: c) ifFalse: [c removeFromSystem]].	SystemOrganization removeCategoriesMatching: 'ST80-Paths'.	SystemOrganization removeCategoriesMatching: 'ST80-Pluggable Views'.	Smalltalk removeClassNamed: 'FormButtonCache'.	Smalltalk removeClassNamed: 'WindowingTransformation'.	Smalltalk removeClassNamed: 'ControlManager'.	Smalltalk removeClassNamed: 'DisplayTextView'.	ScheduledControllers _ nil.	Undeclared removeUnreferencedKeys.	SystemOrganization removeEmptyCategories.	Symbol rehash.! !!SystemDictionary methodsFor: 'shrinking' stamp: 'RAA 11/13/2000 17:27'!zapMVCprojects	"Smalltalk zapMVCprojects"	| window |	self flag: #bob. "zapping projects"	Smalltalk garbageCollect.	"So allInstances is precise"	Project		allSubInstancesDo: [:proj | proj isTopProject				ifTrue: [proj isMorphic						ifFalse: ["Root project is MVC -- we must become the root"							CurrentProjectRefactoring currentBeParentToCurrent]]				ifFalse: [proj parent isMorphic						ifFalse: [proj isMorphic								ifTrue: ["Remove Morphic projects from MVC 									views "									"... and add them back here."									window _ (SystemWindow labelled: proj name)												model: proj.									window										addMorph: (ProjectViewMorph on: proj)										frame: (0 @ 0 corner: 1.0 @ 1.0).									window openInWorld.									CurrentProjectRefactoring currentBeParentTo: proj]].					proj isMorphic						ifFalse: ["Remove MVC projects from Morphic views"							Project deletingProject: proj]]]! !