'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6550] on 22 February 2005 at 3:32:43 pm'!"Change Set:		EvenMoreDepFixes-nkDate:			22 February 2005Author:			Ned KonzFixes for several sends of known deprecated methods to known globals."!!Debugger methodsFor: 'context stack menu' stamp: 'nk 2/22/2005 15:29'!returnValue	"Force a return of a given value to the previous context!!"	| previous selectedContext expression value |	contextStackIndex = 0 ifTrue: [^Beeper beep].	selectedContext := self selectedContext.	expression := FillInTheBlank request: 'Enter expression for return value:'.	value := Compiler new 				evaluate: expression				in: selectedContext				to: selectedContext receiver.	previous := selectedContext sender.	self resetContext: previous.	interruptedProcess popTo: previous value: value! !!ImageSegment methodsFor: 'testing' stamp: 'nk 2/22/2005 15:26'!findRogueRootsAllMorphs: rootArray 	"This is a tool to track down unwanted pointers into the segment.  If we don't deal with these pointers, the segment turns out much smaller than it should.  These pointers keep a subtree of objects out of the segment.1) assemble all objects should be in seg:  morph tree, presenter, scripts, player classes, metaclasses.  Put in a Set.2) Remove the roots from this list.  Ask for senders of each.  Of the senders, forget the ones that are in the segment already.  Keep others.  The list is now all the 'incorrect' pointers into the segment."	| inSeg testRoots scriptEditors pointIn wld xRoots |	Smalltalk garbageCollect.	inSeg := IdentitySet new: 200.	arrayOfRoots := rootArray.	(testRoots := self rootsIncludingPlayers) ifNil: [testRoots := rootArray].	testRoots do: 			[:obj | 			(obj isKindOf: Project) 				ifTrue: 					[inSeg add: obj.					wld := obj world.					inSeg add: wld presenter].			(obj isKindOf: Presenter) ifTrue: [inSeg add: obj]].	xRoots := wld ifNil: [testRoots] ifNotNil: [testRoots , (Array with: wld)].	xRoots do: 			[:obj | 			"root is a project"			obj isMorph 				ifTrue: 					[obj allMorphs do: 							[:mm | 							inSeg add: mm.							mm player ifNotNil: [inSeg add: mm player]].					obj isWorldMorph ifTrue: [inSeg add: obj presenter]]].	inSeg do: 			[:obj | 			(obj isKindOf: Player) 				ifTrue: 					[scriptEditors := obj class tileScriptNames 								collect: [:nn | obj scriptEditorFor: nn].					scriptEditors do: [:se | inSeg addAll: se allMorphs]]].	testRoots do: [:each | inSeg remove: each ifAbsent: []].	"want them to be pointed at from outside"	pointIn := IdentitySet new: 400.	inSeg do: [:ob | pointIn addAll: (PointerFinder pointersTo: ob except: inSeg)].	testRoots do: [:each | pointIn remove: each ifAbsent: []].	pointIn remove: inSeg array ifAbsent: [].	pointIn remove: pointIn array ifAbsent: [].	inSeg do: 			[:obj | 			obj isMorph 				ifTrue: 					[pointIn remove: (obj instVarAt: 3)						ifAbsent: 							["submorphs"							].					"associations in extension"					pointIn remove: obj extension ifAbsent: [].					obj extension ifNotNil: 							[obj extension otherProperties ifNotNil: 									[obj extension otherProperties associationsDo: 											[:ass | 											pointIn remove: ass ifAbsent: []											"*** and extension actorState"											"*** and ActorState instantiatedUserScriptsDictionary ScriptInstantiations"]]]].			(obj isKindOf: Player) 				ifTrue: [obj class scripts values do: [:us | pointIn remove: us ifAbsent: []]]].	"*** presenter playerlist"	self halt: 'Examine local variables pointIn and inSeg'.	^pointIn! !!ImageSegment methodsFor: 'testing' stamp: 'nk 2/22/2005 15:26'!findRogueRootsRefStrm: rootArray 	"This is a tool to track down unwanted pointers into the segment.  If we don't deal with these pointers, the segment turns out much smaller than it should.  These pointers keep a subtree of objects out of the segment.1) assemble all objects that should be in the segment by using SmartReference Stream and a dummyReference Stream.  Put in a Set.2) Remove the roots from this list.  Ask for senders of each.  Of the senders, forget the ones that are in the segment already.  Keep others.  The list is now all the 'incorrect' pointers into the segment."	| dummy goodInSeg inSeg ok pointIn |	dummy := ReferenceStream on: (DummyStream on: nil).	"Write to a fake Stream, not a file"	rootArray do: 			[:root | 			dummy rootObject: root.	"inform him about the root"			dummy nextPut: root].	inSeg := dummy references keys.	dummy := nil.	Smalltalk garbageCollect.	"dump refs dictionary"	rootArray do: [:each | inSeg remove: each ifAbsent: []].	"want them to be pointed at from outside"	pointIn := IdentitySet new: 500.	goodInSeg := IdentitySet new: 2000.	inSeg do: 			[:obj | 			ok := obj class isPointers.			obj class == Color ifTrue: [ok := false].			obj class == TranslucentColor ifTrue: [ok := false].			obj class == Array ifTrue: [obj size = 0 ifTrue: [ok := false]].			"shared #() in submorphs of all Morphs"			ok ifTrue: [goodInSeg add: obj]].	goodInSeg 		do: [:ob | pointIn addAll: (PointerFinder pointersTo: ob except: #())].	inSeg do: [:each | pointIn remove: each ifAbsent: []].	rootArray do: [:each | pointIn remove: each ifAbsent: []].	pointIn remove: inSeg array ifAbsent: [].	pointIn remove: goodInSeg array ifAbsent: [].	pointIn remove: pointIn array ifAbsent: [].	self halt: 'Examine local variables pointIn and inSeg'.	^pointIn! !!JoystickMorph methodsFor: 'menu' stamp: 'nk 2/22/2005 15:26'!chooseJoystickNumber	"Allow the user to select a joystick number"	| result aNumber str |	str := self lastRealJoystickIndex asString.	result := FillInTheBlank 				request: 'Joystick device number (currently ' translated , str , ')'				initialAnswer: str.	[aNumber := result asNumber] on: Error do: [:err | ^Beeper beep].	(aNumber > 0 and: [aNumber <= 32]) 		ifFalse: 			["???"			^Beeper beep].	realJoystickIndex := aNumber.	self setProperty: #lastRealJoystickIndex toValue: aNumber.	self startStepping! !!ProcessBrowser methodsFor: 'process actions' stamp: 'nk 2/22/2005 15:26'!inspectPointers	| tc pointers |	selectedProcess ifNil: [^self].	tc := thisContext.	pointers := PointerFinder pointersTo: selectedProcess				except: { 						self processList.						tc.						self}.	pointers isEmpty ifTrue: [^self].	OrderedCollectionInspector 		openOn: pointers		withEvalPane: false		withLabel: 'Objects pointing to ' , selectedProcess browserPrintString! !!ReleaseBuilder methodsFor: 'squeakland' stamp: 'nk 2/22/2005 15:27'!makeSqueaklandReleasePhaseCleanup	"ReleaseBuilder new makeSqueaklandReleasePhaseCleanup"	Browser initialize.	ChangeSorter 		removeChangeSetsNamedSuchThat: [:cs | cs name ~= ChangeSet current name].	ChangeSet current clear.	ChangeSet current name: 'Unnamed1'.	Smalltalk garbageCollect.	"Reinitialize DataStream; it may hold on to some zapped entitities"	DataStream initialize.	"Remove existing player references"	References keys do: [:k | References removeKey: k].	Smalltalk garbageCollect.	ScheduledControllers := nil.	Behavior flushObsoleteSubclasses.	Smalltalk		garbageCollect;		garbageCollect.	SystemNavigation default obsoleteBehaviors isEmpty 		ifFalse: [self error: 'Still have obsolete behaviors'].	"Reinitialize DataStream; it may hold on to some zapped entitities"	DataStream initialize.	Smalltalk fixObsoleteReferences.	Smalltalk abandonTempNames.	Smalltalk zapAllOtherProjects.	Smalltalk forgetDoIts.	Smalltalk flushClassNameCache.	3 timesRepeat: 			[Smalltalk garbageCollect.			Symbol compactSymbolTable]! !!ShortRunArray class methodsFor: 'class initialization' stamp: 'nk 2/22/2005 15:29'!startUpFrom: anImageSegment 	"In this case, do we need to swap word halves when reading this segement?"	^SmalltalkImage current endianness ~~ anImageSegment endianness 		ifTrue: [Message selector: #swapRuns	"will be run on each instance"]		ifFalse: [nil]! !!SmalltalkImage methodsFor: 'image cleanup' stamp: 'nk 2/22/2005 15:30'!fixObsoleteReferences	"SmalltalkImage current fixObsoleteReferences"	| informee obsoleteBindings obsName realName realClass |	Preference allInstances do: 			[:each | 			informee := each instVarNamed: #changeInformee.			((informee isKindOf: Behavior) and: [informee isObsolete]) 				ifTrue: 					[Transcript						show: each name;						cr.					each instVarNamed: #changeInformee						put: (Smalltalk 								at: (informee name copyReplaceAll: 'AnObsolete' with: '') asSymbol)]].	CompiledMethod allInstances do: 			[:method | 			obsoleteBindings := method literals select: 							[:literal | 							literal isVariableBinding and: [literal value isBehavior]								and: [literal value isObsolete]].			obsoleteBindings do: 					[:binding | 					obsName := binding value name.					Transcript						show: obsName;						cr.					realName := obsName copyReplaceAll: 'AnObsolete' with: ''.					realClass := Smalltalk at: realName asSymbol ifAbsent: [UndefinedObject].					binding isSpecialWriteBinding 						ifTrue: [binding privateSetKey: binding key value: realClass]						ifFalse: [binding key: binding key value: realClass]]].	Behavior flushObsoleteSubclasses.	Smalltalk		garbageCollect;		garbageCollect.	(obsoleteBindings := SystemNavigation default obsoleteBehaviors) size > 0 		ifTrue: [obsoleteBindings inspect]! !!SmalltalkImage methodsFor: 'image cleanup' stamp: 'nk 2/22/2005 15:27'!prepareReleaseImage	"Perform various image cleanups in preparation for making a Squeak gamma release candidate image."	"SmalltalkImage current prepareReleaseImage"	| projectChangeSetNames |	(self 		confirm: 'Are you sure you want to prepare a release image?This will perform several irreversible cleanups on this image.') 			ifFalse: [^self].	Undeclared removeUnreferencedKeys.	StandardScriptingSystem initialize.	Preferences initialize.	Preferences chooseInitialSettings.	(Object classPool at: #DependentsFields) size > 1 		ifTrue: [self error: 'Still have dependents'].	Undeclared isEmpty ifFalse: [self error: 'Please clean out Undeclared'].	Browser initialize.	ScriptingSystem deletePrivateGraphics.	"?"	"Delete all changesets except those currently used by existing projects."	projectChangeSetNames := Project allSubInstances 				collect: [:proj | proj changeSet name].	ChangeSorter 		removeChangeSetsNamedSuchThat: [:cs | (projectChangeSetNames includes: cs) not].	ChangeSet current clear.	ChangeSet current name: 'Unnamed1'.	Smalltalk garbageCollect.	"Reinitialize DataStream; it may hold on to some zapped entitities"	DataStream initialize.	"Remove existing player references"	References keys do: [:k | References removeKey: k].	"?"	Smalltalk garbageCollect.	ScheduledControllers := nil.	Smalltalk garbageCollect.	SMSqueakMap default purge.	Behavior flushObsoleteSubclasses.	Smalltalk		garbageCollect;		garbageCollect.	SystemNavigation default obsoleteBehaviors isEmpty 		ifFalse: [self error: 'Still have obsolete behaviors'].	DataStream initialize.	self fixObsoleteReferences.	Smalltalk forgetDoIts.	"?"	Smalltalk flushClassNameCache.	"?"	3 timesRepeat: 			[Smalltalk garbageCollect.			Symbol compactSymbolTable]! !!SoundBuffer class methodsFor: 'objects from disk' stamp: 'nk 2/22/2005 15:29'!startUpFrom: anImageSegment 	"In this case, do we need to swap word halves when reading this segment?"	^SmalltalkImage current endianness ~~ anImageSegment endianness 		ifTrue: [Message selector: #swapHalves	"will be run on each instance"]		ifFalse: [nil]! !!SystemNavigation methodsFor: 'browse' stamp: 'nk 2/22/2005 15:28'!browseAllObjectReferencesTo: anObject except: objectsToExclude ifNone: aBlock 	"Bring up a list inspector on the objects that point to anObject.	If there are none, then evaluate aBlock on anObject.  "	| aList shortName |	aList := PointerFinder pointersTo: anObject except: objectsToExclude.	aList size > 0 ifFalse: [^aBlock value: anObject].	shortName := (anObject name ifNil: [anObject printString]) contractTo: 20.	OrderedCollectionInspector 		openOn: aList		withEvalPane: false		withLabel: 'Objects pointing to ' , shortName! !!SystemNavigation methodsFor: 'query' stamp: 'nk 2/22/2005 15:31'!obsoleteMethodReferences	"SystemNavigation default obsoleteMethodReferences"	"Open a browser on all referenced behaviors that are obsolete"	| obsClasses obsRefs references |	references := WriteStream on: Array new.	obsClasses := self obsoleteBehaviors.	'Scanning for methods referencing obsolete classes' 		displayProgressAt: Sensor cursorPoint		from: 1		to: obsClasses size		during: 			[:bar | 			obsClasses keysAndValuesDo: 					[:index :each | 					bar value: index.					obsRefs := PointerFinder pointersTo: each except: obsClasses.					obsRefs do: 							[:ref | 							"Figure out if it may be a global"							(ref isVariableBinding and: [ref key isString	"or Symbol"]) 								ifTrue: 									[(PointerFinder pointersTo: ref) do: 											[:meth | 											(meth isKindOf: CompiledMethod) 												ifTrue: [meth methodReference ifNotNilDo: [:mref | references nextPut: mref]]]]]]].	^references contents! !!SystemNavigation methodsFor: '*tools-browser' stamp: 'nk 2/22/2005 15:28'!browseObsoleteMethodReferences	"Open a browser on all referenced behaviors that are obsolete"	"SystemNavigation default browseObsoleteMethodReferences"	| list |	list := self obsoleteMethodReferences.	self 		browseMessageList: list		name: 'Method referencing obsoletes'		autoSelect: nil! !