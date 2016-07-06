'From Squeak 2.3 of January 14, 1999 on 8 April 1999 at 11:00:31 am'!"Change Set:		wnld-updatesDate:			8 April 1999Author:			Jeff PierceUpdates for Wonderlands.- Fixes for animated destroy- Actors modified to use UniClasses"!Object subclass: #Wonderland	instanceVariableNames: 'myScheduler myNamespace sharedMeshDict sharedTextureDict myUndoStack defaultCamera cameraList lightList sceneObject scriptEditor actorClassList '	classVariableNames: 'ActorPrototypeClasses '	poolDictionaries: 'WonderlandConstants '	category: 'Wonderland-Core'!!Wonderland methodsFor: 'initialize-reset-release' stamp: 'jsp 4/6/1999 16:37'!initialize	"This method initializes the Wonderland."	| ground |	"Initialize the list of actor UniClasses"	actorClassList _ OrderedCollection new.	"Initialize the shared mesh and texture directories"	sharedMeshDict _ Dictionary new.	sharedTextureDict _ Dictionary new.	"Initialize this Wonderland's shared namespace"	myNamespace _ WonderlandConstants copy.	"Create the Wonderland's scheduler"	myScheduler _ Scheduler new.	myNamespace at: 'scheduler' put: myScheduler.	"Create the undo stack for this Wonderland."	myUndoStack _ WonderlandUndoStack new.	"The scene object is the root of the object tree - all objects in the Wonderland are children (directly or indirectly) of the scene. "	sceneObject _ WonderlandScene newFor: self.	myNamespace at: 'scene' put: sceneObject.	"Create some default objects"	ground _ self makeActor.	ground setMesh: (WonderlandConstants at: 'groundMesh').	ground setTexturePointer: (WonderlandConstants at: 'groundTexture').	ground becomePart.	ground setName: 'ground'.	myNamespace at: 'ground' put: ground.	myUndoStack closeStack.	"Initialize the light list and create a default light"	lightList _ OrderedCollection new.	lightList addLast: (B3DAmbientLight color: Color white).		"Create the default camera"	cameraList _ OrderedCollection new.	defaultCamera _ WonderlandCamera createFor: self.	cameraList addLast: defaultCamera.	myNamespace at: 'camera' put: defaultCamera.	myNamespace at: 'cameraWindow' put: (defaultCamera getMorph).	defaultCamera setName: 'camera'.	myUndoStack openStack.	"Throw this Wonderland into the shared namespace"	myNamespace at: 'w' put: self.	"Create a WonderlandMorph for this Wonderland"	(WonderlandMorph newFor: self) openInWorld.	"Create a script editor for this Wonderland"	self makeScriptEditor.! !!Wonderland methodsFor: 'initialize-reset-release' stamp: 'jsp 4/6/1999 16:59'!release	"This method cleans up the Wonderland."	cameraList do: [:camera | camera release].	scriptEditor delete.	"Clean up any uniclasses we created"	actorClassList do: [:aClass | Smalltalk removeClassFromSystem: aClass ].! !!Wonderland methodsFor: 'initialize-reset-release' stamp: 'jsp 4/6/1999 17:00'!reset	"Reset this Wonderland"	| ground |	"Reset the scheduler"	myScheduler reset.	"Reset the undo stack"	myUndoStack reset.	"Reset the shared mesh and texture directories"	sharedMeshDict _ Dictionary new.	sharedTextureDict _ Dictionary new.	"Reset the list of actor uniclasses"	actorClassList do: [:aClass | Smalltalk removeClassFromSystem: aClass ].	actorClassList _ OrderedCollection new.	"Initialize this Wonderland's shared namespace"	myNamespace _ WonderlandConstants copy.	"Rebuild the namespace"	myNamespace at: 'scheduler' put: myScheduler.	myNamespace at: 'w' put: self.	"Create a new scene"	sceneObject _ WonderlandScene newFor: self.	myNamespace at: 'scene' put: sceneObject.	"Recreate the default objects"	ground _ self makeActor.	ground setMesh: (WonderlandConstants at: 'groundMesh').	ground setTexturePointer: (WonderlandConstants at: 'groundTexture').	ground becomePart.	ground setName: 'Ground'.	myNamespace at: 'ground' put: ground.	myUndoStack closeStack.	"Re-initialize the light list and create a default light"	lightList _ OrderedCollection new.	lightList addLast: (B3DAmbientLight color: Color white).	"Wipe out the existing cameras"	cameraList do: [:camera | camera release].		"Recreate the default camera"	cameraList _ OrderedCollection new.	defaultCamera _ WonderlandCamera createFor: self.	cameraList addLast: defaultCamera.	myNamespace at: 'camera' put: defaultCamera.	myNamespace at: 'cameraWindow' put: (defaultCamera getMorph).	defaultCamera setName: 'Camera'.	"Reset the script editor's namespace"	scriptEditor resetNamespace.	myUndoStack openStack.! !!Wonderland methodsFor: 'creating' stamp: 'jsp 4/8/1999 10:53'!makeActorFrom: filename	"Creates a new actor using the specification from the given file"	| aFile words line startSubstr index parent name texture meshFile matrix baseActor newActor protoClass actorClass |	myUndoStack closeStack.	words _ (filename findTokens: #.).	((words last) = 'mdl') ifTrue: [			aFile _ (CrLfFileStream readOnlyFileNamed: filename) ascii.			"First see if we need to create a prototype class for this model"			(ActorPrototypeClasses includesKey: (aFile localName))					ifTrue: [ protoClass _ ActorPrototypeClasses at: (aFile localName) ]					ifFalse: [							"Make a new prototype class for this model"							protoClass _ (WonderlandActor newUniqueClassInstVars: ''															classInstVars: '').							ActorPrototypeClasses at: (aFile localName) put: protoClass.							].			newActor _ OrderedCollection new.			[(aFile upTo: $() = ''] whileFalse: [					line _ aFile upTo: (Character cr).					"See if we're creating a new object"					(line beginsWith: 'MakeObject') ifTrue: [						words _ line findTokens: #,.						parent _ (words at: 2) withBlanksTrimmed.						name _ (((words at: 3) withBlanksTrimmed) findBetweenSubStrs: '"') at: 1.						"Now pull in the texture to use"						startSubstr _ name , '.SetTexture'.						[(line _ aFile upTo: (Character cr)) beginsWith: startSubstr] whileFalse: [].						texture _ (line findBetweenSubStrs: '"') at: 2.						texture _ (aFile directory pathName), '\', texture.						"Read the composite matrix to use"						startSubstr _ name , '._SetLocalTransformation'.						[(line _ aFile upTo: (Character cr)) beginsWith: startSubstr] whileFalse: [].						matrix _ B3DMatrix4x4 new.						words _ line findBetweenSubStrs: ',()'.						words removeAllSuchThat: [:str | str = ' '].						index _ words size.						4 to: 1 by: -1 do: [:i | 							4 to: 1 by: -1 do: [:j | matrix at: i at: j put: 										((words at: index) withBlanksTrimmed) asNumber.										   		index _ index - 1. ]. ].						1 to: 4 do: [:i | index _ matrix at: i at: 4.									   matrix at: i at: 4 put: (matrix at: 4 at: i).									   matrix at: 4 at: i put: index. ].						matrix a14: (matrix a14 negated).						"Read the mesh file to use"						startSubstr _ 'LoadGeometry'.						[(line _ aFile upTo: (Character cr)) beginsWith: startSubstr] whileFalse: [].						meshFile _ (line findBetweenSubStrs: '"') at: 2.						meshFile _ (aFile directory pathName), '\', meshFile.						"Now build the actor name"						words _ name findTokens: '.'.						name _ words last.						name at: 1 put: ((name at: 1) asLowercase).						words _ parent findTokens: '.'.						parent _ words last.						parent at: 1 put: ((parent at: 1) asLowercase).						"Now create the object"						(parent = 'none') ifTrue: [								actorClass _ protoClass newUniqueClassInstVars: '' classInstVars: ''.								baseActor _ actorClass createFor: self.								actorClassList addLast: actorClass.								baseActor setName: name.								baseActor setTexture: texture.								baseActor loadMeshFromFile: meshFile.								baseActor setComposite: matrix.								myNamespace at: name put: baseActor.												]  "end base actor creation"										ifFalse: [								actorClass _ WonderlandActor newUniqueClassInstVars: ''																classInstVars: ''.								newActor _ actorClass createFor: self.								actorClassList addLast: actorClass.								newActor setName: name.								parent _ (baseActor getChildNamed: parent).								newActor reparentTo: parent.								actorClassList remove: (parent class).								parent addInstanceVarNamed: name withValue: newActor.								(parent class) compile: (name , '												^ ' , name, '.').								actorClassList addLast: (parent class).								newActor becomePart.								newActor setTexture: texture.								newActor loadMeshFromFile: meshFile.								newActor setComposite: matrix.												]. "end new actor with parent"															]. "end MakeObject parsing"											]. "end file parsing"			aFile close.			myUndoStack openStack.			"Add an undo item to undo the creation of this object"			myUndoStack push: (UndoAction new: [ baseActor removeFromScene.													scriptEditor updateActorBrowser.  ] ).			^ baseActor.								]. " end mdl file parsing"! !!Wonderland class reorganize!('instance creation' new)('class initialization' initialize)('actor prototype mgmt' removeActorPrototypesFromSystem)!!Wonderland class methodsFor: 'class initialization' stamp: 'jsp 4/6/1999 16:52'!initialize	"Initialize the WonderlandClass by creating the ActorPrototypeClasses collection"	ActorPrototypeClasses _ Dictionary new.! !!Wonderland class methodsFor: 'actor prototype mgmt' stamp: 'jsp 4/7/1999 11:00'!removeActorPrototypesFromSystem	"Clean out all the actor prototypes - this involves removing those classes from the Smalltalk dictionary"	ActorPrototypeClasses do: [:aClass | Smalltalk removeClassFromSystem: aClass ].	ActorPrototypeClasses _ Dictionary new.! !!WonderlandActor methodsFor: 'parent-child' stamp: 'jsp 4/7/1999 16:06'!becomeChildOf: anObject	"Update the undo stack and make this actor a child of the specified object."	| oldParent |	"Check our arguments to make sure they're valid"	[ WonderlandVerifier VerifyReferenceFrame: anObject ]		ifError: [ :msg :rcvr |			myWonderland reportErrorToUser: 'Squeak can only make ' , myName , ' a child of other actors or the scene, and ', msg.			^ nil ].	(self == anObject) ifTrue: [		myWonderland reportErrorToUser: 'Squeak can not make ' , myName , ' a child of itself.'.		^ nil ].			oldParent _ myParent.	(myWonderland getUndoStack) push: (UndoAction new: [self reparentTo: oldParent]).	self reparentTo: anObject.! !!WonderlandActor methodsFor: 'primitive behaviors' stamp: 'jsp 4/5/1999 15:59'!destroy: aDuration	"Implements the animated destroy of an actor.  This takes all the actors parts and spins them off in an arbitrary direction"	| anim allAnims undoActions childList partsList |	[ WonderlandVerifier VerifyDuration: aDuration ]		ifError: [ :msg :rcvr |			myWonderland reportErrorToUser:				'Squeak could not determine the duration to use for destroying ' , myName , ' because ', msg.			^ nil ].	"The parameter checks out, so start the setup"	undoActions _ OrderedCollection new.	allAnims _ OrderedCollection new.	childList _ self getAllChildren.	partsList _ self getAllParts.	partsList addFirst: self.	"Do it for the top level object"	"We need to do this for every part"	childList do: [:child |		(child isPart)			ifTrue: [				"Make sure our POV gets reset on undo"				undoActions addFirst: (UndoPOVChange for: child from: (child getPointOfView)).								"Reparent parts"				undoActions addFirst: (UndoParentChange newFor: child from: (child getParent)).				child reparentTo: (myWonderland getScene).				"Create the animation for moving toward a random endpoint"				anim _ child moveTo: { (-2 to: 2) atRandom. (-2 to: 2) atRandom.							(-2 to: 2) atRandom} duration: aDuration style: abruptly.				anim stop.				anim setUndoable: false.				allAnims addLast: anim.				"Create the animation for spinning this object"				child turnToRightNow: { (0 to: 360) atRandom. (0 to: 360) atRandom.							(0 to: 360) atRandom} undoable: false.				anim _ child turn: left turns: aDuration speed: 1.				anim stop.				anim setUndoable: false.				allAnims addLast: anim.					]			ifFalse: [				"Make the non-part children point to this actor's parent"				undoActions addLast: (UndoParentChange newFor: child from: (child getParent)).				child reparentTo: (self getParent).					].					].	"Add an undo action to put the objects back in the scene"		undoActions addFirst: (UndoAction new:				[ partsList do: [:part | (myWonderland getScene) addChild: part ]]).	"Add the undo list to the stack"	(myWonderland getUndoStack) push: (UndoChangeList new setChangeList: undoActions).	"Create the animation to remove the parts from the scene and update the actor browser"	anim _ self do: [ partsList do: [:part | part removeFromScene ].					 myWonderland getEditor updateActorBrowser ].	"Now start our parallel animation"	anim _ myWonderland doInOrder: { myWonderland doTogether: allAnims. anim }.	anim setUndoable: false.! !!WonderlandActor class methodsFor: 'change logging' stamp: 'jsp 4/7/1999 15:19'!acceptsLoggingOfCompilation       ^ self == WonderlandActor.! !!WonderlandActor class methodsFor: 'change logging' stamp: 'jsp 4/7/1999 15:19'!wantsChangeSetLogging       ^ self == WonderlandActor.! !Wonderland initialize!WonderlandActor removeSelector: #acceptsLoggingOfCompilation!