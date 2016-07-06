'From Squeak2.8alpha of 19 January 2000 [latest update: #2098] on 10 May 2000 at 11:31:17 am'!B3DSceneMorph subclass: #AdvancedB3DSceneMorph	instanceVariableNames: 'rotationAngle stepTime isRotating oldPoint headLightStatus savedHeadLight '	classVariableNames: ''	poolDictionaries: 'B3DEngineConstants '	category: 'Balloon3D-Demo Morphs'!!AdvancedB3DSceneMorph methodsFor: 'accessing'!scene: aScene	super scene: (self updateSceneWithDefaults: aScene).	self updateUpVectorForCamera: self scene defaultCamera.	self updateHeadlight.	self changed! !!AdvancedB3DSceneMorph methodsFor: 'camera actions'!addDolly: delta	| camera new |	camera := scene defaultCamera.	new := camera position - (camera direction * delta).	camera target = new ifFalse: [		camera position: new].	"new := camera direction * delta.	camera position: camera position - new.	camera target: camera target - new."	self updateHeadlight.	self changed.! !!AdvancedB3DSceneMorph methodsFor: 'camera actions'!updateHeadlight	| headLight camera |	camera := scene defaultCamera.	(self scene lights isKindOf: Dictionary)		ifTrue: [headLight := self scene lights at: '$HeadLight$' ifAbsent: []]		ifFalse: [headLight := nil].	headLight		ifNil: [			((headLightStatus = #on) and: [self scene lights isKindOf: Dictionary]) ifTrue: [				self scene lights at: '$HeadLight$' put: savedHeadLight.				headLight := savedHeadLight]]		ifNotNil: [			(headLightStatus = #off) ifTrue: [				savedHeadLight := headLight.				self scene lights removeKey: '$HeadLight$']].	headLight ifNotNil: [		headLight			position: camera position;			target: camera target].! !!AdvancedB3DSceneMorph methodsFor: 'initialization' stamp: 'ti 5/10/2000 11:21'!initialize	super initialize.	self stepTime: 0.	self rotationAngle: 1.	self beRotating.	self switchHeadLightOn.! !!AdvancedB3DSceneMorph methodsFor: 'properties'!headLightIsOn	^(headLightStatus = #on)! !!AdvancedB3DSceneMorph methodsFor: 'properties' stamp: 'ti 5/10/2000 11:20'!switchHeadLightOff	headLightStatus := #off.	self updateHeadlight.	self changed! !!AdvancedB3DSceneMorph methodsFor: 'properties' stamp: 'ti 5/10/2000 11:20'!switchHeadLightOn	headLightStatus := #on.	self updateHeadlight.	self changed! !!AdvancedB3DSceneMorph methodsFor: 'properties' stamp: 'ti 5/10/2000 11:21'!switchHeadLightStatus	(headLightStatus = #on)		ifTrue: [self switchHeadLightOff]		ifFalse: [self switchHeadLightOn]! !!AdvancedB3DSceneMorph methodsFor: 'private'!updateSceneWithDefaults: myScene	| headLight mat |	myScene lights at: 'Ambient1' put: (B3DAmbientLight color: (Color gray: 0.2)).	headLight := B3DSpotLight new.	headLight position: myScene defaultCamera position.	headLight target: myScene defaultCamera target.	headLight lightColor: (B3DMaterialColor color: (Color gray: 0.7)).	headLight attenuation: (B3DLightAttenuation constant: 1.0 linear: 0.0 squared: 0.0).	headLight minAngle: 80.	headLight maxAngle: 90.	myScene lights at: '$HeadLight$' put: headLight copy.	mat := B3DMaterial new.	mat diffusePart: (Color gray: 0.25).	mat ambientPart: (Color gray: 0.01).	myScene objects do: [:o|		o material: mat].	^myScene! !!AdvancedB3DSceneMorph methodsFor: 'private'!updateUpVectorForCamera: aCamera	| oldUp |	oldUp := aCamera up.	aCamera up:		((aCamera direction cross: oldUp) cross: (aCamera direction))! !!B3DSceneExplorerMorph methodsFor: 'actions'!openThreeDSFile	| menu result newFileString myScene |	menu := StandardFileMenu oldFileMenu: (FileDirectory default) withPattern: '*.3ds'.	result := menu startUpWithCaption: 'Select 3DS model file ...'.	result ifNotNil: [			newFileString := (result directory pathName),(result directory pathNameDelimiter asString),(result name).		myScene := (B3DScene withoutQuestionsFrom3DS: (ThreeDSParser parseFileNamed: newFileString)).		self scene: myScene].! !!B3DSceneExplorerMorph methodsFor: 'actions'!selectNewCamera	| menu sel |	((self scene cameras isNil) or: [self scene cameras size = 0]) ifTrue: [		(SelectionMenu selections: #('OK'))			startUpWithCaption: 'No cameras defined!!'.		^self].	menu _ SelectionMenu		selections: self scene cameras keys asArray.	sel := menu startUp.	sel ifNotNil: [		self scene defaultCamera: (self scene cameras at: sel) copy.		b3DSceneMorph updateUpVectorForCamera: self scene defaultCamera.		self changed.]! !!B3DSceneExplorerMorph methodsFor: 'actions'!switchHeadLightStatus	b3DSceneMorph switchHeadLightStatus! !!B3DSceneExplorerMorph methodsFor: 'menus'!addCustomMenuItems: aCustomMenu	(aCustomMenu isKindOf: MenuMorph)		ifTrue: [aCustomMenu addUpdating: #rotationString action: #switchRotationStatus]		ifFalse: [aCustomMenu add: 'swich rotation status' action: #switchRotationStatus].	(aCustomMenu isKindOf: MenuMorph)		ifTrue: [aCustomMenu addUpdating: #headLightString action: #switchHeadLightStatus]		ifFalse: [aCustomMenu add: 'swich headlight' action: #switchHeadLightStatus].	aCustomMenu add: 'open 3DS file' action: #openThreeDSFile.	aCustomMenu add: 'select new camera' action: #selectNewCamera.! !!B3DSceneExplorerMorph methodsFor: 'menus'!headLightString	^b3DSceneMorph headLightIsOn		ifTrue: ['swich headlight off']		ifFalse: ['swich headlight on']! !!WheelMorph reorganize!('accessing' actionSelector actionSelector: angle angle: factor factor: maxAngle maxAngle: target target:)('drawing' drawOn:)('event handling' doTargetAction: handlesMouseDown: mouseDown: mouseMove:)('initialization' initialize)('private' addAngle:)('properties' beCircular beHorizontal beLinear beVertical isCircular isHorizontal isLinear isVertical)!B3DSceneExplorerMorph removeSelector: #updateSceneWithDefaults:!B3DSceneExplorerMorph removeSelector: #updateUpVectorForCamera:!!B3DSceneExplorerMorph reorganize!('accessing' scene scene:)('actions' openThreeDSFile selectNewCamera selectNewCamera: switchHeadLightStatus switchRotationStatus)('change reporting' layoutChanged)('drawing' drawOn:)('event handling' handlesMouseDown: mouseDown:)('initialization' initialize)('menus' addCustomMenuItems: addCustomMenuItems:hand: headLightString rotationString yellowButtonMenu)('visual properties' defaultColor)!!AdvancedB3DSceneMorph reorganize!('accessing' rotationAngle rotationAngle: scene: stepTime stepTime:)('camera actions' addDolly: addFovAngle: panBy: rotateX: rotateY: rotateZ: updateHeadlight)('drawing' renderOn:)('event handling' handlesMouseDown: mouseDown: mouseMove:)('initialization' createDefaultScene initialize)('properties' beRotating beStill headLightIsOn isRotating switchHeadLightOff switchHeadLightOn switchHeadLightStatus switchRotationStatus)('stepping' step)('private' updateSceneWithDefaults: updateUpVectorForCamera:)!