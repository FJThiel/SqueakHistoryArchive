'From Squeak 2.4b of April 23, 1999 on 7 July 1999 at 7:01:11 pm'!"Change Set:		WnldActViewDate:			7 July 1999Author:			Jeff PierceThis change set continues the development of the Actor Info tab for Wonderlands.  Specifically, this change set adds some smarts so that the object the user is currently focusing on is correctly framed and at a suitable distance.  This change set also changes rendering for WonderlandStillCameras so that they only return the object they're focusing on (and its children) from the scene graph."!WonderlandCamera subclass: #WonderlandStillCamera	instanceVariableNames: 'focusObject '	classVariableNames: ''	poolDictionaries: ''	category: 'Wonderland-Objects'!!WonderlandActorViewer methodsFor: 'initialization' stamp: 'jsp 7/7/1999 18:42'!initializeWith: aWonderland	"Do the Wonderland specific setup for this viewer."	thumbnailCamera _ WonderlandStillCamera createFor: aWonderland.	thumbnailCamera turnBackgroundOff.	thumbnailMorph _ thumbnailCamera getMorph.	self addMorph: thumbnailMorph.! !!WonderlandActorViewer methodsFor: 'updating' stamp: 'jsp 7/7/1999 16:18'!setSelectedActor: anActor	"Update the actor viewer to reflect the information for this actor"	selectedActor _ anActor.	thumbnailCamera setFocusObject: anActor.! !!WonderlandStillCamera methodsFor: 'initialize-release' stamp: 'jsp 7/7/1999 17:49'!initializeFor: aWonderland	"Initializes the camera."	super initializeFor: aWonderland.	"Set the camera's mesh and geometry"	self setMesh: nil.	self setTexturePointer: nil.	"Delete the WonderlandCameraMorph our superclass created"	myMorph delete.		"Create a WonderlandStillCameraMorph for the camera to render into"	myMorph _ WonderlandStillCameraMorph new.	myMorph initializeWithCamera: self.	myMorph openInWorld.	"Initially we aren't focusing on any particular object"	focusObject _ nil.	"Initially draw the scene background"	drawSceneBackground _ true.! !!WonderlandStillCamera methodsFor: 'assigning focus object' stamp: 'jsp 7/7/1999 18:54'!setFocusObject: anActor	"Assign the object in the Wonderland that the camera should focus on"	| boundingBox origin corner center maxDimension distance frustum |	focusObject _ anActor.	anActor ifNotNil: [			(anActor isKindOf: WonderlandActor)				ifTrue: [ boundingBox _ anActor getBoundingBox.						  origin _ boundingBox origin.						  corner _ boundingBox corner.						  center _ (origin + corner) / 2.0.						  self alignWith: anActor duration: rightNow.						  self turn: left turns: 0.5 duration: rightNow.						  maxDimension _ (((corner x - center x) max: (corner y - center y))												max: (center x - origin x))													max: (center y - origin y).						  frustum _ self getFrustum.						  distance _ ((frustum near) * maxDimension / (0.7 * (frustum right)))										+ (corner z).						  self moveTo: { center x.										center y.										distance }								duration: rightNow asSeenBy: anActor.						  self move: right distance: (maxDimension) duration: rightNow.						  self move: up distance: (maxDimension) duration: rightNow.						  self turn: left turns: ((((maxDimension) arcTan: distance)								radiansToDegrees) / 360.0) duration: rightNow.						  self turn: down turns: ((((maxDimension) arcTan: distance)								radiansToDegrees) / 360.0) duration: rightNow.						]				ifFalse: [ self moveTo: { -1.5. 0.5. 2.6 } duration: rightNow asSeenBy: anActor.						  self move: back distance: 20 duration: rightNow.						  self pointAt: { 0. 0. 0 } duration: rightNow ].		].	myMorph changed.! !!WonderlandStillCamera methodsFor: 'rendering' stamp: 'jsp 7/7/1999 18:42'!render: aRenderEngine pickingAt: aPointOrNil withPrimitiveVertex: aBoolean	"Override the parent method. A WonderlandStillCamera only renders the object that it's focusing on."	| aRenderer pickedObject |	aRenderer _ aRenderEngine. "A couple of things must be set before we can pick!!"	aRenderer viewport: (myMorph bounds insetBy: 1@1).	(drawSceneBackground)		ifTrue: [ aRenderer clearViewport: (myWonderland getScene getColorObject). ]		ifFalse: [ ].	aRenderer clearDepthBuffer.	aRenderer loadIdentity.	focusObject ifNotNil: [		"Add any existing lights to the renderer for this camera"		(myWonderland getLights) do: [:light | aRenderer addLight: light ].		"Calculate our view matrix by inverting the camera's composite matrix and hand it		to the renderer - note that this will eventually have to walk up the tree"		viewMatrix _ self getMatrixToRoot.		aRenderer transformBy: viewMatrix.		aRenderer perspective: perspective.		"Initialize picking if necessary"		aPointOrNil ifNotNil:[aRenderer _ aRenderer asPickerAt: aPointOrNil].		(focusObject isKindOf: WonderlandScene)			ifTrue: [ myWonderland renderWonderland: aRenderer ]			ifFalse: [ aRenderer transformBy: (focusObject getParent getMatrixFromRoot).					  focusObject renderOn: aRenderer ].		"Fetch the picked object"		aPointOrNil ifNotNil:[			aBoolean				ifTrue:[pickedObject _ aRenderer topMostObject -> aRenderer topMostVertex]				ifFalse:[pickedObject _ aRenderer topMostObject]].	].	"Force the renderer to draw to the screen"	aRenderer finish.	aRenderer destroy.	^pickedObject "Will be nil if not picking".! !