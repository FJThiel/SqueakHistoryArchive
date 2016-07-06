'From Squeak3.1alpha of 28 February 2001 [latest update: #3991] on 8 May 2001 at 9:04:56 pm'!"Change Set:		WnldVocab-arDate:			8 May 2001Author:			Andreas RaabThe change set provides a specific vocabulary for Wonderland objects. After filing it in, you can 'drive your own bunny' if you wish :-)The 'viewer-tile' is now back to work with Wonderland objects, providing the vocabulary for use with eToy scripting.Note that various problems remain. Most importantly, to bypass various assumptions about the morph-player relationship, all scripts are associated with a *morph* not the WonderlandActor. Specifically, all the scripts are associated with the camera viewing the scene (which makes kinda sense because that's definitely a place where one might look to see all the running scripts in the scene). However, because of this, scripts cannot be seen in the viewer of the actor but only in the viewer of the scene. Similarly, method triggers will not work as expected. Since method triggers are bound to the costume of the player the method is defined in, making any of the methods act on a specific event (such as mouseDown) will trigger the method when the user clicks anywhere in the scene. So method triggers are basically useless, except for #ticking methods (which work pretty much as expected)."!!Vocabulary class methodsFor: 'testing and demo' stamp: 'ar 5/8/2001 19:23'!newWonderlandVocabulary	"Answer a Wonderland vocabulary -- highly experimental"	| aVocabulary  |	aVocabulary _ Vocabulary new vocabularyName: #WonderlandVocabulary.	aVocabulary documentation: 'A simple vocabulary for scripting Alice objects'.	aVocabulary initializeFromTable:  #(		(color color: () color (basic color) 'The color of the object' unused updating)		"--"		(getX setX: () number (basic geometry) 'The x position' unused updating)		(getY setY: () number (basic geometry) 'The y position' unused updating)		(getZ setZ: () number (basic geometry) 'The z position' unused updating)		"--"		(width setWidth: () number (geometry) 'The width of the object' unused updating)		(height setHeight: () number (geometry) 'The height of the object' unused updating)		(depth setDepth: () number (geometry) 'The depth of the object' unused updating)		"--"		(heading setHeading: () number (basic geometry) 'The heading of the object' unused updating)		(forwardBy: unused ((distance number)) none (basic motion) 'Moves the object by the specified distance' 'forward by')		(turnBy: unused ((angle number)) none (basic motion) 'Turns the object by the specified number of degrees' 'turn by')		(emptyScript unused () none (scripts) 'The empty script')	).	^ aVocabulary"Vocabulary initialize""Quadrangle exampleInViewer"! !!Vocabulary class methodsFor: 'testing and demo' stamp: 'ar 5/8/2001 18:52'!wonderlandVocabulary	"Answer the Quad vocabulary lurking in my AllVocabularies list, creating it if necessary"	| aVocab |	^ (self vocabularyNamed: #WonderlandVocabulary) ifNil:		[aVocab _ self newWonderlandVocabulary.		AllVocabularies add: aVocab.		^ aVocab]	"Vocabulary quadVocabulary"! !!WonderlandActor methodsFor: 'testing' stamp: 'ar 5/8/2001 20:37'!isWonderlandCamera	^false! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:38'!color	^myColor asColor! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:38'!color: aColor	self setColor: {aColor red. aColor green. aColor blue} duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:45'!depth	^self getBoundingBox extent z * 100! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:39'!forwardBy: distance	"Note: This assumes we're moving in centimeters - common Alice reference is meter!!"	self move: forward distance: (distance * 0.01) duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:39'!getX	^(self getPointOfView at: 1) * 100! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:39'!getY	^(self getPointOfView at: 2) * 100! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:39'!getZ	^(self getPointOfView at: 3) * 100! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:51'!heading	^(self getPointOfView at: 5) negated! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:39'!height	^self getBoundingBox extent y * 100! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:45'!setDepth: value	self resize: {1.0. 1.0. value / self depth asFloat} duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:51'!setHeading: heading	self setPointOfView: ((self getPointOfView) at: 5 put: heading negated; yourself) duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:45'!setHeight: value	self resize: {1.0. value / self height asFloat. 1.0} duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:40'!setWidth: value	self resize: {value / self width asFloat. 1.0. 1.0} duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:40'!setX: value	^self setPointOfView: ((self getPointOfView) at: 1 put: value * 0.01; yourself) duration: rightNow! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:40'!setY: value	^self setPointOfView: ((self getPointOfView) at: 2 put: value * 0.01; yourself) duration: rightNow! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:40'!setZ: value	^self setPointOfView: ((self getPointOfView) at: 3 put: value * 0.01; yourself) duration: rightNow! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:40'!turnBy: angle	"Note: This uses angle - Alice uses turns"	self turn: right turns: (angle / 360.0) duration: rightNow.! !!WonderlandActor methodsFor: 'eToy-basic' stamp: 'ar 5/8/2001 19:41'!width	^self getBoundingBox extent x * 100! !!WonderlandActor methodsFor: 'eToy-support' stamp: 'ar 5/8/2001 19:05'!defaultNameStemForInstances	"Answer a basis for names of default instances of the receiver."	^myName ifNil:[super defaultNameStemForInstances].! !!WonderlandActor methodsFor: 'eToy-support' stamp: 'ar 5/8/2001 18:53'!vocabularyDemanded	"Answer the vocabulary that the receiver really would like to use in a Viewer"	^ Vocabulary wonderlandVocabulary! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 19:41'!emptyScript	"Required to see an empty script slot"! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 19:41'!hasUserDefinedScripts	"We associate every script with the world"	^false! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 19:42'!methodInterfaceForEmptyScript	"Copied from Player"	| anInterface |	anInterface _ MethodInterface new.	anInterface documentation: 'an empty script; drop on desktop to get a new empty script for this object.'.	anInterface receiverType: #none.	anInterface flagAttribute: #scripts.	anInterface wording: #emptyScript selector: #emptyScript type: nil setter: nil.	^ anInterface! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 19:42'!methodInterfacesForScriptsCategoryIn: aViewer	"Needed for showing emptyScript"	^ {self methodInterfaceForEmptyScript}! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 20:48'!newScriptorAround: aPhraseTileMorph	"Fake, fake, fake."	| someCamera |	someCamera _ myWonderland getScene getAllChildren detect:[:any| any isWonderlandCamera].	^someCamera getMorph assuredPlayer assureUniClass newScriptorAround: aPhraseTileMorph! !!WonderlandActor methodsFor: 'eToy-fake' stamp: 'ar 5/8/2001 19:42'!updateAllViewersAndForceToShow: mumble	"Ugh."! !!WonderlandCamera methodsFor: 'testing' stamp: 'ar 5/8/2001 20:38'!isWonderlandCamera	^true! !!WonderlandWrapperMorph methodsFor: 'handles' stamp: 'ar 5/8/2001 20:33'!addHandlesTo: aHaloMorph box: box	| dismissHandle s |	s _ aHaloMorph handleSize.	myActor getTexturePointer == nil ifFalse:[		aHaloMorph addHandleAt: box rightCenter color: Color lightGray on: #mouseDown send: #extractTexture: to: aHaloMorph target].	aHaloMorph addHandleAt: box topLeft color: Color red icon: 'Halo-Menu'		on: #mouseDown send: #doMenu:with: to: aHaloMorph.	aHaloMorph addHandleAt: (box leftCenter + (0 @ (s+2)) min: box leftCenter + box bottomLeft // 2)		color: Color lightBrown icon: 'Halo-Tile'		on: #mouseDown send: #yourself "#tearOffTile" to: self.	dismissHandle _ aHaloMorph addHandleAt: (box topLeft + ((s+2)@0) min: box topLeft + box topCenter // 2)		color: Color red muchLighter icon: 'Halo-Dismiss'		on: #mouseDown send: #mouseDownInDimissHandle:with: to: aHaloMorph.	dismissHandle on: #mouseUp send: #maybeDismiss:with: to: aHaloMorph.	dismissHandle on: #mouseDown send: #setDismissColor:with: to: aHaloMorph.	dismissHandle on: #mouseMove send: #setDismissColor:with: to: aHaloMorph.	aHaloMorph addHandleAt: box leftCenter color: Color cyan icon: 'Halo-View'		on: #mouseDown send: #openViewerForArgument to: self.	aHaloMorph addHandleAt: box topCenter color: Color black icon: 'Halo-Grab'		on: #mouseDown send: #grabFromHalo:with: to: self.	(aHaloMorph addHandleAt: (box topCenter + ((s+2)@0) min: box topCenter + box topRight // 2)		color: Color brown icon: 'Halo-Drag'		on: #mouseDown send: #dragStartFromHalo:with: to: self)		on: #mouseMove send: #dragMoveFromHalo:with: to: self;		on: #mouseUp send: #dragEndFromHalo:with: to: self.	(aHaloMorph addHandleAt: box topRight color: Color green icon: 'Halo-Dup'		on: #mouseDown send: #dupStartFromHalo:with: to: self)		on: #mouseMove send: #dupMoveFromHalo:with: to: self;		on: #mouseUp send: #dupEndFromHalo:with: to: self.	Preferences showDebugHaloHandle ifTrue:		[aHaloMorph addHandleAt: ((box topRight + box rightCenter) // 2)			color: Color blue veryMuchLighter icon: 'Halo-Debug'			on: #mouseDown send: #doDebug:with: to: aHaloMorph].	(aHaloMorph addHandleAt: box bottomLeft color: Color blue icon: 'Halo-Rotate'		on: #mouseDown send: #rotateStartFromHalo:with: to: self)		on: #mouseMove send: #rotateMoveFromHalo:with: to: self;		on: #mouseUp send: #rotateEndFromHalo:with: to: self.	(aHaloMorph addHandleAt: box bottomRight color: Color yellow icon: 'Halo-Scale'		on: #mouseDown send: #growStartFromHalo:with: to: self)		on: #mouseMove send: #growMoveFromHalo:with: to: self;		on: #mouseUp send: #growEndFromHalo:with: to: self.	myActor isHandmade		ifTrue: [			(aHaloMorph addHandleAt: box center color: Color white icon: 'Halo-Paint'				on: #mouseUp send: #paintMode to: self getCameraMorph)]! !!WonderlandWrapperMorph methodsFor: 'handles' stamp: 'ar 5/8/2001 20:33'!openViewerForArgument	myActor beViewed.! !