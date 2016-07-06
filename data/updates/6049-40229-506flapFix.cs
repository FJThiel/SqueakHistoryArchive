'From Squeak3.8alpha of ''17 July 2004'' [latest update: #5976] on 6 August 2004 at 11:45:10 am'!!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 8/6/2004 11:37'!newLoneSuppliesFlap	"Answer a fully-instantiated flap named 'Supplies' to be placed at the bottom of the screen, for use when it is the only flap shown upon web launch"	|  aFlapTab aStrip leftEdge |  "Flaps setUpSuppliesFlapOnly"	aStrip _ PartsBin newPartsBinWithOrientation: #leftToRight andColor: Color red muchLighter from:	 #(	(TrashCanMorph			new						'Trash'				'A tool for discarding objects')		(ScriptingSystem 		scriptControlButtons 			'Status'				'Buttons to run, stop, or single-step scripts')	(AllScriptsTool			allScriptsToolForActiveWorld	'All Scripts' 		'A tool that lets you control all the running scripts in your world')	(PaintInvokingMorph	new						'Paint'				'Drop this into an area to start making a fresh painting there')	(RectangleMorph 		authoringPrototype		'Rectangle' 		'A rectangle'	)	(RectangleMorph		roundRectPrototype		'RoundRect'		'A rectangle with rounded corners')	(EllipseMorph			authoringPrototype		'Ellipse'			'An ellipse or circle')	(StarMorph				authoringPrototype		'Star'			'A star')	(CurveMorph			authoringPrototype		'Curve'			'A curve')	(PolygonMorph			authoringPrototype		'Polygon'		'A straight-sided figure with any number of sides')	(TextMorph				authoringPrototype		'Text'			'Text that you can edit into anything you desire.')	(SimpleSliderMorph		authoringPrototype		'Slider'			'A slider for showing and setting numeric values.')	(JoystickMorph			authoringPrototype		'Joystick'		'A joystick-like control')	(ScriptingSystem		prototypicalHolder 		'Holder'			'A place for storing alternative pictures in an animation, ec.')	(ScriptableButton		authoringPrototype		'Button'			'A Scriptable button')	(PasteUpMorph			authoringPrototype		'Playfield'		'A place for assembling parts or for staging animations')	(BookMorph				authoringPrototype		'Book'			'A multi-paged structure')	(TabbedPalette			authoringPrototype		'Tabs'			'A structure with tabs')	(RecordingControlsMorph authoringPrototype			'Sound'				'A device for making sound recordings.')	(MagnifierMorph		newRound					'Magnifier'			'A magnifying glass')	(ImageMorph			authoringPrototype		'Picture'		'A non-editable picture of something')	(ClockMorph				authoringPrototype		'Clock'			'A simple digital clock')	(BookMorph				previousPageButton 		'Previous'		'A button that takes you to the previous page')	(BookMorph				nextPageButton			'Next'			'A button that takes you to the next page')		).	aFlapTab _ FlapTab new referent: aStrip beSticky.	aFlapTab setName: 'Supplies' translated edge: #bottom color: Color red lighter.	aStrip extent: self currentWorld width @ 78.	leftEdge _ ((Display width - (16  + aFlapTab width)) + 556) // 2.	aFlapTab position: (leftEdge @ (self currentWorld height - aFlapTab height)).	aStrip beFlap: true.	aStrip autoLineLayout: true.		^ aFlapTab! !!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 8/6/2004 11:39'!newStackToolsFlap	"Add a flap with stack tools in it"	| aFlapTab aStrip |	aStrip _ PartsBin newPartsBinWithOrientation: #leftToRight		andColor: (Color red muchLighter "alpha: 0.2") from: self quadsDefiningStackToolsFlap.	aFlapTab _ FlapTab new referent: aStrip beSticky.	aFlapTab setName: 'Stack Tools' translated edge: #bottom color: Color brown lighter lighter.	aFlapTab position: ((Display width - (aFlapTab width + 226)) @ (self currentWorld height - aFlapTab height)).	aFlapTab setBalloonText: aFlapTab balloonTextForFlapsMenu.	aStrip extent: self currentWorld width @ 78.	aStrip beFlap: true.	aStrip autoLineLayout: true.	aStrip extent: self currentWorld width @ 70.	^ aFlapTab"Flaps replaceGlobalFlapwithID: 'Stack Tools' translated"! !!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 8/6/2004 11:39'!newSuppliesFlapFromQuads: quads positioning: positionSymbol
	"Answer a fully-instantiated flap named 'Supplies' to be placed at the bottom of the screen.  Use #center as the positionSymbol to have it centered at the bottom of the screen, or #right to have it placed off near the right edge."

	|  aFlapTab aStrip hPosition |
	aStrip _ PartsBin newPartsBinWithOrientation: #leftToRight andColor: Color red muchLighter from:	 quads.
	self twiddleSuppliesButtonsIn: aStrip.
	aFlapTab _ FlapTab new referent: aStrip beSticky.
	aFlapTab setName: 'Supplies' translated edge: #bottom color: Color red lighter.
	hPosition _ positionSymbol == #center
		ifTrue:
			[(Display width // 2) - (aFlapTab width // 2)]
		ifFalse:
			[Display width - (aFlapTab width + 22)].
	aFlapTab position: (hPosition @ (self currentWorld height - aFlapTab height)).
	aFlapTab setBalloonText: aFlapTab balloonTextForFlapsMenu.

	aStrip extent: self currentWorld width @ 78.
	aStrip beFlap: true.
	aStrip autoLineLayout: true.
	
	^ aFlapTab

"Flaps replaceGlobalFlapwithID: 'Supplies' translated"! !!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 8/6/2004 11:39'!newToolsFlap	"Answer a newly-created flap which adheres to the right edge of the screen and which holds prototypes of standard tools."	|  aFlapTab aStrip |	aStrip _ PartsBin newPartsBinWithOrientation: #topToBottom andColor: (Color orange muchLighter alpha: 0.8) from: self quadsDefiningToolsFlap. 	aFlapTab _ FlapTab new referent: aStrip beSticky.	aFlapTab setName: 'Tools' translated edge: #right color: Color orange lighter.	aFlapTab position: (self currentWorld width - aFlapTab width) @ ((Display height - aFlapTab height) // 2).	aFlapTab setBalloonText: aFlapTab balloonTextForFlapsMenu.	aStrip extent: (90 @ self currentWorld height).	aStrip beFlap: true.		^ aFlapTab"Flaps replaceGlobalFlapwithID: 'Tools' translated "! !!Flaps class methodsFor: 'predefined flaps' stamp: 'nk 8/6/2004 11:40'!newWidgetsFlap	"Answer a newly-created flap which adheres to the bottom edge of the screen and which holds prototypes of standard widgets. "	|  aFlapTab aStrip |	aStrip _ PartsBin newPartsBinWithOrientation: #leftToRight andColor: (Color blue muchLighter alpha: 0.8)		from:	 self quadsDefiningWidgetsFlap.	aFlapTab _ FlapTab new referent: aStrip beSticky.	aFlapTab setName: 'Widgets' translated edge: #bottom color: Color blue lighter lighter.	aFlapTab position: ((Display width - (aFlapTab width + 122)) @ (self currentWorld height - aFlapTab height)).	aFlapTab setBalloonText: aFlapTab balloonTextForFlapsMenu.	aStrip extent: self currentWorld width @ 78.	aStrip beFlap: true.	aStrip autoLineLayout: true.		^ aFlapTab"Flaps replaceGlobalFlapwithID: 'Widgets' translated "! !!IconicButton methodsFor: 'initialization' stamp: 'nk 8/6/2004 11:30'!initializeToShow: aMorph withLabel: aLabel andSend: aSelector to: aReceiver 		"Initialize the receiver to show the current appearance of aMorph on its face, giving it the label supplied and arranging for it, when the button goes down on it, to obtain a new morph by sending the specified selector to the specified receiver"	| aThumbnail |	aThumbnail _ Thumbnail new.	aThumbnail makeThumbnailFromForm: aMorph imageForm.	^ self initializeWithThumbnail: aThumbnail withLabel: aLabel andColor: self color andSend: aSelector to: aReceiver 	! !!IconicButton methodsFor: 'initialization' stamp: 'nk 8/6/2004 11:23'!initializeWithThumbnail: aThumbnail withLabel: aLabel andColor: aColor andSend: aSelector to: aReceiver 		"Initialize the receiver to show aThumbnail on its face, giving it the label supplied and arranging for it, when the button goes down on it, to obtain a new morph by sending the supplied selector to the supplied receiver"	| labeledItem  |	labeledItem _ AlignmentMorph newColumn.	labeledItem color: aColor.	labeledItem borderWidth: 0.	labeledItem		layoutInset: 4@0;		cellPositioning: #center.	labeledItem addMorph: aThumbnail.	labeledItem addMorphBack: (Morph new extent: (4@4)) beTransparent.	labeledItem addMorphBack: (TextMorph new		backgroundColor: aColor;		contentsAsIs: aLabel;		beAllFont: Preferences standardEToysFont;		centered).	self		beTransparent;		labelGraphic: (labeledItem imageForm asFormOfDepth: 16);		borderWidth: 0;		target: aReceiver;		actionSelector: #launchPartVia:label:;		arguments: {aSelector. aLabel};		actWhen: #buttonDown.	self stationarySetup.! !!IconicButton methodsFor: 'initialization' stamp: 'nk 8/6/2004 11:34'!initializeWithThumbnail: aThumbnail withLabel: aLabel andSend: aSelector to: aReceiver 		"Initialize the receiver to show aThumbnail on its face, giving it the label supplied and arranging for it, when the button goes down on it, to obtain a new morph by sending the supplied selector to the supplied receiver"	^self initializeWithThumbnail: aThumbnail withLabel: aLabel andColor: Color transparent   andSend: aSelector to: aReceiver 	! !!PartsBin methodsFor: 'dropping/grabbing' stamp: 'nk 8/6/2004 11:31'!morphToDropFrom: aMorph	"Answer the morph to drop if the user attempts to drop aMorph"	| aButton |	aButton _ IconicButton new.	aButton color: self color;		initializeToShow: aMorph withLabel: aMorph externalName andSend: #veryDeepCopy to: aMorph veryDeepCopy.	^ aButton! !!PartsBin methodsFor: 'initialization' stamp: 'nk 8/6/2004 10:42'!listDirection: aListDirection quadList: quadList	"Initialize the receiver to run horizontally or vertically, obtaining its elements from the list of tuples of the form:		(<receiver> <selector> <label> <balloonHelp>)"	| aButton aClass |	self layoutPolicy: TableLayout new.	self listDirection: aListDirection.	self wrapCentering: #topLeft.	self layoutInset: 2.	self cellPositioning: #bottomCenter.	aListDirection == #leftToRight		ifTrue:			[self vResizing: #rigid.			self hResizing: #spaceFill.			self wrapDirection: #topToBottom]		ifFalse:			[self hResizing: #rigid.			self vResizing: #spaceFill.			self wrapDirection: #leftToRight].	quadList do:		[:tuple |			aClass _ Smalltalk at: tuple first.			aButton _ IconicButton new initializeWithThumbnail: (self class thumbnailForQuad: tuple) withLabel: tuple third translated andColor: self color andSend: tuple second to: aClass.			(tuple size > 3 and: [tuple fourth isEmptyOrNil not]) ifTrue:				[aButton setBalloonText: tuple fourth translated]. 			self addMorphBack: aButton]! !!PartsBin class methodsFor: 'instance creation' stamp: 'nk 8/6/2004 11:36'!newPartsBinWithOrientation: aListDirection andColor: aColor from: quadList	"Answer a new PartBin object, to run horizontally or vertically, obtaining its elements from the list of tuples of the form:		(<receiver> <selector> <label> <balloonHelp>)"	| array |	array _ quadList collect: [:each |		| element |		element _ each copy. 		element at: 3 put: (each at: 3) translated.		element.	].	^ self new 		color: aColor;		listDirection: aListDirection quadList: array.! !!Thumbnail methodsFor: 'thumnail creation' stamp: 'nk 8/6/2004 10:25'!makeThumbnailFromForm: aForm	"Make a thumbnail from the form provided, obeying my min and max width and height preferences"	|  scaleX scaleY margin opaque |	scaleY _ minimumHeight / aForm height.  "keep height invariant"	scaleX _ ((aForm width * scaleY) <= maximumWidth)		ifTrue: [scaleY]  "the usual case; same scale factor, to preserve aspect ratio"		ifFalse: [scaleY _ maximumWidth / aForm width].	"self form: (aForm magnify: aForm boundingBox by: (scaleX @ scaleY) smoothing: 2)."	"Note: A problem with magnify:by: fails to reproduce borders properly.		The following code does a better job..."	margin _ 1.0 / (scaleX@scaleY) // 2 max: 0@0.  "Extra margin around border"	opaque _ (Form extent: aForm extent + margin depth: aForm depth) "fillWhite".	aForm displayOn: opaque at: aForm offset negated rule: Form blendAlpha.  "Opaque form shrinks better"	self form: (opaque magnify: opaque boundingBox by: (scaleX @ scaleY) smoothing: 2).	self extent: originalForm extent! !