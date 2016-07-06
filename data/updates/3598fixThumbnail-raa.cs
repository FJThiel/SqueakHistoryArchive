'From Squeak3.1alpha of 5 February 2001 [latest update: #3597] on 14 February 2001 at 4:32:35 pm'!"Change Set:		fixThumbnailDate:			14 February 2001Author:			Bob Arningchange thread navigator to ensure it has a thumbnail for current world"!!Project methodsFor: 'menu messages' stamp: 'RAA 2/14/2001 16:30'!makeThumbnail	"Make a thumbnail image of this project from the Display."	world isMorph ifTrue: [world displayWorldSafely]. "clean pending damage"	viewSize ifNil: [viewSize _ Display extent // 8].	thumbnail _ Form extent: viewSize depth: Display depth.	(WarpBlt current toForm: thumbnail)			sourceForm: Display;			cellSize: 2;  "installs a colormap"			combinationRule: Form over;			copyQuad: (Display boundingBox) innerCorners			toRect: (0@0 extent: viewSize).	^thumbnail! !!ProjectSorterMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/14/2001 16:30'!addControls	| b r partsBinButton newButton |	b _ SimpleButtonMorph new 		target: self;		color: self myColor darker;		borderColor: Color black.	newButton _ ImageMorph new image: (World project makeThumbnail scaledToSize: 24@18).	newButton on: #mouseDown send: #insertNewProject: to: self.	newButton setBalloonText: 'Make a new Project'.	(partsBinButton _ UpdatingThreePhaseButtonMorph checkBox)		target: self;		actionSelector: #togglePartsBinStatus;		arguments: #();		getSelector: #getPartsBinStatus.	r _ AlignmentMorph newRow		color: Color transparent;		borderWidth: 0;		layoutInset: 0;		wrapCentering: #center;		cellPositioning: #topCenter;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		extent: 5@5;		addMorphBack: (self wrapperFor: (b fullCopy label: 'Okay';	actionSelector: #acceptSort));		addMorphBack: (self wrapperFor: (b fullCopy label: 'Cancel';	actionSelector: #delete));		addMorphBack: (self wrapperFor: (newButton));		addTransparentSpacerOfSize: 8 @ 0;		addMorphBack: (self wrapperFor: partsBinButton);		addMorphBack: (self wrapperFor: (StringMorph contents: 'Parts bin') lock).	self addMorphFront: r.! !