'From Squeak3.1alpha of 5 February 2001 [latest update: #3671] on 20 February 2001 at 1:52:08 pm'!"Change Set:		appearance3Date:			20 February 2001Author:			Bob Arningmore improvements to object properties pane including a walkback fix"!!AlignmentMorphBob1 methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 10:40'!fullDrawOn: aCanvas	| mask |	super fullDrawOn: aCanvas.	mask _ self valueOfProperty: #disabledMaskColor ifAbsent: [^self].	aCanvas fillRectangle: bounds color: mask.! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:37'!addARow: anArray	^(super addARow: anArray) cellPositioning: #topLeft! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:40'!buildFakeSlider: nameString selector: aSymbol help: helpString	| col |	col _ self inAColumn: {		self lockedString: nameString.	}.	col 		borderWidth: 2;		borderColor: color darker;		color: color muchLighter;		hResizing: #shrinkWrap;		setBalloonText: helpString;		on: #mouseMove send: #mouseAdjust:in: to: self;		on: #mouseDown send: #mouseAdjust:in: to: self;		setProperty: #changeSelector toValue: aSymbol.	^col! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 10:49'!doEnables	| itsName fs |	fs _ targetMorph fillStyle.	self allMorphsDo: [ :each |		itsName _ each knownName.		itsName == #pickerForColor ifTrue: [			self enable: each when: fs isSolidFill | fs isGradientFill		].		itsName == #pickerForBorderColor ifTrue: [			self enable: each when: (targetMorph respondsTo: #borderColor:)		].		itsName == #pickerForShadowColor ifTrue: [			self enable: each when: targetMorph hasDropShadow		].		itsName == #pickerFor2ndGradientColor ifTrue: [			self enable: each when: fs isGradientFill		].	].! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 10:43'!enable: aMorph when: aBoolean	aBoolean = (aMorph hasProperty: #disabledMaskColor) ifFalse: [^self].	aBoolean ifTrue: [		aMorph 			removeProperty: #disabledMaskColor;			lock: false;			changed.		^self	].	aMorph 		setProperty: #disabledMaskColor toValue: (Color black alpha: 0.5);		lock: true;		changed! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 09:31'!inAColumn: aCollectionOfMorphs	| col |	col _ AlignmentMorphBob1 newColumn		color: Color transparent;		vResizing: #shrinkWrap;		layoutInset: 1;		wrapCentering: #center;		cellPositioning: #topCenter.	aCollectionOfMorphs do: [ :each | col addMorphBack: each].	^col! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:23'!initialize	super initialize.	targetMorph ifNil: [targetMorph _ RectangleMorph new openInWorld].	self borderWidth: 4.	self layoutInset: 4.	self hResizing: #shrinkWrap.	self vResizing: #shrinkWrap.	self color:  (Color r: 0.548 g: 0.839 b: 0.452).	self borderColor: self color darker.	self useRoundedCorners.	self rebuild.! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 10:46'!numberOneColor	targetMorph fillStyle isGradientFill ifFalse: [^targetMorph color].	^targetMorph fillStyle colorRamp first value! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 10:47'!numberOneColor: aColor	targetMorph fillStyle isGradientFill ifFalse: [^targetMorph color: aColor].	targetMorph fillStyle firstColor: aColor forMorph: targetMorph hand: nil! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:37'!rebuild	self removeAllMorphs.	self addARow: {		self lockedString: 'Properties for ',targetMorph name.	}.	self addARow: {		self inAColumn: {			self paneForCornerRoundingToggle.			self paneForStickinessToggle.			self paneForLockedToggle.		}.		self 			buttonNamed: 'Solid' action: #makeTargetSolidFill color: color lighter 			help: 'use a solid fill'.		self 			buttonNamed: 'Gradient' action: #makeTargetGradientFill color: color lighter 			help: 'use a gradient fill'.	}.	self addARow: {		self paneForMainColorPicker.		self paneFor2ndGradientColorPicker.	}.	self addARow: {		self paneForBorderColorPicker.		self paneForShadowColorPicker.	}.	self addARow: {		self 			buttonNamed: 'Accept' action: #doAccept color: color lighter 			help: 'keep changes made and close panel'.		self 			buttonNamed: 'Cancel' action: #doCancel color: color lighter 			help: 'cancel changes made and close panel'.	}.	revertSteps _ Dictionary new.	revertSteps at: #fillStyle: put: targetMorph fillStyle.	revertSteps at: #hasDropShadow: put: targetMorph hasDropShadow.	revertSteps at: #shadowColor: put: targetMorph shadowColor.	(targetMorph respondsTo: #borderColor:) ifTrue: [		revertSteps at: #borderColor: put: targetMorph borderColor.	].	revertSteps at: #borderWidth: put: targetMorph borderWidth.	revertSteps at: #cornerStyle: put: targetMorph cornerStyle.	revertSteps at: #cornerStyle: put: targetMorph cornerStyle.	revertSteps at: #sticky: put: targetMorph isSticky.	revertSteps at: #lock: put: targetMorph isLocked. 	"menu add: 'shadow offset...' target: self selector: #setShadowOffset:."! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 13:15'!targetBorderColor	(targetMorph respondsTo: #borderColor) ifFalse: [^Color black].	^targetMorph borderColor! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 13:16'!targetBorderColor: aColor	(targetMorph respondsTo: #borderColor:) ifFalse: [^self].	targetMorph borderColor: aColor! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:28'!targetRadial	targetMorph fillStyle isGradientFill ifFalse: [^false].	^targetMorph fillStyle radial! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 12:34'!toggleTargetRadial	| fs |	(fs _ targetMorph fillStyle) isGradientFill ifFalse: [^self].	fs radial: fs radial not! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 12:27'!paneFor2ndGradientColorPicker	^self 		inAColumn: {			self colorPickerFor: self getter: #tgt2ndGradientColor setter: #tgt2ndGradientColor:.			self lockedString: '2nd gradient color'.			self paneForRadialGradientToggle.		} 		named: #pickerFor2ndGradientColor! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 13:16'!paneForBorderColorPicker	^self 		inAColumn: {			self colorPickerFor: self			getter: #targetBorderColor			setter: #targetBorderColor:.			self lockedString: 'Border Color'.			self paneForBorderWidth.		} 		named: #pickerForBorderColor.! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 11:44'!paneForBorderWidth	^(self inARow: {		self			buildFakeSlider: 'Border width' 			selector: #borderWidth:			help: 'Drag in here to cahnge the border width'	}) hResizing: #shrinkWrap! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 09:04'!paneForCornerRoundingToggle	^self inARow: {		self			directToggleButtonFor: targetMorph 			getter: #wantsRoundedCorners setter: #toggleCornerRounding			help: 'Turn rounded corners on or off'.		self lockedString: ' Rounded corners'.	}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 12:23'!paneForDropShadowToggle	^self inARow: {		self			directToggleButtonFor: targetMorph 			getter: #hasDropShadow setter: #toggleDropShadow			help: 'Turn drop shadows on or off'.		self lockedString: ' Drop shadow color'.	}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 09:05'!paneForLockedToggle	^self inARow: {		self			directToggleButtonFor: targetMorph 			getter: #isLocked setter: #toggleLocked			help: 'Turn lock on or off'.		self lockedString: ' Lock'.	}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 10:45'!paneForMainColorPicker	^self 		inAColumn: {			self 				colorPickerFor: self 				getter: #numberOneColor 				setter: #numberOneColor:.			self lockedString: 'Color'.		} 		named: #pickerForColor.! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 12:34'!paneForRadialGradientToggle	^self inARow: {		self			directToggleButtonFor: self 			getter: #targetRadial setter: #toggleTargetRadial			help: 'Turn radial gradient on or off'.		self lockedString: ' Radial gradient'.	}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 11:04'!paneForShadowColorPicker	^self 		inAColumn: {			(self inAColumn: {				self colorPickerFor: targetMorph getter: #shadowColor setter: #shadowColor:.			}			named: #pickerForShadowColor) layoutInset: 0.			self inAColumn: {				self paneForDropShadowToggle.			} 		}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 09:04'!paneForStickinessToggle	^self inARow: {		self			directToggleButtonFor: targetMorph 			getter: #isSticky setter: #toggleStickiness			help: 'Turn stickiness on or off'.		self lockedString: ' Sticky'.	}! !ObjectPropertiesMorph removeSelector: #targetRadial:!ObjectPropertiesMorph removeSelector: #tgt1stGradientColor!ObjectPropertiesMorph removeSelector: #tgt1stGradientColor:!ObjectPropertiesMorph removeSelector: #toggleRadial!