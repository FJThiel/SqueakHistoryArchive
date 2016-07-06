'From Squeak3.1alpha of 5 February 2001 [latest update: #3674] on 20 February 2001 at 5:52:18 pm'!"Change Set:		appearanceAgainDate:			20 February 2001Author:			Bob Arningin ObjectPropertiesMorph made solid/gradient checkboxes rather than buttons"!!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:49'!rebuild	self removeAllMorphs.	self addARow: {		self lockedString: 'Properties for ',targetMorph name.	}.	self addARow: {		self inAColumn: {			self paneForCornerRoundingToggle.			self paneForStickinessToggle.			self paneForLockedToggle.		}.	}.	self addARow: {		self paneForMainColorPicker.		self paneFor2ndGradientColorPicker.	}.	self addARow: {		self paneForBorderColorPicker.		self paneForShadowColorPicker.	}.	self addARow: {		self 			buttonNamed: 'Accept' action: #doAccept color: color lighter 			help: 'keep changes made and close panel'.		self 			buttonNamed: 'Cancel' action: #doCancel color: color lighter 			help: 'cancel changes made and close panel'.	}.	revertSteps _ Dictionary new.	revertSteps at: #fillStyle: put: targetMorph fillStyle.	revertSteps at: #hasDropShadow: put: targetMorph hasDropShadow.	revertSteps at: #shadowColor: put: targetMorph shadowColor.	(targetMorph respondsTo: #borderColor:) ifTrue: [		revertSteps at: #borderColor: put: targetMorph borderColor.	].	revertSteps at: #borderWidth: put: targetMorph borderWidth.	revertSteps at: #cornerStyle: put: targetMorph cornerStyle.	revertSteps at: #cornerStyle: put: targetMorph cornerStyle.	revertSteps at: #sticky: put: targetMorph isSticky.	revertSteps at: #lock: put: targetMorph isLocked. 	"menu add: 'shadow offset...' target: self selector: #setShadowOffset:."! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:44'!targetHasGradientFill	^targetMorph fillStyle isGradientFill! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:47'!targetHasSolidFill	^targetMorph fillStyle isSolidFill! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:45'!toggleTargetGradientFill	self targetHasGradientFill ifTrue: [		self makeTargetSolidFill	] ifFalse: [		self makeTargetGradientFill	].	self doEnables! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:50'!toggleTargetRadial	| fs |	(fs _ targetMorph fillStyle) isGradientFill ifFalse: [^self].	fs radial: fs radial not.	self doEnables.! !!ObjectPropertiesMorph methodsFor: 'as yet unclassified' stamp: 'RAA 2/20/2001 17:48'!toggleTargetSolidFill	self targetHasSolidFill ifTrue: [		self makeTargetGradientFill	] ifFalse: [		self makeTargetSolidFill	].	self doEnables! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 17:42'!paneFor2ndGradientColorPicker	^self 		inAColumn: {			(self inAColumn: {				self colorPickerFor: self getter: #tgt2ndGradientColor setter: #tgt2ndGradientColor:.				self lockedString: '2nd gradient color'.				self paneForRadialGradientToggle.			}			named: #pickerFor2ndGradientColor) layoutInset: 0.			self inAColumn: {				self paneForGradientFillToggle.			} 		}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 17:44'!paneForGradientFillToggle	^self inARow: {		self			directToggleButtonFor: self 			getter: #targetHasGradientFill			setter: #toggleTargetGradientFill			help: 'Turn gradient fill on or off'.		self lockedString: ' Gradient fill'.	}! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 17:47'!paneForMainColorPicker	^self 		inAColumn: {			self 				colorPickerFor: self 				getter: #numberOneColor 				setter: #numberOneColor:.			self lockedString: 'Color'.			self paneForSolidFillToggle.		} 		named: #pickerForColor.! !!ObjectPropertiesMorph methodsFor: 'panes' stamp: 'RAA 2/20/2001 17:47'!paneForSolidFillToggle	^self inARow: {		self			directToggleButtonFor: self 			getter: #targetHasSolidFill			setter: #toggleTargetSolidFill			help: 'Turn solid fill on or off'.		self lockedString: ' Solid fill'.	}! !ObjectPropertiesMorph removeSelector: #hasGradientFill!