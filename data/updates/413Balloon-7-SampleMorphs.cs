'From Squeak 2.2 of Sept 23, 1998 on 16 November 1998 at 3:05:28 am'!RectangleMorph subclass: #BalloonRectangleMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Balloon-Demos'!Morph subclass: #MatrixTransformMorph	instanceVariableNames: 'transform '	classVariableNames: 'IdentityTransform '	poolDictionaries: ''	category: 'Morphic-Balloon-Demos'!!MatrixTransformMorph class methodsFor: 'class initialization' stamp: 'ar 11/15/1998 22:09'!initialize	"MatrixTransformMorph initialize"	IdentityTransform _ MatrixTransform2x3 identity.! !!Morph methodsFor: 'rotate scale and flex' stamp: 'ar 11/15/1998 22:19'!addFlexShell	"Wrap a rotating and scaling shell around this morph."	| oldHalo flexMorph anActorState aName |	self isFlexMorph ifTrue: [^ self].	oldHalo _ self halo.	self owner addMorph:		(flexMorph _ self newTransformationMorph asFlexOf: self).	(anActorState _ self actorStateOrNil) ifNotNil:		[flexMorph actorState: anActorState.		self actorState: nil].	(aName _ self knownName) ifNotNil:		[flexMorph setNameTo: aName.		self setNameTo: nil].	self player ifNotNil:		[flexMorph player: self player.		self player rawCostume: flexMorph].	oldHalo ifNotNil: [oldHalo setTarget: flexMorph]! !!Morph methodsFor: 'rotate scale and flex' stamp: 'ar 11/15/1998 22:19'!newTransformationMorph	^TransformationMorph new! !!BalloonRectangleMorph methodsFor: 'initialize' stamp: 'ar 11/15/1998 22:31'!initialize	super initialize.	color _ GradientFillStyle ramp: {0.0 -> Color green. 0.5 -> Color yellow. 1.0 -> Color red}.	color radial: true.	borderColor _ GradientFillStyle ramp: {0.0 -> Color black. 1.0 -> Color white}.	borderWidth _ 10.	self extent: 100@100.! !!BalloonRectangleMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 22:24'!doesBevels	"To return true means that this object can show bevelled borders, and	therefore can accept, eg, #raised or #inset as valid borderColors.	Must be overridden by subclasses that do not support bevelled borders."	^ false! !!BalloonRectangleMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 22:20'!newTransformationMorph	^MatrixTransformMorph new! !!BalloonRectangleMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 22:40'!drawOn: aCanvas	(color isKindOf: OrientedFillStyle) ifTrue:[		color origin: bounds center.		color direction: (bounds extent x * 0.7) @ 0.		color normal: 0@(bounds extent y * 0.7).	].	(borderColor isKindOf: OrientedFillStyle) ifTrue:[		borderColor origin: bounds topLeft.		borderColor direction: (bounds extent x) @ 0.		borderColor normal: 0@(bounds extent y).	].	aCanvas asBalloonCanvas		drawRectangle: (bounds insetBy: borderWidth // 2)		color: color		borderWidth: borderWidth		borderColor: borderColor.! !!MatrixTransformMorph methodsFor: 'initialize' stamp: 'ar 11/15/1998 22:21'!asFlexOf: aMorph	"Initialize me with position and bounds of aMorph,	and with an offset that provides centered rotation."	self addMorph: aMorph.	self computeBounds! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:50'!addOptionalHandlesTo: aHalo box: box	aHalo addHandleAt: self referencePosition color: Color lightGray on: #mouseStillDown send: #changeRotationCenter:with: to: self! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:50'!balloonHelpTextForHandle: aHandle	aHandle eventHandler firstMouseSelector == #changeRotationCenter:with:		ifTrue:[^'set center of rotation'].	^super balloonHelpTextForHandle: aHandle! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:51'!transform	^transform ifNil:[IdentityTransform]! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:51'!transform: aMatrixTransform	transform _ aMatrixTransform.	self computeBounds.! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:52'!visible	extension == nil ifTrue:[^true].	^extension visible! !!MatrixTransformMorph methodsFor: 'accessing' stamp: 'ar 11/15/1998 21:51'!visible: aBool	extension == nil ifTrue:[		aBool ifTrue:[^self].		self assureExtension].	extension visible: aBool.! !!MatrixTransformMorph methodsFor: 'submorphs accessing' stamp: 'ar 11/15/1998 21:52'!morphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point.  Map through my transform.  Must do this recursively because of transforms.  "	| p |	self visible ifFalse:[^mList].	p _ self transform globalPointToLocal: aPoint.	submorphs do: [:m | m morphsAt: p addTo: mList].	(self containsPoint: aPoint) 		ifTrue:[mList addLast: self].	^ mList! !!MatrixTransformMorph methodsFor: 'submorphs accessing' stamp: 'ar 11/15/1998 21:52'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point.  Map through my transform.  Must do this recursively because of transforms.  "	| p |	self isLocked ifTrue:[^mList].	self visible ifFalse:[^mList].	p _ self transform globalPointToLocal: aPoint.	submorphs do: [:m | m unlockedMorphsAt: p addTo: mList].	(self containsPoint: aPoint) 		ifTrue:[mList addLast: self].	^ mList! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 22:17'!boundsChangedFrom: oldBounds to: newBounds	transform ifNil:[transform _ MatrixTransform2x3 identity].	oldBounds extent = newBounds extent ifFalse:[		transform _ transform composedWithGlobal:			(MatrixTransform2x3 withOffset: oldBounds origin negated).		transform _ transform composedWithGlobal:			(MatrixTransform2x3 withScale: newBounds extent / oldBounds extent).		transform _ transform composedWithGlobal:			(MatrixTransform2x3 withOffset: newBounds origin).	].	transform offset: transform offset + (newBounds origin - oldBounds origin)! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 22:01'!computeBounds	| subBounds |	(submorphs isNil or:[submorphs isEmpty]) ifTrue:[^self].	bounds _ nil.	submorphs do:[:m|		subBounds _ self transform localBoundsToGlobal: m bounds.		bounds 			ifNil:[bounds _ subBounds]			ifNotNil:[bounds _ bounds quickMerge: subBounds].	].	bounds ifNil:[bounds _ 0@0 corner: 20@20].	fullBounds _ bounds.! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:57'!containsPoint: aPoint	self visible ifFalse:[^false].	^bounds containsPoint: aPoint! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:52'!extent: extent	self handleBoundsChange:[super extent: extent]! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:52'!fullBounds	| subBounds |	fullBounds ifNil:[		fullBounds _ self bounds.		submorphs do:[:m|			subBounds _ (self transform localBoundsToGlobal: m fullBounds).			fullBounds _ fullBounds quickMerge: subBounds.		].	].	^fullBounds! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:52'!fullContainsPoint: aPoint	| p |	self visible ifFalse:[^false].	(self fullBounds containsPoint: aPoint) ifFalse:[^false].	(self containsPoint: aPoint) ifTrue:[^true].	p _ self transform globalPointToLocal: aPoint.	submorphs do:[:m|		(m fullContainsPoint: p) ifTrue:[^true].	].	^false! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:52'!handleBoundsChange: aBlock	| oldBounds newBounds |	oldBounds _ bounds.	aBlock value.	newBounds _ bounds.	self boundsChangedFrom: oldBounds to: newBounds.! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 21:53'!position: pos	self handleBoundsChange:[super position: pos]! !!MatrixTransformMorph methodsFor: 'geometry' stamp: 'ar 11/15/1998 22:14'!privateFullMoveBy: delta	self handleBoundsChange:[super privateMoveBy: delta]! !!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 21:59'!canvasForSubmorphs: canvasForMe	^canvasForMe asBalloonCanvas copyTransform: self transform! !!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 21:54'!changed	^super invalidRect: (self fullBounds insetBy: -1)! !!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 22:20'!drawOn: aCanvas! !!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 21:59'!fullDrawOn: aCanvas	self visible ifFalse:[^self].	^super fullDrawOn: aCanvas! !!MatrixTransformMorph methodsFor: 'drawing' stamp: 'ar 11/15/1998 21:54'!invalidRect: rect	owner ifNil:[^self].	^owner invalidRect:(self transform localBoundsToGlobal: rect).! !!MatrixTransformMorph methodsFor: 'events' stamp: 'ar 11/15/1998 21:55'!transformFrom: uberMorph	(owner isNil or:[owner == uberMorph]) ifTrue:[^self transform].	^(owner transformFrom: uberMorph) asMatrixTransform2x3 composedWithLocal: self transform! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:55'!addFlexShell	"No flex shell necessary"	self lastRotationDegrees: 0.0.! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/16/1998 01:19'!changeRotationCenter: evt with: rotHandle	| pos |	pos _ evt cursorPoint.	rotHandle referencePosition: pos.	self referencePosition: pos.! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:55'!hasNoScaleOrRotation	^true! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:56'!isFlexMorph	^false! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:56'!lastRotationDegrees	^(self valueOfProperty: #lastRotationDegrees) ifNil:[0.0].! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:56'!lastRotationDegrees: deg	deg = 0.0 		ifTrue:[self removeProperty: #lastRotationDegrees]		ifFalse:[self setProperty: #lastRotationDegrees toValue: deg]! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/16/1998 01:20'!referencePosition	| refPos |	refPos _ self valueOfProperty: #referencePosition.	refPos ifNil:[refPos _ self transform globalPointToLocal: super referencePosition].	^self transformFromWorld localPointToGlobal: refPos! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/16/1998 01:18'!referencePosition: pos	self setProperty: #referencePosition toValue: 		(self transformFromWorld globalPointToLocal: pos)! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:56'!removeFlexShell	"Do nothing"! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/16/1998 01:14'!rotateBy: delta	| pt m |	delta = 0.0 ifTrue:[^self].	self changed.	pt _ self transformFromWorld globalPointToLocal: self referencePosition.	m _ MatrixTransform2x3 withOffset: pt.	m _ m composedWithLocal: (MatrixTransform2x3 withAngle: delta).	m _ m composedWithLocal: (MatrixTransform2x3 withOffset: pt negated).	transform _ self transform composedWithLocal: m.	self computeBounds.	self changed.! !!MatrixTransformMorph methodsFor: 'flexing' stamp: 'ar 11/15/1998 21:56'!rotationDegrees: degrees	| last delta |	last _ self lastRotationDegrees.	delta _ degrees - last.	self rotateBy: delta.	self lastRotationDegrees: degrees.! !MatrixTransformMorph initialize!