'From Squeak2.9alpha of 16 June 2000 [latest update: #2465] on 9 August 2000 at 2:50:19 pm'!!Morph methodsFor: 'submorphs-accessing' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  Must do this recursively because of transforms.  "	(self fullBounds containsPoint: aPoint) ifFalse: [^ mList].  "quick elimination"	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	submorphs size > 0 ifTrue:		[submorphs do: [:m | m unlockedMorphsAt: aPoint addTo: mList]].	(self containsPoint: aPoint) ifTrue: [mList addLast: self].	^ mList! !!Morph methodsFor: 'submorphs-accessing' stamp: 'RAA 6/11/2000 15:43'!unlockedMorphsAtGlobal: aPoint	"Return a collection of all unlocked morphs in this morph structure that contain the given point, possibly including the receiver itself.  Simplified "	^ self unlockedMorphsAt: (self pointFromWorld: aPoint) addTo: OrderedCollection new! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'tk 7/28/2000 23:26'!initialize	super initialize.	colorMemory ifNotNil: [colorMemory on: #mouseDown send: #takeColorEvt:from: to: self].! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 14:02'!eyedropper: aButton action: aSelector cursor: aCursor         "Take total control and pick up a color!!!!"        | pt feedbackColor |        aButton state: #on.        tool ifNotNil: [tool state: #off].        currentCursor _ aCursor.        self activeHand                showTemporaryCursor: currentCursor                 hotSpotOffset: 6 negated @ 4 negated.    "<<<< the form was changed a bit??"        feedbackColor _ Display colorAt: Sensor cursorPoint.        self addMorphFront: colorMemory.        "Full color picker"        [Sensor anyButtonPressed]                whileFalse:                         [pt _ Sensor cursorPoint.                        "deal with the fact that 32 bit displays may have garbage in the alpha bits"                        feedbackColor _ Display depth = 32 ifTrue: [                                Color colorFromPixelValue: ((Display pixelValueAt: pt)														bitOr: 16rFF000000) depth: 32                        ] ifFalse: [                                Display colorAt: pt                        ].                        "the hand needs to be drawn"                        self activeHand position: pt.                        self world displayWorldSafely.                        "Display fill: colorPatch bounds fillColor: feedbackColor"].        Sensor waitNoButton.        self activeHand showTemporaryCursor: nil hotSpotOffset: 0 @ 0.        self currentColor: feedbackColor.        colorMemory delete.		         tool                ifNotNil:                         [tool state: #on.                        currentCursor _ tool arguments at: 3].        aButton state: #off! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 15:23'!findButton: aSelector	"Find this button in me"	(self submorphNamed: aSelector) ifNotNil: [^ self submorphNamed: aSelector].	submorphs do: [:button |		button actionSelector == aSelector ifTrue: [^ button].		(button respondsTo: #arguments) 			ifTrue: [(button arguments atPin: 2) == aSelector ifTrue: [^ button]]			ifFalse: [(button isKindOf: AlignmentMorph) ifTrue: [				button submorphsDo: [:sub |					(sub respondsTo: #arguments) 						ifTrue: [(sub arguments at: 2) == aSelector ifTrue: [^ sub]]]]].			].	^ nil! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 14:01'!showColor	"Display the current color in all brushes, both on and off."	| offIndex onIndex center |	currentColor ifNil: [^ self].	"colorPatch color: currentColor.	May delete later"	(brushes == nil or: [brushes first owner ~~ self]) ifTrue: [		brushes _ OrderedCollection new.		#(brush1: brush2: brush3: brush4: brush5: brush6:) do: [:sel |			brushes addLast: (self findButton: sel)]].	center _ (brushes at: 6) offImage extent // 2.	offIndex _ (brushes at: 6) offImage pixelValueAt: center.	onIndex _ (brushes at: 6) onImage pixelValueAt: center.	brushes do: [:bb |		bb offImage colors at: offIndex+1 put: currentColor.		bb offImage clearColormapCache.		bb onImage colors at: onIndex+1 put: currentColor.		bb onImage clearColormapCache.		bb invalidRect: bb bounds].	self invalidRect: (brushes first topLeft rect: brushes last bottomRight).! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 8/9/2000 14:50'!toggleShapes	| tab sh otherTab |	"The sub panel that has the shape tools on it.  Rect, line..."	(sh _ self submorphNamed: 'stamps') visible ifTrue: [sh hide].	otherTab _ self submorphNamed: 'stampTab'.	tab _ self submorphNamed: 'shapeTab'.	(sh _ self submorphNamed: 'shapes') visible		ifTrue: [sh hide.  tab top: self bottom-1.				otherTab top: self bottom-1]		ifFalse: [sh show.  tab top: sh bottom - tab height + 10.				otherTab top: self bottom-1].	self layoutChanged.! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 8/9/2000 14:49'!toggleStamps	| tab sh otherTab st |	"The sub panel that has the stamps in it.  For saving and moving parts of an image."	(sh _ self submorphNamed: 'shapes') visible ifTrue: [sh hide].	otherTab _ self submorphNamed: 'shapeTab'.	tab _ self submorphNamed: 'stampTab'.	(st _ self submorphNamed: 'stamps') visible		ifTrue: [st hide.  tab top: self bottom-1.				otherTab top: self bottom-1]		ifFalse: [st show.  tab top: st bottom-0.				otherTab top: st bottom-0].	self layoutChanged.! !!PasteUpMorph methodsFor: 'world state' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  Must do this recursively because of transforms.  "	"PasteUpMorphs clip their display, so if not in bounds, exit"	(bounds containsPoint: aPoint) ifFalse: [^mList].	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	submorphs size > 0 ifTrue:		[submorphs do: [:m | m unlockedMorphsAt: aPoint addTo: mList]].	mList addLast: self.	^ mList! !!SketchEditorMorph methodsFor: 'access' stamp: 'tk 7/25/2000 16:10'!forwardDirection	"The direction object will go when issued a sent forward:.  Up iszero.  Clockwise like a compass.  From the arrow control.""	| bb result |	bb _ (self valueOfProperty: #fwdButton).	result _ (self center - bb vertices first) degrees - 90.0.	result abs < 1.0e-10 ifTrue: [result _ 0]."	"Workaround because the above can yield spurious microscopic but nonzero values""	^ result	"	^ hostView setupAngle! !!SketchEditorMorph methodsFor: 'start & finish' stamp: 'tk 7/25/2000 15:52'!addRotationScaleHandles	"Rotation and scaling handles"	| |	rotationButton _ SketchMorph withForm: (palette rotationTabForm).	rotationButton position: bounds topCenter - (6@0).	rotationButton on: #mouseDown send: #rotateScalePrep to: self.	rotationButton on: #mouseStillDown send: #rotateBy: to: self.	rotationButton on: #mouseUp send: #rotateDone: to: self.	rotationButton on: #mouseEnter send: #mouseLeave: to: self.	"Put cursor back"	rotationButton on: #mouseLeave send: #mouseEnter: to: self.	self addMorph: rotationButton.	rotationButton setBalloonText: 'Drag me sideways to\rotate yourpicture.' withCRs.	scaleButton _ SketchMorph withForm: (palette scaleTabForm).	scaleButton position: bounds rightCenter - ((scaleButton width)@6).	scaleButton on: #mouseDown send: #rotateScalePrep to: self.	scaleButton on: #mouseStillDown send: #scaleBy: to: self.	scaleButton on: #mouseEnter send: #mouseLeave: to: self.	"Put cursor back"	scaleButton on: #mouseLeave send: #mouseEnter: to: self.	self addMorph: scaleButton.	scaleButton setBalloonText: 'Drag me up and down to change\the sizeof your picture.' withCRs."REMOVED:	fwdButton _ PolygonMorph new.	pt _ bounds topCenter.	fwdButton borderWidth: 2; makeOpen; makeBackArrow; borderColor:(Color r: 0 g: 0.8 b: 0).	fwdButton removeHandles; setVertices: (Array with: pt+(0@7) with:pt+(0@22)).	fwdButton on: #mouseStillDown send: #forward:direction: to: self.	fwdButton on: #mouseEnter send: #mouseLeave: to: self.		fwdButton on: #mouseLeave send: #mouseEnter: to: self.	self setProperty: #fwdButton toValue: fwdButton.	self addMorph: fwdButton.	fwdButton setBalloonText: 'Drag me around to point\in the directionI go forward.' withCRs.	toggle _ EllipseMorph		newBounds: (Rectangle center: fwdButton vertices last +(-4@4) extent: 8@8)		color: Color gray.	toggle on: #mouseUp send: #toggleDirType:in: to: self.	toggle on: #mouseEnter send: #mouseLeave: to: self.	toggle on: #mouseLeave send: #mouseEnter: to: self.	self setProperty: #fwdToggle toValue: toggle.	fwdButton addMorph: toggle.	toggle setBalloonText: 'When your object turns,\how should itspicture change?\It can rotate, face left or right,\face up or down, or notchange.' withCRs.	"	self setProperty: #rotationStyle toValue: hostView rotationStyle."	self forward: hostView setupAngle direction: fwdButton.	"	"Set to its current value"! !!TransformMorph methodsFor: 'submorphs-accessing' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point.  Map through my transform.  Must do this recursively because of transforms.  "	| p |	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	(self containsPoint: aPoint) ifFalse:		["TransformMorph clips to bounds"		^ mList].	p _ transform globalPointToLocal: aPoint.	submorphs do: [:m | m unlockedMorphsAt: p addTo: mList].	mList addLast: self.	^ mList! !PaintBoxMorph removeSelector: #loadRotScalePics!!ObjectScanner new initialize!!self smartRefStream!      class structure	   
Dictionary       0	   AssociationScrollingToolHolder       pickupButtonsstampButtonsstampsthumbnailPicsstart >Symbol        >MorphExtension       lockedvisiblestickyballoonTextballoonTextSelectorexternalNameisPartsDonor
actorStateplayereventHandlerotherProperties >Morph       boundsowner	submorphs
fullBoundscolor	extension >AlignmentMorph       
  y
  
  ˆ
  “
  Ÿ
  ¦borderWidthborderColororientation	centering	hResizing	vResizinginsetminCellSizelayoutNeededpriorFullBounds >PaintBoxMorph       
  y
  
  ˆ
  “
  Ÿ
  ¦imageactiontoolcurrentCursor	thumbnailcurrentColorcurrentBrushcolorMemory
colorPatchstampHolderrotationTabFormscaleTabFormcolorMemoryThinbrushes
focusMorph >	Rectangle       origincorner >Point       xy >Cursor       bitswidthheightdepthoffset >RectangleMorph   	    
  y
  
  ˆ
  “
  Ÿ
  ¦
  í
  ú >Color       rgbcachedDepthcachedBitPattern >OrderedCollection       array
firstIndex	lastIndex >Array        >Form       
  ³
  ¹
  À
  È
  Ï >ThreePhaseButtonMorph       
  y
  
  ˆ
  “
  Ÿ
  ¦
  £offImagepressedImagestatetargetactionSelector	argumentsactWhen >
ImageMorph       
  y
  
  ˆ
  “
  Ÿ
  ¦
  £ >String        >PasteUpMorph       
  y
  
  ˆ
  “
  Ÿ
  ¦
  í
  ú	presentermodelcursorpaddingbackgroundMorphturtleTrailsForm	turtlePenlastTurtlePositions
isPartsBinautoLineLayoutindicateCursorresizeToFitfileNameisStackLikedataInstancescurrentDataInstanceuserFrameRectanglewantsMouseOverHalos
worldState >TranslucentColor       
  3
  8
  Ealpha >SequenceableCollection        >DisplayObject        >BorderedMorph   	    
  y
  
  ˆ
  “
  Ÿ
  ¦
  í
  ú >
Collection        >DisplayMedium        >Bitmap        >ArrayedCollection        >ProtoObject        >EventHandler       mouseDownRecipientmouseDownSelectormouseStillDownRecipientmouseStillDownSelectormouseUpRecipientmouseUpSelectormouseEnterRecipientmouseEnterSelectormouseLeaveRecipientmouseLeaveSelectormouseEnterDraggingRecipientmouseEnterDraggingSelectormouseLeaveDraggingRecipientmouseLeaveDraggingSelectorkeyStrokeRecipientkeyStrokeSelectorvalueParameterstartDragRecipientstartDragSelectordoubleClickSelectordoubleClickRecipient >PaintBoxColorPicker   
    
  y
  
  ˆ
  “
  Ÿ
  ¦
  £currentColorlocOfCurrent >	ByteArray        >Object        >	ColorForm   	    
  ³
  ¹
  À
  È
  ÏcolorscachedDepthcachedColormapsuperclasses         0 >
   K
  
Å >
   ©
  ² >
   À
  
Å >
  h
  
Å >
  Û
  ” >
  µ
  Û >
  ¡
  ³ >
  l
  u >
  ]
  
Å >
  †
  
Å >
  "
  
Å >
  [
  Q >
  ž
  ' >
  ³
  ó >
  á
  u >
  u
  h >
  ²
  ' >
  È
  ” >
  
  " >
  Q
  Ù >
  w
  
Å >
  ”
  h >
  Ù
  
Å >
  ó
  w >
  
  ' >
  '
  Q >
  Hnil >
  c
  
Å >
  
G
  u >
  
¬
  ' >
  
Å
  H >
  
Û
  ³l%þ+‘
?ÿÿÿ   u%ø+‘F
     á%àø'a
  0    "  ÿ Àstamp:	
Û  ¢Š‰Š‰‰‰‰‰‰‰‰‰‰ŠŠ‰Š	‰‰	â[ŠŠŠ‰Š‰‰‰‰‰‰‰‰’Š‰Š‰‰‰‰‰‰‰‰ŒŒ‰Š‰‰‰‰‰‰‰’‰‰Œ’ŒŒŒŠ‰‰‰‰‰‰’‰Œ?E?;:ŒŒŒ’‰‰Š‰‰‰‰‰?EEE@:C„ŒŒŒ‰‰‰‰‰’‰‰‰Œ;EEE=:+6„ŒŒ‰‰‰‰‰‰ŒC:;=:*1[†ŒŒ‰‰‰’‰’‰„611+11dd‘Œ‰‰‰‰‰’Œo6d1d[d„Œ‘‰‰‘’d’ŒŒŒL:6d‘Œ‘‰‰Œ=@X?poŒŒ?E;d‘„Œd‘‘‘‰’5;=@@@@?@ Œ:;+„ŒŒŒ‘‘‘‘‰‰d%00=@;1611[„Œ‘Œ‘‘‘‘’‰d1$,6d16„Œ‘‘‘‘‘‘‘‰d136d161„‘‘‘†‘‘‰Œdì-6d16†‘‘‘‘†Œ’’†7!66166„‘‰ŒŒŒ‘d6!6dd6+‘ŒŒŒŒŒŒ‘‘†161d6†‰ŒŒ’ŒŒŒŒ‘‘‘††1%671+ŒŒŒŒŒŒ‘‘Œ‘‘Œ‘‘‘„++†‹Œ’ŒŒŒŒŒ‘‘‘‘‘†‘‘ŒŒŒŒŒŒ‘Œ‘‘‘‘‘‘ŒŒŒŒŒŒ‘‘‘‘‘‘‘‘Œ’ŒŒŒŒŒ‘‘‘‘†‘‘            "?ÿÿÿ"CÄ"Ö5"7­ëz"Ö5"„!"„!"7ªV•"3¦5"?÷5Í"?öµ­"'ä")Q€`"/a @"Ð  "+`  "1   "#€ "'€ "7¥¥"-f5H";æµ("1¢ "/fµH"/f5("' "ƒ¤"%5H"9æµ"+fµH"9êÚQ"5©Æ"1¨ÅÍ"+g5i"Ô ä"' ¤"?÷µ"+gµ"“¤"9çµ("! "1©Eí"/hÅÍ"-hE­")W5i"'µH"%5("Ô Ä"?ùEi")Wµ"5©Åí"-gµi"Õ!"3©Åí"'5i"%µH"#5("ƒ¤"5©E­"?úU­"/d  "?úÙÍ"Ô Ä";ëÚQ"?ûÚ")T  "7¦´ "1¨ÅH"
B`"?ýj•";èD "?øÄ "7¬j•"'µ("/g4 "7¨D "-jV1"?üéí";éD "Õ¥("?ýj1"-iEi"+hÅH"5ªÙ"1©ÅH"Õ  "9ëÙ"-iE("9ìiÍ"3ªÙi"„ Ä"×5H";îzq"7­j"?ÿzq"?ÿzQ"9íéí"7­ëZ"?ÿÿ¾"1¬j¶"×µ­"CÄ"5­jÖ"…!"?ÿÿ:"7­ê¶"„ ä"5­j•"3¬êq"7­ê•"B`")ZUÍ"-kYí"5­j1"'Å"?ÿþq"1¬iÍ"?ÿþQ"+jÙ"  "1¬i"9îú¶";ï{"-kÚ1"7®ú¶"-nz"+nyí"%é"%éÍ"ÛY("ŠU"9îûž"#F1"'Vq"!Æ"7¯ÿz"
C¤"×µ"—5i"%ÚQ"ÙEÍ"#Z1")]j•"ØÅ­"!Ú"ÚUí"ÙE­"ÛY"IÄÄ"!Z1"'j¶"ÛZ"ÚÚ1"5®{z"1­k:"1­ëZ"#jö"šÚq"Üêö"-kÚö"+kZÖ"-lë:"ÚV•"ÚV•"%ëz"ÚÚ¶"Ýkž"
IF•"‰Æ¶"Œë¾"[z"Ýëÿ"kÿ"6Q"ÆÖ"kÿ" ¶•" 
Wz"Œëÿ"
Lkÿ" Æö" [Þ"ÙÆö"†¶"ÙÆö")\ëÞ"ÙÇ"ÙFö"'Ûž"%[z"!W:"ÙÇ"ØÆ•"ØFö"
C"%W:"…¦"×6¶"D""…¦"…¦1" Í" ")ZVÖ"„!("„!"Ci"Ö·:"„#¾"Cÿ"•#ž"×7"Õ§"
BH"A("Õ#:"Ö6"Ô#Z"Óz"×61"%·"Ò“¾"€ÿ")YG"!#z"ƒÞ"'ÆÖ"!6q"Óž"-jÚö"
BÄ"%#:"%¶•"'¶ö"3¬k:"-jÚÖ"%6Q"-hÆÖ"3©Ç"5ªÛ:"+hF•"1¨ÆÖ"9ëÛZ"5¨Æ•"1ªÚÖ"?üëz"?ükZ"7§¶1"5©Æ•"9ç¶1"?ûÛ"?÷¶1"?ûZö"?ùÆ•"9æ5­"    
  |off
  pickup:action:cursor:   
  Estamp:¡€
ÿÿ  :À            †ÿÿÿþÿÿÿþbuttonUpá'a+(a:
  0
  U
  [ À
prevStamp:	
Û   µ<	/‹‹‹‹‹‹		‘	‘†‘	‘‘‘‘‘‘	‘‘‘	‘‘‘†‘‘†‘	‘
‘‘‘‘‘‘‘‘‘‘‘Œ‘‘‘‘‘‘‘†‘†‘‘‘†‘            "?ÿÿÿ"CÄ"Ö5"7­ëz"Ö5"„!"„!"7ªV•"3¦5"?÷5Í"?öµ­"'ä")Q€`"/a @"Ð  "+`  "1   "#€ "'€ "7¥¥"-f5H";æµ("1¢ "/fµH"/f5("' "ƒ¤"%5H"9æµ"+fµH"9êÚQ"5©Æ"1¨ÅÍ"+g5i"Ô ä"' ¤"?÷µ"+gµ"“¤"9çµ("! "1©Eí"/hÅÍ"-hE­")W5i"'µH"%5("Ô Ä"?ùEi")Wµ"5©Åí"-gµi"Õ!"3©Åí"'5i"%µH"#5("ƒ¤"5©E­"?úU­"/d  "?úÙÍ"Ô Ä";ëÚQ"?ûÚ")T  "7¦´ "1¨ÅH"
B`"?ýj•";èD "?øÄ "7¬j•"'µ("/g4 "7¨D "-jV1"?üéí";éD "Õ¥("?ýj1"-iEi"+hÅH"5ªÙ"1©ÅH"Õ  "9ëÙ"-iE("9ìiÍ"3ªÙi"„ Ä"×5H";îzq"7­j"?ÿzq"?ÿzQ"9íéí"7­ëZ"?ÿÿ¾"1¬j¶"×µ­"CÄ"5­jÖ"…!"?ÿÿ:"7­ê¶"„ ä"5­j•"3¬êq"7­ê•"B`")ZUÍ"-kYí"5­j1"'Å"?ÿþq"1¬iÍ"?ÿþQ"+jÙ"  "1¬i"9îú¶";ï{"-kÚ1"7®ú¶"-nz"+nyí"%é"%éÍ"ÛY("ŠU"9îûž"#F1"'Vq"!Æ"7¯ÿz"
C¤"×µ"—5i"%ÚQ"ÙEÍ"#Z1")]j•"ØÅ­"!Ú"ÚUí"ÙE­"ÛY"IÄÄ"!Z1"'j¶"ÛZ"ÚÚ1"5®{z"1­k:"1­ëZ"#jö"šÚq"Üêö"-kÚö"+kZÖ"-lë:"ÚV•"ÚV•"%ëz"ÚÚ¶"Ýkž"
IF•"‰Æ¶"Œë¾"[z"Ýëÿ"kÿ"6Q"ÆÖ"kÿ" ¶•" 
Wz"Œëÿ"
Lkÿ" Æö" [Þ"ÙÆö"†¶"ÙÆö")\ëÞ"ÙÇ"ÙFö"'Ûž"%[z"!W:"ÙÇ"ØÆ•"ØFö"
C"%W:"…¦"×6¶"D""…¦"…¦1" Í" ")ZVÖ"„!("„!"Ci"Ö·:"„#¾"Cÿ"•#ž"×7"Õ§"
BH"A("Õ#:"Ö6"Ô#Z"Óz"×61"%·"Ò“¾"€ÿ")YG"!#z"ƒÞ"'ÆÖ"!6q"Óž"-jÚö"
BÄ"%#:"%¶•"'¶ö"3¬k:"-jÚÖ"%6Q"-hÆÖ"3©Ç"5ªÛ:"+hF•"1¨ÆÖ"9ëÛZ"5¨Æ•"1ªÚÖ"?üëz"?ükZ"7§¶1"5©Æ•"9ç¶1"?ûÛ"?÷¶1"?ûZö"?ùÆ•"9æ5­"    ³€áàx
áÛæåÅÄÄ¤¤¤¤ÄÄÄ!h!h!h!H!H'')')ª)Ê)ª)ª)ª%‰!i!H!²*]ÕHH!h!i!i2221ì-ë)Ë!Ò"=*2Ÿ!Õ!i%‰%ª)ª)«:M6-6-2%ñûÿ	ß?.Õ%i%Š)Ë-ì-ì:n:N./¹¿žž¿&_Õ!i%ª-Ì1ì26P”667YZÿ"_´!I%Š-Ì22V ó Ð Ð ñ6Zß?“(!j)Ë1ì26Oq Ð Î ® Ï ñ|	ßs'!i)«-ì2:N6-)Í/ Ï ® ® Ð6|	2!i)«-ì2:N6-1ì)Ëlï Ï Ï ñ6	!i)«-ì2:N6.2-Ë)ª!i* ð ò	/(!i)Ë-ì2:n:N6-2-Ë)ª!iH-5P!i%Š)Ì22>o:n:N6-2-ë)ª%Š!i!ik%Š)Ë-ì22>o:n:n:N6-61ì-Ë)Ë)«)«)Ë-ì26-6.         
  !
  scrollStamps:action:   
  
prevStamp:
  ƒá(+)q:
  0
  U
  [ À
nextStamp:	
Û   Ä<‹‹‹‹‹‹‹ 
‹‹ ‹‹	 ‹  	  
•  	‘ ‘ 	‘‘‘ 
‘‘'‘‘‘ ‘‘‘‘‘†‘‘‘†‘ ‘†‘‘‘‘‘‘‘‘‘‘‘             
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š³  Þx
ÄÄ#Ä¤¤ÄÅåå&  ''
á³'G!H!h!h!h  !i!iÕ*]!ÒHHH!H!h%‰%Š)ª)ª)ª  )ª)«&2Ÿ*"=±!i!i%Š)ª-Ë-ì1ì2  -ì-ì&.?	ßÿû%Š)«-ì266-  1ì2&&_¿žž¿	˜!Œ-Ë1ì6-6.  22""_ÿZY766R%Í26-  1ì2!ö?ßZ6 ñ Ð Ð óV)Í2  -ì2ö	ß| ñ Ï ® Î Ð	!j)Ë2  -ì2Ö|6 Ð ® ® ÏÍè'!i)«1ì  -ì2³6 ñ Ï ÏÎÈÅH%‰)Ë1ì  -ì2’ ò ñ
ææ'!i)ª-Ë2  22³Uo!i(''H!i)ª-Ë26-  22-ï-ì)Ë%Š!i!i!i%Š)ª-ë26-:N  6-6.6-2-ì)Ë)«)«)Ë-ë1ì66-:N:N           
  !
  
  )š   
  )Ë
nextStamp:
  ƒá'pø(ñ
  0
  U
  [ Àstamp:	
Û  (¢’Œ’Œ’ŒŒŒŒŒ’ŒŒŒŒŒŒŒ’ŒŒŒŒŒŒŒ’ŒŒ’Œ’ŒŒ9Œ‘ŒŒŒŒ‘ŒŒ	ŒKŒŒŒ‘Œ‘‘ŒŒ‘{EE=:p‘‘ŒŒŒŒŒ‘Œ‘Œ‘‘Œ‘‘‘LPEEE;o‘‘‘ŒŒ‘ŒŒ‘‘Œ‘‘‘‘‘2@EE?V:6†‘‘Œ‘Œ‘	‘‘p::==:+6[†‘‘‘„7111+1dd‘‘Œ‘‘‘	‘›‘„61d6d‘‘‘‘‘„o‘„‘†‘†‘‘@1[d‘‘‘‘‘l@@@{{p†{E@1„„‘Œ‘‘‘‘‘;=@@@@@?;„*;:1‘†‘„o'0==?+611d[‘‘‘‘‘1!$'11d11…‘‘‘•„!761d6o	{••„1616d1„‘„†!-76d6o„„1!!1d66o•‹„1!ddd1‹„)1,67	G‹‹‹o+1„‹‹‹‹‹‹‹‹‹‹‹‹‹‹
‹‹‹#‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹            
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  
  
  
  &
  1
  <
  G
  R
  ]
  h
  s
  ~
  ‰
  ”
  Ÿ
  ª
  µ
  À
  Ë
  Ö
  á
  ì
  ÷
  
  
  
  #
  .
  9
  D
  O
  Z
  e
  p
  {
  †
  ‘
  œ
  §
  ²
  ½
  È
  Ó
  Þ
  é
  ô
  ÿ
  

  
   
  +
  6
  A
  L
  W
  b
  m
  x
  ƒ
  Ž
  ™
  ¤
  ¯
  º
  Å
  Ð
  Û
  æ
  ñ
  ü
  
  
  
  (
  3
  >
  I
  T
  _
  j
  u
  €
  ‹
  –
  ¡
  ¬
  ·
  Â
  Í
  Ø
  ã
  î
  ù
  
  
  
  %
  0
  ;
  F
  Q
  \
  g
  r
  }
  ˆ
  “
  ž
  ©
  ´
  ¿
  Ê
  Õ
  à
  ë
  ö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  	
  
  
  *
  5
  @
  K
  V
  a
  l
  w
  ‚
  
  ˜
  £
  ®
  ¹
  Ä
  Ï
  Ú
  å
  ð
  û
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  27
  !
  
  +   
  2
  L
  T
  ƒá)ø*±
  0
  U
  [ Àstamp:	
Û  Ú½Œ’	‰‰	‰+‰Š  ŒŒŒ’Œ‰‰‰‰‰‰‰Š‰‰  ŒŒŒŒ	G‰‰‰‰‰‰‰‰‰  ŒŒŒŒŒŒŒ’Œ‰‰‰‰‰‰‰Š  ŒŒŒŒŒŒŒŒŒ’‰‰‰‰‰‰  	ŒŒ‘?EP0:Œ‰‰‰‰  ŒâCŒ?EEE@S3„‰’‰‰‰  ‘‘Œ‘ŒŒŒŒŒŒ’ŒŒ=EEE=:%6Œ‰’‰  ‘Œ‘‘Œ‘ŒŒŒŒŒ’„C:;=:*1[†‰‰  ‘‘‘Œ‘Œ‘ŒŒŒŒŒŒo76++11dd‰‰  ‘‘‘‘‘‘Œ‘Œ‘ŒŒŒŒ*6d6d[†  ‘‘‘L‘‘‘Œ‘ŒŒ‘Œl:6dŒ‰  ‘‘‘=@@?LoŒ‘\?:d†Œd‰  †‘†;=@@@@@@+„:=+ŒŒ’Œ  ‘+00=@=6616„’ŒŒ  ‘‘„,$!ddd6oŒŒŒŒŒŒ  ‘‘‘ì1616d1‘Œ’Œ’  ‘Œ1,!6666†ŒŒŒŒŒ  ‘‘˜†66d116„ŒŒ  ‘‘†‘†16dd6+ŒŒŒ  ‘‘‘††161d7o  ‘‘‘‘‘†d%%676†Œ  ‘‘‘‘‘‘‘Œ†1+doŒ  ‘‘†‘‘‘‘‘ŒŒ‘ŒŒŒ’  ‘‘‘‘‘Œ‘Œ‘ŒŒŒŒŒ  ‘‘‘‘‘‘‘ŒŒŒŒŒ  ‘†‘‘‘‘‘‘ŒŒŒŒŒ              
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  
  
  
  &
  1
  <
  G
  R
  ]
  h
  s
  ~
  ‰
  ”
  Ÿ
  ª
  µ
  À
  Ë
  Ö
  á
  ì
  ÷
  
  
  
  #
  .
  9
  D
  O
  Z
  e
  p
  {
  †
  ‘
  œ
  §
  ²
  ½
  È
  Ó
  Þ
  é
  ô
  ÿ
  

  
   
  +
  6
  A
  L
  W
  b
  m
  x
  ƒ
  Ž
  ™
  ¤
  ¯
  º
  Å
  Ð
  Û
  æ
  ñ
  ü
  
  
  
  (
  3
  >
  I
  T
  _
  j
  u
  €
  ‹
  –
  ¡
  ¬
  ·
  Â
  Í
  Ø
  ã
  î
  ù
  
  
  
  %
  0
  ;
  F
  Q
  \
  g
  r
  }
  ˆ
  “
  ž
  ©
  ´
  ¿
  Ê
  Õ
  à
  ë
  ö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  	
  
  
  *
  5
  @
  K
  V
  a
  l
  w
  ‚
  
  ˜
  £
  ®
  ¹
  Ä
  Ï
  Ú
  å
  ð
  û
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  9Þ
  !
  
  +   
  9­
  L
  T
  ƒµ'q)+
  0   á'‘(á)
  B
  U
  [³   xáá          
  !
  
  +   
  B
  L
  ƒ                   
horizontaltopLeft
shrinkWrap
  B¶        (á;*qSµ%Ñ'a+
  0   á%ñ'A)
  BÙ
  U
  [³   xáá          
  !
  
  +   
  Bî
  L
  ƒ
  B{   
  B‘
  B¡
  B­
  B¶
  B¶        'A;(ÑSµ)*±+
  0   á)1*‘)
  C„
  U
  [³   xáá          
  !
  
  +   
  C™
  L
  ƒ
  B{   
  B{
  B¡
  B­
  B¶
  B¶        *;,!S"  ÿ   €   Àstamps³  ,¾ïØ^ÐoUsuw–··svgV°RoV """"!!!!*"""!"!!!"""""C ^Òk4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ê-Ë-Ë-Ë-Ë-ë-ë-ë
1ë1ë-ë-Ë-Ë)Ê)ª)ª'-ª-Ê-Ë-ë1ë1ì2666-6-6-6-:N:N:N6M6-6-6-6221ì1ë-ë-Ë-ËS-Ê)ª)ª-Ê-Ë-Ë-Ë-ë1ë1ì1ì1ì1ì1ë-ë-ë k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoR 
-Ë-Ë-ë-ë1ë1ì1ì1ì1ì21ì1ì1ì1ë
-ë-ë1ë1ì1ì22666-6-6-6-:N:N:N:M6M.6-6-6221ì1ë-ë-ë-ë
1ì1ëK1ë-ë k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë-Ë-Ë-ë-ë-ë1ì1ì1ì2
21ì1ì1ë
-ë-ë1ë1ì26
6-6-6M:M:M:N:N:N:M6-6-6,
22666-6-6-:N
:N:N:N:M6-6-221ë-ë-Ë-Ë-ë1ë
1ë1ëk k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë-Ë-ë-ë-ë1ì1ì1ì1ì221ì1ì:-rk~Ë~ŠaÆEf1ª)Ê-Ë-ë266-6-:N:N:N6-^Ž~ª~«r(U…=©-Ë-ë1ì26-6-:N:N:Nk6-6-6-6>Nrk~Ë~jaÆEf-Š)ª)ª-Ê-Ë-ë-ë1ë1ë1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoR -Ë-ë-ë-ë-ë1ì1ì1ì
22#1ìjk~Ë/~îv)U…4Á1«)ª-Ê-ë26-6-:N:N:N#:N:MNN~h~‹eçE#5h-ª-Ë1ë26-:M:N:N[:N6-6-6nk~Ì/~ír)Q…0Á1Š%‰)ª)ª-Ë-ë1ë1ë1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
-ë-ë1ë1ì1ì2
22#6qæ~‹~î~¬j	Ie0Ãa-Š%‰-Ê1ì26-:N
:N:N
:n:n#:N:M^
zI~Í~ÍvKY§=`%')‰-Ê1ë66-:N:N[:N6-6->-r~‹~î~ŒfIe,Âa)Š%i)©)ª-Ë-ë1ì1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
-ë-ë3-ë1ì1ì22221ì1ì=MD]§]èQ†9$¢@æ!h)ª-Ë26-:M
:N:N+:n:n:n:N:N6-A©I$U…aÈYÇEE,Ã %H)‰-Ë26-:N:N[:N6-6-:AMD]§]èQ†9$¢@!!H%i)ª-Ë-ë1ì1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 
-ë-ë31ë1ë1ì1ì1ì221ì-Ë1‰`$¢,Ã,Ã(Â ‚@Å'%‰-Ê1ì6-:N:N:N#:N6-:-` (Â,Ã,Ã$¢a '%i)ª1ì6-:N:N[:-6-62-ia$¢,Ã,Ã(¢ ‚@Å'!h)‰-Ê-ë1ì1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 
-ë1ë31ë1ë-ë1ë1ë1ì1ë-ë-Ë-ª1aaaaa !''%h)ª1ì6-:M:N:NŸ:N:M6-6-1ë5Ì €aaa  !H)ª-ë6:M:N:N:N:N:M:-6-6-6-21ë-Ë-aaaaa !H%‰)ª-Ë1ì1ì k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgVRoV -ë-ë-ëB=ë5ë1Ë-Ë
-Ë-Ë+)ª%‰^
Me@    '%h)ª1ì6-:->NJNBM>-
6-6-ä62-Ë5ªj	$¢    A!H)ª1ë6-:M:NJnFM>M:-66221ì-Ë)ª^*IE@    !H%‰)ª-ë1ì1ì k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV -ë-ë6zv'n(j)b)V
Eê5Ê)©)‰n‹~ŠQ…¤æ¤‚ƒå'%‰-Ê1ì6-:Mbmzv(nIj*^KNB5ë-ËR,v‹v($ææƒ‚å'!h)Ê1ì6-:N:Nzv(r)nJb*V+F9ë-Ê-ªn‹~ŠMeÄæ¤bƒå'!h)‰-Ê-ë1ì2 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 1ë-ëUêm¥vz(zHz(z'zH~H9$)HMeYÈ=%'¤¤Äå'!H)ª-ë66->-iÇqæv'zHzHz'z(~Hiæ%=FU§Q†$¢ÅÄÄå!H)‰-Ë66-:N:NmÅvz(zHz'z'zH~(5-HM…YÈ8ã%(¤¤¤å!H%i)ª-Ë1ì22 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 1ë1ë5ª8åH@T¡aCi¥mçv(eçaa$¢ @!ÄÄå!H%‰-Ë1ì6-:M:-=‰@aL`YidmÆrzI,Ã`$¢a@æÅå!H%‰-Ë1ì6-:N:N:N8åH@TÁaCi¥mçz(aÇaa$¢@!¤ÄåG%i)ª-Ë1ë1ì22 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 1ë1ë-Ë-ª-‰$!4 D`L€\à,a‚ å¤ÄÅ!G%i)ª1ë66-:N:N6-6-, <@H`T LÁa‚‚Äå'%i)ª1ì6-:N:n:n:N6-1«$!4 D`L€\á,a‚$æ¤ÄÅæ'!h)‰)Ê-ë1ì266 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 1ë1ë-Ë-Ê)‰%i$æ$B( , 4`0` a‚ £æå!H%‰-Ë26-:N:N:N6-6-ë-Š$ƒ$ , 0@4`$`
âï$ææ!H)‰-Ë26-:N:n>n:N6-2-Ë)$B( ,@4`0` a‚ £æå!G%i)ª-Ë1ë2266 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 1ì1ì1ë-Ë)ª%‰!h!'(æ( 0 0@0`,`aaa$å!'!H)‰-Ë26-:N:N:N:N6-1ì-Ê)‰%((B, 0@0@0`$`aa ¢%'!H)‰-Ë26-:N>o>o:n:M6-ë)ª%H(æ( 0 0@0`,`aaa$å'!G%‰)ª-Ë1ì2266 k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgVRoV 21ì1ì-ë-Ê)ª%‰!h'!' b, , 0@4@$@aa)h%‰)ª1ì66M:N:n:N:M61ì-Ê%‰!H$æ(!, ,@4`,@a‚`$å)Š)Ê1ì6-:N>n>o>n:N6-2-Ë)©!h!($b, , 0@4@$@aa)H%h)‰-Ê-ë1ì226 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 2221ì1ë-Ë)ª)‰%h!G'æ(c, , 0@0@ a‚` Ä-‰-Ë26-:N:n:n:N:M6-1ì-Ë)Š%h!'$Å( , 0@0@(@aaa)H-Ë26-:N>n>o>n:N6-2-Ë)ª%h!G æ(c, , 4@0@ a‚`$Ä)i)©-Ë-ë1ì26 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 
22âo21ì-Ë-Ë)ª%‰!h' æ,ƒ, , 4@0@aa)-Ë26-:N>o>o>n:N6-21ë)Ê%‰!H!'$Å0A, 0@4@$@‚a ¢-ª26-:N>o>o>o:N:-21ë)ª%‰!H'!,ƒ, , 4@,@aa))ª-Ë1ë22 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 22662221ì-ë-Ê)ª%‰!hG$æ(£0‚4 $ aa$Ä-ª1ì6-:N>o>>>>o:N6-2-Ë)ª%‰!H'%,å,ƒ4a, @@-i1ì6-:N>o>>>:n:N6-2-Ë)ª%i!H'$æ,£0‚4 $ aa$ä%‰)ª-Ë1ì2 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 2666-6-6-6-621ì-ë-Ë)ª%‰%h!H''%,å4ƒ$å1«)ª1ë6-:N>o>BBB>>o:N6-1ì-Ë)ª%‰%h!H!'!'-4Å(ƒ)&)ª-ë6:N>o>BB>>o:n6-61ì-Ë)ª%i!H'%,å4ƒ å1«%i)ª-Ë1ë2 g3k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 666-6-+221ë-Ë)ª)ª%‰%h!H!H!G!H%h%‰)ª1ë6-:N>B
B°B°“B>:n:N6-21ë-Ë)ª)‰%‰%h%h%i)‰-ª1ë6-:N>oBB°B°B°>>>o:N6-21ë-Ë)ª%‰%h!H!'''!G!h%‰)ª-Ë1ì2 g3k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 666-6-W6-21ì-ë-Ë)ª)ª)‰%‰%‰)‰)ª-Ë26-:n>B°B°BÐFÑFÑB°B°B>>o:n:N6-21ì-Ë-Ê)ª)ª-Ë-ë66-:n>
B°B°cB°B>>o:n:N6-21ì-Ë)ª)Š%‰%i%h%i%‰)ª-Ê-ë26 g3k4oTsuw–··svgV°w¼^ÐoUsuw–··svgVRoV 66-
6-6-c6-6M6M:N6N:M6-6-6-621ì1ë-ë-Ë-Ë-Ë1ë26-:N>oBB°B°FÑFÑFÑFÑBÐB°B°B°B>>n:N:-6-622266-:N>o>B°B°_B>>:n:N:M6-21ì-Ë-Ë)ª)ª)ª)ª-Ê-Ë1ë26 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 66-6-6-:N:N:M:M
6-6-K6622266-:M:n>o>B°B°B°BÐBÐB°B°BÐB°B°B°B°B>>o>n:N:M6-:M:M:N>n>B
B°B°oB°BB>>>o:n:N:N6-6-21ì1ë-Ë-Ë-Ë-ë1ë1ì26- k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 666222666-6-6-66-6-6-:N:N>o>>BB°B°B°B°B>>>o:n
:N:N:n>o>>s>o>o>n:n:N:N:N6-6-621ì1ì1ë-ë1ë1ì226 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 2221ì1ì1ë1ë1ë1ì1ì1ì222266-6-6M:N:n:n:n>o:n:n>o>o:n:n:N:N
:n:n:n:N:N:N6M6-
6-6-S62221ì-ë-ë-Ë-Ë-Ë-ë-ë1ì2 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 22226-2;-Ë-ë k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì.VÞVÞVÞ)ª.VÞVÞVÞ-Ë.VÞVÞ?VÞ%‰)ª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì.VÞVÞVÞ!h.VÞVÞVÞ%h.VÞVÞ?VÞ!h%‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -ë.VÞVÞVÞ.VÞVÞVÞ.VÞVÞ?VÞ'!H k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞÄ.VÞVÞVÞÅ.VÞVÞ?VÞå' k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞƒ.VÞVÞVÞƒ.VÞVÞ?VÞÅ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞb.VÞVÞVÞb.VÞVÞ?VÞ¤ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞb.VÞVÞVÞb.VÞVÞ?VÞ¤ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞB.VÞVÞ?VÞ¤æ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞB.VÞVÞVÞb.VÞVÞ?VÞ¤ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞb.VÞVÞVÞb.VÞVÞ?VÞ¤ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë.VÞVÞVÞƒ.VÞVÞVÞƒ.VÞVÞ?VÞÅ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -Ë2Ä2Ä2Gå' k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -ë)ª%‰!HåÅ¤¤¤¤ÄÄÄÄÅåå
æåÅÄÄ
¤¤ÄÄ#¤¤ÄÅåå&æåÅÄÄ
ÄÄ¤¤K¤ÄÅå'!H k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV -ë-Ë)ª%i!H''&''''!H!H!h!h!h!H!H'')
''
'G!H!h!h!h!h!HG'
''S'!H%‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì-ë-Ë)ª%‰%i!h!H!H!H!H!h
!h!h!h%i%i%i
%‰%‰?%‰)ª)ª)ª)Ê)ª)ª)ª%‰!i!H!²*]ÕHH!h!i!iÕ*]!ÒHHH!H!h%‰%Š)ª
)ª)ª)ª%‰
%‰%‰%‰%i%i%i%h!h!h!hG!h%i%‰)ª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 21ì-ë-Ë-Ê)ª)ª)ª)Ê-Ê
-Ë-Ë-Ë-ë-ë-ë1ì2
22G1ì-ë)Ë!Ò"=*2Ÿ!Õ!i%‰%ª)ª)«&2Ÿ*"=±!i!i%Š)ª-Ë-ì1ì221ì1ì-ë-ë-ë-ë-Ë
-Ë-Ë-Ë)Ê)Ê)ª
)ª)ªG)ª)Ê-Ë-ë k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 221ì1ì1ì-ë
-ë-ë1ë1ì1ì2
2226
6-6-C6-6M6M:M6-6-2%ñûÿ	ß?.Õ%i%Š)Ë-ì-ì&.?	ßÿû%Š)«-ì266-6-6-6-6221ì1ì
1ì1ë71ì2 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 22266-6-6M:N
:N:NK:N:n:n>n:n:n:N./¹¿žž¿&_Õ!i%ª-Ì1ì2&&_¿žž¿	˜!Œ-Ë1ì6-6.:N:N
:n:n:N:N6M6-
6-6-K6-66662 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 222666-
6-6-6-6M:M:N:N:N:N:n:n:n
>o>o[>>>o6P”667YZÿ"_´!I%Š-Ì22""_ÿZY766R%Í26-:N:o>o>>>>o>o>o:n:n:n
:N:N:N6M6-6-; k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 2266-
6-6-6-:N
:N:N:N:n>n>o>o>o>>G6PV ó Ð Ð ñ6Zß?“(!j)Ë1ì2!ö?ßZ6 ñ Ð Ð óV)Í2:N:o>o>
>>>>o>o>o:n:n:N:N
6-6-; k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 2666-
6-6-6-:M
:N:N:n:n
>o>o>o>
>>G>o6Oq Ð Î ® Ï ñ|	ßs'!i)«-ì2ö	ß| ñ Ï ® Î Ð	!j)Ë26.:N>o>
>>
>o>o>n:n:N:N
6-6-; k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 2266
6-6-6-6M
:N:N:N:n:n>n>o>o>o>
>>G>o:N6-)Í/ Ï ® ® Ð6|	2!i)«-ì2Ö|6 Ð ® ® ÏÍè'!i)«1ì6-:N>o>
>>
>o>o:n:n:n:N:N:N:N:M
6-6-; k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 22266-6-
:N:N:N:n:n>n
>o>o
>>[>o:N6-1ì)Ëlï Ï Ï ñ6	!i)«-ì2³6 ñ Ï ÏÎÈÅH%‰)Ë1ì6-:N>o>>>>>o>o>o>o:n:n:N
:N:N:M6-
6-6-? k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 222666-
6-6-:M:N
:N:Nw:n:n>n>o>o>o>o>>>o>o:N6.2-Ë)ª!i* ð ò	/(!i)Ë-ì2’ ò ñ
ææ'!i)ª-Ë26.:N>o>o>>>o>o>o>n>n:n:n:N:N:N:N:M6-6-3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
2266-
6-6-6-:M
:N:N:N:n:n:n>o>oC>o:n:N6-2-Ë)ª!iH-5P!i%Š)Ì22³Uo!i(''H!i)ª-Ë26-:N:n>o>o>o:n:n:n
:N:N:N:M6-6-3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
22266-6-:M:N
:N:N:n:n:n>o>o>o?:n:N6-2-ë)ª%Š!i!ik%Š)Ë-ì22-ï-ì)Ë%Š!i!i!i%Š)ª-ë26-:N:N:o
>o>o>o>n:n:n:N:N:M6-
6-6-G6-6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì1ì222666-
6-6-6M:M
:N:N:N:n:n:n>n>o
>o>o?:n:n:N6-61ì-Ë)Ë)«)«)Ë-ì26-6.6-2-ì)Ë)«)«)Ë-ë1ì66-:N:N:n>o
>o>o>o>n:n:N
:N:N:N:M6-6-;62 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì1ì
2266
6-6-6-6M:N:N:n:n:n>n
>o>o;>o>n:n:N:N6-221ì1ì226-6N6N6N6-221ì1ì226-:N:N:n>n
>o>o>o>n:n:n:n:N
:N:N:N6-6-6-;62 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 1ì1ì
22266-6-6-:M:N:N:n:n:n>n
>o>o>o:n:n:N:N6-6-6-6-6N
:N:N:N6N
6-6-6N:N:n>n>n>o
>o>o>n:n:n:N
:N:N:N:M6M6-
6-6-;6622 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢  / k4k4oTsuw–··svgV°w¼kWk4k4oUw–{·×{·svoU
k4k4k4oToToT;oTk4k4gcbò^ÑZ±Z±Z°VV°V°Z°Z°Z°Z°Z±^Ñ^Òbòcggk3k4k4oToToTwoTk4k4k4ggbòbò^Ò^ÑZ±Z±Z±ZÑ^Ñ^Ñ^ÑZÑZ±Z±Z±ZÑ^Ñbòggk4oToToUoUsUsususvsvw–w–{·×{·w–k4Z±Row½{Ýk4k4oTsv{·×·{·svsuoU
oToToUoUoToTkTk4k4k4k4k3k3k3k4k4
k3k3k4k4oToToUoUoToToTk4"k4k4Sk3k3k4k4k4oToToUoUoUoUsususvwvw–w–{·{·×{·subóV°Ro{Þ  s™gk4oUw–{·×{·{·w–w–svsu–susu7svsvw–w–w–{·{·{··{·svk4^ÑVs›    {Þggk4suw–{···
{·{·{—w–Žw–w–w–w—{—{·{·{·7{·w–kTbòV°Row½      w»ggk4oUw–{·{·{··××{·{·{··Ž···×××{·{·{·w–svk4bòZ±Vs›	   sšbógk4oTsuwvw–{·
{·{·{··Š···{·
{·{·{·w–svoUk3bòZ±Vs›   w»bòbòggk4oToUsususvw–’w–w–w–svsvsuoUk4g3bó^ÒZ±sšs›   {Þsš^Ò^òbòbògggšk4k4ggcbò^ÒZ±sšsš{Ý     þ{½w›sšszsšsyžsysyszsšw›{½    h   N   u' ù+‘q
     á(±*q#
  q4
  U
  [ Àrect:	
Û  &“c¸¸ºººººº¾º¾¾¾¾¾¾¾½¾¾½½½½½½¼½¸ºººº¿¿¾º¾¾¾¾¾¾½¾½¾½½¾½½½½½¼º¸¸º¿º¿º¿¾¾¾¾¾¾¾¾¾¾½¾½½½½½½½¸ºººº¿¿¿ºººº	¾k½¾½½½½¼½º¸¸¿ººº¸¸À¸À¸¸¸¢¸¸¾¢¾¾¾½½½½¼»¸ººº¸¿ƒ…ddddddddd†½½½½¼½ºº¸º¿º¿…ÀÀãÀÀãÀÀÀÀÀ½½½½½¼¸ºººº¸ºÛØº¾¾	¾Û½½½½½½¸¸¸º¿º¿¾º¾	¾3Ø¾½½½½¼»ºººº¸ºØº¾¾¾¾¾¾½¾½¾Û‰½½½½½½º¸º¸¿º¿Ø¾¾¾	¾3†½½½½¼½¸º¸ºº¸¿Øº¾¾¾¾¾½¾½¾½Û½½½½½¼º¸ºº¿º¿º¾¾	¾3Ø½¾½½½½»º¸ººººØ¾¾¾¾¾¾¾½¾½¾Û‰¾½½½½¼º¸ºº¿¸¿Ø¾¾¾	¾3†½½½½¼½»º¸ººº¿dØºº¾¾¾¾¾¾½¾½Û½½½½½¼º¸º¸¿¸ºØ¿¾º	¾k†¾½½½½½¸¸ºººº¿dØ£¸¢¸¸¸¸¸¢¾¾Ø½½½½¼½ºº¸º¿º¿dddddØd‰½½½½½¼»ºººº¿¿ÀÛÀÀÀãÅÛÀÀÀÀÀÀÀ½½½½½½¸¸¸¿ºººº¾º¿º	¾½½¾½½½½¼            
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  &“c¸¸ºººººº¾º¾¾¾¾¾¾¾½¾¾½½½½½½¼½¸ºººº¿¿¾º¾¾¾¾¾¾½¾½¾½½¾½½½½½¼º¸¸º¿º¿º¿¾¾¾¾¾¾¾¾¾¾½¾½½½½½½½¸ºººº¿¿¿ºººº	¾k½¾½½½½¼½º¸¸¿ººº¸¸À¸À¸¸¸¢¸¸¾¢¾¾¾½½½½¼»¸ººº¸¿ƒ…ddddddddd†½½½½¼½ºº¸º¿º¿…ÀÀãÀÀãÀÀÀÀÀ½½½½½¼¸ºººº¸ºÛØº¾¾	¾Û½½½½½½¸¸¸º¿º¿¾º¾	¾3Ø¾½½½½¼»ºººº¸ºØº¾¾¾¾¾¾½¾½¾Û‰½½½½½½º¸º¸¿º¿Ø¾¾¾	¾3†½½½½¼½¸º¸ºº¸¿Øº¾¾¾¾¾½¾½¾½Û½½½½½¼º¸ºº¿º¿º¾¾	¾3Ø½¾½½½½»º¸ººººØ¾¾¾¾¾¾¾½¾½¾Û‰¾½½½½¼º¸ºº¿¸¿Ø¾¾¾	¾3†½½½½¼½»º¸ººº¿dØºº¾¾¾¾¾¾½¾½Û½½½½½¼º¸º¸¿¸ºØ¿¾º	¾k†¾½½½½½¸¸ºººº¿dØ£¸¢¸¸¸¸¸¢¾¾Ø½½½½¼½ºº¸º¿º¿dddddØd‰½½½½½¼»ºººº¿¿ÀÛÀÀÀãÅÛÀÀÀÀÀÀÀ½½½½½½¸¸¸¿ºººº¾º¿º	¾½½¾½½½½¼            "?ÿÿÿ"CÄ"Ö5"7­ëz"Ö5"„!"„!"7ªV•"3¦5"?÷5Í"?öµ­"'ä")Q€`"/a @"Ð  "+`  "1   "#€ "'€ "7¥¥"-f5H";æµ("1¢ "/fµH"/f5("' "ƒ¤"%5H"9æµ"+fµH"9êÚQ"5©Æ"1¨ÅÍ"+g5i"Ô ä"' ¤"?÷µ"+gµ"“¤"9çµ("! "1©Eí"/hÅÍ"-hE­")W5i"'µH"%5("Ô Ä"?ùEi")Wµ"5©Åí"-gµi"Õ!"3©Åí"'5i"%µH"#5("ƒ¤"5©E­"?úU­"/d  "?úÙÍ"Ô Ä";ëÚQ"?ûÚ")T  "7¦´ "1¨ÅH"
B`"?ýj•";èD "?øÄ "7¬j•"'µ("/g4 "7¨D "-jV1"?üéí";éD "Õ¥("?ýj1"-iEi"+hÅH"5ªÙ"1©ÅH"Õ  "9ëÙ"-iE("9ìiÍ"3ªÙi"„ Ä"×5H";îzq"7­j"?ÿzq"?ÿzQ"9íéí"7­ëZ"?ÿÿ¾"1¬j¶"×µ­"CÄ"5­jÖ"…!"?ÿÿ:"7­ê¶"„ ä"5­j•"3¬êq"7­ê•"B`")ZUÍ"-kYí"5­j1"'Å"?ÿþq"1¬iÍ"?ÿþQ"+jÙ"  "1¬i"9îú¶";ï{"-kÚ1"7®ú¶"-nz"+nyí"%é"%éÍ"ÛY("ŠU"9îûž"#F1"'Vq"!Æ"7¯ÿz"
C¤"×µ"—5i"%ÚQ"ÙEÍ"#Z1")]j•"ØÅ­"!Ú"ÚUí"ÙE­"ÛY"IÄÄ"!Z1"'j¶"ÛZ"ÚÚ1"5®{z"1­k:"1­ëZ"#jö"šÚq"Üêö"-kÚö"+kZÖ"-lë:"ÚV•"ÚV•"%ëz"ÚÚ¶"Ýkž"
IF•"‰Æ¶"Œë¾"[z"Ýëÿ"kÿ"6Q"ÆÖ"kÿ" ¶•" 
Wz"Œëÿ"
Lkÿ" Æö" [Þ"ÙÆö"†¶"ÙÆö")\ëÞ"ÙÇ"ÙFö"'Ûž"%[z"!W:"ÙÇ"ØÆ•"ØFö"
C"%W:"…¦"×6¶"D""…¦"…¦1" Í" ")ZVÖ"„!("„!"Ci"Ö·:"„#¾"Cÿ"•#ž"×7"Õ§"
BH"A("Õ#:"Ö6"Ô#Z"Óz"×61"%·"Ò“¾"€ÿ")YG"!#z"ƒÞ"'ÆÖ"!6q"Óž"-jÚö"
BÄ"%#:"%¶•"'¶ö"3¬k:"-jÚÖ"%6Q"-hÆÖ"3©Ç"5ªÛ:"+hF•"1¨ÆÖ"9ëÛZ"5¨Æ•"1ªÚÖ"?üëz"?ükZ"7§¶1"5©Æ•"9ç¶1"?ûÛ"?÷¶1"?ûZö"?ùÆ•"9æ5­"    
  !
  tool:action:cursor:   
  qIrect:	
Û  ‰xúúú	C   úúúúúú
   úúúúúú   úúúúúú
	úúúú   úúúúúú
	úúúú   úúúúúú	úúúú   úúúúúú
	úúúú   úúúúúú	úúúú   úúúúúú	úúúú   úúúúúú	úúúú   úúúúúú
	úúúú   úúúúúú	úúúú   úúúúúú
	ú_úúú   úúúúú’

   úúúúú’   úúúúú’   úú’’’’’’’úúú	úú   úúúúú’úúúú   úúúúú’úúúú   úúúúú’úúúú            †ÿÿÿúÿÿÿï   "?ÿÿÿ"    "?ÿÿÿ"  "?ð  " ü "  ÿ" ÿÿ"?ÿü 
  B{" €" "€"'ùþ"/ûþÿ"7ýÿ" € " @"€`"
€ " À"€à" "@"`" "À"à"!ø~"#øþ?"%ù~_")ú~Ÿ"+úþ¿"-û~ß"1ü"3üÿ?"5ý_"9þŸ";þÿ¿"=ÿß"    " 0 " d " 	” " È " ü "   Ì" 0Ì" dÌ" 	”Ì" ÈÌ" üÌ"  ™" 1™" e™" 	•™" É™" ý™"  e" 2e" fe" 	–e" Êe" þe"  2" 32" g2" 	—2" Ë2" ÿ2"  ÿ" 3ÿ" gÿ" 	—ÿ" Ëÿ" ÿÿ"À  "Ã0 "Æd "É” "ÌÈ "Ïü "À Ì"Ã0Ì"ÆdÌ"É”Ì"ÌÈÌ"ÏüÌ"À™"Ã1™"Æe™"É•™"ÌÉ™"Ïý™"Àe"Ã2e"Æfe"É–e"ÌÊe"Ïþe"À2"Ã32"Æg2"É—2"ÌË2"Ïÿ2"Àÿ"Ã3ÿ"Ægÿ"É—ÿ"ÌËÿ"Ïÿÿ"  "“0 "–d "™” "œÈ "Ÿü " Ì"“0Ì"–dÌ"™”Ì"œÈÌ"ŸüÌ"™"“1™"–e™"™•™"œÉ™"Ÿý™"e"“2e"–fe"™–e"œÊe"Ÿþe"2"“32"–g2"™—2"œË2"Ÿÿ2"ÿ"“3ÿ"–gÿ"™—ÿ"œËÿ"Ÿÿÿ"&P  "&S0 "&Vd "&Y” "&\È "&_ü "&P Ì"&S0Ì"&VdÌ"&Y”Ì"&\ÈÌ"&_üÌ"&P™"&S1™"&Ve™"&Y•™"&\É™"&_ý™"&Pe"&S2e"&Vfe"&Y–e"&\Êe"&_þe"&P2"&S32"&Vg2"&Y—2"&\Ë2"&_ÿ2"&Pÿ"&S3ÿ"&Vgÿ"&Y—ÿ"&\Ëÿ"&_ÿÿ"3   "3#0 "3&d "3)” "3,È "3/ü "3  Ì"3#0Ì"3&dÌ"3)”Ì"3,ÈÌ"3/üÌ"3 ™"3#1™"3&e™"3)•™"3,É™"3/ý™"3 e"3#2e"3&fe"3)–e"3,Êe"3/þe"3 2"3#32"3&g2"3)—2"3,Ë2"3/ÿ2"3 ÿ"3#3ÿ"3&gÿ"3)—ÿ"3,Ëÿ"3/ÿÿ"?ð  "?ó0 "?öd "?ù” "?üÈ "?ÿü "?ð Ì"?ó0Ì"?ödÌ"?ù”Ì"?üÈÌ"?ÿüÌ"?ð™"?ó1™"?öe™"?ù•™"?üÉ™"?ÿý™"?ðe"?ó2e"?öfe"?ù–e"?üÊe"?ÿþe"?ð2"?ó32"?ög2"?ù—2"?üË2"?ÿÿ2
  B{"?ó3ÿ"?ögÿ"?ù—ÿ"?üËÿ"?ÿÿÿ
  ƒá(±#*q:
  q4
  U
  [ Àellipse:	
Û  v¡C¸ºººº¿¿¾º¾¾¾¾¾¾¾¾¾½¾½¾½½½½¼½º¸¸ºº¿ºº¾º¾¾¾¾¾¾¾½¾½¾½½½½½½¼»ºº¸º¿¿¿	¾7¾¾¾½½¾½½½½½½¸¸ººº¿º¾ºº¿º¾¾¾¾¾¾½¾¾½½½½½½¼ºº¸º¿º¿º¿¿¾¾	¾á½½¾½½½½¼¸¸ººº¿¿¿¿¾º¢ËdÛ¢½¾½½½½½¼½º¸ººººº¿º¸ëØØÛØ¾¾½½½½½¼¸º¸ºº¿¿ºËdÀ¿¾¾¾¾¾Ø¾¾½½½½½¸º¸º¿¸ºâ¸¾¾¾¾¾¾¾½À¾½½½½¼º¸ºº¸º¸Ÿ¸¾¾¾¾¾¾½¾¾¾ÀÛ½½½¼½»º¸º¸¿ËÀ¿¾¾¾¾¾¾¾½¾½¾†¸½½½¼º¸ººººÛºº¾¾¾¾¾¾¾¾½¾½ÀÀ½½½½¸¸º¸¸¸¿¿¾¾¾¾¾½¾½¾½¾¾½½½¼»º¸ººÀãº¾¾¾¾¾¾¾¾¾½¾¾½Û½½½¼º¸ºººÀÛ¾º¾¾
¾¾¾½k¾½Ø½½½½¸º¸ºººØŠ¾º¾¾¾¾¾¾¾½½¾½¾Ø½½½½¸º¸ººººº¾¾¾¾¾¾½¾¾¾¾Ë¢½½¼½¸ººº¿ºÀØ¾¾¾¾¾¾¾¾¾½¾½¾½½½¼»¸ººº¿ºØ‰Àº¾	¾{›À½½½½½º¸º¸º¿¿¸Ûº¾¾¾¾¾¾¾ Ø½½½½¼½¸º¸º¿º¿¿¸‰Û¸¾¾¸ËÛ¾½½½½½¼¸ºººº¿¿¿¿¿Û‰À½¾½½½½½½º¸¸ºº¿º¿¾¾¾¾£ÛØÛ£À½¾½½½½½½½¼            
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  v¡C¸ºººº¿¿¾º¾¾¾¾¾¾¾¾¾½¾½¾½½½½¼½º¸¸ºº¿ºº¾º¾¾¾¾¾¾¾½¾½¾½½½½½½¼»ºº¸º¿¿¿	¾7¾¾¾½½¾½½½½½½¸¸ººº¿º¾ºº¿º¾¾¾¾¾¾½¾¾½½½½½½¼ºº¸º¿º¿º¿¿¾¾	¾á½½¾½½½½¼¸¸ººº¿¿¿¿¾º¢ËdÛ¢½¾½½½½½¼½º¸ººººº¿º¸ëØØÛØ¾¾½½½½½¼¸º¸ºº¿¿ºËdÀ¿¾¾¾¾¾Ø¾¾½½½½½¸º¸º¿¸ºâ¸¾¾¾¾¾¾¾½À¾½½½½¼º¸ºº¸º¸Ÿ¸¾¾¾¾¾¾½¾¾¾ÀÛ½½½¼½»º¸º¸¿ËÀ¿¾¾¾¾¾¾¾½¾½¾†¸½½½¼º¸ººººÛºº¾¾¾¾¾¾¾¾½¾½ÀÀ½½½½¸¸º¸¸¸¿¿¾¾¾¾¾½¾½¾½¾¾½½½¼»º¸ººÀãº¾¾¾¾¾¾¾¾¾½¾¾½Û½½½¼º¸ºººÀÛ¾º¾¾
¾¾¾½k¾½Ø½½½½¸º¸ºººØŠ¾º¾¾¾¾¾¾¾½½¾½¾Ø½½½½¸º¸ººººº¾¾¾¾¾¾½¾¾¾¾Ë¢½½¼½¸ººº¿ºÀØ¾¾¾¾¾¾¾¾¾½¾½¾½½½¼»¸ººº¿ºØ‰Àº¾	¾{›À½½½½½º¸º¸º¿¿¸Ûº¾¾¾¾¾¾¾ Ø½½½½¼½¸º¸º¿º¿¿¸‰Û¸¾¾¸ËÛ¾½½½½½¼¸ºººº¿¿¿¿¿Û‰À½¾½½½½½½º¸¸ºº¿º¿¾¾¾¾£ÛØÛ£À½¾½½½½½½½¼            
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
  †   
  ’åellipse:	
Û  Ì“úúú’úúúúú úúúúúú’úúƒúúú úúúúúú’úúú
úúúúúúúú úúú’’’’’’g
úúúúúú úúúúúú’úúúúúúúúú úúúúúú’úúúúúúúúúúúú úúúúúúhúú	ú
úúúú úúúúúúúúú	úúúúú úúúúúúúúú	úúúúú úúúúú
úúúúúú úúúúúúúú
úúú úúúúúúúúúúú úúúúúúúúú
úúú úúúúúú
úúú	úúúúú úúúúúúúúú	úú
úúú úúúúúúúú	ú'úúúú úúúúúúúúúúúúúúúúúúú 	úúúúúúúú 	úúúúúúúúú 	úúúúúúúúúúúúú úúúú          †ÿÿÿûÿÿÿü   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá(‘;*¡P
  q4
  U
  [ Àpolygon:	
Û  à½»¸º¸º¸¿¿¿º¾¿	¾;¾½¾½½½½½½¼½½¼   ¸»¸º¸ººº¾¾º¾¾¾¾¾¾¾½¾½¾½½½½½½½¼½¼¼   »¸»º
ºº¿ºâgº¾¾¾¾¾¾¾¾½¾½¾½½½½½¼¼¼   »»¸¸¸ººº¿¿¿¿¿º¾¾¾¾¾¾¾¾¾½½½½½½¼½¼¼   »º¸ºººº¿¿º¿Ÿ¢†ØØØÛÛ†Û†À½¾½½½¼½½¼¼   ¸»¸º¸º¿º¿¿£…‰‰‰Š¾½½½½½¼¼½¼   »¸»ºººº¿º¿ÀÛÀ¾¾¾¾¾½¾½ÀÀ¾½½½½½¼¼¼   »»¸¸¸ººº¾ºÀ‰Û¾¾¾¾¾¾¾¾¾‰½½½½¼½¼½¼   »¸ººººº¿ºº¸À¾¾¾¾¾½¾½¾Û¾½½½½¼½¼¼   ¸»¸¸ºº¿º¿¿£À¾¾¾¾¾¾¾½¾¢À½½½½½¼¼¼   »¸»º¸ºº¿º¿ÀÛ¾¾¾¾¾½¾¾½¾½½½¼½½¼½   »»º¸º¸ºº¿º¸Å¾¾¾¾¾¾½¾½¾Û¾½½½¼¼¼¼   »¸¸¸ººº¿¿¿ÀÀ¾¾¾¾¾½¾½¾½¢À½½½½¼½¼   ¸¸»º¸º¿ºº¿¸Û¾¾¾¾¾¾¾¾½¾¾½½½¼½¼¼   »»º¸ººº¿¿ºÀÀ¾¾¾¾¾¾½¾¾½½Û¾½½½¼¼¼   »¸»º¸º¿º¿¿¸Ûº¾¾¾¾¾¾½¾¾½¸À½½¼½½¼   ¸»¸¸ººº¿¿ºÀÛÛ¸£¸¸¸¢¸¸¢¾¢¾½½½¼¼¼   »¸ºº¸º¿º¿¾¸ddddddØdd¾½½½¼¼   »»¸ºººº¿º¿¸ÀÛÀÀãÀãÀÀÀÀÀÀÀÀÀ½½¼½¼¼   ¸»¸¸¸ºº¿º¾¿º¾½½¼½¼¼   »»ºººº¿º¾¿º¾	¾½¾½½½½½½½½½¼¼      !         
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  à½»¸º¸º¸¿¿¿º¾¿	¾;¾½¾½½½½½½¼½½¼   ¸»¸º¸ººº¾¾º¾¾¾¾¾¾¾½¾½¾½½½½½½½¼½¼¼   »¸»º
ºº¿ºâgº¾¾¾¾¾¾¾¾½¾½¾½½½½½¼¼¼   »»¸¸¸ººº¿¿¿¿¿º¾¾¾¾¾¾¾¾¾½½½½½½¼½¼¼   »º¸ºººº¿¿º¿Ÿ¢†ØØØÛÛ†Û†À½¾½½½¼½½¼¼   ¸»¸º¸º¿º¿¿£…‰‰‰Š¾½½½½½¼¼½¼   »¸»ºººº¿º¿ÀÛÀ¾¾¾¾¾½¾½ÀÀ¾½½½½½¼¼¼   »»¸¸¸ººº¾ºÀ‰Û¾¾¾¾¾¾¾¾¾‰½½½½¼½¼½¼   »¸ººººº¿ºº¸À¾¾¾¾¾½¾½¾Û¾½½½½¼½¼¼   ¸»¸¸ºº¿º¿¿£À¾¾¾¾¾¾¾½¾¢À½½½½½¼¼¼   »¸»º¸ºº¿º¿ÀÛ¾¾¾¾¾½¾¾½¾½½½¼½½¼½   »»º¸º¸ºº¿º¸Å¾¾¾¾¾¾½¾½¾Û¾½½½¼¼¼¼   »¸¸¸ººº¿¿¿ÀÀ¾¾¾¾¾½¾½¾½¢À½½½½¼½¼   ¸¸»º¸º¿ºº¿¸Û¾¾¾¾¾¾¾¾½¾¾½½½¼½¼¼   »»º¸ººº¿¿ºÀÀ¾¾¾¾¾¾½¾¾½½Û¾½½½¼¼¼   »¸»º¸º¿º¿¿¸Ûº¾¾¾¾¾¾½¾¾½¸À½½¼½½¼   ¸»¸¸ººº¿¿ºÀÛÛ¸£¸¸¸¢¸¸¢¾¢¾½½½¼¼¼   »¸ºº¸º¿º¿¾¸ddddddØdd¾½½½¼¼   »»¸ºººº¿º¿¸ÀÛÀÀãÀãÀÀÀÀÀÀÀÀÀ½½¼½¼¼   ¸»¸¸¸ºº¿º¾¿º¾½½¼½¼¼   »»ºººº¿º¾¿º¾	¾½¾½½½½½½½½½¼¼      !         
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
  †   
  ©fpolygon:	
Û  ß“úúú’úúúúú úúúúúú’úúúúú úúúúúú’úúúúú úúú’’’’’’’úúúáúúú úúúúúúhúúúúúúúú úúúúúúgúúúúúúú úúúúúúg
úúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúú úúúúúúúúúúúúúúúúúúúúú úúúúúúúúú	úúúúúú úúúúúúúúú	úSúúúú úúúúúúúúú	úúúúú úúúúúú
	Cúúúú úúúúúúúúúú úúúúúúúúúú úúúú úúúú          †ÿÿÿûÿÿÿü   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá(¡P*‘h
  q4
  U
  [ Àstar:	
Û  ûÀáo»¸¸ººº¿º¿¾¾¾¾¾¾¾¾½¾¾½¾½½½½½½¼¼¼ ¸º¸º¸ºº¿¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½½½¼½ »¸¸º¿º¿ºº¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½¼½¼ ¸¸º¸ºº¿¿º¾º¾º¾ºŸØ¾¾¾½¾½½½½½½¼½¼ »º¸ºº¿º¿¾º¾¾¾¾Ã…¸½¾½½½½½½½¼½¼¼ ¸»º¸ºº¿º¿¾º¿¿¿…Ø¾½¾¾½¾½½½½¼½¼ ¸º¸ºººº¿ºº¿¾¿Ã£Ø¸¾¾½¾½½½½½½¼¼ »¸¸º¸¿¿º¿¿¿¾¿d¾¢†¾½¾¾½½½½½¼½½ ¸ººº¸º¸À¢âÀ†À¾¾Ø‰ÛÀÀ¢¾½½¼½¼¼ »¸¸ºººÀ™ŸØØ¿¾¾¿Ø½½½¼½¼ º»º¸¿º¸Û¸¾¾¾¾¾¾½¾¾¾âÛ½½½½¼½ »º¸ºººº¸Øº	¾á¾„Û½½½¼½¼¼ ¸¸º¸º¿¿¿¿ÛØ¿¾¾¾¾¾¾¾…À½½½½½¼½¼ »¸ºº¸ºººººÛÀ¿¾¾¾¾¾ À½½½½½½½¼½ ¸»¸ººº¿¿¿¿£À¾¾¾¾¾¾Û¾½¾½½½½¼½¼ »¸ºººº¿º¿¿À£¿¾¸¿¾¾£¾¾½½½½¼½¼¼ ¸º¸¸º¿º¿¿¿À¸À¢ddÛ¿ÀÀ½½½½½½¼½¼ »¸ºº¸º¿º¿¿†ØØ‰ÛÛ¾½½½½½½¼¼ ¸º¸ºººº¿º¾Û¸¾½À‰Ø½¾½½½½¼½½ ¸»º¸ºº¿¿º¾Ø£¾¾¾¾¾½ÀØ½½½½½¼½½¼ »º¸ºº¿º¿¾º¾¾¾¾¾¾
¾½¾½½g¼¼¼ »¸º¸ºº¿º¿¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½½½¼ »¸º¸ºººº¾º¾¾¾¾¾¾¾¾½¾½¾½½½½½½¼½¼ ¸º¸ºº¿¿º¾º¾¾¾¾¾¾¾½¾¾½½¾½½½½½¼½¼             
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  ûÀáo»¸¸ººº¿º¿¾¾¾¾¾¾¾¾½¾¾½¾½½½½½½¼¼¼ ¸º¸º¸ºº¿¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½½½¼½ »¸¸º¿º¿ºº¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½¼½¼ ¸¸º¸ºº¿¿º¾º¾º¾ºŸØ¾¾¾½¾½½½½½½¼½¼ »º¸ºº¿º¿¾º¾¾¾¾Ã…¸½¾½½½½½½½¼½¼¼ ¸»º¸ºº¿º¿¾º¿¿¿…Ø¾½¾¾½¾½½½½¼½¼ ¸º¸ºººº¿ºº¿¾¿Ã£Ø¸¾¾½¾½½½½½½¼¼ »¸¸º¸¿¿º¿¿¿¾¿d¾¢†¾½¾¾½½½½½¼½½ ¸ººº¸º¸À¢âÀ†À¾¾Ø‰ÛÀÀ¢¾½½¼½¼¼ »¸¸ºººÀ™ŸØØ¿¾¾¿Ø½½½¼½¼ º»º¸¿º¸Û¸¾¾¾¾¾¾½¾¾¾âÛ½½½½¼½ »º¸ºººº¸Øº	¾á¾„Û½½½¼½¼¼ ¸¸º¸º¿¿¿¿ÛØ¿¾¾¾¾¾¾¾…À½½½½½¼½¼ »¸ºº¸ºººººÛÀ¿¾¾¾¾¾ À½½½½½½½¼½ ¸»¸ººº¿¿¿¿£À¾¾¾¾¾¾Û¾½¾½½½½¼½¼ »¸ºººº¿º¿¿À£¿¾¸¿¾¾£¾¾½½½½¼½¼¼ ¸º¸¸º¿º¿¿¿À¸À¢ddÛ¿ÀÀ½½½½½½¼½¼ »¸ºº¸º¿º¿¿†ØØ‰ÛÛ¾½½½½½½¼¼ ¸º¸ºººº¿º¾Û¸¾½À‰Ø½¾½½½½¼½½ ¸»º¸ºº¿¿º¾Ø£¾¾¾¾¾½ÀØ½½½½½¼½½¼ »º¸ºº¿º¿¾º¾¾¾¾¾¾
¾½¾½½g¼¼¼ »¸º¸ºº¿º¿¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½½½¼ »¸º¸ºººº¾º¾¾¾¾¾¾¾¾½¾½¾½½½½½½¼½¼ ¸º¸ºº¿¿º¾º¾¾¾¾¾¾¾½¾¾½½¾½½½½½¼½¼             
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
  †   
  ÀÎstar:	
Û  Ax%úúúú	úúúú’	úú	úúúú’úúúúúúúú	úúúú’úúúúúúú	ú’’’’úúúú	ú;úúú’úúúúúúúgúúúúúúúúúúh
	úúúúúúúúúú	úúúúúúúúúúúúúúúúúú	úúúúúúúúúúú	úúúúúúúúúúú	úúúúúúúúúúú	úúúú
úú	úúúú
ú	úúúúúúú	úúúúúúúúúú
ú5ú         †ÿÿÿþÿÿÿû   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá(Ðù*¡
  q4
  U
  [ Àline:	
Û  m /¸¸ººººº¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½¼½¼   ¸º¸ºº¿ºº¾¾¾¾
¾¾¾½#¾½½½½½½¼½   º¸ºº¿ºº¾ºº¾¾¾¾¾¾¾½½¾	½½   ¸º¸ºº¿¿¿	¾Ÿ½¾¾½½¾½½½½¼½¼   »ººººº¿¾º¾¾¾¾¾¾¾¾½¾¾½¾½½½½½¼½   ¸¸¸¿º¿ºº¾º¾¾¾¾¾½¾¾½½¾½¸£½½½¼½   ¸ºººº¿º¾º¾¾¾¾¾¾¾¾½¾¾½¾†½½¼½¼   º¸¸º¿º¿¾º¾¾¾¾¾¾¾¾¾¾½¢†¾½½½¼½   ººº¸º¿º¿¾º¾¾	¾7†¾½½½½½¼   ¸¸ºººº¿¿º¾¾¾¾¾¾¾¾¾¾†¾½½½½½¼½   ¸º¸ºº¿¿º	¾á3¾¾†¸½½½½½¼½¼   º¸ºº¿º¿¾º¾¾¾¾¾¾¾ ¾½¾½½½½½½¼   »º¸º¿ºº¿¾º¾¾¾¾¾¡¾¾½½¾½½½¼½¼   ¸¸º¸º¿º¾º¾¾ºº¾¡¾¾½¾½½½½½½½¼   º¸ºº¿º¿ºº¾º¾¾¡¿¾½¾½¾½½½½¼½¼   ¸º¸ºº¿¿¾¿º¿¾Ÿ¿¾½¾½¾½½½½½½¼½   »ºººº¿ºº¾¾¾Ë¿¾¾¾¾½½¾½½½½½½¼   º¸¸¿ºº¿¾º¾À¸¾¾½¾½¾½½¾½½½½¼½   »¸ººº¿¿¿º¾¸Û¿¾¾¾¾½¾½¾½½½½½½½½   ºº¸º¿º¾º¾º¾¾¾¾¾¾¾¾½¾½½½½½½½¼¼               
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  m /¸¸ººººº¾º¾¾¾¾¾¾¾¾½¾½½¾½½½½¼½¼   ¸º¸ºº¿ºº¾¾¾¾
¾¾¾½#¾½½½½½½¼½   º¸ºº¿ºº¾ºº¾¾¾¾¾¾¾½½¾	½½   ¸º¸ºº¿¿¿	¾Ÿ½¾¾½½¾½½½½¼½¼   »ººººº¿¾º¾¾¾¾¾¾¾¾½¾¾½¾½½½½½¼½   ¸¸¸¿º¿ºº¾º¾¾¾¾¾½¾¾½½¾½¸£½½½¼½   ¸ºººº¿º¾º¾¾¾¾¾¾¾¾½¾¾½¾†½½¼½¼   º¸¸º¿º¿¾º¾¾¾¾¾¾¾¾¾¾½¢†¾½½½¼½   ººº¸º¿º¿¾º¾¾	¾7†¾½½½½½¼   ¸¸ºººº¿¿º¾¾¾¾¾¾¾¾¾¾†¾½½½½½¼½   ¸º¸ºº¿¿º	¾á3¾¾†¸½½½½½¼½¼   º¸ºº¿º¿¾º¾¾¾¾¾¾¾ ¾½¾½½½½½½¼   »º¸º¿ºº¿¾º¾¾¾¾¾¡¾¾½½¾½½½¼½¼   ¸¸º¸º¿º¾º¾¾ºº¾¡¾¾½¾½½½½½½½¼   º¸ºº¿º¿ºº¾º¾¾¡¿¾½¾½¾½½½½¼½¼   ¸º¸ºº¿¿¾¿º¿¾Ÿ¿¾½¾½¾½½½½½½¼½   »ºººº¿ºº¾¾¾Ë¿¾¾¾¾½½¾½½½½½½¼   º¸¸¿ºº¿¾º¾À¸¾¾½¾½¾½½¾½½½½¼½   »¸ººº¿¿¿º¾¸Û¿¾¾¾¾½¾½¾½½½½½½½½   ºº¸º¿º¾º¾º¾¾¾¾¾¾¾¾½¾½½½½½½½¼¼               
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
  †   
  ×Èline:	
Û  ,~úúú  úúú  úúúúú  úúúú  úúú  úúú  úúúúúúú  úúúúúúú  úúúúúúú  úúúúúúúú  úúúúúúúú  	úúúúúúúúúúúú  	úúúúúúúúúúúú  úúúúú’úú	úúú  úúúúú’úú	úúú  úúúúú’úúú	úúú  úú’’’’’’’úúú	úúú  úúúúú’úúúúú  úúúúú’úúúúú  úúúúú’úúúúú  úúú           †ÿÿÿûÿÿÿï   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒ
  D0 Àshapes³  5ìï ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™B™B¹
BºBºSFºFÚFÛFÛFÛJÛJÛJûJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™B™B¹
BºBºSFºFºFÚFÚFÛFÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™B™B¹
BºBº
FºFºKFÚFÛJÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™B™B¹BºBºKFºFºFÚJÛJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™B™B¹
BºBºSB¹B™-Ñ%nB™B¹FºFÚJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>™>™>™>™B™B™B¹BºBºB¹B™B™1ò¥!>xB˜B™FºFÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™>™>™>™B™
B™B™WB™>˜1ò¥ !-Ò:V>wB˜B¹FÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>™>™>™>™B™B™B™B™>™>x1ñ¥ !-Ñ65:5:V>wB™FºJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>™>™>™>™B™>™>™>˜>x1ñ¥ !)°6665:V>wB™FºJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:x>x>™
>™>™_>™>x>x1Ñ¥ !)°21ó664:V>xB¹FÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>™>™>™>™>˜>x>w:  !)°1ó1ò1ó6:5>WB˜F¹FÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>™>˜>˜>x>x:wFt  !)°1ó1ò1ò264:V>wB™FºJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>y>x>x>x:wFu  !)1ò-ò-ò1ó6:U>wB˜F¹FÚJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>x>x:x:WFu  !)-ò-Ò-ò1ó6:5>WB˜B¹FºJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x>x:x:wFt  !)-ò-Ò-Ò1ó665:W>˜B¹FºFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:x:xk>x:x:w5Ð  !)-ò-Ò-Ò1ó265:V>xB™FºFÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W:X:x:x:xg!m  !)°1ó-ò-Ò1ó265:V>xB™BºFÚFÛJÛJÛJüJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:x:w-òé)±21ó1ò1ó265:V>wB™BºFºFÛFÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:x:w:W6V65222265:V>wB™BºFºFÛFÛJÛJÛJûJüJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x
:x:xk:x:w:W6V656566565:V>w>˜B™FºFºFÛFÛJÛJÛJûJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:xg:w:W6V66656V:V>w>˜B™BºFºFÚFÛFÛJÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:xg:w:W:W:V:V:W>w>x>™B™B¹FºFºFÚFÛJÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:x:w:W:W:W:V:W:W>w>x>˜>˜B™B¹BºFºFÚFÛJÛJÛJüJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W:X:x
:x:xk:W:W:W6V6V6V:V:V:W:W>w>w>x>˜B˜B™FºFºFÛJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:x:w1ó-Ñ)±)°)°S-Ñ-ò>w>xB™FºFÚJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:W:WkZ5­„BBWc¥! :5:V>xB™FºFÛJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:X:W6W6V-kBéééèè
ééWéé" 665:VB˜B¹FÚJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:W:W6W66cd-Ò)Ñ)°)°)°)±-Ñ
-Ñ-ÑSd 1ò2:5>wB™FºJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:w:W6W65!d)Ñ
)°)°_)Ñ-Ñ-Ò-Ò-Ñ-Ñd -Ñ1ó64:VB˜FºJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W:W6W6V65!d)±)°)°)°)Ñ-Ò-ò1ó1ó-ò-Ò… -Ñ1ò6:VB˜FºFÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W:W:W6V25!d)±)°)°)Ñ-ò1ó26621ó… -Ñ1ò6:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6V25!d)Ñ)±)Ñ-ò26565:V:V656… -ò1ò6:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6625!d)Ñ)Ñ-Ò1ó6:V:W>w:w:V65¦ 1ò1ó6:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6625!d)Ñ)Ñ-ò265:W>x>x>x>w:V¦ 1ó1ó6:V>xF¹FÚJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6625!d)Ñ)Ñ-ò265:W>x>˜>x>w:V¦ 1ó26:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6V25!d)Ñ)Ñ-Ò1ó65:V:w>x>w:W:6¦ 1ò1ó6:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6625Bd)Ñ)±)Ñ-ò265:V:V:V:V65¦ 1ò1ò6:V>xF¹FÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W#6W:W:W:W:W6V25BCLL!L!m%n%)°
)°)°Sc -Ñ1ò6:V>xF¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W:W:W6W6V65    !BBWB   -Ñ1ò6:V>˜F¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:w:W6V65+éÈÈÈÈÈèééééèèé-Ñ1ò6:VB˜FºJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:w:W6W662-ó-Ò)±)°)°)°)±-Ñ-Ñ-ÑO-ò265>wB™FºJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:W:W6W6V652-ó-Ò)Ñ)Ñ)Ñ-Ñ-Ñ
-Ò-ÒS-Ò1ò264:V>xB¹FÚJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:w:X:W6W666521ó-ó-ò-ò-ò1ó
1ó1óS2665:V>wB˜FºFÛJÛJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W:W:X:x:x:W:W66652222W66465:5:V>wB˜B¹FºFÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:x:w:W:W6V66656565W65:5:V:W>wB˜B™FºFÚJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W:X:x
:x:x:w:W6V666565W65:6:V>w>xB˜B¹FºFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:W:W65-Ð5Ï„B!B¦%n65:V:V>wB˜B¹FºFÛJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:x:w:W6W-Ñ=ï  c„d !  d)°65:V>wB˜B¹FÚJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:X:x:w:W:W9ñ! é)°-ò-Ò-ò-ò!nd  !)°65:V>wB™FºFÛJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:x:W:W6W1Ñ! !%-ò-Ò-Ñ-Ñ-Ò-Ò-Ò-Ò
 !)°65:V>wB™FºJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W#6W:W:W:w:W6W2FR !%-Ò-Ñ)Ñ-Ñ-Ò-Ò
-ò-òS
 d265>VB˜F¹FÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6V5ñ é-Ò)Ñ)±-Ñ-Ò-ó1ó2221ó1ò… %n6:5>wB™FºJÛJüJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:W6W66Ç 
)±)Ñc-Ò1ó26565656562, È1ó64:VB˜F¹JÛJûJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W6W6V2Bd)Ñ)±)±-Ò-ó265:V:W:w:W:V651ò!d1ó6:U>wB¹FÚJÛJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ws66%°!È)Ñ)±)Ñ-ò265:W>x>x>˜>x:w:V64!d1ò2:5>wB™FÚJûJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ws66%!¦)±)±)Ñ-ó2:V:x>˜B™B™>™>x:W65!d1ò265>VB˜FºJÛJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W6W662!C)±)±-Ñ-ó65:V>x>™B™B¹B™>˜>w1ó „1ò264>VB˜FºJÛJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W6W662d %)±)Ñ-ó65:V>x>™B™B™B™>˜>w1¯ 
1ò264>VB˜FºJÛJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W6W6W662, …)±)Ñ-ó266:w>x>™B™>˜>w65„ )°1ò265>WB˜FºJÛJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W6W6V252C é)Ñ-Ò265:V:w>x>x>w65R¶ é1ò1ò6:5>wB™FÚJÛJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ws6V662% ! Ç)°-ò265:V:V:V2> c1ò1ò1ó6:V>xF¹JÛJûNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6622% ! "é!n1ó-Ò)°-  „-Ñ1ò1ò265>VB˜FºJÛJüJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W:W6W662-ó)±¦   !!   !
-ò-ò1ò1ó6:V>wB™FÚJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:W:W:W6V652-ó-Ò)°
Çd…é!L-Ò-Ò-Ò1ò1ó6:5>wB˜FºFÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:X:x:W:W666521ó-ò-Ò-Ñ
-Ñ-ÑW-Ñ-Ò1ò1ó665:V>˜B¹FÚJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:x:w:W6V6522-ó-ò
-Ò-ÒW-ò1ò2665:V>xB™FºJÛJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:x:x:w:W6V656521ó1ó-ó1ó1ó1ó2665:V>xB™FºFÚJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:w:W6V6562221ó22665:V>w>˜B¹FºFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:w:W6V65652222266465:V>wB˜B™FºFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:w:x
:x:x2Jt)lÇ¦¦W¦…È:5:V>wB˜F¹FÚJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:x:x:W-Ñ-k  S )°65:V>xB™FºJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:x:x:W)±cê-ò-Ò-Ñ-Ñ-Ò-Ò-Ò-Ò
 È665:VB˜F¹FÚJÛJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:x:x:x:W6V)°!é-Ñ)±)°)±-Ñ-Ñ-Ò-Ò)± "1ò6:5>wB™FºJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W6W:W
:x:xo:W6V%!é)°)°)°)±-Ò-ò-ò1ò-ò… %Ž1ó64:VB˜F¹FÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:w:W66%!è)°)°)°-Ñ-ò2221ó+ Ç1ò6:5>wB™FÚJÛJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:w:x:x:x:W66%!è)°)°)Ñ-ò26565656-ò "1ò265>wB˜FºJÛJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:W:W66%!è)°)±-Ò1ó65:V:W:V:V64¦ %1ó64:VB˜FºJÛJüJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:x:x:w:W66%!è)±)Ñ-ò265:W>w>w:W:5!L È1ò6:5>wB¹FÚJûNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:x:x:x:W:W66%!è)±)Ñ-ò266:W>x>x>w:V2 "1ò265>wB™FÚJÛNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:W:W66%!è)±)±-Ò265:W>w>w:W:V6¦ %Ž1ò6:VB˜FºJÛJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:x:x:W:W66%Bè)°)°)Ñ-ó665:V:V:V656!L Ç-Ò6:5>wF¹JÛJûNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:x:x:x:W66%„¦,,!L!m%Ž))°)°)°))%Ž "-Ñ1ó65>wB¹FÚJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:w:W66%!   !BBO   %m1ò64>wB™FÚJûNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W
:x:x:W6V1óéèÈ
ÈÈ[èééééèÈÈÈ)°1ò64>wB™FÚJûNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:x:W6V652-ò)Ñ
)°)°[)Ñ-Ñ-Ò-Ñ-Ñ-Ñ-±)°-Ñ-Ò2:5>wB™FÚJûNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W+6W:W:X:x:x:x:x:W66652-ò-Ñ)Ñ)Ñ-Ñ-Ñ-Ò-Ò-ò
-Ò-ÒK1ò1ó64:VB˜F¹JÛJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:x:x:W6W662521ó-ó-ò-ò
1ó1óS2222665:V>wB™FºJÛJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:x:w:W6V65652
22W6646565:5:V:V>w>xB˜FºFÚJûJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:x:W:V:V65
6565W65:V:V:W>w>w>˜B˜B™FºFÚJÛJûJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:xg:x:w:W65B…6565:5:V>w>xB˜B™B¹FºFºFÛJÛJûJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W
:x:xo:x>x>x>x:x:w:W>3-k %665:V>w>xB™BºFºFÚFÛJÛJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:xg:w:W655Î ! d1ó265:V>wB™B¹FºFÚFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:xk:w:W:W665ñ éd !L-Ò265:V>wB™B¹FºFÚJÛJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:X:x:W:W:W6V66651óc )°, d)°-Ñ265:V>wB˜B¹FºFÚJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W:w:W:W-Ò1Ñ1Ð*Ç  È)°%c  !…Ç	%n-Ñ>W>xB™FºFÛJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W:W)ÑZÖNsB  C…%Ž%%+c!  !  65:V>xB¹FÚJÛJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:W6W2…! !!n)°%%%S
  ¦265>wB˜FºJÛJûJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W66-óC C!n%n%n%%)°)°)°)°!m%) …)°-ò6:VB˜FºJÛJûJüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W662-ó¦ B!M!n%)°)Ñ-Ñ-Ò%-k Ç%)°-Ñ6:VB˜FºJÛJûJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:W:W6W6625-ó)Ñ¦ ¦%n%)°-Ò-ò-ò5Ï 
%%)°-Ñ6:VB˜FºJÛJûJüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:W6W6W652-Ò+ §%n%)±-Ò1ó1ó¦ ))))°1ò64>wB™FºJÛJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:w:w:W6W662-ó!+ 
%n%!m%-ò-òè ,))°-Ñ2:5>xB¹FÚJÛJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:X:X:W6W652* L+%J„!Ç%+ Ç))°-Ò6:VB˜FºJÛJûJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:W:x:x:W:W662Ç c  c"  d …))°1ò65>wB™FÚJÛJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:x:W6V25   c
%n%é!  c)-Ñ1ó:5>xB¹FÚJÛJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6Ww:X:x:x:x:W:W65 !"+%%Ž%Ž%%%é c)°-Ñ2:VB˜FºFÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:W:x:x:x:w:W662-ó)Ñ)°)°))°
)°)°S)°-Ñ1ó64>WB˜FºJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
6W6W{6W:W:X:x:x:x:x:W6V652-ó-Ò-Ñ-Ñ-Ò-Ò-ò-Ò-Ò-ò1ó6:V>wB™FºJÛJûJüJüNüNüNüO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x:x:x:x:w:W6V652
22
66S665:V>wB˜B¹FÚJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 6W6W:X:x
:x:x:w:W:V66656566:V
:V:VS:V:W>wB˜B¹FºFÛJÛJûJüJüNüNüOO k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV ""&!!""""7" ^Òk4oTsuw–··svgV°w¼  kWk4k4oUw–{·×{·svoUk4k4oToTwk4k4k4ggbòbò^Ò^ÑZ±Z±Z±ZÑ^Ñ^Ñ^ÑZÑZ±Z±Z±ZÑ^Ñbòggk4oToToUoUsUsususvsvw–w–{·×{·w–k4Z±Row½  {Ýk4k4oTsv{·×·{·svsuoU
oUoUoUoToToT"k4k4k4k3k3k4k4k4oToT
oUoU;sususvwvw–w–{·{·×{·subóV°Ro{Þ    s™gk4oUw–{·×{·{·w–w–Fsusu?susvsvw–w–w–{·{·{··{·svk4^ÑVs›      {Þggk4suw–{···{·{·Fw–w–w—{—{·{·w–kTbòV°Row½	 w»ggk4oUw–{·{·{··J··×××{·{·{·w–svk4bòZ±Vs›  	   sšbógk4oTsuwvw–{·F··{·{·w–svoUk3bòZ±Vs› w»bòbòggk4oToUJw–w–svsvsuoUk4g3bó^ÒZ±sšs›     {Þsš^Ò^òbòbògJk4k4k4ggcbò^ÒZ±sšsš{Ý   þ{½w›sšszNsysysyszsšw›{½      ?   x   á) z*°“
  
  U
  [ Àbrush6:	
Û  Ú¯¶¶¶¶ ¶¶¶¶ ¶¶ÇÇÇcÇ¶¶ ¶¶ÇÇÇÇÇÇ£¢xwwwwxœœ£ÇÇÇÇÇÇ¶¶ ¶¶ÇÇÇÇÇ¢œqM((Swœ¢ÇÇÇÇÇ¶¶ ¶¶ÇÇÇÇ¢q(LSœœÇÇÇÇ¶¶ ¶¶ÇÇÇ¢w	Mœ¢ÇÇÇ¶¶ ¶¶ÇÇ£œMw¢ÇÇÇ¶¶ ¶¶ÇÇ¢SLLœ¢ÇÇ¶¶ ¶¶ÇÇœw¢ÇÇ¶¶ ¶¶ÇÇœLM£ÇÇ¶¶ ¶¶ÇÇw((ÇÇÇ¶¶ ¶¶ÇÇwÇÇÇ¶¶ ¶¶ÇÇœ(LÇÇÇ¶¶ ¶¶ÇÇxLSÇÇÇ¶¶ ¶¶ÇÇœS¢ñÇÇ¶¶ ¶¶ÇÇ¢œ(ÇÇÇÇ¶¶ ¶¶ÇÇÇ¢S¢òÇÇÇ¶¶ ¶¶ÇÇÇ¢¢S	g¢òÇÇÇÇ¶¶ ¶¶ÇÇÇÇ¢œwL¢òñÇÇÇÇ¶¶ ¶¶ÇÇÇÇÇ£œ¢wM(LS¢ñòÇÇÇÇÇÇ¶¶ ¶¶ÇÇÇÇÇÇÇ££ÇÇÇÇññÇÇÇÇÇÇÇÇ¶¶ ¶¶ÇÇÇÇ¶¶ ¶¶¶¶ ¶¶¶¶             "?ÿÿÿ
  D0"?ÿÿÿ"  "?ð  " ü "  ÿ" ÿÿ"?ÿü "?ðÿ" €" "€"'ùþ"/ûþÿ"7ýÿ" € " @"€`"
€ " À"€à" "@"`" "À"à"!ø~"#øþ?"%ù~_")ú~Ÿ"+úþ¿"-û~ß"1ü"3üÿ?"5ý_"9þŸ";þÿ¿"=ÿß"    " 0 " d " 	” " È " ü "   Ì" 0Ì" dÌ" 	”Ì" ÈÌ" üÌ"  ™" 1™" e™" 	•™" É™" ý™"  e" 2e" fe" 	–e" Êe" þe"  2" 32" g2" 	—2" Ë2" ÿ2"  ÿ" 3ÿ" gÿ" 	—ÿ" Ëÿ" ÿÿ"À  "Ã0 "Æd "É” "ÌÈ "Ïü "À Ì"Ã0Ì"ÆdÌ"É”Ì"ÌÈÌ"ÏüÌ"À™"Ã1™"Æe™"É•™"ÌÉ™"Ïý™"Àe"Ã2e"Æfe"É–e"ÌÊe"Ïþe"À2"Ã32"Æg2"É—2"ÌË2"Ïÿ2"Àÿ"Ã3ÿ"Ægÿ"É—ÿ"ÌËÿ"Ïÿÿ"  "“0 "–d "™” "œÈ "Ÿü " Ì"“0Ì"–dÌ"™”Ì"œÈÌ"ŸüÌ"™"“1™"–e™"™•™"œÉ™"Ÿý™"e"“2e"–fe"™–e"œÊe"Ÿþe"2"“32"–g2"™—2"œË2"Ÿÿ2"ÿ"“3ÿ"–gÿ"™—ÿ"œËÿ"Ÿÿÿ"&P  "&S0 "&Vd "&Y” "&\È "&_ü "&P Ì"&S0Ì"&VdÌ"&Y”Ì"&\ÈÌ"&_üÌ"&P™"&S1™"&Ve™"&Y•™"&\É™"&_ý™"&Pe"&S2e"&Vfe"&Y–e"&\Êe"&_þe"&P2"&S32"&Vg2"&Y—2"&\Ë2"&_ÿ2"&Pÿ"&S3ÿ"&Vgÿ"&Y—ÿ"&\Ëÿ"&_ÿÿ"3   "3#0 "3&d "3)” "3,È "3/ü    €gàgà"3  Ì"3#0Ì"3&dÌ"3)”Ì"3,ÈÌ"3/üÌ"3 ™"3#1™"3&e™"3)•™"3,É™"3/ý™"3 e"3#2e"3&fe"3)–e"3,Êe"3/þe"3 2"3#32"3&g2"3)—2"3,Ë2"3/ÿ2"3 ÿ"3#3ÿ"3&gÿ"3)—ÿ"3,Ëÿ"3/ÿÿ"?ð  "?ó0 "?öd "?ù” "?üÈ "?ÿü "?ð Ì"?ó0Ì"?ödÌ"?ù”Ì"?üÈÌ"?ÿüÌ"?ð™"?ó1™"?öe™"?ù•™"?üÉ™"?ÿý™"?ðe"?ó2e"?öfe"?ù–e"?üÊe"?ÿþe"?ð2"?ó32"?ög2"?ù—2"?üË2"?ÿÿ2"?ðÿ"?ó3ÿ"?ögÿ"?ù—ÿ"?üËÿ"?ÿÿÿ	
Û€áÈ¯%%%% %%%% %%%% 	%W!#/3333/$!%%%%%%%% %%%%%%%#&(ÿ3$"%%%%%%% %%%%%%#'Tÿÿÿÿÿÿÿ0$%%%%%% %%%%%#3T	ÿÿÿÿ-$"%%%%% %%%%!(ÿÿ3#%%%%% %%%%#0ÿÿ0$"%%%% %%%%Tÿÿÿÿ3"%%%% %%%%0ÿÿÿÿ(!%%%% %%%%3ÿÿÿÿ%%%% %%%%3ÿÿÿÿÿÿ%%%%% %%%%ÿÿÿÿ0%%%% %%%%/0ÿÿÿÿ2%%%% %%%%$ÿÿÿÿ%%%% %%%%"ÿÿT%%%% %%%%%#ÿÿ#6%%%% %%%%%"#	ÿGÿÿÿ6%%%%% %%%%%%"30ÿÿÿÿÿÿÿT"6%%%%%% %%%%%%%!3(ÿ026%%%%%%% 	%%!!%%%%%%%% %%%% %%%% %%%%             "?ÿÿÿ"€`")ZV•"5­kZ"7­ëz"9î{ž"-hF" € "9æ5"%¤"'  "/`  "#€@"5¡€ "?ñ€ "1©ÆQ"Ñ  "%€ "/b "+b "ƒ¤"Ñ€ "9êÚQ"#5H"€ "#¥"A€ "/hÅÍ"7©Åí"! ä"+fµ("% „"1¥ @"+hE­"'5i"%µH"#5("5©E­"“`"Ó`"B@")V4Ä"3§´ä"?ùE"=èÄä"‚@";èDÄ"Õ¥"A€@"?úUH"C¤"Ô Ä"-g´ä"+e¤ "?ýj•"7©Å("7§´ "?øÄ ";èÄ "?üiÍ"?ûY"?ûÙH"-hD¤"'4 "9ëYH"!4„")W´¤"1©DÄ"Ö5H"5¬ê•"1«Ú1"1«Ùí"5«Ø "?ýè "5¬éí"!´¤"?ÿzq"?ÿzQ"7­ií"9íéí"/kÚÖ"?ÿÿ¾")ZVQ"„¡"
B„"…!"?ÿÿ:"×5i"3¬êq"„ Ä"'ÅÍ"-kZ"-kYí"7­êQ";îúq"'Å"?ÿþq")ZU"?ÿþQ"×5"+jÙ"%EH"%DÄ"•¤`"?ÿü "9îú¶";ï{"5­ê¶"7®z¶"ÙÄ "3®zQ"/nz"%é")^yí"… ä"è "ØEH"×µ"ÛYH"%éí"%éÍ"×µi"'ê"†µ("ÛY("7¯ÿZ"
Mè "ŠÙ"IÄÄ"1¬ë"#F1"ÙÅí"—5i"†5("D Ä"…¥"—µi"!Z"ÙE­" ´ " D "ŠÙi"ˆÅH"ÛZ"3¯ÿz"3­ëZ"/më:" 	ÅH"ÙF"
Oþ¶"1­ëZ" þ¶"…¥i"#Æq"
CÄ"Oÿÿ"
Oÿÿ"‡5í"#ÚÖ"ÙF•"˜Æq"†µí"
KÛÿ"ÙÆÖ"ØFq"…¥­"‡61"%Ûz"F¶q"1¬ëZ"ÙFÖ"ÙÇ"ÙFö"ˆFö"G6¶"'Ûž"!W:"‡¶¶" "Q"ˆÇ¾"G·z"7ÿ"HGÿ" í" ¢Ö"
C‘H"‰Gÿ" " §ž"D!"
C"'Û¾"×6¶"ØÇZ"ØG:"…¦" Í"
C’¶"+k[Þ"…§ÿ"ÖµÍ"%F¶"!Fq"!Fö"Ö7:"B‘i"„¡Í"„!­"A("
@¶"ƒÿ"Õ¦•"+iÇ:")YG"'ÆÖ"B("+iG"ƒ‘("+c“ÿ"!¶Q"+`ÿ")W¶¶"-jÚÖ"'Fq"+hÆ¶"?õ§ÿ"€H"+g6•"-hF¶"Ó‘i"“‘H")V6Q"7ªW:"5¥¦¶"Ô¡i"?ñ‚¶"1©F•"1ªÚÖ"3©F•"+hÆQ"?ùÆÖ"?û["?øÆ•"?úÚö"1©Fq"9ç6"/e!"+d!H"3©Fq"?÷¶1"?õ¥"7¤¡H"/cä"7£‘"?ô¡H
  D0   €äá ä  ÿ  c  R”  kZ  o{  sœ  Z  !  qŒ  H¥  L   \   Db  h`  |`  br  0@  H`  \¡  T¡  $Å  0`  r²  EŠ  (`  Eh  a  ^.  no  AG  U©  I$  aB  V  MË  Iª  E‰  jM  ,Ã  4ã  ‚  Q†  eç  ~H  z'  $¢  v  9h  b  ~Š  Å  1&  Yç  U`  T  ni  mà  ~   v     ~È  ~ê  Z  M€  rÊ  A„  Qå  bF  5Š  k4  bñ  bï  jà  `  k/  Aå  Ó  Ò  oO  so  ^ö  ý  R’  %(  ¤  )H  ù  9Ë  g3  !  Nn  ZÐ  ZÏ  or  w³  Nl  ó  RŒ  ò  9È  V¬  JJ  JF  -c  à  sµ  wØ  ku  o•  >`  g’  _  K,  S  !G  +`  2
  5ì  :Ê  K/  Kn  1ë  Op  )©  2É  oú  `  *¨  f  c8  FQ  >o  -Ë  %‰    !h  -ë  BÐ  6M  à     *«  &*  :Ð  gû  gz  _y  j  6P  õ  cz  õ  )k  Fs  Æ  ÿ  ÿ  %Ï  F¶  2T  .3  %¯  ÿ  :v  2  !m  )Ñ  Jû  ³  c:  :V  :x  6W  &  Õ  Nü  B™  )õ    "=  û  ß     Ï  6  ê  *_   Ð  |    È  N½  1Õ  >:  :  )p   N  õ  VÞ  )  5®  JU  B  B  1™  «  ).  %  i    (  9t  Vy  RX  N6  ‰  VX   é  Tÿ  A²  T  Qõ  Z¶  NS  V5  }  (
  UÔ  Z  0ë  ,ê  Q’  n™  iu  1+  |u  bT  b¶  fT  V2  ~v  ~Ø  ~4  ~·  bS  qÐ  ]L  U
  fS  }ñ  }l  m*  \Ç  lè  }*   	
Û  •¯:::+	:::+:	:::: +:::	:+:::
::+:::: 
::+:3:+::::::+::::::::+: ::::+:::::::::+:::::+:::::: 	:â%-4>//>4.3C :::+::: :+:::::"/4[4.3::::::: :::+::-&O[83+:::+: +::::-&O[83::::: ::::C"4-::::: :+::6"73::+: ::::4O[!:::: ::::"d+:::: ::+:> :::+ ::::" :::: ::::"d :::: ::::4d:::: :+::-[)::+: ::::3Od:::: ::::.[L?::::: +::::3.LE::::+ ::::::3[ddLE:::::: :::::::+!67ddddL?E::::+:: :+:::::::+% ? ::+::::: ::::+::::::+ ::+::::+	::+:::::+:::             
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  brush:action:nib:   
 #²brush6:³  ‰dá‡  €      ÿ     ÿÿÀ    ÿÿà    ÿÿø    ?ÿÿþ    ÿÿÿ    ÿÿÿ    ÿÿÿÿ€  ÿÿÿÿà  ÿÿÿÿð  ÿÿÿÿð  ÿÿÿÿø  ÿÿÿÿø  ÿÿÿÿü  ?ÿÿÿÿþ  ?ÿÿÿÿþ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ€ ÿÿÿÿÿÿ€ ÿÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ÿÿÿÿÿ  ?ÿÿÿÿþ  ?ÿÿÿÿþ  ÿÿÿÿü  ÿÿÿÿø  ÿÿÿÿø  ÿÿÿÿð  ÿÿÿÿð  ÿÿÿÿà   ÿÿÿÿ€   ÿÿÿ    ÿÿÿ    ?ÿÿþ    ÿÿø    ÿÿà    ÿÿÀ     ÿ       À    2   2   †ÿÿÿçÿÿÿç
  ƒá'Pz(à”
  
  U
  [ Àbrush5:	
Û  ¶¶¶   ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇƒÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇÇœœwwwwx¢¢ÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇ£œwS((MSœœ£ÇÇÇ¶¶   ¶¶ÇÇÇÇÇœqœ¢ÇÇÇ¶¶   ¶¶ÇÇÇÇœwMSœ£ÇÇ¶¶   ¶¶ÇÇÇÇ¢S	œ¢ÇÇ¶¶   ¶¶ÇÇÇÇœ	£ÇÇ¶¶   ¶¶ÇÇÇÇw(	LÇÇÇ¶¶   ¶¶ÇÇÇÇwÇÇÇ¶¶   ¶¶ÇÇÇÇœ(	LÇÇÇ¶¶   ¶¶ÇÇÇÇxM	SÇÇÇ¶¶   ¶¶ÇÇÇÇ¢w	ƒ£ñÇÇ¶¶   ¶¶ÇÇÇÇ¢œwñÇÇÇ¶¶   ¶¶ÇÇÇÇÇ¢œSxññÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ£¢œSLLS£òñÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇÇÇ£ÇÇÇÇññÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶   ¶¶               
 %Ü
  D0
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 'Ž
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×	
Û€áº¶%%   %%   %%   %%   %%   	%s$3333/#"%%%%%%%   %%%%%%%!$3ÿ(!%%%%%   %%%%%%%$'TÿÿÿÿÿÿÿT$"%%%%%   %%%%%%3(ÿÿÿÿÿÿÿÿÿ!%%%%   %%%%%%	ÿÿÿÿ"%%%%   %%%%%%	ÿÿÿÿT!%%%%   %%%%%%3	ÿÿÿÿ0%%%%   %%%%%%3ÿÿ%%   %%%%%%	ÿÿÿÿ0%%%%   %%%%%%/(	ÿÿÿÿY%%%%   %%%%%%#3	ÿGÿÿÿ!%%%%   %%%%%%"Tÿÿÿÿÿÿÿÿÿ3%%%%   %%%%%%%"ÿÿÿÿÿÿÿ/%%%%%   	%!"20ÿ0Y!6%%%%%%   	%%%!%%%%%%%%   %%   %%   %%   %%   %%   %%               
 2É
 2Ô
 2ß
 2ê
 2õ
 3 
 3
 3
 3!
 3,
 37
 3B
 3M
 3X
 3c
 3n
 3y
 3„
 3
 3š
 3¥
 3°
 3»
 3Æ
 3Ñ
 3Ü
 3ç
 3ò
 3ý
 4
 4
 4
 4)
 44
 4?
 4J
 4U
 4`
 4k
 4v
 4
 4Œ
 4—
 4¢
 4­
 4¸
 4Ã
 4Î
 4Ù
 4ä
 4ï
 4ú
 5
 5
 5
 5&
 51
 5<
 5G
 5R
 5]
 5h
 5s
 5~
 5‰
 5”
 5Ÿ
 5ª
 5µ
 5À
 5Ë
 5Ö
 5á
 5ì
 5÷
 6
 6
 6
 6#
 6.
 69
 6D
 6O
 6Z
 6e
 6p
 6{
 6†
 6‘
 6œ
 6§
 6²
 6½
 6È
 6Ó
 6Þ
 6é
 6ô
 6ÿ
 7

 7
 7 
 7+
 76
 7A
 7L
 7W
 7b
 7m
 7x
 7ƒ
 7Ž
 7™
 7¤
 7¯
 7º
 7Å
 7Ð
 7Û
 7æ
 7ñ
 7ü
 8
 8
 8
 8(
 83
 8>
 8I
 8T
 8_
 8j
 8u
 8€
 8‹
 8–
 8¡
 8¬
 8·
 8Â
 8Í
 8Ø
 8ã
 8î
 8ù
 9
 9
 9
 9%
 90
 9;
 9F
 9Q
 9\
 9g
 9r
 9}
 9ˆ
 9“
 9ž
 9©
 9´
 9¿
 9Ê
 9Õ
 9à
 9ë
 9ö
 :
 :
 :
 :"
 :-
 :8
 :C
 :N
 :Y
 :d
 :o
 :z
 :…
 :
 :›
 :¦
 :±
 :¼
 :Ç
 :Ò
 :Ý
 :è
 :ó
 :þ
 ;	
 ;
 ;
 ;*
 ;5
 ;@
 ;K
 ;V
 ;a
 ;l
 ;w
 ;‚
 ;
 ;˜
 ;£
 ;®
 ;¹
 ;Ä
 ;Ï
 ;Ú
 ;å
 ;ð
 ;û
 <
 <
 <
 <'
 <2
 <=
 <H
 <S
 <^
 <i
 <t
 <
 <Š
 <•
 < 
 <«
 <¶
 <Á
 <Ì
 <×
 <â
 <í
 <ø
 =
 =
 =
 =$
 =/
 =:
 =E
 =P
 =[
 =f
 =q
 =|
 =‡
 =’
 =
 =¨
 =³
  D0   €äá ä  ÿ  c  R”  kZ  o{  sœ  Z  !  qŒ  H¥  L   \   Db  h`  |`  br  0@  H`  \¡  T¡  $Å  0`  r²  EŠ  (`  Eh  a  ^.  no  AG  U©  I$  aB  V  MË  Iª  E‰  jM  ,Ã  4ã  ‚  Q†  eç  ~H  z'  $¢  v  9h  b  ~Š  Å  1&  Yç  U`  T  ni  mà  ~   v     ~È  ~ê  Z  M€  rÊ  A„  Qå  bF  5Š  k4  bñ  bï  jà  `  k/  Aå  Ó  Ò  oO  so  ^ö  ý  R’  %(  ¤  )H  ù  9Ë  g3  !  Nn  ZÐ  ZÏ  or  w³  Nl  ó  RŒ  ò  9È  V¬  JJ  JF  -c  à  sµ  wØ  ku  o•  >`  g’  _  K,  S  !G  +`  2
  5ì  :Ê  K/  Kn  1ë  Op  )©  2É  oú  `  *¨  f  c8  FQ  >o  -Ë  %‰    !h  -ë  BÐ  6M  à     *«  &*  :Ð  gû  gz  _y  j  6P  õ  cz  õ  )k  Fs  Æ  ÿ  ÿ  %Ï  F¶  2T  .3  %¯  ÿ  :v  2  !m  )Ñ  Jû  ³  c:  :V  :x  6W  &  Õ  Nü  B™  )õ    "=  û  ß     Ï  6  ê  *_   Ð  |    È  N½  1Õ  >:  :  )p   N  õ  VÞ  )  5®  JU  B  B  1™  «  ).  %  i    (  9t  Vy  RX  N6  ‰  VX   é  Tÿ  A²  T  Qõ  Z¶  NS  V5  }  (
  UÔ  Z  0ë  ,ê  Q’  n™  iu  1+  |u  bT  b¶  fT  V2  ~v  ~Ø  ~4  ~·  bS  qÐ  ]L  U
  fS  }ñ  }l  m*  \Ç  lè  }*   	
Û  ‡¶:+::+::+::+::+   +::+:::::+::::   ::::::+:::::::+:	:+:   ::+::::::::+::::+::+:::+:   :::::::+::   +:::
:+::àã::+::::::+:::   ::+::::::38"/>>4-3+:::::+   :::::::C/4[44::::::   +:::+::/O[8!:::::   ::::::!/-%::+:   ::+:::8473::::   ::::::"Od+::::   :::+::"d ::+:   :+::::"	³ ::::   ::::::"d ::::   :::+::4d::+:   +:::::-8::::   ::::::C4[+?2::::   :+::+::!doH::+::   :::::::+3!6ddd†Lü?::::::   	:#:C+ ?2::::::+   ::+::+::: ::	::+:::   ::   ::::
+::::3:+::+::::   :+:::::::::+::+::::::::+:   ::::::+:	:::+::::::               
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
 I   
 Kibrush5:³   Q    ÿ€ ÿà ÿð ÿø 
ÿü ?ÿþ ÿÿ ÿÿÿ€ÿÿ 
?ÿþ ÿü ÿø ÿð ÿÀ  ÿ€                 †ÿÿÿóÿÿÿó
  ƒá%à{'0“
  
  U
  [ Àbrush4:	
Û  ­¶¶   ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇáÇÇÇ¶¶   ¶¶ÇÇÇ£œxwwwœ¢£ÇÇÇÇÇ¶¶   ¶¶ÇÇ£œSM(LœœÇÇÇÇÇ¶¶   ¶¶ÇÇ¢qœ¢ÇÇÇÇ¶¶   ¶¶ÇÇœS¢ÇÇÇÇ¶¶   ¶¶ÇÇwLL£ÇÇÇÇ¶¶   ¶¶ÇÇwÇÇÇÇÇ¶¶   ¶¶ÇÇœLÇÇÇÇÇ¶¶   ¶¶ÇÇœ¢ñÇÇÇÇ¶¶   ¶¶ÇÇ¢œMSñÇÇÇÇÇ¶¶   ¶¶ÇÇÇ¢¢SM¢ññÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇ£¢ÇÇñññÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶ÇÇÇÇÇÇ¶¶   ¶¶   ¶¶               
 %Ü
  D0
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 'Ž
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×	
Û€áe%%   %%   %%   %%   %%   %á%   %%%%%!$/333$"!%%%%%%%   %%%%!(ÿ0T%%%%%%%   %%%%#'ÿÿÿÿÿ$"%%%%%%   %%%%Tÿÿÿÿÿÿÿ"%%%%%%   %%%%30ÿÿÿÿÿÿÿ0!%%%%%%   %%%%3ÿÿÿÿÿÿÿÿÿ%%%%%%%   %%%%0ÿÿÿÿÿÿÿT%%%%%%   %%%%$Tÿÿÿÿÿÿÿ%%%%%%   %%%%"(ÿÿÿÿÿY%%%%%%   %%%%%"#Tÿ(%%%%%%%   %%%%%%!"%%%%%%%%   %%   %%   %%   %%   %%   %%   %%               
 2É
 2Ô
 2ß
 2ê
 2õ
 3 
 3
 3
 3!
 3,
 37
 3B
 3M
 3X
 3c
 3n
 3y
 3„
 3
 3š
 3¥
 3°
 3»
 3Æ
 3Ñ
 3Ü
 3ç
 3ò
 3ý
 4
 4
 4
 4)
 44
 4?
 4J
 4U
 4`
 4k
 4v
 4
 4Œ
 4—
 4¢
 4­
 4¸
 4Ã
 4Î
 4Ù
 4ä
 4ï
 4ú
 5
 5
 5
 5&
 51
 5<
 5G
 5R
 5]
 5h
 5s
 5~
 5‰
 5”
 5Ÿ
 5ª
 5µ
 5À
 5Ë
 5Ö
 5á
 5ì
 5÷
 6
 6
 6
 6#
 6.
 69
 6D
 6O
 6Z
 6e
 6p
 6{
 6†
 6‘
 6œ
 6§
 6²
 6½
 6È
 6Ó
 6Þ
 6é
 6ô
 6ÿ
 7

 7
 7 
 7+
 76
 7A
 7L
 7W
 7b
 7m
 7x
 7ƒ
 7Ž
 7™
 7¤
 7¯
 7º
 7Å
 7Ð
 7Û
 7æ
 7ñ
 7ü
 8
 8
 8
 8(
 83
 8>
 8I
 8T
 8_
 8j
 8u
 8€
 8‹
 8–
 8¡
 8¬
 8·
 8Â
 8Í
 8Ø
 8ã
 8î
 8ù
 9
 9
 9
 9%
 90
 9;
 9F
 9Q
 9\
 9g
 9r
 9}
 9ˆ
 9“
 9ž
 9©
 9´
 9¿
 9Ê
 9Õ
 9à
 9ë
 9ö
 :
 :
 :
 :"
 :-
 :8
 :C
 :N
 :Y
 :d
 :o
 :z
 :…
 :
 :›
 :¦
 :±
 :¼
 :Ç
 :Ò
 :Ý
 :è
 :ó
 :þ
 ;	
 ;
 ;
 ;*
 ;5
 ;@
 ;K
 ;V
 ;a
 ;l
 ;w
 ;‚
 ;
 ;˜
 ;£
 ;®
 ;¹
 ;Ä
 ;Ï
 ;Ú
 ;å
 ;ð
 ;û
 <
 <
 <
 <'
 <2
 <=
 <H
 <S
 <^
 <i
 <t
 <
 <Š
 <•
 < 
 <«
 <¶
 <Á
 <Ì
 <×
 <â
 <í
 <ø
 =
 =
 =
 =$
 =/
 =:
 =E
 =P
 =[
 =f
 =q
 =|
 =‡
 =’
 =
 =¨
 =³
  D0   €äá ä  ÿ  c  R”  kZ  o{  sœ  Z  !  qŒ  H¥  L   \   Db  h`  |`  br  0@  H`  \¡  T¡  $Å  0`  r²  EŠ  (`  Eh  a  ^.  no  AG  U©  I$  aB  V  MË  Iª  E‰  jM  ,Ã  4ã  ‚  Q†  eç  ~H  z'  $¢  v  9h  b  ~Š  Å  1&  Yç  U`  T  ni  mà  ~   v     ~È  ~ê  Z  M€  rÊ  A„  Qå  bF  5Š  k4  bñ  bï  jà  `  k/  Aå  Ó  Ò  oO  so  ^ö  ý  R’  %(  ¤  )H  ù  9Ë  g3  !  Nn  ZÐ  ZÏ  or  w³  Nl  ó  RŒ  ò  9È  V¬  JJ  JF  -c  à  sµ  wØ  ku  o•  >`  g’  _  K,  S  !G  +`  2
  5ì  :Ê  K/  Kn  1ë  Op  )©  2É  oú  `  *¨  f  c8  FQ  >o  -Ë  %‰    !h  -ë  BÐ  6M  à     *«  &*  :Ð  gû  gz  _y  j  6P  õ  cz  õ  )k  Fs  Æ  ÿ  ÿ  %Ï  F¶  2T  .3  %¯  ÿ  :v  2  !m  )Ñ  Jû  ³  c:  :V  :x  6W  &  Õ  Nü  B™  )õ    "=  û  ß     Ï  6  ê  *_   Ð  |    È  N½  1Õ  >:  :  )p   N  õ  VÞ  )  5®  JU  B  B  1™  «  ).  %  i    (  9t  Vy  RX  N6  ‰  VX   é  Tÿ  A²  T  Qõ  Z¶  NS  V5  }  (
  UÔ  Z  0ë  ,ê  Q’  n™  iu  1+  |u  bT  b¶  fT  V2  ~v  ~Ø  ~4  ~·  bS  qÐ  ]L  U
  fS  }ñ  }l  m*  \Ç  lè  }*   	
Û  ç::   ::+:
:+::+   ::::+:::::   :::+::::+:::	::   
:+::::+::+:::::+:   :+::::   :::::::+:á:   +:+:+,">/"8+:::+::+   :::::8"4[4:+:::::   ::::-jO73::::+:   :+::"O[!::::::   ::::>d%::+:::   ::::" ::::::   +:::"d:::+::   ::::.[)?::::::   ::+:!8†::+:::   ::: C,,[ddL??::::::+   :::: :+% ? :::::::   :+::::   ::+::G:::+:   :::::::+::::::+::::::   ::::+::::::+:::::+:::   :+:::::::+::	:+   ::::::+::::::+::+::::               
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
 I   
 eçbrush4:³   ,   €  
?à  
ð  ÿø  
ð  
?à  €              †ÿÿÿúÿÿÿú
  ƒá'Pi(Ð~
  
  U
  [ Àbrush2:	
Û   »~¶¶¶ÇÇ¶
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇÇÇ¢wœx¢ÇÇÇÇÇ
¶¶ÇÇÇÇÇœ(SœÇÇÇÇÇ
¶¶ÇÇÇÇÇx(LÇÇÇÇÇÇ
¶¶ÇÇÇÇÇœSL¢ñÇÇÇÇÇ
¶¶ÇÇÇÇÇ¢¢ÇòÇÇÇÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ¶¶ÇÇ¶¶¶ÇÇ¶¶¶ÇÇaÇ            
 %Ü
  D0
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 'Ž
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×	
Û€:~É%%%#3/"%%%%T%%%/ÿ0%%%%$0%%%%""6%É%            
 2É
 2Ô
 2ß
 2ê
 2õ
 3 
 3
 3
 3!
 3,
 37
 3B
 3M
 3X
 3c
 3n
 3y
 3„
 3
 3š
 3¥
 3°
 3»
 3Æ
 3Ñ
 3Ü
 3ç
 3ò
 3ý
 4
 4
 4
 4)
 44
 4?
 4J
 4U
 4`
 4k
 4v
 4
 4Œ
 4—
 4¢
 4­
 4¸
 4Ã
 4Î
 4Ù
 4ä
 4ï
 4ú
 5
 5
 5
 5&
 51
 5<
 5G
 5R
 5]
 5h
 5s
 5~
 5‰
 5”
 5Ÿ
 5ª
 5µ
 5À
 5Ë
 5Ö
 5á
 5ì
 5÷
 6
 6
 6
 6#
 6.
 69
 6D
 6O
 6Z
 6e
 6p
 6{
 6†
 6‘
 6œ
 6§
 6²
 6½
 6È
 6Ó
 6Þ
 6é
 6ô
 6ÿ
 7

 7
 7 
 7+
 76
 7A
 7L
 7W
 7b
 7m
 7x
 7ƒ
 7Ž
 7™
 7¤
 7¯
 7º
 7Å
 7Ð
 7Û
 7æ
 7ñ
 7ü
 8
 8
 8
 8(
 83
 8>
 8I
 8T
 8_
 8j
 8u
 8€
 8‹
 8–
 8¡
 8¬
 8·
 8Â
 8Í
 8Ø
 8ã
 8î
 8ù
 9
 9
 9
 9%
 90
 9;
 9F
 9Q
 9\
 9g
 9r
 9}
 9ˆ
 9“
 9ž
 9©
 9´
 9¿
 9Ê
 9Õ
 9à
 9ë
 9ö
 :
 :
 :
 :"
 :-
 :8
 :C
 :N
 :Y
 :d
 :o
 :z
 :…
 :
 :›
 :¦
 :±
 :¼
 :Ç
 :Ò
 :Ý
 :è
 :ó
 :þ
 ;	
 ;
 ;
 ;*
 ;5
 ;@
 ;K
 ;V
 ;a
 ;l
 ;w
 ;‚
 ;
 ;˜
 ;£
 ;®
 ;¹
 ;Ä
 ;Ï
 ;Ú
 ;å
 ;ð
 ;û
 <
 <
 <
 <'
 <2
 <=
 <H
 <S
 <^
 <i
 <t
 <
 <Š
 <•
 < 
 <«
 <¶
 <Á
 <Ì
 <×
 <â
 <í
 <ø
 =
 =
 =
 =$
 =/
 =:
 =E
 =P
 =[
 =f
 =q
 =|
 =‡
 =’
 =
 =¨
 =³
  D0   €äá ä  ÿ  c  R”  kZ  o{  sœ  Z  !  qŒ  H¥  L   \   Db  h`  |`  br  0@  H`  \¡  T¡  $Å  0`  r²  EŠ  (`  Eh  a  ^.  no  AG  U©  I$  aB  V  MË  Iª  E‰  jM  ,Ã  4ã  ‚  Q†  eç  ~H  z'  $¢  v  9h  b  ~Š  Å  1&  Yç  U`  T  ni  mà  ~   v     ~È  ~ê  Z  M€  rÊ  A„  Qå  bF  5Š  k4  bñ  bï  jà  `  k/  Aå  Ó  Ò  oO  so  ^ö  ý  R’  %(  ¤  )H  ù  9Ë  g3  !  Nn  ZÐ  ZÏ  or  w³  Nl  ó  RŒ  ò  9È  V¬  JJ  JF  -c  à  sµ  wØ  ku  o•  >`  g’  _  K,  S  !G  +`  2
  5ì  :Ê  K/  Kn  1ë  Op  )©  2É  oú  `  *¨  f  c8  FQ  >o  -Ë  %‰    !h  -ë  BÐ  6M  à     *«  &*  :Ð  gû  gz  _y  j  6P  õ  cz  õ  )k  Fs  Æ  ÿ  ÿ  %Ï  F¶  2T  .3  %¯  ÿ  :v  2  !m  )Ñ  Jû  ³  c:  :V  :x  6W  &  Õ  Nü  B™  )õ    "=  û  ß     Ï  6  ê  *_   Ð  |    È  N½  1Õ  >:  :  )p   N  õ  VÞ  )  5®  JU  B  B  1™  «  ).  %  i    (  9t  Vy  RX  N6  ‰  VX   é  Tÿ  A²  T  Qõ  Z¶  NS  V5  }  (
  UÔ  Z  0ë  ,ê  Q’  n™  iu  1+  |u  bT  b¶  fT  V2  ~v  ~Ø  ~4  ~·  bS  qÐ  ]L  U
  fS  }ñ  }l  m*  \Ç  lè  }*   	
Û  5~1:+:::::::::+::+::+::::
:+::+:::::+::+::+:::+:::::+::::::::+:	::::+::::+::+	:::+::::::::+	:+:-""83::+::	:+:::::"O[-:::::+:::
::+:::" :::::::+:	:::8dL?:::+::+:3,5E:+::::+:::+:+:+::::+:::::+:::+:::::+:::
::+::+::+::::+::+::+::+::+::+:::::+:::::+:::::::+:	:::+::::::::+::::+::+:::+            
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
 I   
 ~õbrush2:³€@   à   @            †ÿÿÿÿÿÿÿÿ
  ƒá%àj' ~
  
  U
  [ Àbrush1:	
Û   ¢d¶¶¶ÇÇ¶
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇÇÇwwœÇÇÇÇÇÇ
¶¶ÇÇÇÇÇxÇÇÇÇÇÇÇ
¶¶ÇÇÇÇÇœÇòÇÇÇÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ
¶¶ÇÇÇ¶¶ÇÇ¶¶¶ÇÇ¶¶¶ÇÇ)Ç            
 %Ü
  D0
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 'Ž
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×	
Û€$d¥%%%%33$%%%%%%/ÿ%%%%%%$6%%½%            
 2É
 2Ô
 2ß
 2ê
 2õ
 3 
 3
 3
 3!
 3,
 37
 3B
 3M
 3X
 3c
 3n
 3y
 3„
 3
 3š
 3¥
 3°
 3»
 3Æ
 3Ñ
 3Ü
 3ç
 3ò
 3ý
 4
 4
 4
 4)
 44
 4?
 4J
 4U
 4`
 4k
 4v
 4
 4Œ
 4—
 4¢
 4­
 4¸
 4Ã
 4Î
 4Ù
 4ä
 4ï
 4ú
 5
 5
 5
 5&
 51
 5<
 5G
 5R
 5]
 5h
 5s
 5~
 5‰
 5”
 5Ÿ
 5ª
 5µ
 5À
 5Ë
 5Ö
 5á
 5ì
 5÷
 6
 6
 6
 6#
 6.
 69
 6D
 6O
 6Z
 6e
 6p
 6{
 6†
 6‘
 6œ
 6§
 6²
 6½
 6È
 6Ó
 6Þ
 6é
 6ô
 6ÿ
 7

 7
 7 
 7+
 76
 7A
 7L
 7W
 7b
 7m
 7x
 7ƒ
 7Ž
 7™
 7¤
 7¯
 7º
 7Å
 7Ð
 7Û
 7æ
 7ñ
 7ü
 8
 8
 8
 8(
 83
 8>
 8I
 8T
 8_
 8j
 8u
 8€
 8‹
 8–
 8¡
 8¬
 8·
 8Â
 8Í
 8Ø
 8ã
 8î
 8ù
 9
 9
 9
 9%
 90
 9;
 9F
 9Q
 9\
 9g
 9r
 9}
 9ˆ
 9“
 9ž
 9©
 9´
 9¿
 9Ê
 9Õ
 9à
 9ë
 9ö
 :
 :
 :
 :"
 :-
 :8
 :C
 :N
 :Y
 :d
 :o
 :z
 :…
 :
 :›
 :¦
 :±
 :¼
 :Ç
 :Ò
 :Ý
 :è
 :ó
 :þ
 ;	
 ;
 ;
 ;*
 ;5
 ;@
 ;K
 ;V
 ;a
 ;l
 ;w
 ;‚
 ;
 ;˜
 ;£
 ;®
 ;¹
 ;Ä
 ;Ï
 ;Ú
 ;å
 ;ð
 ;û
 <
 <
 <
 <'
 <2
 <=
 <H
 <S
 <^
 <i
 <t
 <
 <Š
 <•
 < 
 <«
 <¶
 <Á
 <Ì
 <×
 <â
 <í
 <ø
 =
 =
 =
 =$
 =/
 =:
 =E
 =P
 =[
 =f
 =q
 =|
 =‡
 =’
 =
 =¨
 =³
  D0   €äá ä  ÿ  c  R”  kZ  o{  sœ  Z  !  qŒ  H¥  L   \   Db  h`  |`  br  0@  H`  \¡  T¡  $Å  0`  r²  EŠ  (`  Eh  a  ^.  no  AG  U©  I$  aB  V  MË  Iª  E‰  jM  ,Ã  4ã  ‚  Q†  eç  ~H  z'  $¢  v  9h  b  ~Š  Å  1&  Yç  U`  T  ni  mà  ~   v     ~È  ~ê  Z  M€  rÊ  A„  Qå  bF  5Š  k4  bñ  bï  jà  `  k/  Aå  Ó  Ò  oO  so  ^ö  ý  R’  %(  ¤  )H  ù  9Ë  g3  !  Nn  ZÐ  ZÏ  or  w³  Nl  ó  RŒ  ò  9È  V¬  JJ  JF  -c  à  sµ  wØ  ku  o•  >`  g’  _  K,  S  !G  +`  2
  5ì  :Ê  K/  Kn  1ë  Op  )©  2É  oú  `  *¨  f  c8  FQ  >o  -Ë  %‰    !h  -ë  BÐ  6M  à     *«  &*  :Ð  gû  gz  _y  j  6P  õ  cz  õ  )k  Fs  Æ  ÿ  ÿ  %Ï  F¶  2T  .3  %¯  ÿ  :v  2  !m  )Ñ  Jû  ³  c:  :V  :x  6W  &  Õ  Nü  B™  )õ    "=  û  ß     Ï  6  ê  *_   Ð  |    È  N½  1Õ  >:  :  )p   N  õ  VÞ  )  5®  JU  B  B  1™  «  ).  %  i    (  9t  Vy  RX  N6  ‰  VX   é  Tÿ  A²  T  Qõ  Z¶  NS  V5  }  (
  UÔ  Z  0ë  ,ê  Q’  n™  iu  1+  |u  bT  b¶  fT  V2  ~v  ~Ø  ~4  ~·  bS  qÐ  ]L  U
  fS  }ñ  }l  m*  \Ç  lè  }*   	
Û   Íd%::+::::+::+:::+::+:::+::::+:
:+::+:::+::/"8:::/:::" :+::+::::+::+::::.?::::::+::::::::+:::+:::	:+::::+:::::+:::::+:::::+	:::+:::+:::::::::+:::::+::::::
+:::::+::::+:
:+::::::+::::            
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
 I   
 •brush1:³€€            †        
  ƒá(`à*Ð÷
  
  U
  [ Àtoss:	
Û  €àæåßßåßßåßßßßßßËLc{{{qp{LL…ðââ	ßßßß 	ßâ_ßßëlq\qqqq]tttttptpp„âèâßßßßßåß ßåßßåßßËl\\\\mqq]]tqtttptpxpvpoìèâßßßßß ßßßßßål\\\\q]]]tttpttptttxppxpvpoìãâßßß ßßßßëm\^y\]]tttttxxxxxxpxpxpvpvvvr„ãèâß ßßßë\^y\]]ttZnDDZxtxtxxxxpxxpvpvvor†ããâ ßß…\\\\]ttxx[eÿZ[xxxpxxxxxpxxxvvvrvrdãã âí{\|\]ttxxxxrÿxxx4Rxxv4vxxIIvxxvvrrrØÛ è…qq]]xtxxxxxIÿxvZwwrvnwwv[ÿÿZpvvvrrrÍØ ãLt]tttxpxxxxrÿxrÿxZD[nx[vw[v[xxvvvrvÌ ÛLtptxpxxxxxxIÿxInxInrÿZvx/ÿ[xxxxvrrrÌ Û„pxppxpxxxxxrÿx[wx[wxrwnvx/ÿ>vpvvrrrÍ ØìpvvvvxvxtpxIÿxrwx/Dxxvw[xx[wvvvrrrRæÌ Ûãovvvvvpvxxprÿxrw[w&rÿIwrDe/nvvrrRrgÂ ÛããorrrvvvvvxrDxxIwZxxZw&xvDw[vrRrr[eÂÍ ãèèìrvrrrvvvvvxvvxvxxvxvxvvvvvrrrr[ˆÌ èâÀãÛdvrrrrvvGvvvrrrR[rgˆæÌÍ ââââãÛ6rrrrrrrvvvvvvrvrrrRrR’r4ˆæÌØÛ âßßâèãØ[rRr³[rrr[4eÌÌÍØØã ßßßßâèãØÌO[[R[RrrrRrrR[[geeÂÌÍØÛãâ ßßßËßââããØÇÍÌÌÌeeÂeÌÌÍÍØØãèââ ßßßßßßââèãÛØÍÍÌÍÌÌÌÌÌÌÍÍÍØÛããèââß ßßßßßßßâââèèãÛØØÇÍÍÍØØØÛãèèââßßß    '         
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
 ªÂ
  !
  
toss:with:   
 ª’toss:
  ƒá(`±*ÐÈ
  
  U
  [ Àkeep:	
Û  uàæßåßßßßßßßëølq]pLLðââß	ß·åßß ßåßßßßßßßô?PPPP]@MX`XXX2XS)Lèââßßßßßßßß ßßßßßåíHPE\EPPP@`@XXXXXXSXtSS*ìèèËßßßßß ßßßßåHEyyEPP]MXXXXVXXVXSXXSSSSSoèèèßßßß ßåßô\EyEPPMXXVVVVVVVVSVSSSSXSx:Sv†èèâßß ßßôEEyEPM&IV8VV	V#VVSSSSSSYSdãèâß ßíPyE^PXXn>Yw8VVVáƒVSSSTQSdÛãâ ß?^EPMXVVD>.wYVT.VVVYIYVYTYISVSSQSQ+ãã ßXPPMXVVVD[wIVVwwDVYDwwVIDwÿ8VYSSQQQdØã ßXMXXVVVVÿDVV>YÿVWDVw.RÿV>eVVYSYQS[ÍÛ âXXXVVVVVDDÿWVeD>ÿI8w>w/IDVWwTVSYSQQ[Ø âXSSSVVVVD89VD[ISUD>ITIDVWwYVYSQQQ4Í â:SSSSSVVD>WÿY/&VUTIeVIIIÿVIDYYSQQQQÌ âLSvSSSSVD>VD/Iw[ÿVTwIÿWIÿ>D&YYQQQQ4Í âè*SQQYYY&IVIeV9wIVV>w/VID/DISQQQQIeÂØ ââì+QSQQYYYYYYVYVVYVVYVV8nYTQQQRQIÍØ ßâÀ†+QQvQQYYYG>wTQQQRQ4æÌÍØÛ ßßèãØd,QQQQQQTTSTSTSTTQQQQQQQ6IeæÌÍÛã ßßâèãØ6RQQQ³RrQ7[jæÌÍÛãè ßßßââãÛÇgO[76RQQQQQQQR7I4ZeæeÂÌÍØÛèèß ßßßßâèèÛØØÍÍÌÌeeeÂeÂÂÌÌÍØÛãèèâß ßßßßßââèããÛØÍÍÌÌÌÌÌÌÌÍÍØØÛÛèèââßß ßåßßßßßââèããÛØØÇÇÍÍÍÇØØØãèèââßßßß    '         
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
 ³Ä
  !
  
keep:with:   
 ³”keep:
  ƒá(`˜*Ð®
  
  U
  [ Àundo:	
Û  BÜ+åßßåßßßßßâââèèèâððèðèèììÛÛØÛããèèââßßßßß 	ßâ3ßßßíïòòïïïïîîîñííððèìãèâââßßßßß ßßåßßåßßîïõòòòïïïïîîñíñíñííííðèèèââßßßß ßßßßßßîòõõòòïïïîîññíññíñíñíííðíðèèèâßßß ßßßßßïõõòòïïïîîññññññíññíñíñííðððèãèèâß ßßßîòõõòïïîÌìñÛñññññîññèííðíðððððããèâ ßßßïõòòïïîñÿØñnñññññíññìwðññíñððððìÛãè ââîòòòïîñññÿìñwñðíìñññììnðñðìííððìðÛØÛ èâïïïïîñîññÿØñnðænÿOñÿænððnÿñððððìØ èâîïïîññíññÿìñwèÿìnñwdÿíOæìÿìíðììðÍ ãðñññíññññîÿØñnðwðÛnðÿðìwñæ4îwdðððìðÂÍ ÛâíííñííñññÿîwìnñØnðwììnínñwððìðìÂ ØìíííííññíñÿìñnðwðØDíÿììwñæñÿdððìììÂ ÛãðððððííññwÌìÿØìnðnñnÿðnØÿìðìììæ ããèððèðððííØwÿæñìDéìæñØÿææððDÿOèìììØÌÍ Ûèèììððì
ðððññ#íññðññðððìììììeæÌÍ èâèèÛìììììðèðììdÂÂÌÍ èââèãØØìðìììðèðð	ðììììdÌææÌÇÛ âßââèãØìììììdææÂÍØØã ßßßßâèãØÍììì	ìcìØdeææÌÍÍØÛãâ ßßßßââèããØÍÍÌÂeeeˆæˆæææÂÌÌÍÍÍØãèèâ ßßßßßßââèãÛØØÍÍÎÌÌÌÌÌÌÌÌÍÍØÛãèèââß    '         
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
 ¼»
  !
  
undo:with:   
 ¼‹undo:
  ƒá%Ð(`<
  
  U
  [ Àpaint:	
Û  "âO8O8888887776767	66   888.O88[[777777777676#6   88888[77777767676676	6+66161   O888887777777777676666666666,
6,61«6   8888[77777777767676666666661661616161   888777777767666666666616116116111111   888777777777676766666616,16,61611,1,1,   88[777777676666666616,616111	1#1   8[7777777776666666616161111   8	76â{66616116111,1,111111%1%%%   8[7777777667666666616111111111%%%%%1%11%   777777767666661161,1,111111%1111%+1+1+   [7777776666666666116111111+†ìÛØãììð+†+1   77777767666666,11111111%%†ãããããØØØØð++   7777776676666616161,1111%1ìãéââèãÛØØL%%+   7777776666661616111111%1ìèßïßßèãØØØ†++++   7777776766666616111111%1Lè¼ïåßèãØÛÛ*++++   77777666666161611,111%%†ØÛâßÜãÛÛãÛØ*+++++   7777676666661,11111%%+dØdØãÛÛãèèã+*+++++   77776676661611,111%1*ØØÛdÛÛÛã†*++*+*+*   77767666666,611111%oÛdØdÛddì†*+++*+*+**   7776666661161,11%1+)ØdddÛ+*++**+*+**+   7767666616611111%+@PX†dddÛ+*++**+****+*   76766666,6111111+@PEP;oØ++++++	*S*   77666666616,11%1?PEPMV:d+++++**+********   676666611111111PEPM;Xv1)**+***+	*'*   7666666161,11%SPEPMXS:%L++***+**	*#)   6766661611111PEPMX;v+o***+*	*â* )**   7666666,1,115MEPP=S:*r+++*+******* *)*))   66666161611MEPP=XS+r++**+*******)*)))))   7666661611*MEPPMV:*rp++*+*******)* ))*) )   676661611+XEPP@XSv+)++*+*+******)))*)))))   66666611%?PPEM=V5+o)+*+*******) *) ))))))   66661611XPPEMXXS+v)+++*++****)**)))))))5)   766661%X?EPM=V:+o*+++*+*******))) )))55)5   666611PPEP=X5v+++++********)*)))))5))   666665MPEMMVS*%L++++*+*+******)*))))))))   66616 VPP@XS*Qo++++*+*+***** ) ))))555)   66661o1+X;S v)*+++++++*****)*)))))5))5   666661†6+:p+o*1+++++*+*+*****)*)))5))5   666611*d1rr+*1+++++++******)*)))))5)))   66616,1%od+*1++++++*+*+******) ))5))   666616111%11+1+1+++++****** 	)á)   6666,11111%11++++++*+*+****** ))))5)))   66666661111+%11+++++++*****)*)*)))))))   6666111111%11++++++++******* *)))))))))   6661661,111%%1+1++++*++*****))*))))5)))   766661,1111%1+1++++++*+*+*****) ))))5)))   666661611111%1+++++++++*****)**) ))))5)))      )   1      
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  "âO8O8888887776767	66   888.O88[[777777777676#6   88888[77777767676676	6+66161   O888887777777777676666666666,
6,61«6   8888[77777777767676666666661661616161   888777777767666666666616116116111111   888777777777676766666616,16,61611,1,1,   88[777777676666666616,616111	1#1   8[7777777776666666616161111   8	76â{66616116111,1,111111%1%%%   8[7777777667666666616111111111%%%%%1%11%   777777767666661161,1,111111%1111%+1+1+   [7777776666666666116111111+†ìÛØãììð+†+1   77777767666666,11111111%%†ãããããØØØØð++   7777776676666616161,1111%1ìãéââèãÛØØL%%+   7777776666661616111111%1ìèßïßßèãØØØ†++++   7777776766666616111111%1Lè¼ïåßèãØÛÛ*++++   77777666666161611,111%%†ØÛâßÜãÛÛãÛØ*+++++   7777676666661,11111%%+dØdØãÛÛãèèã+*+++++   77776676661611,111%1*ØØÛdÛÛÛã†*++*+*+*   77767666666,611111%oÛdØdÛddì†*+++*+*+**   7776666661161,11%1+)ØdddÛ+*++**+*+**+   7767666616611111%+@PX†dddÛ+*++**+****+*   76766666,6111111+@PEP;oØ++++++	*S*   77666666616,11%1?PEPMV:d+++++**+********   676666611111111PEPM;Xv1)**+***+	*'*   7666666161,11%SPEPMXS:%L++***+**	*#)   6766661611111PEPMX;v+o***+*	*â* )**   7666666,1,115MEPP=S:*r+++*+******* *)*))   66666161611MEPP=XS+r++**+*******)*)))))   7666661611*MEPPMV:*rp++*+*******)* ))*) )   676661611+XEPP@XSv+)++*+*+******)))*)))))   66666611%?PPEM=V5+o)+*+*******) *) ))))))   66661611XPPEMXXS+v)+++*++****)**)))))))5)   766661%X?EPM=V:+o*+++*+*******))) )))55)5   666611PPEP=X5v+++++********)*)))))5))   666665MPEMMVS*%L++++*+*+******)*))))))))   66616 VPP@XS*Qo++++*+*+***** ) ))))555)   66661o1+X;S v)*+++++++*****)*)))))5))5   666661†6+:p+o*1+++++*+*+*****)*)))5))5   666611*d1rr+*1+++++++******)*)))))5)))   66616,1%od+*1++++++*+*+******) ))5))   666616111%11+1+1+++++****** 	)á)   6666,11111%11++++++*+*+****** ))))5)))   66666661111+%11+++++++*****)*)*)))))))   6666111111%11++++++++******* *)))))))))   6661661,111%%1+1++++*++*****))*))))5)))   766661,1111%1+1++++++*+*+*****) ))))5)))   666661611111%1+++++++++*****)**) ))))5)))      )   1      
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ùon
  
  †   
 ÅOpaint:	
Û  þá­úúú)úúúú)úúúú)úúúú)úúúú)úúúú!úúwSúúúúúúú!úwqqMwúúúúúúúúúúxqœœqMSúúúúúúúúxqœœÁÁwMxúúúúúú¢qœœœÁëìœxúúúúúqwœÁÁÇììåÍúúúúúqwœœÁìììåÇúúúúúúúúúxwœœÁåììåÇúúúúúúúúúwwœœÁåììåÇúúúúúúúúúxqœœÁåììåÇúúúúúúúúú¢qœœÁÁìììÇúúúúúúúúúúqœœÁÁëììåúúúúúúúúúúúúúqwœœÁåììåÍúúúúúúúúúúúúúqwœœÁëììåÇ	úúúúúúxqœœÁÁììëÇú	úúúúúqœœÁÁììåÇúú	úúúúúúúúqwœÁÁììåÇúúú	úúúúúúúMœÁÁìììÇúúúúúúSÁììåÇúúúúúú
(SÁìÇúúúúúúúúúú
((¢úúúúúúúúúúúú	úúúYR.úúúú	úúRYYRRRR.((úúúúú	úRRRYY„Rúúúúúú	ú..RRY„¨¯¨Yúúúúúúúúú’ú(.RY„¨¯¨Yúúúúúúúúúú’((.RYY}„}Rúúúúúúúúúúú….((..RRYRYúúúúúúúúú’’h…‹‹`Y.RYúúúúúúúúúú’úú!úúúúúúúúú’úú!úúúúúúúúú’úú!úúúú)ú   ,   '   †           
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá(p*Ð?
  
  U
  [ Àfill:	
Û  ´â6
7677	7788  6#67676777777777[888  161616666#76767777777[8  616,61,61666	66767	77[  1,16166161116166	6#6677777777  1611,116,1661661	6776777777777  1361161666666666767777777  111111,11,111*cêšc*1	6677777777  	1116*ôšŸcê ê*6666666766777777  %%%%%1%111)šŸë*%+*ëƒƒL166666676777777  1%111%1%%+cŸL,11166)ƒƒ…L L+6666677777  +1+%+1%+1ôš)	1ã“ëb¡™ƒ™Ÿo66767777  1+1+1+11šL11%1111)…„  ›™››™™ë6676677  +++1+%+%cš*+%+%1*ëœ¡›¡ƒ ™ƒ™™›L667677  +1+++++*›c1+11+L¡›ƒƒ‡ƒ›bŸ ›™™››L66767  +++++++LaL%++LŸ›™™ƒƒ™™™ƒ„ ››››¡L6667  ++++++%ô™*+*ë›™™›™™™ƒƒƒƒƒË„–šš¡¡ 1676  +++++++fš*L¡™›››™™™ƒƒƒ™ƒb …„ ¡ŸŸ L666  *++++++ašcƒ™¡š›™ƒ‡‡ƒƒƒ™™ƒc…„… Ÿ  …+76  +*+*++)ŸŸ¡¡¡›™‡‡ƒ‡ ƒ¤™›¡bê „†… –……„,6  *+*+** …„„„‹¡ƒ ‡ƒƒƒ™›¡š¡ƒc „„„……………66  *****L„„„††Û„¡‡¹ƒƒ™›šš¡Ÿƒê …Û†……¢„„66  ***+* †††Û†††† ‡ƒ¤¡¡¡Ÿ Ÿc …††Û„…†„,6  ****+…†Û†††††ŒÛ šš¡ŸŸ – ƒ …„††††††„6,  *****…†††††††Û„†–¡ŸŸ– ŸŸc ¢„†††„Û†„66  *****…†††††††„„Ûd     ›ô… „†††††††„,1  ** )*L†††††Û„„†„ –……šcëë„†††Û††„+16   )**)L……¢……„„†‰………… Ÿë…„†††„†„*616  *)) ))……  Ÿ¡Ÿd†……„„……„†Û†††…+1161  )))))*c  Ÿ¡š …¢„†††††„„„…11611,  )) )))LŸ¡šš›„„„††Û††††„L1%11166  ))))) 5cšš™ƒŒ†„Û„†††„…L1%111111  )))))) )¡ƒƒƒÛ…†††„„…*%%1%111,1  5555))))ƒ‡™d¢††„„…+%1+%%11111  )5)5))))ë™™	…„†„…*+++111111,6  5))5))) Ÿ¡…„…L+++1+%+1%1111  ))55))))ô……L+++++1+1%11111  )))5))))Û… )+++++++1%1%11,1  
)))àçd…ŸŸ*++++++1+%+11%111  ))5))))dð{L****++++++1%%1111,  ))5)))d†****+*+*+++1++1+%1111  )555))d)******+++++++1%1111,1  ))))))††)****+*+*++++1++1%1111  )5)))))5L)******++++++++1%%1111,  
)K5)) L†)******+*+++1+1+1%1111  ))))5))))5†d****+*+*+++++%+11%11,1  
))'))) d†)*****++++++++1%+1111,  ))5	)oL*****+*+*++++1+%111%111  ))5)55)))) ))))*****+*+++++++1+%111,1  55)))))))*))*******+*+++++1+1%1111,11  	)#))***)******+++++++1++1%11116  	)# )*)******++*+++++++11%1111,1     &   4      
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  ´â6
7677	7788  6#67676777777777[888  161616666#76767777777[8  616,61,61666	66767	77[  1,16166161116166	6#6677777777  1611,116,1661661	6776777777777  1361161666666666767777777  111111,11,111*cêšc*1	6677777777  	1116*ôšŸcê ê*6666666766777777  %%%%%1%111)šŸë*%+*ëƒƒL166666676777777  1%111%1%%+cŸL,11166)ƒƒ…L L+6666677777  +1+%+1%+1ôš)	1ã“ëb¡™ƒ™Ÿo66767777  1+1+1+11šL11%1111)…„  ›™››™™ë6676677  +++1+%+%cš*+%+%1*ëœ¡›¡ƒ ™ƒ™™›L667677  +1+++++*›c1+11+L¡›ƒƒ‡ƒ›bŸ ›™™››L66767  +++++++LaL%++LŸ›™™ƒƒ™™™ƒ„ ››››¡L6667  ++++++%ô™*+*ë›™™›™™™ƒƒƒƒƒË„–šš¡¡ 1676  +++++++fš*L¡™›››™™™ƒƒƒ™ƒb …„ ¡ŸŸ L666  *++++++ašcƒ™¡š›™ƒ‡‡ƒƒƒ™™ƒc…„… Ÿ  …+76  +*+*++)ŸŸ¡¡¡›™‡‡ƒ‡ ƒ¤™›¡bê „†… –……„,6  *+*+** …„„„‹¡ƒ ‡ƒƒƒ™›¡š¡ƒc „„„……………66  *****L„„„††Û„¡‡¹ƒƒ™›šš¡Ÿƒê …Û†……¢„„66  ***+* †††Û†††† ‡ƒ¤¡¡¡Ÿ Ÿc …††Û„…†„,6  ****+…†Û†††††ŒÛ šš¡ŸŸ – ƒ …„††††††„6,  *****…†††††††Û„†–¡ŸŸ– ŸŸc ¢„†††„Û†„66  *****…†††††††„„Ûd     ›ô… „†††††††„,1  ** )*L†††††Û„„†„ –……šcëë„†††Û††„+16   )**)L……¢……„„†‰………… Ÿë…„†††„†„*616  *)) ))……  Ÿ¡Ÿd†……„„……„†Û†††…+1161  )))))*c  Ÿ¡š …¢„†††††„„„…11611,  )) )))LŸ¡šš›„„„††Û††††„L1%11166  ))))) 5cšš™ƒŒ†„Û„†††„…L1%111111  )))))) )¡ƒƒƒÛ…†††„„…*%%1%111,1  5555))))ƒ‡™d¢††„„…+%1+%%11111  )5)5))))ë™™	…„†„…*+++111111,6  5))5))) Ÿ¡…„…L+++1+%+1%1111  ))55))))ô……L+++++1+1%11111  )))5))))Û… )+++++++1%1%11,1  
)))àçd…ŸŸ*++++++1+%+11%111  ))5))))dð{L****++++++1%%1111,  ))5)))d†****+*+*+++1++1+%1111  )555))d)******+++++++1%1111,1  ))))))††)****+*+*++++1++1%1111  )5)))))5L)******++++++++1%%1111,  
)K5)) L†)******+*+++1+1+1%1111  ))))5))))5†d****+*+*+++++%+11%11,1  
))'))) d†)*****++++++++1%+1111,  ))5	)oL*****+*+*++++1+%111%111  ))5)55)))) ))))*****+*+++++++1+%111,1  55)))))))*))*******+*+++++1+1%1111,11  	)#))***)******+++++++1++1%11116  	)# )*)******++*+++++++11%1111,1     &   4      
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  
  †   
 èUfill:	
Û  ôá¹!úú   !úú   !úú   	úúú !#$!úú   	ú!!#&#úúúú   úúúúúúú !úú úúú   úúúúúú úúúúúúúú 	úú   úúúúú	úú$$"  úúúúúúú   úúúúú  ú	úâÛ&!##$úúúúú   úúúúú!úúúúúú!"$#" úúúúú   úúúú#úúúúú!"%"&"#"" úúúú   úúúú "úúúú!#$$$#"#&#""! úúú   úúúú!#úú!##"###$$!úúú   úúúú!#ú##""#$$%úú   úúú#"#"!!"#%%%$##&úú   úúú!#%%&%$"" &ú   úúú!&%%%%$#"!ú   úúú%$#!ú   úú#"!ú   úú!ú   úúú   úú
"ú   úú!úú   úúúúúú   úúúúúúú   úúúú!úúúúú   úúúú!#úúúúúú   úúúú!"$$
úúúúúúú   úúúúú!%(	úú   úúúúú$%#ú	úú   úúúúúú#"úú	úú   úúúúúúú !úúú	úú   	úúúú   	ú
úúúú   	úú
úúúúú   	úúúú   	úú
úúúú   	úúúúúúú   	úúúú   	úúúú   	úúúúú   	úúúú   	úúúú   	úúúú   úúúúúúú’’’’’’’úúúú   	úúú’úúú   	úúú’úúú   	úúú’úúú   !úú      !   1   †ÿÿÿöÿÿÿÔ   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá)0i*€{
  
  U
  [ Àbrush3:	
Û  .l¶¶¶¶ÇÇ   ¶¶¶¶ÇÇ   ¶¶ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇ·Ç¶¶ÇÇ   ¶¶ÇÇÇÇ£œwwœœ£ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇœS(œ¢ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇw£ÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇœ(ÇÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇxMSÇÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇ¢œS(SÍñÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇ£¢ÇÇññÇÇÇÇÇ¶¶ÇÇ   ¶¶ÇÇÇÇ¶¶ÇÇ   ¶¶¶¶ÇÇ   ¶¶¶¶ÇÇ   ÇÇ   ÇÇ               
 %Ü
  D0
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 'Ž
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×	
Û   ìl%%   %%   %%   %%   %%   %[%   %%%%%%!33$!%%%%%%%%   %%%%%%"%%%%%%%%   %%%%%%3ÿÿÿT!%%%%%%%%   %%%%%%ÿÿÿÿ	%C%   %%%%%%/(ÿÿÿY%%%%%%%%   %%%%%%#2ø%%%%%%%%   %%%%%%!"%	%%   %%   %%   %%   %%   %%               
 2É
 2Ô
 2ß
 2ê
 2õ
 3 
 3
 3
 3!
 3,
 37
 3B
 3M
 3X
 3c
 3n
 3y
 3„
 3
 3š
 3¥
 3°
 3»
 3Æ
 3Ñ
 3Ü
 3ç
 3ò
 3ý
 4
 4
 4
 4)
 44
 4?
 4J
 4U
 4`
 4k
 4v
 4
 4Œ
 4—
 4¢
 4­
 4¸
 4Ã
 4Î
 4Ù
 4ä
 4ï
 4ú
 5
 5
 5
 5&
 51
 5<
 5G
 5R
 5]
 5h
 5s
 5~
 5‰
 5”
 5Ÿ
 5ª
 5µ
 5À
 5Ë
 5Ö
 5á
 5ì
 5÷
 6
 6
 6
 6#
 6.
 69
 6D
 6O
 6Z
 6e
 6p
 6{
 6†
 6‘
 6œ
 6§
 6²
 6½
 6È
 6Ó
 6Þ
 6é
 6ô
 6ÿ
 7

 7
 7 
 7+
 76
 7A
 7L
 7W
 7b
 7m
 7x
 7ƒ
 7Ž
 7™
 7¤
 7¯
 7º
 7Å
 7Ð
 7Û
 7æ
 7ñ
 7ü
 8
 8
 8
 8(
 83
 8>
 8I
 8T
 8_
 8j
 8u
 8€
 8‹
 8–
 8¡
 8¬
 8·
 8Â
 8Í
 8Ø
 8ã
 8î
 8ù
 9
 9
 9
 9%
 90
 9;
 9F
 9Q
 9\
 9g
 9r
 9}
 9ˆ
 9“
 9ž
 9©
 9´
 9¿
 9Ê
 9Õ
 9à
 9ë
 9ö
 :
 :
 :
 :"
 :-
 :8
 :C
 :N
 :Y
 :d
 :o
 :z
 :…
 :
 :›
 :¦
 :±
 :¼
 :Ç
 :Ò
 :Ý
 :è
 :ó
 :þ
 ;	
 ;
 ;
 ;*
 ;5
 ;@
 ;K
 ;V
 ;a
 ;l
 ;w
 ;‚
 ;
 ;˜
 ;£
 ;®
 ;¹
 ;Ä
 ;Ï
 ;Ú
 ;å
 ;ð
 ;û
 <
 <
 <
 <'
 <2
 <=
 <H
 <S
 <^
 <i
 <t
 <
 <Š
 <•
 < 
 <«
 <¶
 <Á
 <Ì
 <×
 <â
 <í
 <ø
 =
 =
 =
 =$
 =/
 =:
 =E
 =P
 =[
 =f
 =q
 =|
 =‡
 =’
 =
 =¨
 =³
  D0	
Û  8l::   ::   ::   :+:::   ::   :::+·:   +:::::3./""%::::::::   ::::::84O83:::::+::   ::+:::"O+::+::::+   :::::+" ::::::::   +:::::"d::::+:::   ::+:::,d†@:+:::::+   ::::::C% 2?2::::::::   :::+:: :	:+::+:   :+::	::+:::::::   	:::+::::+:::::   :::+::+:	::+:::   +::::::::::+	::               
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
 à
  
 I   
 tbrush3:³   8   |   þ   |   8            †ÿÿÿýÿÿÿý
  ƒá%ð<( e
  
  U
  [ Àeyedropper:	
Û  áqÏ6616111%1%1+11++++++*+******)*))))) 666,61,111%%+++++++*+********)*) )) 66,616111%111+1+++++++*+****** )))) 666661,1111+11++++++*+*******)****) 66161,11111%1%+1+++++++*+*******) ) 6666661111%1%+1+++++++*+	*ã *) 6666161,111%11+1+1+++++++**5ù
 )*** 766661611111%%1+++++++*+*+ùûýýû** 666666,1,1111+%111++++++*ùýüüýý
%)* 766616161111111+%++++++*ùýüüüüý	+* 6766666161,1%111%1+1+++ýýüüüýýþ6* 766666661,1111%1+%1++1ûüüüüüý	þ6* 767666166111111%11++%)ýýüüüýý	þ%+ 67666666161,1111%+ :	ùýüüüüý	+* 7767666661611,111%	û	ýüúüýû
%d+++ 77676666,616111111)ýý
ýüýûþ6++++ 77767666666161,11%1ýý
	ûþd%++++ 777766666616111111+›÷û	1++%+++ 77777766666666,11*™ ™5	1+%%++++ 77777676666,6166L™ ƒ™â%6%%%+1+1+ 777767766666666Lƒ  ¹½À%6%%1+1+%1+ 7777767666661Lƒ ƒ¹åì11%Ø%%11%11+1 777777676666ð  ƒ¹ß„+11%%11%%1+1%1+ 77777777776…  ƒ¹åÛ+61111111111%111 77777676ë  ™¹â†d16161,11111111%1 877777777ë  ™¹â„1,,661611,11111111 [7776ê  ›¹è†166666616161,111111 887777Ÿ  ¹¼Àd6	6k,616161,,1, 88[7å  ¹½…†,67666666666616161616 88…  ¹¡ìd,-76766666661666,61,61 888dƒƒ¹ß†+77776766616 8887 ¹…†d7	77676	6;666 888Ÿƒ¼èÛ777777776776676666666666 8O8†ƒÃ„6.77777	7G67676666666 888âd87777777776776767666666 88888888[7777	76767676 848O88888888[777C767 88888O8888[.77777777777776777 4848888O88888877	7767 8488488888888888[8
777+777 484488O8O8O888888[7777777    #   )      
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
   
  
  
  !
  ,
  7
  B
  M
  X
  c
  n
  y
  „
  
  š
  ¥
  °
  »
  Æ
  Ñ
  Ü
  ç
  ò
  ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
   
   
   
   &
   1
   <
   G
   R
   ]
   h
   s
   ~
   ‰
   ”
   Ÿ
   ª
   µ
   À
   Ë
   Ö
   á
   ì
   ÷
  !
  !
  !
  !#
  !.
  !9
  !D
  !O
  !Z
  !e
  !p
  !{
  !†
  !‘
  !œ
  !§
  !²
  !½
  !È
  !Ó
  !Þ
  !é
  !ô
  !ÿ
  "

  "
  " 
  "+
  "6
  "A
  "L
  "W
  "b
  "m
  "x
  "ƒ
  "Ž
  "™
  "¤
  "¯
  "º
  "Å
  "Ð
  "Û
  "æ
  "ñ
  "ü
  #
  #
  #
  #(
  #3
  #>
  #I
  #T
  #_
  #j
  #u
  #€
  #‹
  #–
  #¡
  #¬
  #·
  #Â
  #Í
  #Ø
  #ã
  #î
  #ù
  $
  $
  $
  $%
  $0
  $;
  $F
  $Q
  $\
  $g
  $r
  $}
  $ˆ
  $“
  $ž
  $©
  $´
  $¿
  $Ê
  $Õ
  $à
  $ë
  $ö
  %
  %
  %
  %"
  %-
  %8
  %C
  %N
  %Y
  %d
  %o
  %z
  %…
  %
  %›
  %¦
  %±
  %¼
  %Ç
  %Ò
  %Ý
  %è
  %ó
  %þ
  &	
  &
  &
  &*
  &5
  &@
  &K
  &V
  &a
  &l
  &w
  &‚
  &
  &˜
  &£
  &®
  &¹
  &Ä
  &Ï
  &Ú
  &å
  &ð
  &û
  '
  '
  '
  ''
  '2
  '=
  'H
  'S
  '^
  'i
  't
  '
  'Š	
Û  áqÏ6616111%1%1+11++++++*+******)*))))) 666,61,111%%+++++++*+********)*) )) 66,616111%111+1+++++++*+****** )))) 666661,1111+11++++++*+*******)****) 66161,11111%1%+1+++++++*+*******) ) 6666661111%1%+1+++++++*+	*ã *) 6666161,111%11+1+1+++++++**5ù
 )*** 766661611111%%1+++++++*+*+ùûýýû** 666666,1,1111+%111++++++*ùýüüýý
%)* 766616161111111+%++++++*ùýüüüüý	+* 6766666161,1%111%1+1+++ýýüüüýýþ6* 766666661,1111%1+%1++1ûüüüüüý	þ6* 767666166111111%11++%)ýýüüüýý	þ%+ 67666666161,1111%+ :	ùýüüüüý	+* 7767666661611,111%	û	ýüúüýû
%d+++ 77676666,616111111)ýý
ýüýûþ6++++ 77767666666161,11%1ýý
	ûþd%++++ 777766666616111111+›÷û	1++%+++ 77777766666666,11*™ ™5	1+%%++++ 77777676666,6166L™ ƒ™â%6%%%+1+1+ 777767766666666Lƒ  ¹½À%6%%1+1+%1+ 7777767666661Lƒ ƒ¹åì11%Ø%%11%11+1 777777676666ð  ƒ¹ß„+11%%11%%1+1%1+ 77777777776…  ƒ¹åÛ+61111111111%111 77777676ë  ™¹â†d16161,11111111%1 877777777ë  ™¹â„1,,661611,11111111 [7776ê  ›¹è†166666616161,111111 887777Ÿ  ¹¼Àd6	6k,616161,,1, 88[7å  ¹½…†,67666666666616161616 88…  ¹¡ìd,-76766666661666,61,61 888dƒƒ¹ß†+77776766616 8887 ¹…†d7	77676	6;666 888Ÿƒ¼èÛ777777776776676666666666 8O8†ƒÃ„6.77777	7G67676666666 888âd87777777776776767666666 88888888[7777	76767676 848O88888888[777C767 88888O8888[.77777777777776777 4848888O88888877	7767 8488488888888888[8
777+777 484488O8O8O888888[7777777    #   )      
  {
  {
  {
  {%
  {0
  {;
  {F
  {Q
  {\
  {g
  {r
  {}
  {ˆ
  {“
  {ž
  {©
  {´
  {¿
  {Ê
  {Õ
  {à
  {ë
  {ö
  |
  |
  |
  |"
  |-
  |8
  |C
  |N
  |Y
  |d
  |o
  |z
  |…
  |
  |›
  |¦
  |±
  |¼
  |Ç
  |Ò
  |Ý
  |è
  |ó
  |þ
  }	
  }
  }
  }*
  }5
  }@
  }K
  }V
  }a
  }l
  }w
  }‚
  }
  }˜
  }£
  }®
  }¹
  }Ä
  }Ï
  }Ú
  }å
  }ð
  }û
  ~
  ~
  ~
  ~'
  ~2
  ~=
  ~H
  ~S
  ~^
  ~i
  ~t
  ~
  ~Š
  ~•
  ~ 
  ~«
  ~¶
  ~Á
  ~Ì
  ~×
  ~â
  ~í
  ~ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  € 
  €
  €
  €!
  €,
  €7
  €B
  €M
  €X
  €c
  €n
  €y
  €„
  €
  €š
  €¥
  €°
  €»
  €Æ
  €Ñ
  €Ü
  €ç
  €ò
  €ý
  
  
  
  )
  4
  ?
  J
  U
  `
  k
  v
  
  Œ
  —
  ¢
  ­
  ¸
  Ã
  Î
  Ù
  ä
  ï
  ú
  ‚
  ‚
  ‚
  ‚&
  ‚1
  ‚<
  ‚G
  ‚R
  ‚]
  ‚h
  ‚s
  ‚~
  ‚‰
  ‚”
  ‚Ÿ
  ‚ª
  ‚µ
  ‚À
  ‚Ë
  ‚Ö
  ‚á
  ‚ì
  ‚÷
  ƒ
  ƒ
  ƒ
  ƒ#
  ƒ.
  ƒ9
  ƒD
  ƒO
  ƒZ
  ƒe
  ƒp
  ƒ{
  ƒ†
  ƒ‘
  ƒœ
  ƒ§
  ƒ²
  ƒ½
  ƒÈ
  ƒÓ
  ƒÞ
  ƒé
  ƒô
  ƒÿ
  „

  „
  „ 
  „+
  „6
  „A
  „L
  „W
  „b
  „m
  „x
  „ƒ
  „Ž
  „™
  „¤
  „¯
  „º
  „Å
  „Ð
  „Û
  „æ
  „ñ
  „ü
  …
  …
  …
  …(
  …3
  …>
  …I
  …T
  …_
  …j
  …u
  …€
  …‹
  …–
  …¡
  …¬
  …·
  …Â
  …Í
  …Ø
  …ã
  …î
  …ù
  !
  eyedropper:action:cursor:   
 ½eyedropper:	
Û  ­áh%úúú  %úúú  úúúÍÆ¿¢úúúú  úúÆãêé¿›úúú  úÅêêêêê¿wúú  úúúúÆêêñññê¿pú  úúúÌéêñññêê¿ppú  úúúãêñññêê¿›L(ú  úúãêñññêê¿›p((ú  úúÆÆÆéêñññêê¿›p((úú  úú¿ã¿éñññêé¿›p((úúú  úú¢êê¿êñêã¿wL((úúúú  úúú›Æê¿¿¿›p(((úúúúú  úú›ê¿››p(((úúúúúú  ú "›¿›L(((úúúúúúú  úúúú &'$p”L((úúúúúúúú  úúú &%Ú…SS((úúúúúúúúú  úú!'%Ú„úú”S(úúúúúúúúú  ú!''%°„úúúú  úúúú!'%°Súúúúú  úúú'%¯úúúúúú  úú!'$¯úúú  ú"'$¯úúúú  	úúúú"'Ú¯úúúúú  	úúú"%Ú…úúúúúú  	úú!%Ú…úúú  	ú '%°„úúúú  úúúúúúú%¯úúúúú  úúúúúú’"%¯úúú  úúúúúú’$„úúúú  úúúúúú’úúúúúú  úúú’’’’’’’úúúúú  úúúúúú’úúúú  úúúúúú’úúúú  úúúúúú’úúúú  %úúú     &   $   †ÿÿÿôÿÿÿÝ   
  ‡ê
  ‡õ
  ˆ 
  ˆ
  ˆ
  ˆ!
  ˆ,
  ˆ7
  ˆB
  B{
  ˆR
  ˆ]
  ˆh
  ˆs
  ˆ~
  ˆ‰
  ˆ”
  ˆŸ
  ˆª
  ˆµ
  ˆÀ
  ˆË
  ˆÖ
  ˆá
  ˆì
  ˆ÷
  ‰
  ‰
  ‰
  ‰#
  ‰.
  ‰9
  ‰D
  ‰O
  ‰Z
  ‰e
  ‰p
  ‰{
  ‰†
  ‰‘
  ‰œ
  ‰§
  ‰²
  ‰½
  ‰È
  ‰Ó
  ‰Þ
  ‰é
  ‰ô
  ‰ÿ
  Š

  Š
  Š 
  Š+
  Š6
  ŠA
  ŠL
  ŠW
  Šb
  Šm
  Šx
  Šƒ
  ŠŽ
  Š™
  Š¤
  Š¯
  Šº
  ŠÅ
  ŠÐ
  ŠÛ
  Šæ
  Šñ
  Šü
  ‹
  ‹
  ‹
  ‹(
  ‹3
  ‹>
  ‹I
  ‹T
  ‹_
  ‹j
  ‹u
  ‹€
  ‹‹
  ‹–
  ‹¡
  ‹¬
  ‹·
  ‹Â
  ‹Í
  ‹Ø
  ‹ã
  ‹î
  ‹ù
  Œ
  Œ
  Œ
  Œ%
  Œ0
  Œ;
  ŒF
  ŒQ
  Œ\
  Œg
  Œr
  Œ}
  Œˆ
  Œ“
  Œž
  Œ©
  Œ´
  Œ¿
  ŒÊ
  ŒÕ
  Œà
  Œë
  Œö
  
  
  
  "
  -
  8
  C
  N
  Y
  d
  o
  z
  …
  
  ›
  ¦
  ±
  ¼
  Ç
  Ò
  Ý
  è
  ó
  þ
  Ž	
  Ž
  Ž
  Ž*
  Ž5
  Ž@
  ŽK
  ŽV
  Ža
  Žl
  Žw
  Ž‚
  Ž
  Ž˜
  Ž£
  Ž®
  Ž¹
  ŽÄ
  ŽÏ
  ŽÚ
  Žå
  Žð
  Žû
  
  
  
  '
  2
  =
  H
  S
  ^
  i
  t
  
  Š
  •
   
  «
  ¶
  Á
  Ì
  ×
  â
  í
  ø
  
  
  
  $
  /
  :
  E
  P
  [
  f
  q
  |
  ‡
  ’
  
  ¨
  ³
  ¾
  É
  Ô
  ß
  ê
  õ
  ‘ 
  ‘
  ‘
  ‘!
  ‘,
  ‘7
  ‘B
  ‘M
  ‘X
  ‘c
  ‘n
  ‘y
  ‘„
  ‘
  ‘š
  ‘¥
  ‘°
  ‘»
  ‘Æ
  ‘Ñ
  ‘Ü
  ‘ç
  ‘ò
  ‘ý
  ’
  ’
  ’
  ’)
  ’4
  ’?
  ’J
  ’U
  ’`
  ’k
  ’v
  ’
  ’Œ
  ’—
  B{
  ’§
  ’²
  ’½
  ’È
  ’Ó
  ƒá'à>*Ða
  
  U
  [ Àerase:	
Û  ~á¤} ùùùù   %    ùùùùùùù %   ùùùùùùùù %  ùùùùùùùóò % ùùùùùóòòò !    ùùùííòòÎ~ùùò     ùùùíçíÎòùÎóù  ÿùùùíççòòÿ~ùùùù ÿùùùíçççóùùùòòÎùùùùù       ÿÿùùíççççíçíòùùùùùù      ©ÿùùíçççççççíóóùùùùùùù      êÿùíç	çççíóóóóíí     ññÿíçç	çççææææÂæ ÿñøÿùççç	ççæÂÂÂÂì     ÿÿññÿççççççæìçççæÂæÂæó     &ÿÿññ&ÿóççççæìííçæÂÂÂæù     &&ÿÿñãñÿÿíççççííæÂÂÂæí  &&&ÿÿñããÔ&óççççæÂÂææó      &&&&ÿÿãããÔ&ùíçæÂÂæìù      &&&þøÿÿñ¿¿¿ÍÎÎìòù    &&ø÷÷&ñ¿¿¿¿¢ÿ     &øø÷÷÷&&Æ¿¿›››©   ø÷÷÷÷ññÔ©Í&    ÷÷÷÷ññððÍ©       ÷÷ñññððÆÆÅÆñ!    ñññððÆÆÅÆø  % ñðêÆÅÆð   %  ñÆÅÆ÷ á    /   #   †                   "    
 %ì
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 @G
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì
 0×
 =ž
  !
  
  †   
 =merase:	
Û  ~á¤} ùùùù   %    ùùùùùùù %   ùùùùùùùù %  ùùùùùùùóò % ùùùùùóòòò !    ùùùííòòÎ~ùùò     ùùùíçíÎòùÎóù  ÿùùùíççòòÿ~ùùùù ÿùùùíçççóùùùòòÎùùùùù       ÿÿùùíççççíçíòùùùùùù      ©ÿùùíçççççççíóóùùùùùùù      êÿùíç	çççíóóóóíí     ññÿíçç	çççææææÂæ ÿñøÿùççç	ççæÂÂÂÂì     ÿÿññÿççççççæìçççæÂæÂæó     &ÿÿññ&ÿóççççæìííçæÂÂÂæù     &&ÿÿñãñÿÿíççççííæÂÂÂæí  &&&ÿÿñããÔ&óççççæÂÂææó      &&&&ÿÿãããÔ&ùíçæÂÂæìù      &&&þøÿÿñ¿¿¿ÍÎÎìòù    &&ø÷÷&ñ¿¿¿¿¢ÿ     &øø÷÷÷&&Æ¿¿›››©   ø÷÷÷÷ññÔ©Í&    ÷÷÷÷ññððÍ©       ÷÷ñññððÆÆÅÆñ!    ñññððÆÆÅÆø  % ñðêÆÅÆð   %  ñÆÅÆ÷ á    /   #   †ÿÿÿðÿÿÿÞ   
 @G
 @W"?ÿÿ™
 %÷
 &
 &
 &
 &#
 &.
 &9
 &D
 &O
 &Z
 &e
 &p
 &{
 &†
 &‘
 &œ
 &§
 &²
 &½
 &È
 &Ó
 &Þ
 &é
 &ô
 &ÿ
 '

 '
 ' 
 '+
 '6
 'A
 'L
 'W
 'b
 'm
 'x
 'ƒ
 @G
 '™
 '¤
 '¯
 'º
 'Å
 'Ð
 'Û
 'æ
 'ñ
 'ü
 (
 (
 (
 ((
 (3
 (>
 (I
 (T
 (_
 (j
 (u
 (€
 (‹
 (–
 (¡
 (¬
 (·
 (Â
 (Í
 (Ø
 (ã
 (î
 (ù
 )
 )
 )
 )%
 )0
 );
 )F
 )Q
 )\
 )g
 )r
 )}
 )ˆ
 )“
 )ž
 )©
 )´
 )¿
 )Ê
 )Õ
 )à
 )ë
 )ö
 *
 *
 *
 *"
 *-
 *8
 *C
 *N
 *Y
 *d
 *o
 *z
 *…
 *
 *›
 *¦
 *±
 *¼
 *Ç
 *Ò
 *Ý
 *è
 *ó
 *þ
 +	
 +
 +
 +*
 +5
 +@
 +K
 +V
 +a
 +l
 +w
 +‚
 +
 +˜
 +£
 +®
 +¹
 +Ä
 +Ï
 +Ú
 +å
 +ð
 +û
 ,
 ,
 ,
 ,'
 ,2
 ,=
 ,H
 ,S
 ,^
 ,i
 ,t
 ,
 ,Š
 ,•
 , 
 ,«
 ,¶
 ,Á
 ,Ì
 ,×
 ,â
 ,í
 ,ø
 -
 -
 -
 -$
 -/
 -:
 -E
 -P
 -[
 -f
 -q
 -|
 -‡
 -’
 -
 -¨
 -³
 -¾
 -É
 -Ô
 -ß
 -ê
 -õ
 .
 .
 ."
 .-
 .8
 .C
 .N
 .Y
 .d
 .o
 .z
 .…
 .
 .›
 .¦
 .±
 .¼
 .Ç
 .Ò
 .Ý
 .è
 .ó
 .þ
 /	
 /
 /
 /*
 /5
 /@
 /K
 /V
 /a
 /l
 /w
 /‚
 /
 /˜
 /£
 /®
 /¹
 /Ä
 /Ï
 /Ú
 /å
 /ð
 /û
 0
 0
 0
 0'
 02
 0=
 0H
 0S
 0^
 0i
 0t
 0
 0Š
 0•
 0 
 0«
 0¶
 0Á
 0Ì"?ÿÿ™
  ƒu%À”(`ù
  
  U"  ÿ ÀColorPickerClosedc
  showColorPalette:³  áèIV   O?
O?O?O9O9
O9NyNyNy2yNy2y2yŒÆ O?O?O?O?O9O9O?
O9O9NyO9O9NyNyNy2yNy2y2y2yÆ O?O?O?O9O?O?O9O9O9NyO9NyNyNy2y2y2y2yÆ O?
O?O?
O9O9NyO9NyNyO9Ny
NyO9NyNy2y2yNy2y2y2y2yÆ O?O?O?O9O9O?O9O9O9NyO9NyNyNy2yNyNy2y2y2yÆ O?O?O?O?O9O9O?:  
2y2y2yÌ O?
O?O?O?O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿ2ÿÿÿ 
2y2y2yÆ O?
O?O?O?O9 ÿ2ÿÿÿ 
2y2yy%) O?O?O?O?O9O9O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿ2ÿÿÿ 
2y2y2yÆ O?
O?O?O?O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9:  
2y2y2yÆ O?O?O?O9O?O?O9 ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿ2ÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ßßßßÿÿÿÿ 
2y2y2y%) O?O?O?O?O9O9O? ßßßßÿÿÿÿ 
2y2y2yÆ O?O?O?O9O?O9O9 ¿¿¿¾¾¾ÿÿÿÿ 
2y2y2yÆ O?O?O?
O?O9 ¿¿¿
¾¾¾ÿÿÿÿ 
2y2y2yÆ O?O9O9O?O?O9O? ŸŸŸŸž
žžžÿÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9 ŸŸŸ
žžžÿÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 	~
~~}}}ÿÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? _
___^^^^]]]]ÿÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 _
__
^^]]]\\ÿÿÿÿ 
2y2y2yÆ O?
O?O?O9O9 ?
??>>>====<<{Þ{Þ{Þ{Þ 
2y2y2yÆ O?O?O?
O9O9 ?
??>>>===<<<{Þ{Þ{Þ{Þ 
2y2y2yÆ O?
O?O?O?O9 
{Þ{Þ{Þ{Þ 
2y2y72yÆ O?O?O?O?O9O9O9 {Þ{Þ{Þ{Þ 
2y2y2yÆ O?O?O?
O9O?# ~ÿ~ÿ~ÿ~ÿ~þ~þ~þ~ý~ý~ü~ü~û~û~ûw½w½w½w½ 
2y2y2yÆ O?O?O?
O?O9# ~ÿ~ÿ~ÿ~ÿ~þ~þ~ý~ý~ü~ü~ü~û~û~úw½w½w½w½ 
2y2y72yÆ O?O?O?O?O9O9O? ~ß~ß~ß~ß~Þ~Þ~Ý~Ý~Ü~Ü~Û~Û~Ú~Úw½w½w½w½ 
2y2y2yÆ O?O?O?
O?O9# ~¿~¿~¿~¿~¾~¾~½~½~¼~¼~»~»~º~ºsœsœsœsœ 
2y2y72y%) O?O?O?O9O?O9O9 ~¿~¿~¿~¿~¾~¾~½~¼~¼~»~»~º~º~¹sœsœsœsœ 
2y2y2yÆ O?O?O?
O?O9# ~Ÿ~Ÿ~Ÿ~Ÿ~ž~~~œ~œ~›~›~š~™~™sœsœsœsœ 
2y2y2yÆ O?O?O?
O9O?# ~Ÿ~Ÿ~Ÿ~Ÿ~ž~~~œ~œ~›~š~š~™~™o{o{o{o{ 
2y2y2yÆ O?O?O?
O?O9# ~~~~~~~~}~}~|~{~{~z~z~y~xo{o{o{o{ 
2y2y72yÆ O?O?O?O9O?O9O9 ~~~~~~~~}~|~|~{~{~z~y~y~xkZkZkZkZ 
2y2y2yÆ O?O?O?
O?O9# ~_~_~_~^~^~]~\~\~[~Z~Z~Y~X~XkZkZkZkZ 
2y2y72yÆ O?O?O?O?O9O9O? ~?~?~?~>~>~=~<~<~;~:~9~9~8~7g9g9g9g9 
2y2y2yÆ O?
O?O?'O?O9 ~?~?~?~>~>~=~<~;~;~:~9~8~8~7g9g9g9g9 
2y2y72yÆ O?O9O9O?O9O9O9 z~~~~~~~~~~~~~cccc 
2y2y2yÆ O?O?O?
O?O9# z~~~~~~~~~~~~~cccc 
2y2y2yÆ O?O?O?
O?O9# yÿ}ÿ}ÿ}þ}ý}ü}ü}û}ú}ù}ø}ø}÷}ö^÷^÷^÷^÷ 
2y2y72yÆ O?O9O9O?O?O9O9 yÿ}ÿ}ÿ}þ}ý}ü}ü}û}ú}ù}ø}÷}÷}ö^÷^÷^÷^÷ 
2y2y2yÆ O?O?O?
O?O9# yß}ß}ß}Þ}Ý}Ü}Û}Û}Ú}Ù}Ø}×}Ö}ÕZÖZÖZÖZÖ 
2y2y72yÆ O?O?O?O9O?O9O9 yß}ß}ß}Þ}Ý}Ü}Û}Ú}Ú}Ù}Ø}×}Ö}ÕVµVµVµVµ 
2y2y2yÆ O?O?O?
O?O9# y¿}¿}¿}¾}½}¼}»}º}¹}¸}·}·}¶}µVµVµVµVµ 
2y2y72yÌ O?O9O9O?O?O9O? yŸ}Ÿ}Ÿ}ž}}œ}›}š}™}˜}—}–}•}”R”R”R”R” 
2y2y2yÌ O?O?O?
O?O9# yŸ}Ÿ}Ÿ}ž}}œ}›}š}™}˜}—}–}•}”NsNsNsNs 
2y2y72yÆ O?O?O?O?O9O9O9 y}}}~}}}|}{}z}y}x}w}v}u}tNsNsNsNs 
2y2y2yÆ O?
O?O?'O?O9 y}}}~}}}|}{}z}y}x}v}u}t}sJRJRJRJR 
2y2y72yÆ O?O9O9O?O9O9O? y_}_}_}^}]}\}[}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2yÆ O?O?O?
O?O9# y_}_}_}^}]}[}Z}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2yÆ O?O?O?
O?O9# y?}?}?}>}=};}:}9}8}7}6}5}4}2BBBB 
2y2y72yÌ O?O?O?O?O9O9O? y}}}}}}}}}}}}}=ï=ï=ï=ï 
2y2y72yÆ O?O?O?O9O?O?O9 y}}}}}}}}}}}}}9Î9Î9Î9Î 
2y2y2yÆ O?
O?O?'O9O9 xÿ|ÿ|ÿ|ý|ü|û|ú|ù|÷|ö|õ|ô|ó|ñ5­5­5­5­ 
2y2y72yÆ O?O?O?O9O9O9O? xÿ|ÿ|ÿ|ý|ü|û|ú|ø|÷|ö|õ|ô|ò|ñ1Œ1Œ1Œ1Œ 
2y2y2yÆ O?
O?O?'O9O9 xß|ß|ß|Ý|Ü|Û|Ú|Ø|×|Ö|Õ|Ó|Ò|Ñ1Œ1Œ1Œ1Œ 
2y2y2yÆ O?O?O?
O9O?# xß|ß|ß|Ý|Ü|Û|Ù|Ø|×|Ö|Ô|Ó|Ò|Ð-k-k-k-k 
2y2y72y%) O?O9O9O?O?O9O9 x¿|¿|¿|½|¼|»|¹|¸|·|µ|´|³|±|°)J)J)J)J 
2y2y2yÆ O?O?O?
O?O9# x¿|¿|¿|½|¼|»|¹|¸|·|µ|´|²|±|°%)%)%)%) 
2y2y72yÆ O?O?O?O9O?O9O9 xŸ|Ÿ|Ÿ||œ|š|™|˜|–|•|”|’|‘|!!!! 
2y2y2yÆ O?
O?O?'O9O? x|||}|||z|y|x|v|u|s|r|q|oçççç 
2y2y72yÆ O?O?O?O?O9O9O9 x|||}|||z|y|w|v|u|s|r|p|oÆÆÆÆ 
2y2y72yÆ O?O?O?O9O?O?O9 x_|_|_|]|\|Z|Y|W|V|T|S|Q|P|N¥¥¥¥ 
2y2y72yÆ O?O?O?O?O9O9O? x_|_|^|]|\|Z|Y|W|V|T|S|Q|P|N„„„„ 
2y2y72yÌ O?O?O?O9O?O?O9 t?|?|>|=|;|:|8|7|5|4|2|1|/|.cccc 
2y2y2yÆ O?
O?O?'O9O9 t?|?|>|=|;|:|8|7|5|4|2|1|/|-BBBB 
2y2y72yÆ O?O?O?O9O9O?O9 t|||||||||||||!!!! 
2y2y72yÆ O?O?O?O?O9O9O? t|||||||||||||   
2y2y2yÆ O?
O?O?'O?O9 tx||||||||||||   
2y2y72yÆ O?O9O9O?O9O9O9 pxxxxxxxxxxxxx   
2y2y2yÆ O?O?O?
O?O9# ptxxxxxxxxxxxx   
2y2y2yÆ O?O?O?
O?O9# lttttttttttttt   
2y2y2yÆ O?
O?O?'O9O? lptttttttttttt   
2y2y2yÆ O?O?O?
O?O9# hppppppppppppp   
2y2y72yÆ O?O?O?O9O?O9O9 hlpppppppppppp   
2y2y2yÆ O?O?O?
O?O9# dlllllllllllll   
2y2y2yÆ O?
O?O?'O9O? dhhhhhhhhhhhhh   
2y2y2yÆ O?O9O9
O?O9# `dhhhhhhhhhhhh
   
2y2y72yÌ O?O?O?O?O9O9O9 `ddddddddddddd
   
2y2y2yÆ O?
O?O?'O9O9 \`dddddddddddd
   
2y2y2yÆ O?O9O9
O?O9# \`````````````
   
2y2y2yÆ O?
O?O?'O9O9 X\````````````
   
2y2y72yÆ O?O?O?O9O9O?O9 X\\\\\\\\\\\\
\	   
2y2y72yÆ O?O?O?O?O9O9O? TX\\\\\\\\\\\
\	   
2y2y2yÆ O?
O?O?'O?O9 PXXXXXXXXXXXX
X	   
2y2y72y%) O?O9O9O?O9O9O9 PTTTTTTTTTTTT
T	   
2y2y2yÆ O?O?O?
O?O9# LPTTTTTTTTTTT	T   
2y2y2yÆ O?O?O?
O?O9# LPPPPPPPPPPP
P	P   
2y2y2yÆ O?O?O?
O?O9O9O9
O9NyNyNyNy2y2y2y2yÌ O?O?O?
O9O?
O9O9
NyO9NyNyO9NyNyNy2y2y2yÆ O?O?O?
O?O9
O9O9O9NyNyNy2y2y2yÆ O?O?O?O9O?O9O9
NyO9NyNyO9NyNyNy2y2y2yÆ O?O?O?O9O9O?O9O9NyO9O9
NyO9NyNy
2yNy2y2y1“ÌV     *   e   á(`É*Ðß
  
  U"  ÿ   €   Àclear:	
Û  fÜ	ß‡ßââèèìð*ð+ð+ì6ÛìØØÛÛìèèâââßßßßß ßßßåßßßßßðølH??L )*%èãèâââßßßßåß ßåßßßßåð?f?üH?H)))L))„èèââßßßßß ßßßßíôEEü???))	)âK***†ãèâßßßß ßßßíHüEü?H25555555)))))***+„ãèâßß ßßåHüEEH%n*54855552555 5))))*+*%Ûãèâß ßíEEü+w4wD2D922:52 252555 )**+++ÛÛèâ ßøHü??4D2+92D955+%522%, 5+*+)**++1ØÛè ß??2e&5522ˆ921ww45ww2/Dw)**+++Øã ßL55"5252D&2D4,w 4"7ÿ2Dn%5 *+++Ø â5552D42 22D&w"Ow%2*4ÿ2/25 *+++ÍØ â)))552/22 292wDD& /ÿDÿ2ˆ/5 **+++4 â)*)* 5 4D2+D D/2w%275ÿ8 ÿ5/5 *+111Î âð*****)w1n2DD2D&w2ÿ4"ÿ5D/**+11+Í âè*+++***ÿw651ÿ4+ÿw5"ÿDw5/*+1d1eÂÇ ââã+++++**+   55 55* )) 5***+%d116æÍØ ßâèÛd+11+++**** *    ***+++1d161[eæÌÍÛ ßâèãØd6+d1+1+++++**+*++++1d111dææÌÛè ßßâèãØÇ6111
111+d11d6116ææÂÌÍØØãè ßßßâèãØØÍŠd61d+1d11166[eæeÂÌÍÛãèâ ßßßâââãÛØÍÌÌÂÂæææææææÂæÂÂÌÍÇÛãèèâß ßßßßßââèãÛÛØÍÍÍÌÌÌÌÌÌÌÌÍÍØØÛãèââßß    '         "?ÿÿÿ"CÄ"Ö5"7­ëz"Ö5"„!"„!"7ªV•"3¦5"?÷5Í"?öµ­"'ä")Q€`"/a @"Ð  "+`  "1   "#€ "'€ "7¥¥"-f5H";æµ("1¢ "/fµH"/f5("' "ƒ¤"%5H"9æµ"+fµH"9êÚQ"5©Æ"1¨ÅÍ"+g5i"Ô ä"' ¤"?÷µ"+gµ"“¤"9çµ("! "1©Eí"/hÅÍ"-hE­")W5i"'µH"%5("Ô Ä"?ùEi")Wµ"5©Åí"-gµi"Õ!"3©Åí"'5i"%µH"#5("ƒ¤"5©E­"?úU­"/d  "?úÙÍ"Ô Ä";ëÚQ"?ûÚ")T  "7¦´ "1¨ÅH"
B`"?ýj•";èD "?øÄ "7¬j•"'µ("/g4 "7¨D "-jV1"?üéí";éD "Õ¥("?ýj1"-iEi"+hÅH"5ªÙ"1©ÅH"Õ  "9ëÙ"-iE("9ìiÍ"3ªÙi"„ Ä"×5H";îzq"7­j"?ÿzq"?ÿzQ"9íéí"7­ëZ"?ÿÿ¾"1¬j¶"×µ­"CÄ"5­jÖ"…!"?ÿÿ:"7­ê¶"„ ä"5­j•"3¬êq"7­ê•"B`")ZUÍ"-kYí"5­j1"'Å"?ÿþq"1¬iÍ"?ÿþQ"+jÙ"  "1¬i"9îú¶";ï{"-kÚ1"7®ú¶"-nz"+nyí"%é"%éÍ"ÛY("ŠU"9îûž"#F1"'Vq"!Æ"7¯ÿz"
C¤"×µ"—5i"%ÚQ"ÙEÍ"#Z1")]j•"ØÅ­"!Ú"ÚUí"ÙE­"ÛY"IÄÄ"!Z1"'j¶"ÛZ"ÚÚ1"5®{z"1­k:"1­ëZ"#jö"šÚq"Üêö"-kÚö"+kZÖ"-lë:"ÚV•"ÚV•"%ëz"ÚÚ¶"Ýkž"
IF•"‰Æ¶"Œë¾"[z"Ýëÿ"kÿ"6Q"ÆÖ"kÿ" ¶•" 
Wz"Œëÿ"
Lkÿ" Æö" [Þ"ÙÆö"†¶"ÙÆö")\ëÞ"ÙÇ"ÙFö"'Ûž"%[z"!W:"ÙÇ"ØÆ•"ØFö"
C"%W:"…¦"×6¶"D""…¦"…¦1" Í" ")ZVÖ"„!("„!"Ci"Ö·:"„#¾"Cÿ"•#ž"×7"Õ§"
BH"A("Õ#:"Ö6"Ô#Z"Óz"×61"%·"Ò“¾"€ÿ")YG"!#z"ƒÞ"'ÆÖ"!6q"Óž"-jÚö"
BÄ"%#:"%¶•"'¶ö"3¬k:"-jÚÖ"%6Q"-hÆÖ"3©Ç"5ªÛ:"+hF•"1¨ÆÖ"9ëÛZ"5¨Æ•"1ªÚÖ"?üëz"?ükZ"7§¶1"5©Æ•"9ç¶1"?ûÛ"?÷¶1"?ûZö"?ùÆ•"9æ5­"    
 dê
  !
  clear:with:   
 d§clear:
  ƒá%‘(Á
  
  U
  D0 ÀstampTab³  Dâ<C^ÐoUsuw–··svgV°RoV -Ë-Ë-ë1ì1ì1ì1ì-ë-ë-ë1ì266-6-6-6-:N:M6M6-6-»6- k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV -Ë-ë-ë1ì21ìBVJ=¨-Ê-ë26-6-:M:N:N:N6-JMZkIé1Ë1ë26-:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV -ë-ë1ì1ì21ìvi~í]Ç(ä)‰-ë2:N
:N:NcF,zH~íj	4ã)‰-ë6,:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV -ë-ë1ë1ì2-Ë5A%5a)ª2:M
:N:Nã‡:0Â=$=$ £)‰2:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV -ë5ë1Ë-Ë-Ë)Ê5ˆ$@ %‰2:->M:-6-6,1ë5«0ã@ ƒ%i1ì:M k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 9ëqær(jZ	-gb1¤£)ª2N+vr(f)V	5ˆf	A$Å£%‰2:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 1Ê@‚XâiÆna £Äå!h-Ë6-9ËD`a#mæQ†`$¢@Åå!h-Ë6-:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV -Ë)‰(„4@8` a£Å&%‰26-6-1ª,ƒ8@0`a¢Å)‰2:N:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 1ë)Ê%‰!'$b, (@`‚!)‰26M:N6-ª%(A,@,`` å)ª2:N:n k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 21ë-Ë)‰!GÅ(b, $@a Ã-ª6-:N:N6-Ë%h Å(b, (@`£)ˆ6-:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 2221ë-Ê%‰!G$Ä(‚$‚)h2:N>>o:N2)ª%i!G$å(£$‚%1ì:N k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 66-6-6-22-Ë)ª%i!h%h)ª2:nBB°B°>:N6-2-Ê)‰%‰)ª2:n k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
""!!""""7" ^Òk4oTsuw–··svgV°w¼  kWk4k4oUw–{·×{·svoUk4k4
oToTcZ±Z±ZÑ^Ñ^Ñ^ÑZÑZ±Z±Z±ZÑ^Ñbòggk4oToToUoUsUsususvsvw–w–{·×{·w–k4Z±Row½  {Ýk4k4oTsv{·×·{·svsuoU
oUoUk4k4k4k3k3k4k4k4oToT
oUoU;sususvwvw–w–{·{·×{·subóV°Ro{Þ    s™gk4oUw–{·×{·{·w–w–.susu?susvsvw–w–w–{·{·{··{·svk4^ÑVs›      {Þggk4suw–{···{·{·.w–w–w—{—{·{·w–kTbòV°Row½	 w»ggk4oUw–{·{·{··2··×××{·{·{·w–svk4bòZ±Vs›  	   sšbógk4oTsuwvw–{·.··{·{·w–svoUk3bòZ±Vs› w»bòbòggk4oToU2w–w–svsvsuoUk4g3bó^ÒZ±sšs›     {Þsš^Ò^òbòbòg2k4k4k4ggcbò^ÒZ±sšsš{Ý   þ{½w›sšsz6sysysyszsšw›{½      3      
 s×
 s×
 à
  toggleStamps    
  ƒá(Á+
  
  U
  D0 ÀshapeTab³  òáµàÿ^ÐoUsuw–··svgV°RoV :x:x:x>™>™BºBºBºB¹>w65BºBº k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV >™>™>™B™B™BºB¹B™B™-°é>xB˜ k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV >™>™>™B™B™B™B™>˜1òcé:V>w k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
>™>™G>™>˜:V-¯c!m1ó65:V k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
>™>™â§>x>x-¯Çè-ò1ó64:V k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV >™>™>™>x>w: è1ó1ò1ó:5>W k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV >x>x>x>v1Ð !L-Ñ-ò1ó6:V>w k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV >x:x:W1Ð¦È-Ñ-Ò-ò6:5>wB˜ k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV :x:wFt È-ò-Ò-Ò666:wB¹Fº k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 25!m !L-Ñ-Ò-ò265>w>™FºFÛ k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV .
é-Ò-ò-Ò264:V>™B™FÚFÛ k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV :W6V6522265:VB™BºFºFÛJÛ k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV :W6V656665:V>wB™FºFºFÛ   k4k4oTsuw–··svgV°w¼  ^ÐoUsuw–··svgV°RoV 
""!!7! ^Òk4oTsuw–··svgV°w¼  kWk4k4oUw–{·×{·svoUk4k4
oToTGZ±Z±ZÑ^Ñ^Ñ^ÑZÑZ±Z±svsvw–w–{·×{·w–k4Z±Row½  {Ýk4k4oTsv{·×·{·svsuoU
oUoUk4k47k4wvw–w–{·{·×{·subóV°Ro{Þ    s™gk4oUw–{·×{·{·w–w–susu7suw–{·{·{··{·svk4^ÑVs›      {Þggk4suw–{···{·{·w–w–w–{·
{·{·w–kTbòV°Row½	 w»ggk4oUw–{·{·{·····{·{·{·w–svk4bòZ±Vs›  	   sšbógk4oTsuwvw–{····{·w–svoUk3bòZ±Vs› w»bòbòggk4oToUw–w–w–oUk4g3bó^ÒZ±sšs›     {Þsš^Ò^òbòbògk4k4k4cbò^ÒZ±sšsš{Ý   þ{½w›sšszsysysyszsšw›{½      %      
 {“
 {“
 à
  toggleShapes    
  ƒ"?ñ ³  œiÿ  4Ð   {Ýw™oVk5k4¢k4k4k4g4g4oWw»þ   {ÜoUoUoTk4¦k4k4k4gggbóbòoWþ w˜svsuoUªoUoUoTk4k4ggbòbò^Ò{¼  	   w—w–sv
sususvwv¢w–w–svsusuoUk4gbò^Ñ^Ñw¼	 w¹w—w–svsusvw–w–{·{·Š··{·{·+w–w–suk4g^ÒZ±Z±Þ    Ýw–w–svsvw–w–{·{·
×××·’···×
××3{·{·w–suk4bòZÑZ±c    sww–svsvw–w–{·××{·{·{·w–šw–w–?w–{·{·{·××{·w–sug^ÒZ°V°  k5svsvsusvw–{·××{·w–suoUk4k4k3ŽggGgk3k4k4oToUsvw–{··×{·w–oTbòZ±V{ÝbÑsvsusuw–{··×{·svoTgbò^Ò^ÑZ±ŽZ±Z±GZ±ZÑ^Ñ^òbógk4oUw–{··×{·sugZ±Vw¼bñsuoUsvw–{·×{·svk4bó^ÑZ±V°VVŽRR?VVV°Z±^Ñbògk4oUw–{·×{·w–k4^ÑVw›bñoUoUsv{···w–oTbó^ÑZ°–VV?V°Z±Z±^Ñ^ÒZ¯bógk4suw–{·×w–oT^ÒVw¼bÑoUsuw–{·×{·sug^ÑZ° ¢  3 gk4oUw–{·×{·oUbòVw¼^ÑoUsuw–{·×{·oUbòZ±V 
E‰E‰E‰EŠEŠEŠIŠIŠIªIªIªI«
M«M«
M«MË2MËMËM«M«I«IªIªIªIªIŠEŠIŠ
EŠEŠ3 gk4oUsv{·×{·subòVw¼^ÑoUsuw–{·×w–k4^ÒVV 
E‰E‰EŠEŠEŠIŠIªIªIªMª
M«M«MËM«:MËMËM«I«I«Iª"IªIªIªIŠ
EŠEŠ7 k4k4oTsu{·×{·subóVw¼^ÑoUsuw–{·×w–k4ZÑVV E‰E‰
EŠEŠIŠIªIªIª
M«M«MËM«:MËMËMËM«M«IªIªIªEªEŠEŠEŠEªEŠEŠIªIªIªKIªIŠIŠEŠEŠEŠ k4k4oTsuw—·{·sucVw¼^ÑoUsuw–{··w–gZ±RoV E‰EŠEŠEŠEŠIŠIªIªMªM«M«M«M«MËMËMË
MËQËQËMËQËQËQËMËMËMË#M«I«IªIªEªEŠAŠA‰A‰AŠAŠEŠEŠEŠEŠIªIªIª;IªIŠEŠEŠ k4k4oTsuw–·{·svgVw¼^ÑoUsuw–··w–gZ°RoV 
EŠEŠIŠIªIªIªIªM«
M«M«MËMË"QËQË'QËMËMËMËI«I«NZ’^öcV“Eí=‰=i=i=iA‰A‰
EŠEŠEŠIªIªIª?IŠIŠ k4k4oTsuw–·{·svgVw¼^ÑoUsuw–··svgV°RoV EŠEŠEŠIŠIªIªIªI«M«M«M«MËMËMËQËQËQËQÌ
QËQËQËQÌ
QËQË{QËMËMËMËMíZ³ZÕNrNrV´g8s¼^÷A¬9H9H9i=i=iAiA‰A‰E‰EŠEŠEŠEªIªIªIªIªIŠ k4k4oTsuw–·{·svgVw¼^ÐoUsuw–··svgV°RoV EŠIŠIŠIªIªIªIªI«MªM«M«M«MËMËMËQËQËQËQÌQËQËQÌQÌQÌQËQÌQÌQËQËMËMËRZÔJrBA«=Š=Š=«J0o{k[Eî5H5(5H5H9H9H=i=iAiA‰EŠEŠIŠIŠIªIªIª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV EŠIŠIªIªIªM«M«M«MËMËQËQËQËQÌQÌQì
QÌQÌQìQìQËQÌQÌQÌQËMËQìV´FQEí=i=i9i9I5H5H=¬kYo|Aï>>B9‹1(5'5H9H=iAiA‰EŠEŠEŠIªIª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IŠIªIªIªM«M«M«MËMËMË
QËQËMËMËM«M«I«MË
MËMË‡QËQÌQìQìQÌQËQìQËQËMËZ“N“JAŠ=i9i9i9H9H5H5HFQs›gZJ´[Y_YW7Jr9‹-1'5H9H=iA‰EŠEŠEŠIª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IŠIªIªIªIªM«M«M«MËMË
QËQË£QËMËMËMËI«E«5K$Ég‰(é5*AE­9kIÌQÌQìQìMËQËQÌQÌQËV/V´N0AŠ=i=i9i9i9H5H=¬1¬%JB0{ÞZ÷[8WRö[8_YF/(æ-1'9H=iA‰EŠEŠIŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªM«M«M«MËMËMË¯MËQËQËQËQËMËMËMËI«I«9k‰IjiH%$9kMËMËQìQìMËQÌQìQÌMËZ’RÔEÌA‰=i9i9I9H=ŠBBqNÔRöJ´gYo{BQW7cz_Y[8W=î$Å(æ1'5H=iAiEŠEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªMªM«M«M«MËMËMËQË
QËQË§QËMËMËI«Eª(êŠ-1P-/$Ìi& =¬EªI«MËUìUìMËQìQìQìR_NrAŠ=i9i9H9IBJ³Wczc›g»cz[{ÝJR9ï[7[8W7WRö9Í Å(æ1'5H=iAiEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªM«M«M«MËMËMË³QËQËQËQÌQËQËMËMËI«Eª1+‹=“JA´1/ «H% )	=iAŠI«MËUìUìQÌQìQìQÌVP_J=i9i5H=ÌJrS[Y_y_y_Y[X[8[Xs¼c%J5îWWRöNÕJ´1j Å(æ1'9H=iE‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
IªIªIªI«M«M«M«MË
MËMËMËQË
QËQË«QËMËMËI«EŠ=Œ ¬AµJEÔ1P ËhF%#†5I9HAŠIªMËUìUìQÌQìQìQÌZ’_7A«9i9ŠFPS[X[8W7[8[X[8_ycz_Ykzoz5­%J9ïRöNÕJ´F“>1 æ Å)1'9HAi k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
IªIªIªM«M«MËMËMËQËQË»MËMËI«AŠ)C‡1O9r(íŠgggfD-)-9HA‰I«MËUìUìQìQìQìQÌ^´_=ŠFRõ[8[8SS[8[X_Yczcšcz_yczsœB%J%I>0J´FrBr>0-‹¤$Å-5H=i k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªIªI«M«M«M«MË
MËMËÇMËQËQËQËQÌQËQËMËMËI«=Š…"#"Cghg‡ Ë Ëˆ¦$æ1'9IAŠI«MÌVUìUìUìQìMÌg[N³[8W7NöNõW_Yczg›g»g»g›_y[8[8s¼Ns-¬!(%JBQBQ>061ÎÆ¤(æ1'9H k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªI«MªM«M«MËMËÇQËQËQËQÌQËMËMËMËIªE¬c"CCB""e‡¦©§ Æ Å(æ5(=iEŠMËQìVVUìUìQìMíFr>QBrF“JÕS[Xcšg»k¼k¼g»cz_Y[8VöRõsœR“:!(-‹:5î1Í-¬)I„ Å-5H k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªIªM«M«MË
MËMËËMËQËQËQËQÌQËQËMËMËI«E«„"CCBB!!#…¦…ƒ ¥(æ1'9iAŠI«MÌQìZZUìUìQìB/-Œ%I!(!I1ÍNÕk¼k¼g›g»g›cz[8WRöNÕJ´o›R“:!Iç1­1Í-Œ%j%Iƒ¤(æ1' k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªMªM«M«M«MË
MËMËMËQË
QËQË»MËMËIªIª=h"#CBB"!!ƒbƒ„ Å(æ1'9HAŠI«MËQìUìZZUíUìN/%jæÆÅ(Brcšg»cz_Y[7RöNÕN´J“F“kzRr9ï)kæç%I)‹%j!I!)b¤$æ1' k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªI«M«M«
MËMËMËQË
QËQË¿QËMËMËIªIªji~È^ Ä"B"! ƒcƒ¤$Å-1'9IAŠI«MËQìUìVZZVUìBçææÆÆÆÅÆ>0cz_8WRÕN´J“F“BQBQgYJQ5Î)kæææ!I!Ib¤$Å1 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MªM«M«MË
MËMËMËQË
QËQË7QËMËI«I«ni~É~ê~ÊfH$ã!!!„ƒ¤ Å(æ-5H=iEŠI«MËQìUì
ZZVQì-¬ÆÆÆoÅæ:0RöNÕJ´F“Br>Q:>0gYB-¬%JÆÆÆççÆb¤$Å1 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«M«MË
MËMËCMËQËQËQËQËMËMËI«Eªbl~é~êz©nh^I…‚!¤ƒ¤$å-5'9iA‰EªMËQÌQìVZ
ZZVUì-‹ÅÆÆkææ:J“FrBQ>06BQN“F19Í)‹!(ÆÆÆæææ!b¤$å1 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«MËMË
QËQË7MËMËI«Eª^L~È~ê~Ên‰^MÆ=D$Â çƒ Ä(æ19H=iEŠI«MÌQìUìVZZZUì9ÍÅ
ÆÆoæçç„„>0>Q:5î1ÎZ÷F09Í9Í%JçÆÆÆæçæ!c¤(æ1' k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«
MËMË?MËQËQËQËQËMËMËI«EªZ
~È~ê~êr‰^'MÆAe,â1Gƒ Å(æ1'9HAŠIªMËQìUìVZZZ-ZZUìFÆ
ÆÆoçç!Æ  :05î1Í1­ZÕB=Î=î!(ÅÆÆæçç!)Æƒ Å-5( k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«MË
MËMËÇMËQËQËMËMËMËI«Eª^L~¨~ê~Êr‰b'MÆA…11% æ Å(æ1'=iEŠI«MÌQìVZZZZ^.^.Z.Z^.^-ZVN%j)k-Œ)‹)j%J!)ç!   !-¬1Í)Œ%j>F0=î=ÎçÆæççç!)Æƒ¤(æ1'9H k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËÇQËQËQËMËMËI«EªV
~¨~ê~Êv‰b'QæE…5$,â-( Ä(æ1'=iEŠIËQÌUìVZZZZ.^.^.^.^.^-^.^.ZVQí5î1Í6>0BQBrBRc    Æ-­%j!I!)-‹1‹%)Æçç%I¥ƒ¤$å-5H=i k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMË?MËQËQËQËMËMËI«EªV,z‡~ê~êv©fHQÆE¥5$-5H¤(æ1'=iEŠMËQìUíZZZ^.^.Z.ZUìNP:>1BrF“RÕ:
  _ !)‹!I(æÆÆæç%I„ƒ¤$Å-5(=hA‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMË7MËM«IªMëz‰~ê~êzªjhZI…9D,â1F¤$å1'=iEŠMËQìUíZZ^-^.^.^.^.ZVR.BQF“J´RÕ[!)  [!IçæÆææç!(%Icƒ¤$å-5H=iA‰EŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMË3M«IªI«jH~é~êzÊni^M¦=D-5G Å$Å-9HAŠIËQìUíZZ^-^.^.^.Z.ZQìNQNÕW_X_Y¥  [„!IÆæçç%J%c„ Å(æ15H=iA‰EŠIª k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMË3MËM«IªI«jk~É
~êr‰b'QÆAe11$ æ Å-5HA‰I«QìUìZZ"^.^.Z-VRR´czg›czB  [B%jÆçç)JÆc¤ Å(æ1'9H=iEŠIªIªM« k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMË3M«IªEªbJ~¨~ê~Êr‰bHUæE¥5#-) Å)5H=iIªMÌUìZZ^-^.^.#^.b.^.^.b.^.^.^.^.ZUìR/_Yg›[8!  [!)kçç!()j¤ƒ¤$Å-5'9HAiEŠIªI«M«MË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMË3MËI«IªI«bJ~È~ê~Êr‰bHQæE…5$1%%¤(æ1'=iEªMËQìVZ^.^.^.b.
b.bNbNbNbN^.^.Z.ZQìRq[XW!  # )k!))Jcƒ¤$å-5H=iA‰EªI«
MËMË3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMË3M«IªEªbJ~¨~ê~êvÊf(QÆE…9D,â-'¤$å1'9iEŠMËQìVZZ^.^.^.b.b.bNbNbNbNb.^.^.ZUíMìNrNÕ!   !-¬!)%J)Jc„ Å(æ1'9H=iEŠIª
MËMË7QËMË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMË3I«EŠV,z¨~ê~Êv‰jHVI¦9D,â5h¤$å-9HAŠI«QìVZZ-^.^.^.
bNbNbNbObObO
bNbOb.^.^.ZQìMìJQ!  #c1­)j çƒ¤ Å)1'9HA‰EŠI«MËMËQË
QËQËg k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËI«IªA‰=eYæzªzÊjiYçI¦=e-1F¤$Å-9HA‰I«QÌUíZZ^.^.b.b.bNbNbNbObObObNbN^.^.VQìE« 
  “ „-¬1­Åƒ¤$æ-5H=iAŠIªI«MËQËQÌQÌQÌQËQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËM«IªEŠ=i-&9D^^M¦=e-1% Å Å-5HAiI«MÌUìVZ^-^.^.b.bNbNbObObObObNb.^.ZUíIËc
  “%J>Q5î¥¤ Å(æ1'5H=iEŠI«MËMÌQËQìQìQìQÌQËQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËM«IªE‰=h9i Ã(ãAe=e-1%$æ Å(æ5(=iEªMËQìVZZ-^.
^.^.^.b.bNbNbNbObObO/bO^.^.ZQì„   %(5¬%(¤Ä$å-5(9iAŠEªI«MËQìQìkQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËM«IªAŠ9H1'1I Ã$Â--)¤(æ1'=iEªMËQìUìZZ^.^.b.b.bNbN
bObObOfOfOfOfObO
bObO£bObN^.Z-Qìc  Æ$å¤¤ Å(æ1'5H=iEŠI«MËQÌQìQìQìUìUìQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËM«IªA‰=h1'(æ$å$å¢(ä$å¤(æ1'9hEŠI«QìUìZZZ^.^.b.bNbNbO
bObOfOfOfObO
bObOŸ^.^.V¤  -((æ$æ(æ-1(9IAŠEªIËMÌQìQìUìUìUìUìQìQìQìQìQÌQÌQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËM«IªEŠ=i5H-$Å ¤¤¤ Å(æ1'9HAŠI«MÌUìVZZ^.^.^.b.bNbNbObObObOfOfOfofofo
fOfO
bObO'b.^.V-( ¥5H--1'5H=iAŠI«MËQìQìUì
UìUìsUìQìQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËI«IªA‰9H5'-(æ$å(æ-1'9HAŠI«MËQìVZZZ-^-^.^.bNbN
bObOfOfOfOfo
fofo/fofOfObObObObN^.ZEÌ 5j9H5(1'5H=iEŠI«MÌQìUìUìUìoQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËM«I«EŠA‰9H5H1'1'1'5(=iA‰IªMËQìUíVZZZ^.
^.^.^.b.bNbN
bObOfOfOfofo7fofOfObObObOb.^.ZR!-(=i9H9H=iAŠI«MÌQìUìUíVVVUì
UìUì?QìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMË+MËIªEŠA‰=i=i=h=iAiEŠIªMËQìUìVZZZZ-^.^.^.bNbN
bObO;fOfOfofofofpfofofofOfObObObObN^.Z-( ¤AŠ=i=iAŠEªMËQìUì
VVKVUìUìUìUìQìQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËI«Iª
EŠEŠIªI«MËQìUìUí
ZZZ-^-^.^.b.bN
bObOfOfOfofo7fofOfObObObObN^.Z Æ ¥AŠAŠAŠEªI«MÌUìUíVZZVUíUì
UìUìGQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQË
MËMËM«I«I«MËMËQËQìUìUìZ
ZZZ^.^.^.bNbN
bObOfOfOfOfo
fofoƒfOfOfObObObObN^.Z.EÍ!=ŠE«EŠEªI«MËQìUìVZZVVUíUìUìUìUìQìQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQË
QËQËMËMËMËQÌQìQìUìUìVZ
ZZZ-^.^.^.bNbN
bObObOfOfOfo
fofofOfO
bObObN^.^.ZUìMÌMËI«IËMËQÌUìUíVZZZVUíUìUìUìUìQìQìQìQìQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËQËQËQËQÌQÌQì
QìQìQìUìUìVVZ
ZZZ-^-^.^.b.bNbNbObObObOfOfOfOfObO
bObObN^.^.ZVQìQìMÌMÌQìQìUìVZ
VVUíUì
UìUìcQìQìQÌQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQËQËQÌQÌQìQìQìQìUìUìUìUìVVZ
ZZZZ.^.^.b.bNbNbO
bObOfOfOfObObObO'bN^.^.^.ZVUíUìQìQìUíVVZZVUíUí
UìUì
QìQìOQìQËQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQËQËQËQÌQìQìUì
UìUìUíVVZ
ZZZZ-^.^.^.b.bNbNbObObNbNbNb.^.^.Z.Z
ZZVUí
UìUìQìUì
QìQì_QÌQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQËQËQËQÌQìQìQìQìUìUìUìUìVVZZZZ-^.^.^.^.b.bNbObObObObNbNbNb.b.
^.^.^.^-
ZZVV
UìUìQìQìSQìQÌQÌQÌQËQË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQËQËQËQÌQìQìQì
UìUìUìVVVZZZ-^.^.^.^.b.bNbObObO
bNbNb.^.
^.^.^.ZZZVVVUìUìQì
QìQìQÌQìQìQÌQÌQÌ
QËQË3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMËQËQËQÌQìQìQìQìUìUìUìUìUíVVZZZ-Z.^.^.
bNbNbObO
bObNbNbObNbNb.^.^.^.'ZZZVVUìQìQìQìQÌQÌMËMÌMËQËQËQËQÌ
QËQËOQËMË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËMËQËQËQËQÌQìQìQìQìUì
UìUìVVZZZZ-^.^.^.^Nb.bNbNbNbNb.b.^.
^.^.^.ZZZVUìUìQìQÌQË"MËMË3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMËQËQËQÌQÌQìQìQìUì
UìUìUíVVZZZZ-Z-Z.^.
^.^.^.b.
bNbNbNb.b.b.b.^.^.^.Z-ZZVUìQìQìMÌMËIËI«Iª
IªIªI«I«M«MËMËMË3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMËQËQËQËQÌ
QìQì
UìUìUìVVZZZZ.^.^.^.^.b.^.^.b.^.^.^.3^.ZZZVUìQìQÌQìnîj®^MQëEªAŠAŠEŠEŠEŠEªIªIªI«MË
MËMË3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMËMËQËQËQÌ
QìQì
UìUìUìVVVZZVUí]ÍeJeQIQìVZZZ-^.^.^.kZ-ZZVUìQìMÌMËjîwOrïrïrïnÎbmMë=i=iA‰AŠEŠEªIªI«M«MËMËMË k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
MËMËMËQËQËQË
QìQìQìUì
UìUìVV
ZZVUìUìij}	}K}*tÇD¥MÌQìUíZZ-^.^.Z.ZZZVUìQìMÌMËo°w/rïrïKIÊ9H=h=iA‰EŠEŠIªIªI«MËM« k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËQËQËQÌQìQìQìQìUì
UìUì/UìVVVVUìUìUìm}K}Ï}ð}Ï}ld¦4„MÌQÌUìZZ^.^.^.^.Z.ZZZVUìQìMËMËjî°°w
rïrïOj®ZKA«5'5H9H=i=iA‰EŠIªIªI«M« k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV MËMËQËQËQÌQÌ
QìQìUìUì+VUìUìQìQìmJ}K}ð~2~2}ð}Œlè<B5(I«QÌUìZZ-^.^.wZ-ZZZVUìQìMËIËjî°°rïrïrïnÎblR
N--1'5'9H=H=iA‰EŠEªIªIª k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«MË
MËMËQËQËQËQÌ
QìQìUìUì«QìMËeŒ}*}ð~2~S~}Ï}K\Ç4B(BAŠI«QÌUíZ^^.^.^.^.Z.^.ZZZZVUìQìMËIËjÎ°°±‘srïfV+R*V*^ŽZo^=‰-1'5H9H=iA‰EŠIªIª g3k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«
MËMËMËQËQËQËQÌQìQìQìQìUì
UìUì¯QìQÌMË]Í}	}Ï~2~2~2}Ï}lm	H… ! 9iEŠMËQìZZZ^.^.ZZZZZVVUìQìMËIËjî°°±²FrÆZLR*IÉ£^¯•f¯ZL^V-1'1'5H=H=iA‰EŠIŠ gg3k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«
MËMËMËQËQËQËQËQÌQìQì
QìUì/QìQìQËMËYÌ}	}®~2~2~}Ï}liL¥,B  5(AiI«QìVZZ{VVUìQìMËI«fÍ°°±²F b…NR*bŽ5ŠæAÉb{S{1rîMê-1'9H=hAiE‰EŠ ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«M«MË
MËMËQËQËQÌQÌQìQì/Qì]‹UI]Iy	}®~2~2~}®}KhèL¥0B  $å1'=iI«QÌUíZZ{ZVUìUìQìMËIË^±°±²F{ { V(VMs¶w2‚ jñ{1rïrïf-15'9H=iAiE‰ ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«MËMËMËQËQËQË
QìQì3QÌQÌQÌm}	l…y*~~S~}®yJ`ÇD„(B  „$Å1'=iEªMÌUìZZVUìQìQìMËI«^´k0°²²F{ { ráb{R‘f®V+RMN-f®rïrïrïrï9h-1'5H=HAiA‰ ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV M«M«M«MË
MËMË{MËQËQËQËQÌQÌQìQÌQÌMËI«MjuK}lÆyl}ð}®u*XÆ<c!  c¤(æ5'=iI«QÌUìVZZZZVUìQìQìMËI«Zrg8{o²±E{ { { { {" rÂVLs”{0
rïrïKrïMê(æ15'9H=iA‰ ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªM«M«M«MËMËQËQË
QËQÌSMËMËIªEŠDçuŒyl`¥mm	P¥,B   bƒ Å-9HAŠI«QìUìVZZVUìUìQìQÌMËI«UHs9g5‘±{${ { { râw
Irîrð
rðrðKrðf(å-5'9H=hAi cgk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªM«M«MËMËQËQËSMËI«EŠAŠV·Mylm	HcDd("   Bƒ¤(æ5'=iEªMËQìUíVVVUìUìQìQÌMËI«Q«xÆo|kU°{${ { { [{nÌj«f©fˆbgb†R'(å-5'9H=hAi bògk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªI«M«M«MË
MËMË[MËQËQËQËQËMËM«EªEÍc;{ÿ_HÇlèL… !   Bƒ¤$å1'9HAŠI«QÌQìUìUíUíUìUìQìQÌMËI«R/m)}kok2#{ { _ { b`R R N MàR =ƒ Å(å-5'9H=hAi cgk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIªIªM«M«M«MËMË/IªIîkžwÿo¿c^1¯(!@c   Bƒ¤$å19HA‰IªMËQì
UìUìUìQìMÌMËI«UîkZq)yïo|bðzà{ { [và{ {  vàZ@MàR N R R-$¤$Å(å15'9H=hAi ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
IªIªM«M«MËMËSMËI«Nk¾wÿk¿[>è çB  bƒ¤$å-9HA‰EªMËQÌQìUìUìUìQìQìMÌMËI«UÍwkZukyÎg:^örá
{ { _{ V R{ rÀV MàR MàR Iã Ã¤ Ä$å-1'5'9H=iAi ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
IªIªMªM«M«MË
MËMË»MËM«I«RQo¾wÿk¿[9òÇ„b¥B ƒ¤$å-9HAiEªMËQÌQìUìUìUìQìQìMÌMËI«Yï~vw[kZuŒy(fµZùbÍ { {  b`RnÇ#f€R N R N V 9„ƒ£ ¤$Å(æ-1'9H=HAiAi ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
IªIªËIªI«M«M«MËMËM«M«I«IªN1sßwÿkŸVû5ÑÆƒƒƒƒ„ ¤$å-5H=iEŠI«MËQìQìUìUìQìQìQÌMËI«]ï~v~—w[kZmjp aJ[VÖnè { { nÀvå{'b`MàN N R R")ƒ£ ¤$Å(å-1'5'9H=hAiA‰ ggk4oUw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIª
M«M«3IªIªEªRSsÿwÿgŸRÚ1°Æ%¤¤¤¤ Å$Å(æ19H=iEŠI«MËQìQìƒQËMËI«]ï~—~—~¸s:gz]jd \ V1VÖR•nê   vàZ@MàR N V J£ƒ¤ Ä$Å(å-1'5'9H=hAiAiE‰ gg3k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIª3IªEŠVssÿsÿgRÚ-Ž¦ Å¤¤ Å$Å(æ)-5'9H=iEŠI«MËQÌ
QìQì‡QìQÌMËMËYî~—~¸~¸~—n×_9Zs\ X P!R1N•Js^¯w&ràV MàMàR ZB9dƒƒ¤ Ä$å(æ-1'5'9H=H=iAiE‰EŠ g3k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIª3EªRtsÿsßc^N˜)m! æ¤ Å$Å(æ-1'5(9H=iA‰EŠI«MËMËQì
QìQì‡QÌQËMËI«nS~¸~¸~—~4n3Z÷Z÷UJP L H I‹B5Ï=ïJ-J(RGVhN*)ƒ£¤ Å$å(æ-1'5'9H=h=iA‰E‰E‰EŠ k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV IªIª7IªE«Z×sÿoßc^Jw!+¦$æ¤ Å$å)1'5H9H=iAiEŠEªI«MËMËQÌQì
QìQì‹QÌMËMËIª~—~·~vzuÑq^SVÖVµUkH @ < 8 0¥))*5®JS9®ƒƒ¤ ¤$Å(å-15'5G9H=hAiAiA‰E‰EŠEŠ k4k4k4suw–··svgV°w¼^ÐoUsuw–··svgV°RoV IŠIŠ
IªIªá_IªEŠEŠZ×wÿoß_=Fv!*ÆÅ ¤ Å$æ-1'9H=iA‰EŠEªIªM«MËMËQËQÌQìQìQìQìQËMËMËI«Eª~—~vzuÑqmniŽVsN”N”QÎH„< 8 0 ( $  !¥ƒƒ¤ Ä$å(æ-1'5'9H9H=hAiA‰A‰E‰EŠEŠEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV EŠIŠIŠIªIªEŠEŠEŠV•wÿo¿_=BU
%) Å¤ Å$æ-5'9HAiEŠEªIªM«MËMËMËMËQËQÌQÌQìQÌQÌQËMËMËI«EŠr3yóq°qmniNi,aMN1FR:9îA­=J=IAk ¥ƒƒ¤ ¤$Å(å,æ-1'5'9H=H=iAiA‰E‰EŠ
EŠEŠ7 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV EŠIŠ
EŠEŠÇE‰NRsÿo¿[>4È¥Å ¤ Å$å-5'9HAiEŠIªI«MËMËMËMËQËQËQËQËQÌQËQËQËMËMËM«I«EŠY®q°mmniNe-a,`êTÉE
9J-*)*=m9K¤ƒ¤¤ Å$å(æ-15'5H9H=hAiAiA‰E‰E‰EŠEŠEŠIŠEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
EŠEŠ+EŠE‰EÍo¾k¿Vû5ÑÈ%¥¤ Å$å-5'9HAiEŠIªI«
MËMËMËQËQËQËSQËMËMËM«IªEŠEjiŽmniMe-a\êT¨HfHE@$DFHÊ,¦ƒ„¤ Ä$Å(å,æ11'5'9H=h=iAiA‰E‰E‰EŠEŠEŠ
EŠIŠc k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV EŠEŠEŠE‰A‰A‹g[kŸR¹)mÆ%¤ Ä Å$å-5'9HAiEŠIªIªM«MËMË
QËQËKMËMËMËM«IªEŠ=iMkiMe,a\êP¨Hf@$@$Hg@É„„¤ ¤ Å$Å(æ-15'5H9H=h=iAiA‰E‰EŠ
EŠEŠ;EŠIŠIŠEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
E‰E‰+A‰ZÖkŸFv!+Æ$æ Å Å Å(æ-5'9HAiEŠEªIªM«MËMËMË·MËM«IªEŠAi=iMJ]XÉL‡DE@$@EH¨0Ç¤¤ ¤ Å$Å(æ,æ11'5'9H9H=iAiA‰A‰E‰E‰EŠEŠEŠIŠIŠIŠEŠEŠEŠ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV E‰E‰E‰A‰Iîc~9ò è-($å Å Å$Å(æ-5'9HAiEŠEŠIªIªM«MËMËMË?M«M«IªEŠA‰Ai=HA(DÇDE@$@f<È(Æ Å Å$Å$Å(æ,æ-1'5'5H9H=h=iAiA‰E‰
EŠEŠIŠIŠEªEŠ
EŠEŠg k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV E‰E‰A‰AiAŠ9¯)-(æ$æ$å$æ)15'9H=iE‰EŠIªIªMªM«M«M«MËMËMËM«I«IªEŠA‰Ai=i9H5'1'-,æ
(æ(æ(æ,æ-15'5'9H=H=iAiA‰E‰EŠEŠIŠIŠIŠEŠ
EŠEŠW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV E‰E‰AiAi=i9H1'--(æ)-1'5(9HAiE‰EŠ
IªIªMªM«M«M«M«MËMËMËCM«I«IªIŠEŠE‰Ai=h9H5'5'1----115'5'9H9H=h=iAiA‰E‰EŠEŠEŠEŠIŠ
IŠIªcEŠIŠEŠEŠEŠE‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV E‰A‰AiAi=i9H5(1'1-1'5'9H=HAiA‰EŠEª
IªIªIªI«MªM«M«M«MËM«
MËMË7M«I«IªIªEŠEŠAiAi=h9H9H5'5'5'1'5'5'5(9H9H=H=iAiAiE‰E‰
EŠEŠIªIŠIŠIŠEŠIŠ
EŠEŠ[E‰E‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV E‰AiAiAi=i9H9H5(5'5'5H9H=hAiA‰EŠEŠIªIªIªIªI«MªI«M«M«#M«MËMËM«M«I«IªIªIªEŠE‰AiAi=i=H9H9H9H=H=iAiAiA‰E‰
EŠEŠ
IªIŠEªEŠ
EŠEŠ;EŠE‰E‰E‰ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
A‰Ai=i=h
9H9H=H=iAiA‰E‰EŠIªIªCIªI«IªM«M«M«MËM«M«M«M«IªIªIªIªIŠEŠE‰A‰Ai=i=i=i=h=H=h=i=iAiAiA‰E‰
EŠEŠEŠIŠIªIªIŠIªEŠEŠ
E‰E‰3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV AiAi=i=h=i=iAiAiA‰E‰EŠIŠ&IªIªM«M«M«I«I«Iª
IªIªEŠEŠE‰A‰A‰AiAiAiA‰E‰E‰EŠEŠEŠEŠEª
IŠEŠEŠEŠE‰E‰3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV AiAiAiE‰E‰E‰EŠEŠIŠIª6IªIªIªEŠEŠEŠ
E‰E‰A‰A‰E‰E‰EŠEŠ
IŠIŠEŠEŠE‰E‰3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV AiAiAiA‰E‰E‰E‰EŠEŠEŠEŠIŠIªIŠIŠIª2IªIªIŠEŠEŠEŠEŠE‰
E‰EŠEŠEŠ
EŠIŠEŠEŠE‰E‰7E‰Ai k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV AiAiA‰A‰A‰E‰
E‰E‰E‰EŠEŠEŠEŠIŠIŠIŠIŠIª.IªIªIªEŠ*EŠEŠEŠE‰E‰E‰7AiAi k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV 
AiAiAiA‰A‰A‰A‰E‰E‰E‰E‰EŠEŠEŠEŠIŠ
IŠIŠ.IªIªIŠIªIªEŠEŠEŠIŠEŠEŠEŠEŠE‰E‰E‰;A‰AiAiAi k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ž$Å$Å7$Å(å k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢YëYë3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV fjMjMj-j-j-jM6jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV zjMjMjM^Eh5&5&9'MªbjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV zjMjMjMEˆ(Åb ƒ5'UëjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV FjMjMjMQÊ5'=GEhYë*jMjMjM5'b   ¤^jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMjM5&9'E‰.jMjMjM9'£!(åQÊ*jMjMjM9'
   fNjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMjM9G b..jMjMjM=G! AbN*jMjMjM=Gƒ   (ænpjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMjMI©jo.jMjMjME‰(åAE«zó*jMjMjMQª9G ¤ )bpvÑjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV FjMjMjM]ëUËjor*jMjMjMbYëb.jor±vÒnnjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV zjMjMbQª=H5&111=HM©Uëb,jMjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV vjMjMjMQÊ9'$Åƒb! !ƒ$Ä1E‰YëjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV FjMjMUëE‰9G555&=HMª]ëjMjMjMMª,å£!   A(åEhYëjMjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV BjMjMf,I‰1 Äb! !ƒ,å=GQÊf-jMjMjMjMQÊ,åƒ   $ÄEhYëjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMjMf,Mª=G15&9'E‰UÊf-"jMjMI‰-ƒ   ¤AhYëjMjMjMb9G£   (åQÊf-jM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMf,Eh(åƒA A ¤5'QÊf-jMjMjMjMYë1ƒ   $ÄQªbjMjMQª(åA   BAh]ëjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMMª(åb 
  ƒAhYëjMjMjMjME‰$Ä   5'YëjMjMAh£   (åUëjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjM=Gƒ   (åUËjMjMjMjM=Gb    ¤^jMjM9Gb   ƒ^jM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjM5&A   b^jMjMjMjM5&!   Bb.jMjM5&!   Ab.jM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjM5'   bNjMjMjMjM5'   bNjMjM5' "  bNjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjM9GA   ƒnjMjMjMjM5'!   BfNjMjM5'!   AfNjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMI‰ Ä   =izÒjMjMjMjMAhƒ   (æjojMjM=Hb    ÅjojM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMUë9'ƒ 
  )n±rjMjMjMjMQª,å   Qìv±jMjMM©$Ä   AŠvÑjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMf-UÊE‰(åb ƒAŠnÒvÒjMjMjM^Ah ¤   1'zónojMjMYë5&A   ƒjprjM
jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV jMjMjMf-^Yëb.jor±zór°jM"jMjMUÊEh$Å   5(vÒv±jMjMjMf,I‰(å   IÌzójMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV BjMjMf-YëUë9' ÄB b)V4vÒjNjMjMjMjM]ëI‰$Ä   AŠVrjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV FjMjMb^b.jojpnr±vÒrjMjMjMYëI‰,æB   ƒIÌVv±jMjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV vjMjMjM^UËE‰-£A A ÅEªj±{rjMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV zjMjMf-^Yëb.jojpnr±zòr°jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢jMjM3 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV ¢  _ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>˜>x
:x:x:X:W
6W6W6W%°ÈVyVyVyVxVXRXRXVXVXVXVxVxVxVyVy_ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>˜>x
:x:x:X:W
6W6W6W%°ÈVyVyVyVyVxVXRXRXRXRWRWRXRXRXVXVxVxVyVy_ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>˜>x
:x:x:X:W
6W6W6W%°ÈVyVyVyŸVyVXVXRXRXRWR4RRVRVRZRVRV1RNNJJN3N6N7N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>˜>x
:x:x:X:W
6W6W6W%°ÈVy
VyVy£VyVXVXRXRVR2ZRbuj˜j˜fxfWb6^UôUÓQÓM²M±I°II¯IÎIðJN6N6RWRWRXRXVxVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>˜>x
:x:x:X:W
6W6W“6W%°ÈVyVyVyVyVxVXRWR4ZRfwnºnºn™j™fxbW^5YõUôQÓQÒM²M²M±I‘I‘I‘IEEIÎFJN6N7RWRXRXVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W—6W%°ÈVyVyVyVXRXRUZSj˜vÛrÛnºj˜fWb6^YôUÓQÓQ²Q²M²M’M’M‘M‘I‘I‘I‘EEpEoEŽE­EòJN6RWRXRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W—6W%°ÈVxVXVXRXR2buvÛvürºj™fW^YôUÔUÓQ³Q²Q²Q’Q’M’M’M’M’M‘M‘I‘I‘IEpEoAOAOAEÏEôJN7RW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W'6W%°ÈRXRXRWR2j˜vüvÜnºfx^6 §EpQ³$¨9-Q’Q’cAO ¨IpM‘M‘I‘IpEpAO=N=NAmAÎAóJN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W'6W%°ÈRWRWN3fvrÛrÛn™b7YõUÓ 9,Q²"(ÊQ’Q’c=-!IpM’M’M‘I‘IpEoAN=.9-=m=®AÒJ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W—6W%°ÈN6N5Vj™nºjx^6UÔQ³Q² 9Q’",êQ’IpM‘=.M’Q’Q’=.9-"IpQ’Ep=.M‘M‘IpAO=N9-9-=Œ9AÓ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W—6W%°ÈFIñZbWbW^UÔQ³Q’Q’ 9Q’",êEpdC (ÉQ’,ê e"IpIqC  §M’I‘EpAN9-9-=M1M9° k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W—6W%°ÈAÓIðUôZZUÓQ³Q’Q’Q’ 9Q’",êEO 9- §CQ’" §,ë M‘$ÉCAO EOM‘IpAO=-9=-)+1n k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W—6W%°È=±EÏQ²QÓQÓQ²M’Q’Q’Q’ 9Q’",êAO"Iq9CEp AOAN"M‘d(ÉQ’!0ëM‘IpAO=-59- é-L k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü iuiuiuiu |õ|õ |u|u|u|u 
6W6W“6W%°È9A®M‘I‘I‘I‘M‘M’M’Q’ 9Q’",êAO"Iq5CEp AOAN"M‘C(ÉQ’!(ÊM‘EpAN9-59-È)+ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W—6W%°È59ŽIpEpEpEpIpM‘M’M’ 9-Q’"(ÉEO"Iq5BM‘ =NAN!M‘e §Q’1IpAO9-559,Ç% k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W—6W%°È55n=mAOANAOEOEpIqM‘!‡AN 5AO"Iq5"Q’"†(É Iq$É"0ë ANAO9-50ë51
Ç% k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W‹6W%°È951M=M=.=.=.ANEOEp1  CM’=NCIq9CQ’5 edEpIqC  ¨=.9-10ë0ë5¦È)+ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W+6W%°È=±9°1n-+=M9-9-9-=-=NAOEOEOIpIqIqM‘
M‘M‘[M‘IqIqIqIpEpAO=.9-50ë,ê0ë1¦¦ é-L k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W—6W%°ÈAÒ=Ò5-,)
5+9-5599-=.=NAOAOEoEpEpIpIpEpEpEpEOAOAN=.9-50ë,ë,ê1-
¦†È)+5n k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W6W%°ÈEôEó=±5n)+ é(é5,5
559-9-=-=.
=.=.g=.=-9-9550ë,ê,ê,ê1
§…†È%
1n=± k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû (
(
(
(
 }ÿ}ÿ }}}} 
6W6W'6W%°ÈJ6JEó9±1n)+ é§(é15555555g510ë0ë0ë,ë0ë0ë,ê Çd…¦È%1n=±Eô k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû (
(
(
(
 }ÿ}ÿ }}}} 
6W6W36W%°ÈNWN6JEó9±1n), êÈ¦Ç$È1
1555110ë0ë
11_1
,ê Ç…d…¦Ç é),5o=ÒJN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü (
(
(
(
 }ÿ}ÿ }}}} 
6W6W/6W%°ÈRXRWN7JEó=±5-M%+ éÈ§†……„ddc
cc_ddde……¦Ç é)+1M9°AóJRWRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû:  
6W6W/6W%°ÈVXRXRWN7J6EôAÒ9°1n-M)+%
 éÈÇ§¦¦†
††c¦¦§§È é%
),1n9°AÓJN7RXVXVX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü TÿTÿTÿTÿ TT TTTT 
6W6W+6W%°ÈVXVxRXRXRWN6JEôAÒ=±51n-M),%%
!
 é éc ê ê%
)+-L1n5=±AóJN7RXVXVXVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü TÿTÿTÿTÿ TT TTTT 
6W6W6W%°ÈVyVyVyVxVxRXRXVXVxVxVxVyVyC k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü TÿTÿTÿTÿ TT TTTT 
6W6W6W%°ÈVyVyVyVyVxVyVXRXRXRWRWRXRXRXVXVxVxVyVyC k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü TÿTÿTÿTÿ TT TTTT 
6W6W6W%°ÈVyVyVyVXRXRXRXVTZQ^obmffŒfŒbk^KZ+V,VRNN6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû TÿTÿTÿTÿ TT TTTT 
6W6W6W%°ÈVy
VyVy{VyVXVXRXVWZrfŽví{wvírìnÌn«j«jŠf‰bibi^H^HZ'VQëMñN5N6N7RWRXRXVxVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü TÿTÿTÿTÿ TT TTTT 
6W6W6W%°ÈVy
VyVy‡VXVXVTfwOO{.wvírÌnËjªjªf‰f‰fibhbh^H^H^HZ'V'VQçMÌJN6N7RWRXRXVx k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû TÿTÿTÿTÿ TT TTTT 
6W6W—6W%°ÈVyVyVyVxVXVUf°{/‘pO{vìrËnªj‰j‰fhfhfhbgbgbGbG^G^G^'Z'Z'VRQæMÇI¬IóJ6N7RXRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü TÿTÿTÿTÿ TT TTTT 
6W6W/6W%°ÈVyVyVXRXZsrï‘‘P{.vìnÊj©jˆfhfgfgfgbFbFbF[^G^G^'Z&VVQæQåMÅI¨EÑJN6RW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû:  
6W6W+6W%°ÈVyVXRXZr{/‘‘OvìnÊ(ã=dfg9D ÂbFfF
fFfFfFbF
bFbF[^F^&Z&VQåMåMÅMÅE†A¯EôN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W'6W%°ÈVyRXVTwpp.rëj©fha-^& =cfFfF_fFbFbFbF^&Z&VQåMÅIÄMÅ=f=°Eô k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W—6W%°ÈVXRXfŽ{.O{rËjˆfgffa1#9c ^%ffZA„bFffff^%A„Z%ff^%UåZA„bF^&Z&VMÅI¤I¤I¥9I=± k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû  (( (õ(õ(õ(õ 
6W6W—6W%°ÈRXR7j«wvìnÊjˆfgfffF1#@=cffbF  ¡ff^% @bFE„@ 5CbF^&ZQåIÄE¤IÄ5%5m k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû  (( (õ(õ(õ(õ 
6W6W—6W%°ÈRXN6jŠjªjªfˆfgfFfFff affff1#¡^% ^%MÄaff =cE¤ bF1#¡bFbF^%UåMÄE¤I¤5$-* k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W—6W%°ÈRWJbhbhbhbGbFbFfFff MÄffa1# I¤9C@1# (âE¤affQä ZbF^%QåI¤E„E¤5$(è k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W—6W%°ÈN7J^'Z'Z'^&^FbFfFffa5#¡ Âff1#A„^%5CA1#=cQåA„affMÄ Z^&ZQåE¤A„E¤1$ç k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W—6W%°ÈN6EôVVVVZ&^&bFbFa1#Qå Z%(â$âff1#UåE„ff=cA„E„ fFE¤aZZQåI¤A„A„E¤(Ã è k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü  (( (õ(õ(õ(õ 
6W6W‹6W%°ÈN7EôIìQåMÅMÅQåVZ%^%a-bFa(âA„ 5C ffQå@Ad MÄE„ 1#a ÂVQåI¤A„=cA„5#¤ é k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü:  
6W6W—6W%°ÈN7F=°MÈMÅIÄIÄMÅQåV$â9c^%A„¡bF Â@A„fFbF1#@-bF=c ÂaA„MÄE¤=c=c=c=d£¦ é k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ =ÿ=ÿ )))) 
6W6W—6W%°ÈRWJ=±5lI¦I¤E¤I¤I¤MÄQåVZZ^%^%^&^%bFbFbFbFZ^%Z%9C@QåIÄI¤A„=c9c=c=d£…Ç%
 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû õõõõ =ÿ=ÿ )))) 
6W6W+6W%°ÈRWJAÒ5-K=‡I¤E„E¤E¤I¤IÄMÄQåQåVV
ZZgVVUåQå- E¤A„=d9c9c=c5#£„¦ é-L k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ =ÿ=ÿ )))) 
6W6W—6W%°ÈRXN6Eô=±1m%
)A…E„A„A„A„E„E¤E¤I¤IÄIÄMÄMÄIÄIÄI¤E¤E„A„=c=c9c=c=c9C Ãd…¦ è)+5o k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ =ÿ=ÿ )))) 
6W6W'6W%°ÈRXRWJEó9°1M%
È)9EA„A„A„=cA„A„A„gA„=c=c9c9c9c=d=d5#$Ãcd…§ é)+1n=± k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû õõõõ =ÿ=ÿ )))) 
6W6W'6W%°ÈVxRXN7JAÓ91m)+ éÇÆ$å5$=dA„
A„A„k=c=cA„A„Ad=d9D1$$Ã¤dd…¦Ç é),5n=±Eô k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû õõõõ =ÿ=ÿ )))) 
6W6W/6W%°ÈVxRXRWN6JAó=±5o-L% éÇ¦¦¥¥¤¤„„„_„d……†§È é%-M5=±EôN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ =ÿ=ÿ )))) 
6W6W—6W%°ÈVyVxRXRWN7N6JAó=±51M)+%
 éÈ§¦¦¦†………†¦¦¦ÇÈ é%
)+-M5o9±AÓJN6RW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ =ÿ=ÿ )))) 
6W6W“6W%°ÈVyVyVxVXRXRXRWN6JEôAÒ9°5o1M),)%
 é é ééé é é é ê%
)+-,1M5n9=±AÓEôJ6N7RWRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W6W%°ÈVyVyVyVyVxVyRXRXRXRWRWRXRXRXVXVxVxVyVy7 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W6W%°ÈVyVyVyƒVyVXVXRXRXRWV3ZP^ObnbnbM^M^,ZVVRR2N6N6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W6W%°ÈVy
VyVy‡VyVXVXRXRVZ0bnjn°nnjpfOb.^YìUëUËQÊQªQ©QÉQêRNN5N6RWRWRXRXVxVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W—6W%°ÈVyVyVyVyVxVXRWVSbNnrÒrÒr²n±nfob.^^YëUËUÊQÊQªQªMªM©M©M©M©QÊNJN6N7RWRXRXVx k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W‹6W%°ÈVyVyVyVXRXVTfor±zôvórÒn‘jpfNb-^YëYËYÊUÊUÊU©Q©Q©Q©Q©M©M©M‰I‰IˆIˆMÉIñJN6RWRXRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W+6W%°ÈVyVxVXRXZ1jzó{vÓr±jof.b]ëYëYÊUÊU©U©cU©Q©Q©Q©Q©M‰M‰IˆEhEhMˆIÍEôJN7RW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W+6W%°ÈVyRXRXZ0n°zôzôvÒnM«B£Q©UÊ,å5U©U©U©_U©Q©Q©Q©M‰IˆEhEgAGEgIÌAÓJN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W+6W%°ÈVXRXV2jozózór±jOMª ,åAbU©b £U©U©U©[U©Q©Q©M‰IˆEgAGAGEgE¬AÒJ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü )õ)õ)õ)õ êê ÿÿÿÿ 
6W6W“6W%°ÈRXNW^,n‘r²n‘fN^1bU©Ih £U©b £U©U©IhEgU©U©U©IhAGU©U©MˆMˆIhQ©MˆIhAGAGAGIˆ9AÒ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W—6W%°ÈRWN6b,jojob.]ìYÊ £$ÄU©U©U©U©b £U©AG!!0åU©9&  £U©$ÄbAU©Q‰MˆEgAG=GEg=j9 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈN7J]ìb-^YëYÊU©ƒ(ÄU©U©U©U©b £U©B0åAG!Q©0å,ÄAG U©ƒ!EgU©U©MˆIgAG=&EG=H1n k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈN6EóUËUëUËUÊU©U©ƒ(ÄU©U©U©U©b £U©!,ä1 IhU©Mˆ,ä U©‚(ÄU©U©U©MˆIgAG=&AG9G-, k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈJAÒQ©QªQ©Q©Q©U©ƒ(ÄU©U©U©U©b £U©!Ab £Qˆ$¤ B U©‚(ÄU©U©Q©MˆEg=&=&AG5'% k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈJ=ÒMˆI‰IˆIˆMˆQ©(Ä‚U©IˆbQ©b$¤U© EgU©9&U© 9Qˆ U©‚$ÄU©Q©MˆIgAG9&9&AG)%
 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈJ=±E‰IhEhEhIhMˆ5 AGƒ!U©bBU©b £9AU© ,å(Ä U©‚$ÄQˆMˆIgAG9&59&9&Ç%
 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈJ=Ò5mEgAGAGAGEgIhƒ !=&U©AG ,åIh  5U©(Ä ‚ Q©ƒ$ÄIhEG=&9559&¦È% k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W—6W%°ÈJAÒ5o1*AG=G=&=&AGEGAGIhMˆMˆQˆQ‰MˆQ©Q©MˆQ©Q©Q‰MˆMˆMˆIhIgEG=&9&5159&¥¦ é), k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +ÿ+ÿ+ÿ+ÿ ÿÿ ÿÿÿÿ 
6W6W+6W%°ÈN6Eó9-L)	9G=&9&=&=&=&AGEGEgIhIhMˆ
MˆMˆcMˆIhIhEgEGAG=&9&510å55¥…È%
1n k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W6W%°ÈN6J=Ò5o), é-
9&9&9&=&=&='AGAGAGEG
AGAGW=F=&9&9510å0å5$Å„†Ç%
-M9 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü õõõõ õõ jjjj 
6W6W+6W%°ÈRXN6Eô=±5n)+ éÇ(æ5&9&9&95999&
9&9&g955511551$Åd…¦È%
-M9AÓ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ õõ jjjj 
6W6W#6W%°ÈRXR7JEó=±5n-,%
È§Æ(å5
9&9&o9555555551$Å¤d…†§ è%
-M9AÒJ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ õõ jjjj 
6W6W/6W%°ÈRXRXN7JEó=Ò51M)+ êÈ§¦……„ddd
cccddde……¦Ç è%
),1n9°AÓJN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ õõ jjjj 
6W6W—6W%°ÈVxRXRXR7N6JEó=²91n),%
 éÈÇ§¦¦††………¦¦¦§Çè é)-,1n9=ÒEôJN7RX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ õõ jjjj 
6W6W36W%°ÈVyVxVXRXRXRWN6JEôAÓ=±51n-L)+%
%
 é ééé
 é éS%
%),-M1n5=±AÒEôJN6RWRXVX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü õõõõ õõ jjjj 
6W6W6W%°ÈVyVyVyVxVxRXRXRXVXVXVxVxVxVyVyC k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû õõõõ õõ jjjj 
6W6W6W%°ÈVyVyVyVyVxVyRXRXRXRWRWRXRXRXVXVxVxVyVyC k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü õõõõ õõ jjjj 
6W6W6W%°ÈVyVyVy{VXRXRXRWRTRQRoVnVŽVVŒRlNLN,J.JJNN6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü:  
6W6W6W%°ÈVy
VyVy‡VyVXVXRXRVRqVŽbícc^í^ìZÌV«V«RŠR‰NiNiJHJHF(F
EîIóJ6N6N7RWRXRXVxVyVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü ````    àààà 
6W6W6W%°ÈVy
VyVy‡VXRWRTVgkOkOg.c^íZÌZËVªRŠR‰R‰NiNhNhJHJHF'F'F'FAéEÏJN6N7RWRXRXVy k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü ````    àààà 
6W6W—6W%°ÈVyVyVyVxVXRUZ°k/s‘opkOc^ìZËVªR‰R‰NhNhNhNgNgJGJGJGJGF'F'F'BBAæ=ÉA¯EôN6NWRXRX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû ````    àààà 
6W6W/6W%°ÈVxVxVXRXRrcs‘s‘kOg.^ìVªR‰RˆNhNgNgNgJFJFJF_JGJGF'F&BB=æ=å=Æ=ŒEÓJN6RW k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü ````    àààà 
6W6W+6W%°ÈRXRXRXRqkOs‘s‘kO^ìZÊR‰Ng!a!JFJF[JFF&F&B=å9Å9Å9Å9jA±FN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü ````    àààà 
6W6W+6W%°ÈRWRXRScopopg.ZËR‰NgNgJF-d¢ !-cJFJFWF&F&B=å9Å9Ä9Å5J=±Eô k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüJüJüJüJû ````    àààà 
6W6W—6W%°ÈN6N6Vg.kOcZËRˆNgNfJFJFJF=å NfNfJF-c=åNfNfF%-cF%NfJF9Ä1„JFF&F&B9Å5¤5¤5†1,=Ò k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü ````    àààà 
6W6W—6W%°ÈJN2Z«^í^ìZÊRˆNgJFJFNfNfNf9Ä NfJFâ  =åF%a@@F%-c  !JFF&B=å9Ä5¤9¥(É5 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû ````    àààà 
6W6W6W%°ÈAÓJ/VªVªR‰RˆNgJFJF
NfNfs9Ä Nf9Ä JFâ¡-caNf-cJF )CF%-cJFJFF%=å9Ä5¤9Ä §-M k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû:  
6W6W6W%°È=±FNhNhNhJGJFJFJF
NfNfw9Ä Nf5¤aJF5¤a=å âF%Nf%# 1„NfJFJFF%=å5¤1„5¤†)+ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +`+`+`+` àà ```` 
6W6W6W%°È9AíF'F'F'F&JFJFJF
NfNfw9Ä Nf1„AJF5¤@JF9Ä@aF%Nf%# )CJFF&B=å5¤1„5¤†!
 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +`+`+`+` àà ```` 
6W6W6W%°È5=ÎFBBBF&F&JF
JFJFw9Ä Nf9Ä@JF)CaNfJFB -cJFJF-c BB=å5¤1„1„5„† é k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +`+`+`+` àà ```` 
6W6W—6W%°È59=Ê=å9Å=å=åBF%F&JFJFJF9Ä JFB -c@!9Ä 5¤a9Ä¡¡%#a=å=å5¤1„-c1„%§ é k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +`+`+`+` àà ```` 
6W6W—6W%°È99°5Ž=Ç9Å9Ä9Ä9Å=åBF%F%F%=å¡JFJF1„@ÂJFJFâ@!JF=åa@-c9Ä5¤-c-c-c-D…§ ê k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJü +`+`+`+` àà ```` 
6W6W—6W%°È=±=Ò91l9Æ5¤5¤5¤9Ä9Å=åBBF%F%F%F&JFF%JFJFJFF%BF%BB=å9Ä5¤1„-c-c-c-D……Ç% k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû +`+`+`+` àà ```` 
6W6W'6W%°ÈAÒAó=Ò1n)J5‡5¤5¤1„5¤5¤9Ä9Ä=å=åBB_=å=å9Ä5¤5¤1„-c-c)C1„)$……¦ é-M k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJû:  
6W6W6W%°ÈFJEó9°1M%
%'1…1„
1„1„5¤5¤5¤9Ä
9Ä9Ä9Ä5¤5¤1„1„-c
-c-cG)D¥e…¦ é),5 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü#JÛJÛJÛFÛFÛFÛFÚFÚFºBºBºBºBºB¹B™>™
>™>™>x:x
:x:x:x:W
6W6W'6W%°ÈN6N6JAÓ9-M%
È!-e1…1„1„-c1„1„1„1ƒ-c
-c-cw-d-d)$Åed…Ç é-,5AÓ k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºB¹B™B™
>™>™>x>x
:x:x:x:W
6W6W'6W%°ÈR7RWN6JAÓ9-M% éÇÆ å)E1e1d
1„1„_-c-c1„1„1d-d-D)%Å…dd…¦È%
-M9°AóN6 k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü#JûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºBšB™>™
>™>™>x>x
:x:x:x:W
6W6W§6W%°ÈRXRXRWN6JAó9°1n),%
èÇ¦¦¥¤¤¤„„„„„„„„d……¦§È%
-,5=ÒEôN6RX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNüNüJüJüJüJÛJÛJÛFÛFÛFÛFÚFºFºBº
BºBºB™B™
>™>™>˜:x
:x:x:x:W
6W6W/6W%°ÈVXVXRXRWN7JEó=Ò51n),% éèÇ§¦¦¦
††W†¦¦§ÇÈ é%-M5o=±EôJRWRXVX k4k4oTsuw–··svgV°w¼^ÐoUsuw–··svgV°RoV NüNü
JüJü#JûJÛJÛFÛFÛFÛFÛFºFºBºBºBºBºBšB™>™
>™>™>y>x
:x:x:x6W
6W6W/6W%°ÈVXVyVxRXRXRWN6JEó=Ò9°51m-L)+%%
!
 é
 é é[ é ê%
%),-M1n9=ÒEôJNWRXVXVyVy k4k4oTsuw–··svgV°w¼^Ðk4oTsu{·×{·w–oUgc """""""!!!!*""&!!""""/ gsvw–w—{·×w—oT^ÒRw¼kWk4k4oUw–{·×{·svoU
k4k4k4oToToT;oTk4k4gcbò^ÑZ±Z±Z°VV°V°Z°Z°Z°Z°Z±^Ñ^Òbòcggk3k4k4oToToTwoTk4k4k4ggbòbò^Ò^ÑZ±Z±Z±ZÑ^Ñ^Ñ^ÑZÑZ±Z±Z±ZÑ^Ñbòggk4oToToUoUsUsususvsvw–w–{·×{·w–k4Z±Row½{Ýk4k4oTsv{·×·{·svsuoU
oToToUoUoToTkTk4k4k4k4k3k3k3k4k4
k3k3k4k4oToToUoUoToToTk4"k4k4Sk3k3k4k4k4oToToUoUoUoUsususvwvw–w–{·{·×{·subóV°Ro{Þ  s™gk4oUw–{·×{·{·w–w–svsu–susu7svsvw–w–w–{·{·{··{·svk4^ÑVs›    {Þggk4suw–{···
{·{·{—w–Žw–w–w–w—{—{·{·{·7{·w–kTbòV°Row½      w»ggk4oUw–{·{·{··××{·{·{··Ž···×××{·{·{·w–svk4bòZ±Vs›	   sšbógk4oTsuwvw–{·
{·{·{··Š···{·
{·{·{·w–svoUk3bòZ±Vs›   w»bòbòggk4oToUsususvw–’w–w–w–svsvsuoUk4g3bó^ÒZ±sšs›   {Þsš^Ò^òbòbògggšk4k4ggcbò^ÒZ±sšsš{Ý     þ{½w›sšszsšsyžsysyszsšw›{½    h     
 à
 ÅO
 à%
  D0
 t

G ”(a 
  U
 d½ Àc
 sstartColorSelection:
 sselectColor:
 sendColorSelection:
 sdelete³  užÿ  (xá*   O?
O?O?àâO9O9
O9NyNyNy2yNy2y2yŒÆ O?O?O?O?O9O9O?ÞO9O9NyO9O9NyNyNy2yNy2y2y2yÆ O?O?O?O9O?O?O9àâO9O9NyO9NyNyNy2y2y2y2yÆ O?
O?O?ÞO9O9NyO9NyNyO9Ny
NyO9NyNy2y2yNy2y2y2y2yÆ O?O?O?O9O9O?O9àâO9O9NyO9NyNyNy2yNyNy2y2y2yÆ O?O?O?O?O9O9O?á  
2y2y2yÌ O?
O?O?O?O9 ÿáÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9 ÿŠÿÿ ÿzÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ÿŠÿÿ ÿzÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿZÿÿÿ   ÿÿÿ  ÿ
ÿÿ+ÿ   ÿÿ  ÿÿ ÿÿ  ÿÿ   jÿÿÿ 
2y2y2yÆ O?
O?O?O?O9 ÿZÿÿÿ ÿÿ
 ÿÿ 
ÿÿ ÿÿÿÿ ÿÿ ÿÿ jÿÿÿ 
2y2yy%) O?O?O?O?O9O9O9 ÿZÿÿÿ ÿÿ
 ÿÿ 
ÿÿ ÿÿÿÿ ÿÿ ÿ
ÿ nÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ÿZÿÿÿ ÿÿ
 ÿÿ 
ÿÿ ÿÿÿÿ ÿÿ ÿ
ÿ nÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿZÿÿÿ ÿÿ ÿÿ  ÿ
ÿÿ'ÿ   ÿÿ  ÿÿ ÿÿ  ÿÿ nÿÿÿ 
2y2y2yÆ O?
O?O?O?O9 ÿáÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9á  
2y2y2yÆ O?O?O?O9O?O?O9 ÿáÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O? ÿáÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 ß
ßß
ÞÞÞþFþþ
{þ{þ{þ{ÿF{ÿ{ÿ
{ß{ß{ßß:ßßßÿÿÿÿ 
2y2y2y%) O?O?O?O?O9O9O? ÞÞÞÞþ.þþþ{þ{þ{þ{þ{ÿ.{ÿ{ÿ{ÿ{ß{ß{ß{ßß.ßßßÿÿÿÿ 
2y2y2yÆ O?O?O?O9O?O9O9 ¾
¾¾¾½½½½ÝÝÝ&ýý{ý{ý{ýwýwýwýwýwþwþwþ&wÿwÿwßwßwßw¿w¿w¿w¿{¿{¿{¿&¿¿¾¾¾ÿÿÿÿ 
2y2y2yÆ O?O?O?
O?O9 ¾¾½½½½ÝÝÝýý{ý{ý{ýwýwýwýwýwþwþwþwÿwÿwßwßwßw¿w¿w¿w¿{¿{¿{¿¿¿
¾¾¾ÿÿÿÿ 
2y2y2yÆ O?O9O9O?O?O9O? 
œœœœ¼
¼¼¼Ü
ÜÜÜüüüü{ü
{ü{ü{üwü
wüwüwüsüsüsüsüsý
sýsýsýsþ
sþsþsþsÿsÿsÿsÿsß
sßsßsßs¿
s¿s¿s¿sŸsŸsŸsŸwŸ
wŸwŸwŸ{Ÿ
{Ÿ{Ÿ{ŸŸŸŸŸž
žžžÿÿÿÿ 
2y2y2yÆ O?O?O?O?O9O9O9 œœ
¼¼¼Ü
ÜÜüü
{ü{ü{üwü
wüwüsüsü
sýsýsýsþ
sþsþsÿsÿ
sßsßsßs¿
s¿s¿sŸsŸ
wŸwŸwŸ{Ÿ
{Ÿ{ŸŸŸ
žžžÿÿÿÿ 
2y2y2yÆ O?O?O?O9O?O?O9 }
|||{{{{›
››
»»
ÛÛûûû{û
{û{û
wûwû
sûsûsûoûoûoûoûoü
oüoü
oýoý
oþoþoÿoÿoÿoß
oßoß
o¿o¿
oŸoŸoŸoooos
ss
ww
{{~
~~}}}ÿÿÿÿ 
2y2yK2yÆ O?O?O?O?O9O9O? \\\\[[[[zzzzššššººººÚÚÚÚúúúGú{ú{ú{ú{úwúwúwúwúsúsúsúsúoúoúoúoúkûkûkûkûkükükükükýkýkýkýkþkþkþkþkÿkÿkÿGkÿkßkßkßkßk¿k¿k¿k¿kŸkŸkŸkŸkkkko_o_o_o_s_s_s_s_w_w_w_w_{_{_{_{_____^^^^]]]]ÿÿÿÿ 
2y2y;2yÆ O?O?O?O9O?O?O9 \\[[[[ZZZZzzzzššš
ººÚÚÚúúú
{ú{ú/wúwúwúsúsúsúsúoúoúoúoúkúkúkúkúkûkûkûkûkükükü
kýkýkþkþkþkÿkÿkÿ
kßkß/k¿k¿k¿kŸkŸkŸkŸkkkkk_k_k_k_o_o_o_o_s_s_s_
w_w_{_{_{____
^^]]]\\ÿÿÿÿ 
2y2y2yÆ O?
O?O?3O9O9 <;;;:::9YYYYyyy™™™¹¹¹
ÙÙùù?{ù{ù{ùwùwùwùwùsùsùsùoùoùoùkùkùkùgùgúgúgúgúgûgûgûgügügügýgýgý
gþgþgÿgÿ?gßgßgßg¿g¿g¿g¿gŸgŸgŸgggg_g_g_g?k?k?k?k?o?o?o?s?s?s?w?w?w?
{?{???>>>====<<{Þ{Þ{Þ{Þ 
2y2y2yÆ O?O?O?
O9O97 ;;;:::9999YYYyyy™™™¹¹¹ÙÙÙùùG{ù{ù{ùwùwùwùsùsùsùoùoùoùkùkùkùgùgùgùgùgúgúgúgûgûgûgügügügýgýgýgþgþgþgÿgÿGgßgßgßg¿g¿g¿gŸgŸgŸgggg_g_g_g?g?g?g?k?k?k?o?o?o?s?s?s?w?w?w?{?{?{???>>>===<<<{Þ{Þ{Þ{Þ 
2y2y2yÆ O?
O?O?;O?O9 88XXXxxx˜˜˜¸¸ØØØøøG{ø{øwøwøwøsøsøsøoøoøoøkøkøgøgøgøcøcøcùcùcúcúcúcûcûcûcücücücýcýcþcþcþcÿcÿGcßcßc¿c¿c¿cŸcŸcŸcccc_c_c?c?c?ccggkkkooosssww{{{{Þ{Þ{Þ{Þ 
2y2yK2yÆ O?O?O?O?O9O9O9 888XXxxx˜˜¸¸¸ØØ
øøKø{ø{ø{øwøwøsøsøsøoøoøoøkøkøgøgøgøcøcøcøcøcùcùcùcúcúcûcûcûcücücýcýcýcþcþ
cÿcÿKcÿcßcßcßc¿c¿cŸcŸcŸcccc_c_c?c?c?ccccgggkkooosswww{{
{Þ{Þ{Þ{Þ 
2y2y2yÆ O?O?O?
O9O?7 ~ú~ú~ù~ù~ø~ø~ø~÷~÷777WWww———··××
÷÷K÷{÷{÷{÷w÷w÷s÷s÷o÷o÷o÷k÷k÷g÷g÷c÷c÷c÷_÷_÷_ø_ø_ù_ù_ù_ú_ú_û_û_ü_ü_ü_ý_ý_þ_þ
_ÿ_ÿK_ÿ_ß_ß_ß_¿_¿_Ÿ_Ÿ________?_?___^ÿ^ÿbÿbÿfÿfÿfÿjÿjÿnÿnÿrÿrÿrÿvÿvÿzÿzÿ
~ÿ~ÿ~ÿ~þ~þ~þ~ý~ý~ü~ü~û~û~ûw½w½w½w½ 
2y2y2yÆ O?O?O?
O?O9à÷ ~ú~ù~ù~ø~ø~ø~÷~÷~÷~÷77WWWww——··×××÷÷÷÷{÷{÷w÷w÷s÷s÷s÷o÷o÷k÷k÷g÷g÷c÷c÷c÷_÷_÷_÷_÷_ø_ø_ù_ù_ú_ú_ú_û_û_ü_ü_ý_ý_þ_þ_þ_ÿ_ÿ_ÿ_ÿ_ß_ß_¿_¿_Ÿ_Ÿ_Ÿ_______?_?___^ÿ^ÿ^ÿ^ÿbÿbÿfÿfÿjÿjÿjÿnÿnÿrÿrÿvÿvÿzÿzÿzÿ~ÿ~ÿ~ÿ~ÿ~þ~þ~ý~ý~ü~ü~ü~û~û~úw½w½w½w½ 
2y2yá2yÆ O?O?O?O?O9O9O? ~Ù~Ù~Ø~Ø~Ø~×~×~Ö~Ö~ö~ö66VVvv–––¶¶ÖÖöööö{ö{öwöwösösöoöoökökögögöcöcöcö_ö_ö[ö[ö[÷[÷[ø[ø[ù[ù[ú[ú[û[û[ü[ü[ü[ý[ý[þ[þ[ÿ[ÿ[ÿ[ÿ[ß[ß[¿[¿[Ÿ[Ÿ[[[_[_[?[?[[[ZÿZÿZßZß^ß^ßbßbßfßfßjßjßnßnßrßrßrßvßvßzßzß~ß~ß~ß~ß~Þ~Þ~Ý~Ý~Ü~Ü~Û~Û~Ú~Úw½w½w½w½ 
2y2y2yÆ O?O?O?
O?O9à÷ ~¹~¹~¸~¸~·~·~¶~¶~Õ~Õ~õ~õ55UUuu••µµÕÕõõõõ{õ{õwõwõsõsõoõoõkõkõgõgõcõcõ_õ_õ[õ[õWöWöW÷W÷WøWøWùWùWúWúWûWûWüWüWýWýWþWþWÿWÿWÿWÿWßWßW¿W¿WŸWŸWWW_W_W?W?WWVÿVÿVßVßZ¿Z¿^¿^¿b¿b¿f¿f¿j¿j¿n¿n¿r¿r¿v¿v¿z¿z¿~¿~¿~¿~¿~¾~¾~½~½~¼~¼~»~»~º~ºsœsœsœsœ 
2y2yá2y%) O?O?O?O9O?O9O9 ~¹~¸~¸~·~·~¶~¶~µ~µ~Õ~Õ~õ~õ55Uuu••µµÕÕõõõõ{õ{õwõsõsõoõoõkõkõgõgõcõcõ_õ_õ[õ[õWõWõWöWöW÷W÷WøWøWùWùWúWûWûWüWüWýWýWþWþWÿWÿWÿWÿWßWßW¿WŸWŸWWW_W_W?W?WWVÿVÿVßVßV¿V¿Z¿Z¿^¿^¿b¿b¿f¿f¿j¿n¿n¿r¿r¿v¿v¿z¿z¿~¿~¿~¿~¿~¾~¾~½~¼~¼~»~»~º~º~¹sœsœsœsœ 
2y2y2yÆ O?O?O?
O?O9à÷ ~˜~˜~—~—~–~–~•~”~´~´~Ô~Ô~ô~ô44TTtt””´ÔÔôôôô{ôwôwôsôsôoôoôkôgôgôcôcô_ô_ô[ô[ôWôSôSõSõSöSöS÷S÷SøSùSùSúSúSûSûSüSüSýSþSþSÿSÿSÿSÿSßS¿S¿SŸSŸSSS_S?S?SSRÿRÿRßRßR¿RŸVŸVŸZŸZŸ^Ÿ^ŸbŸfŸfŸjŸjŸnŸnŸrŸrŸvŸzŸzŸ~Ÿ~Ÿ~Ÿ~Ÿ~ž~~~œ~œ~›~›~š~™~™sœsœsœsœ 
2y2y2yÆ O?O?O?
O9O?à÷ ~˜~—~—~–~–~•~”~”~”~´~´~Ô~Ô~ô44Ttt””´ÔÔôôôô{ôwôwôsôsôoôkôkôgôgôcô_ô_ô[ô[ôWôSôSôSôSõSõSöSöS÷SøSøSùSùSúSûSûSüSüSýSþSþSÿSÿSÿSÿSßS¿S¿SŸSŸSS_S_S?S?SRÿRÿRßRßR¿RŸRŸRŸVŸVŸZŸZŸ^ŸbŸbŸfŸfŸjŸnŸnŸrŸrŸvŸzŸzŸ~Ÿ~Ÿ~Ÿ~Ÿ~ž~~~œ~œ~›~š~š~™~™o{o{o{o{ 
2y2y2yÆ O?O?O?
O?O9à÷ ~x~w~v~v~u~u~t~s~“~“~³~³~Ó~ó~ó3SSs““³³Óóóó{ó{ówówósóoóoókókógócócó_ó[ó[óWóWóSóOóOôOôOõOõOöO÷O÷OøOøOùOúOúOûOüOüOýOýOþOÿOÿOÿOßOßO¿O¿OŸOOO_O_O?OONÿNßNßN¿N¿NŸNRRVVZ^^bbfjjnrrvvz~~~~~~~~}~}~|~{~{~z~z~y~xo{o{o{o{ 
2y2yá2yÆ O?O?O?O9O?O9O9 ~w~w~v~u~u~t~s~s~s~“~“~³~Ó~Ó~ó33Sss“³³Óóóó{ó{ówósósóoóoókógógócó_ó_ó[óWóWóSóOóOóOóOôOôOõOöOöO÷OøOøOùOùOúOûOûOüOýOýOþOÿOÿOÿOßOßO¿OŸOŸOOO_O?O?ONÿNÿNßN¿N¿NŸNNNRRVZZ^bbffjnnrvvz~~~~~~~~}~|~|~{~{~z~y~y~xkZkZkZkZ 
2y2y2yÆ O?O?O?
O?O9à÷ ~W~V~V~U~T~T~S~R~R~r~’~’~²~Ò~ò~ò22Rrr’²²Òòòò{ò{òwòsòsòoòkòkògòcòcò_ò[ò[òWòSòSòOòKòKòKóKôKôKõKöK÷K÷KøKùKùKúKûKûKüKýKýKþKÿKÿKÿKßKßK¿KŸKŸKK_K_K?KKJÿJßJßJ¿JŸJŸJJ_J_N_R_R_V_Z_^_^_b_f_f_j_n_n_r_v_v_z_~_~_~_~^~^~]~\~\~[~Z~Z~Y~X~XkZkZkZkZ 
2y2yá2yÆ O?O?O?O?O9O9O? ~7~6~5~5~4~3~2~2~Q~q~q~‘~±~±~Ñ~ñ1QQq‘‘±Ññññ{ñ{ñwñsñsñoñkñgñgñcñ_ñ_ñ[ñWñWñSñOñKñKñGòGóGóGôGõGõGöG÷GøGøGùGúGúGûGüGüGýGþGÿGÿGÿGßGßG¿GŸGŸGG_G?G?GFÿFÿFßF¿F¿FŸFF_F_J?N?N?R?V?V?Z?^?b?b?f?j?j?n?r?r?v?z?~?~?~?~>~>~=~<~<~;~:~9~9~8~7g9g9g9g9 
2y2y2yÆ O?
O?O?àûO?O9 ~6~6~5~4~3~3~2~1~1~Q~q~‘~‘~±~Ñ~Ñ~ñ11Qq‘‘±Ññññ{ñ{ñwñsñoñoñkñgñcñcñ_ñ[ñ[ñWñSñOñOñKñGñGñGòGóGôGôGõGöGöG÷GøGùGùGúGûGüGüGýGþGÿGÿGÿGßGßG¿GŸGGG_G?GGFÿFßFßF¿FŸFFF_F?F?J?N?R?R?V?Z?Z?^?b?f?f?j?n?r?r?v?z?~?~?~?~>~>~=~<~;~;~:~9~8~8~7g9g9g9g9 
2y2yá2yÆ O?O9O9O?O9O9O9 ~~~~~~~~~0~P~P~p~~°~°~Ð~ð0Ppp°ÐÐðð{ðwðwðsðoðkðkðgðcð_ð_ð[ðWðSðSðOðKðGðGðCñCòCòCóCôCõCõCöC÷CøCøCùCúCûCûCüCýCþCþCÿCÿCßC¿C¿CŸCC_C_C?CBÿBÿBßB¿BŸBŸBB_B?B?FJJNRVVZ^bbfjnnrvzz~~~~~~~~~~~~~cccc 
2y2y2yÆ O?O?O?
O?O9à÷ ~~~~~~~~~~0~P~p~p~~°~Ð~ð~ð0Ppp°ÐÐðð{ðwðwðsðoðkðgðgðcð_ð[ð[ðWðSðOðKðKðGðCðCðCñCòCóCóCôCõCöC÷C÷CøCùCúCûCûCüCýCþCþCÿCÿCßC¿C¿CŸCC_C?C?CBÿBßBßB¿BŸBB_B_B?BBFJNNRVZ^^bfjnnrvzz~~~~~~~~~~~~~cccc 
2y2y2yÆ O?O?O?
O?O9à÷ }õ}ô}ô}ó}ò}ñ}ð}ï~~/~/~O~o~~¯~¯~Ï~ï/OOo¯ÏÏïï{ïwïsïsïoïkïgïcïcï_ï[ïWïSïSïOïKïGïCï?ï?ð?ñ?ñ?ò?ó?ô?õ?õ?ö?÷?ø?ù?ú?ú?û?ü?ý?þ?þ?ÿ?ÿ?ß?¿?Ÿ?Ÿ??_????>ÿ>ß>¿>Ÿ>Ÿ>>_>?>=ÿAÿEÿEÿIÿMÿQÿUÿUÿYÿ]ÿaÿeÿiÿiÿmÿqÿuÿyÿyÿ}ÿ}ÿ}þ}ý}ü}ü}û}ú}ù}ø}ø}÷}ö^÷^÷^÷^÷ 
2y2yá2yÆ O?O9O9O?O?O9O9 }õ}ô}ó}ò}ñ}ñ}ð}ï}ï~~/~O~o~o~~¯~Ï~ï/Oo¯ÏÏïï{ïwïsïsïoïkïgïcï_ï_ï[ïWïSïOïKïGïGïCï?ï?ï?ð?ñ?ò?ó?ó?ô?õ?ö?÷?ø?ø?ù?ú?û?ü?ý?þ?þ?ÿ?ÿ?ß?¿?Ÿ?Ÿ??_???>ÿ>ÿ>ß>¿>Ÿ>>_>?>?>=ÿ=ÿAÿEÿIÿMÿMÿQÿUÿYÿ]ÿaÿaÿeÿiÿmÿqÿuÿyÿyÿ}ÿ}ÿ}þ}ý}ü}ü}û}ú}ù}ø}÷}÷}ö^÷^÷^÷^÷ 
2y2y2yÆ O?O?O?
O?O9à÷ }Ô}Ô}Ó}Ò}Ñ}Ð}Ï}Î}î~~~.~N~n~Ž~®~Î~Î~î.NnŽ®®Îîî{îwîsîoîoîkîgîcî_î[îWîSîSîOîKîGîCî?î;î;ï;ð;ð;ñ;ò;ó;ô;õ;ö;ö;÷;ø;ù;ú;û;ü;ý;ý;þ;ÿ;ÿ;ß;¿;Ÿ;;;_;?;:ÿ:ß:¿:Ÿ:Ÿ::_:?:9ÿ9ß=ßAßAßEßIßMßQßUßYßYß]ßaßeßißmßqßußußyß}ß}ß}Þ}Ý}Ü}Û}Û}Ú}Ù}Ø}×}Ö}ÕZÖZÖZÖZÖ 
2y2yá2yÆ O?O?O?O9O?O9O9 }Ô}Ó}Ò}Ñ}Ñ}Ð}Ï}Î}Î}î~~.~N~n~Ž~Ž~®~Î~î.NnŽ®®Îîî{îwîsîoîkîkîgîcî_î[îWîSîOîKîGîGîCî?î;î;î;ï;ð;ñ;ò;ó;ô;ô;õ;ö;÷;ø;ù;ú;û;ü;ý;ý;þ;ÿ;ÿ;ß;¿;Ÿ;;_;_;?;:ÿ:ß:¿:Ÿ::_:?:?:9ÿ9ß9ß=ßAßEßIßMßQßQßUßYß]ßaßeßißmßqßqßußyß}ß}ß}Þ}Ý}Ü}Û}Ú}Ú}Ù}Ø}×}Ö}ÕVµVµVµVµ 
2y2y2yÆ O?O?O?
O?O9à÷ }´}³}²}±}°}¯}®}­}Í}í}í~~-~M~m~~­~Í~í-Mmm­Ííí{íwísíoíkígící_í_í[íWíSíOíKíGíCí?í;í7í7î7ï7ï7ð7ñ7ò7ó7ô7õ7ö7÷7ø7ù7ú7û7û7ü7ý7þ7ÿ7ÿ7ß7¿7Ÿ77_7?76ÿ6ÿ6ß6¿6Ÿ66_6?65ÿ5ß5¿9¿=¿=¿A¿E¿I¿M¿Q¿U¿Y¿]¿a¿e¿i¿m¿m¿q¿u¿y¿}¿}¿}¾}½}¼}»}º}¹}¸}·}·}¶}µVµVµVµVµ 
2y2yá2yÌ O?O9O9O?O?O9O? }“}’}’}‘}}}Ž}}¬}Ì}ì~~,~L~l~Œ~¬~Ì~ì,LlŒ¬Ììì{ìwìsìoìkìgìcì_ì[ìWìSìOìKìKìGìCì?ì;ì7ì3í3î3ï3ð3ñ3ò3ó3ô3õ3ö3÷3ø3ø3ù3ú3û3ü3ý3þ3ÿ3ÿ3ß3¿3Ÿ33_3?32ÿ2ß2¿2Ÿ22_2_2?21ÿ1ß1¿5Ÿ9Ÿ=ŸAŸEŸIŸMŸQŸUŸYŸ]ŸaŸaŸeŸiŸmŸqŸuŸyŸ}Ÿ}Ÿ}ž}}œ}›}š}™}˜}—}–}•}”R”R”R”R” 
2y2y2yÌ O?O?O?
O?O9à÷ }“}’}‘}}}Ž}}Œ}¬}¬}Ì}ì~~,~L~l~Œ~¬~Ì~ì,LlŒ¬Ììì{ìwìsìoìkìgìcì_ì[ìWìSìOìKìGìCì?ì;ì7ì3ì3í3í3î3ï3ð3ñ3ò3ó3ô3õ3ö3÷3ø3ù3ú3û3ü3ý3þ3ÿ3ÿ3ß3¿3Ÿ33_3?32ÿ2ß2¿2Ÿ22_2?21ÿ1ß1¿1Ÿ5Ÿ5Ÿ9Ÿ=ŸAŸEŸIŸMŸQŸUŸYŸ]ŸaŸeŸiŸmŸqŸuŸyŸ}Ÿ}Ÿ}ž}}œ}›}š}™}˜}—}–}•}”NsNsNsNs 
2y2yá2yÆ O?O?O?O?O9O9O9 }s}r}q}p}o}n}m}l}‹}«}Ë}ë~~+~K~k~‹~«~Ë~ë+Kk‹«Ëëë{ëwësëoëkëgëcë_ë[ëWëSëOëKëGëCë?ë;ë7ë3ë/ì/í/î/ï/ð/ñ/ò/ó/ô/õ/ö/÷/ø/ù/ú/û/ü/ý/þ/ÿ/ÿ/ß/¿/Ÿ//_/?/.ÿ.ß.¿.Ÿ.._.?.-ÿ-ß-¿-Ÿ159=AEIMQUY]aeimquy}}}~}}}|}{}z}y}x}w}v}u}tNsNsNsNs 
2y2y2yÆ O?
O?O?àûO?O9 }r}q}p}o}n}m}l}k}k}‹}«}ë~~+~K~k~‹~«~Ë~ë+Kk‹«Ëëë{ëwësëoëkëgëcë[ëWëSëOëKëGëCë?ë;ë7ë3ë/ë/ë/ì/í/ï/ð/ñ/ò/ó/ô/õ/ö/÷/ø/ù/ú/û/ü/ý/þ/ÿ/ÿ/ß/¿/Ÿ//_/?/.ß.¿.Ÿ.._.?.-ÿ-ß-¿-Ÿ--15=AEIMQUY]aeimquy}}}~}}}|}{}z}y}x}v}u}t}sJRJRJRJR 
2y2yá2yÆ O?O9O9O?O9O9O? }R}Q}P}O}N}M}L}K}j}Š}ª}Ê}ê~
~*~J~j~Š~Ê~ê
*JjŠªÊêê{êwêsêoêgêcê_ê[êWêSêOêKêGêCê?ê;ê7ê3ê/ê+ë+ì+í+î+ï+ð+ñ+ò+ó+ô+ö+÷+ø+ù+ú+û+ü+ý+þ+ÿ+ÿ+ß+¿+Ÿ++?+*ÿ*ß*¿*Ÿ**_*?*)ÿ)ß)¿)Ÿ)-_1_5_9_=_A_E_I_M_Q_Y_]_a_e_i_m_q_u_y_}_}_}^}]}\}[}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2yÆ O?O?O?
O?O9à÷ }R}Q}O}N}M}L}K}J}J}j}ª}Ê}ê~
~*~J~j~Š~ª~Ê~ê*JjŠªÊêê{êwêoêkêgêcê_ê[êWêSêOêKêGê?ê;ê7ê3ê/ê+ê+ê+ë+í+î+ï+ð+ñ+ò+ó+ô+õ+ö+÷+ù+ú+û+ü+ý+þ+ÿ+ÿ+ß+¿++_+?+*ÿ*ß*¿*Ÿ**_*?)ÿ)ß)¿)Ÿ))_)_-_5_9_=_A_E_I_M_Q_U_Y_]_e_i_m_q_u_y_}_}_}^}]}[}Z}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2yÆ O?O?O?
O?O9à÷ }1}0}/}.}-},}+})}I}i}‰}©}É}é~)~I~i~‰~©~É~é	)i‰©Ééé{éwéoékégécé_é[éWéSéKéGéCé?é;é7é3é/é'é'ê'ë'ì'í'î'ï'ñ'ò'ó'ô'õ'ö'÷'ø'ù'û'ü'ý'þ'ÿ'ÿ'ß'¿''_'?'&ÿ&ß&¿&Ÿ&_&?&%ÿ%ß%¿%Ÿ%%?)?-?1?5?9?=?E?I?M?Q?U?Y?]?a?e?m?q?u?y?}?}?}>}=};}:}9}8}7}6}5}4}2BBBB 
2y2yá2yÌ O?O?O?O?O9O9O? }}}}}}}
}	}(}H}ˆ}¨}È}è~~(~H~ˆ~¨~È~è(Hˆ¨Èèè{èsèoèkègècè_è[èSèOèKèGèCè?è7è3è/è+è'è#é#ê#ì#í#î#ï#ð#ñ#ò#ô#õ#ö#÷#ø#ù#ú#ü#ý#þ#ÿ#ÿ#ß#Ÿ##_#?#"ÿ"ß"Ÿ""_"?"!ÿ!¿!Ÿ!!_!?%)159=AEIQUY]aeiquy}}}}}}}}}}}}}=ï=ï=ï=ï 
2y2yá2yÆ O?O?O?O9O?O?O9 }}}}}}}	}}(}H}h}ˆ}¨}è~~(~H~h~¨~È~è(Hˆ¨Èèè{èsèoèkègècè[èWèSèOèKèGè?è;è7è3è/è'è#è#é#ê#ë#ì#í#ï#ð#ñ#ò#ó#õ#ö#÷#ø#ù#ú#ü#ý#þ#ÿ#ÿ#ß#Ÿ##_#?#"ß"¿"Ÿ""_"?!ÿ!ß!¿!Ÿ!!?!%)-15=AEIMUY]aeiquy}}}}}}}}}}}}}9Î9Î9Î9Î 
2y2y2yÆ O?
O?O?àûO9O9 |ð|ï|î|í|ë|ê|é|è}}'}g}‡}§}Ç}ç~'~G~g~‡~§~ç'Gg§Çççwçsçoçkçgç_ç[çWçSçOçGçCç?ç;ç7ç/ç+ç'ç#çèéëìíîïñòóôõ÷øùúûýþÿÿ¿Ÿ_?ÿß¿Ÿ?ÿß¿_? ÿ$ÿ,ÿ0ÿ4ÿ8ÿ<ÿDÿHÿLÿPÿTÿ\ÿ`ÿdÿhÿlÿtÿxÿ|ÿ|ÿ|ý|ü|û|ú|ù|÷|ö|õ|ô|ó|ñ5­5­5­5­ 
2y2yá2yÆ O?O?O?O9O9O9O? |ð|ï|í|ì|ë|ê|è|ç}}'}G}g}§}Ç}ç~~'~g~‡~§~Ç'Gg§Çççwçsçoçkçcç_ç[çWçSçKçGçCç?ç7ç3ç/ç+ç#ççèéêëíîïðñóôõöøùúûýþÿÿ¿Ÿ_ÿß¿Ÿ_?ÿ¿Ÿ_ÿ ÿ$ÿ(ÿ,ÿ4ÿ8ÿ<ÿ@ÿDÿLÿPÿTÿXÿ`ÿdÿhÿlÿtÿxÿ|ÿ|ÿ|ý|ü|û|ú|ø|÷|ö|õ|ô|ò|ñ1Œ1Œ1Œ1Œ 
2y2y2yÆ O?
O?O?àûO9O9 |Ï|Î|Í|Ì|Ê|É|È|Ç|æ}}F}f}†}¦}æ~~&~F~†~¦~Æ~æ&Ff¦Æææwæsæoækæcæ_æ[æWæOæKæGæ?æ;æ7æ3æ+æ'æ#ææçèêëìíïðñòôõö÷ùúûýþÿÿ¿Ÿ_ÿß¿_?ÿß¿Ÿ_?ÿß ß(ß,ß0ß4ß<ß@ßDßHßPßTßXß\ßdßhßlßtßxß|ß|ß|Ý|Ü|Û|Ú|Ø|×|Ö|Õ|Ó|Ò|Ñ1Œ1Œ1Œ1Œ 
2y2y2yÆ O?O?O?
O9O?à÷ |Ï|Î|Í|Ë|Ê|É|Ç|Æ|æ}}&}F}†}¦}Æ~~&~F~f~¦~Æ~æ&Ff†Æææwæsæoægæcæ_æ[æSæOæKæCæ?æ;æ7æ/æ+æ'æææçèéêìíîðñòóõö÷ùúûüþÿÿ¿Ÿ?ÿßŸ_ÿß¿_?ÿßß ß$ß(ß0ß4ß8ß@ßDßHßLßTßXß\ßdßhßlßpßxß|ß|ß|Ý|Ü|Û|Ù|Ø|×|Ö|Ô|Ó|Ò|Ð-k-k-k-k 
2y2yá2y%) O?O9O9O?O?O9O9 |¯|­|¬|«|ª|¨|§|¦|Å|å}%}E}e}¥}Å}å~%~E~e~¥~Å~åEe…Åååwåsåoågåcå_åWåSåOåGåCå?å7å3å/å+å#åååæçéêëíîïñòóõö÷øúûüþÿÿ¿Ÿ?ÿ¿Ÿ?ÿ¿Ÿ_ÿß¿¿$¿(¿,¿4¿8¿<¿D¿H¿L¿T¿X¿\¿`¿h¿l¿p¿x¿|¿|¿|½|¼|»|¹|¸|·|µ|´|³|±|°)J)J)J)J 
2y2y2yÆ O?O?O?
O?O9à÷ |®|­|¬|ª|©|¨|¦|¥|¥|å}}E}e}…}Å}å~~E~e~…~Å~åEe…Åååwåsåoågåcå_åWåSåKåGåCå;å7å3å+å'å#ååååçèêëìîïðòóôö÷øúûüþÿÿ¿Ÿ?ÿ¿Ÿ_?ß¿Ÿ_?ß¿¿¿ ¿(¿,¿0¿8¿<¿@¿H¿L¿P¿X¿\¿`¿h¿l¿p¿x¿|¿|¿|½|¼|»|¹|¸|·|µ|´|²|±|°%)%)%)%) 
2y2yá2yÆ O?O?O?O9O?O9O9 |Ž||‹|Š|‰|‡|†|„|¤|Ä}}$}D}„}¤}ä~~$~d~„~¤~äDd„Äääwäsäkägäcä[äWäSäKäGä?ä;ä7ä/ä+ä'ääääåæèéêìíïðñóôõ÷øúûüþÿÿ¿Ÿ_?ß¿Ÿ_?ÿß¿_?ÿßŸŸŸ Ÿ$Ÿ(Ÿ0Ÿ4Ÿ<Ÿ@ŸDŸLŸPŸTŸ\Ÿ`ŸhŸlŸpŸxŸ|Ÿ|Ÿ||œ|š|™|˜|–|•|”|’|‘|!!!! 
2y2y2yÆ O?
O?O?àûO9O? |n|l|k|j|h|g|e|d|ƒ|Ã|ã}#}C}c}£}Ã~~#~C~ƒ~£~ã#cƒÃããwãsãkãgãcã[ãWãOãKãGã?ã;ã3ã/ã+ã#ããããäæçéêëíîðñòôõ÷øùûüþÿÿ¿Ÿ_?ß¿_?ÿßŸ_ÿ¿Ÿ$(,48@DHPT\`dlpx|||}|||z|y|x|v|u|s|r|q|oçççç 
2y2yá2yÆ O?O?O?O?O9O9O9 |m|l|k|i|h|f|e|c|ƒ|£|ã}}C}c}ƒ}Ã}ã~#~C~ƒ~£~Ã#cƒÃããwãsãkãgã_ã[ãWãOãKãCã?ã7ã3ã/ã'ã#ããããäåçèêëìîïñòôõöøùûüþÿÿ¿Ÿ_?ÿß¿_ÿ¿Ÿ?ß¿ (,08<DHPTX`dlpx|||}|||z|y|w|v|u|s|r|p|oÆÆÆÆ 
2y2yá2yÆ O?O?O?O9O?O?O9 |M|L|J|I|G|F|D|C|b|¢|Â}}"}b}‚}¢}â~~B~b~¢~Â"b‚Âââwâsâkâgâ_â[âSâOâGâCâ;â7â3â+â'âââââãåæèéëìíïðòóõöøùûüþÿÿ¿Ÿ_?
ÿ
ß
Ÿ

?
	ß	¿	Ÿ	_	?ÿßŸ___ _$_,_0_4_<_@_H_L_T_X_`_d_l_p_x_|_|_|]|\|Z|Y|W|V|T|S|Q|P|N¥¥¥¥ 
2y2yá2yÆ O?O?O?O?O9O9O? |M|K|J|H|G|E|D|B|b|‚|Â|â}"}B}‚}¢}â~~B~b~¢~Â"b‚Ââ{âwâsâkâgâ_â[âSâOâGâCâ;â7â/â+â#âââââãäæçéêìíïðòóõöøùûüþÿß¿Ÿ_?
ÿ
ß
Ÿ

?
	ß	¿		_	ÿ¿Ÿ_____$_(_0_4_<_@_H_L_T_X_`_d_l_p_x_|_|^|]|\|Z|Y|W|V|T|S|Q|P|N„„„„ 
2y2yá2yÌ O?O?O?O9O?O?O9 |,|+|)|(|&|%|#|"|A||¡|á}}A}a}¡}Á~~!~a~~Á~á!A¡á{áwáoákácá_áWáSáKáGá?á;á3á/á'á#áááááâäåçèêëíîðñóôö÷ùúüýÿß¿_ÿ¿Ÿ_?ÿßŸ?ß¿_???? ?(?,?4?8?@?D?L?P?X?\?d?h?p?t?|?|>|=|;|:|8|7|5|4|2|1|/|.cccc 
2y2y2yÆ O?
O?O?àûO9O9 |,|*|)|'|&|$|#|!|A|a|¡|Á}}!}a}}Á~~!~a~~Á~á!A¡á{áwáoákácá_áWáSáKáGá?á7á3á+á'ááááááâãåæèéëìîðñóôö÷ùúüýÿß¿_ÿ¿Ÿ_?ÿ¿Ÿ_?ÿßŸ????? ?$?,?0?8?@?D?L?P?X?\?d?h?p?t?|?|>|=|;|:|8|7|5|4|2|1|/|-BBBB 
2y2yá2yÆ O?O?O?O9O9O?O9 ||
||||||| |`|€|À|à} }`}€}À}à~ ~@~€~À~à @€ à{àwàoàkàcà_àWàOàKàCà?à7à3à+à#ààààààáãäæçéëìîïñòôö÷ùúüýÿß¿_ÿ¿_ÿ¿Ÿ_ ÿ ¿ Ÿ _ ?$,08<DHPX\dhpt|||||||||||||!!!! 
2y2yá2yÆ O?O?O?O?O9O9O? ||
|||||| | |@|€| |à} }@}€} }à~ ~@~€~ ~à @€ à{àwàoàkàcà[àWàOàKàCà;à7à/à+à#ààààààáâäåçéêìíïñòôõ÷ùúüýÿß¿_ß¿_ß¿_ ß ¿  _ $(04<DHPT\dhpt|||||||||||||   
2y2y2yÆ O?
O?O?àûO?O9 ||	|||||| | |@|€| |à} }@}€} }à~ ~@~`~ ~à @` À{àsàoàgàcà[àSàOàGàCà;à7à/à'à#ààààààáâäåçèêìíïðòóõ÷øúûýþßŸ?ßŸ?ß¿? ß ¿  _  (04<@HLT\`hltx||||||||||||   
2y2yá2yÆ O?O9O9O?O9O9O9 xx	xxxxxx x x@x€x xày y@y`y yÀz z@z`z zÀ{ { {`{€{ÀwÀsÀkÀgÀ_À[ÀSÀOÀGÀ?À;À3À/À'À#ÀÀÀÀÀÀÁÂÄÅÇÈÊËÍÎÐÒÓÕÖØÙÛÜÞ¾ž^>þÞž~>þÞž~> Þ ¾ ~ ^  (,48@HLTX`dlpxxxxxxxxxxxxx   
2y2y2yÆ O?O?O?
O?O9à÷ xx	xxxxxx x x@x€x xày y@y`y yÀz z z`z€zÀzà{ {@{€{ wÀoÀkÀcÀ_ÀWÀSÀKÀGÀ?À;À3À/À'À#ÀÀÀÀÀÀÁÂÄÅÇÈÊËÍÎÐÑÓÔÖ×ÙÚÜÝ¾~^þ¾ž^>þÞž~> Þ ¾ ~ ^  (,48@DLPX\dhptxxxxxxxxxxxx   
2y2y2yÆ O?O?O?
O?O9à÷ t
t	tttttt t t@t`t tÀu u u`u€uÀuàv v@v€v vàw w@w`w s o g c [ W O K C ? 7 3 + '       ¡¢£¥¦¨©«¬®¯±²´µ·¸º»½}=Ý½}]ý½]= ý Ý  } =  $,08<DHPT\`hlttttttttttttt   
2y2y2yÆ O?
O?O?àûO9O? t
t	tttttt t t@t`t tÀu u u`u€uÀuàv v@v`v vÀw w w`w€s k g _ [ S O K C ? 7 3 + '        ¢£¥¦¨©«¬®¯±²³µ¶¸¹»¼]=ýÝ}]ý½]= ý Ý  } =   $,08<DHLTX`dlptttttttttttt   
2y2y2yÆ O?O?O?
O?O9à÷ p
p	pppppp p p@p`p pÀq q q@q€q qàr r@r`r€rÀràs s@s€o€k€c€_€W€S€O€G€C€;€7€/€+€'€€€€€€€€‚ƒ…†ˆ‰ŠŒ’“”–—™šœ|\ü¼œ|<Ü¼|\< ü Ü œ | <   $(04<@HLPX\dhppppppppppppp   
2y2yá2yÆ O?O?O?O9O?O9O9 p
ppppppp p p@p`p pÀpàq q@q€q qÀr r r`r€r ràs s@s`o€g€c€[€W€S€K€G€?€;€7€/€+€#€€€€€€€€‚ƒ…†‡‰ŠŒŽ‘“”•—˜š›|<Ü¼œ\<üÜ¼|\ ü Ü œ | <  $(048@DLPT\`hlpppppppppppp   
2y2y2yÆ O?O?O?
O?O9à÷ l
lllllll l l@l`l lÀlàm m@m`m mÀn n n@n€n nÀo o o`k`g`_`[`W`O`K`C`?`;`3`/`+`#````````bcefgijkmnpqrtuvxy{[;ûÛ»{[ûÛ›{[ û » › { ;  $(,48@DHPTX`dlllllllllllll   
2y2y2yÆ O?
O?O?àûO9O? h	hhhhhhh h h@h`h€hÀhài i@i`i iÀiàj j@j`j jÀjàk k@k@c@_@W@S@O@G@C@?@7@3@/@'@#@@@@@@@@BCDFGHJKMNOQRSUVWYZZúºšz:úºšz: ú º š z :   (,48<DHLTX\dhhhhhhhhhhhhh   
2y2y2yÆ O?O9O9
O?O9à÷ h	hhhhhhh h h@h`h€hÀhài i@i`i€iÀiàj j@j`j€j jàk k g@c@[@W@S@K@G@C@;@7@3@+@'@#@@@@@@@@BCDFGHJKLNOPRSTUWXY:ÚºšZ:ÚºšZ: ú º š z :   (,08<@HLPT\`dhhhhhhhhhhhh
   
2y2yá2yÌ O?O?O?O?O9O9O9 d	ddddddd d d@d`d€dÀdàe e e`e€e eàf f f@f€f fÀg g c _ [ W O K G ? ; 7 3 + ' #        "#$&'()+,-/01245689ùÙ¹yY9ùÙ¹™Y9 Ù ¹ ™ y 9   $,04<@DHPTX`ddddddddddddd
   
2y2y2yÆ O?
O?O?àûO9O9 d	ddddddd d d@d`d€d dàe e e@e€e eÀf f f@f`f fÀfàg c _ W S O K C ? ; 7 / + ' #        "#$%'()*,-.01235678ù¹™yYùÙ¹yY9 Ù ¹ ™ Y 9   $(048@DHLTX\`dddddddddddd
   
2y2y2yÆ O?O9O9
O?O9à÷ `	``````` ` `@```€` `àa a a@a`a aÀaàb b@b`b€b bàc _ [ W S K G C ? ; 3 / + '         	
øØ¸˜X8øØ˜xX8 ø Ø ¸ ˜ X 8   $(,48<@HLPT\`````````````
   
2y2y2yÆ O?
O?O?àûO9O9 ```````` ` `@```€` `Àa a a@a`a€aÀaàb b b@b€b bÀbà_ [ S O K G C ; 7 3 / + #         	
øØ˜xX8Ø¸˜xX ø Ø ¸ ˜ X 8   $(,08<@DHPTX\````````````
   
2y2yá2yÆ O?O?O?O9O9O?O9 \\\\\\\\ \ \ \`\€\ \À\à] ]@]`]€] ]À^ ^ ^@^`^€^À^àZàVàRàNàJàBà>à:à6à2à*à&à"ààààà
ààààáãäåæçéêëìíîðñòóôö÷×·—wW÷×·—W7 ÷ × · w W 7  $(,048@DHLPX\\\\\\\\\\\\
\	   
2y2yá2yÆ O?O?O?O?O9O9O? \\\\\\\\ \ \ \`\€\ \À\à] ] ]`]€] ]À]à^ ^@^`^€^ ^ÀZàVàNàJàFàBà>à:à6à.à*à&à"ààààà
ààààáãäåæçèéëìíîïðòóôõö×·wW7÷×·wW7 ÷ × · w W 7   $,048<@HLPTX\\\\\\\\\\\
\	   
2y2y2yÆ O?
O?O?àûO?O9 XXXXXXXX X X X@X€X XÀXàY Y Y@Y`Y YÀYàZ Z Z@Z`Z€ZÀVÀRÀNÀJÀFÀBÀ:À6À2À.À*À&À"ÀÀÀÀÀ
ÀÀÀÀÁÂÄÅÆÇÈÉÊËÍÎÏÐÑÒÓÔÖ¶–vV6Ö¶–vV6 ö Ö – v V 6   $(,48<@DHLPXXXXXXXXXXXX
X	   
2y2yá2y%) O?O9O9O?O9O9O9 TTTTTTTT T T T@T€T TÀTàU U U@U`U€U UÀV V V@V`V€V V R J F B > : 6 2 . * & "     
    ¡¢¤¥¦§¨©ª«¬­®°±²³´µµ•U5õÕµ•uU5 Õ µ • u U 5   $(,048@DHLPTTTTTTTTTTTT
T	   
2y2y2yÆ O?O?O?
O?O9à÷ TTTTTTTT T T T@T`T€TÀTàU U U@U`U€U UÀUàV V V@V`V€R N J F B > : 6 2 . & "      
    ¡¢£¤¦§¨©ª«¬­®¯°±²³´•uU5õÕµ•u5 õ Õ µ • u U 5   $(,048<@DHLPTTTTTTTTTTT	T   
2y2y2yÆ O?O?O?
O?O9à÷ PPPPPPPP P P P@P`P€P PÀPàQ Q@Q`Q€Q QÀQàR R R@R`R€R€N€J€B€>€:€6€2€.€*€&€"€€€€€€
€€€€‚ƒ„…†‡‰Š‹ŒŽ‘’“””tTôÔ´”tT4 ô Ô ´ ” t T 4  $(,048<@DHLPPPPPPPPPPP
P	P   
2y2yá2yÆ O?O9O9O?O?O9O? PPPPPPPP P P P@P`P€P PÀPàQ Q Q@Q`Q€Q QÀQàR R R@R`N€J€F€B€>€:€6€2€.€*€&€"€€€€€€
€€€€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“tT4ôÔ´”tT4 ô Ô ´ ” t T 4   $(,048<@DHLPPPPPPPPPP
P	P   
2y2y2yÆ O?O?O?
O?O9à÷ LLLLLLLL L L L@L`L€L LÀLàM M M@M`M€M MÀMàN N N@N`J`J`F`B`>`:`6`2`.`*`&`"``````
````abcdefghijklmnopqrsSS3óÓ³“sS3 ó Ó ³ “ s S 3   $(,048<@DHLLLLLLLLLLL
L	L   
2y2yá2yÆ O?O?O?O?O9O9O9 LLLLLLLL L L L@L`L€L LÀLàM M M@M`M€M M MÀMàN N N@J`F`B`>`:`6`2`.`*`&`"```````
````abcdefghijklmmnopqrS3óÓ³“sS3 ó ó Ó ³ “ s S 3   $(,0448<@DHLLLLLLLLL
L	LL   
2y2yá2yÆ O?O?O?O9O?O?O9 HHHHHHHH H H H@H`H€H HÀHàI I I I@I`I€I IÀIàJ J J@F@B@B@>@:@6@2@.@*@&@"@@@@@@@
@@@@ABCDEFGHHIJKLMNOPQR2òÒ²’rR2 ò Ò ² ’ ’ r R 2    $(,048<@DHHHHHHHHHH
H	HH   
2y2yá2yÆ O?O?O?O?O9O9O9 DDDDDDDD D D D@D`D€D D DÀDàE E E@E`E€E EÀEÀEàF F F B > : 6 2 . . * & "       
    !"#$%%&'()*+,-../011ñÑ±‘qqQ1 ñ Ñ ± ‘ q Q Q 1   $(,0488<@DDDDDDDDDD
D	DD   
2y2y2yÆ O?O?O?
O9O?à÷ DDDDDDDD D D D@D`D€D€D DÀDàE E E@E`E`E€E EÀEàF F B > > : 6 2 . * & & "      
 
    !"#$$%&'()*++,-./01ññÑ±‘qQ11 ñ Ñ ± ‘ q Q Q 1   $(,,048<@DDDDDDDDD
D	D	DD   
2y2y2yÆ O?O?O?
O?O9à÷ @@@@@@@@ @ @ @@@`@`@€@ @À@àA A A A@A`A€A AÀAÀAàB B > : 6 2 2 . * & "       
     	
ðÐ°pP0 ð ð Ð °  p P 0 0    $(,0488<@@@@@@@@@
@	@@@   
2y2yá2yÌ O?O?O?O?O9O9O? @@@@@@@@ @ @ @@@`@`@€@ @À@à@àA A A@A`A`A€A AÀAàB > : : 6 2 . * * & "       
     	
ðÐÐ°pPP0 ð Ð ° °  p P 0 0   $(,,048<@@@@@@@@
@
@	@@@   
2y2yá2yÆ O?O9O9O?O?O9O9 <<<<<<<< < < <@<@<`<€< <À<À<à= = = =@=`=€= = =À=à=à9à5à1à1à-à)à%à!à!ààààààà	àààààáââãäåææçèééêëìííîïïÏ¯oO/ ï Ï ¯   o O / /   $$(,0448<<<<<<<<
<	<<<<   
2y2yá2yÆ O?O?O?O?O9O9O? <<<<<<<< < < <@<@<`<€< < <À<à= = = =@=`=`=€= =À=À9à5à5à1à-à)à)à%à!àààààààà	àààààáââãäååæçèèéêëëìíîîÏ¯¯oOO/ ï ï Ï ¯   o O / /    $(,,0488<<<<<<
<
<	<<<<   
2y2y2yÆ O?O?O?
O?O9à÷ 8888888 8 8 8 8 8@8`8€8€8 8À8à8à9 9 9 9@9`9€9€9 9À9À5À1À-À-À)À%À%À!ÀÀÀÀÀÀÀÀ	ÀÀÀÀÀÁÁÂÃÄÄÅÆÇÇÈÉÉÊËÌÌÍÎÎ®ŽnnN.. î Î Î ® Ž n n N .    $$(,0048888888
8	8	8888   
2y2yá2yÌ O?O?O?O?O9O9O? 8888888 8 8 8 8 8@8`8`8€8 8À8À8à9 9 9 9@9@9`9€9 9 5À1À1À-À)À)À%À!ÀÀÀÀÀÀÀÀÀ	ÀÀÀÀÀÁÁÂÃÃÄÅÆÆÇÈÈÉÊÊËÌÍÍ®ŽŽnNN. î î Î ® ® Ž n n N .     $((,04488888
8
8	88888   
2y2yá2yÆ O?O?O?O9O?O?O9 4444444 4 4 4 4 4@4`4`4€4 4 4À4à4à5 5 5 5@5`5`5€5 5 1 - - ) % ! !        	 	     ¡¡¢££¤¥¥¦§§¨©©ª««¬­­mmM- í Í Í ­   m M M -    $$(,,0444444
4	444444   
2y2y2yÆ O?
O?O?àûO9O9 0000000 0 0 0 0 0@0`0`0€0€0 0À0À0à1 1 1 1@1@1`1€1€1€-€-€)€%€%€!€€€€€€€€€	€	€€€€€‚ƒƒ„„…††‡ˆˆ‰ŠŠ‹ŒŒŒllL,, ì ì Ì Ì ¬ Œ Œ l L L ,     $((,000000
0	0	000000   
2y2yá2yÆ O?O?O?O9O9O?O9 0000000 0 0 0 0 0@0@0`0€0€0 0 0À0à0à1 1 1 1@1@1`1€-€-€)€)€%€!€!€€€€€€€€€	€	€€€€€‚‚ƒ„„……†‡‡ˆ‰‰ŠŠ‹ŒllLL, ì ì Ì ¬ ¬ Œ l l L L ,    $$((,0000
0
0	0000000   
2y2yá2yÌ O?O?O?O?O9O9O? ,,,,,,, , , , , ,@,@,`,`,€, , ,À,À,à- - - - -@-`-`-`)`)`%`%`!``````````	```````abbccdeeffghhiijkkkKK++ ë ë Ë Ë « ‹ ‹ k k K + +      $$(,,,,
,
,	,	,,,,,,,   
2y2yá2yÆ O?O9O9O?O?O?O9 ,,,,,,, , , , , ,@,@,`,`,€,€, ,À,À,à,à- - - - -@-`)`)`%`%`!`!``````````	```````abbccddeffgghhiijkKK++ ë Ë Ë « « ‹ ‹ k k K + +      $$(,,
,
,	,	,,,,,,,,   
2y2yá2yÆ O?O?O?O?O9O9O9 ((((((( ( ( ( ( ( (@(`(`(€(€( ( (À(À(à(à) ) ) ) )@)@%@%@!@!@@@@@@@@@@	@	@@@@@@@AABCCDDEEFFGGHHIIJJ**

 ê ê Ê Ê ª Š Š j j J J * * 
 
 
 













 
 
$
$
(
(
(	(	(((((((((   
2y2y2yÆ O?O?O?
O?O9à÷ ((((((( ( ( ( ( ( (@(@(`(`(€(€( ( (À(À(à(à) ) ) ) %@%@!@!@@@@@@@@@@@	@	@@@@@@@AABBCCDDEEFFGGHHII**

 ê ê Ê Ê ª ª Š Š j j J J * * 
 
 
 














 
 
$
$
(	(	((((((((((   
2y2yá2yÆ O?O?O?O?O9O9O? $$$$$$$ $ $ $ $ $ $@$@$`$`$€$€$ $ $À$À$à$à$à% % % % ! !            	 	       !!""##$$%%&&'''(())		 é é É É © © ‰ ‰ i i i I I ) ) 	 	 	 																 	 	$	$	$$$$$$$$$$$   
2y2yá2yÆ O?O?O?O9O?O?O9                    @ @ ` ` ` € €     À À à à à! ! ! !            	 	         è è È È ¨ ¨ ¨ ˆ ˆ h h H H ( ( (                     
2y2yy%) O?
O?O?O9O9      
    K         @ @ ` ` € € €     À À à à à! !             	 	   
  K  è è È È È ¨ ¨ ˆ ˆ h h h H H ( (
  C                 
2y2y#2yÆ O?O?O?O9O9O?O9 
  K    @@```€€€  ÀÀÀààààààààààààààààààà
 à àK à á á á â â ã ã ã ä ä ä å å æ æ æ ç ç ç ç Ç Ç Ç § § ‡ ‡ ‡ g g G G G ' '
  C    
2y2y#2yÆ O?O?O?O?O9O9O9   G  @@@```€€€  ÀÀÀàààààààààààààààààà à àG á á â â â ã ã ã ä ä ä å å æ æ æ ç ç Ç Ç § § § ‡ ‡ ‡ g g g G G ' ' '  ?   
2y2y#2yÌ O?O9O9O?O?O9O?   G   @@@```€€€   ÀÀÀÀÀÀÀÀÀÀÀÀÀÀÀÀÀÀÀ À ÀG Á Á Á Â Â Â Ã Ã Ã Ä Ä Ä Å Å Å Æ Æ Æ Æ ¦ ¦ ¦ † † † f f f F F F & & &  ?   
2y2y2yÆ O?O?O?
O?O9 
  ?   @@@@```€€€   ÀÀÀÀÀÀÀÀÀÀÀÀÀÀ
ÀÀ À À? Á Á Á Â Â Â Â Ã Ã Ã Ä Ä Ä Å Å Å Æ ¦ ¦ ¦ ¦ † † † f f f F F F
 & &  ?   
2y2y#2yÆ O?O?O?O9O?O9O9    
  /@@@````€€€€           
           
 ¡ ¡/ ¢ ¢ ¢ £ £ £ £ ¤ ¤ ¤ ¤ ¥ ¥ ¥ ¥ … … … … e e e
 E E % % %   
7   
2y2y2yÆ O?O?O?
O?O9    G     @@@@````€€€€                      G   ¡ ¡ ¡ ¡ ¢ ¢ ¢ ¢ £ £ £ £ ¤ ¤ ¤ ¤ … … … … e e e e E E E E % % % %   ?    
2y2y2yÆ O?O9O9O?O?O9O?     
  
@@
```€€€€€
€€
€€
€€ € € € 
  
 ‚ ‚
 ƒ ƒ ƒ „ „ „ „ d
 d d
 D D
 $ $   	



   
2y2y2yÆ O?O?O?
O?O9   
   @
@@``
````
`` ` `
 a a a b
 b b c c
 C C C #
 # #  


   
2y2y2yÆ O?O?O?O?O9O9O9      
   @
@@@`````
````
``` ` ` ` ` a
 a a a b
 b b b c c c c C
 C C C #
 # # #    


   
2y2y2yÆ O?O?O?O9O?O?O9      @@@@@@@ @ @ A A A B B B B " " "  
   
2y2y2yÆ O?O?O?O?O9O9O?  "     @@@@@@@& @ @ A A A B B B B " " "&      
2y2y2yÆ O?O?O?
O?O9  &         .       ! ! ! ! .          
2y2y2yÆ O?O?O?O9O?O9O9  2  
     F    
 ! ! ! F  
       
2y2y2yÌ O?O?O?
O?O9á  
2y2y2yÆ O?O?O?
O?O9á  
2y2y2yÆ O?O9O9O?O?O9O9á  
2y2y2yÆ O?O?O?
O?O9àâO9O9
O9NyNyNyNy2y2y2y2yÌ O?O?O?
O9O?ÞO9O9
NyO9NyNyO9NyNyNy2y2y2yÆ O?O?O?
O?O9ÞO9O9O9NyNyNy2y2y2yÆ O?O?O?O9O?àâO9O9
NyO9NyNyO9NyNyNy2y2y2yÆ O?O?O?O9O9O?O9ÖO9O9O9NyO9O9
NyO9NyNy
2yNy2y2y1“ÌâR     ”   Œ   
  D0	Û/!0#
  U
 d½   "       €   K   
  E
  2
  9­   
  Bî
  B
  C™[         [            ³  Ëá   wna)\¥TcT„Tç]ÍjÖ   {Zijdƒdcdc`c\cXcTcPcHbLçjÖ #vµh¥hƒhƒhchcdc`c`c\cTcPcHc@bUï   '  rRhƒlƒlƒlƒlƒlchcdcdc\cXcTcLcDB<BIŒ ovµlƒpƒp„p„dc8B!¥ÆB !DBXcPcHB@B4BIÎ        {ZlÅpƒtÆykUB=ïkZ{Þ{ÞsœZÖç !LBHb@B8B,AZ”      mJpƒt„y¬Z1¥g9ÿÿâ#ÿ=ï HbDB8B0B(csœ  {8hƒpƒt¤qï%)o{ÿÿsœ=ï-k1ŒR”ÿÿ1Œ(!DB<B0B$!N1  rlƒpƒt¤MJg9ÿÿcc8BPbHB !%){Þw½ <B8B0A$!9J  i)lƒpƒt£AŒÿÿÿ%eît¤lƒhƒdƒ(!9Îÿ%),A8B0A(!(¥  d¤lƒpƒtƒJ1ÿÿg9@Åy'pƒlƒhƒ`ƒXbBÿB !8A0A$!B  `ƒhƒlƒpƒBÿÿJRDBpƒlƒhƒXb@BTb!{ÞJR!4A,A$!!  \¤dƒlƒlƒ Æ{ÞÿNs0!lƒhƒdbDB!B!ÿ5­$!0A,A !B  ]`ƒdƒlƒ$!ZÖÿw½„Lbdƒ`bXbccg9ÿ! !0A(! ! „  eÍ\bdƒdƒLbçÿÿNs `bXbTb!ZÖÿÿJR   $!!-  nÖTb\b`‚d‚$!=ïÿ%)@AXbTbPb4A1Œ{ÞNs¥ ! !!Eï    TæTbXb\bXb ! TbPbLbHb<A!„ $!(!$!!Ak9    nöLbPbTbTbTb0ADAPbLbDBDB<A(!,!0A(!$!!!Eï	 ^Hb
LbLbHbHADA@A<A8A4A0!,!$!!!-  	 +  QŒ@ADbHADADA@A<A8A4A0A,A(!$!! $Æsœ   #UÎ8A<A<A<A8A8A4A0A,!(!$!! -(sœ   b”8„0A0A0A,A,A(!$! ! AEï wœV1AJ0¥$b !A$„1(Eïk9            ³  ºá   {ni)`¥XcT„Tç]ÍjÖ   {Zmjhƒhƒhchc`cXcTcPcHbLçjÖ #vµpÅpƒlƒlƒpclcdc`c\cXcPcHB@BUï   '  vRpƒtƒxƒt„tƒp„lchcdc`c\cTcPBHB<!Ik §zµpƒtƒxƒ|„x„x„t„pchcdc`c\cXcTBHB@!4 I­        {ZpÅtƒx¤|æ|Æ|„|„x„pƒlchcdc`c\cTBLBD!8 ( Zs      qJtƒx£}}Î}Í}0B,¥,Æ cBBB!! 0 < 0 $Bsœ  {8lƒtƒ|¤}j~”~”}­$ÆkZsœo{
kZkZ'o{)J( < 0   J1  rpƒtƒ|¤}j~1~0}I=Œ{Þÿÿ'ÿ5­( < 0 $ 1)  m)pƒxƒx£|æ}(y(|æF{Þÿÿ'ÿ1Œ( < 0 $  „  d¤lƒtƒxƒx£x¤x¤x¤Iï{Þÿÿÿ-k( < 0 $ !  `ƒlƒpƒtƒxƒ\bTƒ\¥1JZÖZÖVµw½ÿÿÿÿ)J$ 8 ,      `¤hƒlƒpƒpƒB5­JRF1B5­ÆF1ÿÿÿÿ)J$ 4 ,     adƒhƒlƒlƒ c
ÿÿÛÿJR=ïÿÿÿÿ)J  0 (  c  eÍ\bdƒdƒdƒcw½ÿÿÿÿF1Bÿÿÿÿ-k , $  )  nÖTb\b`‚`bcw½ÿÿÿÿF1!B9Î5­9Îc (   AÎ    TæTbXb\bbw½ÿÿÿÿJR , ( (    $     k9    nöLbPbTbBsœ{Þ{Þ{Þ{ÞF1( D @ 8 0 (     Eï	 +]ïHALA ¥ÆÆÆÆc( < 4 0 (     (ç  	 +  Q‹@AD@D D D D @ < 8 0 , (      ¥sœ   Q­4 
8 8 8 4 , ( $    )s{   ^s0B, , , ( $      !Aï s{R19)(„! !c)Aïg9            
 M=[   

 •
 ~õ
 t
 eç
 Ki
 #²      !PaintBoxMorph classPool at: #Prototype put: (SmartRefStream scannedObject).!