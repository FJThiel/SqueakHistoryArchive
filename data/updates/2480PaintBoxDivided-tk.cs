'From Squeak2.9alpha of 16 June 2000 [latest update: #2465] on 9 August 2000 at 2:50:19 pm'!!Morph methodsFor: 'submorphs-accessing' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  Must do this recursively because of transforms.  "	(self fullBounds containsPoint: aPoint) ifFalse: [^ mList].  "quick elimination"	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	submorphs size > 0 ifTrue:		[submorphs do: [:m | m unlockedMorphsAt: aPoint addTo: mList]].	(self containsPoint: aPoint) ifTrue: [mList addLast: self].	^ mList! !!Morph methodsFor: 'submorphs-accessing' stamp: 'RAA 6/11/2000 15:43'!unlockedMorphsAtGlobal: aPoint	"Return a collection of all unlocked morphs in this morph structure that contain the given point, possibly including the receiver itself.  Simplified "	^ self unlockedMorphsAt: (self pointFromWorld: aPoint) addTo: OrderedCollection new! !!PaintBoxMorph methodsFor: 'initialization' stamp: 'tk 7/28/2000 23:26'!initialize	super initialize.	colorMemory ifNotNil: [colorMemory on: #mouseDown send: #takeColorEvt:from: to: self].! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 14:02'!eyedropper: aButton action: aSelector cursor: aCursor         "Take total control and pick up a color!!!!"        | pt feedbackColor |        aButton state: #on.        tool ifNotNil: [tool state: #off].        currentCursor _ aCursor.        self activeHand                showTemporaryCursor: currentCursor                 hotSpotOffset: 6 negated @ 4 negated.    "<<<< the form was changed a bit??"        feedbackColor _ Display colorAt: Sensor cursorPoint.        self addMorphFront: colorMemory.        "Full color picker"        [Sensor anyButtonPressed]                whileFalse:                         [pt _ Sensor cursorPoint.                        "deal with the fact that 32 bit displays may have garbage in the alpha bits"                        feedbackColor _ Display depth = 32 ifTrue: [                                Color colorFromPixelValue: ((Display pixelValueAt: pt)														bitOr: 16rFF000000) depth: 32                        ] ifFalse: [                                Display colorAt: pt                        ].                        "the hand needs to be drawn"                        self activeHand position: pt.                        self world displayWorldSafely.                        "Display fill: colorPatch bounds fillColor: feedbackColor"].        Sensor waitNoButton.        self activeHand showTemporaryCursor: nil hotSpotOffset: 0 @ 0.        self currentColor: feedbackColor.        colorMemory delete.		         tool                ifNotNil:                         [tool state: #on.                        currentCursor _ tool arguments at: 3].        aButton state: #off! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 15:23'!findButton: aSelector	"Find this button in me"	(self submorphNamed: aSelector) ifNotNil: [^ self submorphNamed: aSelector].	submorphs do: [:button |		button actionSelector == aSelector ifTrue: [^ button].		(button respondsTo: #arguments) 			ifTrue: [(button arguments atPin: 2) == aSelector ifTrue: [^ button]]			ifFalse: [(button isKindOf: AlignmentMorph) ifTrue: [				button submorphsDo: [:sub |					(sub respondsTo: #arguments) 						ifTrue: [(sub arguments at: 2) == aSelector ifTrue: [^ sub]]]]].			].	^ nil! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 7/28/2000 14:01'!showColor	"Display the current color in all brushes, both on and off."	| offIndex onIndex center |	currentColor ifNil: [^ self].	"colorPatch color: currentColor.	May delete later"	(brushes == nil or: [brushes first owner ~~ self]) ifTrue: [		brushes _ OrderedCollection new.		#(brush1: brush2: brush3: brush4: brush5: brush6:) do: [:sel |			brushes addLast: (self findButton: sel)]].	center _ (brushes at: 6) offImage extent // 2.	offIndex _ (brushes at: 6) offImage pixelValueAt: center.	onIndex _ (brushes at: 6) onImage pixelValueAt: center.	brushes do: [:bb |		bb offImage colors at: offIndex+1 put: currentColor.		bb offImage clearColormapCache.		bb onImage colors at: onIndex+1 put: currentColor.		bb onImage clearColormapCache.		bb invalidRect: bb bounds].	self invalidRect: (brushes first topLeft rect: brushes last bottomRight).! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 8/9/2000 14:50'!toggleShapes	| tab sh otherTab |	"The sub panel that has the shape tools on it.  Rect, line..."	(sh _ self submorphNamed: 'stamps') visible ifTrue: [sh hide].	otherTab _ self submorphNamed: 'stampTab'.	tab _ self submorphNamed: 'shapeTab'.	(sh _ self submorphNamed: 'shapes') visible		ifTrue: [sh hide.  tab top: self bottom-1.				otherTab top: self bottom-1]		ifFalse: [sh show.  tab top: sh bottom - tab height + 10.				otherTab top: self bottom-1].	self layoutChanged.! !!PaintBoxMorph methodsFor: 'actions' stamp: 'tk 8/9/2000 14:49'!toggleStamps	| tab sh otherTab st |	"The sub panel that has the stamps in it.  For saving and moving parts of an image."	(sh _ self submorphNamed: 'shapes') visible ifTrue: [sh hide].	otherTab _ self submorphNamed: 'shapeTab'.	tab _ self submorphNamed: 'stampTab'.	(st _ self submorphNamed: 'stamps') visible		ifTrue: [st hide.  tab top: self bottom-1.				otherTab top: self bottom-1]		ifFalse: [st show.  tab top: st bottom-0.				otherTab top: st bottom-0].	self layoutChanged.! !!PasteUpMorph methodsFor: 'world state' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point, possibly including the receiver itself.  Must do this recursively because of transforms.  "	"PasteUpMorphs clip their display, so if not in bounds, exit"	(bounds containsPoint: aPoint) ifFalse: [^mList].	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	submorphs size > 0 ifTrue:		[submorphs do: [:m | m unlockedMorphsAt: aPoint addTo: mList]].	mList addLast: self.	^ mList! !!SketchEditorMorph methodsFor: 'access' stamp: 'tk 7/25/2000 16:10'!forwardDirection	"The direction object will go when issued a sent forward:.  Up iszero.  Clockwise like a compass.  From the arrow control.""	| bb result |	bb _ (self valueOfProperty: #fwdButton).	result _ (self center - bb vertices first) degrees - 90.0.	result abs < 1.0e-10 ifTrue: [result _ 0]."	"Workaround because the above can yield spurious microscopic but nonzero values""	^ result	"	^ hostView setupAngle! !!SketchEditorMorph methodsFor: 'start & finish' stamp: 'tk 7/25/2000 15:52'!addRotationScaleHandles	"Rotation and scaling handles"	| |	rotationButton _ SketchMorph withForm: (palette rotationTabForm).	rotationButton position: bounds topCenter - (6@0).	rotationButton on: #mouseDown send: #rotateScalePrep to: self.	rotationButton on: #mouseStillDown send: #rotateBy: to: self.	rotationButton on: #mouseUp send: #rotateDone: to: self.	rotationButton on: #mouseEnter send: #mouseLeave: to: self.	"Put cursor back"	rotationButton on: #mouseLeave send: #mouseEnter: to: self.	self addMorph: rotationButton.	rotationButton setBalloonText: 'Drag me sideways to\rotate yourpicture.' withCRs.	scaleButton _ SketchMorph withForm: (palette scaleTabForm).	scaleButton position: bounds rightCenter - ((scaleButton width)@6).	scaleButton on: #mouseDown send: #rotateScalePrep to: self.	scaleButton on: #mouseStillDown send: #scaleBy: to: self.	scaleButton on: #mouseEnter send: #mouseLeave: to: self.	"Put cursor back"	scaleButton on: #mouseLeave send: #mouseEnter: to: self.	self addMorph: scaleButton.	scaleButton setBalloonText: 'Drag me up and down to change\the sizeof your picture.' withCRs."REMOVED:	fwdButton _ PolygonMorph new.	pt _ bounds topCenter.	fwdButton borderWidth: 2; makeOpen; makeBackArrow; borderColor:(Color r: 0 g: 0.8 b: 0).	fwdButton removeHandles; setVertices: (Array with: pt+(0@7) with:pt+(0@22)).	fwdButton on: #mouseStillDown send: #forward:direction: to: self.	fwdButton on: #mouseEnter send: #mouseLeave: to: self.		fwdButton on: #mouseLeave send: #mouseEnter: to: self.	self setProperty: #fwdButton toValue: fwdButton.	self addMorph: fwdButton.	fwdButton setBalloonText: 'Drag me around to point\in the directionI go forward.' withCRs.	toggle _ EllipseMorph		newBounds: (Rectangle center: fwdButton vertices last +(-4@4) extent: 8@8)		color: Color gray.	toggle on: #mouseUp send: #toggleDirType:in: to: self.	toggle on: #mouseEnter send: #mouseLeave: to: self.	toggle on: #mouseLeave send: #mouseEnter: to: self.	self setProperty: #fwdToggle toValue: toggle.	fwdButton addMorph: toggle.	toggle setBalloonText: 'When your object turns,\how should itspicture change?\It can rotate, face left or right,\face up or down, or notchange.' withCRs.	"	self setProperty: #rotationStyle toValue: hostView rotationStyle."	self forward: hostView setupAngle direction: fwdButton.	"	"Set to its current value"! !!TransformMorph methodsFor: 'submorphs-accessing' stamp: 'tk 7/28/2000 17:39'!unlockedMorphsAt: aPoint addTo: mList	"Return a collection of all morphs in this morph structure that contain the given point.  Map through my transform.  Must do this recursively because of transforms.  "	| p |	self isLocked ifTrue: [^ mList].	self visible ifFalse: [^ mList].	(self containsPoint: aPoint) ifFalse:		["TransformMorph clips to bounds"		^ mList].	p _ transform globalPointToLocal: aPoint.	submorphs do: [:m | m unlockedMorphsAt: p addTo: mList].	mList addLast: self.	^ mList! !PaintBoxMorph removeSelector: #loadRotScalePics!!ObjectScanner new initialize!!self smartRefStream!      class structure	   
Dictionary       0	   AssociationScrollingToolHolder       pickupButtonsstampButtonsstampsthumbnailPicsstart >Symbol        >MorphExtension       lockedvisiblestickyballoonTextballoonTextSelectorexternalNameisPartsDonor
actorStateplayereventHandlerotherProperties >Morph       boundsowner	submorphs
fullBoundscolor	extension >AlignmentMorph       
  y
  �
  �
  �
  �
  �borderWidthborderColororientation	centering	hResizing	vResizinginsetminCellSizelayoutNeededpriorFullBounds >PaintBoxMorph       
  y
  �
  �
  �
  �
  �imageactiontoolcurrentCursor	thumbnailcurrentColorcurrentBrushcolorMemory
colorPatchstampHolderrotationTabFormscaleTabFormcolorMemoryThinbrushes
focusMorph >	Rectangle       origincorner >Point       xy >Cursor       bitswidthheightdepthoffset >RectangleMorph   	    
  y
  �
  �
  �
  �
  �
  �
  � >Color       rgbcachedDepthcachedBitPattern >OrderedCollection       array
firstIndex	lastIndex >Array        >Form       
  �
  �
  �
  �
  � >ThreePhaseButtonMorph       
  y
  �
  �
  �
  �
  �
  �offImagepressedImagestatetargetactionSelector	argumentsactWhen >
ImageMorph       
  y
  �
  �
  �
  �
  �
  � >String        >PasteUpMorph       
  y
  �
  �
  �
  �
  �
  �
  �	presentermodelcursorpaddingbackgroundMorphturtleTrailsForm	turtlePenlastTurtlePositions
isPartsBinautoLineLayoutindicateCursorresizeToFitfileNameisStackLikedataInstancescurrentDataInstanceuserFrameRectanglewantsMouseOverHalos
worldState >TranslucentColor       
  3
  8
  Ealpha >SequenceableCollection        >DisplayObject        >BorderedMorph   	    
  y
  �
  �
  �
  �
  �
  �
  � >
Collection        >DisplayMedium        >Bitmap        >ArrayedCollection        >ProtoObject        >EventHandler       mouseDownRecipientmouseDownSelectormouseStillDownRecipientmouseStillDownSelectormouseUpRecipientmouseUpSelectormouseEnterRecipientmouseEnterSelectormouseLeaveRecipientmouseLeaveSelectormouseEnterDraggingRecipientmouseEnterDraggingSelectormouseLeaveDraggingRecipientmouseLeaveDraggingSelectorkeyStrokeRecipientkeyStrokeSelectorvalueParameterstartDragRecipientstartDragSelectordoubleClickSelectordoubleClickRecipient >PaintBoxColorPicker   
    
  y
  �
  �
  �
  �
  �
  �currentColorlocOfCurrent >	ByteArray        >Object        >	ColorForm   	    
  �
  �
  �
  �
  �colorscachedDepthcachedColormapsuperclasses         0 >
   K
  
� >
   �
  � >
   �
  
� >
  h
  
� >
  �
  � >
  �
  � >
  �
  � >
  l
  u >
  ]
  
� >
  �
  
� >
  "
  
� >
  [
  Q >
  �
  ' >
  �
  � >
  �
  u >
  u
  h >
  �
  ' >
  �
  � >
  
  " >
  Q
  � >
  w
  
� >
  �
  h >
  �
  
� >
  �
  w >
  
  ' >
  '
  Q >
  Hnil >
  c
  
� >
  
G
  u >
  
�
  ' >
  
�
  H >
  
�
  �l%�+�
?���   u%�+�F
     �%��'a
  0    "  � �stamp:	
�  ��������������������������	�����	��[��������������������������������������������������������������������������������������?E?;:������������������?EEE@:C�����������������;EEE=:+6����������������C:;=:*1[�����������������611+11dd����������������o6d1d[d��������d��������L:6d������=@X?po���?E;d���d�����5;=@@@@?@ �:;+����������d%00=@;1611[������������d1$,6d16���������������d136d161��������������d�-6d16����������������7!66166���������������d6!6dd6+����������������161d6������������������1%671+��������������������++��������������������������������������������������������������������������������������������������            "?���"C�"�5�"7��z"�5�"�!"�!"7�V�"3�5�"?�5�"?���"'��")Q�`"/a @"�  "+`  "1�  "#� "'� "7��"-f5H";�("1�� "/f�H"/f5("' "��"%5H"9�"+f�H"9��Q"5��"1���"+g5i"Ԡ�"'��"?��"+g��"���"9�("!� "1�E�"/h��"-hE�")W5i"'�H"%5("� �"?�Ei")W��"5���"-g�i"�!"3���"'5i"%�H"#5("���"5�E�"?�U�"/d� "?���"Ԡ�";��Q"?��")T� "7�� "1��H"
B`"?�j�";�D "?�� "7�j�"'�("/g4 "7�D "-jV1"?���";�D "ե("?�j1"-iEi"+h�H"5�ٍ"1��H"�  "9�ٍ"-iE("9�i�"3��i"���"�5H";�zq"7�j"?�zq"?�zQ"9���"7��Z"?���"1�j�"׵�"C��"5�j�"�!"?��:"7��"���"5�j�"3��q"7��"B`")ZU�"-kY�"5�j1"'ō"?��q"1�i�"?��Q"+jٍ"  "1�i�"9���";�{"-k�1"7���"-nz"+ny�"%�"%��"�Y("�U"9���"#F1"'Vq"!�"7��z"
C�"׵�"�5i"%�Q"�E�"#Z1")]j�"�ŭ"!�"�U�"�E�"�Y�"I��"!Z1"'j�"�Z"��1"5�{z"1�k:"1��Z"#j�"��q"���"-k��"+kZ�"-l�:"�V�"�V�"%�z"�ڶ"�k�"
IF�"�ƶ"��"[z"���"�k�"6Q"��"k�" ��" 
Wz"���"
Lk�" ��" [�"���"��"���")\��"��"�F�"'۞"%[z"!W:"��"�ƕ"�F�"
C"%W:"��"�6�"D""��"��1" �" ")ZV�"�!("�!�"Ci"ַ:"�#�"C�"�#�"�7"է"
BH"A�("�#:"�6"�#Z"�z"�61"%�"ғ�"��")YG"!#z"���"'��"!6q"��"-j��"
B�"%#:"%��"'��"3�k:"-j��"%6Q"-h��"3��"5��:"+hF�"1���"9��Z"5�ƕ"1���"?��z"?�kZ"7��1"5�ƕ"9�1"?��"?��1"?�Z�"?�ƕ"9�5�"    
  |off
  pickup:action:cursor:   
  Estamp:��
��  :�            ���������buttonUp�'a+(a:
  0
  U
  [ �
prevStamp:	
�   �<�	�/��������������������������������������������	������	�����������������	�������������	������������������	������	�����������������	�
���������������������������������            "?���"C�"�5�"7��z"�5�"�!"�!"7�V�"3�5�"?�5�"?���"'��")Q�`"/a @"�  "+`  "1�  "#� "'� "7��"-f5H";�("1�� "/f�H"/f5("' "��"%5H"9�"+f�H"9��Q"5��"1���"+g5i"Ԡ�"'��"?��"+g��"���"9�("!� "1�E�"/h��"-hE�")W5i"'�H"%5("� �"?�Ei")W��"5���"-g�i"�!"3���"'5i"%�H"#5("���"5�E�"?�U�"/d� "?���"Ԡ�";��Q"?��")T� "7�� "1��H"
B`"?�j�";�D "?�� "7�j�"'�("/g4 "7�D "-jV1"?���";�D "ե("?�j1"-iEi"+h�H"5�ٍ"1��H"�  "9�ٍ"-iE("9�i�"3��i"���"�5H";�zq"7�j"?�zq"?�zQ"9���"7��Z"?���"1�j�"׵�"C��"5�j�"�!"?��:"7��"���"5�j�"3��q"7��"B`")ZU�"-kY�"5�j1"'ō"?��q"1�i�"?��Q"+jٍ"  "1�i�"9���";�{"-k�1"7���"-nz"+ny�"%�"%��"�Y("�U"9���"#F1"'Vq"!�"7��z"
C�"׵�"�5i"%�Q"�E�"#Z1")]j�"�ŭ"!�"�U�"�E�"�Y�"I��"!Z1"'j�"�Z"��1"5�{z"1�k:"1��Z"#j�"��q"���"-k��"+kZ�"-l�:"�V�"�V�"%�z"�ڶ"�k�"
IF�"�ƶ"��"[z"���"�k�"6Q"��"k�" ��" 
Wz"���"
Lk�" ��" [�"���"��"���")\��"��"�F�"'۞"%[z"!W:"��"�ƕ"�F�"
C"%W:"��"�6�"D""��"��1" �" ")ZV�"�!("�!�"Ci"ַ:"�#�"C�"�#�"�7"է"
BH"A�("�#:"�6"�#Z"�z"�61"%�"ғ�"��")YG"!#z"���"'��"!6q"��"-j��"
B�"%#:"%��"'��"3�k:"-j��"%6Q"-h��"3��"5��:"+hF�"1���"9��Z"5�ƕ"1���"?��z"?�kZ"7��1"5�ƕ"9�1"?��"?��1"?�Z�"?�ƕ"9�5�"    ����x
��������������!h!h!h!H!H'')')�)�)�)�)�%�!i!H!�*]�HH!h!i!i2221�-�)�!�"=*2�!�!i%�%�)�)�:M6-6-2%���	�?.�%i%�)�-�-�:n:N./�����&_�!i%�-�1�26P�667YZ��"_�!I%�-�22V � � � �6Z�?�(!j)�1�26Oq � � � � �|	�s'!i)�-�2:N6-)�/ � � � �6|	2!i)�-�2:N6-1�)�l� � � �6	!i)�-�2:N6.2-�)�!i* � �	/(!i)�-�2:n:N6-2-�)�!iH-5P!i%�)�22>o:n:N6-2-�)�%�!i!ik%�)�-�22>o:n:n:N6-61�-�)�)�)�)�-�26-6.         
  !
  scrollStamps:action:   
  �
prevStamp:
  ��(�+)q:
  0
  U
  [ �
nextStamp:	
�   �<��������������� 
�������� ����	���� ��������������� ���� 	�������� ��������������� 
����������� ��������������� ����	���� ���� 	�������� ����
����'��� ��������������� ���������������             
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '��  �x
��#�������&  ''
�'G!H!h!h!h  !i!i�*]!�HHH!H!h%�%�)�)�)�  )�)�&2�*"=�!i!i%�)�-�-�1�2  -�-�&.?	����%�)�-�266-  1�2&&_����	�!�-�1�6-6.  22""_��ZY766R%�26-  1�2!�?�Z6 � � � �V)�2  -�2�	�| � � � � �	!j)�2  -�2�|6 � � � ���'!i)�1�  -�2�6 � � ����H%�)�1�  -�2� � �
��'!i)�-�2  22�Uo!i(''H!i)�-�26-  22-�-�)�%�!i!i!i%�)�-�26-:N  6-6.6-2-�)�)�)�)�-�1�66-:N:N           
  !
  
  )�   
  )�
nextStamp:
  ��'p�(�
  0
  U
  [ �stamp:	
�  (������������������������������������������9���������	�K����������{EE=:p�����������������LPEEE;o����������������2@EE?V:6�������	��p::==:+6[�����7111+1dd������	�����61d6d������o����������@1[d�����l@@@{{p��{E@1����������;=@@@@@?;�*;:1����������o'0==?+611d[������������1!$'11d11���������������!761d6o��	�{�����1616d1����������������!-76d6o���������������1!!1d66o���������������1!ddd1�����������������)1,67����	�G��������o+1���������������������������������������������������������
����#��������������������������������            
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  
  
  27
  !
  
  +   
  2
  L
  T
  ��)�*�
  0
  U
  [ �stamp:	
�  ڽ����	�����	�+��  ��������������������������  ��������	�G����������  ��������������������������  ��������������������������  	�������?EP0:�������  ��C�?EEE@S3������  �������������=EEE=:%6�����  �������������C:;=:*1[�����  �������������o76++11dd����  ��������������*6d6d[�����  ���L����������l:6d�����  ���=@@?Lo���\?:d��d������  ���;=@@@@@@+�:=+����������  ���+00=@=6616����������  �����,$!ddd6o����������  �������1616d1���������  ��������1,!6666��������  ����������66d116������  ������������16dd6+����  ��������������161d7o��  ����������������d%%676���  ��������������������1+do��  ��������������������������  ��������������������������  ��������������������������  ��������������������������              
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  
  
  9�
  !
  
  +   
  9�
  L
  T
  ��'q)+
  0   �'�(�)
  B
  U
  [�   x��          
  !
  
  +   
  B
  L
  �                   
horizontaltopLeft
shrinkWrap
  B�        (�;*qS�%�'a+
  0   �%�'A)
  B�
  U
  [�   x��          
  !
  
  +   
  B�
  L
  �
  B{   
  B�
  B�
  B�
  B�
  B�        'A;(�S�)*�+
  0   �)1*�)
  C�
  U
  [�   x��          
  !
  
  +   
  C�
  L
  �
  B{   
  B{
  B�
  B�
  B�
  B�        *�;,!S"  �   �   �stamps�  ,���^�oUsuw���svgV�RoV� """"!!!!*"""!"!!!"""""C ^�k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�-�-�-�-�-�-�-�
1�1�-�-�-�)�)�)�'-�-�-�-�1�1�2666-6-6-6-:N:N:N6M6-6-6-6221�1�-�-�-�S-�)�)�-�-�-�-�-�1�1�1�1�1�1�-�-� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoR� 
-�-�-�-�1�1�1�1�1�21�1�1�1�
-�-�1�1�1�22666-6-6-6-:N:N:N:M6M.6-6-6221�1�-�-�-�
1�1�K1�-� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�-�-�-�-�-�1�1�1�2
21�1�1�
-�-�1�1�26
6-6-6M:M:M:N:N:N:M6-6-6,
22666-6-6-:N
:N:N:N:M6-6-221�-�-�-�-�1�
1�1�k k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�-�-�-�-�1�1�1�1�221�1�:-rk~�~�a�Ef1�)�-�-�266-6-:N:N:N6-^�~�~�r(U�=�-�-�1�26-6-:N:N:Nk6-6-6-6>Nrk~�~ja�Ef-�)�)�-�-�-�-�1�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoR� -�-�-�-�-�1�1�1�
22#1�jk~�/~�v)U�4�1�)�-�-�26-6-:N:N:N#:N:MNN~h~�e�E#5h-�-�1�26-:M:N:N[:N6-6-6nk~�/~�r)Q�0�1�%�)�)�-�-�1�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
-�-�1�1�1�2
22#6q�~�~�~�j	Ie0�a-�%�-�1�26-:N
:N:N
:n:n#:N:M^
zI~�~�vKY�=`%')�-�1�66-:N:N[:N6-6->-r~�~�~�fIe,�a)�%i)�)�-�-�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
-�-�3-�1�1�22221�1�=MD]�]�Q�9$�@�!h)�-�26-:M
:N:N+:n:n:n:N:N6-A�I$U�a�Y�EE,�� %H)�-�26-:N:N[:N6-6-:AMD]�]�Q�9$�@!!H%i)�-�-�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
-�-�31�1�1�1�1�221�-�1�`$�,�,�(� �@�'%�-�1�6-:N:N:N#:N6-:-` �(�,�,�$�a '%i)�1�6-:N:N[:-6-62-ia$�,�,�(� �@�'!h)�-�-�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
-�1�31�1�-�1�1�1�1�-�-�-�1aaaaa !''%h)�1�6-:M:N:N�:N:M6-6-1�5� �aa�a  !H)�-�6:M:N:N:N:N:M:-6-6-6-21�-�-aaaaa !H%�)�-�1�1� k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� -�-�-�B=�5�1�-�
-�-�+)�%�^
Me@    '%h)�1�6-:->NJNBM>-
6-6-�62-�5�j	$�    A!H)�1�6-:M:NJnFM>M:-66221�-�)�^*IE@    !H%�)�-�1�1� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�-�6zv'n(j)b)V
E�5�)�)�n�~�Q�������'%�-�1�6-:Mbmzv(nIj*^KNB5�-�R,v�v($������'!h)�1�6-:N:Nzv(r)nJb*V+F9�-�-�n�~�Me���b��'!h)�-�-�1�2 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�-�U�m�vz(zHz(z'zH~H9$)HMeY�=%'����'!H)�-�66->-i�q�v'zHzHz'z(~Hi�%=FU�Q�$�����!H)�-�66-:N:Nm�vz(zHz'z'zH~(5-HM�Y�8�%(����!H%i)�-�1�22 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�5�8�H@T�aCi�m�v(e�aa$� �@!���!H%�-�1�6-:M:-=�@aL`Yidm�rzI,�`�$�a@���!H%�-�1�6-:N:N:N8�H@T�aCi�m�z(a�aa$��@!���G%i)�-�1�1�22 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�-�-�-�$!4 D`L�\�,�a��� ����!G%i)�1�66-:N:N6-6-, <@H`T�L�a������'%i)�1�6-:N:n:n:N6-1�$!4 D`L�\�,�a���$�����'!h)�)�-�1�266 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�-�-�)�%i$�$B( , 4`0` a��� ���!H%�-�26-:N:N:N6-6-�-�$�$ , 0@4`$`
����$��!H)�-�26-:N:n>n:N6-2-�)$B( ,@4`0` a��� ���!G%i)�-�1�2266 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�1�-�)�%�!h!'(�( 0 0@0`,`a�aa$�!'!H)�-�26-:N:N:N:N6-1�-�)�%((B, 0@0@0`$`a�a �%'!H)�-�26-:N>o>o:n:M6-�)�%H(�( 0 0@0`,`a�aa$�'!G%�)�-�1�2266 k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� 21�1�-�-�)�%�!h'!' b, , 0@4@$@a��a)h%�)�1�66M:N:n:N:M61�-�%�!H$�(!, ,@4`,@a��`$�)�)�1�6-:N>n>o>n:N6-2-�)�!h!($b, , 0@4@$@a��a)H%h)�-�-�1�226 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2221�1�-�)�)�%h!G'�(c, , 0@0@ a��` �-�-�26-:N:n:n:N:M6-1�-�)�%h!'$�( , 0@0@(@a�aa)H-�26-:N>n>o>n:N6-2-�)�%h!G �(c, , 4@0@ a��`$�)i)�-�-�1�26 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
22�o21�-�-�)�%�!h' �,�, , 4@0@a��a)-�26-:N>o>o>n:N6-21�)�%�!H!'$�0A, 0@4@$@��a �-�26-:N>o>o>o:N:-21�)�%�!H'!,�, , 4@,@a��a))�-�1�22 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 22662221�-�-�)�%�!hG$�(�0�4 $ aa$�-�1�6-:N>o>�>�>�>o:N6-2-�)�%�!H'%,�,�4a, @�@-i1�6-:N>o>�>�>�:n:N6-2-�)�%i!H'$�,�0�4 $ aa$�%�)�-�1�2 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2666-6-6-6-621�-�-�)�%�%h!H''%,�4�$�1�)�1�6-:N>o>�B�B�B�>�>o:N6-1�-�)�%�%h!H!'!'-4�(�)&)�-�6:N>o>�B�B�>�>o:n6-61�-�)�%i!H'%,�4� �1�%i)�-�1�2 g3k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 666-6-+221�-�)�)�%�%h!H!H!G!H%h%�)�1�6-:N>�B�
B�B��B�>�:n:N6-21�-�)�)�%�%h%h%i)�-�1�6-:N>oB�B�B�B�>�>�>o:N6-21�-�)�%�%h!H!'''!G!h%�)�-�1�2 g3k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 666-6-W6-21�-�-�)�)�)�%�%�)�)�-�26-:n>�B�B�B�F�F�B�B�B�>�>o:n:N6-21�-�-�)�)�-�-�66-:n>�
B�B�cB�B�>�>o:n:N6-21�-�)�)�%�%i%h%i%�)�-�-�26 g3k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 66-
6-6-c6-6M6M:N6N:M6-6-6-621�1�-�-�-�-�1�26-:N>oB�B�B�F�F�F�F�B�B�B�B�B�>�>n:N:-6-622266-:N>o>�B�B�_B�>�>�:n:N:M6-21�-�-�)�)�)�)�-�-�1�26 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 66-6-6-:N:N:M:M
6-6-K6622266-:M:n>o>�B�B�B�B�B�B�B�B�B�B�B�B�B�>�>o>n:N:M6-:M:M:N>n>�B�
B�B�oB�B�B�>�>�>o:n:N:N6-6-21�1�-�-�-�-�1�1�26- k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 666222666-6-6-66-6-6-:N:N>o>�>�B�B�B�B�B�B�>�>�>o:n
:N:N:n>o>�>�s>o>o>n:n:N:N:N6-6-621�1�1�-�1�1�226 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2221�1�1�1�1�1�1�1�222266-6-6M:N:n:n:n>o:n:n>o>o:n:n:N:N
:n:n:n:N:N:N6M6-
6-6-S62221�-�-�-�-�-�-�-�1�2 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 22226-2;-�-� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�.V�V�V�)�.V�V�V�-�.V�V�?V�%�)� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�.V�V�V�!h.V�V�V�%h.V�V�?V�!h%� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�.V�V�V�.V�V�?V�'!H k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V��.V�V�V��.V�V�?V��' k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V��.V�V�V��.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�b.V�V�V�b.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�b.V�V�V�b.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�B.V�V�?V��� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�B.V�V�V�b.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V�b.V�V�V�b.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�.V�V�V��.V�V�V��.V�V�?V�� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�2�2�2G�' k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�)�%�!H�������������
�����
����#������&�����
����K����'!H k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� -�-�)�%i!H''&''''!H!H!h!h!h!H!H'')
''
'G!H!h!h!h!h!HG'
''S'!H%� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�-�-�)�%�%i!h!H!H!H!H!h
!h!h!h%i%i%i
%�%�?%�)�)�)�)�)�)�)�%�!i!H!�*]�HH!h!i!i�*]!�HHH!H!h%�%�)�
)�)�)�%�
%�%�%�%i%i%i%h!h!h!hG!h%i%�)� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 21�-�-�-�)�)�)�)�-�
-�-�-�-�-�-�1�2
22G1�-�)�!�"=*2�!�!i%�%�)�)�&2�*"=�!i!i%�)�-�-�1�221�1�-�-�-�-�-�
-�-�-�)�)�)�
)�)�G)�)�-�-� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 221�1�1�-�
-�-�1�1�1�2
2226
6-6-C6-6M6M:M6-6-2%���	�?.�%i%�)�-�-�&.?	����%�)�-�266-6-6-6-6221�1�
1�1�71�2 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 22266-6-6M:N
:N:NK:N:n:n>n:n:n:N./�����&_�!i%�-�1�2&&_����	�!�-�1�6-6.:N:N
:n:n:N:N6M6-
6-6-K6-66662 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 222666-
6-6-6-6M:M:N:N:N:N:n:n:n
>o>o[>�>�>o6P�667YZ��"_�!I%�-�22""_��ZY766R%�26-:N:o>o>�>�>�>o>o>o:n:n:n
:N:N:N6M6-6-; k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2266-
6-6-6-:N
:N:N:N:n>n>o>o>o>�>�G6PV � � � �6Z�?�(!j)�1�2!�?�Z6 � � � �V)�2:N:o>o>�
>�>�>�>o>o>o:n:n:N:N
6-6-; k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2666-
6-6-6-:M
:N:N:n:n
>o>o>o>�
>�>�G>o6Oq � � � � �|	�s'!i)�-�2�	�| � � � � �	!j)�26.:N>o>�
>�>�
>o>o>n:n:N:N
6-6-; k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 2266
6-6-6-6M
:N:N:N:n:n>n>o>o>o>�
>�>�G>o:N6-)�/ � � � �6|	2!i)�-�2�|6 � � � ���'!i)�1�6-:N>o>�
>�>�
>o>o:n:n:n:N:N:N:N:M
6-6-; k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 22266-6-
:N:N:N:n:n>n
>o>o
>�>�[>o:N6-1�)�l� � � �6	!i)�-�2�6 � � ����H%�)�1�6-:N>o>�>�>�>�>o>o>o>o:n:n:N
:N:N:M6-
6-6-? k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 222666-
6-6-:M:N
:N:Nw:n:n>n>o>o>o>o>�>�>o>o:N6.2-�)�!i* � �	/(!i)�-�2� � �
��'!i)�-�26.:N>o>o>�>�>o>o>o>n>n:n:n:N:N:N:N:M6-6-3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
2266-
6-6-6-:M
:N:N:N:n:n:n>o>oC>o:n:N6-2-�)�!iH-5P!i%�)�22�Uo!i(''H!i)�-�26-:N:n>o>o>o:n:n:n
:N:N:N:M6-6-3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
22266-6-:M:N
:N:N:n:n:n>o>o>o?:n:N6-2-�)�%�!i!ik%�)�-�22-�-�)�%�!i!i!i%�)�-�26-:N:N:o
>o>o>o>n:n:n:N:N:M6-
6-6-G6-6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�222666-
6-6-6M:M
:N:N:N:n:n:n>n>o
>o>o?:n:n:N6-61�-�)�)�)�)�-�26-6.6-2-�)�)�)�)�-�1�66-:N:N:n>o
>o>o>o>n:n:N
:N:N:N:M6-6-;62 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�
2266
6-6-6-6M:N:N:n:n:n>n
>o>o;>o>n:n:N:N6-221�1�226-6N6N6N6-221�1�226-:N:N:n>n
>o>o>o>n:n:n:n:N
:N:N:N6-6-6-;62 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 1�1�
22266-6-6-:M:N:N:n:n:n>n
>o>o>o:n:n:N:N6-6-6-6-6N
:N:N:N6N
6-6-6N:N:n>n>n>o
>o>o>n:n:n:N
:N:N:N:M6M6-
6-6-;6622 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �  / k4k4oTsuw���svgV�w�kWk4k4oUw�{��{�svoU
k4k4k4oToToT;oTk4k4gcb�^�Z�Z�Z�V�V�V�Z�Z�Z�Z�Z�^�^�b�cggk3k4k4oToToTwoTk4k4k4ggb�b�^�^�Z�Z�Z�Z�^�^�^�Z�Z�Z�Z�Z�^�b�ggk4oToToUoUsUsususvsvw�w�{��{�w�k4Z�Row�{�k4k4oTsv{���{�svsuoU
oToToUoUoToTkTk4k4k4k4k3k3k3k4k4
k3k3k4k4oToToUoUoToToTk4"k4k4Sk3k3k4k4k4oToToUoUoUoUsususvwvw�w�{�{��{�sub�V�Ro{�  s�gk4oUw�{��{�{�w�w�svsu�susu7svsvw�w�w�{�{�{��{�svk4^�V�s�    {�ggk4suw�{���
{�{�{�w��w�w�w�w�{�{�{�{�7{�w�kTb�V�Row�      w�ggk4oUw�{�{�{����{�{�{���������{�{�{�w�svk4b�Z�V�s�	   s�b�gk4oTsuwvw�{�
{�{�{������{�
{�{�{�w�svoUk3b�Z�V�s�   w�b�b�ggk4oToUsususvw��w�w�w�svsvsuoUk4g3b�^�Z�s�s�   {�s�^�^�b�b�ggg�k4k4ggcb�^�Z�s�s�{�     �{�w�s�szs�sy�sysyszs�w�{�    h   N   u'��+�q
     �(�*q#
  q4
  U
  [ �rect:	
�  &�c������������������������������������������������������������������������������������������������	�k���������������������������������������������ddddddddd����������������������������������������غ��	�������������������	�3���������������غ����������ۉ�������������ؾ��	�3���������������غ����������������������������	�3���������������ؾ����������ۉ�������������ؾ��	�3��������������dغ������������������������؏���	�k��������������dأ������������������������ddddd�d�����������������������������������������������	���������            
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  &�c������������������������������������������������������������������������������������������������	�k���������������������������������������������ddddddddd����������������������������������������غ��	�������������������	�3���������������غ����������ۉ�������������ؾ��	�3���������������غ����������������������������	�3���������������ؾ����������ۉ�������������ؾ��	�3��������������dغ������������������������؏���	�k��������������dأ������������������������ddddd�d�����������������������������������������������	���������            "?���"C�"�5�"7��z"�5�"�!"�!"7�V�"3�5�"?�5�"?���"'��")Q�`"/a @"�  "+`  "1�  "#� "'� "7��"-f5H";�("1�� "/f�H"/f5("' "��"%5H"9�"+f�H"9��Q"5��"1���"+g5i"Ԡ�"'��"?��"+g��"���"9�("!� "1�E�"/h��"-hE�")W5i"'�H"%5("� �"?�Ei")W��"5���"-g�i"�!"3���"'5i"%�H"#5("���"5�E�"?�U�"/d� "?���"Ԡ�";��Q"?��")T� "7�� "1��H"
B`"?�j�";�D "?�� "7�j�"'�("/g4 "7�D "-jV1"?���";�D "ե("?�j1"-iEi"+h�H"5�ٍ"1��H"�  "9�ٍ"-iE("9�i�"3��i"���"�5H";�zq"7�j"?�zq"?�zQ"9���"7��Z"?���"1�j�"׵�"C��"5�j�"�!"?��:"7��"���"5�j�"3��q"7��"B`")ZU�"-kY�"5�j1"'ō"?��q"1�i�"?��Q"+jٍ"  "1�i�"9���";�{"-k�1"7���"-nz"+ny�"%�"%��"�Y("�U"9���"#F1"'Vq"!�"7��z"
C�"׵�"�5i"%�Q"�E�"#Z1")]j�"�ŭ"!�"�U�"�E�"�Y�"I��"!Z1"'j�"�Z"��1"5�{z"1�k:"1��Z"#j�"��q"���"-k��"+kZ�"-l�:"�V�"�V�"%�z"�ڶ"�k�"
IF�"�ƶ"��"[z"���"�k�"6Q"��"k�" ��" 
Wz"���"
Lk�" ��" [�"���"��"���")\��"��"�F�"'۞"%[z"!W:"��"�ƕ"�F�"
C"%W:"��"�6�"D""��"��1" �" ")ZV�"�!("�!�"Ci"ַ:"�#�"C�"�#�"�7"է"
BH"A�("�#:"�6"�#Z"�z"�61"%�"ғ�"��")YG"!#z"���"'��"!6q"��"-j��"
B�"%#:"%��"'��"3�k:"-j��"%6Q"-h��"3��"5��:"+hF�"1���"9��Z"5�ƕ"1���"?��z"?�kZ"7��1"5�ƕ"9�1"?��"?��1"?�Z�"?�ƕ"9�5�"    
  !
  tool:action:cursor:   
  qIrect:	
�  �x���	C   ������
   ������   ������
	����   ������
	����   ������	����   ������
	����   ������	����   ������	����   ������	����   ������
	����   ������	����   ������
	�_���   ������

   ������   ������   ������������	��   ����������   ����������   ����������            ���������   "?���"    "?���"  "?�  " � "  �" ��"?�� 
  B{" �" "�"'��"/���"7��" � " @"�`"
��" �"��"� "@"�`"��"�"��"!�~"#��?"%�~_")�~�"+���"-�~�"1�"3��?"5�_"9��";���"=��"    " 0 " d " 	� " � " � "   �" 0�" d�" 	��" ��" ��"  �" 1�" e�" 	��" ə" ��"  e" 2e" fe" 	�e" �e" �e"  2" 32" g2" 	�2" �2" �2"  �" 3�" g�" 	��" ��" ��"�  "�0 "�d "ɔ "�� "�� "� �"�0�"�d�"ɔ�"���"���"��"�1�"�e�"ɕ�"�ə"���"�e"�2e"�fe"ɖe"��e"��e"�2"�32"�g2"ɗ2"��2"��2"��"�3�"�g�"ɗ�"���"���"�  "�0 "�d "�� "�� "�� "� �"�0�"�d�"���"���"���"��"�1�"�e�"���"�ə"���"�e"�2e"�fe"��e"��e"��e"�2"�32"�g2"��2"��2"��2"��"�3�"�g�"���"���"���"&P  "&S0 "&Vd "&Y� "&\� "&_� "&P �"&S0�"&Vd�"&Y��"&\��"&_��"&P�"&S1�"&Ve�"&Y��"&\ə"&_��"&Pe"&S2e"&Vfe"&Y�e"&\�e"&_�e"&P2"&S32"&Vg2"&Y�2"&\�2"&_�2"&P�"&S3�"&Vg�"&Y��"&\��"&_��"3   "3#0 "3&d "3)� "3,� "3/� "3  �"3#0�"3&d�"3)��"3,��"3/��"3 �"3#1�"3&e�"3)��"3,ə"3/��"3 e"3#2e"3&fe"3)�e"3,�e"3/�e"3 2"3#32"3&g2"3)�2"3,�2"3/�2"3 �"3#3�"3&g�"3)��"3,��"3/��"?�  "?�0 "?�d "?�� "?�� "?�� "?� �"?�0�"?�d�"?���"?���"?���"?��"?�1�"?�e�"?���"?�ə"?���"?�e"?�2e"?�fe"?��e"?��e"?��e"?�2"?�32"?�g2"?��2"?��2"?��2
  B{"?�3�"?�g�"?���"?���"?���
  ��(�#*q:
  q4
  U
  [ �ellipse:	
�  v�C����������������������������������������������������������������	�7����������������������������������������������������	�����������������������d��ۢ�����������������������ؾ����������������d������������������������������������������������������������۽�����������������������������������������������������������������������������������������㺾�������������۽���������۾���
����k��ؽ���������؊���������������ؽ������������������������������������ؾ������������������������؉���	�{���������������ۺ��������ؽ����������������۸����۾�������������������������������������������ۣ�����������            
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  v�C����������������������������������������������������������������	�7����������������������������������������������������	�����������������������d��ۢ�����������������������ؾ����������������d������������������������������������������������������������۽�����������������������������������������������������������������������������������������㺾�������������۽���������۾���
����k��ؽ���������؊���������������ؽ������������������������������������ؾ������������������������؉���	�{���������������ۺ��������ؽ����������������۸����۾�������������������������������������������ۣ�����������            
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
  �   
  ��ellipse:	
�  ̓��������� ������������� ����������
�������� ���������g
������ ���������������� ������������������� ������h��	�
���� ���������	����� ���������	����� �����
������ ��������
��� ����������� ���������
��� ������
���	����� ���������	��
��� ��������	�'���� ������������������� 	�������� 	��������� 	������������� ����          ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��(�;*�P
  q4
  U
  [ �polygon:	
�  �������������	�;�������������   ���������������������������������   ����
�����g���������������������   ���������������������������������   ������������������ۆۆ�����������   ��������������������������   ��������������������������������   ������������۾������������������   �������������������������������   �������������������������������   ������������۾������������������   �����������ž������������������   �������������������������������   ������������۾������������������   �������������������������������   ������������ۺ������������������   ������������۸������������������   �����������dddddd�dd������   ���������������������������������   �������������������   ������������	��������������      !         
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  �������������	�;�������������   ���������������������������������   ����
�����g���������������������   ���������������������������������   ������������������ۆۆ�����������   ��������������������������   ��������������������������������   ������������۾������������������   �������������������������������   �������������������������������   ������������۾������������������   �����������ž������������������   �������������������������������   ������������۾������������������   �������������������������������   ������������ۺ������������������   ������������۸������������������   �����������dddddd�dd������   ���������������������������������   �������������������   ������������	��������������      !         
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
  �   
  �fpolygon:	
�  ߓ��������� ������������ ������������ ����������������� ������h�������� ������g������� ������g
��������������� ���������������������� ��������������������� ��������������������� ���������������������� ��������������������� ��������������������� ���������	������ ���������	�S���� ���������	����� ������
	C���� ���������� ���������� ���� ����          ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��(�P*�h
  q4
  U
  [ �star:	
�  ���o������������������������������� ������������������������������� ������������������������������� ����������������ؾ������������� ��������������Å�������������� ���������������ؾ������������ ����������������������������� �������������d��������������� ���������������������������� �����������ؿ���؏������� ��������������������۽����� �����������	����۽������ ����������ؿ����������������� ����������������������������� ����������������������������� ����������������������������� ��������������ddۿ����������� ������������؉��۾�������� ����������۸����ؽ�������� ����������أ��������ؽ�������� ����������������
�����g��� ������������������������������� ������������������������������� �������������������������������             
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  ���o������������������������������� ������������������������������� ������������������������������� ����������������ؾ������������� ��������������Å�������������� ���������������ؾ������������ ����������������������������� �������������d��������������� ���������������������������� �����������ؿ���؏������� ��������������������۽����� �����������	����۽������ ����������ؿ����������������� ����������������������������� ����������������������������� ����������������������������� ��������������ddۿ����������� ������������؉��۾�������� ����������۸����ؽ�������� ����������أ��������ؽ�������� ����������������
�����g��� ������������������������������� ������������������������������� �������������������������������             
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
  �   
  ��star:	
�  Ax%����	�����	��	�������������	������������	���������	�;�����������g����������h
	����������	������������������	�����������	�����������	�����������	����
��	����
�	�������	����������
�5�         ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��(��*�
  q4
  U
  [ �line:	
�  m�/�����������������������������   ������������
����#���������   ��������������������	��   ��������	���������������   �����������������������������   �����������������������������   ����������������������������   ����������������������������   ������������	�7��������   ����������������������������   ��������	��3������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   �����������ۿ����������������   �����������������������������               
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  m�/�����������������������������   ������������
����#���������   ��������������������	��   ��������	���������������   �����������������������������   �����������������������������   ����������������������������   ����������������������������   ������������	�7��������   ����������������������������   ��������	��3������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   ���������������������������   �����������ۿ����������������   �����������������������������               
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
  �   
  ��line:	
�  ,~���  ���  �����  ����  ���  ���  �������  �������  �������  ��������  ��������  	������������  	������������  ��������	���  ��������	���  ���������	���  ������������	���  �����������  �����������  �����������  ���           ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  �
  D0 �shapes�  5�� ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�B�B�
B�B�SF�F�F�F�F�J�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�B�B�
B�B�SF�F�F�F�F�F�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�B�B�
B�B�
F�F�KF�F�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�B�B�B�B�KF�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�B�B�
B�B�SB�B�-�%nB�B�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>�>�>�>�B�B�B�B�B�B�B�B�1��!>xB�B�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�>�>�>�B�
B�B�WB�>�1�� !-�:V>wB�B�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>�>�>�>�B�B�B�B�>�>x1�� !-�65:5:V>wB�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>�>�>�>�B�>�>�>�>x1�� !)�6665:V>wB�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:x>x>�
>�>�_>�>x>x1�� !)�21�664:V>xB�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>�>�>�>�>�>x>w:  !)�1�1�1�6:5>WB�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>�>�>�>x>x:wFt  !)�1�1�1�264:V>wB�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>y>x>x>x:wFu  !)�1�-�-�1�6:U>wB�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>x>x:x:WFu  !)�-�-�-�1�6:5>WB�B�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x>x:x:wFt  !)�-�-�-�1�665:W>�B�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:x:xk>x:x:w5�  !)�-�-�-�1�265:V>xB�F�F�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W:X:x:x:xg!m  !)�1�-�-�1�265:V>xB�B�F�F�J�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:x:w-��)�21�1�1�265:V>wB�B�F�F�F�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:x:w:W6V65222265:V>wB�B�F�F�F�J�J�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x
:x:xk:x:w:W6V656566565:V>w>�B�F�F�F�F�J�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:xg:w:W6V66656V:V>w>�B�B�F�F�F�F�J�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:xg:w:W:W:V:V:W>w>x>�B�B�F�F�F�F�J�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:x:w:W:W:W:V:W:W>w>x>�>�B�B�B�F�F�F�J�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W:X:x
:x:xk:W:W:W6V6V6V:V:V:W:W>w>w>x>�B�B�F�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:x:w1�-�)�)�)�S-�-�>w>xB�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:W:WkZ5��BBWc�! :5:V>xB�F�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:X:W6W6V-kB�����
��W��" 665:VB�B�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:W:W6W66cd-�)�)�)�)�)�-�
-�-�Sd 1�2:5>wB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:w:W6W65!d)�
)�)�_)�-�-�-�-�-�d -�1�64:VB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W:W6W6V65!d)�)�)�)�)�-�-�1�1�-�-�� -�1�6:VB�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W:W:W6V25!d)�)�)�)�-�1�26621�� -�1�6:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6V25!d)�)�)�-�26565:V:V656� -�1�6:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6625!d)�)�-�1�6:V:W>w:w:V65� 1�1�6:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6625!d)�)�-�265:W>x>x>x>w:V� 1�1�6:V>xF�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6625!d)�)�-�265:W>x>�>x>w:V� 1�26:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6V25!d)�)�-�1�65:V:w>x>w:W:6� 1�1�6:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6625Bd)�)�)�-�265:V:V:V:V65� 1�1�6:V>xF�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W#6W:W:W:W:W6V25BCLL!L!m%n%�)�
)�)�Sc -�1�6:V>xF�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W:W:W6W6V65    !BBWB   -�1�6:V>�F�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:w:W6V65+��������������-�1�6:VB�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:w:W6W662-�-�)�)�)�)�)�-�-�-�O-�265>wB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:W:W6W6V652-�-�)�)�)�-�-�
-�-�S-�1�264:V>xB�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:w:X:W6W666521�-�-�-�-�1�
1�1�S2665:V>wB�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W:W:X:x:x:W:W66652222W66465:5:V>wB�B�F�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:x:w:W:W6V66656565W65:5:V:W>wB�B�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W:X:x
:x:x:w:W6V666565W65:6:V>w>xB�B�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:W:W65-�5��B!B�%n65:V:V>wB�B�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:x:w:W6W-�=�  c�d !  d)�65:V>wB�B�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:X:x:w:W:W9�! �)�-�-�-�-�!nd  !)�65:V>wB�F�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:x:W:W6W1�! !%�-�-�-�-�-�-�-�-�
 !)�65:V>wB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W#6W:W:W:w:W6W2FR !%�-�-�)�-�-�-�
-�-�S
 d265>VB�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6V5� �-�)�)�-�-�-�1�2221�1�� %n6:5>wB�F�J�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:W6W66� 
)�)�c-�1�26565656562, �1�64:VB�F�J�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W6W6V2Bd)�)�)�-�-�265:V:W:w:W:V651�!d1�6:U>wB�F�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ws66%�!�)�)�)�-�265:W>x>x>�>x:w:V64!d1�2:5>wB�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ws66%�!�)�)�)�-�2:V:x>�B�B�>�>x:W65!d1�265>VB�F�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W6W662!C)�)�-�-�65:V>x>�B�B�B�>�>w1� �1�264>VB�F�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W6W662d %�)�)�-�65:V>x>�B�B�B�>�>w1� 
1�264>VB�F�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W6W6W662, �)�)�-�266:w>x>�B�>�>w65� )�1�265>WB�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W6W6V252C �)�-�265:V:w>x>x>w65R� �1�1�6:5>wB�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ws6V662%� ! �)�-�265:V:V:V2> c1�1�1�6:V>xF�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6622%� ! "�!n1�-�)�-�  �-�1�1�265>VB�F�J�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W:W6W662-�)��   !!   !
-�-�1�1�6:V>wB�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:W:W:W6V652-�-�)�
�d��!L-�-�-�1�1�6:5>wB�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:X:x:W:W666521�-�-�-�
-�-�W-�-�1�1�665:V>�B�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:x:w:W6V6522-�-�
-�-�W-�1�2665:V>xB�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:x:x:w:W6V656521�1�-�1�1�1�2665:V>xB�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:w:W6V6562221�22665:V>w>�B�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:w:W6V65652222266465:V>wB�B�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:w:x
:x:x2Jt)l���W���:5:V>wB�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:x:x:W-�-k  S )�65:V>xB�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:x:x:W)�c�-�-�-�-�-�-�-�-�
 �665:VB�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:x:x:x:W6V)�!�-�)�)�)�-�-�-�-�)� "1�6:5>wB�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W6W:W
:x:xo:W6V%�!�)�)�)�)�-�-�-�1�-�� %�1�64:VB�F�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:w:W66%�!�)�)�)�-�-�2221�+ �1�6:5>wB�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:w:x:x:x:W66%�!�)�)�)�-�26565656-� "1�265>wB�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:W:W66%�!�)�)�-�1�65:V:W:V:V64� %�1�64:VB�F�J�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:x:x:w:W66%�!�)�)�-�265:W>w>w:W:5!L �1�6:5>wB�F�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:x:x:x:W:W66%�!�)�)�-�266:W>x>x>w:V2 "1�265>wB�F�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:W:W66%�!�)�)�-�265:W>w>w:W:V6� %�1�6:VB�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:x:x:W:W66%�B�)�)�)�-�665:V:V:V656!L �-�6:5>wF�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:x:x:x:W66%���,,!L!m%�)�)�)�)�)�)�%� "-�1�65>wB�F�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:w:W66%�!   !BBO   %m1�64>wB�F�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W
:x:x:W6V1����
��[���������)�1�64>wB�F�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:x:W6V652-�)�
)�)�[)�-�-�-�-�-�-�)�-�-�2:5>wB�F�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W+6W:W:X:x:x:x:x:W66652-�-�)�)�-�-�-�-�-�
-�-�K1�1�64:VB�F�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:x:x:W6W662521�-�-�-�
1�1�S2222665:V>wB�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:x:w:W6V65652
22W6646565:5:V:V>w>xB�F�F�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:x:W:V:V65
6565W65:V:V:W>w>w>�B�B�F�F�J�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:xg:x:w:W65B�6565:5:V>w>xB�B�B�F�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W
:x:xo:x>x>x>x:x:w:W>3-k %�665:V>w>xB�B�F�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:xg:w:W655� ! d1�265:V>wB�B�F�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:xk:w:W:W665� �d !L-�265:V>wB�B�F�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:X:x:W:W:W6V66651�c )�, d)�-�265:V>wB�B�F�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W:w:W:W-�1�1�*�  �)�%�c  !��	%n-�>W>xB�F�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W:W)�Z�NsB  C�%�%�%�+c!  !  65:V>xB�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:W6W2�! !!n)�%�%�%�S
  �265>wB�F�J�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W66-�C C!n%n%n%�%�)�)�)�)�!m%) �)�-�6:VB�F�J�J�J�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W662-�� B!M!n%�)�)�-�-�%�-k �%�)�-�6:VB�F�J�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:W:W6W6625-�)�� �%n%�)�-�-�-�5� 
%�%�)�-�6:VB�F�J�J�J�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:W6W6W652-�+ �%n%�)�-�1�1�� )�)�)�)�1�64>wB�F�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:w:w:W6W662-�!+ 
%n%�!m%�-�-�� ,)�)�-�2:5>xB�F�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:X:X:W6W652* L+%J�!�%�+ �)�)�-�6:VB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:W:x:x:W:W662� c  c"  d �)�)�1�65>wB�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:x:W6V25   c
%n%��!  c)�-�1�:5>xB�F�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6Ww:X:x:x:x:W:W65 !"+%�%�%�%�%�%�� c)�-�2:VB�F�F�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:W:x:x:x:w:W662-�)�)�)�)�)�
)�)�S)�-�1�64>WB�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
6W6W{6W:W:X:x:x:x:x:W6V652-�-�-�-�-�-�-�-�-�-�1�6:V>wB�F�J�J�J�J�N�N�N�O k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x:x:x:x:w:W6V652
22
66S665:V>wB�B�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 6W6W:X:x
:x:x:w:W:V66656566:V
:V:VS:V:W>wB�B�F�F�J�J�J�J�N�N�OO k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� ""&!!""""7" ^�k4oTsuw���svgV�w�  kWk4k4oUw�{��{�svoUk4k4oToTwk4k4k4ggb�b�^�^�Z�Z�Z�Z�^�^�^�Z�Z�Z�Z�Z�^�b�ggk4oToToUoUsUsususvsvw�w�{��{�w�k4Z�Row�  {�k4k4oTsv{���{�svsuoU
oUoUoUoToToT"k4k4k4k3k3k4k4k4oToT
oUoU;sususvwvw�w�{�{��{�sub�V�Ro{�    s�gk4oUw�{��{�{�w�w�Fsusu?susvsvw�w�w�{�{�{��{�svk4^�V�s�      {�ggk4suw�{���{�{�Fw�w�w�{�{�{�w�kTb�V�Row�	 w�ggk4oUw�{�{�{��J�����{�{�{�w�svk4b�Z�V�s�  	   s�b�gk4oTsuwvw�{�F��{�{�w�svoUk3b�Z�V�s� w�b�b�ggk4oToUJw�w�svsvsuoUk4g3b�^�Z�s�s�     {�s�^�^�b�b�gJk4k4k4ggcb�^�Z�s�s�{�   �{�w�s�szNsysysyszs�w�{�      ?   x   �) z*��
  
  U
  [ �brush6:	
�  گ���� ���� �����cǶ� �������ǣ�xwwwwx��������Ƕ� ������Ǣ�qM((Sw������Ƕ� �����Ǣq(LS�����Ƕ� ����Ǣw	M����Ƕ� ���ǣ�Mw���Ƕ� ���ǢSLL���Ƕ� ���ǜw��Ƕ� ���ǜLM��Ƕ� ����w((��Ƕ� ����w��Ƕ� ���ǜ(L��Ƕ� ����xLS��Ƕ� ���ǜS���Ƕ� ���Ǣ�(���Ƕ� ����ǢS����Ƕ� ����Ǣ�S	g�����Ƕ� �����Ǣ�wL������Ƕ� ������ǣ��wM(LS��������Ƕ� ��������ǣ��������������Ƕ� �����Ƕ� ���� ����             "?���
  D0"?���"  "?�  " � "  �" ��"?�� "?��" �" "�"'��"/���"7��" � " @"�`"
��" �"��"� "@"�`"��"�"��"!�~"#��?"%�~_")�~�"+���"-�~�"1�"3��?"5�_"9��";���"=��"    " 0 " d " 	� " � " � "   �" 0�" d�" 	��" ��" ��"  �" 1�" e�" 	��" ə" ��"  e" 2e" fe" 	�e" �e" �e"  2" 32" g2" 	�2" �2" �2"  �" 3�" g�" 	��" ��" ��"�  "�0 "�d "ɔ "�� "�� "� �"�0�"�d�"ɔ�"���"���"��"�1�"�e�"ɕ�"�ə"���"�e"�2e"�fe"ɖe"��e"��e"�2"�32"�g2"ɗ2"��2"��2"��"�3�"�g�"ɗ�"���"���"�  "�0 "�d "�� "�� "�� "� �"�0�"�d�"���"���"���"��"�1�"�e�"���"�ə"���"�e"�2e"�fe"��e"��e"��e"�2"�32"�g2"��2"��2"��2"��"�3�"�g�"���"���"���"&P  "&S0 "&Vd "&Y� "&\� "&_� "&P �"&S0�"&Vd�"&Y��"&\��"&_��"&P�"&S1�"&Ve�"&Y��"&\ə"&_��"&Pe"&S2e"&Vfe"&Y�e"&\�e"&_�e"&P2"&S32"&Vg2"&Y�2"&\�2"&_�2"&P�"&S3�"&Vg�"&Y��"&\��"&_��"3   "3#0 "3&d "3)� "3,� "3/�    �g�g�"3  �"3#0�"3&d�"3)��"3,��"3/��"3 �"3#1�"3&e�"3)��"3,ə"3/��"3 e"3#2e"3&fe"3)�e"3,�e"3/�e"3 2"3#32"3&g2"3)�2"3,�2"3/�2"3 �"3#3�"3&g�"3)��"3,��"3/��"?�  "?�0 "?�d "?�� "?�� "?�� "?� �"?�0�"?�d�"?���"?���"?���"?��"?�1�"?�e�"?���"?�ə"?���"?�e"?�2e"?�fe"?��e"?��e"?��e"?�2"?�32"?�g2"?��2"?��2"?��2"?��"?�3�"?�g�"?���"?���"?���	
���ȯ%%%% %%%% %%%% 	%W!#/3333/$!%%%%%%%% %%%%%%%#&(�3$"%%%%%%% %%%%%%#'T�������0$%%%%%% %%%%%#3T	����-$"%%%%% %%%%!(��3#%%%%% %%%%#0��0$"%%%% %%%%T����3"%%%% %%%%0����(!%%%% %%%%3����%%%% %%%%3������%%%%% %%%%����0%%%% %%%%/0����2%%%% %%%%$����%%%% %%%%"��T%%%% %%%%%#��#6%%%% %%%%%"#	�G���6%%%%% %%%%%%"30�������T"6%%%%%% %%%%%%%!3(�026%%%%%%% 	%%!!%%%%%%%% %%%% %%%% %%%%             "?���"�`")ZV�"5�kZ"7��z"9�{�"-hF" � "9�5�"%��"'  "/`  "#�@"5�� "?� "1��Q"�  "%� "/b� "+b� "��"р "9��Q"#5H"�� "#�"A� "/h��"7���"! �"+f�("%��"1� @"+hE�"'5i"%�H"#5("5�E�"�`"Ӑ`"B@")V4�"3���"?�E"=���"��@";�D�"ե"A�@"?�UH"C�"Ԡ�"-g��"+e� "?�j�"7��("7�� "?�� ";�� "?�i�"?�Y"?��H"-hD�"'4 "9�YH"!4�")W��"1�D�"�5H"5��"1��1"1���"5�� "?�� "5���"!��"?�zq"?�zQ"7�i�"9���"/k��"?���")ZVQ"��"
B��"�!"?��:"�5i"3��q"� �"'��"-kZ"-kY�"7��Q";��q"'ō"?��q")ZU�"?��Q"�5"+jٍ"%EH"%D�"��`"?�� "9���";�{"5��"7�z�"�� "3�zQ"/nz"%�")^y�"� �"�� "�EH"׵�"�YH"%��"%��"׵i"'�"��("�Y("7��Z"
M� "��"I��"1��"#F1"���"�5i"�5("D �"��"��i"!Z"�E�" � " D "��i"��H"�Z"3��z"3��Z"/m�:" 	�H"�F"
O��"1��Z" ��"��i"#�q"
C�"O��"
O��"�5�"#��"�F�"��q"���"
K��"���"�Fq"���"�61"%�z"F�q"1��Z"�F�"��"�F�"�F�"G6�"'۞"!W:"���" "Q"�Ǿ"G�z"7�"HG�" �" ��"
C�H"�G�" " ��"D!�"
C"'۾"�6�"��Z"�G:"��" �"
C��"+k[�"���"ֵ�"%F�"!Fq"!F�"�7:"B�i"���"�!�"A�("
@�"���"զ�"+i�:")YG"'��"B("+iG"��("+c��"!�Q"+`�")W��"-j��"'Fq"+hƶ"?���"�H"+g6�"-hF�"ӑi"��H")V6Q"7�W:"5���"ԡi"?�"1�F�"1���"3�F�"+h�Q"?���"?�["?�ƕ"?���"1�Fq"9�6"/e!�"+d!H"3�Fq"?��1"?���"7��H"/c�"7��"?��H
  D0   ��� �  �  c  R�  kZ  o{  s�  Z  !  q�  H�  L   \   Db  h`  |`  br  0@  H`  \�  T�  $�  0`  r�  E�  (`  Eh  a  ^.  no  AG  U�  I$  aB  V  M�  I�  E�  jM  ,�  4�  �  Q�  e�  ~H  z'  $�  v  9h  b  ~�  �  1&  Y�  U`  T  ni  m�  ~   v     ~�  ~�  Z  M�  r�  A�  Q�  bF  5�  k4  b�  b�  j�  `  k/  A�  �  �  oO  so  ^�  �  R�  %(  �  )H  �  9�  g3  !  Nn  Z�  Z�  or  w�  Nl  �  R�  �  9�  V�  JJ  JF  -c  �  s�  w�  ku  o�  >`  g�  _�  K,  S�  !G  +`  2
  5�  :�  K/  Kn  1�  Op  )�  2�  o�  `  *�  f  c8  FQ  >o  -�  %�    !h  -�  B�  6M  �     *�  &*  :�  g�  gz  _y  j  6P  �  cz  �  )k  Fs  �  �  �  %�  F�  2T  .3  %�  �  :v  2  !m  )�  J�  �  c:  :V  :x  6W  &  �  N�  B�  )�    "=  �  �     �  6  �  *_   �  |    �  N�  1�  >:  :  )p   N  �  V�  )  5�  JU  B  B  1�  �  ).  %  i    (  9t  Vy  RX  N6  �  VX   �  T�  A�  T  Q�  Z�  NS  V5  }  (
  U�  Z  0�  ,�  Q�  n�  iu  1+  |u  bT  b�  fT  V2  ~v  ~�  ~4  ~�  bS  q�  ]L  U
  fS  }�  }l  m*  \�  l�  }*   	
�  ��:::+	:::+:	:::: +:::	:+:::
::+:::: 
::+:3:+::::::+::::::::+: ::::+:::::::::+:::::+:::::: 	:�%-4>//>4.3C :::+::: :+:::::"/4[4.3::::::: :::+::-&O[83+:::+: +::::-&O[83::::: ::::C"4-::::: :+::6"73::+: ::::4O[!:::: ::::"d+:::: ::+:> :::+ ::::" :::: ::::"d :::: ::::4d:::: :+::-[)::+: ::::3Od:::: ::::.[L?::::: +::::3.LE::::+ ::::::3[ddLE:::::: :::::::+!67ddddL?E::::+:: :+:::::::+% ? ::+::::: ::::+::::::+ ::+::::+	::+:::::+:::             
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  brush:action:nib:   
 #�brush6:�  �d�  �      �     ���    ���    ���    ?���    ���    ���    �����  �����  �����  �����  �����  �����  �����  ?�����  ?�����  �����  �����  �����  �����  �����  �����  ������ ������� ������  �����  �����  �����  �����  �����  �����  ?�����  ?�����  �����  �����  �����  �����  �����  �����   �����   ���    ���    ?���    ���    ���    ���     �       �    2   2   ���������
  ��'Pz(��
  
  U
  [ �brush5:	
�  ���   ��   �������Ƕ�   �������Ƕ�   ����ǃ��Ƕ�   ��������ǜ�wwwwx������Ƕ�   ������ǣ�wS((MS�����Ƕ�   ������ǜq����Ƕ�   �����ǜwMS���Ƕ�   �����ǢS	���Ƕ�   �����ǜ	��Ƕ�   ������w(	L��Ƕ�   ������w��Ƕ�   �����ǜ(	L��Ƕ�   ������xM	S��Ƕ�   �����Ǣw	����Ƕ�   �����Ǣ�w���Ƕ�   ������Ǣ�Sx����Ƕ�   �������ǣ��SLLS������Ƕ�   ���������ǣ�����������Ƕ�   �������Ƕ�   �������Ƕ�   �������Ƕ�   �������Ƕ�   ��   ��               
 %�
  D0
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�	
��Ặ%%   %%   %%   %%   %%   	%s$3333/#"%%%%%%%   %%%%%%%!$3�(!%%%%%   %%%%%%%$'T�������T$"%%%%%   %%%%%%3(���������!%%%%   %%%%%%	����"%%%%   %%%%%%	����T!%%%%   %%%%%%3	����0%%%%   %%%%%%3��%%   %%%%%%	����0%%%%   %%%%%%/(	����Y%%%%   %%%%%%#3	�G���!%%%%   %%%%%%"T���������3%%%%   %%%%%%%"�������/%%%%%   	%!"20�0Y!6%%%%%%   	%%%!%%%%%%%%   %%   %%   %%   %%   %%   %%               
 2�
 2�
 2�
 2�
 2�
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
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
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
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
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
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
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
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
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
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
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
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
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
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
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
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
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
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
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
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
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
 =�
 =�
 =�
 =�
 =�
  D0   ��� �  �  c  R�  kZ  o{  s�  Z  !  q�  H�  L   \   Db  h`  |`  br  0@  H`  \�  T�  $�  0`  r�  E�  (`  Eh  a  ^.  no  AG  U�  I$  aB  V  M�  I�  E�  jM  ,�  4�  �  Q�  e�  ~H  z'  $�  v  9h  b  ~�  �  1&  Y�  U`  T  ni  m�  ~   v     ~�  ~�  Z  M�  r�  A�  Q�  bF  5�  k4  b�  b�  j�  `  k/  A�  �  �  oO  so  ^�  �  R�  %(  �  )H  �  9�  g3  !  Nn  Z�  Z�  or  w�  Nl  �  R�  �  9�  V�  JJ  JF  -c  �  s�  w�  ku  o�  >`  g�  _�  K,  S�  !G  +`  2
  5�  :�  K/  Kn  1�  Op  )�  2�  o�  `  *�  f  c8  FQ  >o  -�  %�    !h  -�  B�  6M  �     *�  &*  :�  g�  gz  _y  j  6P  �  cz  �  )k  Fs  �  �  �  %�  F�  2T  .3  %�  �  :v  2  !m  )�  J�  �  c:  :V  :x  6W  &  �  N�  B�  )�    "=  �  �     �  6  �  *_   �  |    �  N�  1�  >:  :  )p   N  �  V�  )  5�  JU  B  B  1�  �  ).  %  i    (  9t  Vy  RX  N6  �  VX   �  T�  A�  T  Q�  Z�  NS  V5  }  (
  U�  Z  0�  ,�  Q�  n�  iu  1+  |u  bT  b�  fT  V2  ~v  ~�  ~4  ~�  bS  q�  ]L  U
  fS  }�  }l  m*  \�  l�  }*   	
�  ��:+::+::+::+::+   +::+:::::+::::   ::::::+:::::::+:	:+:   ::+::::::::+::::+::+:::+:   :::::::+::   +:::
:+::��::+::::::+:::   ::+::::::38"/>>4-3+:::::+   :::::::C/4[44::::::   +:::+::/O[8!:::::   ::::::!/-%::+:   ::+:::8473::::   ::::::"Od+::::   :::+::"d ::+:   :+::::"	� ::::   ::::::"d ::::   :::+::4d::+:   +:::::-8::::   ::::::C4[+?2::::   :+::+::!doH::+::   :::::::+3!6ddd�L�?::::::   	:#:C+ ?2::::::+   ::+::+::: ::	::+:::   ::   ::::
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
 I�   
 Kibrush5:�   Q    �� �� �� �� 
�� ?�� �� ������ 
?�� �� �� �� ��  ��                 ���������
  ��%�{'0�
  
  U
  [ �brush4:	
�  ����   ��   �������Ƕ�   �������Ƕ�   �������Ƕ�   ��������Ƕ�   ����ǣ�xwww�������Ƕ�   ���ǣ�SM(L������Ƕ�   ���Ǣq�����Ƕ�   ���ǜS����Ƕ�   ����wLL����Ƕ�   ����w����Ƕ�   ���ǜL����Ƕ�   ���ǜ�����Ƕ�   ���Ǣ�MS�����Ƕ�   ����Ǣ�SM�������Ƕ�   �����ǣ�����������Ƕ�   �������Ƕ�   �������Ƕ�   �������Ƕ�   �������Ƕ�   �������Ƕ�   ��   ��               
 %�
  D0
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�	
���e�%%   %%   %%   %%   %%   %�%   %%%%%!$/333$"!%%%%%%%   %%%%!(�0T%%%%%%%   %%%%#'�����$"%%%%%%   %%%%T�������"%%%%%%   %%%%30�������0!%%%%%%   %%%%3���������%%%%%%%   %%%%0�������T%%%%%%   %%%%$T�������%%%%%%   %%%%"(�����Y%%%%%%   %%%%%"#T�(%%%%%%%   %%%%%%!"%%%%%%%%   %%   %%   %%   %%   %%   %%   %%               
 2�
 2�
 2�
 2�
 2�
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
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
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
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
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
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
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
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
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
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
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
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
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
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
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
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
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
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
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
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
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
 =�
 =�
 =�
 =�
 =�
  D0   ��� �  �  c  R�  kZ  o{  s�  Z  !  q�  H�  L   \   Db  h`  |`  br  0@  H`  \�  T�  $�  0`  r�  E�  (`  Eh  a  ^.  no  AG  U�  I$  aB  V  M�  I�  E�  jM  ,�  4�  �  Q�  e�  ~H  z'  $�  v  9h  b  ~�  �  1&  Y�  U`  T  ni  m�  ~   v     ~�  ~�  Z  M�  r�  A�  Q�  bF  5�  k4  b�  b�  j�  `  k/  A�  �  �  oO  so  ^�  �  R�  %(  �  )H  �  9�  g3  !  Nn  Z�  Z�  or  w�  Nl  �  R�  �  9�  V�  JJ  JF  -c  �  s�  w�  ku  o�  >`  g�  _�  K,  S�  !G  +`  2
  5�  :�  K/  Kn  1�  Op  )�  2�  o�  `  *�  f  c8  FQ  >o  -�  %�    !h  -�  B�  6M  �     *�  &*  :�  g�  gz  _y  j  6P  �  cz  �  )k  Fs  �  �  �  %�  F�  2T  .3  %�  �  :v  2  !m  )�  J�  �  c:  :V  :x  6W  &  �  N�  B�  )�    "=  �  �     �  6  �  *_   �  |    �  N�  1�  >:  :  )p   N  �  V�  )  5�  JU  B  B  1�  �  ).  %  i    (  9t  Vy  RX  N6  �  VX   �  T�  A�  T  Q�  Z�  NS  V5  }  (
  U�  Z  0�  ,�  Q�  n�  iu  1+  |u  bT  b�  fT  V2  ~v  ~�  ~4  ~�  bS  q�  ]L  U
  fS  }�  }l  m*  \�  l�  }*   	
�  �::   ::+:
:+::+   ::::+:::::   :::+::::+:::	::   
:+::::+::+:::::+:   :+::::   :::::::+:�:   +:+:+,">/"8+:::+::+   :::::8"4[4:+:::::   ::::-jO73::::+:   :+::"O[!::::::   ::::>d%::+:::   ::::" ::::::   +:::"d:::+::   ::::.[)?::::::   ::+:!8�::+:::   ::: C,,[ddL??::::::+   :::: :+% ? :::::::   :+::::   ::+::G:::+:   :::::::+::::::+::::::   ::::+::::::+:::::+:::   :+:::::::+::	:+   ::::::+::::::+::+::::               
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
 I�   
 e�brush4:�   ,   �  
?�  
�  ��  
�  
?�  �              ���������
  ��'Pi(�~
  
  U
  [ �brush2:	
�   �~������
�����
�����
�����
�����
�����
�����
������Ǣw�x������
������ǜ(S������
�������x(L������
������ǜSL�������
������Ǣ���������
�����
�������������������a�            
 %�
  D0
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�	
��:~�%%%#3/"%%%%T%%%/�0%%%%$0%%%%""6%�%            
 2�
 2�
 2�
 2�
 2�
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
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
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
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
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
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
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
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
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
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
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
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
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
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
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
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
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
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
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
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
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
 =�
 =�
 =�
 =�
 =�
  D0   ��� �  �  c  R�  kZ  o{  s�  Z  !  q�  H�  L   \   Db  h`  |`  br  0@  H`  \�  T�  $�  0`  r�  E�  (`  Eh  a  ^.  no  AG  U�  I$  aB  V  M�  I�  E�  jM  ,�  4�  �  Q�  e�  ~H  z'  $�  v  9h  b  ~�  �  1&  Y�  U`  T  ni  m�  ~   v     ~�  ~�  Z  M�  r�  A�  Q�  bF  5�  k4  b�  b�  j�  `  k/  A�  �  �  oO  so  ^�  �  R�  %(  �  )H  �  9�  g3  !  Nn  Z�  Z�  or  w�  Nl  �  R�  �  9�  V�  JJ  JF  -c  �  s�  w�  ku  o�  >`  g�  _�  K,  S�  !G  +`  2
  5�  :�  K/  Kn  1�  Op  )�  2�  o�  `  *�  f  c8  FQ  >o  -�  %�    !h  -�  B�  6M  �     *�  &*  :�  g�  gz  _y  j  6P  �  cz  �  )k  Fs  �  �  �  %�  F�  2T  .3  %�  �  :v  2  !m  )�  J�  �  c:  :V  :x  6W  &  �  N�  B�  )�    "=  �  �     �  6  �  *_   �  |    �  N�  1�  >:  :  )p   N  �  V�  )  5�  JU  B  B  1�  �  ).  %  i    (  9t  Vy  RX  N6  �  VX   �  T�  A�  T  Q�  Z�  NS  V5  }  (
  U�  Z  0�  ,�  Q�  n�  iu  1+  |u  bT  b�  fT  V2  ~v  ~�  ~4  ~�  bS  q�  ]L  U
  fS  }�  }l  m*  \�  l�  }*   	
�  5~1:+:::::::::+::+::+::::
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
 I�   
 ~�brush2:��@   �   @            ���������
  ��%�j' ~
  
  U
  [ �brush1:	
�   �d������
�����
�����
�����
�����
�����
�����
�������ww�������
�������x�������
������ǜ��������
�����
�����
�����
�����
�������������������)�            
 %�
  D0
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�	
��$d�%%%%33$%%%%%%/�%%%%%%$6%%�%            
 2�
 2�
 2�
 2�
 2�
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
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
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
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
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
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
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
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
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
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
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
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
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
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
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
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
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
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
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
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
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
 =�
 =�
 =�
 =�
 =�
  D0   ��� �  �  c  R�  kZ  o{  s�  Z  !  q�  H�  L   \   Db  h`  |`  br  0@  H`  \�  T�  $�  0`  r�  E�  (`  Eh  a  ^.  no  AG  U�  I$  aB  V  M�  I�  E�  jM  ,�  4�  �  Q�  e�  ~H  z'  $�  v  9h  b  ~�  �  1&  Y�  U`  T  ni  m�  ~   v     ~�  ~�  Z  M�  r�  A�  Q�  bF  5�  k4  b�  b�  j�  `  k/  A�  �  �  oO  so  ^�  �  R�  %(  �  )H  �  9�  g3  !  Nn  Z�  Z�  or  w�  Nl  �  R�  �  9�  V�  JJ  JF  -c  �  s�  w�  ku  o�  >`  g�  _�  K,  S�  !G  +`  2
  5�  :�  K/  Kn  1�  Op  )�  2�  o�  `  *�  f  c8  FQ  >o  -�  %�    !h  -�  B�  6M  �     *�  &*  :�  g�  gz  _y  j  6P  �  cz  �  )k  Fs  �  �  �  %�  F�  2T  .3  %�  �  :v  2  !m  )�  J�  �  c:  :V  :x  6W  &  �  N�  B�  )�    "=  �  �     �  6  �  *_   �  |    �  N�  1�  >:  :  )p   N  �  V�  )  5�  JU  B  B  1�  �  ).  %  i    (  9t  Vy  RX  N6  �  VX   �  T�  A�  T  Q�  Z�  NS  V5  }  (
  U�  Z  0�  ,�  Q�  n�  iu  1+  |u  bT  b�  fT  V2  ~v  ~�  ~4  ~�  bS  q�  ]L  U
  fS  }�  }l  m*  \�  l�  }*   	
�   �d%::+::::+::+:::+::+:::+::::+:
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
 I�   
 �brush1:���            �        
  ��(`�*��
  
  U
  [ �toss:	
�  �����������������Lc{{{qp{LL����	���� 	��_���lq\qqqq]tttttptpp����������� ��������l\\\\mqq]]tqtttptpxpvpo�������� ������l\\\\q]]]tttpttptttxppxpvpo������ �����m\^y\]]tttttxxxxxxpxpxpvpvvvr����� ����\^y\]]ttZnDDZxtxtxxxxpxxpvpvvor���� �߅\\\\]ttxx[e�Z[xxxpxxxxxpxxxvvvrvrd�� ��{\|\]ttxxxxr�xxx4Rxxv4vxxIIvxxvvrrr�� �qq]]xtxxxxxI�xvZwwrvnwwv[��Zpvvvrrr�� �Lt]tttxpxxxxr�xr�xZD[nx[vw[v[xxvvvrv� �LtptxpxxxxxxI�xInxInr�Zvx/�[xxxxvrrr� ۄpxppxpxxxxxr�x[wx[wxrwnvx/�>vpvvrrr� ��pvvvvxvxtpxI�xrwx/Dxxvw[xx[wvvvrrrR�� ��ovvvvvpvxxpr�xrw[w&r�IwrDe/nvvrrRrg� ���orrrvvvvvxrDxxIwZxxZw&xvDw[vrRrr[e�� ����rvrrrvvvvvxvvxvxxvxvxvvvvvrrrr[�� �����dvrrrrvvGvvvrrrR[rg���� ������6rrrrrrrvvvvvvrvrrrRrR�r4����� �������[rRr�[rrr[4e������ ���������O[[R[RrrrRrrR[[gee������� ���������������ee�e���������� ��������������������������������� ��������������������������������    '         
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
 ��
  !
  
toss:with:   
 ��toss:
  ��(`�*��
  
  U
  [ �keep:	
�  u�������������lq]pLL����	߷��� ����������?PPPP]@MX`XXX2XS)L����������� �������HPE\EPPP@`@XXXXXXSXtSS*��������� �����HEyyEPP]MXXXXVXXVXSXXSSSSSo������� ����\EyEPPMXXVVVVVVVVSVSSSSXSx:Sv������ ���EEyEPM&IV8VV	V#VVSSSSSSYSd���� ��PyE^PXXn>Yw8VVV�VSSSTQSd��� �?^EPMXVVD>.wYVT.VVVYIYVYTYISVSSQSQ+�� �XPPMXVVVD[wIVVwwDVYDwwVIDw�8VYSSQQQd�� �XMXXVVVV�DVV>Y�VWDVw.R�V>eVVYSYQS[�� �XXXVVVVVDD�WVeD>�I8w>w/IDVWwTVSYSQQ[� �XSSSVVVVD89VD[ISUD>ITIDVWwYVYSQQQ4� �:SSSSSVVD>W�Y/&VUTIeVIII�VIDYYSQQQQ� �LSvSSSSVD>VD/Iw[�VTwI�WI�>D&YYQQQQ4� ��*SQQYYY&IVIeV9wIVV>w/VID/DISQQQQIe�� ���+QSQQYYYYYYVYVVYVVYVV8nYTQQQRQI�� ����+QQvQQYYYG>wTQQQRQ4����� �����d,QQQQQQTTSTSTSTTQQQQQQQ6Ie����� ������6RQQQ�RrQ7[j������ ��������gO[76RQQQQQQQR7I4Ze�e�������� ��������������eee�e������������ ��������������������������������� ���������������������������������    '         
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
 ��
  !
  
keep:with:   
 ��keep:
  ��(`�*Ю
  
  U
  [ �undo:	
�  B�+��������������������������������������� 	��3������������������������������� ��������������������������������������� ��������������������������������������� ��������������������������������������� ������������������������������������� ��������������n���������w������������� ��������������w���������n������������� ��������������n��n�O���n��n�������� ��������������w���n�wd��O���������� ��������������n�w��n����w��4�wd������� �������������w�n��n�w��n�n�w������ ��������������n�w��D����w����d������ �����������w�����n�n�n��n�������� ������������w����D����������D�O������� ��������
�����#��������������e��� ���������������d���� ����������������	�����d������ ������������d������� ������������	�c��de��������� ��������������eee���������������� ����������������������������������    '         
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
 ��
  !
  
undo:with:   
 ��undo:
  ��%�(`<
  
  U
  [ �paint:	
�  "�O8O8888887776767	66   888.O88[[777777777676#6   88888[77777767676676	6+66161   O888887777777777676666666666,
6,61�6   8888[77777777767676666666661661616161   888777777767666666666616116116111111   888777777777676766666616,16,61611,1,1,   88[777777676666666616,616111	1#1   8[7777777776666666616161111   8	76�{66616116111,1,111111%1%%%   8[7777777667666666616111111111%%%%%1%11%   777777767666661161,1,111111%1111%+1+1+   [7777776666666666116111111+��������+�+1   77777767666666,11111111%%�����������++   7777776676666616161,1111%1����������L%%+   7777776666661616111111%1����������؆++++   7777776766666616111111%1L���������*++++   77777666666161611,111%%������������*+++++   7777676666661,11111%%+d�d��������+*+++++   77776676661611,111%1*؏��d����*++*+*+*   77767666666,611111%o�d�d�dd�*+++*+*+**   7776666661161,11%1+)�ddd�+*++**+*+**+   7767666616611111%+@PX�ddd�+*++**+****+*   76766666,6111111+@PEP;o�++++++	*S*   77666666616,11%1?PEPMV:d+++++**+********   676666611111111PEPM;Xv1)**+***+	*'*   7666666161,11%SPEPMXS:%L++***+**	*#)   6766661611111PEPMX;v+o***+*	*�* )**   7666666,1,115MEPP=S:*r+++*+******* *)*))   66666161611MEPP=XS+r++**+*******)*)))))   7666661611*MEPPMV:*rp++*+*******)* ))*) )   676661611+XEPP@XSv+)++*+*+******)))*)))))   66666611%?PPEM=V5+o)+*+*******) *) ))))))   66661611XPPEMXXS+v)+++*++****)**)))))))5)   766661%X?EPM=V:+o*+++*+*******))) )))55)5   666611PPEP=X5v+++++********)*)))))5))   666665MPEMMVS*%L++++*+*+******)*))))))))   66616 VPP@XS*Qo++++*+*+***** ) ))))555)   66661o1+X;S v)*+++++++*****)*)))))5))5   666661�6+:p+o*1+++++*+*+*****)*)))5))5   666611*d1rr+*1+++++++******)*)))))5)))   66616,1%od+*1++++++*+*+******) ))5))   666616111%11+1+1+++++****** 	)�)   6666,11111%11++++++*+*+****** ))))5)))   66666661111+%11+++++++*****)*)*)))))))   6666111111%11++++++++******* *)))))))))   6661661,111%%1+1++++*++*****))*))))5)))   766661,1111%1+1++++++*+*+*****) ))))5)))   666661611111%1+++++++++*****)**) ))))5)))      )   1      
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  "�O8O8888887776767	66   888.O88[[777777777676#6   88888[77777767676676	6+66161   O888887777777777676666666666,
6,61�6   8888[77777777767676666666661661616161   888777777767666666666616116116111111   888777777777676766666616,16,61611,1,1,   88[777777676666666616,616111	1#1   8[7777777776666666616161111   8	76�{66616116111,1,111111%1%%%   8[7777777667666666616111111111%%%%%1%11%   777777767666661161,1,111111%1111%+1+1+   [7777776666666666116111111+��������+�+1   77777767666666,11111111%%�����������++   7777776676666616161,1111%1����������L%%+   7777776666661616111111%1����������؆++++   7777776766666616111111%1L���������*++++   77777666666161611,111%%������������*+++++   7777676666661,11111%%+d�d��������+*+++++   77776676661611,111%1*؏��d����*++*+*+*   77767666666,611111%o�d�d�dd�*+++*+*+**   7776666661161,11%1+)�ddd�+*++**+*+**+   7767666616611111%+@PX�ddd�+*++**+****+*   76766666,6111111+@PEP;o�++++++	*S*   77666666616,11%1?PEPMV:d+++++**+********   676666611111111PEPM;Xv1)**+***+	*'*   7666666161,11%SPEPMXS:%L++***+**	*#)   6766661611111PEPMX;v+o***+*	*�* )**   7666666,1,115MEPP=S:*r+++*+******* *)*))   66666161611MEPP=XS+r++**+*******)*)))))   7666661611*MEPPMV:*rp++*+*******)* ))*) )   676661611+XEPP@XSv+)++*+*+******)))*)))))   66666611%?PPEM=V5+o)+*+*******) *) ))))))   66661611XPPEMXXS+v)+++*++****)**)))))))5)   766661%X?EPM=V:+o*+++*+*******))) )))55)5   666611PPEP=X5v+++++********)*)))))5))   666665MPEMMVS*%L++++*+*+******)*))))))))   66616 VPP@XS*Qo++++*+*+***** ) ))))555)   66661o1+X;S v)*+++++++*****)*)))))5))5   666661�6+:p+o*1+++++*+*+*****)*)))5))5   666611*d1rr+*1+++++++******)*)))))5)))   66616,1%od+*1++++++*+*+******) ))5))   666616111%11+1+1+++++****** 	)�)   6666,11111%11++++++*+*+****** ))))5)))   66666661111+%11+++++++*****)*)*)))))))   6666111111%11++++++++******* *)))))))))   6661661,111%%1+1++++*++*****))*))))5)))   766661,1111%1+1++++++*+*+*****) ))))5)))   666661611111%1+++++++++*****)**) ))))5)))      )   1      
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��on
  
  �   
 �Opaint:	
�  �����)����)����)����)����)����!��wS�������!�wqqMw����������xq��qMS��������xq����wMx�������q������x�����qw�������������qw�����������������xw�����������������ww�����������������xq������������������q������������������q���������������������qw���������������������qw��������	������xq���������	�����q����������	��������qw����������	�������M�������������S�����������
(S�������������
((�������������	���YR.����	��RYYRRRR.((�����	�RRRYY�R������	�..RRY����Y�����������(.RY����Y�����������((.RYY}�}R������������.((..RRYRY�����������h���`Y.RY�������������!������������!������������!����)�   ,   '   �           
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��(p*�?
  
  U
  [ �fill:	
�  ��6
7677	7788  6#67676777777777[888  161616666#76767777777[8  616,61,61666	66767	77[  1,16166161116166	6#6677777777  1611,116,1661661	6776777777777  1361161666666666767777777  111111,11,111*c�c*1	6677777777  	1116*���c� �*6666666766777777  %%%%%1%111)���*%+*냃L166666676777777  1%111%1%%+c�L,11166)���L�L+6666677777  +1+%+1%+1��)	1��b�����o66767777  1+1+1+11�L11%1111)��� �������6676677  +++1+%+%c�*+%+%1*뜡���������L667677  +1+++++*�c1+11+L�������b�������L66767  +++++++LaL%++L�����������������L6667  ++++++%��*+*뛙����������˄������1676  +++++++f�*L�������������b��������L666  *++++++a�c���������������c��������+76  +*+*++)����������� �����bꠄ�������,6  *+*+**�������� ����������c���������66  *****L�����ۄ������������ꠅۆ�����66  ***+*����ۆ�������������c����ۄ���,6  ****+��ۆ�����۠�������������������6,  *****��������ۄ���������c�������ۆ�66  *****�����������d������􅠄��������,1  ** )*L�����ۄ��������c�넆��ۆ��+16   )**)L���������������년������*616  *)) ))�������d���������ۆ���+1161  )))))*c������������������11611,  )) )))L����������ۆ����L1%11166  ))))) 5c�������ۄ�����L1%111111  )))))) )������������*%%1%111,1  5555))))���d������+%1+%%11111  )5)5))))뙙	������*+++111111,6  5))5))) ������L+++1+%+1%1111  ))55))))����L+++++1+1%11111  )))5))))ۅ�)+++++++1%1%11,1  
)))��d����*++++++1+%+11%111  ))5))))d�{L****++++++1%%1111,  ))5)))d�****+*+*+++1++1+%1111  )555))d)******+++++++1%1111,1  ))))))��)****+*+*++++1++1%1111  )5)))))5L)******++++++++1%%1111,  
)K5)) L�)******+*+++1+1+1%1111  ))))5))))5�d****+*+*+++++%+11%11,1  
))'))) d�)*****++++++++1%+1111,  ))5	)oL*****+*+*++++1+%111%111  ))5)55)))) ))))*****+*+++++++1+%111,1  55)))))))*))*******+*+++++1+1%1111,11  	)#))***)******+++++++1++1%11116  	)# )*)******++*+++++++11%1111,1     &   4      
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  ��6
7677	7788  6#67676777777777[888  161616666#76767777777[8  616,61,61666	66767	77[  1,16166161116166	6#6677777777  1611,116,1661661	6776777777777  1361161666666666767777777  111111,11,111*c�c*1	6677777777  	1116*���c� �*6666666766777777  %%%%%1%111)���*%+*냃L166666676777777  1%111%1%%+c�L,11166)���L�L+6666677777  +1+%+1%+1��)	1��b�����o66767777  1+1+1+11�L11%1111)��� �������6676677  +++1+%+%c�*+%+%1*뜡���������L667677  +1+++++*�c1+11+L�������b�������L66767  +++++++LaL%++L�����������������L6667  ++++++%��*+*뛙����������˄������1676  +++++++f�*L�������������b��������L666  *++++++a�c���������������c��������+76  +*+*++)����������� �����bꠄ�������,6  *+*+**�������� ����������c���������66  *****L�����ۄ������������ꠅۆ�����66  ***+*����ۆ�������������c����ۄ���,6  ****+��ۆ�����۠�������������������6,  *****��������ۄ���������c�������ۆ�66  *****�����������d������􅠄��������,1  ** )*L�����ۄ��������c�넆��ۆ��+16   )**)L���������������년������*616  *)) ))�������d���������ۆ���+1161  )))))*c������������������11611,  )) )))L����������ۆ����L1%11166  ))))) 5c�������ۄ�����L1%111111  )))))) )������������*%%1%111,1  5555))))���d������+%1+%%11111  )5)5))))뙙	������*+++111111,6  5))5))) ������L+++1+%+1%1111  ))55))))����L+++++1+1%11111  )))5))))ۅ�)+++++++1%1%11,1  
)))��d����*++++++1+%+11%111  ))5))))d�{L****++++++1%%1111,  ))5)))d�****+*+*+++1++1+%1111  )555))d)******+++++++1%1111,1  ))))))��)****+*+*++++1++1%1111  )5)))))5L)******++++++++1%%1111,  
)K5)) L�)******+*+++1+1+1%1111  ))))5))))5�d****+*+*+++++%+11%11,1  
))'))) d�)*****++++++++1%+1111,  ))5	)oL*****+*+*++++1+%111%111  ))5)55)))) ))))*****+*+++++++1+%111,1  55)))))))*))*******+*+++++1+1%1111,11  	)#))***)******+++++++1++1%11116  	)# )*)******++*+++++++11%1111,1     &   4      
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  
  �   
 �Ufill:	
�  ��!��   !��   !��   	��� !#$!��   	�!!#&#����   ������� !�� ���   ������ �������� 	��   �����	��$$"  �������   �����  �	���&!##$�����   �����!������!"$#" �����   ����#�����!"%"&"#"" ����   ���� "����!#$$$#"#&#""! ���   ����!#��!##"###$$!���   ����!#�##""#$$%��   ���#"#"!!"#%%%$##&��   ���!#%%&%$"" &�   ���!&%%%%$#"!�   ���%$#!�   ��#"!�   ��!�   ���   ��
"�   ��!��   ������   �������   ����!�����   ����!#������   ����!"$$
�������   �����!%(	��   �����$%#�	��   ������#"��	��   ������� !���	��   	����   	�
����   	��
�����   	����   	��
����   	�������   	����   	����   	�����   	����   	����   	����   ������������������   	�������   	�������   	�������   !��      !   1   ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��)0i*�{
  
  U
  [ �brush3:	
�  .l������   ������   �����Ƕ���   �����Ƕ���   �����Ƕ���   ����ǷǶ���   �����ǣ�ww������Ƕ���   �����ǜS(�����Ƕ���   ������w����Ƕ���   �����ǜ(����Ƕ���   ������xMS����Ƕ���   �����Ǣ�S(S�����Ƕ���   �����ǣ���������Ƕ���   �����Ƕ���   ������   ������   ��   ��               
 %�
  D0
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�	
�   �l%%   %%   %%   %%   %%   %[%   %%%%%%!33$!%%%%%%%%   %%%%%%"%%%%%%%%   %%%%%%3���T!%%%%%%%%   %%%%%%����	%C%   %%%%%%/(���Y%%%%%%%%   %%%%%%#2�%%%%%%%%   %%%%%%!"%	%%   %%   %%   %%   %%   %%               
 2�
 2�
 2�
 2�
 2�
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
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
 3�
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
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
 4�
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
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
 5�
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
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
 6�
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
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
 7�
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
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
 8�
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
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
 9�
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
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
 :�
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
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
 ;�
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
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
 <�
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
 =�
 =�
 =�
 =�
 =�
  D0	
�  8l::   ::   ::   :+:::   ::   :::+�:   +:::::3./""%::::::::   ::::::84O83:::::+::   ::+:::"O+::+::::+   :::::+" ::::::::   +:::::"d::::+:::   ::+:::,d�@:+:::::+   ::::::C% 2?2::::::::   :::+:: :	:+::+:   :+::	::+:::::::   	:::+::::+:::::   :::+::+:	::+:::   +::::::::::+	::               
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
 �
  
 I�   
 tbrush3:�   8   |   �   |   8            ���������
  ��%�<( e
  
  U
  [ �eyedropper:	
�  ��q�6616111%1%1+11++++++*+******)*))))) 666,61,111%%+++++++*+********)*) )) 66,616111%111+1+++++++*+****** )))) 666661,1111+11++++++*+*******)****) 66161,11111%1%+1+++++++*+*******) ) 6666661111%1%+1+++++++*+	*� *) 6666161,111%11+1+1+++++++**5�
 )*** 766661611111%%1+++++++*+*+�����** 666666,1,1111+%111++++++*������
%)* 766616161111111+%++++++*�������	+* 6766666161,1%111%1+1+++��������6* 766666661,1111%1+%1++1�������	�6* 767666166111111%11++%)�������	�%+ 67666666161,1111%+ :	�������	+* 7767666661611,111%	�	������
%d+++ 77676666,616111111)��
�����6++++ 77767666666161,11%1��
	��d%++++ 777766666616111111+���	1++%+++ 77777766666666,11*� �5	1+%%++++ 77777676666,6166L� ���%6%%%+1+1+ 777767766666666L�  ���%6%%1+1+%1+ 7777767666661L� ����11%�%%11%11+1 777777676666�  ��߄+11%%11%%1+1%1+ 77777777776�  ����+61111111111%111 77777676�  ���d16161,11111111%1 877777777�  ���1,,661611,11111111 [7776�  ���166666616161,111111 887777�  ���d6	6k,616161,,1, 88[7�  ����,67666666666616161616 88�  ���d,-76766666661666,61,61 888d���߆+77776766616 8887 ���d7	77676	6;666 888�����777777776776676666666666 8O8��Ä6.77777	7G67676666666 888�d87777777776776767666666 88888888[7777	76767676 848O88888888[777C767 88888O8888[.77777777777776777 4848888O88888877	7767 8488488888888888[8
777+777 484488O8O8O888888[7777777    #   )      
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
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
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
   �
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
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
  !�
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
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
  "�
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
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
  #�
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
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
  $�
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
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
  %�
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
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
  &�
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
  '�	
�  ��q�6616111%1%1+11++++++*+******)*))))) 666,61,111%%+++++++*+********)*) )) 66,616111%111+1+++++++*+****** )))) 666661,1111+11++++++*+*******)****) 66161,11111%1%+1+++++++*+*******) ) 6666661111%1%+1+++++++*+	*� *) 6666161,111%11+1+1+++++++**5�
 )*** 766661611111%%1+++++++*+*+�����** 666666,1,1111+%111++++++*������
%)* 766616161111111+%++++++*�������	+* 6766666161,1%111%1+1+++��������6* 766666661,1111%1+%1++1�������	�6* 767666166111111%11++%)�������	�%+ 67666666161,1111%+ :	�������	+* 7767666661611,111%	�	������
%d+++ 77676666,616111111)��
�����6++++ 77767666666161,11%1��
	��d%++++ 777766666616111111+���	1++%+++ 77777766666666,11*� �5	1+%%++++ 77777676666,6166L� ���%6%%%+1+1+ 777767766666666L�  ���%6%%1+1+%1+ 7777767666661L� ����11%�%%11%11+1 777777676666�  ��߄+11%%11%%1+1%1+ 77777777776�  ����+61111111111%111 77777676�  ���d16161,11111111%1 877777777�  ���1,,661611,11111111 [7776�  ���166666616161,111111 887777�  ���d6	6k,616161,,1, 88[7�  ����,67666666666616161616 88�  ���d,-76766666661666,61,61 888d���߆+77776766616 8887 ���d7	77676	6;666 888�����777777776776676666666666 8O8��Ä6.77777	7G67676666666 888�d87777777776776767666666 88888888[7777	76767676 848O88888888[777C767 88888O8888[.77777777777776777 4848888O88888877	7767 8488488888888888[8
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
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
  {�
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
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
  |�
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
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
  }�
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
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
  ~�
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
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �&
  �1
  �<
  �G
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  !
  eyedropper:action:cursor:   
 �eyedropper:	
�  ��h%���  %���  ����ƿ�����  �����鿛���  �������w��  �����������p�  �����������pp�  ���������꿛L(�  ��������꿛p((�  �����������꿛p((��  ���������鿛p((���  ���������wL((����  �����꿿��p(((�����  ���꿛�p(((������  � "���L(((�������  ���� &'$p�L((��������  ��� &%څSS((���������  ��!'%ڄ���S(���������  �!''%������  ����!'%�S�����  ���'%�������  ��!'$����  �"'$�����  	����"'گ�����  	���"%څ������  	��!%څ���  	� '%������  �������%������  �������"%����  �������$�����  �������������  ���������������  �����������  �����������  �����������  %���     &   $   ���������   
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  B{
  �R
  �]
  �h
  �s
  �~
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �#
  �.
  �9
  �D
  �O
  �Z
  �e
  �p
  �{
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �

  �
  � 
  �+
  �6
  �A
  �L
  �W
  �b
  �m
  �x
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �(
  �3
  �>
  �I
  �T
  �_
  �j
  �u
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �%
  �0
  �;
  �F
  �Q
  �\
  �g
  �r
  �}
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �"
  �-
  �8
  �C
  �N
  �Y
  �d
  �o
  �z
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �	
  �
  �
  �*
  �5
  �@
  �K
  �V
  �a
  �l
  �w
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �'
  �2
  �=
  �H
  �S
  �^
  �i
  �t
  �
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �$
  �/
  �:
  �E
  �P
  �[
  �f
  �q
  �|
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  � 
  �
  �
  �!
  �,
  �7
  �B
  �M
  �X
  �c
  �n
  �y
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  ��
  �
  �
  �
  �)
  �4
  �?
  �J
  �U
  �`
  �k
  �v
  ��
  ��
  ��
  B{
  ��
  ��
  ��
  ��
  ��
  ��'�>*�a
  
  U
  [ �erase:	
�  ~�} ����   %    ������� %   �������� %  ��������� % ��������� !    ��������~���     ������������  ����������~���� ��������������������       �������������������      ����������������������      �����	����������     ������	��������� ��������	��������     ����������������������     &����&�����������������     &&��������������������  &&&������&�����������      &&&&������&���������      &&&����񿿿������    &&���&񿿿���     &�����&&ƿ�����   ����������&    ��������ͩ       ������������!    ����������  % �������   %  ����� �    /   #   �                   "    
 %�
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 @G
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 0�
 =�
  !
  
  �   
 =merase:	
�  ~�} ����   %    ������� %   �������� %  ��������� % ��������� !    ��������~���     ������������  ����������~���� ��������������������       �������������������      ����������������������      �����	����������     ������	��������� ��������	��������     ����������������������     &����&�����������������     &&��������������������  &&&������&�����������      &&&&������&���������      &&&����񿿿������    &&���&񿿿���     &�����&&ƿ�����   ����������&    ��������ͩ       ������������!    ����������  % �������   %  ����� �    /   #   ���������   
 @G
 @W"?���
 %�
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
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
 &�
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
 '�
 @G
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
 '�
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
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
 (�
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
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
 )�
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
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
 *�
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
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
 +�
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
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
 ,�
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
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
 -�
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
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
 .�
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
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
 /�
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
 0�
 0�
 0�
 0�
 0�
 0�
 0�"?���
  �u%��(`�
  
  U"  � �ColorPickerClosedc
  showColorPalette:�  ��IV   O?
O?O?O9O9
O9NyNyNy2yNy2y2y�� O?O?O?O?O9O9O?
O9O9NyO9O9NyNyNy2yNy2y2y2y� O?O?O?O9O?O?O9O9O9NyO9NyNyNy2y2y2y2y� O?
O?O?
O9O9NyO9NyNyO9Ny
NyO9NyNy2y2yNy2y2y2y2y� O?O?O?O9O9O?O9O9O9NyO9NyNyNy2yNyNy2y2y2y� O?O?O?O?O9O9O?:  
2y2y2y� O?
O?O?O?O9 �2��� 
2y2y2y� O?O?O?O?O9O9O9 �2��� 
2y2y2y� O?O?O?O9O?O?O9 �2��� 
2y2y2y� O?O?O?O?O9O9O? �2��� 
2y2y2y� O?
O?O?O?O9 �2��� 
2y2yy%) O?O?O?O?O9O9O9 �2��� 
2y2y2y� O?O?O?O9O?O?O9 �2��� 
2y2y2y� O?O?O?O?O9O9O? �2��� 
2y2y2y� O?
O?O?O?O9 �2��� 
2y2y2y� O?O?O?O?O9O9O9:  
2y2y2y� O?O?O?O9O?O?O9 �2��� 
2y2y2y� O?O?O?O?O9O9O? �2��� 
2y2y2y� O?O?O?O9O?O?O9 �������� 
2y2y2y%) O?O?O?O?O9O9O? �������� 
2y2y2y� O?O?O?O9O?O9O9 ���������� 
2y2y2y� O?O?O?
O?O9 ���
������� 
2y2y2y� O?O9O9O?O?O9O? �����
������� 
2y2y2y� O?O?O?O?O9O9O9 ���
��������� 
2y2y2y� O?O?O?O9O?O?O9 	~
~~}}}���� 
2y2y2y� O?O?O?O?O9O9O? _
___^^^^]]]]���� 
2y2y2y� O?O?O?O9O?O?O9 _
__
^^]]]\\���� 
2y2y2y� O?
O?O?O9O9 ?
??>>>====<<{�{�{�{� 
2y2y2y� O?O?O?
O9O9 ?
??>>>===<<<{�{�{�{� 
2y2y2y� O?
O?O?O?O9 
{�{�{�{� 
2y2y72y� O?O?O?O?O9O9O9 {�{�{�{� 
2y2y2y� O?O?O?
O9O?# ~�~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y2y� O?O?O?
O?O9# ~�~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y72y� O?O?O?O?O9O9O? ~�~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y2y� O?O?O?
O?O9# ~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y72y%) O?O?O?O9O?O9O9 ~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y2y� O?O?O?
O?O9# ~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y2y� O?O?O?
O9O?# ~�~�~�~�~�~�~�~�~�~�~�~�~�~�o{o{o{o{ 
2y2y2y� O?O?O?
O?O9# ~~~~~~~~}~}~|~{~{~z~z~y~xo{o{o{o{ 
2y2y72y� O?O?O?O9O?O9O9 ~~~~~~~~}~|~|~{~{~z~y~y~xkZkZkZkZ 
2y2y2y� O?O?O?
O?O9# ~_~_~_~^~^~]~\~\~[~Z~Z~Y~X~XkZkZkZkZ 
2y2y72y� O?O?O?O?O9O9O? ~?~?~?~>~>~=~<~<~;~:~9~9~8~7g9g9g9g9 
2y2y2y� O?
O?O?'O?O9 ~?~?~?~>~>~=~<~;~;~:~9~8~8~7g9g9g9g9 
2y2y72y� O?O9O9O?O9O9O9 z~~~~~~~~~~~~~cccc 
2y2y2y� O?O?O?
O?O9# z~~~~~~~~~~~~~cccc 
2y2y2y� O?O?O?
O?O9# y�}�}�}�}�}�}�}�}�}�}�}�}�}�^�^�^�^� 
2y2y72y� O?O9O9O?O?O9O9 y�}�}�}�}�}�}�}�}�}�}�}�}�}�^�^�^�^� 
2y2y2y� O?O?O?
O?O9# y�}�}�}�}�}�}�}�}�}�}�}�}�}�Z�Z�Z�Z� 
2y2y72y� O?O?O?O9O?O9O9 y�}�}�}�}�}�}�}�}�}�}�}�}�}�V�V�V�V� 
2y2y2y� O?O?O?
O?O9# y�}�}�}�}�}�}�}�}�}�}�}�}�}�V�V�V�V� 
2y2y72y� O?O9O9O?O?O9O? y�}�}�}�}�}�}�}�}�}�}�}�}�}�R�R�R�R� 
2y2y2y� O?O?O?
O?O9# y�}�}�}�}�}�}�}�}�}�}�}�}�}�NsNsNsNs 
2y2y72y� O?O?O?O?O9O9O9 y}}}~}}}|}{}z}y}x}w}v}u}tNsNsNsNs 
2y2y2y� O?
O?O?'O?O9 y}}}~}}}|}{}z}y}x}v}u}t}sJRJRJRJR 
2y2y72y� O?O9O9O?O9O9O? y_}_}_}^}]}\}[}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2y� O?O?O?
O?O9# y_}_}_}^}]}[}Z}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2y� O?O?O?
O?O9# y?}?}?}>}=};}:}9}8}7}6}5}4}2BBBB 
2y2y72y� O?O?O?O?O9O9O? y}}}}}}}}}}}}}=�=�=�=� 
2y2y72y� O?O?O?O9O?O?O9 y}}}}}}}}}}}}}9�9�9�9� 
2y2y2y� O?
O?O?'O9O9 x�|�|�|�|�|�|�|�|�|�|�|�|�|�5�5�5�5� 
2y2y72y� O?O?O?O9O9O9O? x�|�|�|�|�|�|�|�|�|�|�|�|�|�1�1�1�1� 
2y2y2y� O?
O?O?'O9O9 x�|�|�|�|�|�|�|�|�|�|�|�|�|�1�1�1�1� 
2y2y2y� O?O?O?
O9O?# x�|�|�|�|�|�|�|�|�|�|�|�|�|�-k-k-k-k 
2y2y72y%) O?O9O9O?O?O9O9 x�|�|�|�|�|�|�|�|�|�|�|�|�|�)J)J)J)J 
2y2y2y� O?O?O?
O?O9# x�|�|�|�|�|�|�|�|�|�|�|�|�|�%)%)%)%) 
2y2y72y� O?O?O?O9O?O9O9 x�|�|�|�|�|�|�|�|�|�|�|�|�|�!!!! 
2y2y2y� O?
O?O?'O9O? x|||}|||z|y|x|v|u|s|r|q|o���� 
2y2y72y� O?O?O?O?O9O9O9 x|||}|||z|y|w|v|u|s|r|p|o���� 
2y2y72y� O?O?O?O9O?O?O9 x_|_|_|]|\|Z|Y|W|V|T|S|Q|P|N���� 
2y2y72y� O?O?O?O?O9O9O? x_|_|^|]|\|Z|Y|W|V|T|S|Q|P|N���� 
2y2y72y� O?O?O?O9O?O?O9 t?|?|>|=|;|:|8|7|5|4|2|1|/|.cccc 
2y2y2y� O?
O?O?'O9O9 t?|?|>|=|;|:|8|7|5|4|2|1|/|-BBBB 
2y2y72y� O?O?O?O9O9O?O9 t|||||||||||||!!!! 
2y2y72y� O?O?O?O?O9O9O? t|||||||||||||   
2y2y2y� O?
O?O?'O?O9 tx||||||||||||   
2y2y72y� O?O9O9O?O9O9O9 pxxxxxxxxxxxxx   
2y2y2y� O?O?O?
O?O9# ptxxxxxxxxxxxx   
2y2y2y� O?O?O?
O?O9# lttttttttttttt   
2y2y2y� O?
O?O?'O9O? lptttttttttttt   
2y2y2y� O?O?O?
O?O9# hppppppppppppp   
2y2y72y� O?O?O?O9O?O9O9 hlpppppppppppp   
2y2y2y� O?O?O?
O?O9# dlllllllllllll   
2y2y2y� O?
O?O?'O9O? dhhhhhhhhhhhhh   
2y2y2y� O?O9O9
O?O9# `dhhhhhhhhhhhh
   
2y2y72y� O?O?O?O?O9O9O9 `ddddddddddddd
   
2y2y2y� O?
O?O?'O9O9 \`dddddddddddd
   
2y2y2y� O?O9O9
O?O9# \`````````````
   
2y2y2y� O?
O?O?'O9O9 X\````````````
   
2y2y72y� O?O?O?O9O9O?O9 X\\\\\\\\\\\\
\	   
2y2y72y� O?O?O?O?O9O9O? TX\\\\\\\\\\\
\	   
2y2y2y� O?
O?O?'O?O9 PXXXXXXXXXXXX
X	   
2y2y72y%) O?O9O9O?O9O9O9 PTTTTTTTTTTTT
T	   
2y2y2y� O?O?O?
O?O9# LPTTTTTTTTTTT	T   
2y2y2y� O?O?O?
O?O9# LPPPPPPPPPPP
P	P   
2y2y2y� O?O?O?
O?O9O9O9
O9NyNyNyNy2y2y2y2y� O?O?O?
O9O?
O9O9
NyO9NyNyO9NyNyNy2y2y2y� O?O?O?
O?O9
O9O9O9NyNyNy2y2y2y� O?O?O?O9O?O9O9
NyO9NyNyO9NyNyNy2y2y2y� O?O?O?O9O9O?O9O9NyO9O9
NyO9NyNy
2yNy2y2y1��V     *   e   �(`�*��
  
  U"  �   �   �clear:	
�  f�	߇�������*�+�+�6����������������� �����������lH??L )*%������������ ��������?f?�H?H)))L))���������� ������EE�???))	)�K***�������� ����H�E�?H25555555)))))***+������ ���H�EEH%n*54855552555 5))))*+*%����� ��EE�+w4wD2D922:52 252555 )**+++���� ��H�??4D2+92D955+%522%, 5+*+)**++1��� �??2e&5522�921ww45ww2/Dw)**+++�� �L55"5252D&2D4,w 4"7�2Dn%5 *+++� �5552D42 22D&w"Ow%2*4�2/25 *+++�� �)))552/22 292wDD& /�D�2�/5 **+++4 �)*)* 5 4D2+D D/2w%275�8 �5/5 *+111� ��*****)w1n2DD2D&w2�4"�5D/**+11+� ��*+++***�w651�4+�w5"�Dw5/*+1d1e�� ���+++++**+   55 55* )) 5***+%d116��� ����d+11+++**** *    ***+++1d161[e���� �����d6+d1+1+++++**+*++++1d111d����� �������6111
111+�d11d6116��������� ��������͊d61d+1d11166[e�e������� ���������������������������������� ����������������������������������    '         "?���"C�"�5�"7��z"�5�"�!"�!"7�V�"3�5�"?�5�"?���"'��")Q�`"/a @"�  "+`  "1�  "#� "'� "7��"-f5H";�("1�� "/f�H"/f5("' "��"%5H"9�"+f�H"9��Q"5��"1���"+g5i"Ԡ�"'��"?��"+g��"���"9�("!� "1�E�"/h��"-hE�")W5i"'�H"%5("� �"?�Ei")W��"5���"-g�i"�!"3���"'5i"%�H"#5("���"5�E�"?�U�"/d� "?���"Ԡ�";��Q"?��")T� "7�� "1��H"
B`"?�j�";�D "?�� "7�j�"'�("/g4 "7�D "-jV1"?���";�D "ե("?�j1"-iEi"+h�H"5�ٍ"1��H"�  "9�ٍ"-iE("9�i�"3��i"���"�5H";�zq"7�j"?�zq"?�zQ"9���"7��Z"?���"1�j�"׵�"C��"5�j�"�!"?��:"7��"���"5�j�"3��q"7��"B`")ZU�"-kY�"5�j1"'ō"?��q"1�i�"?��Q"+jٍ"  "1�i�"9���";�{"-k�1"7���"-nz"+ny�"%�"%��"�Y("�U"9���"#F1"'Vq"!�"7��z"
C�"׵�"�5i"%�Q"�E�"#Z1")]j�"�ŭ"!�"�U�"�E�"�Y�"I��"!Z1"'j�"�Z"��1"5�{z"1�k:"1��Z"#j�"��q"���"-k��"+kZ�"-l�:"�V�"�V�"%�z"�ڶ"�k�"
IF�"�ƶ"��"[z"���"�k�"6Q"��"k�" ��" 
Wz"���"
Lk�" ��" [�"���"��"���")\��"��"�F�"'۞"%[z"!W:"��"�ƕ"�F�"
C"%W:"��"�6�"D""��"��1" �" ")ZV�"�!("�!�"Ci"ַ:"�#�"C�"�#�"�7"է"
BH"A�("�#:"�6"�#Z"�z"�61"%�"ғ�"��")YG"!#z"���"'��"!6q"��"-j��"
B�"%#:"%��"'��"3�k:"-j��"%6Q"-h��"3��"5��:"+hF�"1���"9��Z"5�ƕ"1���"?��z"?�kZ"7��1"5�ƕ"9�1"?��"?��1"?�Z�"?�ƕ"9�5�"    
 d�
  !
  clear:with:   
 d�clear:
  ��%�(�
  
  U
  D0 �stampTab�  D�<C^�oUsuw���svgV�RoV� -�-�-�1�1�1�1�-�-�-�1�266-6-6-6-:N:M6M6-6-�6- k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� -�-�-�1�21�BVJ=�-�-�26-6-:M:N:N:N6-JMZkI�1�1�26-:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� -�-�1�1�21�vi~�]�(�)�-�2:N
:N:NcF,zH~�j	4�)�-�6,:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� -�-�1�1�2-�5A%5a)�2:M
:N:N�:0�=$=$ ��)�2:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� -�5�1�-�-�)�5�$�@ %�2:->M:-6-6,1�5�0�@ �%i1�:M k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 9�q�r(jZ	-gb1��)�2N+vr(f)V	5�f	A$��%�2:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 1�@�X�i�na ����!h-�6-9�D`a#m�Q�`$�@��!h-�6-:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� -�)�(�4@8` a���&%�26-6-1�,�8@0`a���)�2:N:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 1�)�%�!'$b, (@`�!)�26M:N6-�%(A,@,``� �)�2:N:n k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 21�-�)�!G�(b, $@a �-�6-:N:N6-�%h �(b, (@`�)�6-:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 2221�-�%�!G$�(�$�)h2:N>�>o:N2)�%i!G$�(�$�%1�:N k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 66-6-6-22-�)�%i!h%h)�2:nB�B�B�>�:N6-2-�)�%�)�2:n k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
""!!""""7" ^�k4oTsuw���svgV�w�  kWk4k4oUw�{��{�svoUk4k4
oToTcZ�Z�Z�^�^�^�Z�Z�Z�Z�Z�^�b�ggk4oToToUoUsUsususvsvw�w�{��{�w�k4Z�Row�  {�k4k4oTsv{���{�svsuoU
oUoUk4k4k4k3k3k4k4k4oToT
oUoU;sususvwvw�w�{�{��{�sub�V�Ro{�    s�gk4oUw�{��{�{�w�w�.susu?susvsvw�w�w�{�{�{��{�svk4^�V�s�      {�ggk4suw�{���{�{�.w�w�w�{�{�{�w�kTb�V�Row�	 w�ggk4oUw�{�{�{��2�����{�{�{�w�svk4b�Z�V�s�  	   s�b�gk4oTsuwvw�{�.��{�{�w�svoUk3b�Z�V�s� w�b�b�ggk4oToU2w�w�svsvsuoUk4g3b�^�Z�s�s�     {�s�^�^�b�b�g2k4k4k4ggcb�^�Z�s�s�{�   �{�w�s�sz6sysysyszs�w�{�      3      
 s�
 s�
 �
  toggleStamps    
  ��(�+
  
  U
  D0 �shapeTab�  ����^�oUsuw���svgV�RoV� :x:x:x>�>�B�B�B�B�>w65B�B� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� >�>�>�B�B�B�B�B�B�-��>xB� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� >�>�>�B�B�B�B�>�1�c�:V>w k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
>�>�G>�>�:V-�c!m1�65:V k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
>�>��>x>x-���-�1�64:V k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� >�>�>�>x>w: �1�1�1�:5>W k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� >x>x>x>v1� !L-�-�1�6:V>w k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� >x:x:W1���-�-�-�6:5>wB� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� :x:wFt �-�-�-�666:wB�F� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 25!m !L-�-�-�265>w>�F�F� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� .
�-�-�-�264:V>�B�F�F� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� :W6V6522265:VB�B�F�F�J� k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� :W6V656665:V>wB�F�F�F�   k4k4oTsuw���svgV�w�  ^�oUsuw���svgV�RoV� 
""!!7! ^�k4oTsuw���svgV�w�  kWk4k4oUw�{��{�svoUk4k4
oToTGZ�Z�Z�^�^�^�Z�Z�Z�svsvw�w�{��{�w�k4Z�Row�  {�k4k4oTsv{���{�svsuoU
oUoUk4k47k4wvw�w�{�{��{�sub�V�Ro{�    s�gk4oUw�{��{�{�w�w�susu7suw�{�{�{��{�svk4^�V�s�      {�ggk4suw�{���{�{�w�w�w�{�
{�{�w�kTb�V�Row�	 w�ggk4oUw�{�{�{�����{�{�{�w�svk4b�Z�V�s�  	   s�b�gk4oTsuwvw�{����{�w�svoUk3b�Z�V�s� w�b�b�ggk4oToUw�w�w�oUk4g3b�^�Z�s�s�     {�s�^�^�b�b�gk4k4k4cb�^�Z�s�s�{�   �{�w�s�szsysysyszs�w�{�      %      
 {�
 {�
 �
  toggleShapes    
  �"?� �  �i�  4�   {�w�oVk5k4�k4k4k4g4g4oWw��   {�oUoUoTk4�k4k4k4gggb�b�oW� w�svsuoU�oUoUoTk4k4ggb�b�^�{�  	   w�w�sv
sususvwv�w�w�svsusuoUk4gb�^�^�w�	 w�w�w�svsusvw�w�{�{����{�{�+w�w�suk4g^�Z�Z��    �w�w�svsvw�w�{�{�
���������
��3{�{�w�suk4b�Z�Z�c    sww�svsvw�w�{���{�{�{�w��w�w�?w�{�{�{���{�w�sug^�Z�V�  k5svsvsusvw�{���{�w�suoUk4k4k3�ggGgk3k4k4oToUsvw�{���{�w�oTb�Z�V�{�b�svsusuw�{���{�svoTgb�^�^�Z��Z�Z�GZ�Z�^�^�b�gk4oUw�{���{�sugZ�V�w�b�suoUsvw�{��{�svk4b�^�Z�V�V�V��R�R�?V�V�V�Z�^�b�gk4oUw�{��{�w�k4^�V�w�b�oUoUsv{���w�oTb�^�Z��V�V�?V�Z�Z�^�^�Z�b�gk4suw�{��w�oT^�V�w�b�oUsuw�{��{�sug^�Z� �  3 gk4oUw�{��{�oUb�V�w�^�oUsuw�{��{�oUb�Z�V� 
E�E�E�E�E�E�I�I�I�I�I�I�
M�M�
M�M�2M�M�M�M�I�I�I�I�I�I�E�I�
E�E�3 gk4oUsv{��{�sub�V�w�^�oUsuw�{��w�k4^�V�V� 
E�E�E�E�E�I�I�I�I�M�
M�M�M�M�:M�M�M�I�I�I�"I�I�I�I�
E�E�7 k4k4oTsu{��{�sub�V�w�^�oUsuw�{��w�k4Z�V�V� E�E�
E�E�I�I�I�I�
M�M�M�M�:M�M�M�M�M�I�I�I�E�E�E�E�E�E�E�I�I�I�KI�I�I�E�E�E� k4k4oTsuw��{�sucV�w�^�oUsuw�{��w�gZ�RoV� E�E�E�E�E�I�I�I�M�M�M�M�M�M�M�M�
M�Q�Q�M�Q�Q�Q�M�M�M�#M�I�I�I�E�E�A�A�A�A�A�E�E�E�E�I�I�I�;I�I�E�E� k4k4oTsuw��{�svgV�w�^�oUsuw���w�gZ�RoV� 
E�E�I�I�I�I�I�M�
M�M�M�M�"Q�Q�'Q�M�M�M�I�I�NZ�^�cV�E�=�=i=i=iA�A�
E�E�E�I�I�I�?I�I� k4k4oTsuw��{�svgV�w�^�oUsuw���svgV�RoV� E�E�E�I�I�I�I�I�M�M�M�M�M�M�Q�Q�Q�Q�
Q�Q�Q�Q�
Q�Q�{Q�M�M�M�M�Z�Z�NrNrV�g8s�^�A�9H9H9i=i=iAiA�A�E�E�E�E�E�I�I�I�I�I� k4k4oTsuw��{�svgV�w�^�oUsuw���svgV�RoV� E�I�I�I�I�I�I�I�M�M�M�M�M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�Q�Q�Q�Q�Q�Q�M�M�RZ�JrBA�=�=�=�J0o{k[E�5H5(5H5H9H9H=i=iAiA�E�E�I�I�I�I�I� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�I�I�I�I�M�M�M�M�M�Q�Q�Q�Q�Q�Q�
Q�Q�Q�Q�Q�Q�Q�Q�Q�M�Q�V�FQE�=i=i9i9I5H5H=�kYo|A�>>B9�1(5'5H9H=iAiA�E�E�E�I�I� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�I�M�M�M�M�M�M�
Q�Q�M�M�M�M�I�M�
M�MˇQ�Q�Q�Q�Q�Q�Q�Q�Q�M�Z�N�JA�=i9i9i9H9H5H5HFQs�gZJ�[Y_YW7Jr9�-1'5H9H=iA�E�E�E�I� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�I�I�M�M�M�M�M�
Q�QˣQ�M�M�M�I�E�5K$�g�(�5*A�E�9kI�Q�Q�Q�M�Q�Q�Q�Q�V/V�N0A�=i=i9i9i9H5H=�1�%JB0{�Z�[8WR�[8_YF/(�-1'9H=iA�E�E�I� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�M�M�M�M�M�M˯M�Q�Q�Q�Q�M�M�M�I�I�9k�IjiH%$9kM�M�Q�Q�M�Q�Q�Q�M�Z�R�E�A�=i9i9I9H=�BBqN�R�J�gYo{BQW7cz_Y[8W=�$�(�1'5H=iAiE�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�M�M�M�M�M�M�M�Q�
Q�Q˧Q�M�M�I�E�(��-1P-/$�i& =�E�I�M�U�U�M�Q�Q�Q�R_NrA�=i9i9H9IBJ�Wczc�g�cz[{�JR9�[7[8W7WR�9� �(�1'5H=iAiE� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�M�M�M�M�M�M˳Q�Q�Q�Q�Q�Q�M�M�I�E�1+�=�JA�1/ �H% )	=iA�I�M�U�U�Q�Q�Q�Q�VP_J=i9i5H=�JrS[Y_y_y_Y[X[8[Xs�c%J5�WWR�N�J�1j �(�1'9H=iE� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
I�I�I�I�M�M�M�M�
M�M�M�Q�
Q�Q˫Q�M�M�I�E�=� �A�JE�1P �hF%#�5I9HA�I�M�U�U�Q�Q�Q�Q�Z�_7A�9i9�FPS[X[8W7[8[X[8_ycz_Ykzoz5�%J9�R�N�J�F�>1 � �)1'9HAi k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
I�I�I�M�M�M�M�M�Q�Q˻M�M�I�A�)C�1O9r(��gggfD-)-9HA�I�M�U�U�Q�Q�Q�Q�^�_=�FR�[8[8SS[8[X_Yczc�cz_yczs�B%J%I>0J�FrBr>0-��$�-5H=i k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�I�M�M�M�M�
M�M��M�Q�Q�Q�Q�Q�Q�M�M�I�=��"#"Cghg� � ���$�1'9IA�I�M�VU�U�U�Q�M�g[N�[8W7N�N�W_Yczg�g�g�g�_y[8[8s�Ns-�!(%JBQBQ>061���(�1'9H k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�M�M�M�M�M��Q�Q�Q�Q�Q�M�M�M�I�E�c"CCB""e���� � �(�5(=iE�M�Q�VVU�U�Q�M�Fr>QBrF�J�S[Xc�g�k�k�g�cz_Y[8V�R�s�R�:!(-�:5�1�-�)I� �-5H k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�M�M�M�
M�M��M�Q�Q�Q�Q�Q�Q�M�M�I�E��"CCBB!!#���� �(�1'9iA�I�M�Q�ZZU�U�Q�B/-�%I!(!I1�N�k�k�g�g�g�cz[8WR�N�J�o�R�:!I�1�1�-�%j%I��(�1' k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�M�M�M�M�M�
M�M�M�Q�
Q�Q˻M�M�I�I�=h"#CBB"!!�b�� �(�1'9HA�I�M�Q�U�ZZU�U�N/%j���(Brc�g�cz_Y[7R�N�N�J�F�kzRr9�)k��%I)�%j!I!)b�$�1' k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� I�I�M�M�
M�M�M�Q�
Q�Q˿Q�M�M�I�I�ji~�^ �"B"! �c��$�-1'9IA�I�M�Q�U�VZZVU�B��������>0cz_8WR�N�J�F�BQBQgYJQ5�)k���!I!Ib�$�1 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�
M�M�M�Q�
Q�Q�7Q�M�I�I�ni~�~�~�fH$�!!!��� �(�-5H=iE�I�M�Q�U�
ZZVQ�-����o��:0R�N�J�F�Br>Q:>0gYB-�%J������b�$�1 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�
M�M�CM�Q�Q�Q�Q�M�M�I�E�bl~�~�z�nh^I��!���$�-5'9iA�E�M�Q�Q�VZ
ZZVU�-����k��:J�FrBQ>06BQN�F19�)�!(������!b�$�1 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�
Q�Q�7M�M�I�E�^L~�~�~�n�^M�=D$� �� �(�19H=iE�I�M�Q�U�VZZZU�9��
��o�����>0>Q:5�1�Z�F09�9�%J�������!c�(�1' k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�
M�M�?M�Q�Q�Q�Q�M�M�I�E�Z
~�~�~�r�^'M�Ae,�1G� �(�1'9HA�I�M�Q�U�VZZZ-ZZU�F�
��o��!�  :05�1�1�Z�B=�=�!(������!)�� �-5( k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�
M�M��M�Q�Q�M�M�M�I�E�^L~�~�~�r�b'M�A�11% � �(�1'=iE�I�M�Q�VZZZZ^.^.Z.Z^.^-ZVN%j)k-�)�)j%J!)�!   !-�1�)�%j>F0=�=�������!)���(�1'9H k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M��Q�Q�Q�M�M�I�E�V
~�~�~�v�b'Q�E�5$,�-( �(�1'=iE�I�Q�U�VZZZZ.^.^.^.^.^-^.^.ZVQ�5�1�6>0BQBrBRc    �-�%j!I!)-�1�%)���%I���$�-5H=i k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�?M�Q�Q�Q�M�M�I�E�V,z�~�~�v�fHQ�E�5$-5H�(�1'=iE�M�Q�U�ZZZ^.^.Z.ZU�NP:>1BrF�R�:
  _ !)�!I(�����%I���$�-5(=hA� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�7M�M�I�M�z�~�~�z�jhZI�9D,�1F�$�1'=iE�M�Q�U�ZZ^-^.^.^.^.ZVR.BQF�J�R�[!)  [!I������!(%Ic��$�-5H=iA�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�3M�I�I�jH~�~�z�ni^M�=D-5G �$�-9HA�I�Q�U�ZZ^-^.^.^.Z.ZQ�NQN�W_X_Y�  [�!I����%J%c� �(�15H=iA�E�I� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�3M�M�I�I�jk~�
~�r�b'Q�Ae11$ � �-5HA�I�Q�U�ZZ"^.^.Z-VRR�czg�czB  [B%j���)J�c� �(�1'9H=iE�I�I�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�3M�I�E�bJ~�~�~�r�bHU�E�5#-) �)5H=iI�M�U�ZZ^-^.^.#^.b.^.^.b.^.^.^.^.ZU�R/_Yg�[8!  [!)k��!()j���$�-5'9HAiE�I�I�M�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�3M�I�I�I�bJ~�~�~�r�bHQ�E�5$1%%�(�1'=iE�M�Q�VZ^.^.^.b.
b.bNbNbNbN^.^.Z.ZQ�Rq[XW!  # )k!))Jc��$�-5H=iA�E�I�
M�M�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�3M�I�E�bJ~�~�~�v�f(Q�E�9D,�-'�$�1'9iE�M�Q�VZZ^.^.^.b.b.bNbNbNbNb.^.^.ZU�M�NrN�!   !-�!)%J)Jc� �(�1'9H=iE�I�
M�M�7Q�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�3I�E�V,z�~�~�v�jHVI�9D,�5h�$�-9HA�I�Q�VZZ-^.^.^.
bNbNbNbObObO
bNbOb.^.^.ZQ�M�JQ!  #c1�)j ��� �)1'9HA�E�I�M�M�Q�
Q�Q�g k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�I�A�=eY�z�z�jiY�I�=e-1F�$�-9HA�I�Q�U�ZZ^.^.b.b.bNbNbNbObObObNbN^.^.VQ�E� 
  � �-�1����$�-5H=iA�I�I�M�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�E�=i-&�9D^^M�=e-1% � �-5HAiI�M�U�VZ^-^.^.b.bNbNbObObObObNb.^.ZU�I�c
  �%J>Q5��� �(�1'5H=iE�I�M�M�Q�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�E�=h9i ��(�Ae=e-1%$� �(�5(=iE�M�Q�VZZ-^.
^.^.^.b.bNbNbNbObObO/bO^.^.ZQ��   %(5�%(��$�-5(9iA�E�I�M�Q�Q�kQ�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�A�9H1'1I ��$�--)�(�1'=iE�M�Q�U�ZZ^.^.b.b.bNbN
bObObOfOfOfOfObO
bObO�bObN^.Z-Q�c  �$��� �(�1'5H=iE�I�M�Q�Q�Q�Q�U�U�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�A�=h1'(�$�$��(�$��(�1'9hE�I�Q�U�ZZZ^.^.b.bNbNbO
bObOfOfOfObO
bObO�^.^.V�  -((�$�(�-1(9IA�E�I�M�Q�Q�U�U�U�U�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�E�=i5H-$� ��� �(�1'9HA�I�M�U�VZZ^.^.^.b.bNbNbObObObOfOfOfofofo
fOfO
bObO'b.^.V-( �5H--1'5H=iA�I�M�Q�Q�U�
U�U�sU�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�I�I�A�9H5'-(�$�(�-1'9HA�I�M�Q�VZZZ-^-^.^.bNbN
bObOfOfOfOfo
fofo/fofOfObObObObN^.ZE� 5j9H5(1'5H=iE�I�M�Q�U�U�U�oQ�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�I�E�A�9H5H1'1'1'5(=iA�I�M�Q�U�VZZZ^.
^.^.^.b.bNbN
bObOfOfOfofo7fofOfObObObOb.^.ZR!-(=i9H9H=iA�I�M�Q�U�U�VVVU�
U�U�?Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�+M�I�E�A�=i=i=h=iAiE�I�M�Q�U�VZZZZ-^.^.^.bNbN
bObO;fOfOfofofofpfofofofOfObObObObN^.Z-( �A�=i=iA�E�M�Q�U�
VVKVU�U�U�U�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�I�I�
E�E�I�I�M�Q�U�U�
ZZZ-^-^.^.b.bN
bObOfOfOfofo7fofOfObObObObN^.Z � �A�A�A�E�I�M�U�U�VZZVU�U�
U�U�GQ�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�
M�M�M�I�I�M�M�Q�Q�U�U�Z
ZZZ^.^.^.bNbN
bObOfOfOfOfo
fofo�fOfOfObObObObN^.Z.E�!=�E�E�E�I�M�Q�U�VZZVVU�U�U�U�U�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�
Q�Q�M�M�M�Q�Q�Q�U�U�VZ
ZZZ-^.^.^.bNbN
bObObOfOfOfo
fofofOfO
bObObN^.^.ZU�M�M�I�I�M�Q�U�U�VZZZVU�U�U�U�U�Q�Q�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�Q�Q�Q�Q�Q�Q�
Q�Q�Q�U�U�VVZ
ZZZ-^-^.^.b.bNbNbObObObOfOfOfOfObO
bObObN^.^.ZVQ�Q�M�M�Q�Q�U�VZ
VVU�U�
U�U�cQ�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�U�U�U�U�VVZ
ZZZZ.^.^.b.bNbNbO
bObOfOfOfObObObO'bN^.^.^.ZVU�U�Q�Q�U�VVZZVU�U�
U�U�
Q�Q�OQ�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�Q�Q�Q�Q�Q�U�
U�U�U�VVZ
ZZZZ-^.^.^.b.bNbNbObObNbNbNb.^.^.Z.Z
ZZVU�
U�U�Q�U�
Q�Q�_Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�U�U�U�U�VVZZZZ-^.^.^.^.b.bNbObObObObNbNbNb.b.
^.^.^.^-
ZZVV
U�U�Q�Q�SQ�Q�Q�Q�Q�Q� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�Q�Q�Q�Q�Q�Q�
U�U�U�VVVZZZ-^.^.^.^.b.bNbObObO
bNbNb.^.
^.^.^.ZZZVVVU�U�Q�
Q�Q�Q�Q�Q�Q�Q�Q�
Q�Q�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�Q�Q�Q�Q�Q�Q�Q�U�U�U�U�U�VVZZZ-Z.^.^.
bNbNbObO
bObNbNbObNbNb.^.^.^.'ZZZVVU�Q�Q�Q�Q�Q�M�M�M�Q�Q�Q�Q�
Q�Q�OQ�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�U�
U�U�VVZZZZ-^.^.^.^Nb.bNbNbNbNb.b.^.
^.^.^.ZZZVU�U�Q�Q�Q�"M�M�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�Q�Q�Q�Q�Q�Q�Q�U�
U�U�U�VVZZZZ-Z-Z.^.
^.^.^.b.
bNbNbNb.b.b.b.^.^.^.Z-ZZVU�Q�Q�M�M�I�I�I�
I�I�I�I�M�M�M�M�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�Q�Q�Q�Q�
Q�Q�
U�U�U�VVZZZZ.^.^.^.^.b.^.^.b.^.^.^.3^.ZZZVU�Q�Q�Q�n�j�^MQ�E�A�A�E�E�E�E�I�I�I�M�
M�M�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�M�Q�Q�Q�
Q�Q�
U�U�U�VVVZZVU�]�eJeQIQ�VZZZ-^.^.^.kZ-ZZVU�Q�M�M�j�wOr�r�r�n�bmM�=i=iA�A�E�E�I�I�M�M�M�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
M�M�M�Q�Q�Q�
Q�Q�Q�U�
U�U�VV
ZZVU�U�ij}	}K}*t�D�M�Q�U�ZZ-^.^.Z.ZZZVU�Q�M�M�o�w/r�r�KI�9H=h=iA�E�E�I�I�I�M�M� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� M�M�Q�Q�Q�Q�Q�Q�Q�U�
U�U�/U�VVVVU�U�U�m}K}�}�}�}ld�4�M�Q�U�ZZ^.^.^.^.Z.ZZZVU�Q�M�M�j���w
r�r�Oj�ZKA�5'5H9H=i=iA�E�I�I�I�M� k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� M�M�Q�Q�Q�Q�
Q�Q�U�U�+VU�U�Q�Q�mJ}K}�~2~2}�}�l�<B5(I�Q�U�ZZ-^.^.wZ-ZZZVU�Q�M�I�j����r�r�r�n�blR
N--1'5'9H=H=iA�E�E�I�I� k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� M�M�
M�M�Q�Q�Q�Q�
Q�Q�U�U�Q�M�e�}*}�~2~S~}�}K\�4B(BA�I�Q�U�Z^^.^.^.^.Z.^.ZZZZVU�Q�M�I�j�����sr�f�V+R*V*^�Zo^�=�-1'5H9H=iA�E�I�I� g3k4k4suw���svgV�w�^�oUsuw���svgV�RoV� M�M�
M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�U�
U�U�Q�Q�M�]�}	}�~2~2~2}�}lm	H� ! 9iE�M�Q�ZZZ^.^.ZZZZZVVU�Q�M�I�j�����Fr�ZLR*I��^��f�ZL^�V-1'1'5H=H=iA�E�I� gg3k4suw���svgV�w�^�oUsuw���svgV�RoV� M�M�
M�M�M�Q�Q�Q�Q�Q�Q�Q�
Q�U�/Q�Q�Q�M�Y�}	}�~2~2~}�}liL�,B  5(AiI�Q�VZZ{VVU�Q�M�I�f�����F b�NR*b�5��A�b�{S{1r�M�-1'9H=hAiE�E� ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�
M�M�Q�Q�Q�Q�Q�Q�/Q�]�UI]Iy	}�~2~2~}�}Kh�L�0B  $�1'=iI�Q�U�ZZ{ZVU�U�Q�M�I�^�����F{ { V(VMs�w2� j�{1r�r�f�-15'9H=iAiE� ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�M�Q�Q�Q�
Q�Q�3Q�Q�Q�m}	l�y*~~S~}�yJ`�D�(B  �$�1'=iE�M�U�ZZVU�Q�Q�M�I�^�k0���F{ { r�b�{R�f�V+RMN-f�r�r�r�r�9h-1'5H=HAiA� ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� M�M�M�M�
M�M�{M�Q�Q�Q�Q�Q�Q�Q�Q�M�I�MjuK}�l�yl}�}�u*X�<c!  c�(�5'=iI�Q�U�VZZZZVU�Q�Q�M�I�Zrg8{o��E{ { { { {" r�VLs�{0
r�r�Kr�M�(�15'9H=iA� ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� I�M�M�M�M�M�Q�Q�
Q�Q�SM�M�I�E�D�u�yl`�mm	P�,B   b� �-9HA�I�Q�U�VZZVU�U�Q�Q�M�I�UHs9g5��{${ { { r�w
Ir�r�
r�r�Kr�f�(�-5'9H=hAi cgk4oUw���svgV�w�^�oUsuw���svgV�RoV� I�I�M�M�M�M�Q�Q�SM�I�E�A�V�Mylm	HcDd("   B��(�5'=iE�M�Q�U�VVVU�U�Q�Q�M�I�Q�x�o|kU�{${ { { [{n�j�f�f�bgb�R'(�-5'9H=hAi b�gk4oUw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�M�M�M�
M�M�[M�Q�Q�Q�Q�M�M�E�E�c;{�_H�l�L� !   B��$�1'9HA�I�Q�Q�U�U�U�U�U�Q�Q�M�I�R/m)}ko�k2#{ { _ { b`R R N M�R =� �(�-5'9H=hAi cgk4oUw���svgV�w�^�oUsuw���svgV�RoV� I�I�I�M�M�M�M�M�/I�I�k�w�o�c^1�(!@c   B��$�19HA�I�M�Q�
U�U�U�Q�M�M�I�U�kZq)y�o|b�z�{ { [v�{ {  v�Z@M�R N R R-$�$�(�15'9H=hAi ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� 
I�I�M�M�M�M�SM�I�Nk�w�k�[>� �B  b��$�-9HA�E�M�Q�Q�U�U�U�Q�Q�M�M�I�U�wkZuky�g:^�r�
{ { _{ V R{ r�V M�R M�R I� �� �$�-1'5'9H=iAi ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� 
I�I�M�M�M�M�
M�M˻M�M�I�RQo�w�k�[9���b�B ��$�-9HAiE�M�Q�Q�U�U�U�Q�Q�M�M�I�Y�~vw[kZu�y(f�Z�b� { {  b`Rn�#f�R N R N V 9��� �$�(�-1'9H=HAiAi ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� 
I�I��I�I�M�M�M�M�M�M�I�I�N1s�w�k�V�5������� �$�-5H=iE�I�M�Q�Q�U�U�Q�Q�Q�M�I�]�~v~�w[kZmjp aJ[V�n� { { n�v�{'b`M�N N R R")�� �$�(�-1'5'9H=hAiA� ggk4oUw���svgV�w�^�oUsuw���svgV�RoV� I�I�
M�M�3I�I�E�RSs�w�g�R�1��%���� �$�(�19H=iE�I�M�Q�Q�Q�M�I�]�~�~�~�s:gz]jd \ V1V�R�n�   v�Z@M�R N V J��� �$�(�-1'5'9H=hAiAiE� gg3k4suw���svgV�w�^�oUsuw���svgV�RoV� I�I�3I�E�Vss�s�gR�-�� ��� �$�(�)-5'9H=iE�I�M�Q�
Q�Q�Q�Q�M�M�Y�~�~�~�~�n�_9Zs\ X P!R1N�Js^�w&r�V M�M�R ZB9d��� �$�(�-1'5'9H=H=iAiE�E� g3k4k4suw���svgV�w�^�oUsuw���svgV�RoV� I�I�3E�Rts�s�c^N�)m! �� �$�(�-1'5(9H=iA�E�I�M�M�Q�
Q�Q�Q�Q�M�I�nS~�~�~�~4n3Z�Z�UJP L H I�B5�=�J-J(RGVhN*)��� �$�(�-1'5'9H=h=iA�E�E�E� k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� I�I�7I�E�Z�s�o�c^Jw!+�$�� �$�)1'5H9H=iAiE�E�I�M�M�Q�Q�
Q�Q�Q�M�M�I�~�~�~vzu�q�^SV�V�UkH @ < 8 0�))*5�JS9���� �$�(�-15'5G9H=hAiAiA�E�E�E� k4k4k4suw���svgV�w�^�oUsuw���svgV�RoV� I�I�
I�I��_I�E�E�Z�w�o�_=Fv!*�� � �$�-1'9H=iA�E�E�I�M�M�M�Q�Q�Q�Q�Q�Q�Q�M�M�I�E�~�~vzu�q�mni�VsN�N�Q�H�< 8 0 ( $  !���� �$�(�-1'5'9H9H=hAiA�A�E�E�E�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�I�I�I�I�E�E�E�V�w�o�_=BU
%) �� �$�-5'9HAiE�E�I�M�M�M�M�M�Q�Q�Q�Q�Q�Q�Q�M�M�I�E�r3y�q�q�mniNi,aMN1FR:9�A�=J=IAk ���� �$�(�,�-1'5'9H=H=iAiA�E�E�
E�E�7 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�I�
E�E��E�NRs�o�[>4��� � �$�-5'9HAiE�I�I�M�M�M�M�Q�Q�Q�Q�Q�Q�Q�Q�M�M�M�I�E�Y�q�m�mniNe-a,`�T�E
9J-*)*=m9K���� �$�(�-15'5H9H=hAiAiA�E�E�E�E�E�I�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
E�E�+E�E�E�o�k�V�5��%�� �$�-5'9HAiE�I�I�
M�M�M�Q�Q�Q�SQ�M�M�M�I�E�Eji�mniMe-a\�T�HfHE@$DFH�,���� �$�(�,�11'5'9H=h=iAiA�E�E�E�E�E�
E�I�c k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�E�E�E�A�A�g[k�R�)m�%� � �$�-5'9HAiE�I�I�M�M�M�
Q�Q�KM�M�M�M�I�E�=iMkiMe,a\�P�Hf@$@$Hg@���� � �$�(�-15'5H9H=h=iAiA�E�E�
E�E�;E�I�I�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
E�E�+A�Z�k�Fv!+�$� � � �(�-5'9HAiE�E�I�M�M�M�M˷M�M�I�E�Ai=iMJ]X�L�DE@$@EH�0��� � �$�(�,�11'5'9H9H=iAiA�A�E�E�E�E�E�I�I�I�E�E�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�E�E�A�I�c~9� �-($� � �$�(�-5'9HAiE�E�I�I�M�M�M�M�?M�M�I�E�A�Ai=HA(D�DE@$@f<�(� � �$�$�(�,�-1'5'5H9H=h=iAiA�E�
E�E�I�I�E�E�
E�E�g k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�E�A�AiA�9�)-(�$�$�$�)15'9H=iE�E�I�I�M�M�M�M�M�M�M�M�I�I�E�A�Ai=i9H5'1'-,�
(�(�(�,�-15'5'9H=H=iAiA�E�E�E�I�I�I�E�
E�E�W k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�E�AiAi=i9H1'--(�)-1'5(9HAiE�E�
I�I�M�M�M�M�M�M�M�M�CM�I�I�I�E�E�Ai=h9H5'5'1----115'5'9H9H=h=iAiA�E�E�E�E�E�I�
I�I�cE�I�E�E�E�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�A�AiAi=i9H5(1'1-1'5'9H=HAiA�E�E�
I�I�I�I�M�M�M�M�M�M�
M�M�7M�I�I�I�E�E�AiAi=h9H9H5'5'5'1'5'5'5(9H9H=H=iAiAiE�E�
E�E�I�I�I�I�E�I�
E�E�[E�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� E�AiAiAi=i9H9H5(5'5'5H9H=hAiA�E�E�I�I�I�I�I�M�I�M�M�#M�M�M�M�M�I�I�I�I�E�E�AiAi=i=H9H9H9H=H=iAiAiA�E�
E�E�
I�I�E�E�
E�E�;E�E�E�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
A�Ai=i=h
9H9H=H=iAiA�E�E�I�I�CI�I�I�M�M�M�M�M�M�M�M�I�I�I�I�I�E�E�A�Ai=i=i=i=h=H=h=i=iAiAiA�E�
E�E�E�I�I�I�I�I�E�E�
E�E�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� AiAi=i=h=i=iAiAiA�E�E�I�&I�I�M�M�M�I�I�I�
I�I�E�E�E�A�A�AiAiAiA�E�E�E�E�E�E�E�
I�E�E�E�E�E�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� AiAiAiE�E�E�E�E�I�I�6I�I�I�E�E�E�
E�E�A�A�E�E�E�E�
I�I�E�E�E�E�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� AiAiAiA�E�E�E�E�E�E�E�I�I�I�I�I�2I�I�I�E�E�E�E�E�
E�E�E�E�
E�I�E�E�E�E�7E�Ai k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� AiAiA�A�A�E�
E�E�E�E�E�E�E�I�I�I�I�I�.I�I�I�E�*E�E�E�E�E�E�7AiAi k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� 
AiAiAiA�A�A�A�E�E�E�E�E�E�E�E�I�
I�I�.I�I�I�I�I�E�E�E�I�E�E�E�E�E�E�E�;A�AiAiAi k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �$�$�7$�(� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �Y�Y�3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� fjMjMj-j-j-jM6jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� zjMjMjM^Eh5&5&9'M�bjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� zjMjMjME�(�b �5'U�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� FjMjMjMQ�5'=GEhY�*jMjMjM5'b   �^jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMjM5&9'E�.jMjMjM9'�!(�Q�*jMjMjM9'
   fNjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMjM9G b..jMjMjM=G! AbN*jMjMjM=G�   (�npjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMjMI�jo.jMjMjME�(�AE�z�*jMjMjMQ�9G � )bpv�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� FjMjMjM]�U�jor�*jMjMjMbY�b.jor�v�nnjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� zjMjMbQ�=H5&111=HM�U�b,jMjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� vjMjMjMQ�9'$��b! !�$�1E�Y�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� FjMjMU�E�9G555&=HM�]�jMjMjMM�,��!   A(�EhY�jMjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� BjMjMf,I�1 �b! !�,�=GQ�f-jMjMjMjMQ�,��   $�EhY�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMjMf,M�=G15&9'E�U�f-"jMjMI�-�   �AhY�jMjMjMb9G�   (�Q�f-jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMf,Eh(��A A �5'Q�f-jMjMjMjMY�1�   $�Q�bjMjMQ�(�A   BAh]�jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMM�(�b 
  �AhY�jMjMjMjME�$�   5'Y�jMjMAh�   (�U�jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjM=G�   (�U�jMjMjMjM=Gb    �^jMjM9Gb   �^jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjM5&A   b^jMjMjMjM5&!   Bb.jMjM5&!   Ab.jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjM5'   bNjMjMjMjM5'   bNjMjM5' "  bNjM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjM9GA   �n�jMjMjMjM5'!   BfNjMjM5'!   AfNjM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMI� �   =iz�jMjMjMjMAh�   (�jojMjM=Hb    �jojM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMU�9'� 
  )n�r�jMjMjMjMQ�,�   Q�v�jMjMM�$�   A�v�jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMf-U�E�(�b �A�n�v�jMjMjM^Ah �   1'z�nojMjMY�5&A   �jpr�jM
jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� jMjMjMf-^Y�b.jor�z�r�jM"jMjMU�Eh$�   5(v�v�jMjMjMf,I�(�   I�z�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� BjMjMf-Y�U�9' �B b)V4v�jNjMjMjMjM]�I�$�   A�Vr�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� FjMjMb^b.jojpn�r�v�r�jMjMjMY�I�,�B   �I�Vv�jMjMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� vjMjMjM^U�E�-�A A �E�j�{r�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� zjMjMf-^Y�b.jojpn�r�z�r�jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �jMjM3 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� �  _ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>�>x
:x:x:X:W
6W6W6W%��VyVyVyVxVXRXRXVXVXVXVxVxVxVyVy_ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>�>x
:x:x:X:W
6W6W6W%��VyVyVyVyVxVXRXRXRXRWRWRXRXRXVXVxVxVyVy_ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>�>x
:x:x:X:W
6W6W6W%��VyVyVy�VyVXVXRXRXRWR4RRVRVRZRVRV1RNNJJN3N6N7N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>�>x
:x:x:X:W
6W6W6W%��Vy
VyVy�VyVXVXRXRVR2ZRbuj�j�fxfWb6^U�U�Q�M�M�I�I�I�I�I�JN6N6RWRWRXRXVxVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>�>x
:x:x:X:W
6W6W�6W%��VyVyVyVyVxVXRWR4ZRfwn�n�n�j�fxbW^5Y�U�Q�Q�M�M�M�I�I�I�I�E�E�I�FJN6N7RWRXRXVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W�6W%��VyVyVyVXRXRUZSj�v�r�n�j�fWb6^Y�U�Q�Q�Q�M�M�M�M�M�I�I�I�E�EpEoE�E�E�JN6RWRXRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��VxVXVXRXR2buv�v�r�j�fW^Y�U�U�Q�Q�Q�Q�Q�M�M�M�M�M�M�I�I�I�EpEoAOAOA�E�E�JN7RW k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W'6W%��RXRXRWR2j�v�v�n�fx^6 �EpQ�$�9-Q�Q�cAO �IpM�M�I�IpEpAO=N=NAmA�A�JN6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W'6W%��RWRWN3fvr�r�n�b7Y�U� 9,Q�"(�Q�Q�c=-!IpM�M�M�I�IpEoAN=.9-=m=�A�J k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��N6N5Vj�n�jx^6U�Q�Q� 9Q�",�Q�IpM�=.M�Q�Q�=.9-"IpQ�Ep=.M�M�IpAO=N9-9-=�9�A� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��FI�ZbWbW^U�Q�Q�Q� 9Q�",�EpdC (�Q�,� e"IpIqC  �M�I�EpAN9-9-=M1M9� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��A�I�U�ZZU�Q�Q�Q�Q� 9Q�",�EO 9- �CQ�" �,� M�$�CAO EOM�IpAO=-9=-)+1n k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��=�E�Q�Q�Q�Q�M�Q�Q�Q� 9Q�",�AO"Iq9CEp AOAN"M�d(�Q�!0�M�IpAO=-59- �-L k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� iuiuiuiu |�|� |u|u|u|u 
6W6W�6W%��9�A�M�I�I�I�M�M�M�Q� 9Q�",�AO"Iq5CEp AOAN"M�C(�Q�!(�M�EpAN9-59-�)+ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W�6W%��5�9�IpEpEpEpIpM�M�M� 9-Q�"(�EO"Iq5BM� =NAN!M�e �Q�1IpAO9-559,�% k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W�6W%��5�5n=mAOANAOEOEpIqM�!�AN 5AO"Iq5"Q�"�(� Iq$�"0� ANAO9-50�51
�% k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W�6W%��9�5�1M=M=.=.=.ANEOEp1  CM�=NCIq9CQ�5 edEpIqC  �=.9-10�0�5��)+ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� (
(
(
(
 }�}� }}}} 
6W6W+6W%��=�9�1n-+=M9-9-9-=-=NAOEOEOIpIqIqM�
M�M�[M�IqIqIqIpEpAO=.9-50�,�0�1�� �-L k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� (
(
(
(
 }�}� }}}} 
6W6W�6W%��A�=�5�-,)
5+9-5599-=.=NAOAOEoEpEpIpIpEpEpEpEOAOAN=.9-50�,�,�1-
���)+5n k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W6W%��E�E�=�5n)+ �(�5,5
559-9-=-=.
=.=.g=.=-9-9550�,�,�,�1
����%
1n=� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W'6W%��J6JE�9�1n)+ ��(�15555555g510�0�0�,�0�0�,� �d���%1n=�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W36W%��NWN6JE�9�1n), ����$�1
1555110�0�
11_1
,� ��d��� �),5o=�JN6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� (
(
(
(
 }�}� }}}} 
6W6W/6W%��RXRWN7JE�=�5�-M%+ �������ddc
cc_ddde���� �)+1M9�A�JRWRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J�:  
6W6W/6W%��VXRXRWN7J6E�A�9�1n-M)+%
 �������
��c����� �%
),1n9�A�JN7RXVXVX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W+6W%��VXVxRXRXRWN6JE�A�=�5�1n-M),%%
!
 � �c � �%
)+-L1n5�=�A�JN7RXVXVXVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W6W%��VyVyVyVxVxRXRXVXVxVxVxVyVyC k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W6W%��VyVyVyVyVxVyVXRXRXRWRWRXRXRXVXVxVxVyVyC k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W6W%��VyVyVyVXRXRXRXVTZQ^obmf�f�f�bk^KZ+V,VRNN6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W6W%��Vy
VyVy{VyVXVXRXVWZrf�v�{wv�r�n�n�j�j�f�bibi^H^HZ'VQ�M�N5N6N7RWRXRXVxVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� T�T�T�T� TT TTTT 
6W6W6W%��Vy
VyVy�VXVXVTf�wOO{.wv�r�n�j�j�f�f�fibhbh^H^H^HZ'V'VQ�M�JN6N7RWRXRXVx k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W�6W%��VyVyVyVxVXVUf�{/�pO{v�r�n�j�j�fhfhfhbgbgbGbG^G^G^'Z'Z'VRQ�M�I�I�J6N7RXRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� T�T�T�T� TT TTTT 
6W6W/6W%��VyVyVXRXZsr���P{.v�n�j�j�fhfgfgfgbFbFbF[^G^G^'Z&VVQ�Q�M�I�E�JN6RW k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W+6W%��VyVXRXZr{/��Ov�n�(�=dfg9D �bFfF
fFfFfFbF
bFbF[^F^&Z&VQ�M�M�M�E�A�E�N6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W'6W%��VyRXVTwpp.r�j�fha-^& =cfFfF_fFbFbFbF^&Z&VQ�M�I�M�=f=�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��VXRXf�{.O{r�j�fgffa1#9c ^%ffZA�bFffff^%A�Z%ff^%U�ZA�bF^&Z&VM�I�I�I�9I=� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��RXR7j�wv�n�j�fgfffF�1#@=cffbF  �ff^%� @bFE��@ 5CbF^&ZQ�I�E�I�5%5m k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��RXN6j�j�j�f�fgfFfFff� affff1#�^% ^%M�aff =cE� bF1#�bFbF^%U�M�E�I�5$-* k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��RWJbhbhbhbGbFbFfFff�� M�ff�a1# I�9C@1# (�E�affQ� ZbF^%Q�I�E�E�5$(� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��N7J^'Z'Z'^&^FbFfFffa5#� �ff��1#A�^%5CA1#=cQ�A�affM� Z^&ZQ�E�A�E�1$� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��N6E�VVVVZ&^&bFbFa1#Q� Z%(�$�ff1#U�E��ff=cA�E� fFE�aZZQ�I�A�A�E�(� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�  (( (�(�(�(� 
6W6W�6W%��N7E�I�Q�M�M�Q�VZ%^%a-bFa(�A� 5C ffQ�@Ad M�E� 1#a �VQ�I�A�=cA�5#� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J�:  
6W6W�6W%��N7F=�M�M�I�I�M�Q�V$�9c^%A��bF �@A�fFbF1#@-bF=c� �aA�M�E�=c=c=c=d�� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W�6W%��RWJ=�5lI�I�E�I�I�M�Q�VZZ^%^%^&^%bFbFbFbFZ^%Z%9C@Q�I�I�A�=c9c=c=d���%
 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J� ���� =�=� )))) 
6W6W+6W%��RWJA�5�-K=�I�E�E�E�I�I�M�Q�Q�VV
ZZgVVU�Q�- E�A�=d9c9c=c5#��� �-L k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W�6W%��RXN6E�=�1m%
)A�E�A�A�A�E�E�E�I�I�I�M�M�I�I�I�E�E�A�=c=c9c=c=c9C �d�� �)+5o k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W'6W%��RXRWJE�9�1M%
�)9EA�A�A�=cA�A�A�gA�=c=c9c9c9c=d=d5#$�cd�� �)+1n=� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W'6W%��VxRXN7JA�9�1m)+ ���$�5$=dA�
A�A�k=c=cA�A�Ad=d9D1$$��dd��� �),5n=�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W/6W%��VxRXRWN6JA�=�5o-L% �����������_�d����� �%-M5�=�E�N6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W�6W%��VyVxRXRWN7N6JA�=�5�1M)+%
 ���������������� �%
)+-M5o9�A�JN6RW k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� =�=� )))) 
6W6W�6W%��VyVyVxVXRXRXRWN6JE�A�9�5o1M),)%
 � � ��� � � � �%
)+-,1M5n9�=�A�E�J6N7RWRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W6W%��VyVyVyVyVxVyRXRXRXRWRWRXRXRXVXVxVxVyVy7 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� )�)�)�)� �� ���� 
6W6W6W%��VyVyVy�VyVXVXRXRXRWV3ZP^ObnbnbM^M^,ZVVRR2N6N6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� )�)�)�)� �� ���� 
6W6W6W%��Vy
VyVy�VyVXVXRXRVZ0bnj�n�n�n�jpfOb.^Y�U�U�Q�Q�Q�Q�Q�RNN5N6RWRWRXRXVxVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J� )�)�)�)� �� ���� 
6W6W�6W%��VyVyVyVyVxVXRWVSbNn�r�r�r�n�n�fob.^^Y�U�U�Q�Q�Q�M�M�M�M�M�Q�NJN6N7RWRXRXVx k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� )�)�)�)� �� ���� 
6W6W�6W%��VyVyVyVXRXVTfor�z�v�r�n�jpfNb-^Y�Y�Y�U�U�U�Q�Q�Q�Q�M�M�M�I�I�I�M�I�JN6RWRXRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� )�)�)�)� �� ���� 
6W6W+6W%��VyVxVXRXZ1j�z�{v�r�jof.b]�Y�Y�U�U�U�cU�Q�Q�Q�Q�M�M�I�EhEhM�I�E�JN7RW k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J� )�)�)�)� �� ���� 
6W6W+6W%��VyRXRXZ0n�z�z�v�n�M�B�Q�U�,�5U�U�U�_U�Q�Q�Q�M�I�EhEgAGEgI�A�JN6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� )�)�)�)� �� ���� 
6W6W+6W%��VXRXV2joz�z�r�jOM� ,�AbU�b �U�U�U�[U�Q�Q�M�I�EgAGAGEgE�A�J k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� )�)�)�)� �� ���� 
6W6W�6W%��RXNW^,n�r�n�fN^1bU�Ih �U�b �U�U�IhEgU�U�U�IhAGU�U�M�M�IhQ�M�IhAGAGAGI�9�A� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W�6W%��RWN6b,jojob.]�Y� �$�U�U�U�U�b �U�AG!!0�U�9&  �U�$�bAU�Q�M�EgAG=GEg=j9� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��N7J]�b-^Y�Y�U��(�U�U�U�U�b �U�B0�AG!Q�0�,�AG U��!EgU�U�M�IgAG=&EG=H1n k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��N6E�U�U�U�U�U�U��(�U�U�U�U�b �U�!,�1 IhU�M�,� U��(�U�U�U�M�IgAG=&AG9G-, k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��JA�Q�Q�Q�Q�Q�U��(�U�U�U�U�b �U�!Ab �Q�$� B U��(�U�U�Q�M�Eg=&=&AG5'% k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��J=�M�I�I�I�M�Q�(��U�I�bQ�b$�U� EgU�9&U� 9Q� U��$�U�Q�M�IgAG9&9&AG)%
 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��J=�E�IhEhEhIhM�5 AG�!U�bBU�b �9AU� ,�(� U��$�Q�M�IgAG9&59&9&�%
 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��J=�5mEgAGAGAGEgIh� !=&U�AG ,�Ih  5U�(� � Q��$�IhEG=&9559&��% k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W�6W%��JA�5o1*AG=G=&=&AGEGAGIhM�M�Q�Q�M�Q�Q�M�Q�Q�Q�M�M�M�IhIgEG=&9&5159&�� �), k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +�+�+�+� �� ���� 
6W6W+6W%��N6E�9�-L)	9G=&9&=&=&=&AGEGEgIhIhM�
M�M�cM�IhIhEgEGAG=&9&510�55���%
1n k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W6W%��N6J=�5o), �-
9&9&9&=&=&='AGAGAGEG
AGAGW=F=&9&9510�0�5$����%
-M9� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� ���� �� jjjj 
6W6W+6W%��RXN6E�=�5n)+ ��(�5&9&9&95999&
9&9&g955511551$�d���%
-M9�A� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W#6W%��RXR7JE�=�5n-,%
���(�5
9&9&o9555555551$��d��� �%
-M9�A�J k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W/6W%��RXRXN7JE�=�5�1M)+ �������ddd
cccddde���� �%
),1n9�A�JN6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W�6W%��VxRXRXR7N6JE�=�9�1n),%
 ����������������� �)-,1n9�=�E�JN7RX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W36W%��VyVxVXRXRXRWN6JE�A�=�5�1n-L)+%
%
 � ���
 � �S%
%),-M1n5�=�A�E�JN6RWRXVX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J� ���� �� jjjj 
6W6W6W%��VyVyVyVxVxRXRXRXVXVXVxVxVxVyVyC k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W6W%��VyVyVyVyVxVyRXRXRXRWRWRXRXRXVXVxVxVyVyC k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ���� �� jjjj 
6W6W6W%��VyVyVy{VXRXRXRWRTRQRoVnV�V�V�RlNLN,J.JJNN6N7RWRWRXRXRXVxVxVyVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W6W%��Vy
VyVy�VyVXVXRXRVRqV�b�cc^�^�Z�V�V�R�R�NiNiJHJHF(F
E�I�J6N6N7RWRXRXVxVyVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W6W%��Vy
VyVy�VXRWRTV�gkOkOg.c^�Z�Z�V�R�R�R�NiNhNhJHJHF'F'F'FA�E�JN6N7RWRXRXVy k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W�6W%��VyVyVyVxVXRUZ�k/s�opkOc^�Z�V�R�R�NhNhNhNgNgJGJGJGJGF'F'F'BBA�=�A�E�N6NWRXRX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W/6W%��VxVxVXRXRrcs�s�kOg.^�V�R�R�NhNgNgNgJFJFJF_JGJGF'F&BB=�=�=�=�E�JN6RW k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W+6W%��RXRXRXRqkOs�s�kO^�Z�R�Ng!a��!JFJF[JFF&F&B=�9�9�9�9jA�FN6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W+6W%��RWRXRScopopg.Z�R�NgNgJF-d� !-cJFJFWF&F&B=�9�9�9�5J=�E� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�J�J�J�J� ````    ���� 
6W6W�6W%��N6N6V�g.kOcZ�R�NgNfJFJFJF=� NfNfJF-c=�NfNfF%-cF%NfJF9�1�JFF&F&B9�5�5�5�1,=� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W�6W%��JN2Z�^�^�Z�R�NgJFJFNfNfNf9� NfJF�  =�F%a@@F%-c  !JFF&B=�9�5�9�(�5� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� ````    ���� 
6W6W6W%��A�J/V�V�R�R�NgJFJF
NfNfs9� Nf9� JF��-caNf-cJF )CF%-cJFJFF%=�9�5�9� �-M k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W6W%��=�FNhNhNhJGJFJFJF
NfNfw9� Nf5�aJF5�a=� �F%Nf%# 1�NfJFJFF%=�5�1�5��)+ k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W6W%��9�A�F'F'F'F&JFJFJF
NfNfw9� Nf1�AJF5�@JF9�@aF%Nf%# )CJFF&B=�5�1�5��!
 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W6W%��5�=�FBBBF&F&JF
JFJFw9� Nf9�@JF)CaNfJFB -cJFJF-c BB=�5�1�1�5�� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W�6W%��5�9�=�=�9�=�=�BF%F&JFJFJF9� JFB -c@!9� 5�a9���%#a=�=�5�1�-c1�%� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W�6W%��9�9�5�=�9�9�9�9�=�BF%F%F%=��JFJF1�@�JFJF�@!JF=�a@-c9�5�-c-c-c-D�� � k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W�6W%��=�=�9�1l9�5�5�5�9�9�=�BBF%F%F%F&JFF%JFJFJFF%BF%BB=�9�5�1�-c-c-c-D���% k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J� +`+`+`+` �� ```` 
6W6W'6W%��A�A�=�1n)J5�5�5�1�5�5�9�9�=�=�BB_=�=�9�5�5�1�-c-c)C1�)$��� �-M k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�:  
6W6W6W%��FJE�9�1M%
%'1�1�
1�1�5�5�5�9�
9�9�9�5�5�1�1�-c
-c-cG)D�e�� �),5� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J�#J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�>�
>�>�>x:x
:x:x:x:W
6W6W'6W%��N6N6JA�9�-M%
�!-e1�1�1�-c1�1�1�1�-c
-c-cw-d-d)$�ed�� �-,5�A� k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�B�
>�>�>x>x
:x:x:x:W
6W6W'6W%��R7RWN6JA�9�-M% ��� �)E1e1d
1�1�_-c-c1�1�1d-d-D)%��dd���%
-M9�A�N6 k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J�#J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�>�
>�>�>x>x
:x:x:x:W
6W6W�6W%��RXRXRWN6JA�9�1n),%
����������������d�����%
-,5�=�E�N6RX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�N�J�J�J�J�J�J�F�F�F�F�F�F�B�
B�B�B�B�
>�>�>�:x
:x:x:x:W
6W6W/6W%��VXVXRXRWN7JE�=�5�1n),% �������
��W������ �%-M5o=�E�JRWRXVX k4k4oTsuw���svgV�w�^�oUsuw���svgV�RoV� N�N�
J�J�#J�J�J�F�F�F�F�F�F�B�B�B�B�B�B�>�
>�>�>y>x
:x:x:x6W
6W6W/6W%��VXVyVxRXRXRWN6JE�=�9�5�1m-L)+%%
!
 �
 � �[ � �%
%),-M1n9�=�E�JNWRXVXVyVy k4k4oTsuw���svgV�w�^�k4oTsu{��{�w�oUgc """""""!!!!*""&!!""""/ gsvw�w�{��w�oT^�R�w�kWk4k4oUw�{��{�svoU
k4k4k4oToToT;oTk4k4gcb�^�Z�Z�Z�V�V�V�Z�Z�Z�Z�Z�^�^�b�cggk3k4k4oToToTwoTk4k4k4ggb�b�^�^�Z�Z�Z�Z�^�^�^�Z�Z�Z�Z�Z�^�b�ggk4oToToUoUsUsususvsvw�w�{��{�w�k4Z�Row�{�k4k4oTsv{���{�svsuoU
oToToUoUoToTkTk4k4k4k4k3k3k3k4k4
k3k3k4k4oToToUoUoToToTk4"k4k4Sk3k3k4k4k4oToToUoUoUoUsususvwvw�w�{�{��{�sub�V�Ro{�  s�gk4oUw�{��{�{�w�w�svsu�susu7svsvw�w�w�{�{�{��{�svk4^�V�s�    {�ggk4suw�{���
{�{�{�w��w�w�w�w�{�{�{�{�7{�w�kTb�V�Row�      w�ggk4oUw�{�{�{����{�{�{���������{�{�{�w�svk4b�Z�V�s�	   s�b�gk4oTsuwvw�{�
{�{�{������{�
{�{�{�w�svoUk3b�Z�V�s�   w�b�b�ggk4oToUsususvw��w�w�w�svsvsuoUk4g3b�^�Z�s�s�   {�s�^�^�b�b�ggg�k4k4ggcb�^�Z�s�s�{�     �{�w�s�szs�sy�sysyszs�w�{�    h     
 �
 �O
 �%
  D0
 t

G �(a 
  U
 d� �c
 sstartColorSelection:
 sselectColor:
 sendColorSelection:
 sdelete�  u��  (x�*   O?
O?O?��O9O9
O9NyNyNy2yNy2y2y�� O?O?O?O?O9O9O?�O9O9NyO9O9NyNyNy2yNy2y2y2y� O?O?O?O9O?O?O9��O9O9NyO9NyNyNy2y2y2y2y� O?
O?O?�O9O9NyO9NyNyO9Ny
NyO9NyNy2y2yNy2y2y2y2y� O?O?O?O9O9O?O9��O9O9NyO9NyNyNy2yNyNy2y2y2y� O?O?O?O?O9O9O?�  
2y2y2y� O?
O?O?O?O9 ����� 
2y2y2y� O?O?O?O?O9O9O9 ���� �z��� 
2y2y2y� O?O?O?O9O?O?O9 ���� �z��� 
2y2y2y� O?O?O?O?O9O9O? �Z���   ���  �
��+�   ��  �� ��  ��   j��� 
2y2y2y� O?
O?O?O?O9 �Z��� ��
 �� 
�� ���� �� �� j��� 
2y2yy%) O?O?O?O?O9O9O9 �Z��� ��
 �� 
�� ���� �� �
� n��� 
2y2y2y� O?O?O?O9O?O?O9 �Z��� ��
 �� 
�� ���� �� �
� n��� 
2y2y2y� O?O?O?O?O9O9O? �Z��� �� ��  �
��'�   ��  �� ��  �� n��� 
2y2y2y� O?
O?O?O?O9 ����� 
2y2y2y� O?O?O?O?O9O9O9�  
2y2y2y� O?O?O?O9O?O?O9 ����� 
2y2y2y� O?O?O?O?O9O9O? ����� 
2y2y2y� O?O?O?O9O?O?O9 �
��
����F��
{�{�{�{�F{�{�
{�{�{��:������� 
2y2y2y%) O?O?O?O?O9O9O? �����.���{�{�{�{�{�.{�{�{�{�{�{�{��.������� 
2y2y2y� O?O?O?O9O?O9O9 �
����������&��{�{�{�w�w�w�w�w�w�w�&w�w�w�w�w�w�w�w�w�{�{�{�&��������� 
2y2y2y� O?O?O?
O?O9 �����������{�{�{�w�w�w�w�w�w�w�w�w�w�w�w�w�w�w�w�{�{�{���
������� 
2y2y2y� O?O9O9O?O?O9O? �
��������
����
�������{�
{�{�{�w�
w�w�w�s�s�s�s�s�
s�s�s�s�
s�s�s�s�s�s�s�s�
s�s�s�s�
s�s�s�s�s�s�s�w�
w�w�w�{�
{�{�{������
������� 
2y2y2y� O?O?O?O?O9O9O9 �����
����
����
{�{�{�w�
w�w�s�s�
s�s�s�s�
s�s�s�s�
s�s�s�s�
s�s�s�s�
w�w�w�{�
{�{���
��������� 
2y2y2y� O?O?O?O9O?O?O9 }
|||{{{{�
��
��
�����{�
{�{�
w�w�
s�s�s�o�o�o�o�o�
o�o�
o�o�
o�o�o�o�o�o�
o�o�
o�o�
o�o�o�oooos
ss
ww
{{~
~~}}}���� 
2y2yK2y� O?O?O?O?O9O9O? \\\\[[[[zzzz���������������G�{�{�{�{�w�w�w�w�s�s�s�s�o�o�o�o�k�k�k�k�k�k�k�k�k�k�k�k�k�k�k�k�k�k�k�Gk�k�k�k�k�k�k�k�k�k�k�k�k�kkkko_o_o_o_s_s_s_s_w_w_w_w_{_{_{_{_____^^^^]]]]���� 
2y2y;2y� O?O?O?O9O?O?O9 \\[[[[ZZZZzzzz���
��������
{�{�/w�w�w�s�s�s�s�o�o�o�o�k�k�k�k�k�k�k�k�k�k�k�
k�k�k�k�k�k�k�k�
k�k�/k�k�k�k�k�k�k�kkkkk_k_k_k_o_o_o_o_s_s_s_
w_w_{_{_{____
^^]]]\\���� 
2y2y2y� O?
O?O?3O9O9 <;;;:::9YYYYyyy������
����?{�{�{�w�w�w�w�s�s�s�o�o�o�k�k�k�g�g�g�g�g�g�g�g�g�g�g�g�g�g�
g�g�g�g�?g�g�g�g�g�g�g�g�g�g�gggg_g_g_g?k?k?k?k?o?o?o?s?s?s?w?w?w?
{?{???>>>====<<{�{�{�{� 
2y2y2y� O?O?O?
O9O97 ;;;:::9999YYYyyy�����������G{�{�{�w�w�w�s�s�s�o�o�o�k�k�k�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�g�Gg�g�g�g�g�g�g�g�g�gggg_g_g_g?g?g?g?k?k?k?o?o?o?s?s?s?w?w?w?{?{?{???>>>===<<<{�{�{�{� 
2y2y2y� O?
O?O?;O?O9 88XXXxxx����������G{�{�w�w�w�s�s�s�o�o�o�k�k�g�g�g�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�Gc�c�c�c�c�c�c�c�cccc_c_c?c?c?ccggkkkooosssww{{{{�{�{�{� 
2y2yK2y� O?O?O?O?O9O9O9 888XXxxx�������
��K�{�{�{�w�w�s�s�s�o�o�o�k�k�g�g�g�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�c�
c�c�Kc�c�c�c�c�c�c�c�c�cccc_c_c?c?c?ccccgggkkooosswww{{
{�{�{�{� 
2y2y2y� O?O?O?
O9O?7 ~�~�~�~�~�~�~�~�~�777WWww�������
��K�{�{�{�w�w�s�s�o�o�o�k�k�g�g�c�c�c�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�
_�_�K_�_�_�_�_�_�_�_�________?_?___^�^�b�b�f�f�f�j�j�n�n�r�r�r�v�v�z�z�
~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y2y� O?O?O?
O?O9�� ~�~�~�~�~�~�~�~�~�~�77WWWww�����������{�{�w�w�s�s�s�o�o�k�k�g�g�c�c�c�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_�_______?_?___^�^�^�^�b�b�f�f�j�j�j�n�n�r�r�v�v�z�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y�2y� O?O?O?O?O9O9O? ~�~�~�~�~�~�~�~�~�~�~�66VVvv�����������{�{�w�w�s�s�o�o�k�k�g�g�c�c�c�_�_�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[�[[[_[_[?[?[[[Z�Z�Z�Z�^�^�b�b�f�f�j�j�n�n�r�r�r�v�v�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�w�w�w�w� 
2y2y2y� O?O?O?
O?O9�� ~�~�~�~�~�~�~�~�~�~�~�~�55UUuu����������{�{�w�w�s�s�o�o�k�k�g�g�c�c�_�_�[�[�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�WWW_W_W?W?WWV�V�V�V�Z�Z�^�^�b�b�f�f�j�j�n�n�r�r�v�v�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y�2y%) O?O?O?O9O?O9O9 ~�~�~�~�~�~�~�~�~�~�~�~�~�55Uuu����������{�{�w�s�s�o�o�k�k�g�g�c�c�_�_�[�[�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�W�WWW_W_W?W?WWV�V�V�V�V�V�Z�Z�^�^�b�b�f�f�j�n�n�r�r�v�v�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y2y� O?O?O?
O?O9�� ~�~�~�~�~�~�~�~�~�~�~�~�~�~�44TTtt���������{�w�w�s�s�o�o�k�g�g�c�c�_�_�[�[�W�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�SSS_S?S?SSR�R�R�R�R�R�V�V�Z�Z�^�^�b�f�f�j�j�n�n�r�r�v�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�s�s�s�s� 
2y2y2y� O?O?O?
O9O?�� ~�~�~�~�~�~�~�~�~�~�~�~�~�~�44Ttt���������{�w�w�s�s�o�k�k�g�g�c�_�_�[�[�W�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�S�SS_S_S?S?SR�R�R�R�R�R�R�R�V�V�Z�Z�^�b�b�f�f�j�n�n�r�r�v�z�z�~�~�~�~�~�~�~�~�~�~�~�~�~�~�o{o{o{o{ 
2y2y2y� O?O?O?
O?O9�� ~x~w~v~v~u~u~t~s~�~�~�~�~�~�~�3SSs��������{�{�w�w�s�o�o�k�k�g�c�c�_�[�[�W�W�S�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�OOO_O_O?OON�N�N�N�N�N�NRRVVZ^^bbfjjnrrvvz~~~~~~~~}~}~|~{~{~z~z~y~xo{o{o{o{ 
2y2y�2y� O?O?O?O9O?O9O9 ~w~w~v~u~u~t~s~s~s~�~�~�~�~�~�33Sss�������{�{�w�s�s�o�o�k�g�g�c�_�_�[�W�W�S�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�O�OOO_O?O?ON�N�N�N�N�N�NNNRRVZZ^bbffjnnrvvz~~~~~~~~}~|~|~{~{~z~y~y~xkZkZkZkZ 
2y2y2y� O?O?O?
O?O9�� ~W~V~V~U~T~T~S~R~R~r~�~�~�~�~�~�22Rrr�������{�{�w�s�s�o�k�k�g�c�c�_�[�[�W�S�S�O�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�K�KK_K_K?KKJ�J�J�J�J�J�JJ_J_N_R_R_V_Z_^_^_b_f_f_j_n_n_r_v_v_z_~_~_~_~^~^~]~\~\~[~Z~Z~Y~X~XkZkZkZkZ 
2y2y�2y� O?O?O?O?O9O9O? ~7~6~5~5~4~3~2~2~Q~q~q~�~�~�~�~�1QQq�������{�{�w�s�s�o�k�g�g�c�_�_�[�W�W�S�O�K�K�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�GG_G?G?GF�F�F�F�F�F�FF_F_J?N?N?R?V?V?Z?^?b?b?f?j?j?n?r?r?v?z?~?~?~?~>~>~=~<~<~;~:~9~9~8~7g9g9g9g9 
2y2y2y� O?
O?O?��O?O9 ~6~6~5~4~3~3~2~1~1~Q~q~�~�~�~�~�~�11Qq�������{�{�w�s�o�o�k�g�c�c�_�[�[�W�S�O�O�K�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�G�GGG_G?GGF�F�F�F�F�FFF_F?F?J?N?R?R?V?Z?Z?^?b?f?f?j?n?r?r?v?z?~?~?~?~>~>~=~<~;~;~:~9~8~8~7g9g9g9g9 
2y2y�2y� O?O9O9O?O9O9O9 ~~~~~~~~~0~P~P~p~�~�~�~�~�0Ppp������{�w�w�s�o�k�k�g�c�_�_�[�W�S�S�O�K�G�G�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�CC_C_C?CB�B�B�B�B�B�BB_B?B?FJJNRVVZ^bbfjnnrvzz~~~~~~~~~~~~~cccc 
2y2y2y� O?O?O?
O?O9�� ~~~~~~~~~~0~P~p~p~�~�~�~�~�0Ppp������{�w�w�s�o�k�g�g�c�_�[�[�W�S�O�K�K�G�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�CC_C?C?CB�B�B�B�B�BB_B_B?BBFJNNRVZ^^bfjnnrvzz~~~~~~~~~~~~~cccc 
2y2y2y� O?O?O?
O?O9�� }�}�}�}�}�}�}�}�~~/~/~O~o~�~�~�~�~�/OOo������{�w�s�s�o�k�g�c�c�_�[�W�S�S�O�K�G�C�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�??_????>�>�>�>�>�>>_>?>=�A�E�E�I�M�Q�U�U�Y�]�a�e�i�i�m�q�u�y�y�}�}�}�}�}�}�}�}�}�}�}�}�}�^�^�^�^� 
2y2y�2y� O?O9O9O?O?O9O9 }�}�}�}�}�}�}�}�}�~~/~O~o~o~�~�~�~�/Oo������{�w�s�s�o�k�g�c�_�_�[�W�S�O�K�G�G�C�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�?�??_???>�>�>�>�>�>>_>?>?>=�=�A�E�I�M�M�Q�U�Y�]�a�a�e�i�m�q�u�y�y�}�}�}�}�}�}�}�}�}�}�}�}�}�^�^�^�^� 
2y2y2y� O?O?O?
O?O9�� }�}�}�}�}�}�}�}�}�~~~.~N~n~�~�~�~�~�.Nn������{�w�s�o�o�k�g�c�_�[�W�S�S�O�K�G�C�?�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;;;_;?;:�:�:�:�:�::_:?:9�9�=�A�A�E�I�M�Q�U�Y�Y�]�a�e�i�m�q�u�u�y�}�}�}�}�}�}�}�}�}�}�}�}�}�Z�Z�Z�Z� 
2y2y�2y� O?O?O?O9O?O9O9 }�}�}�}�}�}�}�}�}�}�~~.~N~n~�~�~�~�~�.Nn������{�w�s�o�k�k�g�c�_�[�W�S�O�K�G�G�C�?�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;�;;_;_;?;:�:�:�:�::_:?:?:9�9�9�=�A�E�I�M�Q�Q�U�Y�]�a�e�i�m�q�q�u�y�}�}�}�}�}�}�}�}�}�}�}�}�}�V�V�V�V� 
2y2y2y� O?O?O?
O?O9�� }�}�}�}�}�}�}�}�}�}�}�~~-~M~m~�~�~�~�-Mmm�����{�w�s�o�k�g�c�_�_�[�W�S�O�K�G�C�?�;�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�7�77_7?76�6�6�6�6�66_6?65�5�5�9�=�=�A�E�I�M�Q�U�Y�]�a�e�i�m�m�q�u�y�}�}�}�}�}�}�}�}�}�}�}�}�}�V�V�V�V� 
2y2y�2y� O?O9O9O?O?O9O? }�}�}�}�}�}�}�}�}�}�}�~~,~L~l~�~�~�~�,Ll�����{�w�s�o�k�g�c�_�[�W�S�O�K�K�G�C�?�;�7�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�33_3?32�2�2�2�22_2_2?21�1�1�5�9�=�A�E�I�M�Q�U�Y�]�a�a�e�i�m�q�u�y�}�}�}�}�}�}�}�}�}�}�}�}�}�R�R�R�R� 
2y2y2y� O?O?O?
O?O9�� }�}�}�}�}�}�}�}�}�}�}�}�~~,~L~l~�~�~�~�,Ll�����{�w�s�o�k�g�c�_�[�W�S�O�K�G�C�?�;�7�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�3�33_3?32�2�2�2�22_2?21�1�1�1�5�5�9�=�A�E�I�M�Q�U�Y�]�a�e�i�m�q�u�y�}�}�}�}�}�}�}�}�}�}�}�}�}�NsNsNsNs 
2y2y�2y� O?O?O?O?O9O9O9 }s}r}q}p}o}n}m}l}�}�}�}�~~+~K~k~�~�~�~�+Kk�����{�w�s�o�k�g�c�_�[�W�S�O�K�G�C�?�;�7�3�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�//_/?/.�.�.�.�.._.?.-�-�-�-�159=AEIMQUY]aeimquy}}}~}}}|}{}z}y}x}w}v}u}tNsNsNsNs 
2y2y2y� O?
O?O?��O?O9 }r}q}p}o}n}m}l}k}k}�}�}�~~+~K~k~�~�~�~�+Kk�����{�w�s�o�k�g�c�[�W�S�O�K�G�C�?�;�7�3�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�/�//_/?/.�.�.�.._.?.-�-�-�-�--15=AEIMQUY]aeimquy}}}~}}}|}{}z}y}x}v}u}t}sJRJRJRJR 
2y2y�2y� O?O9O9O?O9O9O? }R}Q}P}O}N}M}L}K}j}�}�}�}�~
~*~J~j~�~�~�
*Jj�����{�w�s�o�g�c�_�[�W�S�O�K�G�C�?�;�7�3�/�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�++?+*�*�*�*�**_*?*)�)�)�)�)-_1_5_9_=_A_E_I_M_Q_Y_]_a_e_i_m_q_u_y_}_}_}^}]}\}[}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2y� O?O?O?
O?O9�� }R}Q}O}N}M}L}K}J}J}j}�}�}�~
~*~J~j~�~�~�~�*Jj�����{�w�o�k�g�c�_�[�W�S�O�K�G�?�;�7�3�/�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�+�++_+?+*�*�*�*�**_*?)�)�)�)�))_)_-_5_9_=_A_E_I_M_Q_U_Y_]_e_i_m_q_u_y_}_}_}^}]}[}Z}Y}X}W}V}U}T}SF1F1F1F1 
2y2y2y� O?O?O?
O?O9�� }1}0}/}.}-},}+})}I}i}�}�}�}�~)~I~i~�~�~�~�	)i�����{�w�o�k�g�c�_�[�W�S�K�G�C�?�;�7�3�/�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�'�''_'?'&�&�&�&�&_&?&%�%�%�%�%%?)?-?1?5?9?=?E?I?M?Q?U?Y?]?a?e?m?q?u?y?}?}?}>}=};}:}9}8}7}6}5}4}2BBBB 
2y2y�2y� O?O?O?O?O9O9O? }}}}}}}
}	}(}H}�}�}�}�~~(~H~�~�~�~�(H�����{�s�o�k�g�c�_�[�S�O�K�G�C�?�7�3�/�+�'�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�##_#?#"�"�"�""_"?"!�!�!�!!_!?%)159=AEIQUY]aeiquy}}}}}}}}}}}}}=�=�=�=� 
2y2y�2y� O?O?O?O9O?O?O9 }}}}}}}	}}(}H}h}�}�}�~~(~H~h~�~�~�(H�����{�s�o�k�g�c�[�W�S�O�K�G�?�;�7�3�/�'�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�#�##_#?#"�"�"�""_"?!�!�!�!�!!?!%)-15=AEIMUY]aeiquy}}}}}}}}}}}}}9�9�9�9� 
2y2y2y� O?
O?O?��O9O9 |�|�|�|�|�|�|�|�}}'}g}�}�}�}�~'~G~g~�~�~�'Gg����w�s�o�k�g�_�[�W�S�O�G�C�?�;�7�/�+�'�#������������������������_?����?���_? �$�,�0�4�8�<�D�H�L�P�T�\�`�d�h�l�t�x�|�|�|�|�|�|�|�|�|�|�|�|�|�5�5�5�5� 
2y2y�2y� O?O?O?O9O9O9O? |�|�|�|�|�|�|�|�}}'}G}g}�}�}�~~'~g~�~�~�'Gg����w�s�o�k�c�_�[�W�S�K�G�C�?�7�3�/�+�#�������������������������_����_?���_� �$�(�,�4�8�<�@�D�L�P�T�X�`�d�h�l�t�x�|�|�|�|�|�|�|�|�|�|�|�|�|�1�1�1�1� 
2y2y2y� O?
O?O?��O9O9 |�|�|�|�|�|�|�|�|�}}F}f}�}�}�~~&~F~�~�~�~�&Ff����w�s�o�k�c�_�[�W�O�K�G�?�;�7�3�+�'�#�������������������������_���_?����_?�� �(�,�0�4�<�@�D�H�P�T�X�\�d�h�l�t�x�|�|�|�|�|�|�|�|�|�|�|�|�|�1�1�1�1� 
2y2y2y� O?O?O?
O9O?�� |�|�|�|�|�|�|�|�|�}}&}F}�}�}�~~&~F~f~�~�~�&Ff����w�s�o�g�c�_�[�S�O�K�C�?�;�7�/�+�'��������������������������?���_���_?��� �$�(�0�4�8�@�D�H�L�T�X�\�d�h�l�p�x�|�|�|�|�|�|�|�|�|�|�|�|�|�-k-k-k-k 
2y2y�2y%) O?O9O9O?O?O9O9 |�|�|�|�|�|�|�|�|�|�}%}E}e}�}�}�~%~E~e~�~�~�Ee����w�s�o�g�c�_�W�S�O�G�C�?�7�3�/�+�#��������������������������?���?���_����$�(�,�4�8�<�D�H�L�T�X�\�`�h�l�p�x�|�|�|�|�|�|�|�|�|�|�|�|�|�)J)J)J)J 
2y2y2y� O?O?O?
O?O9�� |�|�|�|�|�|�|�|�|�|�}}E}e}�}�}�~~E~e~�~�~�Ee����w�s�o�g�c�_�W�S�K�G�C�;�7�3�+�'�#��������������������������?���_?���_?���� �(�,�0�8�<�@�H�L�P�X�\�`�h�l�p�x�|�|�|�|�|�|�|�|�|�|�|�|�|�%)%)%)%) 
2y2y�2y� O?O?O?O9O?O9O9 |�|�|�|�|�|�|�|�|�|�}}$}D}�}�}�~~$~d~�~�~�Dd����w�s�k�g�c�[�W�S�K�G�?�;�7�/�+�'���������������������������_?���_?���_?����� �$�(�0�4�<�@�D�L�P�T�\�`�h�l�p�x�|�|�|�|�|�|�|�|�|�|�|�|�|�!!!! 
2y2y2y� O?
O?O?��O9O? |n|l|k|j|h|g|e|d|�|�|�}#}C}c}�}�~~#~C~�~�~�#c����w�s�k�g�c�[�W�O�K�G�?�;�3�/�+�#���������������������������_?��_?���_���$(,48@DHPT\`dlpx|||}|||z|y|x|v|u|s|r|q|o���� 
2y2y�2y� O?O?O?O?O9O9O9 |m|l|k|i|h|f|e|c|�|�|�}}C}c}�}�}�~#~C~�~�~�#c����w�s�k�g�_�[�W�O�K�C�?�7�3�/�'�#���������������������������_?���_���?�� (,08<DHPTX`dlpx|||}|||z|y|w|v|u|s|r|p|o���� 
2y2y�2y� O?O?O?O9O?O?O9 |M|L|J|I|G|F|D|C|b|�|�}}"}b}�}�}�~~B~b~�~�"b����w�s�k�g�_�[�S�O�G�C�;�7�3�+�'����������������������������_?
�
�
�

?
	�	�	�	_	?���___ _$_,_0_4_<_@_H_L_T_X_`_d_l_p_x_|_|_|]|\|Z|Y|W|V|T|S|Q|P|N���� 
2y2y�2y� O?O?O?O?O9O9O? |M|K|J|H|G|E|D|B|b|�|�|�}"}B}�}�}�~~B~b~�~�"b���{�w�s�k�g�_�[�S�O�G�C�;�7�/�+�#����������������������������_?
�
�
�

?
	�	�		_	���_____$_(_0_4_<_@_H_L_T_X_`_d_l_p_x_|_|^|]|\|Z|Y|W|V|T|S|Q|P|N���� 
2y2y�2y� O?O?O?O9O?O?O9 |,|+|)|(|&|%|#|"|A|�|�|�}}A}a}�}�~~!~a~�~�~�!A���{�w�o�k�c�_�W�S�K�G�?�;�3�/�'�#���������������������������_���_?���?��_???? ?(?,?4?8?@?D?L?P?X?\?d?h?p?t?|?|>|=|;|:|8|7|5|4|2|1|/|.cccc 
2y2y2y� O?
O?O?��O9O9 |,|*|)|'|&|$|#|!|A|a|�|�}}!}a}�}�~~!~a~�~�~�!A���{�w�o�k�c�_�W�S�K�G�?�7�3�+�'����������������������������_���_?���_?���????? ?$?,?0?8?@?D?L?P?X?\?d?h?p?t?|?|>|=|;|:|8|7|5|4|2|1|/|-BBBB 
2y2y�2y� O?O?O?O9O9O?O9 ||
||||||| |`|�|�|�} }`}�}�}�~ ~@~�~�~� @���{�w�o�k�c�_�W�O�K�C�?�7�3�+�#����������������������������_��_���_ � � � _ ?$,08<DHPX\dhpt|||||||||||||!!!! 
2y2y�2y� O?O?O?O?O9O9O? ||
|||||| | |@|�|�|�} }@}�}�}�~ ~@~�~�~� @���{�w�o�k�c�[�W�O�K�C�;�7�/�+�#����������������������������_��_��_ � �  _ $(04<DHPT\dhpt|||||||||||||   
2y2y2y� O?
O?O?��O?O9 ||	|||||| | |@|�|�|�} }@}�}�}�~ ~@~`~�~� @`��{�s�o�g�c�[�S�O�G�C�;�7�/�'�#����������������������������?��?��? � �  _  (04<@HLT\`hltx||||||||||||   
2y2y�2y� O?O9O9O?O9O9O9 xx	xxxxxx x x@x�x�x�y y@y`y�y�z z@z`z�z�{ { {`{�{�w�s�k�g�_�[�S�O�G�?�;�3�/�'�#����������������������������^>���~>���~> � � ~ ^  (,48@HLTX`dlpxxxxxxxxxxxxx   
2y2y2y� O?O?O?
O?O9�� xx	xxxxxx x x@x�x�x�y y@y`y�y�z z z`z�z�z�{ {@{�{�w�o�k�c�_�W�S�K�G�?�;�3�/�'�#���������������������������~^���^>���~> � � ~ ^  (,48@DLPX\dhptxxxxxxxxxxxx   
2y2y2y� O?O?O?
O?O9�� t
t	tttttt t t@t`t�t�u u u`u�u�u�v v@v�v�v�w w@w`w�s�o�g�c�[�W�O�K�C�?�7�3�+�'����������������������������}=��}]���]= � � � } =  $,08<DHPT\`hlttttttttttttt   
2y2y2y� O?
O?O?��O9O? t
t	tttttt t t@t`t�t�u u u`u�u�u�v v@v`v�v�w w w`w�s�k�g�_�[�S�O�K�C�?�7�3�+�'����������������������������]=���}]���]= � � � } =   $,08<DHLTX`dlptttttttttttt   
2y2y2y� O?O?O?
O?O9�� p
p	pppppp p p@p`p�p�q q q@q�q�q�r r@r`r�r�r�s s@s�o�k�c�_�W�S�O�G�C�;�7�/�+�'���������������������������|\���|<��|\< � � � | <   $(04<@HLPX\dhppppppppppppp   
2y2y�2y� O?O?O?O9O?O9O9 p
ppppppp p p@p`p�p�p�q q@q�q�q�r r r`r�r�r�s s@s`o�g�c�[�W�S�K�G�?�;�7�/�+�#���������������������������|<���\<���|\ � � � | <  $(048@DLPT\`hlpppppppppppp   
2y2y2y� O?O?O?
O?O9�� l
lllllll l l@l`l�l�l�m m@m`m�m�n n n@n�n�n�o o o`k`g`_`[`W`O`K`C`?`;`3`/`+`#````````bcefgijkmnpqrtuvxy{[;���{[���{[ � � � { ;  $(,48@DHPTX`dlllllllllllll   
2y2y2y� O?
O?O?��O9O? h	hhhhhhh h h@h`h�h�h�i i@i`i�i�i�j j@j`j�j�j�k k@k@c@_@W@S@O@G@C@?@7@3@/@'@#@@@@@@@@BCDFGHJKMNOQRSUVWYZZ���z:���z: � � � z :   (,48<DHLTX\dhhhhhhhhhhhhh   
2y2y2y� O?O9O9
O?O9�� h	hhhhhhh h h@h`h�h�h�i i@i`i�i�i�j j@j`j�j�j�k k g@c@[@W@S@K@G@C@;@7@3@+@'@#@@@@@@@@BCDFGHJKLNOPRSTUWXY:���Z:���Z: � � � z :   (,08<@HLPT\`dhhhhhhhhhhhh
   
2y2y�2y� O?O?O?O?O9O9O9 d	ddddddd d d@d`d�d�d�e e e`e�e�e�f f f@f�f�f�g g c _ [ W O K G ? ; 7 3 + ' #        "#$&'()+,-/01245689���yY9����Y9 � � � y 9   $,04<@DHPTX`ddddddddddddd
   
2y2y2y� O?
O?O?��O9O9 d	ddddddd d d@d`d�d�d�e e e@e�e�e�f f f@f`f�f�f�g c _ W S O K C ? ; 7 / + ' #        "#$%'()*,-.01235678���yY���yY9 � � � Y 9   $(048@DHLTX\`dddddddddddd
   
2y2y2y� O?O9O9
O?O9�� `	``````` ` `@```�`�`�a a a@a`a�a�a�b b@b`b�b�b�c _ [ W S K G C ? ; 3 / + '         	
����X8���xX8 � � � � X 8   $(,48<@HLPT\`````````````
   
2y2y2y� O?
O?O?��O9O9 ```````` ` `@```�`�`�a a a@a`a�a�a�b b b@b�b�b�b�_ [ S O K G C ; 7 3 / + #         	
���xX8���xX � � � � X 8   $(,08<@DHPTX\````````````
   
2y2y�2y� O?O?O?O9O9O?O9 \\\\\\\\ \ \ \`\�\�\�\�] ]@]`]�]�]�^ ^ ^@^`^�^�^�Z�V�R�N�J�B�>�:�6�2�*�&�"�����
��������������������������wW����W7 � � � w W 7  $(,048@DHLPX\\\\\\\\\\\\
\	   
2y2y�2y� O?O?O?O?O9O9O? \\\\\\\\ \ \ \`\�\�\�\�] ] ]`]�]�]�]�^ ^@^`^�^�^�Z�V�N�J�F�B�>�:�6�.�*�&�"�����
�������������������������wW7���wW7 � � � w W 7   $,048<@HLPTX\\\\\\\\\\\
\	   
2y2y2y� O?
O?O?��O?O9 XXXXXXXX X X X@X�X�X�X�Y Y Y@Y`Y�Y�Y�Z Z Z@Z`Z�Z�V�R�N�J�F�B�:�6�2�.�*�&�"�����
�������������������������vV6���vV6 � � � v V 6   $(,48<@DHLPXXXXXXXXXXXX
X	   
2y2y�2y%) O?O9O9O?O9O9O9 TTTTTTTT T T T@T�T�T�T�U U U@U`U�U�U�V V V@V`V�V�V�R�J�F�B�>�:�6�2�.�*�&�"�����
�������������������������U5����uU5 � � � u U 5   $(,048@DHLPTTTTTTTTTTTT
T	   
2y2y2y� O?O?O?
O?O9�� TTTTTTTT T T T@T`T�T�T�U U U@U`U�U�U�U�V V V@V`V�R�N�J�F�B�>�:�6�2�.�&�"������
������������������������uU5����u5 � � � � u U 5   $(,048<@DHLPTTTTTTTTTTT	T   
2y2y2y� O?O?O?
O?O9�� PPPPPPPP P P P@P`P�P�P�P�Q Q@Q`Q�Q�Q�Q�R R R@R`R�R�N�J�B�>�:�6�2�.�*�&�"������
������������������������tT����tT4 � � � � t T 4  $(,048<@DHLPPPPPPPPPPP
P	P   
2y2y�2y� O?O9O9O?O?O9O? PPPPPPPP P P P@P`P�P�P�P�Q Q Q@Q`Q�Q�Q�Q�R R R@R`N�J�F�B�>�:�6�2�.�*�&�"������
�����������������������tT4����tT4 � � � � t T 4   $(,048<@DHLPPPPPPPPPP
P	P   
2y2y2y� O?O?O?
O?O9�� LLLLLLLL L L L@L`L�L�L�L�M M M@M`M�M�M�M�N N N@N`J`J`F`B`>`:`6`2`.`*`&`"``````
````abcdefghijklmnopqrsSS3����sS3 � � � � s S 3   $(,048<@DHLLLLLLLLLLL
L	L   
2y2y�2y� O?O?O?O?O9O9O9 LLLLLLLL L L L@L`L�L�L�L�M M M@M`M�M�M�M�M�N N N@J`F`B`>`:`6`2`.`*`&`"```````
````abcdefghijklmmnopqrS3����sS3 � � � � � s S 3   $(,0448<@DHLLLLLLLLL
L	LL   
2y2y�2y� O?O?O?O9O?O?O9 HHHHHHHH H H H@H`H�H�H�H�I I I I@I`I�I�I�I�J J J@F@B@B@>@:@6@2@.@*@&@"@@@@@@@
@@@@ABCDEFGHHIJKLMNOPQR2����rR2 � � � � � r R 2    $(,048<@DHHHHHHHHHH
H	HH   
2y2y�2y� O?O?O?O?O9O9O9 DDDDDDDD D D D@D`D�D�D�D�D�E E E@E`E�E�E�E�E�F F F B > : 6 2 . . * & "       
    !"#$%%&'()*+,-../011����qqQ1 � � � � q Q Q 1   $(,0488<@DDDDDDDDDD
D	DD   
2y2y2y� O?O?O?
O9O?�� DDDDDDDD D D D@D`D�D�D�D�D�E E E@E`E`E�E�E�E�F F B > > : 6 2 . * & & "      
 
    !"#$$%&'()*++,-./01�����qQ11 � � � � q Q Q 1   $(,,048<@DDDDDDDDD
D	D	DD   
2y2y2y� O?O?O?
O?O9�� @@@@@@@@ @ @ @@@`@`@�@�@�@�A A A A@A`A�A�A�A�A�B B > : 6 2 2 . * & "       
     	
�����pP0 � � � � � p P 0 0    $(,0488<@@@@@@@@@
@	@@@   
2y2y�2y� O?O?O?O?O9O9O? @@@@@@@@ @ @ @@@`@`@�@�@�@�@�A A A@A`A`A�A�A�A�B > : : 6 2 . * * & "       
     	
�����pPP0 � � � � � p P 0 0   $(,,048<@@@@@@@@
@
@	@@@   
2y2y�2y� O?O9O9O?O?O9O9 <<<<<<<< < < <@<@<`<�<�<�<�<�= = = =@=`=�=�=�=�=�=�9�5�1�1�-�)�%�!�!�������	�����������������������������oO/ � � � � � o O / /   $$(,0448<<<<<<<<
<	<<<<   
2y2y�2y� O?O?O?O?O9O9O? <<<<<<<< < < <@<@<`<�<�<�<�<�= = = =@=`=`=�=�=�=�9�5�5�1�-�)�)�%�!��������	����������������������������oOO/ � � � � � � o O / /    $(,,0488<<<<<<
<
<	<<<<   
2y2y2y� O?O?O?
O?O9�� 8888888 8 8 8 8 8@8`8�8�8�8�8�8�9 9 9 9@9`9�9�9�9�9�5�1�-�-�)�%�%�!��������	���������������������������nnN.. � � � � � n n N .    $$(,0048888888
8	8	8888   
2y2y�2y� O?O?O?O?O9O9O? 8888888 8 8 8 8 8@8`8`8�8�8�8�8�9 9 9 9@9@9`9�9�9�5�1�1�-�)�)�%�!���������	���������������������������nNN. � � � � � � n n N .     $((,04488888
8
8	88888   
2y2y�2y� O?O?O?O9O?O?O9 4444444 4 4 4 4 4@4`4`4�4�4�4�4�4�5 5 5 5@5`5`5�5�5�1�-�-�)�%�!�!��������	�	��������������������������mmM- � � � � � � m M M -    $$(,,0444444
4	444444   
2y2y2y� O?
O?O?��O9O9 0000000 0 0 0 0 0@0`0`0�0�0�0�0�0�1 1 1 1@1@1`1�1�1�-�-�)�%�%�!���������	�	�������������������������llL,, � � � � � � � l L L ,     $((,000000
0	0	000000   
2y2y�2y� O?O?O?O9O9O?O9 0000000 0 0 0 0 0@0@0`0�0�0�0�0�0�0�1 1 1 1@1@1`1�-�-�)�)�%�!�!���������	�	������������������������llLL, � � � � � � l l L L ,    $$((,0000
0
0	0000000   
2y2y�2y� O?O?O?O?O9O9O? ,,,,,,, , , , , ,@,@,`,`,�,�,�,�,�,�- - - - -@-`-`-`)`)`%`%`!``````````	```````abbccdeeffghhiijkkkKK++ � � � � � � � k k K + +      $$(,,,,
,
,	,	,,,,,,,   
2y2y�2y� O?O9O9O?O?O?O9 ,,,,,,, , , , , ,@,@,`,`,�,�,�,�,�,�,�- - - - -@-`)`)`%`%`!`!``````````	```````abbccddeffgghhiijkKK++ � � � � � � � k k K + +      $$(,,
,
,	,	,,,,,,,,   
2y2y�2y� O?O?O?O?O9O9O9 ((((((( ( ( ( ( ( (@(`(`(�(�(�(�(�(�(�(�) ) ) ) )@)@%@%@!@!@@@@@@@@@@	@	@@@@@@@AABCCDDEEFFGGHHIIJJ**

 � � � � � � � j j J J * * 
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
2y2y2y� O?O?O?
O?O9�� ((((((( ( ( ( ( ( (@(@(`(`(�(�(�(�(�(�(�(�) ) ) ) %@%@!@!@@@@@@@@@@@	@	@@@@@@@AABBCCDDEEFFGGHHII**

 � � � � � � � � j j J J * * 
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
2y2y�2y� O?O?O?O?O9O9O? $$$$$$$ $ $ $ $ $ $@$@$`$`$�$�$�$�$�$�$�$�$�% % % % ! !            	 	       !!""##$$%%&&'''(())		 � � � � � � � � i i i I I ) ) 	 	 	 																 	 	$	$	$$$$$$$$$$$   
2y2y�2y� O?O?O?O9O?O?O9                    @ @ ` ` ` � � � � � � � � �! ! ! !            	 	         � � � � � � � � � h h H H ( ( (                     
2y2yy%) O?
O?O?O9O9      
    K         @ @ ` ` � � � � � � � � � �! !             	 	   
  K  � � � � � � � � � h h h H H ( (
  C                 
2y2y#2y� O?O?O?O9O9O?O9 
  K    @@```���������������������������
 � �K � � � � � � � � � � � � � � � � � � � � � � � � � � � � � g g G G G ' '
  C    
2y2y#2y� O?O?O?O?O9O9O9   G  @@@```�������������������������� � �G � � � � � � � � � � � � � � � � � � � � � � � � � � g g g G G ' ' '  ?   
2y2y#2y� O?O9O9O?O?O9O?   G   @@@```������������������������� � �G � � � � � � � � � � � � � � � � � � � � � � � � � f f f F F F & & &  ?   
2y2y2y� O?O?O?
O?O9 
  ?   @@@@```��������������������
�� � �? � � � � � � � � � � � � � � � � � � � � � � � � f f f F F F
 & &  ?   
2y2y#2y� O?O?O?O9O?O9O9    
  /@@@````���������������
����� � � �
 � �/ � � � � � � � � � � � � � � � � � � � e e e
 E E % % %   
7   
2y2y2y� O?O?O?
O?O9    G     @@@@````�������������������� � � �G � � � � � � � � � � � � � � � � � � � � � e e e e E E E E % % % %   ?    
2y2y2y� O?O9O9O?O?O9O?     
  
@@
```�����
��
��
�� � � � �
 � �
 � �
 � � � � � � � d
 d d
 D D
 $ $   	



   
2y2y2y� O?O?O?
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
2y2y2y� O?O?O?O?O9O9O9      
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
2y2y2y� O?O?O?O9O?O?O9      @@@@@@@ @ @ A A A B B B B " " "  
   
2y2y2y� O?O?O?O?O9O9O?  "     @@@@@@@& @ @ A A A B B B B " " "&      
2y2y2y� O?O?O?
O?O9  &         .       ! ! ! ! .          
2y2y2y� O?O?O?O9O?O9O9  2  
     F    
 ! ! ! F  
       
2y2y2y� O?O?O?
O?O9�  
2y2y2y� O?O?O?
O?O9�  
2y2y2y� O?O9O9O?O?O9O9�  
2y2y2y� O?O?O?
O?O9��O9O9
O9NyNyNyNy2y2y2y2y� O?O?O?
O9O?�O9O9
NyO9NyNyO9NyNyNy2y2y2y� O?O?O?
O?O9�O9O9O9NyNyNy2y2y2y� O?O?O?O9O?��O9O9
NyO9NyNyO9NyNyNy2y2y2y� O?O?O?O9O9O?O9�O9O9O9NyO9O9
NyO9NyNy
2yNy2y2y1���R     �   �   
  D0	�/!0�#
  U
 d�   "       �   K   
  E
  2
  9�   
  B�
  B
  C�[         [            �  ��   wna)\�TcT�T�]�j�   {Zijd�dcdc`c\cXcTcPcHbL�j� #v�h�h�h�hchcdc`c`c\cTcPcHc@bU�   '  rRh�l�l�l�l�lchcdcdc\cXcTcLcDB<BI� ov�l�p�p�p�dc8B!��B !DBXcPcHB@B4BI�        {Zl�p�t�ykUB=�kZ{�{�s�Z�� !LBHb@B8B,AZ�      mJp�t�y�Z1�g9���#�=� HbDB8B0B(cs�  {8h�p�t�q�%)o{��s�=�-k1�R���1�(!DB<B0B$!N1  rl�p�t�MJg9��cc8BPbHB !%){�w� <B8B0A$!9J  i)l�p�t�A����%e�t�l�h�d�(!9��%),A8B0A(!(�  d�l�p�t�J1��g9@�y'p�l�h�`�XbB�B !8A0A$!B  `�h�l�p�B��JRDBp�l�h�Xb@BTb!{�JR!4A,A$!!  \�d�l�l� �{��Ns0!l�h�dbDB!B!�5�$!0A,A !B  ]`�d�l�$!Z��w��Lbd�`bXbccg9�! !0A(! ! �  e�\bd�d�Lb���Ns `bXbTb!Z���JR   $!!-  n�Tb\b`�d�$!=��%)@AXbTbPb4A1�{�Ns� ! !!E�    T�TbXb\bXb ! TbPbLbHb<A!� $!(!$!!Ak9    n�LbPbTbTbTb0ADAPbLbDBDB<A(!,!0A(!$!!!E�	 ^Hb
LbLbHbHADA@A<A8A4A0!,!$!!!-  	 +  Q�@ADbHADADA@A<A8A4A0A,A(!$!! $�s�   #U�8A<A<A<A8A8A4A0A,!(!$!! -(s�   b�8�0A0A0A,A,A(!$! ! AE� w�V1AJ0�$b !A$�1(E�k9            �  ��   {ni)`�XcT�T�]�j�   {Zmjh�h�hchc`cXcTcPcHbL�j� #v�p�p�l�l�pclcdc`c\cXcPcHB@BU�   '  vRp�t�x�t�t�p�lchcdc`c\cTcPBHB<!Ik �z�p�t�x�|�x�x�t�pchcdc`c\cXcTBHB@!4 I�        {Zp�t�x�|�|�|�|�x�p�lchcdc`c\cTBLBD!8 ( Zs      qJt�x�}}�}�}0B,�,� cBBB!! 0 < 0 $Bs�  {8l�t�|�}j~�~�}�$�kZs�o{
kZkZ'o{)J( < 0   J1  rp�t�|�}j~1~0}I=�{���'�5�( < 0 $ 1)  m)p�x�x�|�}(y(|�F{���'�1�( < 0 $  �  d�l�t�x�x�x�x�x�I�{����-k( < 0 $ !  `�l�p�t�x�\bT�\�1JZ�Z�V�w�����)J$ 8 ,      `�h�l�p�p�B5�JRF1B5��F1����)J$ 4 ,     ad�h�l�l� c
����JR=�����)J  0 (  c  e�\bd�d�d�cw�����F1B����-k , $  )  n�Tb\b`�`bcw�����F1!B9�5�9�c (   A�    T�TbXb\bbw�����JR , ( (    $     k9    n�LbPbTbBs�{�{�{�{�F1( D @ 8 0 (     E�	 +]�HALA �����c( < 4 0 (     (�  	 +  Q�@AD@D D D D @ < 8 0 , (      �s�   Q�4 
8 8 8 4 , ( $    )s{   ^s0B, , , ( $      !A� s{R19)(�! !c)A�g9            
 M=[   

 �
 ~�
 t
 e�
 Ki
 #�      !PaintBoxMorph classPool at: #Prototype put: (SmartRefStream scannedObject).!