'From Squeak 2.0 of May 22, 1998 on 28 June 1998 at 7:10:06 pm'!"Change Set:		jhmMiscDate:			28 June 1998Author:			John MaloneyNumerous little fixes and improvements:  a. support for driving JoystickMorphs from real joysticks  b. menus for starting infections, etc in BouncingAtomsMorph  c. reset button color when acting on buttonDown  d. temporary fix for st-st-stuttering sound on BookMorph page transitions  e. added 'timeToRun' method to BlockContext for convenient timing  f. eliminate obsolete mouseEnter/mouseLeave protocol (new versions take an argument)"!SketchMorph subclass: #JoystickMorph	instanceVariableNames: 'handleMorph xScale yScale radiusScale lastAngle autoCenter realJoystickIndex lastRealJoystickValue '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Widgets'!!BlockContext methodsFor: 'evaluating' stamp: 'jm 6/3/1998 14:25'!timeToRun	"Answer the number of milliseconds taken to execute this block."	^ Time millisecondsToRun: self! !!BouncingAtomsMorph reorganize!('initialization' initialize)('menu' addCustomMenuItems:hand: setAtomCount startInfection)('stepping' step stepTime)('other' addAtoms: addMorphFront: collisionPairs drawOn: invalidRect: showInfectionHistory: transmitInfection)!!BouncingAtomsMorph methodsFor: 'menu' stamp: 'jm 6/28/1998 18:17'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'startInfection' action: #startInfection.	aCustomMenu add: 'set atom count' action: #setAtomCount.	aCustomMenu add: 'show infection history' action: #showInfectionHistory:.! !!BouncingAtomsMorph methodsFor: 'menu' stamp: 'jm 6/28/1998 18:04'!setAtomCount	| countString count |	countString _ FillInTheBlank		request: 'Number of atoms?'		initialAnswer: self submorphCount printString.	countString isEmpty ifTrue: [^ self].	count _ Integer readFrom: (ReadStream on: countString).	self removeAllMorphs.	self addAtoms: count.! !!BouncingAtomsMorph methodsFor: 'stepping' stamp: 'jm 6/28/1998 18:10'!stepTime	"As fast as possible."	^ 0! !!BouncingAtomsMorph methodsFor: 'other' stamp: 'jm 6/28/1998 18:10'!addAtoms: n	"Add a bunch of new atoms."	| a |	n timesRepeat: [		a _ AtomMorph new.		a randomPositionIn: bounds maxVelocity: 10.		self addMorph: a].	self stopStepping.! !!BouncingAtomsMorph methodsFor: 'other' stamp: 'jm 6/28/1998 18:31'!showInfectionHistory: evt	"Place a graph of the infection history in the world."	| graph |	infectionHistory isEmpty ifTrue: [^ self].	graph _ GraphMorph new data: infectionHistory.	graph extent: ((infectionHistory size + (2 * graph borderWidth) + 5)@(infectionHistory last max: 50)).	evt hand attachMorph: graph.! !!BouncingAtomsMorph methodsFor: 'other' stamp: 'jm 6/28/1998 18:20'!transmitInfection	| infected count |	self collisionPairs do: [:pair |		infected _ false.		pair do: [:atom | atom infected ifTrue: [infected _ true]].		infected			ifTrue: [pair do: [:atom | atom infected: true]]].	count _ 0.	self submorphsDo: [:m | m infected ifTrue: [count _ count + 1]].	infectionHistory addLast: count.	count = submorphs size ifTrue: [		transmitInfection _ false.		self stopStepping].! !!Color methodsFor: 'transformations' stamp: 'jm 6/25/1998 10:12'!darker	"Answer a darker shade of this color."	^ self mixed: 0.8333 with: Color black! !!Color methodsFor: 'transformations' stamp: 'jm 6/17/1998 11:23'!lighter	"Answer a lighter shade of this color."	^ self mixed: 0.8333 with: Color white! !!Form methodsFor: 'transitions' stamp: 'jm 6/18/1998 12:57'!wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock	| i clipRect t rectOrList waitTime |	i _ 0.	clipRect _ topLeft extent: otherImage extent.	clipBox ifNotNil: [clipRect _ clipRect intersect: clipBox].	[rectOrList _ rectForIndexBlock value: (i _ i + 1).	 rectOrList == nil]		whileFalse: [			t _ Time millisecondClockValue.			rectOrList asOrderedCollection do: [:r |				self copyBits: r from: otherImage at: topLeft + r topLeft					clippingBox: clipRect rule: Form over fillColor: nil].			Display forceDisplayUpdate.			waitTime _ 3 - (Time millisecondClockValue - t).			waitTime > 0 ifTrue:				["(Delay forMilliseconds: waitTime) wait"]].! !!HandMorph methodsFor: 'event dispatching' stamp: 'jm 6/17/1998 08:57'!updateMouseDownTransform	"To help with, eg, autoscrolling"	mouseDownMorph		ifNil: [eventTransform _ MorphicTransform identity]		ifNotNil: [eventTransform _ mouseDownMorph transformFrom: self].! !!JoystickMorph reorganize!('initialization' initialize)('accessing' amount angle leftRight parts slotNamesAndTypesForBank: upDown)('other' handlesMouseDown: mouseMove: mouseUp: moveHandleToCenter)('stepping' step stepTime)('menu' addCustomMenuItems:hand: setXRange setYRange stopTrackingJoystick toggleAutoCenter trackRealJoystick)!!JoystickMorph methodsFor: 'stepping' stamp: 'jm 6/22/1998 18:05'!step	"Track the real joystick whose index is realJoystickIndex."	"Details:	  a. stop stepping if realJoystickIndex is nil; that means we're not tracking a joystick	  b. [-joyMax..joyMax] is nominal range of joystick in both X and Y	  c. [-threshold..threshold] is considered 0 to compensate for poor joystick centering"	| threshold joyMax joyPt m mCenter r scaledPt |	realJoystickIndex ifNil: [^ self stopStepping].	threshold _ 30.	joyMax _ 350.	joyPt _ Sensor joystickXY: realJoystickIndex.	joyPt x abs < threshold ifTrue: [joyPt _ 0@joyPt y].	joyPt y abs < threshold ifTrue: [joyPt _ joyPt x@0].	lastRealJoystickValue = joyPt ifTrue: [^ self].	lastRealJoystickValue _ joyPt.	m _ handleMorph.	mCenter _ m center.	r _ m owner innerBounds insetBy:		((mCenter - m fullBounds origin) corner: (m fullBounds corner - mCenter)).	scaledPt _ r center + ((r extent * joyPt) / (joyMax * 2)) truncated.	m position: (scaledPt adhereTo: r) - (m extent // 2).! !!JoystickMorph methodsFor: 'stepping' stamp: 'jm 6/22/1998 18:00'!stepTime	^ 0! !!JoystickMorph methodsFor: 'menu' stamp: 'jm 6/22/1998 17:19'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu add: 'set X range' action: #setXRange.	aCustomMenu add: 'set Y range' action: #setYRange.	autoCenter		ifTrue: [aCustomMenu add: 'turn auto-center off' action: #toggleAutoCenter]		ifFalse: [aCustomMenu add: 'turn auto-center on' action: #toggleAutoCenter].	realJoystickIndex		ifNil: [aCustomMenu add: 'track real joystick' action: #trackRealJoystick]		ifNotNil: [aCustomMenu add: 'stop tracking joystick' action: #stopTrackingJoystick].! !!JoystickMorph methodsFor: 'menu' stamp: 'jm 6/22/1998 17:24'!stopTrackingJoystick	realJoystickIndex _ nil.	self stopStepping.! !!JoystickMorph methodsFor: 'menu' stamp: 'jm 6/22/1998 17:59'!trackRealJoystick	| s |	s _ FillInTheBlank		request: 'Number of joystick to track?'		initialAnswer: '1'.	s isEmpty ifTrue: [^ self].	realJoystickIndex _ Number readFromString: s.	self startStepping.! !!PasteUpMorph methodsFor: 'object fileIn' stamp: 'jm 6/18/1998 10:31'!convertbosfcepcbbfgccpmcpbttloiairfidcuw0: varDict bosfcepcbbfgccpmcpbttloiairfidcu0: smartRefStrm	"These variables are automatically stored into the new instance ('presenter' 'model' 'cursor' 'padding' 'backgroundMorph' 'turtleTrailsForm' 'turtlePen' 'lastTurtlePositions' 'openToDragNDrop' 'isPartsBin' 'autoLineLayout' 'indicateCursor' 'resizeToFit' 'fileName' 'isStackLike' 'dataInstances' 'currentDataInstance' 'userFrameRectangle')."	"Incoming morphs have extra inst var: 'wantsMouseOverHalos'"! !!ScrollPane methodsFor: 'initialization' stamp: 'jm 6/17/1998 11:57'!fullCopy	| copy |	self mouseEnter: nil.		 "Make sure scrollBar is in morphic structure"	copy _ super fullCopy.		"So that references are updated properly"	self mouseLeave: nil.	^ copy mouseLeave: nil! !!SimpleButtonMorph methodsFor: 'events' stamp: 'jm 6/17/1998 09:01'!mouseMove: evt	actWhen == #buttonDown ifTrue: [^ self].	(self containsPoint: evt cursorPoint)		ifTrue: [self color: (oldColor mixed: 1/2 with: Color white).				(actWhen == #whilePressed and: [evt anyButtonPressed])					 ifTrue: [self doButtonAction]]		ifFalse: [self color: oldColor].! !!SimpleClientSocket class methodsFor: 'other examples' stamp: 'jm 6/8/1998 16:05'!httpTestHost: hostName port: port url: url	"This test fetches a URL from the given host and port."	"SimpleClientSocket httpTestHost: 'www.disney.com' port: 80 url: '/'"	"Tests URL fetch through a local HTTP proxie server:		(SimpleClientSocket			httpTestHost: '127.0.0.1'			port: 8080			url: 'HTTP://www.exploratorium.edu/index.html')"	| hostAddr s result buf bytes totalBytes t |	Transcript cr; show: 'starting http test'; cr.	Socket initializeNetwork.	hostAddr _ NetNameResolver addressForName: hostName timeout: 10.	hostAddr = nil ifTrue: [^ self inform: 'Could not find an address for ', hostName].	s _ SimpleClientSocket new.	Transcript show: '---------- Connecting ----------'; cr.	s connectTo: hostAddr port: port.	s waitForConnectionUntil: "self standardDeadline" (Socket deadlineSecs: 10).	(s isConnected) ifFalse: [		s destroy.		^ self inform: 'could not connect'].	Transcript show: 'connection open; waiting for data'; cr.	s sendCommand: 'GET ', url, ' HTTP/1.0'.	s sendCommand: 'User-Agent: Squeak 1.19'.	s sendCommand: 'ACCEPT: text/html'.	"always accept plain text"	s sendCommand: 'ACCEPT: application/octet-stream'.  "also accept binary data"	s sendCommand: ''.  "blank line"	result _ WriteStream on: (String new: 10000).	buf _ String new: 10000.	totalBytes _ 0.	t _ Time millisecondsToRun: [		[s isConnected] whileTrue: [			s waitForDataUntil: (Socket deadlineSecs: 5).			bytes _ s receiveDataInto: buf.			1 to: bytes do: [:i | result nextPut: (buf at: i)].			totalBytes _ totalBytes + bytes.			Transcript show: totalBytes printString, ' bytes received'; cr]].	s destroy.	Transcript show: '---------- Connection Closed ----------'; cr; endEntry.	Transcript show: 'http test done; ', totalBytes printString, ' bytes read in '.	Transcript show: ((t / 1000.0) roundTo: 0.01) printString, ' seconds'; cr.	Transcript show: ((totalBytes asFloat / t) roundTo: 0.01) printString, ' kBytes/sec'; cr.	Transcript endEntry.	(StringHolder new contents: (result contents))		openLabel: 'HTTP Test Result: URL Contents'.! !!SystemWindow methodsFor: 'resize/collapse' stamp: 'jm 6/17/1998 11:55'!mouseLeaveEvent: event fromPane: pane	"For backward compatibility only.  Not used by any newly created window"	(pane isKindOf: ScrollPane) ifTrue: [pane mouseLeave: event].! !!TransformMorph methodsFor: 'drawing' stamp: 'jm 6/17/1998 12:22'!fullDrawOn: aCanvas	"Overridden to clip submorph drawing to my bounds,	and to translate, rotate and scale as appropriate."	| clippingCanvas sourceQuad imageForm imageQuad warp innerRect |	(aCanvas isVisible: self bounds) ifFalse: [^ self].	self drawOn: aCanvas.	transform isPureTranslation		ifTrue:		[clippingCanvas _ aCanvas copyOffset: transform offset negated truncated									clipRect: self innerBounds.		submorphs reverseDo: [:m | m fullDrawOn: clippingCanvas]]		ifFalse:		[innerRect _ self innerBounds.		sourceQuad _ transform sourceQuadFor: innerRect.		submorphs reverseDo:			[:m | imageForm _ m imageForm.			imageQuad _ sourceQuad collect: [:p | p - imageForm offset].			warp _ aCanvas warpFrom: imageQuad toRect: innerRect.			warp cellSize: smoothing;  "installs a colormap if smoothing > 1"				sourceForm: imageForm;				warpBits]]	! !BouncingAtomsMorph removeSelector: #setGermCount!ScrollPane removeSelector: #mouseLeave!ScrollPane removeSelector: #mouseEnter!SimpleClientSocket class removeSelector: #httpFetchFromHost:url:!