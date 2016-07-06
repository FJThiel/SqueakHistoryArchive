'From Squeak3.3alpha of 18 January 2002 [latest update: #4981] on 6 October 2002 at 2:23:37 am'!"Change Set:		arrowhead-swDate:			5 October 2002Author:			Scott WallaceMakes the metrics of arrowheads used on pen trails configurable, as per request from Alan:� Adds an item in the 'appearance' menu that lets you set the global default size for pen-trail arrowheads.� Adds an item in the 'extras' branch of the morph halo menu that lets you override the size of pen-trail arrowheads used with that particular morph.(Later, a graphical interface for specifying the arrowhead metrics would be nice,"!!TheWorldMenu commentStamp: 'sw 10/5/2002 00:44' prior: 0!Instances of TheWorldMenu serve to present the primary Squeak menu obtained by clicking on open desktop, which is variously spoken of as the "screen menu", the "desktop menu", or the "world menu".myProject is the Project I pertain to.myWorld is the world, a PasteUpMorph, that I pertain to.myHand is the hand that invoked the menu.!!Morph methodsFor: 'menus' stamp: 'sw 10/5/2002 00:34'!adMiscExtrasTo: aMenu	"Add a submenu of miscellaneous extra items to the menu."	| realOwner realMorph subMenu |	subMenu _ MenuMorph new defaultTarget: self.	(self isWorldMorph not and: [(self renderedMorph isKindOf: SystemWindow) not])		ifTrue: [subMenu add: 'put in a window' action: #embedInWindow].	self isWorldMorph ifFalse:		[subMenu add: 'adhere to edge...' action: #adhereToEdge.		subMenu addLine].	realOwner _ (realMorph _ self topRendererOrSelf) owner.	(realOwner isKindOf: TextPlusPasteUpMorph) ifTrue:		[subMenu add: 'GeeMail stuff...' subMenu: (realOwner textPlusMenuFor: realMorph)].	self affiliatedSelector ifNotNil:		[subMenu add: 'open a messenger' action: #openMessenger.		subMenu balloonTextForLastItem: 'Open a Messenger on the actual method call used when the button action of this object is triggered.'.		subMenu addLine].	subMenu		add: 'add mouse up action' action: #addMouseUpAction;		add: 'remove mouse up action' action: #removeMouseUpAction;		add: 'hand me tiles to fire this button' action: #handMeTilesToFire.	subMenu addLine.	subMenu add: 'arrowheads on pen trails...' action: #setArrowheads.	subMenu addLine.	subMenu defaultTarget: self topRendererOrSelf.	subMenu add: 'draw new path' action: #definePath.	subMenu add: 'follow existing path' action: #followPath.	subMenu add: 'delete existing path' action: #deletePath.	subMenu addLine.	self addGenieMenuItems: subMenu hand: ActiveHand.	aMenu add: 'extras...' subMenu: subMenu! !!Morph methodsFor: 'menus' stamp: 'sw 10/5/2002 01:48'!setArrowheads	"Let the user edit the size of arrowheads for this object"	| aParameter result  |	aParameter _ self renderedMorph valueOfProperty:  #arrowSpec ifAbsent:		[Preferences parameterAt: #arrowSpec ifAbsent: [5 @ 4]].	result _ Utilities obtainArrowheadFor: 'Head size for arrowheads: ' defaultValue: aParameter asString.	result ifNotNil:			[self renderedMorph  setProperty: #arrowSpec toValue: result]		ifNil:			[self beep]! !!PasteUpMorph methodsFor: 'pen' stamp: 'sw 10/4/2002 17:35'!drawPenTrailFor: aMorph from: oldPoint to: targetPoint	"Draw a pen trail for aMorph, using its pen state (the pen is assumed to be down)."	"The turtleTrailsForm is created on demand when the first pen is put down and removed (to save space) when turtle trails are cleared."	| origin mPenSize offset turtleTrailsDelta newPoint aPlayer |	turtleTrailsDelta _ self valueOfProperty: #turtleTrailsDelta ifAbsent:[0@0].	newPoint _ targetPoint - turtleTrailsDelta.	oldPoint = newPoint ifTrue: [^ self].	self createOrResizeTrailsForm.	origin _ self topLeft.	mPenSize _ aMorph getPenSize.	turtlePen sourceForm width ~= mPenSize		ifTrue: [turtlePen squareNib: mPenSize].	offset _ (mPenSize // 2)@(mPenSize // 2).	turtlePen color: aMorph getPenColor.	turtlePen drawFrom: (oldPoint - origin - offset) asIntegerPoint				to: (newPoint - origin - offset) asIntegerPoint.	((aPlayer _ aMorph player) getPenArrowheads and: [oldPoint ~= newPoint]) ifTrue:		[turtlePen			arrowHeadFrom: (oldPoint - origin - offset) 			to: (newPoint - origin - offset)			forPlayer: aPlayer].	self invalidRect: ((oldPoint rect: newPoint) expandBy: mPenSize)! !!Pen methodsFor: 'operations' stamp: 'sw 10/5/2002 03:17'!arrowHead	"Put an arrowhead on the previous pen stroke"	" | pen | pen _ Pen new. 20 timesRepeat: [pen turn: 360//20; go: 20; arrowHead]."	penDown ifTrue:		[self arrowHeadFrom: (direction degreeCos @ direction degreeSin) * -40 + location 			to: location			arrowSpec: (Preferences parameterAt: #arrowSpec ifAbsent: [5 @ 4])]! !!Pen methodsFor: 'operations' stamp: 'sw 10/5/2002 02:29'!arrowHeadForArrowSpec: anArrowSpec	"Put an arrowhead on the previous pen stroke""	 | pen aPoint |	aPoint _ Point fromUser.	pen _ Pen new.	20 timesRepeat: [pen turn: 360//20; go: 20; arrowHeadForArrowSpec: aPoint]."	penDown ifTrue:		[self arrowHeadFrom: (direction degreeCos @ direction degreeSin) * -40 + location 			to: location			arrowSpec: anArrowSpec]! !!Pen methodsFor: 'operations' stamp: 'sw 10/5/2002 02:25'!arrowHeadFrom: prevPt to: newPt arrowSpec: anArrowSpec	"Put an arrowhead on the pen stroke from oldPt to newPt"	| pm af myColor finalPt delta |	myColor _ self color.	delta _ newPt - prevPt.	delta r <= 2 "pixels" ifTrue: [^ self].	finalPt _ newPt + (Point r: sourceForm width degrees: delta degrees).	"in same direction"	pm _ PolygonMorph vertices: (Array with: prevPt asIntegerPoint with: finalPt asIntegerPoint)  		color: myColor  "not used"		borderWidth: sourceForm width borderColor: myColor.	pm makeOpen; makeForwardArrow.	anArrowSpec ifNotNil: [pm arrowSpec: anArrowSpec].	af _ pm arrowForms first.	"render it onto the destForm"	(FormCanvas on: destForm "Display") stencil: af at: af offset + (1@1)		color: myColor! !!Pen methodsFor: 'operations' stamp: 'sw 10/5/2002 02:11'!arrowHeadFrom: prevPt to: newPt forPlayer: aPlayer	"Put an arrowhead on the pen stroke from oldPt to newPt"		| aSpec |	(aPlayer notNil and: [(aSpec _ aPlayer costume renderedMorph valueOfProperty: #arrowSpec) notNil]) 		ifFalse:			[aSpec _ Preferences parameterAt: #arrowSpec "may well be nil"].	self arrowHeadFrom: prevPt to: newPt arrowSpec: aSpec! !!Player methodsFor: 'pen' stamp: 'sw 10/4/2002 13:14'!clearTurtleTrails	"Clear all turtle trails within my costume, presumed to be a playfield"	self costume renderedMorph clearTurtleTrails! !!Preferences class methodsFor: 'misc' stamp: 'sw 10/5/2002 01:55'!setArrowheads	"Let the user edit the size of arrowheads"	| aParameter result  |	aParameter _ self parameterAt: #arrowSpec ifAbsent: [5 @ 4].	result _ Utilities obtainArrowheadFor: 'Default size of arrowheads on pen trails ' defaultValue: aParameter asString.	result ifNotNil:			[self setParameter: #arrowSpec to: result]		ifNil:			[self beep]! !!TheWorldMenu methodsFor: 'construction' stamp: 'sw 10/5/2002 00:45'!appearanceMenu	"Build the appearance menu for the world."	| screenCtrl |	screenCtrl _ ScreenController new.	^self fillIn: (self menu: 'appearance...') from: {		{'preferences...' . { Preferences . #openFactoredPanel} . 'Opens a "Preferences Panel" which allows you to alter many settings' } .		{'choose theme...' . { Preferences . #offerThemesMenu} . 'Presents you with a menu of themes; each item''s balloon-help will tell you about the theme.  If you choose a theme, many different preferences that come along with that theme are set at the same time; you can subsequently change any settings by using a Preferences Panel'} .		nil .		{'window colors...' . { Preferences . #windowSpecificationPanel} . 'Lets you specify colors for standard system windows.'}.		{'system fonts...' . { self . #standardFontDo} . 'Choose the standard fonts to use for code, lists, menus, window titles, etc.'}.		{'text highlight color...' . { Preferences . #chooseTextHighlightColor} . 'Choose which color should be used for text highlighting in Morphic.'}.		{'insertion point color...' . { Preferences . #chooseInsertionPointColor} . 'Choose which color to use for the text insertion point in Morphic.'}.		{'keyboard focus color' . { Preferences . #chooseKeyboardFocusColor} . 'Choose which color to use for highlighting which pane has the keyboard focus'}.		nil.		{#menuColorString . { Preferences . #toggleMenuColorPolicy} . 'Governs whether menu colors should be derived from the desktop color.'}.		{#roundedCornersString . { Preferences . #toggleRoundedCorners} . 'Governs whether morphic windows and menus should have rounded corners.'}.		nil.		{'full screen on' . { screenCtrl . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.		{'full screen off' . { screenCtrl . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.		nil.		{'set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.		{'set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.		{'set gradient color...' . {self. #setGradientColor} . 'choose second color to use as gradient for desktop background.'}.		{'use texture background' . { #myWorld . #setStandardTexture} . 'apply a graph-paper-like texture background to the desktop.'}.		nil.		{'clear turtle trails from desktop' . { #myWorld . #clearTurtleTrails} . 'remove any pigment laid down on the desktop by objects moving with their pens down.'}.		{'pen-trail arrowhead size...' . { Preferences. #setArrowheads} . 'choose the shape to be used in arrowheads on pen trails.'}.	}! !!Utilities class methodsFor: 'user interface' stamp: 'sw 10/5/2002 03:13'!obtainArrowheadFor: aPrompt defaultValue: defaultPoint	"Allow the user to supply a point to serve as an arrowhead size.  Answer nil if we fail to get a good point"	| result  |	result _ FillInTheBlank request: aPrompt initialAnswer: defaultPoint asString.	result isEmptyOrNil ifTrue: [^ nil].	^ Utilities pointOrNilFrom: result"Utilities obtainArrowheadFor: 'give me a point:' defaultValue: (6@7)"! !!Utilities class methodsFor: 'user interface' stamp: 'sw 10/6/2002 02:23'!pointOrNilFrom: aString	"Lightly parse aString, looking for either of two syntaxes -- integers separated either by a # character or by a comma, as illutrated by the following, which are equivalent:		2 @ 3		2 , 3If the input fits one of these patterns, return the Point object represented; anything else will result in nil being returned."	| tokens |	tokens _ Scanner new scanTokens: aString.	^ (tokens size = 3 and: [tokens first isKindOf: SmallInteger] and: [#(@ ,) includes: tokens second] and: [tokens third isKindOf: SmallInteger])		ifTrue:			[tokens first @ tokens third]		ifFalse:			[nil]"Utilities pointOrNilFrom: '23 @ 19'Utilities pointOrNilFrom: '23, 19'Utilities pointOrNilFrom: 'X23 @ 19'Utilities pointOrNilFrom: '23 @ 19 @ 7'"! !Pen removeSelector: #arrowHeadFrom:to:!