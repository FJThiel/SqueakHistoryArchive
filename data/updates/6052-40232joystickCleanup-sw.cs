'From Squeakland 3.8.5976 of 10 August 2004 [latest update: #229] on 11 August 2004 at 6:22:43 pm'!"Change Set:		joystickCleanup-swDate:			11 August 2004Author:			Scott WallaceNo longer puts up magic halos, which interfered with operation of the knob, on JoystickMorph.Joystick's menu now uses self-updating-menu-items to govern auto-center and track-real-joystick.Joystick's menu now includes an item letting the user see and change the reall-joystick-number."!!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 20:00'!addCustomMenuItems: aCustomMenu hand: aHandMorph	"Add custom items to the menu"	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	aCustomMenu addLine.	aCustomMenu add: 'set X range' translated action: #setXRange.	aCustomMenu add: 'set Y range' translated action: #setYRange.	aCustomMenu addLine.	aCustomMenu addUpdating: #autoCenterString target: self action: #toggleAutoCenter.	aCustomMenu balloonTextForLastItem: 'When auto-center is on, every time you let go of the Joystick knob, it springs back to the neutral position at the center of the device' translated. 	aCustomMenu addUpdating: #realJoystickString target: self action: #toggleRealJoystick.	aCustomMenu balloonTextForLastItem: 'Governs whether this joystick should track the motions of a real, physical joystick attached to the computer.' translated. 	aCustomMenu addUpdating: #joystickNumberString enablementSelector: #realJoystickInUse target: self selector: #chooseJoystickNumber argumentList: #().	aCustomMenu balloonTextForLastItem: 'Choose which physical device is associated with the joystick.' translated! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 18:30'!autoCenterString	"Answer a string characterizing whether or not I have auto-center on"	^ (autoCenter == true	ifTrue: ['<yes>'] ifFalse: ['<no>']), ('auto-center' translated)! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 20:22'!chooseJoystickNumber	"Allow the user to select a joystick number"	| result aNumber str |	str _ self lastRealJoystickIndex asString.	result _ FillInTheBlank request: 'Joystick device number (currently ' translated, str, ')' initialAnswer: str.	[aNumber _ result asNumber] on: Error do: [:err | ^ self beep].	(aNumber > 0 and: [aNumber <= 32]) "???" ifFalse: [^ self beep].	realJoystickIndex _ aNumber.	self setProperty: #lastRealJoystickIndex toValue: aNumber.	self startStepping			! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 20:20'!joystickNumberString	"Answer a string characterizing the joystick number"	^ 'set real joystick number (now ' translated, (self lastRealJoystickIndex asString, ')')! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 20:08'!lastRealJoystickIndex	"Answer the last remembered real joystick index.  Initialize it to 1 if need be"	^ self valueOfProperty: #lastRealJoystickIndex ifAbsentPut: [1] ! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 19:57'!realJoystickInUse	"Answer whether a real joystick is in use"	^ realJoystickIndex notNil! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 4/29/2004 18:29'!realJoystickString	"Answer a string characterizing whether or not I am currenty tracking a real joystick"	^ (realJoystickIndex ifNil: ['<no>'] ifNotNil: ['<yes>']), ('track real joystick' translated)! !!JoystickMorph methodsFor: 'menu' stamp: 'sw 8/11/2004 18:15'!toggleRealJoystick	"Toggle whether or not one is using a real joystick"	realJoystickIndex		ifNil:			[realJoystickIndex _ self valueOfProperty: #lastRealJoystickIndex ifAbsentPut: [1].			self startStepping]		ifNotNil:			[self stopTrackingJoystick]! !!JoystickMorph methodsFor: 'halos and balloon help' stamp: 'sw 8/11/2004 18:10'!isLikelyRecipientForMouseOverHalos	"The automatic mouseover halos interere with the proper functioning of the joystick's knob"	^ false! !!JoystickMorph reorganize!('accessing' amount angle leftRight upDown)('event handling' handlesMouseDown: mouseMove: mouseUp:)('initialization' initialize)('menu' addCustomMenuItems:hand: autoCenterString chooseJoystickNumber joystickNumberString lastRealJoystickIndex realJoystickInUse realJoystickString setXRange setYRange stopTrackingJoystick toggleAutoCenter toggleRealJoystick trackRealJoystick)('other' moveHandleToCenter)('parts bin' initializeToStandAlone)('stepping and presenter' step stepTime)('stepping')('halos and balloon help' isLikelyRecipientForMouseOverHalos)!