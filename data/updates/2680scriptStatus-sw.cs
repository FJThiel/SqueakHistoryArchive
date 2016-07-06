'From Squeak2.9alpha of 5 August 2000 [latest update: #2676] on 21 September 2000 at 11:04:41 pm'!"Change Set:		scriptStatus-swDate:			21 September 2000Author:			Scott WallacePuts script-status controls in the Viewer as well as the Scriptor.  Thus, you can quickly see, and change, the status of all the scripts that an object has all at once, by looking in the 'scripts' category of its Viewer.Adds little 'one-touch' controls to put any script into paused or ticking status quickly, to the script-status controls as seen both in Viewer and Scriptor."!AlignmentMorph subclass: #ScriptStatusControl	instanceVariableNames: 'tickPauseWrapper tickPauseButtonsShowing scriptInstantiation '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Scripting'!!Player methodsFor: 'scripts-kernel' stamp: 'sw 9/20/2000 16:52'!commandPhraseFor: commandSpec inViewer: aViewer	"Translate commandSpec into a PhraseTileMorph.  Put appropriate balloon help into the phrase"	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst |	names _ self class namedTileScriptSelectors.	resultType _ (commandSpec at: 1).	cmd _ (commandSpec at: 2).	commandSpec size = 3		ifTrue:			[aPhrase _ PhraseTileMorph new setOperator: cmd				type: resultType				rcvrType: #player]		ifFalse: "commandSpec size is four"			[argType _ commandSpec at: 4.			aPhrase _ PhraseTileMorph new setOperator: cmd				type: resultType				rcvrType: #player				argType: argType.			argTile _ self tileForArgType: argType inViewer: aViewer.			argTile position: aPhrase lastSubmorph position.			aPhrase lastSubmorph addMorph: argTile].	(self slotInfo includesKey: cmd)		ifTrue: [balloonTextSelector _ #userSlot].	(self belongsToUniClass and:			[self class includesSelector: cmd])		ifTrue: [balloonTextSelector _ #userScript].	aPhrase operatorTile balloonTextSelector: (balloonTextSelector ifNil: [cmd]).	aPhrase markAsPartsDonor.	selfTile _ aViewer tileForSelf.	selfTile position: aPhrase firstSubmorph position.	aPhrase firstSubmorph addMorph: selfTile.	aRow _ ViewerRow newRow borderWidth: 0; color: aViewer color.	aRow elementSymbol: cmd asSymbol.	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).	aRow addMorphBack: (Morph new extent: 4@2; beTransparent).	aRow addMorphBack: (aViewer infoButtonFor: cmd).	aRow addMorphBack: aPhrase.	(names includes: cmd) ifTrue:		[aPhrase userScriptSelector: cmd.		aPhrase beTransparent.		aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.		aRow addMorphBack: (stat _ (inst _ self scriptInstantiationForSelector: cmd) statusControlMorph).		inst updateStatusMorph: stat.].	aRow beSticky; disableDragNDrop.	^ aRow! !!Player methodsFor: 'scripts-kernel' stamp: 'sw 9/18/2000 16:52'!renameScript: oldSelector newSelector: newSelector	"Rename the given script to have the new selector"	|  oldStatus aUserScript aScriptEditor |	aUserScript _ self class userScriptForPlayer: self selector: oldSelector.	aScriptEditor _ aUserScript instantiatedScriptEditor.	oldStatus _ aScriptEditor scriptInstantiation status.	aScriptEditor renameScript: newSelector.	aScriptEditor bringUpToDate.	self class removeScriptNamed: oldSelector.	self class atSelector: newSelector putScriptEditor: aScriptEditor.	aScriptEditor scriptInstantiation status: oldStatus.	self actorState instantiatedUserScriptsDictionary removeKey: oldSelector.	aScriptEditor arrangeToStartStepping.	self updateAllViewersAndForceToShow: 'scripts'! !!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sw 9/21/2000 10:36'!buttonRowForEditor	"Answer a row of buttons that comprises the header at the top of the Scriptor"	| aRow aString aButtonMorph buttonFont aStatusMorph |	buttonFont _ Preferences standardButtonFont.	aRow _ AlignmentMorph newRow color: Color transparent; inset: 0.	aRow addMorphFront:		(SimpleButtonMorph new			label: '!!' font: (StrikeFont familyName: #ComicBold size: 16);			target: self;			color: Color yellow;			borderWidth: 0;			actWhen: #whilePressed;			actionSelector: #tryMe;			balloonTextSelector: #tryMe).	aRow addTransparentSpacerOfSize: 6@10.	self addDismissButtonTo: aRow.	aRow addTransparentSpacerOfSize: 6@10.	aString _ playerScripted externalName, ' ', self scriptTitle.	aRow addMorphBack:		(aButtonMorph _ SimpleButtonMorph new useSquareCorners label: aString font: buttonFont; target: self; setNameTo: 'title').	aButtonMorph actWhen: #buttonDown; actionSelector: #offerScriptorMenu.	aButtonMorph borderColor: (Color fromRgbTriplet: #(0.065 0.258 1.0)).	aButtonMorph color: (self isAnonymous ifTrue: [Color blue muchLighter] ifFalse: [ScriptingSystem uniformTileInteriorColor]).	aButtonMorph balloonTextSelector: #offerScriptorMenu.	aRow addMorphBack: (aStatusMorph _ self scriptInstantiation statusControlMorph).	aRow addTransparentSpacerOfSize: 10@10.	aRow addMorphBack:		(IconicButton new borderWidth: 0;			labelGraphic: (ScriptingSystem formAtKey: 'AddTest'); color: Color transparent; 			actWhen: #buttonDown;			target: self;			actionSelector: #addYesNoToHand;			shedSelvedge;			balloonTextSelector: #addYesNoToHand).	aRow addTransparentSpacerOfSize: 12@10.	self addDestroyButtonTo: aRow.	self scriptInstantiation updateStatusMorph: aStatusMorph.	^ aRow! !!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sw 9/18/2000 16:53'!updateStatus	"Update that status in the receiver's header"	(self topEditor == self) ifTrue:		[self updateStatusMorph: (self firstSubmorph findA: ScriptStatusControl)]! !!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sw 9/18/2000 16:53'!updateStatusMorph: statusMorph	"my status button may need to reflect an externally-induced change in status"	self scriptInstantiation updateStatusMorph: statusMorph.! !!ScriptInstantiation methodsFor: 'running' stamp: 'sw 9/21/2000 10:38'!startRunningIfPaused	"If the receiver is paused, start it ticking"	status == #paused ifTrue:		[status _ #ticking.		self updateAllStatusMorphs]! !!ScriptInstantiation methodsFor: 'running' stamp: 'sw 9/18/2000 19:43'!stopTicking	"If I'm ticking stop, else do nothing"	status == #ticking ifTrue:		[status _ #paused.		self updateAllStatusMorphs]! !!ScriptInstantiation methodsFor: 'status control' stamp: 'sw 9/21/2000 10:38'!chooseTriggerFrom: aMorph	"Put up a menu of status alternatives and carry out the request"	| aMenu reply standardStati  m |	"NB; the keyStroke branch commented out temporarily until keystrokes can actually be passed along to the user's scripting code"	standardStati _ #(normal paused ticking mouseDown mouseStillDown mouseUp mouseEnter mouseLeave mouseEnterDragging mouseLeaveDragging opening closing "keyStroke").	aMenu _ SelectionMenu labelList:  #(		'normal'		" -- run when called"				'paused' 		"ready to run all the time"		'ticking'		"run all the time"		'mouseDown'	"run when mouse goes down on me"		'mouseStillDown'	"while mouse still down"		'mouseUp'		"when mouse comes back up"		'mouseEnter'	"when mouse enters my bounds, button up"		'mouseLeave'	"when mouse exits my bounds, button up"		'mouseEnterDragging'	"when mouse enters my bounds, button down"		'mouseLeaveDragging'	"when mouse exits my bounds, button down"		'opening'	"when I am being opened"		'closing'	"when I am being closed"	"	'keyStroke'	run when user hits a key"		'what do these mean?'		)		lines: #(1 3 6 10 12)		selections: (standardStati, #(explainStatusAlternatives)).	reply _ aMenu startUpWithCaption: 'When should this script run?'.	(reply == #keyStroke) ifTrue: [^ self inform: 'user-scripted fieldingof keystrokes is notyet available.'].	reply == #explainStatusAlternatives ifTrue: [^ self explainStatusAlternatives].	reply ifNotNil: 		[self status: reply.  "Gets event handlers fixed up"		reply == #ticking ifTrue: [player costume arrangeToStartStepping].		reply == #paused ifTrue:			[m _ player costume.			(m isKindOf: SpeakerMorph) ifTrue: [m stopSound]].		self updateAllStatusMorphs]! !!ScriptInstantiation methodsFor: 'status control' stamp: 'sw 9/18/2000 16:54'!explainStatusAlternatives	"Open a little window that explains the various status alternatives" 	(StringHolder new contents: ScriptingSystem statusHelpString)		openLabel: 'Script Status'! !!ScriptInstantiation methodsFor: 'status control' stamp: 'sw 9/21/2000 10:38'!statusControlMorph	"Answer a control that will serve to reflect (and allow the user to change) the status of the receiver"	^ ScriptStatusControl new initializeFor: self! !!ScriptInstantiation methodsFor: 'status control' stamp: 'sw 9/21/2000 10:37'!updateAllStatusMorphs	"Update all status morphs bound to the receiver.  Done with a sledge-hammer at present."	(self currentWorld allMorphs select: [:m | (m isKindOf: ScriptStatusControl) and:			[m scriptInstantiation == self]]) do:		[:aStatusControl | self updateStatusMorph: aStatusControl]! !!ScriptInstantiation methodsFor: 'status control' stamp: 'sw 9/18/2000 16:54'!updateStatusMorph: statusControlMorph	"the status control may need to reflect an externally-induced change in status"	|  statusSymbol colorSelector statusReadoutButton |	statusControlMorph ifNil: [^ self].	statusSymbol _ self status.	(#(paused ticking) includes: statusSymbol)		ifTrue:			[statusControlMorph assurePauseTickControlsShow]		ifFalse:			[statusControlMorph maybeRemovePauseTickControls].	statusReadoutButton _ statusControlMorph submorphs last.	colorSelector _ ScriptingSystem statusColorSymbolFor: statusSymbol.	statusReadoutButton color: (Color perform: colorSelector) muchLighter.	statusReadoutButton label: statusSymbol asString font: Preferences standardButtonFont! !!ScriptStatusControl methodsFor: 'initialization' stamp: 'sw 9/20/2000 17:03'!assurePauseTickControlsShow	"Add two little buttons that allow the user quickly to toggle between paused and ticking state"	| aButton |	tickPauseButtonsShowing ifTrue: [^ self].	self beTransparent.	aButton _  UpdatingThreePhaseButtonMorph new.	aButton image:  (ScriptingSystem formAtKey: 'PausedPicOn');			offImage: (ScriptingSystem formAtKey: 'PausedPicOff');			pressedImage: (ScriptingSystem formAtKey: 'PausedPicPressed');			actionSelector: #pausedUp:with:; 			arguments: (Array with: nil with: aButton);			getSelector: #scriptIsPaused;			actWhen: #buttonUp;			target: self;			setBalloonText:'Pause this script for this object'.	submorphs first "the script button wrapper" addMorphBack: aButton;		addTransparentSpacerOfSize: (0 @ 3).	aButton _  UpdatingThreePhaseButtonMorph new.	aButton image:  (ScriptingSystem formAtKey: 'TickingPicOn');			offImage: (ScriptingSystem formAtKey: 'TickingPicOff');			pressedImage: (ScriptingSystem formAtKey: 'TickingPicPressed');			actionSelector: #tickingUp:with:; 			arguments: (Array with: nil with: aButton);			actWhen: #buttonUp;			getSelector: #scriptIsTicking;			target: self;			setBalloonText:'Set this script for this object to tick'.	submorphs first "the script button wrapper" addMorphBack: aButton.	self currentWorld startSteppingSubmorphsOf: self.	tickPauseButtonsShowing _ true! !!ScriptStatusControl methodsFor: 'initialization' stamp: 'sw 9/21/2000 21:08'!initializeFor: aScriptInstantiation	"Answer a control that will serve to reflect and allow the user to change the status of the receiver"	|  statusReadout |	hResizing _ #shrinkWrap.	scriptInstantiation _ aScriptInstantiation.	tickPauseButtonsShowing _ false.	tickPauseWrapper _ AlignmentMorph newColumn beTransparent; yourself.	self addMorphBack: tickPauseWrapper.	"self addTransparentSpacerOfSize: 5@0."	self addMorphBack: (statusReadout _ UpdatingSimpleButtonMorph new).	statusReadout setNameTo: 'trigger'.	statusReadout target: aScriptInstantiation; wordingSelector: #status; actionSelector: #chooseTriggerFrom:; arguments: {self}.	statusReadout setBalloonText: 'when this script should run'.	statusReadout actWhen: #buttonDown.	self assurePauseTickControlsShow! !!ScriptStatusControl methodsFor: 'initialization' stamp: 'sw 9/21/2000 10:40'!maybeRemovePauseTickControls	"If we're in the business of removing pauseTick controls when we're neither paused nor ticking, then do it now.  The present take is not to remove these controls, which explains why the body of this method is currently commented out."	"tickPauseWrapper removeAllMorphs.	tickPauseButtonsShowing _ false"! !!ScriptStatusControl methodsFor: 'access' stamp: 'sw 9/21/2000 10:41'!scriptInstantiation	"Answer the scriptInstantiation object with which the receiver is associated"	^ scriptInstantiation! !!ScriptStatusControl methodsFor: 'mouse gestures' stamp: 'sw 9/18/2000 19:42'!pausedUp: ignored with: alsoIgnored	"The paused button was hit -- respond to it"	(scriptInstantiation status == #paused)		ifFalse:			[scriptInstantiation status: #paused; updateAllStatusMorphs]! !!ScriptStatusControl methodsFor: 'mouse gestures' stamp: 'sw 9/18/2000 19:42'!tickingUp: ignored with: alsoIgnored	"The user hit the ticking control; make the status become one of ticking"	scriptInstantiation status == #ticking		ifFalse:			[scriptInstantiation status: #ticking; updateAllStatusMorphs]! !!ScriptStatusControl methodsFor: 'script status' stamp: 'sw 9/18/2000 16:55'!scriptIsPaused	"Answer whether the script is paused"	^ scriptInstantiation status == #paused! !!ScriptStatusControl methodsFor: 'script status' stamp: 'sw 9/18/2000 16:55'!scriptIsTicking	"Answer whether the script is ticking"	^ scriptInstantiation status == #ticking! !!TileMorph methodsFor: 'initialization' stamp: 'sw 9/21/2000 23:04'!setOperatorAndUseArrows: aString	"Set the operator as per aString, and add up/down arrows"	type _ #operator.	operatorOrExpression _ aString asSymbol. 	self line1: (ScriptingSystem wordingForOperator: aString).	self addArrows; updateLiteralLabel.	submorphs last setBalloonText: (ScriptingSystem helpStringForOperator: operatorOrExpression)! !!ScriptInstantiation reorganize!('anonymity' anonymous: initializeAnonymousScriptFor: initializePermanentScriptFor: isAnonymous)('player & selector access' player player: player:selector: player:selector:status: selector selector:)('running' runIfClosing runIfOpening runIfTicking startRunningIfPaused stopTicking)('misc' assureEventHandlerRepresentsStatus copyWithPlayerObliterated userScriptObject)('frequency' frequency frequency:)('status control' chooseTriggerFrom: explainStatusAlternatives status status: statusControlMorph updateAllStatusMorphs updateStatusMorph:)!"Postscript:"#(PausedPicOn PausedPicOff PausedPicPressed TickingPicOn TickingPicOff TickingPicPressed)		with: #(	( 1 65537 65536 98072 2132311832 2132279297 98072 2132311832 2132279297 98072 2132311832 2132279297 98072 2132311832 2132279297 1 65537 65536)	( 32536 2132311832 2132279296 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 2132311832 32536 2132311832 2132279296)	( 24582 1611030534 1611005952 1611038488 2132311832 2132303878 1611038488 2132311832 2132303878 1611038488 2132311832 2132303878 1611038488 2132311832 2132303878 24582 1611030534 1611005952)	( 1057 69272609 69271552 69296927 1663001375 1662977057 69296927 1663001375 1662977057 69296927 1663001375 1662977057 69296927 1663001375 1662977057 1057 69272609 69271552)	( 25375 1663001375 1662976000 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 1663001375 25375 1663001375 1662976000)	( 24582 1611030534 1611005952 1611031327 1663001375 1663000582 1611031327 1663001375 1663000582 1611031327 1663001375 1663000582 1611031327 1663001375 1663000582 24582 1611030534 1611005952))	do:		[:aKey :bitsArray |			ScriptingSystem saveForm:				(Form extent: 6@6 depth: 16 fromArray: bitsArray offset: 0@0)				atKey: aKey].!