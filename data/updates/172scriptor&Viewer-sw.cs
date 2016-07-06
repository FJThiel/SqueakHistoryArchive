'From Squeak 2.1 of June 30, 1998 on 5 August 1998 at 6:24:03 pm'!"Change Set:		scriptor&Viewer-swDate:			5 August 1998Author:			Scott WallaceVarious enhancements to scriptors and viewers, including:*  Add scripting commands to lift all pens, lower all pens, clear pen trails.*  Add scripting commands for pausing a script (in addition to totally stopping a script)*  Don't allow multple lines into a TEST area (always broke things).*  Don't fail if we have a naked CompoundTileMorph on the world (for example)*  A Viewer open on the World no longer invites you to do things that Mother Nature doesn't like you to do to a World."!!Player methodsFor: 'pen' stamp: 'sw 8/3/1998 16:10'!clearTurtleTrails	self costume clearTurtleTrails! !!Player methodsFor: 'pen' stamp: 'sw 8/3/1998 16:10'!liftAllPens	self costume liftAllPens! !!Player methodsFor: 'pen' stamp: 'sw 8/3/1998 16:10'!lowerAllPens	self costume lowerAllPens! !!Player methodsFor: 'slots-kernel' stamp: 'sw 8/3/1998 16:47'!standardSlotsForBank: aBank	"Return an array of slot names and slot info for use in a viewer on the receiver""		name		type		r/w			get selector			put selector		-----------	---------		-----------	---------------------	-------------   "	(costume isKindOf: WorldMorph) ifTrue: [^ self standardWorldSlotsForBank: aBank].	aBank = 1 ifTrue: [^ #(		(heading	number		readWrite	getHeading			setHeading:)		(x 			number		readWrite	getX					setX:)		(y			number		readWrite	getY				setY:)		(colorUnder	color		readOnly	getColorUnder		unused))].	aBank = 3 ifTrue: [^ #(		(penDown	boolean		readWrite	getPenDown			setPenDown:)		(penColor	color		readWrite	getPenColor			setPenColor:)		(penSize 	number		readWrite	getPenSize			setPenSize:))].	aBank = 4 ifTrue: [^ #(		(colorSees	boolean		readOnly	dummy				unused)		(scaleFactor	number		readWrite	getScaleFactor		setScaleFactor:)		(width 		number		readWrite	getWidth			setWidth:)		(height 		number		readWrite	getHeight			setHeight:)		(left 		number		readWrite	getLeft				setLeft:)		(right 		number		readWrite	getRight			setRight:)		(top 		number		readWrite	getTop				setTop:)		(bottom 		number		readWrite	getBottom			setBottom:)		)].	^ #()! !!Player methodsFor: 'slots-kernel' stamp: 'sw 8/3/1998 16:46'!standardWorldSlotsForBank: aBank	"Return an array of slot names and slot info for use in a viewer on the receiver, special-cased for the situation where the receiver's costume is the World""		name		type		r/w			get selector			put selector		-----------	---------		-----------	---------------------	-------------   "	aBank = 1 ifTrue: [^ #(		(x 			number		readOnly	getX					unused)		(y			number		readOnly	getY				unused))].	aBank = 4 ifTrue: [^ #(		(colorSees	boolean		readOnly	dummy				unused)		(width 		number		readOnly	getWidth			unused)		(height 		number		readOnly	getHeight			unused)		(left 		number		readOnly	getLeft				unused)		(right 		number		readOnly	getRight			unused)		(top 		number		readOnly	getTop				unused)		(bottom 		number		readOnly	getBottom			unused)		)].	^ #()! !!Player methodsFor: 'scripts-standard' stamp: 'sw 8/4/1998 23:56'!changeScript: scriptName toStatus: statusSymbol	scriptName ifNil: [^ self].	Symbol hasInterned: scriptName ifTrue:		[:sym | self instantiatedUserScriptsDo:			[:aUserScript | aUserScript selector == sym				ifTrue:					[aUserScript status: statusSymbol.					^ costume world updateStatusForAllScriptEditors]]]! !!Player methodsFor: 'scripts-standard' stamp: 'sw 8/4/1998 23:57'!pauseScript: scriptName	self changeScript: scriptName toStatus: #paused! !!Player methodsFor: 'scripts-standard' stamp: 'sw 8/5/1998 00:00'!standardCommandsForBank: aBank	"Return a list of typed-command arrays of the form:		<result type> <command> <argType>" 	costume isWorldOrHandMorph ifTrue:		[^ #((command beep: sound)			(command stopScript: string)			(command pauseScript: string)			(command startScript: string)			(command initiatePainting))].	(aBank = 1) ifTrue:		[^ #((command forward: number)			(command turn: number)			(command wearCostumeOf: player)			(command moveToward: player)			(command beep: sound))].	(aBank = 2) ifTrue:		[^ #((command show)			(command hide)			(command bounce: sound)			(command wrap)			(command goToRightOf: player)		"	(command stopProgramatically)  "			(command stopScript: string)			(command pauseScript: string)			(command startScript: string))].	(aBank == 3 and: [costume isKindOf: PasteUpMorph]) ifTrue:		[^ #((command liftAllPens)			(command lowerAllPens)			(command clearTurtleTrails)			(command goToNextCard)			(command goToPreviousCard)			(command newCard)			(command deleteCard)			(command initiatePainting))].	^ #()! !!Player methodsFor: 'scripts-standard' stamp: 'sw 8/4/1998 23:57'!startScript: scriptName	self changeScript: scriptName toStatus: #ticking! !!Player methodsFor: 'scripts-standard' stamp: 'sw 8/4/1998 23:57'!stopScript: scriptName	self changeScript: scriptName toStatus: #normal! !!ScriptEditorMorph methodsFor: 'other' stamp: 'sw 8/3/1998 15:08'!isTextuallyCoded	(self topEditor isKindOf: ScriptEditorMorph) ifFalse: [^ false].  "workaround for the case where the receiver is embedded in a free-standing CompoundTileMorph.  Yecch!!"	^ self userScriptObject isTextuallyCoded! !!BooleanScriptEditor methodsFor: 'all' stamp: 'sw 8/3/1998 17:15'!wantsDroppedMorph: aMorph	((aMorph isKindOf: PhraseTileMorph) and:		[submorphs size == 1]) ifTrue: [^ false].	^ aMorph isTileLike and: [aMorph resultType ~~ #command]! !