'From Squeakland.396-Nihongo7.29 of 14 March 2005 [latest update: #33] on 14 March 2005 at 1:15:49 pm'!"Change Set:		AddTranslationMar14Date:			14 March 2005Author:			Yoshiki OhshimaAdd more translations pointed by Abe-san."!!ButtonProperties methodsFor: 'menu' stamp: 'yo 3/14/2005 13:07'!setArguments	| s newArgs newArgsArray |	s _ WriteStream on: ''.	arguments do: [:arg | arg printOn: s. s nextPutAll: '. '].	newArgs _ FillInTheBlank		request:'Please type the arguments to be sent to the targetwhen this button is pressed separated by periods' translated		initialAnswer: s contents.	newArgs isEmpty ifFalse: [		newArgsArray _ Compiler evaluate: '{', newArgs, '}' for: self logged: false.		self arguments: newArgsArray].! !!JoystickMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:11'!setXRange	| range |	range _ FillInTheBlank		request:'Type the maximum value for the X axis' translated		initialAnswer: ((xScale * (self width - handleMorph width) / 2.0) roundTo: 0.01) printString.	range isEmpty ifFalse: [		xScale _ (2.0 * range asNumber asFloat) / (self width - handleMorph width)].! !!JoystickMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:11'!setYRange	| range |	range _ FillInTheBlank		request:'Type the maximum value for the Y axis'  translated		initialAnswer: ((yScale * (self width - handleMorph width) / 2.0) roundTo: 0.01) printString.	range isEmpty ifFalse: [		yScale _ (2.0 * range asNumber asFloat) / (self width - handleMorph width)].! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'yo 3/14/2005 13:03'!changeEmphasis	| aList reply  |	aList _ #(normal bold italic narrow underlined struckOut).	reply _ (SelectionMenu labelList: (aList collect: [:t | t translated]) selections: aList) startUp.	reply ~~ nil ifTrue:		[self setEmphasis: reply.		paragraph composeAll.		self recomputeSelection.		self mvcRedisplay].	^ true! !!PolygonMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 12:48'!addCustomMenuItems: aMenu hand: aHandMorph	| lineName |	super addCustomMenuItems: aMenu hand: aHandMorph.	aMenu addUpdating: #handlesShowingPhrase target: self action: #showOrHideHandles.	vertices size > 2 ifTrue:		[aMenu addUpdating: #openOrClosePhrase target: self action: #makeOpenOrClosed.		lineName _ (closed ifTrue: ['outline'] ifFalse: ['line']) translated.		self isCurve			ifTrue: [aMenu add: ('make segmented {1}' translated format: {lineName translated}) action: #toggleSmoothing]			ifFalse: [aMenu add: ('make smooth {1}' translated format: {lineName translated}) action: #toggleSmoothing]]. 	aMenu add: 'specify dashed line' translated action:  #specifyDashedLine.	self isOpen ifTrue:		[aMenu addLine.		aMenu addWithLabel: '---' enablement: [self isOpen and: [arrows ~~ #none]] action:  #makeNoArrows.		aMenu addWithLabel: '-->' enablement: [self isOpen and: [arrows ~~ #forward]] action:  #makeForwardArrow.		aMenu addWithLabel: '<--' enablement: [self isOpen and: [arrows ~~ #back]] action:  #makeBackArrow.		aMenu addWithLabel: '<->' enablement: [self isOpen and: [arrows ~~ #both]] action:  #makeBothArrows.		aMenu add: 'customize arrows' translated action: #customizeArrows:.		(self hasProperty: #arrowSpec)			ifTrue: [aMenu add: 'standard arrows' translated action: #standardArrows]].! !!PolygonMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 12:54'!specifyDashedLine	| executableSpec newSpec |	executableSpec _ FillInTheBlank		request:'Enter a dash specification as{ major dash length. minor dash length. minor dash color }The major dash will have the normal border color.A blank response will remove the dash specification.[Note: You may give 5 items as, eg, {10. 5. Color white. 0. 3}where the 4th ityem is zero, and the 5th is the number of pixelsby which the dashes will move in each step of animation]' translated		initialAnswer: '{ 10. 5. Color red }'.	executableSpec isEmpty ifTrue:		[^ self stopStepping; dashedBorder: nil].	newSpec _ [Compiler evaluate: executableSpec] ifError:		[^ self stopStepping; dashedBorder: nil].	newSpec first isNumber & newSpec second isNumber & newSpec third isColor ifFalse:		[^ self stopStepping; dashedBorder: nil].	newSpec size = 3 ifTrue:		[^ self stopStepping; dashedBorder: newSpec].	(newSpec size = 5 and: [newSpec fourth isNumber & newSpec fifth isNumber]) ifTrue:		[^ self dashedBorder: newSpec; startStepping].! !!ScriptEditorMorph methodsFor: 'other' stamp: 'yo 3/14/2005 12:43'!toggleWhetherShowingTiles	"Toggle between showing the method pane and showing the tiles pane"	self showingMethodPane		ifFalse:				"currently showing tiles"			[self showSourceInScriptor]		ifTrue:				"current showing textual source"			[Preferences universalTiles				ifTrue: [^ self revertToTileVersion].			self savedTileVersionsCount >= 1				ifTrue:					[(self userScriptObject lastSourceString = (playerScripted class compiledMethodAt: scriptName) decompileString)						ifFalse:							[(self confirm: 'Caution -- this script was changedtextually; if you revert to tiles at thispoint you will lose all the changes youmay have made textually.  Do youreally want to do this?' translated) ifFalse: [^ self]].					self revertToTileVersion]				ifFalse:					[Beeper beep]]! !!SimpleButtonMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:09'!setArguments	| s newArgs newArgsArray |	s _ WriteStream on: ''.	arguments do: [:arg | arg printOn: s. s nextPutAll: '. '].	newArgs _ FillInTheBlank		request:'Please type the arguments to be sent to the targetwhen this button is pressed separated by periods' translated		initialAnswer: s contents.	newArgs isEmpty ifFalse: [		newArgsArray _ Compiler evaluate: '{', newArgs, '}' for: self logged: false.		self arguments: newArgsArray].! !!SimpleSliderMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:06'!setActionSelector	| oldSel newSel |	oldSel := setValueSelector isNil ifTrue: [''] ifFalse: [setValueSelector].	newSel := FillInTheBlank 				request: 'Please type the selector to be sent tothe target when this slider is changed' translated				initialAnswer: oldSel.	newSel isEmpty ifFalse: [self actionSelector: newSel]! !!SimpleSliderMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:09'!setArguments	| s newArgs newArgsArray |	s _ WriteStream on: ''.	arguments do: [:arg | arg printOn: s. s nextPutAll: '. '].	newArgs _ FillInTheBlank		request:'Please type the arguments to be sent to the targetwhen this button is pressed separated by periods' translated		initialAnswer: s contents.	newArgs isEmpty ifFalse: [		newArgsArray _ Compiler evaluate: '{', newArgs, '}' for: self logged: false.		self arguments: newArgsArray].! !!StringMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:03'!changeEmphasis	| reply aList |	aList _ #(normal bold italic narrow underlined struckOut).	reply _ (SelectionMenu labelList: (aList collect: [:t | t translated]) selections: aList) startUp.	reply ifNotNil:[		self emphasis: (TextEmphasis perform: reply) emphasisCode.	].! !!StringButtonMorph methodsFor: 'menu' stamp: 'yo 3/14/2005 13:09'!setArguments	| s newArgs newArgsArray |	s _ WriteStream on: ''.	arguments do: [:arg | arg printOn: s. s nextPutAll: '. '].	newArgs _ FillInTheBlank		request:'Please type the arguments to be sent to the targetwhen this button is pressed separated by periods' translated		initialAnswer: s contents.	newArgs isEmpty ifFalse: [		newArgsArray _ Compiler evaluate: '{', newArgs, '}' for: self logged: false.		self arguments: newArgsArray].! !!StringType methodsFor: 'tiles' stamp: 'yo 3/14/2005 21:27'!defaultArgumentTile        "Answer a tile to represent the type"        ^ 'abc' translated newTileMorphRepresentative typeColor: self typeColor! !