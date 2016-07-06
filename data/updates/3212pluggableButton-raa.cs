'From Squeak2.9alpha of 17 July 2000 [latest update: #3275] on 17 January 2001 at 2:35:53 pm'!"Change Set:		pluggableButtonDate:			17 January 2001Author:			Bob ArningChange PluggableButtonMorph>>initialize so that a new instance is #shrinkWrap-ped. Change all users that need #spaceFill so that they explicitly request this. Part of the AlignmentMorph cleanup."!!Browser methodsFor: 'initialize-release' stamp: 'RAA 1/17/2001 14:17'!buildMorphicSwitches	| instanceSwitch commentSwitch classSwitch row aColor |	instanceSwitch _ PluggableButtonMorph		on: self		getState: #instanceMessagesIndicated		action: #indicateInstanceMessages.	instanceSwitch		label: 'instance';		askBeforeChanging: true.	commentSwitch _ PluggableButtonMorph		on: self		getState: #classCommentIndicated		action: #plusButtonHit.	commentSwitch		label: '?' asText allBold;		askBeforeChanging: true;		setBalloonText: 'class comment'.	classSwitch _ PluggableButtonMorph		on: self		getState: #classMessagesIndicated		action: #indicateClassMessages.	classSwitch		label: 'class';		askBeforeChanging: true.	row _ AlignmentMorph newRow		hResizing: #spaceFill;		vResizing: #spaceFill;		layoutInset: 0;		borderWidth: 0;		addMorphBack: instanceSwitch;		addMorphBack: commentSwitch;		addMorphBack: classSwitch.	aColor _ Color colorFrom: self defaultBackgroundColor.	row submorphs do: [:m | 		m 			color: aColor;			onColor: aColor darker offColor: aColor;			hResizing: #spaceFill;			vResizing: #spaceFill.	].	^ row! !!Browser methodsFor: 'initialize-release' stamp: 'RAA 1/17/2001 14:18'!optionalButtonRow	"Answer a row of control buttons"	| aRow aButton |	aRow _ AlignmentMorph newRow.	aRow setNameTo: 'buttonPane'.	aRow beSticky.	aRow hResizing: #spaceFill.	aRow wrapCentering: #center; cellPositioning: #leftCenter.	aRow clipSubmorphs: true.	aRow addTransparentSpacerOfSize: (5@0).	self optionalButtonPairs  do:			[:tuple |				aButton _ PluggableButtonMorph					on: self					getState: nil					action: tuple second.				aButton 					useRoundedCorners;					hResizing: #spaceFill;					vResizing: #spaceFill;					label: tuple first asString;					onColor: Color transparent offColor: Color transparent.				tuple size > 2 ifTrue: [aButton setBalloonText: tuple third].				aRow addMorphBack: aButton.				aRow addTransparentSpacerOfSize: (3 @ 0)].	aRow addMorphBack: self diffButton.	Preferences sourceCommentToggleInBrowsers ifTrue: [aRow addMorphBack: self sourceOrInfoButton].	^ aRow! !!Celeste class methodsFor: 'instance creation' stamp: 'RAA 1/17/2001 14:19'!openOnDatabase: aMailDB	"Open a MailReader on the given mail database."	| model views buttons topWindow |	model _ self new openOnDatabase: aMailDB.	views _ self buildViewsFor: model.	buttons _ self buildButtonsFor: model.	Smalltalk isMorphic		ifTrue: 			[topWindow _ (SystemWindow labelled: 'Celeste') model: model.			buttons do: [ :each | each hResizing: #spaceFill; vResizing: #spaceFill].			topWindow addMorph: (buttons at: 1) frame: (0.0 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 2) frame: (0.125 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 3) frame: (0.25 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 4) frame: (0.375 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 5) frame: (0.5 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 6) frame: (0.625 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 7) frame: (0.75 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (buttons at: 8) frame: (0.875 @ 0.0 extent: 0.125 @ 0.05).			topWindow addMorph: (views at: 1) frame: (0.0 @ 0.05 extent: 0.2 @ 0.25).			topWindow addMorph: (views at: 2) frame: (0.2 @ 0.05 extent: 0.8 @ 0.25).			topWindow addMorph: (views at: 3) frame: (0.0 @ 0.3 extent: 1.0 @ 0.65).			topWindow addMorph: (views at: 4) frame: (0.0 @ 0.95 extent: 1.0 @ 0.05).			buttons do: [:b | b onColor: Color lightGray offColor: Color white].			topWindow openInWorld]		ifFalse: 			[topWindow _ StandardSystemView new model: model;					 label: 'Celeste';					 minimumSize: 400 @ 250.			(views at: 1) window: (0 @ 0 extent: 20 @ 25).			(views at: 2) window: (0 @ 0 extent: 80 @ 25).			(views at: 3) window: (0 @ 0 extent: 100 @ 70).			(buttons at: 1) window: (0 @ 0 extent: 12 @ 5).			(buttons at: 2) window: (0 @ 0 extent: 12 @ 5).			(buttons at: 3) window: (0 @ 0 extent: 12 @ 5).			(buttons at: 4) window: (0 @ 0 extent: 10 @ 5).			(buttons at: 5) window: (0 @ 0 extent: 13 @ 5).			(buttons at: 6) window: (0 @ 0 extent: 13 @ 5).			(buttons at: 7) window: (0 @ 0 extent: 15 @ 5).			(buttons at: 8) window: (0 @ 0 extent: 13 @ 5).			topWindow addSubView: (buttons at: 1);			 addSubView: (buttons at: 2) toRightOf: (buttons at: 1);			 addSubView: (buttons at: 3) toRightOf: (buttons at: 2);			 addSubView: (buttons at: 4) toRightOf: (buttons at: 3);			 addSubView: (buttons at: 5) toRightOf: (buttons at: 4);			 addSubView: (buttons at: 6) toRightOf: (buttons at: 5);			 addSubView: (buttons at: 7) toRightOf: (buttons at: 6);			 addSubView: (buttons at: 8) toRightOf: (buttons at: 7);			 addSubView: (views at: 1) below: (buttons at: 1);			 addSubView: (views at: 2) toRightOf: (views at: 1);			 addSubView: (views at: 3) below: (views at: 1).			topWindow controller open].	^model		"in case the sender wants to know"! !!CelesteComposition methodsFor: 'interface' stamp: 'RAA 1/17/2001 14:20'!openInMorphic	"open an interface for sending a mail message with the given initial 	text "	| textMorph buttonsList sendButton attachmentButton |	morphicWindow _ SystemWindow labelled: 'Mister Postman'.	morphicWindow model: self.	textEditor _ textMorph _ PluggableTextMorph						on: self						text: #messageText						accept: #messageText:.	morphicWindow addMorph: textMorph frame: (0 @ 0.1 corner: 1 @ 1).	buttonsList _ AlignmentMorph newRow.	sendButton _ PluggableButtonMorph				on: self				getState: nil				action: #submit.	sendButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'send message';		setBalloonText: 'add this to the queue of messages to be sent';		onColor: Color white offColor: Color white.	buttonsList addMorphBack: sendButton.		attachmentButton _ PluggableButtonMorph				on: self				getState: nil				action: #addAttachment.	attachmentButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'add attachment';		setBalloonText: 'Send a file with the message';		onColor: Color white offColor: Color white.	buttonsList addMorphBack: attachmentButton.		morphicWindow addMorph: buttonsList frame: (0 @ 0 extent: 1 @ 0.1).	morphicWindow openInMVC! !!ChangeList methodsFor: 'menu actions' stamp: 'RAA 1/17/2001 14:25'!optionalButtonRow	| aRow aButton |	aRow _ AlignmentMorph newRow.	aRow hResizing: #spaceFill.	aRow clipSubmorphs: true.	aRow addTransparentSpacerOfSize: (5@0).	aRow wrapCentering: #center; cellPositioning: #leftCenter.	self changeListButtonSpecs do:		[:triplet |			aButton _ PluggableButtonMorph				on: self				getState: nil				action: triplet second.			aButton				hResizing: #spaceFill;				vResizing: #spaceFill;				useRoundedCorners;				label: triplet first asString;				askBeforeChanging: true;				onColor: Color transparent offColor: Color transparent.			aRow addMorphBack: aButton.			aRow addTransparentSpacerOfSize: (3 @ 0).			aButton setBalloonText: triplet third.		].	aRow addMorphBack: self diffButton.		^ aRow! !!Debugger methodsFor: 'initialize' stamp: 'RAA 1/17/2001 14:26'!optionalButtonRow	"Answer a button pane affording the user one-touch access to certain functions; the pane is given the formal name 'buttonPane' by which it can be retrieved by code wishing to send messages to widgets residing on the pane"	| aRow aButton |	aRow _ AlignmentMorph newRow beSticky.	aRow setNameTo: 'buttonPane'.	aRow clipSubmorphs: true.	aButton _ SimpleButtonMorph new target: self.	aButton color: Color lightRed; borderWidth: 1; borderColor: Color red darker.	aRow addTransparentSpacerOfSize: (5@0).	self optionalButtonPairs do:		[:pair |				aButton _ PluggableButtonMorph					on: self					getState: nil					action: pair second.				aButton					hResizing: #spaceFill;					vResizing: #spaceFill;					useRoundedCorners;					label: pair first asString;					askBeforeChanging: true;					onColor: Color transparent offColor: Color transparent.				aRow addMorphBack: aButton.				aRow addTransparentSpacerOfSize: (3 @ 0)].	^ aRow! !!FileList methodsFor: 'initialization' stamp: 'RAA 1/17/2001 14:26'!optionalButtonRow	| aRow aButton |	aRow _ AlignmentMorph newRow beSticky.	aRow clipSubmorphs: true.	aRow addTransparentSpacerOfSize: (5@0).	self optionalButtonSpecs do:			[:spec |				aButton _ PluggableButtonMorph					on: self					getState: nil					action: spec second.				aButton					hResizing: #spaceFill;					vResizing: #spaceFill;					useRoundedCorners;					label: spec first asString;					askBeforeChanging: true;					onColor: Color transparent offColor: Color transparent.				aRow addMorphBack: aButton.				aRow addTransparentSpacerOfSize: (3 @ 0).				aButton setBalloonText: spec fourth.				aRow addTransparentSpacerOfSize: (3 @ 0).				(spec second == #sortBySize)					ifTrue:						[aRow addTransparentSpacerOfSize: (4@0)]].	^ aRow! !!IRCChannelListBrowser methodsFor: 'interface' stamp: 'RAA 1/17/2001 14:28'!openMorphicView	| win descListView updateButton actionColumn openChannelButton createChannelButton |	win _ SystemWindow new.	win setLabel: 'Channel Listing'.	win model: self.	descListView _ PluggableListMorph on: self  list: #channelDescriptions selected: #channelIndex changeSelected: #channelIndex:.	win addMorph: descListView  frame: (0@0 extent: 0.8@0.9).	updateButton _ PluggableButtonMorph on: connection getState: nil action: #requestChannelList.	updateButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'update'.	win addMorph: updateButton  frame: (0@0.9 extent: 1@0.1).	actionColumn _ AlignmentMorph newColumn.	openChannelButton _ PluggableButtonMorph on: self getState: nil action: #openSelectedChannel.	openChannelButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'join channel'.	actionColumn addMorphBack: openChannelButton.	createChannelButton _ PluggableButtonMorph on: self getState: nil action: #createChannel.	createChannelButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'create channel'.	actionColumn addMorphBack: createChannelButton.	win addMorph: actionColumn  frame: (0.8@0 extent: 0.2@0.9).	win openInWorld! !!IRCChannelObserver methodsFor: 'as yet unclassified' stamp: 'RAA 1/17/2001 14:28'!openMorphicView	"open a view for interacting with this collector"	| win textArea inputArea topicArea usersButton |	win _ SystemWindow new.	win setLabel: channel name.	win model: self.	topicArea _ PluggableTextMorph on: channel  text: #topic  accept: #changeTopic:.	topicArea acceptOnCR: true.	win addMorph: topicArea frame: (0@0 extent: 0.9@0.1).	usersButton _ PluggableButtonMorph on: channel  getState: nil  action: #openUserList.	usersButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'users'.	win addMorph: usersButton frame: (0.9@0 extent: 0.1@0.1).	textArea _ PluggableTextMorph on: self text: #chatText accept: nil readSelection: #chatTextSelection menu: nil.	win addMorph: textArea frame: (0@0.1 extent: 1@0.8).	inputArea _ PluggableTextMorph on: self text: nil accept: #sendMessage:.	inputArea acceptOnCR: true.	win addMorph: inputArea frame: (0@0.9 extent: 1@0.1) .	win openInWorld.! !!IRCChannelUsersBrowser methodsFor: 'as yet unclassified' stamp: 'RAA 1/17/2001 14:28'!openAsMorph	| win listView talkToButton |	win _ SystemWindow labelled: 'users in ', channel name.	win model: self.	listView _ PluggableListMorph on: self list: #userList selected: #userIndex  changeSelected: #userIndex:.	win addMorph: listView  frame: (0@0 extent: 1@0.9).	talkToButton _ PluggableButtonMorph on: self getState: nil action: #talkTo.	talkToButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'talk to selected user'.	win addMorph: talkToButton  frame: (0@0.9 extent: 1@0.1).	win openInWorld! !!IRCConnection methodsFor: 'UI' stamp: 'RAA 1/17/2001 14:29'!openConnectionDialogue	"open a dialogue for making new connections"	| dialogue textEntry connectButton y yDelta descMorph textEntryList |	dialogue _ SystemWindow new.	y _ 0.	yDelta _ 0.8 / 5.	textEntryList _ OrderedCollection new.	#(		'server'		server		'port'		portAsString		'nick'		nick		'username'	userName		'full name'	fullName	) pairsDo: [ :desc :meth |		descMorph _ PluggableButtonMorph on: self getState: nil action: nil.		descMorph			hResizing: #spaceFill;			vResizing: #spaceFill;			label: desc.		dialogue addMorph: descMorph  frame: (0@y extent: 0.3@yDelta).		textEntry _ PluggableTextMorph on: self text: meth accept: (meth, ':') asSymbol.		textEntry 			extent: 200@20;			color: (Color r: 1.0 g: 1.0 b: 0.599);			acceptOnCR: true.		dialogue addMorph: textEntry frame: (0.3@y extent: 0.7@yDelta).		textEntryList add: textEntry.		y _ y + yDelta.	].	connectButton _ PluggableButtonMorph on: [			textEntryList do: [ :m | m hasUnacceptedEdits ifTrue:[ m accept ] ].			self connect 		] fixTemps getState: nil action: #value.	connectButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'connect'.	dialogue addMorph: connectButton frame: (0@0.8 extent: 1@0.2).	dialogue setLabel: 'connect to an IRC server'.	dialogue openInWorld.! !!IRCConnection methodsFor: 'UI' stamp: 'RAA 1/17/2001 14:29'!openMotd	"open a view on the MOTD"	| win textView updateButton |	win _ SystemWindow labelled: 'MOTD'.	textView _ PluggableTextMorph on: self text: #motd accept: nil.	win addMorph: textView  frame: (0@0 extent: 1@0.9).	updateButton _ PluggableButtonMorph on: self getState: nil action: #requestMotd.	updateButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'update'.	win addMorph: updateButton  frame: (0@0.9 extent: 1@0.1).	win openInWorld.! !!IRCMorph methodsFor: 'initialization' stamp: 'RAA 1/17/2001 14:30'!initialize 	|  connectButton column channelListButton motdButton consoleView inputPane |	super initialize.	connection _ IRCConnection new.	server _ DefaultServer.	port _ DefaultPort.	nick _ DefaultNick.	userName _ DefaultUserName.	fullName _ DefaultFullName.	self setLabel: 'IRC'.	self extent: 200@100.	column _ AlignmentMorph newColumn.	connectButton _ PluggableButtonMorph		on: self		getState: nil		action: #openConnection.	connectButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'connect to server'.	column addMorphBack: connectButton.	motdButton _ PluggableButtonMorph		on: self		getState: nil		action: #openMotd.	motdButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'view MOTD'.	column addMorphBack: motdButton.	channelListButton _ PluggableButtonMorph		on: self		getState: nil		action: #openChannelList.	channelListButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'channel list'.	column addMorphBack: channelListButton.	self addMorph: column frame: (0@0 extent: 0.4@0.8).	consoleText _ Text new.	consoleView _ PluggableTextMorph on: self  text: #consoleText accept: nil readSelection: #consoleTextSelection menu: nil.	self addMorph: consoleView frame: (0.4@0 extent: 0.6@0.8).	consoleView color: (Color r: 0.937 g: 0.937 b: 0.937).	inputPane _ PluggableTextMorph on: self text: nil accept: #sendCommand:.	self addMorph: inputPane frame: (0@0.8 corner: 1@1).! !!IRCMorph methodsFor: 'interface' stamp: 'RAA 1/17/2001 14:30'!openConnection	"open a dialogue for making new connections"	| dialogue textEntry connectButton y yDelta descMorph |	dialogue _ SystemWindow new.	y _ 0.	yDelta _ 0.8 / 5.	#(		'server'		server		'port'		portAsString		'nick'		nick		'username'	userName		'full name'	fullName	) pairsDo: [ :desc :meth |		descMorph _ PluggableButtonMorph on: self getState: nil action: nil.		descMorph			hResizing: #spaceFill;			vResizing: #spaceFill;			label: desc.		dialogue addMorph: descMorph  frame: (0@y extent: 0.3@yDelta).		textEntry _ PluggableTextMorph on: self text: meth accept: (meth, ':') asSymbol.		textEntry extent: 200@20.		dialogue addMorph: textEntry frame: (0.3@y extent: 0.7@yDelta).		y _ y + yDelta.	].	connectButton _ PluggableButtonMorph on: self getState: nil action: #connect.	connectButton		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'connect'.	dialogue addMorph: connectButton frame: (0@0.8 extent: 1@0.2).	dialogue setLabel: 'connect to an IRC server'.	dialogue openInWorld.! !!PluggableButtonMorph methodsFor: 'initialize-release' stamp: 'RAA 1/17/2001 14:16'!initialize 	super initialize.	self listDirection: #topToBottom.	self hResizing: #shrinkWrap.	"<--so naked buttons work right"	self vResizing: #shrinkWrap.	self wrapCentering: #center; cellPositioning: #topCenter.	borderWidth _ 1.	model _ nil.	label _ nil.	getStateSelector _ nil.	actionSelector _ nil.	getLabelSelector _ nil.	getMenuSelector _ nil.	shortcutCharacter _ nil.	askBeforeChanging _ false.	triggerOnMouseDown _ false.	color _ Color lightGreen.	onColor _ color darker.	offColor _ color.	feedbackColor _ Color red.	showSelectionFeedback _ false.	allButtons _ nil.	self extent: 20@15.! !!PluggableButtonMorph class methodsFor: 'example' stamp: 'RAA 1/17/2001 14:32'!example	"PluggableButtonMorph example openInWorld"	| s1 s2 s3 b1 b2 b3 row |	s1 _ Switch new.	s2 _ Switch new turnOn.	s3 _ Switch new.	s2 onAction: [s3 turnOff].	s3 onAction: [s2 turnOff].	b1 _ (PluggableButtonMorph on: s1 getState: #isOn action: #switch) label: 'S1'.	b2 _ (PluggableButtonMorph on: s2 getState: #isOn action: #turnOn) label: 'S2'.	b3 _ (PluggableButtonMorph on: s3 getState: #isOn action: #turnOn) label: 'S3'.	b1		hResizing: #spaceFill;		vResizing: #spaceFill.	b2		hResizing: #spaceFill;		vResizing: #spaceFill.	b3		hResizing: #spaceFill;		vResizing: #spaceFill.	row _ AlignmentMorph newRow		hResizing: #spaceFill;		vResizing: #spaceFill;		addAllMorphs: (Array with: b1 with: b2 with: b3);		extent: 120@35.	^ row! !!PluggableFileList methodsFor: 'initialize-release' stamp: 'RAA 1/17/2001 14:32'!openAsMorphLabel: aString inWorld: aWorld	"Open a view of an instance of me."	"PluggableFileList new openAsMorphLabel: 'foo' inWorld: World"	| windowMorph volListMorph templateMorph fileListMorph leftButtonMorph middleButtonMorph rightButtonMorph |		self directory: directory.	windowMorph _ (SystemWindow labelled: aString) model: self.	volListMorph _ PluggableListMorph on: self		list: #volumeList		selected: #volumeListIndex		changeSelected: #volumeListIndex:		menu: #volumeMenu:.	volListMorph autoDeselect: false.	windowMorph addMorph: volListMorph frame: (0@0 corner: 0.4@0.5625).	templateMorph _ PluggableTextMorph on: self		text: #pattern		accept: #pattern:.	templateMorph askBeforeDiscardingEdits: false.	windowMorph addMorph: templateMorph frame: (0@0.5625 corner: 0.4@0.75).	fileListMorph _ PluggableListMorph on: self		list: #fileList		selected: #fileListIndex		changeSelected: #fileListIndex:		menu: #fileListMenu:.	windowMorph addMorph: fileListMorph frame: (0.4@0 corner: 1.0@0.75).	leftButtonMorph _ PluggableButtonMorph 		on: self		getState: #leftButtonState		action: #leftButtonPressed.	leftButtonMorph		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'Cancel';		onColor: Color red offColor: Color red;		feedbackColor: Color orange;		borderWidth: 3.	middleButtonMorph _ PluggableButtonMorph		on: self		getState: nil		action: nil.	middleButtonMorph		hResizing: #spaceFill;		vResizing: #spaceFill;		label: prompt;		onColor: Color lightYellow offColor: Color lightYellow;		feedbackColor: Color lightYellow;		borderWidth: 1.	rightButtonMorph _ PluggableButtonMorph		on: self		getState: #rightButtonState		action: #rightButtonPressed.	rightButtonMorph		hResizing: #spaceFill;		vResizing: #spaceFill;		label: 'Accept';		onColor: Color green offColor: Color lightYellow;		feedbackColor: Color black;		borderWidth: (self canAccept ifTrue: [3] ifFalse: [1]).	"self canAccept ifFalse: [rightButtonMorph controller: NoController new]."	windowMorph		addMorph: leftButtonMorph frame: (0@0.75 corner: 0.25@1.0);		addMorph: middleButtonMorph frame: (0.25@0.75 corner: 0.75@1.0);		addMorph: rightButtonMorph frame: (0.75@0.75 corner: 1.0@1.0).	self changed: #getSelectionSel.	windowMorph openInWorld! !!TestModel methodsFor: 'interface opening' stamp: 'RAA 1/17/2001 14:33'!openAsMorph	"doIt: [TestModel new openAsMorph]"	| topWindowM runButtonM detailsTextM failureListM errorListM |	self updateColorSelector: #updateColorM.	"=== build the parts ... ==="	(topWindowM := SystemWindow labelled: self windowLabel)		model: self.	self patternTextM: (PluggableTextMorph		on: self		text: #patternText		accept: #patternText:		readSelection: nil		menu: #patternHistoryMenu:).	self patternTextM setBalloonText: self balloonPatternText.	runButtonM := PluggableButtonMorph		on: self		getState: #runButtonState		action: #runTests		label: #runButtonLabel.	runButtonM		hResizing: #spaceFill;		vResizing: #spaceFill;		onColor: self runButtonColor		offColor: self runButtonColor.	runButtonM setBalloonText: self balloonRunButton.	self summaryTextM: (PluggableTextMorph		on: self		text: #summaryText		accept: nil).	self summaryTextM setBalloonText: self balloonSummaryText.	detailsTextM := PluggableTextMorph		on: self		text: #detailsText		accept: nil.	detailsTextM setBalloonText: self balloonDetailsText.	failureListM := PluggableListMorph		on: self		list: #failureList		selected: #failureListSelectionIndex		changeSelected: #failureListSelectionIndex:.	failureListM setBalloonText: self balloonFailureList.	errorListM := PluggableListMorph		on: self		list: #errorList		selected: #errorListSelectionIndex		changeSelected: #errorListSelectionIndex:.	errorListM setBalloonText: self balloonErrorList.	"=== assemble the whole ... ==="	topWindowM		addMorph: self patternTextM frame: (0.0@0.0 extent: 1.0@0.1);		addMorph: runButtonM frame: (0.0@0.1 extent: 0.2@0.1);		addMorph: self summaryTextM frame: (0.2@0.1 extent: 0.8@0.1);		addMorph: detailsTextM frame: (0.0@0.2 extent: 1.0@0.1);		addMorph: failureListM frame: (0.0@0.3 extent: 1.0@0.35);		addMorph: errorListM frame: (0.0@0.65 extent: 1.0@0.35).	"=== open it ... ==="	topWindowM openInWorldExtent: 250@200.	^topWindowM! !