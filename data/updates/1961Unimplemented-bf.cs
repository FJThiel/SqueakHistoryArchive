'From Squeak2.8alpha of 4 February 2000 [latest update: #1919] on 15 March 2000 at 10:57:59 pm'!"Change Set:		10Unimplemented-bfDate:			16 March 2000Author:			Bert FreudenbergThere are a lot of sent but unimplemented methods (Smalltalk browseAllUnimplementedCalls) so I fixed some of the calls. There are more, a lot methods are probably just dead code and can be removed."!!FlashFileStream methodsFor: 'writing data' stamp: 'bf 3/16/2000 19:01'!nextColorMatrixPut: cm	"Write a (possibly compressed) color transformation"	self flushBits.	self nextBits: 2 put: 3. "Always write full transform"	self nextBits: 4 put: 15. "Always use full accuracy"	self nextSignedBits: 15 put: cm rMul.	self nextSignedBits: 15 put: cm gMul.	self nextSignedBits: 15 put: cm bMul.	hasAlpha ifTrue:[self nextSignedBits: 15 put: cm aMul].	self nextSignedBits: 15 put: cm rAdd.	self nextSignedBits: 15 put: cm gAdd.	self nextSignedBits: 15 put: cm bAdd.	hasAlpha ifTrue:[self nextSignedBits: 15 put: cm aAdd].! !!Float class methodsFor: 'plugin generation' stamp: 'bf 3/16/2000 19:06'!ccg: cg emitLoadFor: aString from: anInteger on: aStream	cg emitLoad: aString asFloatValueFrom: anInteger on: aStream! !!MailMessage methodsFor: 'access' stamp: 'bf 3/16/2000 18:31'!date	"Answer a date string for this message."	^(Date fromSeconds: time + (Date newDay: 1 year: 1980) asSeconds) 		printFormat: #(2 1 3 47 1 2)! !!MouseOverMorph methodsFor: 'event handling' stamp: 'bf 3/16/2000 18:57'!mouseMove: event	"Relay a mouseMove event to my model."	mouseMoveSelector ifNotNil:		[mouseMoveSelector numArgs = 0			ifTrue: [^ model perform: mouseMoveSelector].		mouseMoveSelector numArgs = 1			ifTrue: [^ model perform: mouseMoveSelector with: event cursorPoint].		mouseMoveSelector numArgs = 2			ifTrue: [^ model perform: mouseMoveSelector with: event cursorPoint with: event].		^ self error: 'mouseMoveSelector must take 0, 1, or 2 arguments']! !!ParagraphEditor methodsFor: 'editing keys' stamp: 'bf 3/16/2000 18:25'!copyHiddenInfo	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hiddeninfo.  Copy that to the clipboard.  You can paste it and see what it is.Usually enclosed in <>."	| attrList |	attrList _ paragraph text attributesAt: (startBlock stringIndex +stopBlock stringIndex)//2 forStyle: paragraph textStyle.	attrList do: [:attr |		(attr isKindOf: TextAction) ifTrue:			[^ self clipboardTextPut: ('<', attr info, '>') asText]].	"If none of the above"	attrList do: [:attr |		attr class == TextColor ifTrue:			[^ self clipboardTextPut: attr color printString asText]].	^ self clipboardTextPut: '[No hidden info]' asText! !!RNInboxFile methodsFor: 'scanning' stamp: 'bf 3/16/2000 19:19'!startOfArticle: aString	"Answer the newsgroup name if the given string is the start of a news article, for example:		Article 2958 of comp.lang.smalltalk:	Otherwise, answer nil."	| s name |	s _ ReadStream on: aString.	(self nextStringOf: s equals: 'Article ') ifFalse: [^nil].	[s next isDigit] whileTrue.	"consumes digits plus the following space"	(self nextStringOf: s equals: 'of ') ifFalse: [^nil].	name _ s upTo: $:.	((name size > 1) & (s atEnd)) ifFalse: [^nil].	^name! !!ScreenController methodsFor: 'menu messages' stamp: 'bf 3/16/2000 18:26'!soundOnOrOff	Preferences setPreference: #soundsEnabled		toValue: Preferences soundsEnabled not! !!TestCodeGenerator methodsFor: 'linking' stamp: 'bf 3/16/2000 19:20'!emitLoad: aString asMemberOf: aClass from: anInteger on: aStream	self emitLoad: aString asNakedOopFrom: anInteger on: aStream.	aStream		crtab;		nextPutAll: 'interpreterProxy->success(interpreterProxy->isMemberOf(';		nextPutAll: aString;		nextPutAll: 	', ''';		nextPutAll:	aClass asString;		nextPutAll: '''))'! !!ThreeDSChunkDescription methodsFor: 'printing' stamp: 'bf 3/16/2000 18:58'!printOn: aStream	aStream nextPutAll: 'Chunk('.	id printOn: aStream base: 16.	aStream 		nextPut: $h;		space;		nextPutAll: name.	Sensor commandKeyPressed ifTrue: [		aStream space; nextPut: $<; nextPutAll: comment; nextPut: $>].	aStream nextPut: $)! !!ThreeDSParser methodsFor: 'reading-keyframe' stamp: 'bf 3/16/2000 18:22'!trackCollect: aBlock	"Read keyframes, return Array with values of aBlock "		| flags keys frame unknown1 paramFlags params paramMask param |	"Track header"	flags := self short.	unknown1 := (1 to: 2) collect: [:i | self long].	keys := self long.	"Log"	log == nil ifFalse: [log crtab: indent; nextPutAll: 'Flags: '; print: flags; 		nextPutAll: ' {', flags hex, '}';		nextPutAll: ' Unknown: '; print: unknown1].	"Keys"	^(1 to: keys) collect: [:i |		params := nil.		"Key header"		frame := self long.		paramFlags := self short.		"Log"		log == nil ifFalse: [log crtab: indent; print: frame; nextPutAll: ':	['; print: paramFlags; nextPutAll: ']	'].		"Params, if not default"		paramFlags = 0 ifFalse: [			params := Dictionary new.			paramMask := 1.			#(tension: continuity: bias: easeTo: easeFrom:) with: 			#(true true true false false) do: [:what :symmetric |				param := (paramFlags bitAnd: paramMask) ~= 0					ifTrue: [self float]					ifFalse: [0.0].				log == nil ifFalse: [log crtab: indent+1; print: param; space; nextPutAll: what; space;					print: (symmetric ifTrue: [25 * (1 + param)] ifFalse: [param * 50]) rounded].				params at: what put: param.				paramMask := paramMask bitShift: 1].			log == nil ifFalse: [log crtab: indent+1]].		log == nil ifFalse: [log nextPutAll: 'Data: '].		"Data"		frame -> (aBlock value: params)]! !!ThreeDSParser class methodsFor: 'utilities' stamp: 'sma 3/15/2000 20:59'!initializeChunkDescriptions	"Initialize Class variable ChunkDescriptions from the documentation"	"ThreeDSParser initializeChunkDescriptions "	| s id tag comment sl c |	s := ReadStream on: self chunkDocumentation.	ChunkDescriptions := Dictionary new: 100.	id := tag := comment := nil.	[s atEnd] whileFalse: [		s peek = Character tab			ifFalse: [				id isNil ifFalse: [					ChunkDescriptions add: (ThreeDSChunkDescription						id: id name: tag comment: comment contents)].				id := Integer readFrom: s base: 16.				s skip: 2.				sl := ReadStream on: (s upTo: Character cr).				tag := OrderedCollection new.				[(c := sl next) isNil or: [c isSeparator]] whileFalse: [tag add: c].				tag := String withAll: tag.				sl skipSeparators.				comment := WriteStream on: String new]			ifTrue: [comment nextPutAll: s nextLine; nextPut: Character cr]		].	ChunkDescriptions add: (ThreeDSChunkDescription						id: id name: tag comment: comment contents)! !!VRMLWriter class methodsFor: 'private' stamp: 'bf 3/16/2000 19:16'!compileMultiFieldMethod: selString type: typeString	| source selParts |	source := String streamContents:[:s|		selParts := selString findTokens: ':'.		s nextPutAll: (selParts at: 1).		s nextPutAll: ': fields '.		s nextPutAll: (selParts at: 2).		s nextPutAll: ': aVRMLStream '.		s nextPutAll: (selParts at: 3).		s nextPutAll: ': level '.		s nextPutAll:('	"This method was automatically generated"	aVRMLStream nextPut: $[.	1 to: fields size do:[:i|		i > 1 ifTrue:[aVRMLStream crtab: level+1].		self storeSingleField$TYPESTRING$: (fields at: i) on: aVRMLStream indent: level+1].	aVRMLStream nextPut:$].' copyReplaceAll:'$TYPESTRING$' with: typeString).	].	self compile: source classified:'multi field writing'! !!VRMLWriter class methodsFor: 'private' stamp: 'bf 3/16/2000 19:17'!compileSingleFieldMethod: selString type: typeString	| source selParts |	source := String streamContents:[:s|		selParts := selString findTokens: ':'.		s nextPutAll: (selParts at: 1).		s nextPutAll: ': aField '.		s nextPutAll: (selParts at: 2).		s nextPutAll: ': aVRMLStream '.		s nextPutAll: (selParts at: 3).		s nextPutAll: ': level '.		s crtab.		s nextPutAll:'"This method was automatically generated"'.		s crtab.		s nextPutAll:'^aVRMLStream write'.		s nextPutAll: typeString.		s nextPutAll:': aField'.	].	self compile: source classified:'single field writing'! !!WonderlandActor methodsFor: 'set property' stamp: 'bf 3/16/2000 19:00'!setVisibility: newVisibility duration: time	"Sets the current object's visibility"	(time = rightNow) ifTrue: [		[ WonderlandVerifier Verify0To1Number: newVisibility ]			ifError: [ :msg :rcvr |				myWonderland reportErrorToUser: 'Squeak could not determine what visibility to make ' , myName , ' because ', msg.				^ nil ].		self setVisibilityRightNow: newVisibility undoable: true.		^ self. ].	^ (self setVisibility: newVisibility duration: time			style: gently).! !!WonderlandActor methodsFor: 'set property' stamp: 'bf 3/16/2000 19:00'!setVisibility: newVisibility duration: time style: aStyle	"Sets the current object's visibility"	| anim |	"Check our arguments to make sure they're valid"	[ WonderlandVerifier Verify0To1Number: newVisibility ]		ifError: [ :msg :rcvr |			myWonderland reportErrorToUser: 'Squeak could not determine what visibility to make ' , myName , ' because ', msg.			^ nil ].	[ WonderlandVerifier VerifyDuration: time ]		ifError: [ :msg :rcvr |			myWonderland reportErrorToUser:				'Squeak could not determine the duration to use for changing the visibility of ' , myName , ' because ', msg.			^ nil ].	[ WonderlandVerifier VerifyStyle: aStyle ]		ifError: [ :msg :rcvr |			myWonderland reportErrorToUser: 'Squeak could not determine the style to use for changing the visibility of ' , myName , ' because ', msg.			^ nil ].	anim _ AbsoluteAnimation new.	anim object: self			update: [:tColor | self setColorVector: tColor]			getStartState: [self getColorVector]			getEndState: [B3DColor4 red: (myColor red) green: (myColor green)						blue: (myColor blue) alpha: newVisibility]			style: aStyle			duration: time			undoable: true			inWonderland: myWonderland.	^ anim.! !