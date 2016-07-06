'From Squeak3.3alpha of 18 January 2002 [latest update: #4966] on 5 September 2002 at 12:15:19 am'!"Change Set:		divers-swDate:			4 September 2002Author:			Scott Wallace� Fixes the bug that would drop you into a debugger when you used the 'find window' feature when there was a window with a zero-length title.� Only show the extra row of buttons in the debugger (as per update 4964) if the preference #extraDebuggerButtons  is true. To minimize upset.� When a message-list shows a class definition or hierarchy, now shows something appropriate in the annotation pane.� Repairs the long-standing annoyance that methods imported via change-list browsers and versions browsers did not get noted in the recent-submissions list. and hence would not be seen in recent-submissions browsers.� Fixes a bug that could make a pop-out behavior change for flap tabs not work properly.� Fixes the bug that String.withFirsCharacterDownshifted would generate an error when called with an empty string."!!ChangeRecord methodsFor: 'initialization' stamp: 'sw 8/25/2002 17:16'!fileIn	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."	| methodClass s aSelector |	Cursor read showWhile:		[(methodClass _ self methodClass) notNil ifTrue:			[methodClass compile: self text classified: category withStamp: stamp notifying: nil.			(aSelector _ self methodSelector) ifNotNil:				[Utilities noteMethodSubmission: aSelector forClass: methodClass]].		(type == #doIt) ifTrue:			[((s _ self string) beginsWith: '----') ifFalse: [Compiler evaluate: s]].		(type == #classComment) ifTrue:			[(Smalltalk at: class asSymbol) comment: self text stamp: stamp.			Utilities noteMethodSubmission: #Comment forClass: methodClass]]! !!CodeHolder methodsFor: 'annotation' stamp: 'sw 8/26/2002 10:19'!annotationForClassDefinitionFor: aClass	"Provide a line of content for an annotation pane, given that the receiver is pointing at the class definition of the given class."	^ 'Class definition for ', aClass name! !!CodeHolder methodsFor: 'annotation' stamp: 'sw 8/26/2002 10:19'!annotationForHierarchyFor: aClass	"Provide a line of content for an annotation pane, given that the receiver is pointing at the hierarchy of the given class."	^ 'Hierarchy for ', aClass name! !!CodeHolder methodsFor: 'annotation' stamp: 'sw 8/26/2002 10:15'!annotationForSelector: aSelector ofClass: aClass	"Provide a line of content for an annotation pane, representing information about the given selector and class"	| stamp  sendersCount implementorsCount aCategory separator aString aList  aComment aStream requestList |	aSelector == #Comment ifTrue: [^ self annotationForClassCommentFor: aClass].	aSelector == #Definition ifTrue: [^ self annotationForClassDefinitionFor: aClass].	aSelector == #Hierarchy ifTrue: [^ self annotationForHierarchyFor: aClass].	aStream _ ReadWriteStream on: ''.	requestList _ self annotationRequests.	 	separator _ requestList size > 1		ifTrue:			[self annotationSeparator]		ifFalse:			[''].	requestList do:		[:aRequest |		aRequest == #firstComment ifTrue:			[aComment _ aClass firstCommentAt: aSelector.			aComment isEmptyOrNil ifFalse:				[aStream nextPutAll: aComment, separator]].		aRequest == #masterComment ifTrue:			[aComment _ aClass supermostPrecodeCommentFor: aSelector.			aComment isEmptyOrNil ifFalse:				[aStream nextPutAll: aComment, separator]].		aRequest == #documentation ifTrue:			[aComment _ aClass precodeCommentOrInheritedCommentFor:   aSelector.			aComment isEmptyOrNil ifFalse:				[aStream nextPutAll: aComment, separator]].		aRequest == #timeStamp ifTrue:			[stamp _ self timeStamp.			aStream nextPutAll: (stamp size > 0				ifTrue: [stamp, separator]				ifFalse: ['no timeStamp', separator])].		aRequest == #messageCategory ifTrue:			[aCategory _ aClass organization categoryOfElement: aSelector.			aCategory ifNotNil: "woud be nil for a method no longer present, e.g. in a recent-submissions browser"				[aStream nextPutAll: aCategory, separator]].		aRequest == #sendersCount ifTrue:			[sendersCount _ (Smalltalk allCallsOn: aSelector) size.			sendersCount _ sendersCount == 1				ifTrue:					['1 sender']				ifFalse:					[sendersCount printString, ' senders'].			aStream nextPutAll: sendersCount, separator].		aRequest == #implementorsCount ifTrue:			[implementorsCount _ Smalltalk numberOfImplementorsOf: aSelector.			implementorsCount _ implementorsCount == 1				ifTrue:					['1 implementor']				ifFalse:					[implementorsCount printString, ' implementors'].			aStream nextPutAll: implementorsCount,  separator].		aRequest == #priorVersionsCount ifTrue:			[self addPriorVersionsCountForSelector: aSelector ofClass: aClass to: aStream].		aRequest == #priorTimeStamp ifTrue:			[stamp _ VersionsBrowser timeStampFor: aSelector class: aClass reverseOrdinal: 2.			stamp ifNotNil: [aStream nextPutAll: 'prior time stamp: ', stamp, separator]].		aRequest == #recentChangeSet ifTrue:			[aString _ ChangeSorter mostRecentChangeSetWithChangeForClass: aClass selector: aSelector.			aString size > 0 ifTrue: [aStream nextPutAll: aString, separator]].		aRequest == #allChangeSets ifTrue:			[aList _ ChangeSorter allChangeSetsWithClass: aClass selector: aSelector.			aList size > 0				ifTrue:					[aList size = 1						ifTrue:							[aStream nextPutAll: 'only in change set ']						ifFalse:							[aStream nextPutAll: 'in change sets: '].					aList do:						[:aChangeSet | aStream nextPutAll: aChangeSet name, ' ']]				ifFalse:					[aStream nextPutAll: 'in no change set'].			aStream nextPutAll: separator]].			^ aStream contents! !!Browser methodsFor: 'message list' stamp: 'sw 8/26/2002 09:55'!selectedMessageName	"Answer the message selector of the currently selected message, if any. 	Answer nil otherwise."	| aList |	editSelection == #editComment ifTrue: [^ #Comment].	editSelection == #editClass ifTrue: [^ #Definition].	messageListIndex = 0 ifTrue: [^ nil].	^ (aList _ self messageList) size >= messageListIndex		ifTrue:			[aList at: messageListIndex]		ifFalse:			[nil]! !!Browser methodsFor: 'annotation' stamp: 'sw 8/26/2002 10:00'!annotation	"Provide a line of content for an annotation pane, representing information about the method associated with the selected class and selector in the receiver."	|  aSelector aClass |	(aClass _ self selectedClassOrMetaClass) == nil ifTrue: [^ '------'].	self editSelection == #editComment ifTrue:		[^ self annotationForSelector: #Comment ofClass: aClass].	self editSelection == #editClass ifTrue:		[^ self annotationForSelector: #Definition ofClass: aClass].	(aSelector _ self selectedMessageName) ifNil: [^ '------'].	^ self annotationForSelector: aSelector ofClass: aClass! !!Debugger methodsFor: 'controls' stamp: 'sw 9/3/2002 10:24'!addOptionalButtonsTo: window at: fractions plus: verticalOffset	"Add button panes to the window.  A row of custom debugger-specific buttons (Proceed, Restart, etc.) is always added, and if optionalButtons is in force, then the standard code-tool buttons are also added.  Answer the verticalOffset plus the height added."	| delta buttons divider anOffset |	anOffset _ (Preferences optionalButtons and: [Preferences extraDebuggerButtons])		ifTrue:			[super addOptionalButtonsTo: window at: fractions plus: verticalOffset]		ifFalse:			[verticalOffset].	delta _ self defaultButtonPaneHeight.	buttons _ self customButtonRow.	buttons	 color: (Display depth <= 8 ifTrue: [Color transparent] ifFalse: [Color gray alpha: 0.2]);		borderWidth: 0.	Preferences alternativeWindowLook ifTrue:		[buttons color: Color transparent.		buttons submorphsDo:[:m | m borderWidth: 2; borderColor: #raised]].	divider _ BorderedSubpaneDividerMorph forBottomEdge.	Preferences alternativeWindowLook ifTrue:		[divider extent: 4@4; color: Color transparent; borderColor: #raised; borderWidth: 2].	window 		addMorph: buttons		fullFrame: (LayoutFrame 				fractions: fractions 				offsets: (0@anOffset corner: 0@(anOffset + delta - 1))).	window 		addMorph: divider		fullFrame: (LayoutFrame 				fractions: fractions 				offsets: (0@(anOffset + delta - 1) corner: 0@(anOffset + delta))).	^ anOffset + delta! !!FlapTab methodsFor: 'mouseover & dragover' stamp: 'sw 7/31/2002 00:53'!arrangeToPopOutOnMouseOver: aBoolean	aBoolean		ifTrue:			[self on: #mouseEnter send: #showFlap to: self.			referent on: #mouseLeave send: #hideFlapUnlessBearingHalo to: self.			self on: #mouseLeave send: #maybeHideFlapOnMouseLeave to: self]		ifFalse:			[self on: #mouseEnter send: nil to: nil.			self on: #mouseLeave send: nil to: nil.			referent on: #mouseLeave send: nil to: nil]! !!MenuItemMorph methodsFor: 'accessing' stamp: 'sw 9/4/2002 22:31'!contents: aString withMarkers: aBool inverse: inverse	"Set the menu item entry. If aBool is true, parse aString for embedded markers."	| markerIndex marker indent |	self contentString: nil. "get rid of old"	aBool ifFalse:[^super contents: aString].	self removeAllMorphs. "get rid of old markers if updating"	(aString size > 0 and: [(aString at: 1) = $<]) ifFalse:[^ super contents: aString].	markerIndex _ aString indexOf: $>.	markerIndex = 0 ifTrue:[^super contents: aString].	marker _ (aString copyFrom: 1 to: markerIndex) asLowercase.	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) ifFalse:[^super contents: aString].	self contentString: aString. "remember actual string"	(marker = '<on>' or:[marker = '<yes>']) ~= inverse		ifTrue:[marker _ self onImage]		ifFalse:[marker _ self offImage].	"Indent the string using white spaces"	indent _ ' '.	font _ self fontToUse.	[ (font widthOfString: indent) < (marker width + 4) ] 		whileTrue:[indent _ indent copyWith: Character space].	"Set the string"	super contents: indent, (aString copyFrom: markerIndex+1 to: aString size).	"And set the marker"	marker _ ImageMorph new image: marker.	marker position: (self left) @ (self top + 2).	self addMorphFront: marker.! !!String methodsFor: 'converting' stamp: 'sw 6/6/2002 18:24'!withFirstCharacterDownshifted	"Answer an object like the receiver but with first character downshifted if necesary"	"'MElViN' withFirstCharacterDownshifted"	"#Will withFirstCharacterDownshifted"	| answer |	self isEmpty ifTrue: [^ self].	answer _ self isString				ifTrue: ["don't change receiver"					self copy]				ifFalse: [self asString].	answer at: 1 put: (answer at: 1) asLowercase.	^ self isString		ifTrue: [answer]		ifFalse: [answer as: self class]! !"Postscript:"Preferences addPreference: #extraDebuggerButtons category: #debug default: false balloonHelp: 'if true, when the optionalButtons preference is on, debugger windows in Morphic will show *two* rows of buttons -- the debugger-specific row (proceed, restart, etc.) and also the conventional code-tools row.'.!