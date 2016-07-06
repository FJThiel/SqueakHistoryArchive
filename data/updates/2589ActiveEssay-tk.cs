'From Squeak2.9alpha of 16 June 2000 [latest update: #2558] on 9 September 2000 at 9:16:02 pm'!"Change Set:		tileLayout-tkDate:			28 August 2000Author:			Ted KaehlerA previous update made EToy tiles slightly narrower than they were before.  Existing scripts did not know to re-lay themselves out.  This tells them to do it."!BooklikeMorph subclass: #BookMorph	instanceVariableNames: 'pages currentPage '	classVariableNames: 'MethodHolders VersionNames VersionTimes '	poolDictionaries: ''	category: 'Morphic-Books'!RectangleMorph subclass: #TextFieldMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Text Support'!!TextFieldMorph commentStamp: 'tk 8/30/2000 14:13' prior: 0!Act as a field in a HyperCard-like setting.  Has both properties of a Rectangle, and exposes some proteries of the TextMorph it owns.!!Association methodsFor: 'objects from disk' stamp: 'tk 9/6/2000 16:44'!objectForDataStream: refStrm	"I am about to be written on an object file.  If I am a known global, write a proxy that will hook up with the same resource in the destination system."	^ (Smalltalk associationAt: key ifAbsent: [nil]) == self 		ifTrue: [DiskProxy global: key selector: #yourself args: #()]		ifFalse: [self]! !!ChangeRecord methodsFor: 'access' stamp: 'tk 9/7/2000 15:09'!stamp: threePartString	stamp _ threePartString! !!Character methodsFor: 'converting' stamp: 'tk 9/4/2000 12:05'!asText	^ self asString asText! !!CodeHolder methodsFor: 'as yet unclassified' stamp: 'tk 9/9/2000 21:08'!releaseCachedState	"Can always be found again.  Don't write on a file."	currentCompiledMethod _ nil.! !!CompiledMethod class methodsFor: 'instance creation' stamp: 'tk 9/9/2000 20:36'!basicNew: size	self error: 'CompiledMethods may only be created with newMethod:header:' ! !!ImageSegment methodsFor: 'read/write segment' stamp: 'tk 9/8/2000 14:23'!rootsIncludingPlayers	"Return a new roots array with more objects.  (Caller should store into rootArray.) Player (non-systemDefined) gets its class and metaclass put into the Roots array.  Then ask for the segment again."| extras havePresenter players morphs env |userRootCnt ifNil: [userRootCnt _ arrayOfRoots size].extras _ OrderedCollection new.arrayOfRoots do: [:root | 	(root isKindOf: Presenter) ifTrue: [havePresenter _ root].	(root isKindOf: PasteUpMorph) ifTrue: [			root isWorldMorph ifTrue: [havePresenter _ root presenter]].	(root isKindOf: Project) ifTrue: [havePresenter _ root world presenter]].havePresenter ifNotNil: [	havePresenter flushPlayerListCache.		"old and outside guys"	morphs _ IdentitySet new: 400.	havePresenter associatedMorph allMorphsAndBookPagesInto: morphs.	players _ (morphs select: [:m | m player ~~ nil] 				thenCollect: [:m | m player]) asArray.	players _ players select: [:ap | (arrayOfRoots includes: ap class) not		& (ap class isSystemDefined not)].	extras addAll: (players collect: [:each | each class]).	(env _ havePresenter world project environment) ifNil: [		extras addAll: (players collect: [:each | each class class])].	extras addAll: morphs.	"Make then ALL roots!!"	].extras isEmpty ifTrue: [^ nil].	"no change"env 	ifNil: ["old pre-environment"		havePresenter _ players _ morphs _ nil.		^ arrayOfRoots, extras]	"will contain multiples of some, but reduced later"	ifNotNil: [		(env includesKey: #Object) ifTrue: [self error: 'only look in local env, not up chain'].			"If get error, use a message other than includesKey:"		extras do: [:cls | 			(env includesKey: cls name) ifFalse: [				env declare: cls name from: Smalltalk]].		havePresenter _ players _ morphs _ env _ nil.		^ arrayOfRoots, extras	"still need in roots in case outside pointers"		]! !!MethodHolder methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 13:07'!compiledMethod	^ methodClass compiledMethodAt: methodSelector! !!MethodHolder methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 11:27'!doItReceiver	"This class's classPool has been jimmied to be the classPool of the class 	being browsed. A doIt in the code pane will let the user see the value of 	the class variables."	^ self selectedClass ifNil: [FakeClassPool new]! !!MethodHolder methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 13:08'!versions	"Return a VersionsBrowser (containing a list of ChangeRecords) of older versions of this method."	^ VersionsBrowser new scanVersionsOf: self compiledMethod			class: self selectedClass 			meta: methodClass isMeta 			category: self selectedMessageCategoryName				"(classOfMethod whichCategoryIncludesSelector: selectorOfMethod)"			selector: methodSelector! !!MethodHolder class methodsFor: 'as yet unclassified' stamp: 'tk 8/28/2000 13:35'!makeIsolatedCodePaneForClass: aClass selector: aSelector	| aCodePane aMethodHolder |	"Create and schedule a message list browser populated only by the currently selected message"	Smalltalk isMorphic ifFalse:		[^ self inform: 'sorry, this feature is currentlyonly available in morphic projects.'].	aMethodHolder _ self new.	aMethodHolder methodClass: aClass methodSelector: aSelector.	aCodePane _ MethodMorph on: aMethodHolder text: #contents accept: #contents:notifying:			readSelection: #contentsSelection menu: #codePaneMenu:shifted:.	aMethodHolder addDependent: aCodePane.	aCodePane borderWidth: 2; color: Color white.	aCodePane scrollBarOnLeft: false.	aCodePane width: 300.	"aCodePane changeStyleTo: 'ComicBold'.	aCodePane changeFontForAllTo: 'ComicBold16'."	self currentHand attachMorph: aCodePane.	^ aCodePane! !!Morph methodsFor: 'submorphs-accessing' stamp: 'sw 8/29/2000 16:10'!submorphNamed: aName ifNone: aBlock	"Find the first submorph with this name, or a button with an action selector of that name"	| sub args |	self submorphs do: [:p | p knownName = aName ifTrue: [^ p]].	self submorphs do: [:button |		(button respondsTo: #actionSelector) ifTrue:			[button actionSelector == aName ifTrue: [^ button]].		((button respondsTo: #arguments) and: [(args _ button arguments) notNil])			ifTrue: [(args at: 2 ifAbsent: [nil]) == aName ifTrue: [^ button]].		(button isKindOf: AlignmentMorph) ifTrue:			[(sub _ button submorphNamed: aName ifNone: [nil]) ifNotNil: [^ sub]]].	^ aBlock value! !!Morph methodsFor: 'submorphs-add/remove' stamp: 'tk 8/29/2000 14:04'!delete	"Remove the receiver as a submorph of its owner and make its new owner be nil."	| aWorld |	owner ifNotNil:		[(extension == nil or: [self player == nil])		ifTrue: [owner privateRemoveMorph: self.				owner _ nil]		ifFalse: ["Player must be notified"				aWorld _ self world ifNil: [World].	"or some proper of getting the World"				owner privateRemoveMorph: self.				owner _ nil.				self player noteDeletionOf: self fromWorld: aWorld]		].! !!Morph methodsFor: 'event handling' stamp: 'tk 9/6/2000 12:42'!click	"Pretend the user clicked on me."	(self handlesMouseDown: nil) ifTrue: [		self mouseDown: nil.		self mouseUp: nil].! !!BookMorph methodsFor: 'scripting' stamp: 'tk 9/7/2000 15:10'!chooseAndRevertToVersion	| time which |	"Let the user choose an older version for all code in MethodMorphs in this book.  Run through that code and revert each one to that time."	self methodHolders.	"find them in me"	self methodHolderVersions.	which _ PopUpMenu withCaption: 					'Put all scripts in this book back the way they were at this time:' 				chooseFrom: #('leave as is'), VersionNames.	which <= 1 ifTrue: [^ self].	time _ VersionTimes at: which-1.	self revertToCheckpoint: time.! !!BookMorph methodsFor: 'scripting' stamp: 'tk 9/8/2000 14:42'!installRollBackButtons	| all |	"In each script in me, put a versions button it the upper right."	all _ IdentitySet new.	self allMorphsAndBookPagesInto: all.	all _ all select: [:mm | mm class = MethodMorph].	all do: [:mm | mm installRollBackButtons: self].! !!BookMorph methodsFor: 'scripting' stamp: 'tk 9/6/2000 23:31'!methodHolderVersions	| arrayOfVersions vTimes strings |	"Create lists of times of older versions of all code in MethodMorphs in this book."	arrayOfVersions _ MethodHolders collect: [:mh | 		mh versions].	"equality, hash for MethodHolders?"	vTimes _ SortedCollection new.	arrayOfVersions do: [:versionBrowser |  		versionBrowser changeList do: [:cr | 			(strings _ cr stamp findTokens: ' ') size > 2 ifTrue: [				vTimes add: strings second asDate asSeconds + 						strings third asTime asSeconds]]].	VersionTimes _ Time condenseBunches: vTimes.	VersionNames _ Time namesForTimes: VersionTimes.! !!BookMorph methodsFor: 'scripting' stamp: 'tk 9/8/2000 14:41'!methodHolders	| all |	"search for all scripts that are in MethodHolders.  These are the ones that have versions."	all _ IdentitySet new.	self allMorphsAndBookPagesInto: all.	all _ all select: [:mm | mm class = MethodMorph].	MethodHolders _ all asArray collect: [:mm | mm model].! !!BookMorph methodsFor: 'scripting' stamp: 'tk 9/7/2000 15:08'!revertToCheckpoint: secsSince1901	| cngRecord |	"Put all scripts (that appear in MethodPanes) back to the way they were at an earlier time."	MethodHolders do: [:mh | 		cngRecord _ mh versions versionFrom: secsSince1901.		cngRecord ifNotNil: [			(cngRecord stamp: Utilities changeStamp) fileIn]].		"does not delete method if no earlier version"! !!Player methodsFor: 'misc' stamp: 'tk 9/1/2000 13:49'!contents: aValue	| |	"Pass this on to my costume.  For TextMorphs."	costume contents: aValue! !!Project methodsFor: 'file in/out' stamp: 'tk 9/8/2000 23:31'!exportSegmentWithCatagories: catList classes: classList	"Store my project out on the disk as an *exported* ImageSegment.  All outPointers will be in a form that can be resolved in the target image.  Name it <project name>.extSeg.  What do we do about subProjects, especially if they are out as local image segments?  Force them to come in?	Player classes are included automatically."| is str ans revertSeg roots holder |	self flag: #bob.		"probably can be deleted""world == World ifTrue: [^ false]."	"self inform: 'Can''t send the current world out'."world isMorph ifFalse: [	self projectParameters at: #isMVC put: true.	^ false].	"Only Morphic projects for now"world ifNil: [^ false].  world presenter ifNil: [^ false].Utilities emptyScrapsBook.Display checkCurrentHandForObjectToPaste.Command initialize.world fullReleaseCachedState. world cleanseStepList.world localFlapTabs size = world flapTabs size ifFalse: [	self error: 'Still holding onto Global flaps'].world releaseSqueakPages.holder _ Project allInstances.	"force them in to outPointers, where DiskProxys are made""Just export me, not my previous version"revertSeg _ self projectParameters at: #revertToMe ifAbsent: [nil].self projectParameters removeKey: #revertToMe ifAbsent: [].roots _ OrderedCollection new.roots add: self; add: world; add: transcript; add: changeSet; add: thumbnail.roots add: world activeHand; addAll: classList; addAll: (classList collect: [:cls | cls class]).catList do: [:sysCat | 	(SystemOrganization listAtCategoryNamed: sysCat asSymbol) do: [:symb |		roots add: (Smalltalk at: symb); add: (Smalltalk at: symb) class]].is _ ImageSegment new copyFromRootsForExport: roots asArray.	"world, and all Players"is state = #tooBig ifTrue: [^ false].str _ ''.is segment size < 3000 ifTrue: [	str _ 'Segment is only ', is segment size printString, ' long.'].(is outPointers detect: [:out | out isMorph] ifNone: [nil]) ifNotNil: [	str _ str, '\Morphs are pointed at from the outside.' withCRs].(is outPointers includes: world) ifTrue: [	str _ str, '\Project''s own world is not in the segment.' withCRs].str isEmpty ifFalse: [	ans _ (PopUpMenu labels: 'Do not write fileWrite file anywayDebug') startUpWithCaption: str.	ans = 1 ifTrue: [		revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].		^ false].	ans = 3 ifTrue: [self halt: 'Segment not written']].is writeForExportWithSources: self name, '.pr' inDirectory: self squeakletDirectory.revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].holder.^ true! !!Project methodsFor: 'file in/out' stamp: 'tk 9/8/2000 23:31'!exportSegmentWithCatagories: catList classes: classList fileName: aFileName directory: aDirectory	"Store my project out on the disk as an *exported* ImageSegment.  All outPointers will be in a form that can be resolved in the target image.  Name it <project name>.extSeg.  What do we do about subProjects, especially if they are out as local image segments?  Force them to come in?	Player classes are included automatically."| is str ans revertSeg roots holder |"world == World ifTrue: [^ false]."	"self inform: 'Can''t send the current world out'."world isMorph ifFalse: [	self projectParameters at: #isMVC put: true.	^ false].	"Only Morphic projects for now"world ifNil: [^ false].  world presenter ifNil: [^ false].Utilities emptyScrapsBook.Display checkCurrentHandForObjectToPaste.Command initialize.world fullReleaseCachedState. world cleanseStepList.world localFlapTabs size = world flapTabs size ifFalse: [	self error: 'Still holding onto Global flaps'].world releaseSqueakPages.holder _ Project allInstances.	"force them in to outPointers, where DiskProxys are made""Just export me, not my previous version"revertSeg _ self projectParameters at: #revertToMe ifAbsent: [nil].self projectParameters removeKey: #revertToMe ifAbsent: [].roots _ OrderedCollection new.roots add: self; add: world; add: transcript; add: changeSet; add: thumbnail.roots add: world activeHand; addAll: classList; addAll: (classList collect: [:cls | cls class]).catList do: [:sysCat | 	(SystemOrganization listAtCategoryNamed: sysCat asSymbol) do: [:symb |		roots add: (Smalltalk at: symb); add: (Smalltalk at: symb) class]].is _ ImageSegment new copyFromRootsForExport: roots asArray.	"world, and all Players"is state = #tooBig ifTrue: [^ false].str _ ''.is segment size < 3000 ifTrue: [	str _ 'Segment is only ', is segment size printString, ' long.'].(is outPointers detect: [:out | out isMorph] ifNone: [nil]) ifNotNil: [	str _ str, '\Morphs are pointed at from the outside.' withCRs].(is outPointers includes: world) ifTrue: [	str _ str, '\Project''s own world is not in the segment.' withCRs].str isEmpty ifFalse: [	ans _ (PopUpMenu labels: 'Do not write fileWrite file anywayDebug') startUpWithCaption: str.	ans = 1 ifTrue: [		revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].		^ false].	ans = 3 ifTrue: [self halt: 'Segment not written']].is writeForExportWithSources: aFileName inDirectory: aDirectory.revertSeg ifNotNil: [projectParameters at: #revertToMe put: revertSeg].holder.^ true! !!ScrollPane methodsFor: 'geometry' stamp: 'tk 8/25/2000 12:52'!resizeScrollBar	| w topLeft |	w _ self scrollbarWidth.	topLeft _ scrollBarOnLeft		ifTrue: [retractableScrollBar ifTrue: [bounds topLeft - ((w-1)@0)]									ifFalse: [bounds topLeft]]		ifFalse: [retractableScrollBar ifTrue: [bounds topRight]									ifFalse: [bounds topRight - (w@0)]].	scrollBar bounds: (topLeft extent: w @ bounds height)! !!PluggableTextMorph methodsFor: 'model access' stamp: 'tk 9/9/2000 21:13'!releaseCachedState	(model respondsTo: #releaseCachedState) ifTrue: [		model releaseCachedState].	"CodeHolder let go of CompiledMethod"! !!MethodMorph methodsFor: 'as yet unclassified' stamp: 'tk 9/7/2000 22:07'!installRollBackButtons: target	| mine |	"If I don't already have such a button, put one in at the upper right.  Set its target to the furtherest enclosing book.  Send chooseAndRevertToVersion when clicked.  Stay in place via scrollBar install."	mine _ self submorphNamed: #chooseAndRevertToVersion ifNone: [nil].	mine ifNil: [mine _ SimpleButtonMorph new.		"mine height: mine height - 2."		mine label: 'Roll Back'; cornerStyle: #square.		mine color: Color white; borderColor: Color black.		mine actionSelector: #chooseAndRevertToVersion.		mine align: mine topRight with: (self findA: ScrollBar) topLeft +(1@1).		self addMorphFront: mine.		mine height: mine height - 5 "14"].	mine target: target.! !!String methodsFor: 'comparing' stamp: 'tk 9/5/2000 10:42'!charactersExactlyMatching: aString	"Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."	| count |	count _ self size min: aString size.	1 to: count do: [:i | 		(self at: i) == (aString at: i) ifFalse: [			^ i - 1]].	^ count! !!String methodsFor: 'comparing' stamp: 'tk 9/5/2000 10:48'!howManyMatch: aString	"Count the number of characters that match up in self and aString."| count shorterLength |count _ 0.shorterLength _ self size min: aString size.1 to: shorterLength do: [:index | 	(self at: index) = (aString at: index) ifTrue: [		count _ count + 1]].^ count! !!StringMorph methodsFor: 'accessing' stamp: 'tk 8/28/2000 13:59'!fontName: fontName size: fontSize	^ self font: (StrikeFont familyName: fontName size: fontSize) 			emphasis: 0! !!Text methodsFor: 'accessing' stamp: 'tk 9/4/2000 16:04'!append: stringOrText	self replaceFrom: string size + 1				to: string size with: stringOrText! !!Text methodsFor: 'accessing' stamp: 'tk 9/6/2000 12:33'!lineCount	^ string lineCount! !!Text methodsFor: 'comparing' stamp: 'tk 9/6/2000 11:59'!howManyMatch: aString	^ self string howManyMatch: aString! !!TextContainer methodsFor: 'access' stamp: 'tk 8/31/2000 14:50'!textMorph	^ textMorph! !!TextContainer methodsFor: 'private' stamp: 'tk 8/30/2000 14:41'!bounds	| bounds theText |	self fillsOwner ifFalse: [^ textMorph textBounds].	theText _ textMorph meOrMyDropShadow.	bounds _ theText owner innerBounds.	bounds _ bounds insetBy: (textMorph valueOfProperty: #margins ifAbsent: [1@1]).	theText owner submorphsBehind: theText do:		[:m | bounds _ bounds merge: m fullBounds].	^ bounds! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 9/6/2000 11:03'!append: stringOrText	"add to my text"	| tm |	(tm _ self findA: TextMorph) ifNil: [^ nil].	tm contents append: stringOrText.	tm releaseParagraph; paragraph.	! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 14:22'!contents	| tm |	"talk to my text"	(tm _ self findA: TextMorph) ifNil: [^ nil].	^ tm contents! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 9/1/2000 14:03'!contents: textOrString	"talk to my text"	| tm newText atts |	(tm _ self findA: TextMorph) ifNil: [^ nil].	textOrString class == String ifTrue: [		tm contents ifNotNil: ["Keep previous properties of the field"			newText _ textOrString asText.			atts _ tm contents attributesAt: 1.			atts do: [:each | newText addAttribute: each].			^ tm contents: newText]].	^ tm contents: textOrString! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 9/4/2000 16:28'!fit	"tell my text to recompute its looks"	| tm |	(tm _ self findA: TextMorph) ifNil: [^ nil].	tm releaseParagraph; paragraph.! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 14:24'!fontName: fontName size: fontSize	| tm |	"talk to my text"	(tm _ self findA: TextMorph) ifNil: [^ nil].	^ tm fontName: fontName size: fontSize! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 8/30/2000 14:21'!initialize	| tm |	"set up the field"	super initialize.	self addMorph: (tm _ TextMorph new).	tm fillingOnOff.  "filling on"! !!TextFieldMorph methodsFor: 'as yet unclassified' stamp: 'tk 9/6/2000 12:33'!lineCount	| tm |	"how many lines in my text"	(tm _ self findA: TextMorph) ifNil: [^ nil].	^ tm contents string lineCount! !!TextMorph methodsFor: 'accessing' stamp: 'tk 8/31/2000 14:59'!contentsAsIs: stringOrText	"Accept new text contents with line breaks only as in the text.	Fit my width and height to the result."	wrapFlag _ false.	container ifNotNil: [container fillsOwner ifTrue: [wrapFlag _ true]].	self newContents: stringOrText! !!TextMorph methodsFor: 'accessing' stamp: 'tk 9/1/2000 14:01'!newContents: stringOrText 	"Accept new text contents."	| newText embeddedMorphs |	newText _ stringOrText asText.	text = newText ifTrue: [^ self].	"No substantive change"	text ifNotNil: [(embeddedMorphs _ text embeddedMorphs)			ifNotNil: 				[self removeAllMorphsIn: embeddedMorphs.				embeddedMorphs do: [:m | m delete]]].	text _ newText.	"add all morphs off the visible region; they'll be moved into the right 	place when they become visible. (this can make the scrollable area too 	large, though)"	stringOrText asText embeddedMorphs do: 		[:m | 		self addMorph: m.		m position: -1000 @ 0].	self releaseParagraph.	"update the paragraph cache"	self paragraph.	"re-instantiate to set bounds"	self world ifNotNil: [self world startSteppingSubmorphsOf: self]! !!TextMorph methodsFor: 'accessing' stamp: 'tk 9/1/2000 13:50'!textStyle	^textStyle! !!Time methodsFor: 'printing' stamp: 'tk 9/7/2000 00:09'!printMinutes	"Return as string 'hh:mm pm'  "	^String streamContents:		[ :aStream | self print24: false showSeconds: false on: aStream ]! !!Time class methodsFor: 'general inquiries' stamp: 'tk 8/30/2000 21:46'!condenseBunches: aCollectionOfSeconds	| secArray pause now out prev bunchEnd ago |	"Identify the major intervals in a bunch of numbers.  Each number is a seconds since 1901 that represents a date and time.  We want the last event in a bunch.  Return array of seconds for:	Every event in the last half hour.	Every bunch separated by 30 min in the last 24 hours.	Every bunch separated by two hours before that.""Time condenseBunches: 	(#(20 400 401  20000 20200 20300   40000 45000  200000 201000 202000) collect: [:tt | 		self totalSeconds - tt]) "	secArray _ aCollectionOfSeconds asSortedCollection.	pause _ 1.	now _ self totalSeconds.	out _ OrderedCollection new.	prev _ 0.	bunchEnd _ nil.	secArray reverseDo: [:secs | "descending"		ago _ now - secs.		ago > (60*30) ifTrue: [pause _ "60*30" 1800].		ago > (60*60*24) ifTrue: [pause _ "60*120" 7200].		ago - prev >= pause ifTrue: [out add: bunchEnd.  bunchEnd _ secs].		prev _ ago].	out add: bunchEnd.	out removeFirst.	^ out! !!Time class methodsFor: 'general inquiries' stamp: 'tk 8/30/2000 22:42'!humanWordsForSecondsAgo: secs	| date today |	"Return natural language for this date and time in the past."	secs <= 1 ifTrue: [^ 'a second ago'].	secs < 45 ifTrue: [^ secs printString, ' seconds ago'].	secs < 90 ifTrue: [^ 'a minute ago'].	secs < "45*60" 2700 ifTrue: [^ (secs//60) printString, ' minutes ago'].	secs < "90*60" 5400 ifTrue: [^ 'an hour ago'].	secs < "18*60*60" 64800 ifTrue: [^ (secs//3600) printString, ' hours ago'].	date _ Date fromSeconds: self totalSeconds - secs.		"now work with dates"	today _ Date today.	date > (today subtractDays: 2) ifTrue: [^ 'yesterday'].	date > (today subtractDays: 8) ifTrue: [^ 'last ', date weekday].	date > (today subtractDays: 13) ifTrue: [^ 'a week ago'].	date > (today subtractDays: 28) ifTrue: [		^ ((today subtractDate: date)//7) printString, ' weeks ago'].	date > (today subtractDays: 45) ifTrue: [^ 'a month ago'].	date > (today subtractDays: 300) ifTrue: [^ 'last ', date monthName].	^ date monthName, ', ', date year printString"Example#(0.5 30 62 130 4000 10000 60000 90000 345600 864000 1728000 3456000 17280000 34560000 345600000) collect: [:ss | Time humanWordsForSecondsAgo: ss]."! !!Time class methodsFor: 'general inquiries' stamp: 'tk 9/7/2000 00:10'!namesForTimes: arrayOfSeconds	| simpleEnglish prev final prevPair myPair |	"Return English descriptions of the times in the array.  They are each seconds since 1901.  If two names are the same, append the date and time to distinguish them."	simpleEnglish _ arrayOfSeconds collect: [:secsAgo |		self humanWordsForSecondsAgo: self totalSeconds - secsAgo].	prev _ ''.	final _ simpleEnglish copy.	simpleEnglish withIndexDo: [:eng :ind | 		eng = prev ifFalse: [eng]			ifTrue: [				prevPair _ self dateAndTimeFromSeconds: 						(arrayOfSeconds at: ind-1).				myPair _ self dateAndTimeFromSeconds: 						(arrayOfSeconds at: ind).				prevPair first = myPair first 					ifTrue: [final at: ind-1 put: 							(final at: ind-1), ', ', prevPair second printMinutes.						final at: ind put: 							(final at: ind), ', ', myPair second printMinutes]					ifFalse: [final at: ind-1 put: 							(final at: ind-1), ', ', prevPair first mmddyyyy.						final at: ind put: 							(final at: ind), ', ', myPair first mmddyyyy]].		prev _ eng].	^ final! !!VersionsBrowser methodsFor: 'menu' stamp: 'tk 9/7/2000 15:05'!versionFrom: secsSince1901	| strings vTime |	"Return changeRecord of the version in effect at that time.  Accept in the VersionsBrowser does not use this code."	changeList do: [:cngRec |		(strings _ cngRec stamp findTokens: ' ') size > 2 ifTrue: [				vTime _ strings second asDate asSeconds + 							strings third asTime asSeconds.				vTime <= secsSince1901 ifTrue: ["this one"					^ cngRec == changeList first ifTrue: [nil] ifFalse: [cngRec]]]].	"was not defined that early.  Don't delete the method."	^ changeList last	"earliest one may be OK"	! !TextMorph removeSelector: #text!"Postscript:Force tiles to re-layout."TileMorph allSubInstancesDo: [:tm | tm layoutChanged.  tm changed].!