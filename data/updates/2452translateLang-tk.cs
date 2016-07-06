'From Squeak2.9alpha of 16 June 2000 [latest update: #2447] on 12 July 2000 at 5:57:45 pm'!"Change Set:		translateLang-tkDate:			12 July 2000Author:			Ted KaehlerYou can now get any text in any pane of Squeak translated into another language.  Invoke it in any Squeak text pane by choosing 'translate it' from the shift-menu. (For the moment, you have to press Option-Shift to get the shift menu.  The 'more...' mechanism is broken and is not capable of getting the current selection from the window.)	Languages are set by the 'choose language' menu item of the shift menu.  Or by changing (Preferences valueOfFlag: #languageTranslateFrom) and (Preferences valueOfFlag: #languageTranslateTo).   	  A class that is the Squeak interface to the translation server at www.freetranslation.com.The name of the preference for dictionary lookup of a word and spelling has changed.  The preference is now called #myLanguage.Added an easier message for invoking a menu.   (PopUpMenu withCaption: cap chooseFrom: labels).  labels may be either an array of items or a string with CRs in it.  The label string may be submitted with backslashes instead of returns."!Object subclass: #FreeTranslation	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Network-TelNet WordNet'!!FreeTranslation commentStamp: 'tk 7/12/2000 17:43' prior: 0!Squeak interface to the translation server at www.freetranslation.com.  Invoke it in any Squeak text pane by choosing 'translate it' from the shift-menu.  Languages are set by the 'choose language; menu item of the shift menu.  Or by changing (Preferences valueOfFlag: #languageTranslateFrom) and (Preferences valueOfFlag: #languageTranslateTo).   	See class method openScamperOn:.	FreeTranslation openScamperOn: 'Why don''t you ever write anymore?'!!WordNet commentStamp: 'tk 7/12/2000 14:47' prior: 0!Query the WordNet lexicon at Princeton Univ.  At http://www.cogsci.princeton.edu/cgi-bin/webwn/   To get the definition of a word, select any word in any text pane, and choose "definition of word" from the shift menu.  WordNet is also used for the "verify spelling of word" menu item.	Subclasses are interfaces to other dictionaries.  The "choose language" item on the shift-menu lets you select a language (and its server).  (Preferences setPreference: #myLanguage toValue: #Portuguese).WordNet openScamperOn: 'balloon'.DD _ WordNet new.DD definition: 'balloon'.DD parts "of speech".	 OrderedCollection ('noun' 'verb' )DD sensesFor: 'noun'.	 2DD def: 1 for: 'noun'.	 '(large tough non-rigid bag filled with gas or hot air)'After the initial response, keep a separate stream for the definition of each part of speech.  Caller may later query them for information.!!PortugueseLexiconServer commentStamp: 'tk 7/12/2000 14:47' prior: 0!Provide a standard interface for the Portuguese language dictionary at http://www.priberam.pt/.The "choose language" item on the shift-menu lets you select a language (and its server).  (Preferences setPreference: #myLanguage toValue: #Portuguese).  To get the definition of a word, select any word in any text pane, and choose "definition of word" from the shift menu.  Also used for the "verify spelling of word" menu item.PortugueseLexiconServer openScamperOn: 'palavra'.See class WordNet.Converts an input string from Apple character encoding to the encoding used on this server.  'partic�pio' -> 'partic�pio'Not yet completed:** Better parse of the definition page, so it can be used by a program.!]style[(591 17 104)f1,f1cblack;,f1!!FreeTranslation class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 13:54'!openScamperOn: currentSelection	"Submit the string to the translation server at www.freetranslation.com.  Ask it to translate from (Preferences valueOfFlag: #languageTranslateFrom) to (Preferences valueOfFlag: #languageTranslateTo).  Display the results in a Scamper window, reusing the previous one if possible."	| inputs scamperWindow from to | 	currentSelection size >= 10000 ifTrue: [^ self inform: 'Text selection is too long.'].	(Preferences valueOfFlag: #languageTranslateFrom) == false ifTrue: [			Preferences setPreference: #languageTranslateFrom toValue: 'English'].	(Preferences valueOfFlag: #languageTranslateTo) == false ifTrue: [			Preferences setPreference: #languageTranslateTo toValue: 'German'].	(from _ Preferences valueOfFlag: #languageTranslateFrom) = 		(to _ Preferences valueOfFlag: #languageTranslateTo) ifTrue: [			^ self inform: 'You asked to translate from ', from, ' to ', to, '.\' withCRs,				'Use "choose language" to set these.'].  	inputs _ Dictionary new.	inputs at: 'SrcText' put: (Array with: currentSelection).	inputs at: 'Sequence' put: #('core').	inputs at: 'Mode' put: #('html').	inputs at: 'template' put: #('TextResult2.htm').	inputs at: 'Language' put: (Array with: from, '/', to).	scamperWindow _ Scamper newOrExistingOn: 'http://ets.freetranslation.com'.	scamperWindow model submitFormWithInputs: inputs 		url: 'http://ets.freetranslation.com:5081' asUrl		method: 'post'.	scamperWindow activate.! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'tk 7/9/2000 22:28'!translateIt	| aWord |	"Translate a passage of text and open its definition in a separate window.  Use the FreeTranslation.com server.  Requires internet access.  Default is English-> Spanish, but set it with the 'choose language' menu item."	self lineSelectAndEmptyCheck: [^ self].	aWord _ self selection asString.	self terminateAndInitializeAround: [		FreeTranslation openScamperOn: aWord].! !!ParagraphEditor methodsFor: 'menu messages' stamp: 'tk 7/9/2000 22:29'!wordDefinition	| aWord |	"Look up a single word and open its definition in a separate window.  Use the WordNet server.  Requires internet access.  Default is English, but set it like this	Preferences setPreference: #naturalLanguage toValue: #Portuguese.	"	self lineSelectAndEmptyCheck: [^ self].	aWord _ self selection asString.	self terminateAndInitializeAround: [		WordNet lexiconServer openScamperOn: aWord]."This code for showing definition in a Workspace	Cursor execute showWhile: [		(aDefinition _ WordNet lexiconServer definitionsFor: aWord) ifNil: [			^ view flash]].	self terminateAndInitializeAround: [		(StringHolder new contents: aDefinition) openLabel: aWord		]."! !!ParagraphEditor class methodsFor: 'class initialization' stamp: 'tk 7/9/2000 22:24'!shiftedYellowButtonMenu	"Answer the menu to be presented when the yellow button is pressed while the shift key is down"	^ SelectionMenu fromArray: #(		('set font... (k)'					offerFontMenu)		('set style... (K)'					changeStyle)		('set alignment...'				chooseAlignment)		-		('explain'						explain)		('pretty print'					prettyPrint)		('pretty print with color'		prettyPrintWithColor)		('file it in'						fileItIn)		('recognizer (r)'					recognizeCharacters)		('spawn (o)'						spawn)		-		('definition of word'				wordDefinition)		('verify spelling of word'		verifyWordSpelling)"		('spell check it'					spellCheckIt)	"		('translate it'					translateIt)		('choose language'				languagePrefs)		-		('browse it (b)'					browseIt)		('senders of it (n)'				sendersOfIt)		('implementors of it (m)'		implementorsOfIt)		('references to it (N)'			referencesToIt)		('selectors containing it (W)'		methodNamesContainingIt)		('method strings with it (E)'		methodStringsContainingit)		('method source with it'			methodSourceContainingIt)		-		('save contents to file...'			saveContentsInFile)		('send contents to printer'		sendContentsToPrinter)		('printer setup'					printerSetup)		-		('special menu...'				presentSpecialMenu)		('more...'						yellowButtonActivity))! !!PluggableTextMorph methodsFor: 'menu commands' stamp: 'tk 7/9/2000 22:38'!translateIt	textMorph editor translateIt! !!PopUpMenu class methodsFor: 'instance creation' stamp: 'tk 7/12/2000 17:29'!withCaption: cap chooseFrom: labels	"Simply put up a menu.  Get the args in the right order with the caption first.  labels may be either an array of items or a string with CRs in it.  May use backslashes for returns."	(labels isKindOf: String) 		ifTrue: [^ (self labels: labels withCRs lines: nil) startUpWithCaption: cap withCRs]		ifFalse: [^ (self labelArray: labels lines: nil) startUpWithCaption: cap withCRs]! !!Scamper methodsFor: 'document handling' stamp: 'tk 7/9/2000 23:10'!displayTextHtmlPage: newSource	"HTML page--format it"	| formatter bgimageUrl bgimageDoc bgimage |	currentUrl _ newSource url.	pageSource _ newSource content isoToSqueak.	self status: 'parsing...'.	document _ (HtmlParser parse: (ReadStream on: pageSource)).	self status: 'laying out...'.	formatter _ HtmlFormatter preferredFormatterClass new.	formatter browser: self.	formatter baseUrl: currentUrl.	document addToFormatter: formatter.	formattedPage _ formatter text.	(bgimageUrl _ document body background)		ifNotNil:			[bgimageDoc _ (bgimageUrl asUrlRelativeTo: currentUrl) retrieveContents.			[bgimage _ ImageReadWriter formFromStream: bgimageDoc contentStream binary]				ifError: [:err :rcvr | "ignore" bgimage _ nil]].	bgimage		ifNotNil: [backgroundColor _ bgimage]		ifNil: [backgroundColor _ Color fromString: document body bgcolor].	currentUrl fragment		ifNil: [ currentAnchorLocation _ nil ]		ifNotNil: [ currentAnchorLocation _				formatter anchorLocations 					at: currentUrl fragment asLowercase					ifAbsent: [ nil ] ].	self startDownloadingMorphState: (formatter incompleteMorphs).	self changeAll: 	#(currentUrl relabel hasLint lint backgroundColor formattedPage formattedPageSelection).	self status: 'done.'.	"pardon this horrible hack...(tk)"	(currentUrl authority beginsWith: 'ets.freetranslation.com') ifTrue: [		self status: 'done.**** Please Scroll Down To See Your Results ****'].	^true! !!Scamper class methodsFor: 'instance creation' stamp: 'tk 7/9/2000 21:42'!newOrExistingOn2: aStringOrUrl	| aUrl siteStr |	"If a Scamper is open on the same site, return it, else return a new Scamper.	"aUrl _ aStringOrUrl asUrl.siteStr _ aUrl schemeName, '://', aUrl authority.Smalltalk isMorphic ifTrue: [	World submorphsDo: [:m | 		((m isKindOf: SystemWindow) and: [m model isKindOf: Scamper]) ifTrue: [			(m model currentUrl asString beginsWith: siteStr) ifTrue: [					^ m model]]]].^ self new! !!Scamper class methodsFor: 'instance creation' stamp: 'tk 7/9/2000 21:43'!newOrExistingOn: aStringOrUrl	| aUrl siteStr |	"If a Scamper is open on the same site, return its SystemWindow, else return a new Scamper."aUrl _ aStringOrUrl asUrl.siteStr _ aUrl schemeName, '://', aUrl authority.Smalltalk isMorphic ifTrue: [	World submorphsDo: [:m | 		((m isKindOf: SystemWindow) and: [m model isKindOf: Scamper]) ifTrue: [			(m model currentUrl asString beginsWith: siteStr) ifTrue: [					^ m]]]].^ self new openAsMorph! !!PortugueseLexiconServer methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 14:48'!definition: theWord	"look this word up in the basic way.  Return nil if there is trouble accessing the web site."	| doc |	word _ theWord.	doc _ HTTPSocket 		httpGetDocument: 'http://www.priberam.pt/scripts/dlpouniv.dll' 		args: 'search_value=', (self class decodeAccents: word).	replyHTML _ (doc isKindOf: MIMEDocument)		ifTrue: [doc content]		ifFalse: [nil].	"self parseReply."	^ replyHTML! !!WordNet class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 17:31'!languagePrefs	| ch ll |	"Set preference of which natural language is primary. Look up definitions in it, and correct spelling in it.  Also, let user set languages to translate from and to."	Languages ifNil: [Languages _ #(English Portuguese).		CanTranslateFrom _ #(French German Spanish English Portuguese 			Italian Norwegian)].		"see www.freetranslation.com/"	ch _ PopUpMenu withCaption: 'Choose the natural language to use for:'			chooseFrom: 'word definition and spelling verification (', 					(Preferences valueOfFlag: #myLanguage) asString ,')...\',				'language to translate from (',					(Preferences valueOfFlag: #languageTranslateFrom) asString ,')...\',				'language to translate to (',					(Preferences valueOfFlag: #languageTranslateTo) asString ,')...\'.	ch = 1 ifTrue: [		ll _ PopUpMenu withCaption: 'The language for word definitions and spelling verification:'			chooseFrom: Languages.		ll > 0 ifTrue: [			^ Preferences setPreference: #myLanguage toValue: (Languages at: ll) asSymbol]].	ch = 2 ifTrue: [		ll _ PopUpMenu withCaption: 'The language to translate from:'			chooseFrom: CanTranslateFrom.		ll > 0 ifTrue: [			^ Preferences setPreference: #languageTranslateFrom 				toValue: (CanTranslateFrom at: ll) asSymbol]].	ch = 3 ifTrue: [		ll _ PopUpMenu withCaption: 'The language to translate to'			chooseFrom: CanTranslateFrom.		ll > 0 ifTrue: [			^ Preferences setPreference: #languageTranslateTo 				toValue: (CanTranslateFrom at: ll) asSymbol]]."Maybe let the user add another language when he knows ther server can take it.""	ch _ (PopUpMenu labelArray: Languages, {'other...'.			'Choose language to translate from...'})		startUpWithCaption: 'Choose the language of dictionary for word definitions.'.	ch = 0 ifTrue: [^ Preferences valueOfFlag: #myLanguage].	(ch <= Languages size) ifTrue: [ll _ Languages at: ch].	ch = (Languages size + 1) ifTrue: [		ll _ FillInTheBlank request: 'Name of the primary language'].	ll ifNotNil: [^ Preferences setPreference: #myLanguage toValue: ll asSymbol]."! !!WordNet class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 13:57'!lexiconServer	| nl |	"Look in Preferences to see what language the user wants, and what class knows about it."	nl _ Preferences valueOfFlag: #myLanguage.	nl == false ifTrue: [^ self].			"English, WordNet server"	nl == #English ifTrue: [^ self].		"English, WordNet server"	nl == #Portuguese ifTrue: [^ PortugueseLexiconServer].	"www.priberam.pt""	nl == #Deutsch ifTrue: [^ DeutschServerClass]. "	"class that knows about a server"	self inform: 'Sorry, no known online dictionary in that language.'.	^ self languagePrefs! !!WordNet class methodsFor: 'as yet unclassified' stamp: 'tk 7/9/2000 22:05'!openScamperOn: aWord	| aUrl scamperWindow |	"Open a Scamper web browser on the WordNet entry for this word.  If Scamper is already pointing at WordNet, use the same browser."	aUrl _ 'http://www.cogsci.princeton.edu/cgi-bin/webwn/', 		'?stage=1&word=', aWord.	scamperWindow _ Scamper newOrExistingOn: aUrl.	scamperWindow model jumpToUrl: aUrl asUrl.	scamperWindow activate.! !!WordNet class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 13:58'!verify: aWord	"See if this spelling is in the WordNet lexicon.  Return a string of success, no-such-word, or can't reach the server."	| aDef nl |		aDef _ self new.	(aDef definition: aWord) ifNil:		[^ 'Sorry, cannot reach that web site.  Task abandoned.(Make sure you have an internet connection.)'].	nl _ Preferences valueOfFlag: #myLanguage.	nl == false ifTrue: [nl _ #English].	(aDef parts) size = 0 		ifTrue: [^ 'Sorry, ', aWord, ' not found. (', nl, ' lexicon)']		ifFalse: [^ aWord, ' is spelled correctly.'].! !!PortugueseLexiconServer class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 14:48'!decodeAccents: appleLikeString	"change characters like �, to the form used in Portuguese"	| encodedStream rem |	encodedStream _ WriteStream on: (String new).		appleLikeString do: [ :c |		rem _ encodedStream position.		c == $� ifTrue: [encodedStream nextPut: (Character value: 237)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 225)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 233)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 231)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 227)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 243)].		c == $� ifTrue: [encodedStream nextPut: (Character value: 234)].		"and more, such as e with a backwards accent"		rem = encodedStream position ifTrue: [			encodedStream nextPut: c].		].	^encodedStream contents. ! !!PortugueseLexiconServer class methodsFor: 'as yet unclassified' stamp: 'tk 7/12/2000 14:48'!openScamperOn: aWord	| aUrl scamperWindow |	"Open a Scamper web browser on the web dictionary entry for this word.  If Scamper is already pointing at it, use the same browser.  Special code for this server."	aUrl _ 'http://www.priberam.pt/scripts/dlpouniv.dll', 		'?search_value=', (self decodeAccents: aWord).	scamperWindow _ Scamper newOrExistingOn: aUrl.	scamperWindow model jumpToUrl: aUrl asUrl.	scamperWindow activate.! !PortugueseLexiconServer removeSelector: #decodeAccents:!