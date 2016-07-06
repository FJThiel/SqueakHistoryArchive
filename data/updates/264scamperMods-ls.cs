'From Squeak 2.1 of June 30, 1998 on 15 September 1998 at 12:11:09 am'!"Change Set:		scamperMods-lsDate:			14 September 1998Author:			Lex SpoonA myriad of mods to the Web Browser and subsystem.  I don't remember what the exact state things were in last week, but at this point the following notably works:	1. If an error occurs downloading a URL, catch it and display it in the pane, instead of throwing up a Debugger window.  The Debugger ends up running in the background thread, which means there are two threads running the Morphic event loop simultaneously	2. Images in a web page that are beindownloaded, take advantage of the size hints provided in HTML; thus they don't have to resize themselves when the image finishes downloading.	3. Links into the middle of a web page, for instance 'http:myPage#part2', work now.  	4. Links like 'http:#part2' work, ie URLs that have a fragment but no path.  The path is assumed to be the same as the path of the original page's URL.	5. Scamper offers to save binary objects to a file.	6. DoIts work from a Scamper screen.  So you can highlight things off a web page and hit alt-d.	7.  The Lint button is removed.  The linting code never really got advanced enough that this is useful.	8. one-line form inputs can be given a different delt size by the 'size' attribute.	9. several comment adjustments.	10. small tweaks to how tables are displayed.  They still look terrible, but might be ittle more readable.	11. TextURLs outside of a browser will spawn a new browser for viewing the link. "!Morph subclass: #DownloadingImageMorph	instanceVariableNames: 'url altText defaultExtent image downloadQueue '	classVariableNames: ''	poolDictionaries: ''	category: 'HTML-Formatter'!Object subclass: #HtmlFormatter	instanceVariableNames: 'browser baseUrl formDatas outputStream preformattedLevel indentLevel boldLevel italicsLevel underlineLevel strikeLevel centerLevel urlLink listLengths listTypes precedingSpaces precedingNewlines morphsToEmbed incompleteMorphs anchorLocations '	classVariableNames: 'CSNonSeparators CSSeparators '	poolDictionaries: ''	category: 'HTML-Formatter'!HtmlTableDataItem subclass: #HtmlTableHeader	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'HTML-Parser-Entities'!Model subclass: #Scamper	instanceVariableNames: 'status currentUrl pageSource document formattedPage downloadingProcess documentQueue recentDocuments currentAnchorLocation '	classVariableNames: 'StartUrl '	poolDictionaries: ''	category: 'NetTools-Scamper'!!AttributedTextStream methodsFor: 'access' stamp: 'ls 9/10/1998 03:36'!size	"number of characters in the stream so far"	^characters size! !!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/9/1998 06:59'!defaultExtent: aPoint	"set the size to use when the image hasn't yet downloaded"	defaultExtent _ aPoint! !!DownloadingImageMorph methodsFor: 'as yet unclassified' stamp: 'ls 9/11/1998 21:05'!setContents	"set up our morphic contents"	| stringMorph imageMorph outlineMorph extent |	self removeAllMorphs.	image		ifNil: [ 			altText = '' ifTrue: [ "don't display anything..." ^self ].			stringMorph _ StringMorph new.			stringMorph contents: altText.			stringMorph position: self position+(2@2).			self addMorph: stringMorph.			outlineMorph _ RectangleMorph new.			outlineMorph borderWidth: 1.			outlineMorph color: Color transparent.			outlineMorph position: self position.			"figure out how big to make the box"			extent _ defaultExtent ifNil: [ 0 @ 0 ].			stringMorph width + 4 > extent x ifTrue: [				extent _ (stringMorph width + 4) @ extent y ].			stringMorph height + 4 > extent y ifTrue: [				extent _ extent x @ (stringMorph height + 4) ].			outlineMorph extent: extent.			self addMorph: outlineMorph.			self extent: outlineMorph extent ]		ifNotNil: [			imageMorph _ ImageMorph new.			imageMorph image: image.			imageMorph position: self position.			self addMorph: imageMorph.			imageMorph extent ~= self extent ifTrue: [				self extent: imageMorph extent ] ].! !!HandMorph methodsFor: 'meta menu' stamp: 'ls 9/14/1998 23:21'!openWebBrowser	Scamper openAsMorph ! !!HierarchicalUrl methodsFor: 'parsing' stamp: 'ls 9/10/1998 03:20'!privateInitializeFromText: aString relativeTo: aUrl	| remainder ind nextTok s |	remainder _ aString.	"set the scheme"	schemeName _ aUrl schemeName.	"a leading // means the authority is specified, meaning it is absolute"	(remainder beginsWith: '//') ifTrue: [		^self privateInitializeFromText: aString ].	"otherwise, use the same authority"	authority _ aUrl authority.	"get the query"	ind _ remainder indexOf: $?.	ind > 0 ifTrue: [		query _ (remainder copyFrom: ind+1 to: remainder size).		remainder _ remainder copyFrom: 1 to: ind-1 ]. 	"get the path"	remainder isEmpty ifTrue: [ 		path _ aUrl path ]	ifFalse: [		(remainder beginsWith: '/')			ifTrue: [ path _ OrderedCollection new ]			ifFalse: [ path _ aUrl path shallowCopy.				path size > 0 ifTrue: [ path removeLast ] ]. 				s _ ReadStream on: remainder.		[ 			s peek = $/ ifTrue: [ s next ].			nextTok _ WriteStream on: String new.			[ s atEnd or: [ s peek = $/ ] ] whileFalse: [ nextTok nextPut: s next ].			nextTok _ nextTok contents unescapePercents.			nextTok = '..' 				ifTrue: [ path size > 0 ifTrue: [ path removeLast ] ]				ifFalse: [ nextTok ~= '.' ifTrue: [ path add: nextTok ] ].			s atEnd 		] whileFalse.		path isEmpty ifTrue: [ path add: '' ]	. ].! !!HtmlAnchor methodsFor: 'formatting' stamp: 'ls 9/10/1998 03:22'!addToFormatter: formatter	| href name |	name _ self getAttribute: 'name'.	name ifNotNil: [		formatter noteAnchorStart: name ].	href _ self getAttribute: 'href'.	href isNil		ifTrue: [ super addToFormatter: formatter ]		ifFalse: [ 				formatter startLink: href.			super addToFormatter: formatter.			formatter endLink: href. ].! !!HtmlFormatter methodsFor: 'access' stamp: 'ls 9/10/1998 03:20'!anchorLocations	"return a dictionary mapping lowercase-ed anchor names into the integer positions they are located at in the text"	^anchorLocations! !!HtmlFormatter methodsFor: 'private-initialization' stamp: 'ls 9/10/1998 03:17'!initialize	outputStream _ AttributedTextStream new.	preformattedLevel _ 0.	indentLevel _ boldLevel _ italicsLevel _ underlineLevel _ strikeLevel _ centerLevel _ 0.	listLengths _ OrderedCollection new.	listTypes _ OrderedCollection new.	formDatas _ OrderedCollection new.	precedingSpaces _ 0.	precedingNewlines _ 1000.   "more than will ever be asked for"	morphsToEmbed _ OrderedCollection new.	incompleteMorphs _ OrderedCollection new.	anchorLocations _ Dictionary new.	outputStream nextPut: Character cr.! !!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/12/1998 00:14'!addChar: c	"add a single character, updating all the tallies"	"add the character to the output"	outputStream nextPut: c.	"update counters for preceeding spaces and preceding newlines"	(c = Character space or: [ c = Character tab ]) 	ifTrue: [ precedingSpaces _ precedingSpaces+1.  precedingNewlines _ 0 ]	ifFalse: [		(c = Character cr) ifTrue: [			precedingSpaces _ 0.			precedingNewlines _ precedingNewlines + 1 ]		ifFalse: [			precedingSpaces _ precedingNewlines _ 0 ] ].! !!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/12/1998 00:15'!hr	"add an (attempt at a) horizontal rule"	self ensureNewlines: 1.	25 timesRepeat: [ self addChar: $- ].	self ensureNewlines: 1.	precedingSpaces _ 0.	precedingNewlines _ 1000.    "pretend it's the top of a new page"! !!HtmlFormatter methodsFor: 'formatting commands' stamp: 'ls 9/10/1998 03:26'!noteAnchorStart: anchorName	"note that an anchor starts at this point in the output"	anchorLocations at: anchorName asLowercase put: outputStream size! !!HtmlImage methodsFor: 'attributes' stamp: 'ls 9/9/1998 07:01'!imageExtent	"the image extent, according to the WIDTH and HEIGHT attributes.  returns nil if either WIDTH or HEIGHT is not specified"	| widthText heightText |	widthText _ self getAttribute: 'width' ifAbsent: [ ^nil ].	heightText _ self getAttribute: 'height' ifAbsent: [ ^nil ].	^ widthText asNumber @ heightText asNumber! !!HtmlImage methodsFor: 'formatting' stamp: 'ls 9/9/1998 07:05'!addToFormatter: formatter	| morph url |	self src isNil ifTrue: [ ^self ].	url _ self src.	formatter baseUrl ifNotNil: [ 		url _ url asUrlRelativeTo: formatter baseUrl ].	morph _ DownloadingImageMorph new.	morph defaultExtent: self imageExtent.	morph altText: self alt.	morph url: url.	formatter addIncompleteMorph: morph.! !!HtmlInput methodsFor: 'formatting' stamp: 'ls 9/11/1998 23:42'!addToFormatter: formatter	"is it a submit button?"	| inputMorph formData size |	self type = 'submit' ifTrue: [		formatter addMorph: ((PluggableButtonMorph on: formatter currentFormData getState: nil action: #submit) label: (self getAttribute: 'value' default: 'Submit')).		^self ].	self type = 'image' ifTrue: [		"fake it"		value _ self getAttribute: 'value' default: 'Submit'.		formData _ formatter currentFormData.		formatter addMorph: ((PluggableButtonMorph on: formData getState: nil action: #submit) label: value).		formData addInput: (HiddenInput name: (value,'.x') value: '0').		formData addInput: (HiddenInput name: (value,'.y') value: '0').			^self ].	self type = 'text' ifTrue: [		inputMorph _ PluggableTextMorph on: StringHolder new text: #contents accept: #acceptContents:.		size _ self getAttribute: 'size' default: '12'.		size _ size asNumber.		inputMorph extent: (size*10@20).		formatter addMorph: inputMorph.		formatter currentFormData addInput: (TextInput name: self name  defaultValue: self defaultValue  textMorph: inputMorph).		^self ].	self type = 'hidden' ifTrue: [		formatter currentFormData addInput: (HiddenInput name: self name  value: self defaultValue).		^self ].	self type = 'radio' ifTrue: [ 		^self addRadioButtonToFormatter: formatter ].	formatter addString: '[form input of type: ', self type, ']'.! !!HtmlTable methodsFor: 'formatting' stamp: 'ls 9/12/1998 00:52'!addToFormatter: formatter	formatter ensureNewlines: 1.	super addToFormatter: formatter.	formatter ensureNewlines: 1.! !!HtmlTableHeader commentStamp: '<historical>' prior: 0!a TH tag.  Currently treated the same as a TD!!HtmlTableHeader reorganize!('accessing' tagName)!!HtmlTableHeader methodsFor: 'accessing' stamp: 'ls 9/12/1998 00:51'!tagName	^'th'! !!HtmlTableRow methodsFor: 'formatting' stamp: 'ls 9/12/1998 00:52'!addToFormatter: formatter	super addToFormatter: formatter.	formatter ensureNewlines: 1.! !!HtmlTag class methodsFor: 'parser support' stamp: 'ls 9/12/1998 00:52'!entityClasses	"a Dictionary mapping tag names into the correct entity class"	"EntityClasses _ nil"	EntityClasses isNil ifFalse: [ ^EntityClasses ].	EntityClasses _ Dictionary new.	#( 		frameset	HtmlFrame		frame	HtmlFrame		title		HtmlTitle		style	HtmlStyle		meta	HtmlMeta		p		HtmlParagraph		form	HtmlForm		blockquote	HtmlBlockQuote		input	HtmlInput		textarea	HtmlTextArea		select	HtmlSelect		optgroup	HtmlOptionGroup		option		HtmlOption		img		HtmlImage		a		HtmlAnchor		br		HtmlBreak		li		HtmlListItem		dd		HtmlDefinitionDefinition		dt		HtmlDefinitionTerm		ol		HtmlOrderedList		ul		HtmlUnorderedList		dl		HtmlDefinitionList		h1		HtmlHeader		h2		HtmlHeader		h3		HtmlHeader		h4		HtmlHeader		h5		HtmlHeader		h6		HtmlHeader		hr		HtmlHorizontalRule		strong	HtmlBoldEntity		b		HtmlBoldEntity		em		HtmlItalicsEntity		i		HtmlItalicsEntity		dfn 	HtmlItalicsEntity		u		HtmlUnderlineEntity 		tt		HtmlFixedWidthEntity		kbd		HtmlFixedWidthEntity				strike	HtmlStrikeEntity		big		HtmlBiggerFontEntity		small	HtmlSmallerFontEntity		sub		HtmlSubscript		sup		HtmlSuperscript		font	HtmlFontEntity		pre		HtmlPreformattedRegion 		table	HtmlTable		tr		HtmlTableRow		td		HtmlTableDataItem 		th		HtmlTableHeader		) pairsDo: [ 			:tagName :className |			EntityClasses at: tagName asString put: (Smalltalk at: className) ].	^EntityClasses ! !!MIMEDocument commentStamp: '<historical>' prior: 0!a MIME object, along with its type and the URL it was found at (if any)!!Scamper methodsFor: 'not yet categorized' stamp: 'ls 9/14/1998 20:15'!doItContext	^nil! !!Scamper methodsFor: 'not yet categorized' stamp: 'ls 9/14/1998 20:15'!doItReceiver	^nil! !!Scamper methodsFor: 'lint' stamp: 'ls 9/14/1998 20:38'!lint	"return a string describing any questionable HTML that was noticed in the current page"	"(not currently very comprehensive)"	document ifNil: [ ^'' ].	^document lint! !!Scamper methodsFor: 'changing page' stamp: 'ls 9/13/1998 06:46'!displayDocument: mimeDocument	"switch to viewing the given MIMEDocument"	| newUrl  newSource handled  formatter fileName file |	newUrl _ mimeDocument url.	newSource _ mimeDocument.	handled _ false.	"add it to the history"	recentDocuments removeAllSuchThat: [ :d | d url = mimeDocument url ].	recentDocuments addLast: mimeDocument.	recentDocuments size > 20 ifTrue: [ recentDocuments removeFirst ].			newSource contentType = 'text/html' ifTrue: [		"HTML page--format it"		currentUrl _ newUrl.		pageSource _ newSource content.		self status: 'parsing...'.		document _ (HtmlParser parse: (ReadStream on: pageSource)).		self status: 'laying out...'.		formatter _ HtmlFormatter new.		formatter browser: self.		formatter baseUrl: currentUrl.		document addToFormatter: formatter.		formattedPage _ formatter text.		currentUrl fragment			ifNil: [ currentAnchorLocation _ nil ]			ifNotNil: [ currentAnchorLocation _				formatter anchorLocations 					at: currentUrl fragment asLowercase					ifAbsent: [ nil ] ].		self startDownloadingMorphState: (formatter incompleteMorphs).		handled _ true.		self status: 'sittin'.		self changed: #currentUrl.			self changed: #title.		self changed: #hasLint.		self changed: #lint.		self changed: #formattedPage.		self changed: #formattedPageSelection.  ].	(#('audio/midi' 'audio/x-midi') includes: newSource contentType) ifTrue: [		MIDIFileReader playStream: (RWBinaryOrTextStream with: newSource content) reset binary.		self status: 'sittin'.		handled _ true. ].	(handled not and: [ newSource mainType = 'text']) ifTrue: [		"treat as plain text"		pageSource _ newSource content.		document _ nil.		formattedPage _ pageSource withSqueakLineEndings.		currentUrl _ newUrl.				self status: 'sittin'.		self changed: #currentUrl.		self changed: #title.		self changed: #hasLint.		self changed: #lint.		self changed: #formattedPage.  		handled _ true].	handled ifFalse: [		"offer to save it to a file"		self status: 'sittin'.		(self confirm: 'unkown content-type ', newSource contentType,'--Would you like to save to a file?') ifFalse: [			^self ].		fileName _ ''.		[			fileName _ FillInTheBlank request: 'file to save in' initialAnswer: fileName.			fileName isEmpty ifTrue: [ ^self ].			file _ FileStream fileNamed: fileName.			file == nil		] whileTrue.		file reset.		file binary.		file nextPutAll: newSource content.		file close.	].! !!Scamper methodsFor: 'changing page' stamp: 'ls 9/13/1998 07:14'!jumpToAbsoluteUrl: urlText	"start downloading a new page.  The page source is downloaded in a background thread"	|  newUrl newSource |	self stopEverything.	"get the new url"	newUrl _ urlText asUrl.	"if it fundamentally doesn't fit the pages-and-contents model used internally, spawn off an external viewer for it"	newUrl hasContents ifFalse: [ newUrl activate.  ^true ].	"fork a Process to do the actual downloading, parsing, and formatting.  It's results will be picked up in #step"	self status: 'downloading ', newUrl toText, '...'.	downloadingProcess _ [ 	  	newSource _ [ newUrl retrieveContentsForBrowser: self ] ifError: [ :msg :ctx |			MIMEDocument contentType: 'text/plain' content: msg ].		newSource 			ifNil: [ newSource _ MIMEDocument contentType: 'text/plain' content: 'Error retrieving this URL' ].			newSource url ifNil: [				newSource _ MIMEDocument contentType: newSource contentType  content: newSource content  url: newUrl ].			documentQueue nextPut: newSource.			downloadingProcess _ nil.	] newProcess.	downloadingProcess resume.	^true! !!Scamper methodsFor: 'access' stamp: 'ls 9/10/1998 03:38'!formattedPageSelection	currentAnchorLocation ifNil: [ ^0 to: -1 ].	^currentAnchorLocation to: currentAnchorLocation! !!Scamper methodsFor: 'user interface' stamp: 'ls 9/14/1998 23:20'!openAsMorph	"open a set of windows for viewing this browser"	|win |	"create a window for it"	win _ SystemWindow labelled: 'Scamper'.	win model: self.	win setProperty: #webBrowserView toValue: true.	"create a title view"	win addMorph: (PluggableTextMorph on: self text: #title accept: nil) frame: (0.0@0 extent: 1@0.1).	"create a view of the current url"	"win addMorph: (RectangleMorph new) frame: (0@0.1 extent: 0.3@0.1)."	win addMorph: (PluggableTextMorph on: self text: #currentUrl accept: #jumpToAbsoluteUrl:) frame: (0@0.1 extent: 1@0.1).	"create a status view"	win addMorph: (PluggableTextMorph on: self text: #status accept: nil) frame: (0@0.9 extent: 1.0@0.1).	"create the text area"	win addMorph: (PluggableTextMorph on: self text: #formattedPage accept: nil readSelection: #formattedPageSelection menu: #menu:shifted:)		frame: (0@0.2 extent: 1@0.7).		win openInWorld.	^win! !!Scamper class methodsFor: 'instance creation' stamp: 'ls 9/14/1998 23:21'!openAsMorph	"Scamper openAsMorph"	^self new openAsMorph! !HtmlAnchor removeSelector: #initialize:!"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one.""cause the name-->tag-class dictionary to be rebuilt"HtmlTag initialize.!