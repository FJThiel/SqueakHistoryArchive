'From Squeak3.8-Nihongo of 25 February 2005 [latest update: #2] on 1 March 2005 at 12:52:53 pm'!"Change Set:		LangEditorMimeExportDate:			1 March 2005Author:			Yoshiki OhshimaAdd a feature to remember the changed keys since last save."!SystemWindow subclass: #LanguageEditor	instanceVariableNames: 'translator selectedTranslation selectedTranslations selectedUntranslated translationsList untranslatedList translationText translationsFilter untranslatedFilter newerKeys '	classVariableNames: 'CheckMethods '	poolDictionaries: ''	category: 'Multilingual-Editor'!!LanguageEditor methodsFor: 'accessing' stamp: 'yo 3/1/2005 12:44'!translation: aStringOrText 	"change the translation for the selected phrase"	| phrase |	self selectedTranslation isZero		ifTrue: [^ self].	phrase _ self translations at: self selectedTranslation.	translator		phrase: phrase		translation: aStringOrText asString.	newerKeys add: phrase.	^ true! !!LanguageEditor methodsFor: 'initialization' stamp: 'yo 3/1/2005 12:33'!initializeNewerKeys	newerKeys _ Set new.! !!LanguageEditor methodsFor: 'initialization' stamp: 'yo 3/1/2005 12:33'!initializeOn: aLanguage 	"initialize the receiver on aLanguage"	""	selectedTranslation := 0.	selectedUntranslated := 0.	selectedTranslations := IdentitySet new.	""	translator := aLanguage.	""	self setLabel: 'Language editor for: ' translated , self translator name.	""	self initializeToolbars.	self initializePanels.	self initializeStatusbar.	self initializeNewerKeys.! !!LanguageEditor methodsFor: 'gui methods' stamp: 'yo 3/1/2005 12:27'!codeSelectedTranslationAsMimeString	| keys code tmpStream s2 gzs cont |	keys := selectedTranslations				collect: [:key | self translations at: key].	code := String				streamContents: [:aStream | self translator fileOutOn: aStream keys: keys].	tmpStream _ MultiByteBinaryOrTextStream on: ''.	tmpStream converter: UTF8TextConverter new.	tmpStream nextPutAll: code.	s2 _ RWBinaryOrTextStream on: ''.	gzs := GZipWriteStream on: s2.	tmpStream reset.	gzs nextPutAll: (tmpStream binary contentsOfEntireFile asString) contents.	gzs close.	s2 reset.	cont _ String streamContents: [:strm |		strm nextPutAll: 'NaturalLanguageTranslator loadForLocaleIsoString: '.		strm nextPut: $'.		strm nextPutAll: translator localeID isoString.		strm nextPut: $'.		strm nextPutAll: ' fromGzippedMimeLiteral: '.		strm nextPut: $'.		strm nextPutAll: (Base64MimeConverter mimeEncode: s2) contents.		strm nextPutAll: '''.!!'.		strm cr.	].		(StringHolder new contents: cont)		openLabel: 'exported codes in Gzip+Base64 encoding'! !!LanguageEditor methodsFor: 'gui methods' stamp: 'yo 3/1/2005 12:32'!phrase: phraseString translation: translationString 	"set the models's translation for phraseString"	self translator phrase: phraseString translation: translationString.	self changed: #translations.	self changed: #untranslated.	newerKeys add: phraseString.! !!LanguageEditor methodsFor: 'gui methods' stamp: 'yo 3/1/2005 12:36'!resetNewerKeys	self initializeNewerKeys.! !!LanguageEditor methodsFor: 'gui methods' stamp: 'tak 11/9/2004 18:39'!saveToFile	"save the translator to a file"	| fileName |	fileName := FillInTheBlank request: 'file name' translated initialAnswer: translator localeID isoString , '.translation'.	(fileName isNil			or: [fileName isEmpty])		ifTrue: [""			self beep.			^ self].	""Cursor wait		showWhile: [	self translator saveToFileNamed: fileName]! !!LanguageEditor methodsFor: 'gui methods' stamp: 'yo 3/1/2005 12:40'!selectNewerKeys	| translations index |	self deselectAllTranslation.	translations _ self translations.	newerKeys do: [:k |		index _ translations indexOf: k ifAbsent: [0].		index > 0 ifTrue: [			self selectedTranslationsAt: index put: true		].	].! !!LanguageEditor methodsFor: 'gui methods' stamp: 'yo 3/1/2005 12:49'!translationsMenu: aMenu 	^ aMenu add: 'remove (x)' translated action: #removeTranslation;		 add: 'where (E)' translated action: #browseMethodsWithTranslation;		 add: 'select all' translated action: #selectAllTranslation;		 add: 'deselect all' translated action: #deselectAllTranslation;		 add: 'select changed keys' translated action: #selectNewerKeys;		 add: 'export selection' translated action: #codeSelectedTranslation;		 add: 'export selection in do-it form' translated action: #codeSelectedTranslationAsMimeString;		 add: 'reset changed keys' translated action: #resetNewerKeys;		 yourself! !SystemWindow subclass: #LanguageEditor	instanceVariableNames: 'translator selectedTranslation selectedTranslations selectedUntranslated translationsList untranslatedList translationText translationsFilter untranslatedFilter newerKeys'	classVariableNames: 'CheckMethods'	poolDictionaries: ''	category: 'Multilingual-Editor'!