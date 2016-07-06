'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5497] on 31 October 2003 at 9:26:20 am'!"Change Set:		Babel-FileListServices-dgdDate:			31 October 2003Author:			Diego Gomez Deck <DiegoGomezDeck@ConsultAr.com>New file service to merge translation filesNOTE: Depends on Babel-dgd31/October:- added the file service available for "!!Language class methodsFor: 'class initialization' stamp: 'dgd 10/4/2003 14:21'!initialize	"initialize the receiver"	" 	Language initialize.	"	FileList registerFileReader: self! !!Language class methodsFor: 'file-list services' stamp: 'dgd 10/31/2003 09:23'!fileReaderServicesForFile: fullName suffix: suffix 	"Answer the file services associated with given file"	^ (suffix = 'translation') | (suffix = '*')		ifTrue: [{self serviceMergeLanguageTranslations}]		ifFalse: [#()]! !!Language class methodsFor: 'file-list services' stamp: 'dgd 10/4/2003 14:50'!mergeTranslationFileNamed: fileFullNameString 	"merge the translation in the file named fileFullNameString"	| stream |	stream := FileStream oldFileNamed: fileFullNameString.	self mergeFromStream: stream named: stream localName sansPeriodSuffix.	stream close! !!Language class methodsFor: 'file-list services' stamp: 'dgd 10/4/2003 14:34'!serviceMergeLanguageTranslations	"Answer a service for merging of translation files"	^ SimpleServiceEntry		provider: self		label: 'merge the translation file'		selector: #mergeTranslationFileNamed:		description: 'merge the translation file into the language named like the file'		buttonLabel: 'merge'! !!Language class methodsFor: 'file-list services' stamp: 'dgd 10/4/2003 14:24'!services	"Answer potential file services associated with this class"	^ {self serviceMergeLanguageTranslations}! !Language initialize!!Language class reorganize!('accessing - instances' clearAllInstances clearInstance instance newInstance)('accessing - languages' availableLanguageSymbols availableLanguages defaultLanguage languageNamed: languageNamed:ifNone:)('applying' applyTranslations recreateFlaps)('class initialization' initialize)('file-list services' fileReaderServicesForFile:suffix: mergeTranslationFileNamed: serviceMergeLanguageTranslations services)('instance creation' new)('merging' mergeFromStream:named:)!