"Change Set:		SubjectPrefixForCS-dewDate:			27 August 2000Author:			Doug WayFixes/enhances the 'mail to list' option in the change sorter so that the email will have a subject prefix of [FIX], [ENH] or [GOODIE], so it will get picked up by the Bug Fixes Archive.  A PopUpMenu appears offering the three options or none of the above.  (Note that this changeset was submitted from a change sorter in Squeak 2.9alpha using the enhanced 'mail to list'.)Also fixes MailMessage>>fieldsAsMimeHeader so that the email subject is not forced to lowercase.  (Side note: I don't understand why the MIMEHeaderValue objects are always lowercase in the first place.)"!!ChangeSet methodsFor: 'fileIn/Out' stamp: 'dew 8/26/2000 21:45'!chooseSubjectPrefixForEmail	| subjectIndex |	subjectIndex _		(PopUpMenu labels: 'Bug fix [FIX]\Enhancement [ENH]\Goodie [GOODIE]\None of the above (will not be archived)' withCRs)			startUpWithCaption: 'What type of change set\are you submitting to the list?' withCRs.	^ #('[CS] ' '[FIX] ' '[ENH] ' '[GOODIE] ' '[CS] ') at: subjectIndex + 1! !!ChangeSet methodsFor: 'fileIn/Out' stamp: 'dew 8/27/2000 14:25'!mailOut	"File out the receiver, to a file whose name is a function of the           	change-set name and either of the date & time or chosen to have a	unique numeric tag, depending on the preference	'sequentialChangeSetRevertableFileNames'."	| subjectPrefix slips messageStrm message compressBuffer compressStream data compressedStream compressTarget |	(Smalltalk includesKey: #Celeste)		ifFalse: [^ self notify: 'no mail reader present'].	subjectPrefix _ self chooseSubjectPrefixForEmail.	self checkForConversionMethods.	Cursor write		showWhile: 			["Prepare the message"			messageStrm _ WriteStream on: (String new: 30).			messageStrm nextPutAll: 'From: ';			 nextPutAll: Celeste userName;			 cr;			 nextPutAll: 'To: squeak@cs.uiuc.edu';			 cr;			 nextPutAll: 'Subject: ';			 nextPutAll: subjectPrefix;			 nextPutAll: name;			 cr;			 nextPutAll: 'from preamble:';			 cr;			 cr.			self fileOutPreambleOn: messageStrm.			"Prepare the gzipped data"			message _ MailMessage from: messageStrm contents.			message _ MailMessage from: message asMultipartText.			data _ WriteStream on: String new.			data header.			self fileOutPreambleOn: data.			self fileOutOn: data.			self fileOutPostscriptOn: data.			data trailer.			data _ ReadStream on: data contents.			compressBuffer _ ByteArray new: 1000.			compressStream _ GZipWriteStream on: (compressTarget _ WriteStream on: (ByteArray new: 1000)).			[data atEnd]				whileFalse: [compressStream nextPutAll: (data nextInto: compressBuffer)].			compressStream close.			compressedStream _ ReadStream on: compressTarget contents asString.			CelesteComposition				openForCeleste: Celeste current 				initialText: (message asTextEncodingNewPart: compressedStream named: name , '.cs.gz')].	Preferences suppressCheckForSlips ifTrue: [^ self].	slips _ self checkForSlips.	(slips size > 0 and: [self confirm: 'Methods in this fileOut have haltsor references to the Transcriptor other ''slips'' in them.Would you like to browse them?'])		ifTrue: [Smalltalk browseMessageList: slips name: 'Possible slips in ' , name]! !!MailMessage methodsFor: 'printing/formatting' stamp: 'dew 8/27/2000 02:07'!fieldsAsMimeHeader	"return the entire header in proper MIME format"	| strm |	strm _ WriteStream on: (String new: 100).	self fields associationsDo: [:e | strm nextPutAll: e key;		 nextPutAll: ': ';		 "circumvent the forced lowercasing of the subject MimeHeader"		 nextPutAll: (e key = 'subject' ifTrue: [subject] ifFalse: [e value asHeaderValue]);		 cr]. 	^ strm contents! !