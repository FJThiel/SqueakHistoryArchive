'From Squeak3.7-m17n of 30 June 2004 [latest update: #6] on 26 July 2004 at 11:20:05 pm'!"Change Set:		isoSqueakSendersDate:			26 July 2004Author:			Yoshiki OhshimaRemove the unnecessary sends of isoToSqueak and squeakToIso for the contents from/to the Internet."!!MailComposition methodsFor: 'access' stamp: 'yo 7/26/2004 22:06'!messageText	"return the current text"	^messageText.! !!MailComposition methodsFor: 'access' stamp: 'yo 7/26/2004 22:47'!messageText: aText	"change the current text"	messageText _ aText.	self changed: #messageText.	^true! !!MailMessage methodsFor: 'printing/formatting' stamp: 'yo 7/26/2004 22:06'!bodyTextFormatted	"Answer a version of the text in my body suitable for display.  This will parse multipart forms, decode HTML, and other such things"	"check for multipart"	self body isMultipart ifTrue: [		"check for alternative forms"		self body isMultipartAlternative ifTrue: [			"it's multipart/alternative.  search for a part that we can display, biasing towards nicer formats"			#('text/html' 'text/plain') do: [ :format |				self parts do: [ :part |					part body contentType = format ifTrue: [ ^part bodyTextFormatted ] ] ].			"couldn't find a desirable part to display; just display the first part"			^self parts first bodyTextFormatted ].		"not alternative parts.  put something for each part"		^Text streamContents: [ :str |			self parts do: [ :part |				((#('text' 'multipart') includes: part body mainType) or: 					[ part body contentType = 'message/rfc822'])				ifTrue: [					"try to inline the message part"					str nextPutAll: part bodyTextFormatted. ]				ifFalse: [ 					|descript |					str cr.					descript := part name ifNil: [ 'attachment' ].					str nextPutAll: (Text string: '[', descript, ']'  attribute: (TextMessageLink message: part)). ] ] ]. ].	"check for HTML"	(self body contentType = 'text/html') ifTrue: [		Smalltalk at: #HtmlParser ifPresentAndInMemory: [ :htmlParser |			^(htmlParser parse: (ReadStream on: body content)) formattedText		]	].	"check for an embedded message"	self body contentType = 'message/rfc822' ifTrue: [		^(MailMessage from: self body content) formattedText ].	"nothing special--just return the text"	^body content.! !!MailMessage methodsFor: 'printing/formatting' stamp: 'yo 7/26/2004 22:06'!cleanedHeader	"Reply with a cleaned up version email header.  First show fields people would normally want to see (in a regular order for easy browsing), and then any other fields not explictly excluded"	| new priorityFields omittedFields |	new _ WriteStream on: (String new: text size).	priorityFields _ #('Date' 'From' 'Subject' 'To' 'Cc').	omittedFields _ MailMessage omittedHeaderFields.	"Show the priority fields first, in the order given in priorityFields"	priorityFields do: [ :pField |		"We don't check whether the priority field is in the omitted list!!"		self headerFieldsNamed: pField do:			[: fValue | new nextPutAll: pField, ': ', fValue decodeMimeHeader; cr]].	"Show the rest of the fields, omitting the uninteresting ones and ones we have already shown"	omittedFields _ omittedFields, priorityFields.	self fieldsFrom: (ReadStream on: text) do:		[: fName : fValue |		((fName beginsWith: 'x-') or:			[omittedFields anySatisfy: [: omitted | fName sameAs: omitted]])				ifFalse: [new nextPutAll: fName, ': ', fValue; cr]].	^new contents! !!MailSender class methodsFor: 'as yet unclassified' stamp: 'yo 7/26/2004 22:47'!setUserName	"Change the user's email name for use in composing messages."	(UserName isNil) ifTrue: [UserName _ ''].	UserName _ FillInTheBlank		request: 'What is your email address?\(This is the address other people will reply to you)' withCRs		initialAnswer: UserName.	UserName ifNotNil: [UserName _ UserName]! !!SMPackage methodsFor: 'accessing' stamp: 'yo 7/26/2004 22:05'!fullDescription	"Return a full textual description of the package. 	Most of the description is taken from the last release."	| s tab |	s := TextStream				on: (Text new: 400).	tab := String with: Character tab.	self		describe: name		withBoldLabel: 'Name:' , tab , tab		on: s.	summary isEmptyOrNil		ifFalse: [self				describe: summary				withBoldLabel: 'Summary:' , tab				on: s].	author isEmptyOrNil		ifFalse: [s				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Author:'];				 tab;				 tab.			s				withAttribute: (PluggableTextAttribute						evalBlock: [self userInterface sendMailTo: author regardingPackageRelease: self lastRelease])				do: [s nextPutAll: author];				 cr].	self owner		ifNotNil: [s				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Owner:'];				 tab.			s				withAttribute: (PluggableTextAttribute						evalBlock: [self userInterface sendMailTo: self owner email regardingPackageRelease: self lastRelease])				do: [s nextPutAll: self owner email];				 cr].	self maintainers isEmpty		ifFalse: [s				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Co-maintainers:'];				 tab.			self maintainers do: [:com |				s withAttribute: (PluggableTextAttribute					evalBlock: [self userInterface								sendMailTo: com email								regardingPackageRelease: self lastRelease])				do: [s nextPutAll: com email];				 cr]].	categories isEmptyOrNil		ifFalse: [s cr;				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Categories: ']; cr.			(self categories collect: [:c | c path]) asSortedCollection				do: [:cName |					s tab; withAttribute: TextEmphasis bold do: [s nextPutAll: cName]].		s cr]."Do: [:c |					s tab;						withAttribute: TextEmphasis italic						do: [c parentsDo: [:p | s nextPutAll: p name; nextPutAll: '/'].							s nextPutAll: c name]; nextPutAll: ' - ' , c summary; cr].			s cr]."	self currentVersion isEmptyOrNil		ifTrue: [self				describe: self smartVersion				withBoldLabel: 'Calculated version: '				on: s]		ifFalse: [self				describe: self currentVersion				withBoldLabel: 'Current version: '				on: s].	self versionComment isEmptyOrNil		ifFalse: [s cr;				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Version Comment:'].			s cr.			s				withAttribute: (TextIndent tabs: 1)				do: [s nextPutAll: self versionComment].			s cr; cr].	url isEmptyOrNil		ifFalse: [s				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Homepage:'];				 tab;								withAttribute: (TextURL new url: url)				do: [s nextPutAll: url];				 cr].	description isEmptyOrNil		ifFalse: [s cr.			s				withAttribute: TextEmphasis bold				do: [s nextPutAll: 'Description:'].			s cr.			s				withAttribute: (TextIndent tabs: 1)				do: [s nextPutAll: description].			s cr; cr].	^ s contents.! !!SMPackageRelease methodsFor: 'printing' stamp: 'yo 7/26/2004 22:06'!fullDescription	"Return a full textual description of the package release."	| s |	s := TextStream on: (Text new: 400).	self describe: self package name withBoldLabel: 'Package name: ' on: s.	self 		describe: self version		withBoldLabel: 'version: '		on: s.	categories isEmptyOrNil 		ifFalse: 			[s				cr;				withAttribute: TextEmphasis bold do: [s nextPutAll: 'Categories: '];				cr.			self categoriesDo: 					[:c | 					s						tab;						withAttribute: TextEmphasis italic							do: 								[c parentsDo: 										[:p | 										s											nextPutAll: p name;											nextPutAll: '/'].								s nextPutAll: c name];						nextPutAll: ' - ' , c summary;						cr].			s cr].	self note isEmptyOrNil 		ifFalse: 			[s				cr;				withAttribute: TextEmphasis bold do: [s nextPutAll: 'Version Comment:'].			s cr.			s withAttribute: (TextIndent tabs: 1) do: [s nextPutAll: self note].			s				cr;				cr].	url isEmptyOrNil 		ifFalse: 			[s				withAttribute: TextEmphasis bold do: [s nextPutAll: 'Homepage:'];				tab;				withAttribute: (TextURL new url: url) do: [s nextPutAll: url];				cr].	self downloadUrl isEmptyOrNil 		ifFalse: 			[s				withAttribute: TextEmphasis bold do: [s nextPutAll: 'Download:'];				tab;				withAttribute: (TextURL new url: self downloadUrl)					do: [s nextPutAll: self downloadUrl];				cr].	^s contents.! !!SMUtilities class methodsFor: 'as yet unclassified' stamp: 'yo 7/26/2004 22:48'!mail: anAccount subject: sub message: msg	"Send a mail to the holder of <anAccount>."	SMTPClient		deliverMailFrom: 'squeakmap@squeakfoundation.org'		to: {anAccount email}		text:('From: SqueakMap <squeakmap@squeakfoundation.org>To: ', anAccount email, 'Subject: ', sub,'', msg, (self randomPhrase), ', SqueakMap') usingServer: MailServer! !