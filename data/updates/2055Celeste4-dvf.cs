'From Squeak2.8alpha of 4 February 2000 [latest update: #2052] on 28 April 2000 at 6:06:39 pm'!"Change Set:		004Celeste4-dvfDate:			28 April 2000Author:			Daniel VainsencherWARNING: The class MIMEHeaderValue is missing.  I just added a stub to filein the stuff --smaThis version adds a couple of features -* You can send attachments from celeste (base64 encoding only, for now).* let's you save attachments even if they're recursively embedded.Also beginning to clean some of the mess up."!Model subclass: #CelesteComposition	instanceVariableNames: 'celeste messageText textEditor morphicWindow mvcWindow attachmentSeparator '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Mail Reader'!Object subclass: #MIMEHeaderValue	instanceVariableNames: 'parameters '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Mail Reader'!Object subclass: #MIMEPart	instanceVariableNames: 'text fields content separator parts '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Mail Reader'!!CelesteComposition methodsFor: 'private' stamp: 'dvf 4/28/2000 02:19'!hasAttachments	^ attachmentSeparator notNil! !!CelesteComposition methodsFor: 'interface' stamp: 'dvf 4/28/2000 03:21'!addAttachment	| file fileResult fileName strm |	textEditor		ifNotNil: [self hasUnacceptedEdits ifTrue: [textEditor accept]].	"transform into multipart if needed"	self hasAttachments ifFalse: [self transformToMultipart].	"then simply append another attachment section"	(fileResult _ StandardFileMenu oldFile)		ifNotNil: 			[fileName _ fileResult directory fullNameFor: fileResult name.			file _ FileStream oldFileNamed: fileName.			file				ifNotNil: 					[strm _ WriteStream on: (String new: 100).					strm nextPutAll: 'Content-Type: application/octet-stream; name="' , fileResult name , '"';					 cr;					 nextPutAll: 'Content-Disposition: attachment; filename="' , fileResult name , '"';					 cr;					 nextPutAll: 'Content-Transfer-Encoding: base64';					 cr;					 cr.					Base64MimeConverter new dataStream: file;					 mimeStream: strm;					 mimeEncode.					strm cr; nextPutAll: '--' , attachmentSeparator; cr. self messageText: (self messageText, strm contents)]]! !!CelesteComposition methodsFor: 'interface' stamp: 'dvf 4/28/2000 02:06'!openInMorphic	"open an interface for sending a mail message with the given initial 	text "	| textMorph buttonsList sendButton attachmentButton |	morphicWindow _ SystemWindow labelled: 'Mister Postman'.	morphicWindow model: self.	textEditor _ textMorph _ PluggableTextMorph						on: self						text: #messageText						accept: #messageText:.	morphicWindow addMorph: textMorph frame: (0 @ 0.1 corner: 1 @ 1).	buttonsList _ AlignmentMorph newRow.	sendButton _ PluggableButtonMorph				on: self				getState: nil				action: #submit.	sendButton label: 'send message'.	sendButton setBalloonText: 'add this to the queue of messages to be sent'.	sendButton onColor: Color white offColor: Color white.	buttonsList addMorphBack: sendButton.		attachmentButton _ PluggableButtonMorph				on: self				getState: nil				action: #addAttachment.	attachmentButton label: 'add attachment'.	attachmentButton setBalloonText: 'Send a file with the message'.	attachmentButton onColor: Color white offColor: Color white.	buttonsList addMorphBack: attachmentButton.		morphicWindow addMorph: buttonsList frame: (0 @ 0 extent: 1 @ 0.1).	morphicWindow openInMVC! !!CelesteComposition methodsFor: 'interface' stamp: 'dvf 4/28/2000 03:25'!transformToMultipart	| oldPart newText strm |	oldPart _ MIMEPart on: self messageText asString.	attachmentSeparator _ '==CelesteAttachment' , (10000 to: 99999) atRandom asString , '=='.	newText _ String new: 200.	strm _ WriteStream on: newText.	strm nextPutAll: 'Mime-Version: 1.0';	 cr;	 nextPutAll: 'Content-Type: multipart/mixed; boundary="';	 nextPutAll: attachmentSeparator;	 nextPut: $";	 cr;	 nextPutAll: oldPart fieldsAsMimeHeader;	 cr;cr;	 nextPutAll: '--' , attachmentSeparator;	 cr;	 nextPutAll: 'Content-Type: text/plain; charset="us-ascii"';	 cr;	 cr;	 nextPutAll: oldPart content;	 cr;	 nextPutAll: '--' , attachmentSeparator;	 cr.	self messageText: strm contents! !!MIMEHeaderValue methodsFor: 'printing' stamp: 'dvf 4/28/2000 02:48'!asHeaderValue	| strm |	strm _ WriteStream on: (String new: 20).	strm nextPutAll: mainValue.	parameters associationsDo: [:e | strm nextPut: $; ; nextPutAll: e key;		 nextPutAll: '="';		 nextPutAll: e value , '"'].	^ strm contents! !!MIMEPart methodsFor: 'as yet unclassified' stamp: 'dvf 4/28/2000 02:50'!fieldsAsMimeHeader	| strm |	strm _ WriteStream on: (String new: 100).	self fields associationsDo: [:e | strm nextPutAll: e key;		 nextPutAll: ': ';		 nextPutAll: e value asHeaderValue;		 cr]. 	^ strm contents! !!MIMEPart methodsFor: 'as yet unclassified' stamp: 'dvf 4/28/2000 01:20'!parseParts	| parseStream currLine msgStream messages |	self isMultipart ifFalse: [^ parts _ #()].	parseStream _ ReadStream on: self content.	currLine _ ''.	['--*' match: currLine]		whileFalse: [currLine _ parseStream nextLine].	separator _ currLine copy.	msgStream _ LimitingLineStreamWrapper on: parseStream delimiter: separator.	messages _ OrderedCollection new.	[parseStream atEnd]		whileFalse: 			[messages add: msgStream upToEnd.			msgStream skipThisLine].	parts _ messages collect: [:e | MIMEPart on: e]! !!MIMEPart methodsFor: 'as yet unclassified' stamp: 'dvf 4/28/2000 01:24'!parts	parts ifNil: [self parseParts].	^ parts! !!MIMEPart methodsFor: 'as yet unclassified' stamp: 'dvf 4/28/2000 00:48'!separator	separator ifNil: [self parts].	^ separator! !Object subclass: #MIMEPart	instanceVariableNames: 'text fields separator parts content '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Mail Reader'!Model subclass: #CelesteComposition	instanceVariableNames: 'celeste messageText textEditor attachmentSeparator morphicWindow mvcWindow '	classVariableNames: ''	poolDictionaries: ''	category: 'Network-Mail Reader'!