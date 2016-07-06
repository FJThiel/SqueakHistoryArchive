'From Squeak 2.3 of January 14, 1999 on 2 February 1999 at 9:48:31 pm'!"Change Set:		FastSMTP-mdrDate:			2 February 1999Author:			Michael Rutenberg (mdr@scn.org)Changes to SMTPSocket class to allow multiple mail messages to be sent over a single socket.  This can be significantly faster, especially when connecting through a firewall or over a slow connection.Also:adds a class comment to SMTPSocket.changes Celeste sendQueuedMail to use the new streamlined sockets."!!Celeste methodsFor: 'sending mail' stamp: 'mdr 1/28/1999 16:15'!sendQueuedMail	"Post queued messages to the SMTP server."	| outgoing sender n message recipients socket |	outgoing _ mailDB messagesIn: '.tosend.'.	outgoing isEmpty ifTrue: [^ self inform: 'no mail to be sent'].	sender _ (MailAddressParser addressesIn: self class userName) first.	[socket _ SMTPSocket usingServer: Celeste smtpServer]		ifError: [ :a :b | self error: 'error opening connection to mail server'].	('sending ', outgoing size printString, ' messages...')		displayProgressAt: Sensor mousePoint		from: 1		to: outgoing size		during: [:progressBar |			n _ 0.			outgoing do: [:id |				progressBar value: (n _ n + 1).				message _ mailDB getMessage: id.				recipients _ Set new.				recipients addAll: (MailAddressParser addressesIn: message to).				recipients addAll: (MailAddressParser addressesIn: message cc).				[socket 					mailFrom: sender					to: recipients 					text: message text.	"send this one message on the stream"				mailDB remove: id fromCategory: '.tosend.'.				mailDB file: id inCategory: '.sent.'				] ifError: [ :a :b | self error: 'error posting mail']		]].	socket quit; close.	mailDB saveDB.	self changed: #categoryList.	self updateTOC.! !!SMTPSocket commentStamp: 'mdr 1/29/1999 19:01' prior: 0!This class implements the SMTP (mail sending) protocol specified in RFC 821.Note that it does not look up DNS MX records when sending to the specified host, so it will not work correctly to send directly to large structured domain names like "intel.com" or "microsoft.com", but it will work fine for talking with your local mail server which will then pass along the mail to the real destination.Note also that the error reporting is done with error: and so an exception handler must be wrapped around the sending call in case it fails.!!SMTPSocket methodsFor: 'public protocol' stamp: 'mdr 10/12/1998 14:41'!mailFrom: sender to: recipientList text: messageText	"deliver this mail to a list of users.  NOTE: the recipient list should be a collection of simple internet style addresses -- no '<>' or '()' stuff"	self mailFrom: sender.	recipientList do: [ :recipient |		self recipient: recipient ].	self data: messageText.! !!SMTPSocket class methodsFor: 'sending mail' stamp: 'mdr 1/28/1999 16:42'!deliverMailFrom: fromAddress to: recipientList text: messageText usingServer: serverName	"Deliver a single email to a list of users and then close the connection.  For delivering multiple messages, it is best to create a single connection and send all mail over it.  NOTE: the recipient list should be a collection of simple internet style addresses -- no '<>' or '()' stuff"	| sock |	sock _ self usingServer: serverName.	sock mailFrom: fromAddress to: recipientList text: messageText.	sock quit.	sock close.	^true! !!SMTPSocket class methodsFor: 'instance creation' stamp: 'mdr 1/29/1999 18:59'!usingServer: serverName	"Create a SMTP socket to the specified server for sending one or more mail messages"	| sock |	Socket initializeNetwork.	sock _ self new.	sock connectToSMTPServer: serverName.	^sock! !"Postscript:"!