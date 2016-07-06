'From Squeak3.6beta of ''4 July 2003'' [latest update: #5395] on 22 August 2003 at 11:59:33 am'!"Change Set:		RevertExceptionDescription2-abDate:			22 August 2003Author:			Avi Bryantv.2Now uses Class>>selector as the message text for MessageNotUnderstood, after discussion with Doug and Julian.v.1One thing that (perhaps irrationally) drives me nuts about the ContextEnh changes is that, for no apparent reason, MessageNotUnderstood exceptions now yield 'SmallInteger quux?' as their description text rather than 'MessageNotUnderstood: quux'.  This changeset simply reverts two methods (Exception>>description and MessageNotUnderstood>>messageText) back to their original versions."!!Exception methodsFor: 'printing' stamp: 'pnm 8/16/2000 14:53'!description	"Return a textual description of the exception."	| desc mt |	desc := self class name asString.	^(mt := self messageText) == nil		ifTrue: [desc]		ifFalse: [desc, ': ', mt]! !!MessageNotUnderstood methodsFor: 'exceptionBuilder' stamp: 'ab 8/22/2003 11:56'!messageText	"Return an exception's message text."	^messageText == nil		ifTrue:			[message == nil				ifTrue: [super messageText]				ifFalse: [message lookupClass printString, '>>', message selector asString]]		ifFalse: [messageText]! !