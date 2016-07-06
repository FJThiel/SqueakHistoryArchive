'From Squeak2.7 of 5 January 2000 [latest update: #1780] on 19 January 2000 at 8:49:52 pm'!"Change Set:		PluralStrings-ccnDate:			19 January 2000Author:			Chris NortonOK.  You could say that this is a silly crusade, but I like my Squeak messages to be in proper English (at least when the language you're using is English).  I was messing around with the HangMan file-in and I got a walkback that had the following title:  'Error: HangManPhrases classs are not indexable'.  I just couldn't stand the extra 's' on 'class', so I created a nifty new String method called asPlural and I modified the sender of the odd string to use the new String modifier.  Take it or leave it, but I think this makes auto-generated text just a little easier to read."!!Object methodsFor: 'private' stamp: 'ccn 1/19/2000 20:37'!errorNotIndexable	"Create an error notification that the receiver is not indexable."	self error: self class name asPlural , ' are not indexable'! !!String methodsFor: 'converting' stamp: 'ccn 1/19/2000 20:33'!asPlural	"Answer a String made up from the receiver that is plural."	| newString upcaseString endString |	(self isEmpty or: [self withBlanksTrimmed isEmpty])		ifTrue: [^self copy].	newString _ self withBlanksTrimmed copy.	upcaseString _ newString asUppercase.	endString _ ''.	(upcaseString last = $S)		ifTrue:			[(newString size = 1)				ifTrue:					[endString _ 'es']				ifFalse:					[((upcaseString at: (upcaseString size - 1)) = $E)						ifFalse: [endString _ 'es']]]		ifFalse:			[endString _ 's'].	^newString , endString	"Tests:		'' asPlural	' ' asPlural	' high ' asPlural	'low' asPlural	'horse' asPlural	'kites' asPlural	'boss' asPlural	's' asPlural	'a' asPlural	"! !"Postscript:As always, send your questions or comments to:  chrisn@kronos.com"!