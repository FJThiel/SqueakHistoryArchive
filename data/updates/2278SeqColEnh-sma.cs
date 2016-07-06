'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 1 June 2000 at 4:21:35 pm'!"Change Set:		108SeqColEnh-smaDate:			1 June 2000Author:			Stefan Matthias AustDavid N Smith suggested a few useful methods which I implemented - following Dan's suggestion to throw errors if not enough elements are available.I also deprecated the following, already or at least now superfluous methods:	copyWithoutFirst (no senders)	copyLast: (2 senders)	upTo: (a lot of senders)Finally, I added lastIndexOf: and lastIndexOf:ifAbsent:."!!BookMorph methodsFor: 'menu' stamp: 'sma 6/1/2000 16:08'!menuPageSoundFor: target event: evt	| tSpec menu |	tSpec _ self transitionSpecFor: target.	menu _ (MenuMorph entitled: 'Choose a sound(it is now ' , tSpec first , ')') defaultTarget: target.	SampledSound soundNames do:		[:soundName |		menu add: soundName target: target			selector: #setProperty:toValue:			argumentList: (Array with: #transitionSpec								with: (tSpec copy at: 1 put: soundName; yourself))].	menu popUpAt: evt hand position forHand: evt hand! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:34'!allButFirst	"Answer a copy of the receiver containing all but the first	element. Raise an error if there are not enough elements."	^ self allButFirst: 1! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!allButFirst: n	"Answer a copy of the receiver containing all but the first n	elements. Raise an error if there are not enough elements."	^ self copyFrom: n + 1 to: self size! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!allButLast	"Answer a copy of the receiver containing all but the last	element. Raise an error if there are not enough elements."	^ self allButLast: 1! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!allButLast: n	"Answer a copy of the receiver containing all but the last n	elements. Raise an error if there are not enough elements."	^ self copyFrom: 1 to: self size - n! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:31'!first: n	"Answer the first n elements of the receiver.	Raise an error if there are not enough elements."	^ self copyFrom: 1 to: n! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:46'!indexOf: anElement	"Answer the index of the first occurence of anElement within the  	receiver. If the receiver does not contain anElement, answer 0."	^ self indexOf: anElement ifAbsent: [0]! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:47'!indexOf: anElement ifAbsent: exceptionBlock	"Answer the index of the first occurence of anElement within the  	receiver. If the receiver does not contain anElement, answer the 	result of evaluating the argument, exceptionBlock."	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:47'!indexOf: anElement startingAt: start ifAbsent: exceptionBlock	"Answer the index of the first occurence of anElement after start	within the receiver. If the receiver does not contain anElement, 	answer the 	result of evaluating the argument, exceptionBlock."	start to: self size do:		[:index |		(self at: index) = anElement ifTrue: [^ index]].	^ exceptionBlock value! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:30'!last: n	"Answer the last n elements of the receiver.  	Raise an error if there are not enough elements."	| size |	size _ self size.	^ self copyFrom: size - n + 1 to: size! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:43'!lastIndexOf: anElement	"Answer the index of the last occurence of anElement within the 	receiver. If the receiver does not contain anElement, answer 0."	^ self lastIndexOf: anElement ifAbsent: [0]! !!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:45'!lastIndexOf: anElement ifAbsent: exceptionBlock	"Answer the index of the last occurence of anElement within the  	receiver. If the receiver does not contain anElement, answer the	result of evaluating the argument, exceptionBlock."	self size to: 1 by: -1 do:		[:index |		(self at: index) = anElement ifTrue: [^ index]].	^ exceptionBlock value! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:05'!copyAfter: anElement	"Answer a copy of the receiver from after the first occurence	of anElement up to the end. If no such element exists, answer 	an empty copy."	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:05'!copyAfterLast: anElement	"Answer a copy of the receiver from after the last occurence	of anElement up to the end. If no such element exists, answer 	an empty copy."	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:07'!copyEmpty	^ self species new: 0! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:06'!copyLast: num	"Deprecated. Use #last:"	^ self last: num! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:00'!copyUpTo: anElement 	"Answer all elements up to but not including anObject. If there	is no such object, answer a copy of the receiver."	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:02'!copyUpToLast: anElement	"Answer a copy of the receiver from index 1 to the last occurrence of 	anElement, not including anElement."	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1! !!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 15:38'!copyWithoutFirst	"Deprecatd. Return a copy of the receiver which doesn't include	the first element."	^ self allButFirst! !!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 6/1/2000 16:00'!upTo: anObject	"Deprecated. Use copyUpTo:"	^ self copyUpTo: anObject! !SequenceableCollection removeSelector: #copyAt:put:!