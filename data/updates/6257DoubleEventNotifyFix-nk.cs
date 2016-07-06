'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #308] on 6 September 2004 at 8:25:29 am'!"Change Set:		DoubleEventNotifyFix-nkDate:			6 September 2004Author:			Ned KonzSince Utilities>>event: was signaling an InMidstOfFileinNotification, the WeakActionSequenceTrappingErrors was re-sending the notifications.Instead of trapping any Exception, the WeakActionSequenceTrappingErrors should have been looking for Halt and Error.This change set modifies the WeakActionSequenceTrappingErrors to ignore Notifications."!!WeakActionSequenceTrappingErrors methodsFor: 'evaluating' stamp: 'nk 9/6/2004 08:22'!valueStartingFrom: startIndex	"Do the same as my parent, but make sure that all actions that do not 	give errors are evaluated before resignaling the ones that gave errors 	(giving the chance to clients to handle them)."	"Note: I currently trap Halt,Error so that I am sure to get a Halt event in case of a Halt. This is being fixed in the exception system - when the fix is done it will be enough to capture only Error."	| each answer |	startIndex to: self size do: [:index |		each := self at: index.		each isReceiverOrAnyArgumentGarbage ifFalse: [			[answer _ each value]				on: Halt, Error				do: [:exc | 						self valueStartingFrom: index + 1.						exc pass]]].	^ answer! !!WeakActionSequenceTrappingErrors methodsFor: 'evaluating' stamp: 'nk 9/6/2004 08:22'!valueWithArguments: anArray startingFrom: startIndex	"Do the same as my parent, but make sure that all actions that do not 	give errors are evaluated before resignaling the ones that gave errors 	(giving the chance to clients to handle them)."	"Note: I currently trap Halt,Error so that I am sure to get a Halt event in case of a Halt. This is being fixed in the exception system - when the fix is done it will be enough to capture only Error."	| each answer |	startIndex to: self size do: [:index |		each := self at: index.		each isReceiverOrAnyArgumentGarbage ifFalse: [			[answer _ each valueWithArguments: anArray]				on: Halt, Error				do: [:exc | 						self valueWithArguments: anArray startingFrom: index + 1.						exc pass]]].	^ answer! !