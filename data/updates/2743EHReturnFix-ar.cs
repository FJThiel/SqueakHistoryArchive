'From Squeak2.9alpha of 13 June 2000 [latest update: #2767] on 28 September 2000 at 8:33:20 pm'!"Change Set:		EHReturnFix-arDate:			28 September 2000Author:			Andreas RaabFixes a problem with return values from default actions."!!Exception methodsFor: 'signaledException' stamp: 'ar 9/28/2000 20:29'!outer	"Evaluate the enclosing exception action for the receiver and return."	^self isResumable		ifTrue:			[self setHandlerFrom: handlerContext sender.			handlerContext == nil				ifTrue: [self defaultAction]				ifFalse: [self handlerAction]]		ifFalse: [self pass]! !!Exception methodsFor: 'signaledException' stamp: 'ar 9/28/2000 20:28'!pass	"Yield control to the enclosing exception action for the receiver."	| result |	self setHandlerFrom: handlerContext sender.	handlerContext == nil		ifTrue:			[result _ self defaultAction.			self isResumable				ifTrue: [self resume: result]				ifFalse: [IllegalResumeAttempt signal]]		ifFalse: [self handlerAction]! !