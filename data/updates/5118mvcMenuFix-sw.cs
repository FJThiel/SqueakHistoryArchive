'From Squeak3.4alpha of 6 November 2002 [latest update: #5109] on 16 November 2002 at 11:47:42 pm'!"Change Set:		mvcMenuFix-swDate:			16 November 2002Author:			Scott WallaceWhen a CustomMenu uses the targets-list and arguments-list feature, use the invoked-on object as the recipient if an item beyond the section having explicit targets is chosen"!!CustomMenu methodsFor: 'invocation' stamp: 'sw 11/16/2002 23:45'!invokeOn: targetObject orSendTo: anObject	"Pop up this menu and return the result of sending to the target object the selector corresponding to the menu item selected by the user. Return  nil if no item is selected.  If the chosen selector has arguments, obtain appropriately.  If the recipient does not respond to the resulting message, send it to the alternate object provided"	| aSelector anIndex recipient |	^ (aSelector _ self startUp) ifNotNil:		[anIndex _ self selection.		recipient _ ((targets _ self targets) isEmptyOrNil or: [anIndex > targets size])			ifTrue:				[targetObject]			ifFalse:				[targets at: anIndex].		aSelector numArgs == 0			ifTrue:				[recipient perform: aSelector orSendTo: anObject]			ifFalse:				[recipient perform: aSelector withArguments: (self arguments at: anIndex)]]! !