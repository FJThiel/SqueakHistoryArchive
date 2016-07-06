'From Squeak3.4alpha of 6 November 2002 [latest update: #5109] on 18 November 2002 at 4:29 pm'!"Change Set:		yellowButFix-swDate:			18 November 2002Author:			Scott WallaceFixes the bug that dropped one into a debugger when one tried (in an mvc project) to invoke the yellow-button menu of, for example, the BitEdtor"!!SelectionMenu methodsFor: 'invocation' stamp: 'sw 11/18/2002 16:24'!invokeOn: targetObject orSendTo: anObject	"Pop up the receiver, obtaining a selector; return the result of having the target object perform the selector.  If it dos not understand the selector, give the alternate object a chance"	| aSelector |	^ (aSelector _ self startUp) ifNotNil:		[(targetObject respondsTo: aSelector)			ifTrue:				[targetObject perform: aSelector]			ifFalse:				[anObject perform: aSelector]]! !