'From Squeak3.1alpha of 28 February 2001 [latest update: #3840] on 16 March 2001 at 5:23:45 pm'!"Change Set:		debuggerShiftMenu-swDate:			16 March 2001Author:			Scott WallaceFixes bug pointed out to Mike Rutenberg whereby the more... item in the context-stack menu of the debugger did not do the right thing."!!Debugger methodsFor: 'context stack menu' stamp: 'sw 3/16/2001 17:20'!messageListMenu: aMenu shifted: shifted	"The context-stack menu takes the place of the message-list menu in the debugger, so pass it on"	^ self contextStackMenu: aMenu shifted: shifted! !