'From Squeak3.1alpha [latest update: #''Squeak3.1alpha'' of 28 February 2001 update 3955] on 27 April 2001 at 11:45:10 am'!"Change Set:		TickDelayDate:			27 April 2001Author:			Andreas RaabMake the clock menu popup later to not confuse people."!!ScriptStatusControl methodsFor: 'mouse gestures' stamp: 'ar 4/27/2001 11:33'!mouseDownTick: evt onItem: aMorph	aMorph color: Color veryLightGray.	self addAlarm: #offerTickingMenu: with: aMorph after: 1000.! !