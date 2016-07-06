'From Squeak2.9alpha of 17 July 2000 [latest update: #3316] on 28 January 2001 at 4:29:49 pm'!"Change Set:		interCyclePauseDate:			28 January 2001Author:			Bob ArningA couple of tweaks to the inter-cycle pause:- There was a test for the wait being less than the target. If the cpu was fast, the wait could be equal and would thus be skipped. Changed to less or equal.- The lastCycleTime was recorded as the time before the wait which meant that on the next cycle it might appear that no wait was necessary. Changed to record the time after the wait.The effect of these changes is to reduce the number of morphic cycles run per second to something closer to the number expected for the value of MinCycleLapse. This means lower cpu utilization by Squeak when nothing is going on (which is good). It also means a possibly lower frame rate (which may not be so good). If the latter is the case, then we may need to think about giving the user some control over the cycle rate."!!WorldState methodsFor: 'update cycle' stamp: 'RAA 1/28/2001 16:22'!interCyclePause: milliSecs	"delay enough that the next interaction cycle won't happen too soon after the original; thus, if all the system is doing is polling for interaction, the overall CPU usage of Squeak will be low"	| currentTime wait |	currentTime _ Time millisecondClockValue.	(lastCycleTime notNil and: [CanSurrenderToOS ~~ false]) ifTrue: [ 		wait _ lastCycleTime + milliSecs - currentTime.		wait > 0 ifTrue: [ 			wait <= milliSecs  "big waits happen after a snapshot"				ifTrue: [DisplayScreen checkForNewScreenSize.						(Delay forMilliseconds: wait) wait ]. 		].	].	"record the time AFTER the wait"	lastCycleTime _  Time millisecondClockValue.	CanSurrenderToOS _ true.! !