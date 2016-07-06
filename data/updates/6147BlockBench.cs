'From Squeak3.4gamma of ''7 January 2003'' [latest update: #5169] on 23 February 2003 at 10:54:10 pm'!"Change Set:		[ENH] Block>>benchDate:			23 February 2003Author:			Chris MullerA benchmark that repeats up to five seconds or one run, whichever is longer, to provide a meaningful String describing the rate of execution.example:[ 100 factorial ] bench"!!BlockContext methodsFor: 'evaluating' stamp: 'cmm 2/16/2003 16:08'!bench	"See how many times I can value in 5 seconds.  I'll answer a meaningful description."	| startTime endTime count |	count _ 0.	endTime _ Time millisecondClockValue + 5000.	startTime _ Time millisecondClockValue.	[ Time millisecondClockValue > endTime ] whileFalse: [ self value.  count _ count + 1 ].	endTime _ Time millisecondClockValue.	^count = 1		ifTrue: [ ((endTime - startTime) // 1000) printString, ' seconds.' ]		ifFalse:			[ ((count * 1000) / (endTime - startTime)) asFloat printString, ' per second.' ]! !"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."!