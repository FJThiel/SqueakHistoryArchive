'From Squeak2.8 of 13 June 2000 [latest update: #2359] on 27 October 2000 at 4:29:55 pm'!!MethodContext methodsFor: 'initialize-release' stamp: 'ikp 12/23/1999 15:56'!restartWith: aCompiledMethod 	"Reinitialize the receiver as though it had been for a different method. 	Used by a Debugger when one of the methods to which it refers is 	recompiled."	method _ aCompiledMethod.	receiverMap _ nil.	^self restart! !!MethodContext methodsFor: 'private' stamp: 'ikp 12/23/1999 15:56'!setSender: s receiver: r method: m arguments: args 	"Create the receiver's initial state."	sender _ s.	receiver _ r.	method _ m.	receiverMap _ nil.	pc _ method initialPC.	self stackp: method numTemps.	1 to: args size do: [:i | self at: i put: (args at: i)]! !