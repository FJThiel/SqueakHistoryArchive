'From Squeak 2.5 of August 6, 1999 on 26 September 1999 at 7:48:51 pm'!"Change Set:		ifErrorFixDate:			26 September 1999Author:			Paul McDonoughA simple compatibility fix so that ifError: works mostly as before, after inclusion of the new exceptions architecture."!!BlockContext methodsFor: 'evaluating' stamp: 'pnm 9/23/1999 11:34'!ifError: errorHandlerBlock	"Evaluate the block represented by the receiver. If an error occurs the given is evaluated with the error message and the receiver as parameters. The error handler block may return a value to be used if the receiver block gets an error. The receiver should not contain an explicit return statement as this would leave an obsolete error handler hanging around."	"Examples:		[1 whatsUpDoc] ifError: [:err :rcvr | ^ 'huh?'].		[1 / 0] ifError: [:err :rcvr |			'division by 0' = err				ifTrue: [^ Float infinity]				ifFalse: [self error: err]]"	| lastHandler val activeProcess |	activeProcess _ Processor activeProcess.	lastHandler _ activeProcess errorHandler.	activeProcess errorHandler: [:aString :aReceiver |		activeProcess errorHandler: lastHandler.		^ errorHandlerBlock value: aString value: aReceiver].	[val _ self value.	activeProcess errorHandler: lastHandler.	^ val]		on: Error		do:			[activeProcess errorHandler: [:aString :aReceiver |				activeProcess errorHandler: lastHandler.				^ errorHandlerBlock value: aString value: aReceiver]]! !