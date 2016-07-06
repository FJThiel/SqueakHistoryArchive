'From Squeak3.9alpha of 4 July 2005 [latest update: #6702] on 5 November 2005 at 6:53:26 pm'!"Change Set:		fixesDate:			5 November 2005Author:			Stephane DucasseSome fixes to get back some cleans of marcus on the compiler and also the possibilities to load MC code.The definitions of the two interactive methods is clearly a temporary solution: since they make compiler depends on tools via the faked exception SyntaxError (which is a subclass of StringHolder)"!!Compiler methodsFor: 'private' stamp: 'stephaneducassse 11/5/2005 16:39'!interactive	"this version of the method is necessary to load code from MC else the interactive mode is one. 	This method is really bad since it links the compiler package with the Tools	one. The solution would be to have a real SyntaxError exception belonging to the 	compiler package and not a subclass of StringHolder - sd Nov 2005"	"the code submitted by PlusTools is ideally the one that should be used	interactive	      ^requestor ~~ nil "		^ (requestor == nil or: [requestor isKindOf: SyntaxError]) not! !!Parser methodsFor: 'public access' stamp: 'stephaneducassse 11/5/2005 16:35'!parse: sourceStream class: class category: aCategory noPattern: noPattern context: ctxt notifying: req ifFail: aBlock         "Answer a MethodNode for the argument, sourceStream, that is the root of         a parse tree. Parsing is done with respect to the argument, class, to find         instance, class, and pool variables; and with respect to the argument,         ctxt, to find temporary variables. Errors in parsing are reported to the         argument, req, if not nil; otherwise aBlock is evaluated. The argument         noPattern is a Boolean that is true if the the sourceStream does not         contain a method header (i.e., for DoIts)."		category := aCategory.        	^ self parse: sourceStream class: class  noPattern: noPattern context: ctxt notifying: req ifFail: aBlock ! !!Parser methodsFor: 'public access' stamp: 'stephaneducassse 11/5/2005 16:26'!parse: sourceStream class: class noPattern: noPattern context: ctxt notifying: req ifFail: aBlock         "Answer a MethodNode for the argument, sourceStream, that is the root of         a parse tree. Parsing is done with respect to the argument, class, to find         instance, class, and pool variables; and with respect to the argument,         ctxt, to find temporary variables. Errors in parsing are reported to the         argument, req, if not nil; otherwise aBlock is evaluated. The argument         noPattern is a Boolean that is true if the the sourceStream does not         contain a method header (i.e., for DoIts)."         | methNode repeatNeeded myStream s p |         myStream _ sourceStream.        [repeatNeeded _ false.	   p _ myStream position.	   s _ myStream upToEnd.	   myStream position: p.        self init: myStream notifying: req failBlock: [^ aBlock value].        doitFlag _ noPattern.        failBlock_ aBlock.        [methNode _ self method: noPattern context: ctxt                                encoder: (Encoder new init: class context: ctxt notifying: self)]                 on: ParserRemovedUnusedTemps                 do:                         [ :ex | repeatNeeded _ (requestor isKindOf: TextMorphEditor) not.                        myStream _ ReadStream on: requestor text string.                        ex resume].        repeatNeeded] whileTrue.        encoder _ failBlock _ requestor _ parseNode _ nil. "break cycles & mitigate refct overflow"	   methNode sourceText: s.        ^ methNode! !!Parser methodsFor: 'error handling' stamp: 'stephaneducassse 11/5/2005 16:39'!interactive	"this version of the method is necessary to load code from MC else the interactive mode is one. 	This method is really bad since it links the compiler package with the Tools	one. The solution would be to have a real SyntaxError exception belonging to the 	compiler package and not a subclass of StringHolder - sd Nov 2005"	"the code submitted by PlusTools is ideally the one that should be used	interactive	      ^requestor ~~ nil "		^ (requestor == nil or: [requestor isKindOf: SyntaxError]) not! !