'From Squeak3.6beta of ''4 July 2003'' [latest update: #5373] on 23 July 2003 at 11:20:55 am'!"Change Set:		DebuggerUnwindFix-ajhDate:			23 July 2003Author:			Anthony HannanProcess>>terminate now (as originally) removes itself from it Semaphore queue before unwinding."!!Process methodsFor: 'changing process state' stamp: 'ajh 7/23/2003 11:10'!terminate 	"Stop the process that the receiver represents forever.  Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."	self isActiveProcess ifTrue: [		| ctxt unwindBlock |		[	ctxt _ thisContext findNextUnwindContextUpTo: nil.			ctxt isNil		] whileFalse: [			unwindBlock _ ctxt tempAt: 1.			thisContext terminateTo: ctxt sender.			unwindBlock value.		].		thisContext terminateTo: nil.		myList _ nil.		self primitiveSuspend.	] ifFalse: [		myList ifNotNil: [			myList remove: self ifAbsent: [].			myList _ nil].		self popTo: nil.	].! !