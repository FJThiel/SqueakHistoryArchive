"ExceptionSupport.st
Support for exception handling: BlockContext forking.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    06/30/97"!

!BlockContext methodsFor: 'private - for ExceptionEvent only'!
newExceptionProcess: aHandlerDictionary
	"Create an ExceptionProcess from a block"

	| block |
	block := [Processor activeProcess beginExecution. 
			self value.
			Processor activeProcess toggleAndDie].
"	block initBlock."
	^ExceptionProcess do: block handlerDictionary: aHandlerDictionary!
newExceptionProcess: aHandlerDictionary anyOther: aHandlerBlock

	| block |
	block := [Processor activeProcess beginExecution. 
			self value.
			Processor activeProcess toggleAndDie].
"	block initBlock."
	^ExceptionProcess do: block handlerDictionary: aHandlerDictionary 
			anyOther: aHandlerBlock! !