"VitalFixFor1.17.st
Fixes an inconsistency between compiler sizing and code generation.

        AUTHOR          Dan Ingalls (DanI@WDI.Disney.com)
        VERSION          1.17
        PREREQUISITES   none
        DISTRIBUTION    world
        VERSION ID       1
        VERSION DATE    November 19, 1996"!

'From Squeak 1.13 of October 17, 1996 on 19 November 1996 at 3:37:43 pm'!

!VariableNode methodsFor: 'code generation'!
sizeForStore: encoder
	self reserve: encoder.
	code < 256 ifTrue: [^ 2].
	(code \\ 256) <= 63 ifTrue: [^ 2].
	^ 3! !

