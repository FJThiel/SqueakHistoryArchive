'From Squeak 2.4b of April 23, 1999 on 28 July 1999 at 10:10:21 pm'!"Change Set:		Alice2AnimChngDate:			28 July 1999Author:			Jeff PierceTightens up the animation hierarchy for Alice v2.  Because we're splitting out composite animations into AliceScripts, we can eliminate some classes to streamline the hierarchy."!AliceAbstractAnimation subclass: #AliceAbsoluteAnimation	instanceVariableNames: 'lastStartState '	classVariableNames: ''	poolDictionaries: ''	category: 'Alice2.0-Time'!AliceAbstractAnimation subclass: #AliceRelativeAnimation	instanceVariableNames: 'getReverseStateFunction '	classVariableNames: ''	poolDictionaries: ''	category: 'Alice2.0-Time'!Smalltalk removeClassNamed: #AliceSimpleAnimation!