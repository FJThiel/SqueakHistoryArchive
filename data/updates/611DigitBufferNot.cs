'From Squeak 2.3 of January 14, 1999 on 28 January 1999 at 1:12:32 pm'!"Change Set:		DigitBufferNotDate:			28 January 1999Author:			Dan IngallsRemoves the unused class variable 'DigitBuffer' from SmallInteger.Suggested by Luciano Notarfrancesco"!SmallInteger class removeSelector: #initialize!Integer subclass: #SmallInteger	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Numeric-Numbers'!