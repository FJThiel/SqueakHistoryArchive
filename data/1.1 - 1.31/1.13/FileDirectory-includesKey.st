'From Squeak 1.13 of October 17, 1996 on 6 December 1996 at 8:57:21 pm'!

!FileDirectory methodsFor: 'dictionary access'!
includesKey:aString
	"Answer whether the receiver includes an element of the given name."
	"Note: aString may designate a file local to this directory, or it may be a full path name."

	^ (StandardFileStream isAFileNamed:(self fullNameFor: aString)! !
