"FileModel-put.st
This removes the prompter before overwriting a file."!

!FileModel methodsFor: 'accessing' stamp: 'stp 07/01/97 10:28'!
put: aString 
	"Write with no warning about overwrite."

	| f |
	(aString size >= 5 and:
		[#('File ' '16r0 ') includes: (aString copyFrom: 1 to: 5)])
		ifTrue: [(self confirm:
'Abbreviated and hexadecimal file views
cannot be meaningfully saved at present.
Is this REALLY what you want to do?')
				ifFalse: [^ self]].
"	f _ FileStream newFileNamed: self fullName."
	f _ FileStream fileNamed: self fullName.
	Cursor write showWhile: [f nextPutAll: aString; close]! !
