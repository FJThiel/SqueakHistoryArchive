"TextEmphasis-storeOn.st
This code makes storing texts a bit more compact by storing the emphasis and font changes in a 'native' form.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    December 5, 1996"!


!TextEmphasis methodsFor: 'printing'!
storeOn: aStream
	aStream nextPutAll: '(TextEmphasis code: ', self emphasisCode printString, ')'.! !

!TextEmphasis class methodsFor: 'as yet unclassified'!
code: anInteger
	^ self new emphasisCode: anInteger! !

!TextFontChange methodsFor: 'as yet unclassified'!
storeOn: aStream
	aStream nextPutAll: '(TextFontChange fontNumber: ', self fontNumber printString, ')'.! !

