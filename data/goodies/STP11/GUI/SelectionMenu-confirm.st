"SelectionMenu-confirm.st
This simply puts spaces around the yes/no prompt in the simply confirmer..

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 25, 1996"!


!SelectionMenu class methodsFor: 'instance creation'!
confirm: queryString 
	"Put up a yes/no menu with caption queryString. Answer true if the response is yes, false if no. This is a modal question--the user must respond yes or no."
	"SelectionMenu confirm: 'Are you hungry?'"

	| menu choice |
	menu _ self selections: #(' yes ' ' no ').
	[true] whileTrue: [
		choice _ menu startUpWithCaption: queryString.
		choice = ' yes ' ifTrue: [^ true].
		choice = ' no ' ifTrue: [^ false]]! !