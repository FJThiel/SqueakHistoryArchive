"WindowColors.st
Set your favorite colors for the standard applications here. The pre-defined colors are:
	black blue cyan darkGray gray green lightBlue lightBrown lightCyan lightGray 
	lightGreen lightMagenta lightOrange lightRed lightYellow magenta red 
	veryDarkGray veryLightGray white yellow

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 17, 1996"!

!Object methodsFor: 'user interface'!
defaultBackgroundColor
	"Answer the symbol representing the default background color to use if the receiver is used as the model for a StandardSystemView.  4/30/96 sw"

	^ #white! !

!Browser methodsFor: 'initialize-release'!
defaultBackgroundColor
	^ #yellow! !

!ChangeList methodsFor: 'viewing access'!
defaultBackgroundColor
	^ #lightBlue! !

!ChangeSorter methodsFor: 'creation'!
defaultBackgroundColor
	^ #lightBlue! !

!Debugger methodsFor: 'initialize-release'!
defaultBackgroundColor
	^ #lightRed! !

!DualChangeSorter methodsFor: 'everything'!
defaultBackgroundColor
	^ #lightBlue! !

!FileModel methodsFor: 'accessing'!
defaultBackgroundColor
	^ #lightGray! !

!FillInTheBlank methodsFor: 'initialize-release'!
defaultBackgroundColor
	^ #lightBrown! !

!MessageSet methodsFor: 'private'!
defaultBackgroundColor
	^ #lightBlue! !

!Project methodsFor: 'initialization'!
defaultBackgroundColor
	^ #lightOrange! !

!StringHolder methodsFor: 'initialize-release'!
defaultBackgroundColor
	^ #lightYellow! !

!TextCollector methodsFor: 'initialize-release'!
defaultBackgroundColor
	^ #yellow! !
