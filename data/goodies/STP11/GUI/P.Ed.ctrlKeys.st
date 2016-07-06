"P.Ed.ctrlKeys.st
For UNIX keyboards--treat <CTRL> like <COMMAND> for accel keys.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    VERSION ID      1
    VERSION DATE    November 18, 1996"!


!ParagraphEditor methodsFor: 'typing support'!
dispatchOnCharacter: char with: typeAheadStream
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."
	"Changed for UNIX keyboards to use CONTROL keys as 'command keys' and SHIFT-CONTROL as 'shift-command'."

	"enter, backspace, and escape keys (ascii 3, 8, and 27) are command keys"
	(sensor commandKeyPressed or: [#(3 8 27) includes: char asciiValue])
		ifTrue: [sensor leftShiftDown
			ifTrue: [^ self perform: (ShiftCmdActions at: char asciiValue + 1) with: typeAheadStream]
			ifFalse: [^ self perform: (CmdActions at: char asciiValue + 1) with: typeAheadStream]].

	"the control key can be used to invoke cmd shortcuts"
	sensor controlKeyPressed
		ifTrue: [sensor leftShiftDown
			ifTrue: [^ self perform: (ShiftCmdActions at: char asciiValue + 1) with: typeAheadStream]
			ifFalse: [^ self perform: (CmdActions at: char asciiValue + 97) with: typeAheadStream]].
	^ self perform: #normalCharacter: with: typeAheadStream! !