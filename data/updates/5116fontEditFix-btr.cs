'From Squeak3.2 of 11 July 2002 [latest update: #4956] on 18 November 2002 at 3:03:12 pm'!"Change Set:		fontEditFix-btrDate:			18 November 2002Author:			Brian T. RiceFixed BitEditor's initializing method setColor: to grab the right bit-depth, which prevented it from opening, and added a note to StrikeFont>>edit: that it only works in MVC."!!BitEditor methodsFor: 'menu messages' stamp: 'btr 11/18/2002 14:57'!setColor: aColor 	"Set the color that the next edited dots of the model to be the argument,  	aSymbol. aSymbol can be any color changing message understood by a  	Form, such as white or black."	color _ aColor pixelValueForDepth: Display depth.	squareForm fillColor: aColor! !!StrikeFont methodsFor: 'character shapes' stamp: 'btr 11/18/2002 15:00'!edit: character 	"Open a Bit Editor on the given character. Note that you must do an accept 	(in the option menu of the bit editor) if you want this work. 	Accepted edits will not take effect in the font until you leave or close the bit editor. 	Also note that unaccepted edits will be lost when you leave or close."	"Note that BitEditor only works in MVC currently."	"(TextStyle default fontAt: 1) edit: $_"	| charForm editRect scaleFactor bitEditor savedForm r |	charForm _ self characterFormAt: character.	editRect _ BitEditor locateMagnifiedView: charForm scale: (scaleFactor _ 8 @ 8).	bitEditor _ BitEditor				bitEdit: charForm				at: editRect topLeft				scale: scaleFactor				remoteView: nil.	savedForm _ Form fromDisplay: (r _ bitEditor displayBox							expandBy: (0 @ 23 corner: 0 @ 0)).	bitEditor controller startUp.	bitEditor release.	savedForm displayOn: Display at: r topLeft.	self characterFormAt: character put: charForm! !