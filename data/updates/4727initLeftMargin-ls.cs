'From Squeak3.2gamma of 14 January 2002 [latest update: #4653] on 14 January 2002 at 9:31:40 pm'!"Change Set:		initLeftMargin-lsDate:			14 January 2002Author:			Lex SpoonMake sure leftMargin gets initialized in CharacterScanner; this bug was brought out recently when Scamper is showing a page with indentation, and the mouse is waved over an indented line."!!CharacterScanner methodsFor: 'initialize' stamp: 'ls 1/14/2002 21:26'!initialize	destX _ destY _ leftMargin _ 0.! !