'From Squeak2.8alpha of 4 February 2000 [latest update: #2005] on 22 April 2000 at 8:52:52 pm'!"Change Set:		1055FreeCellWorldBug-smgDate:			27 March 2000Author:			Sean McGrathOpen a morphic World, new morph, games, freecell, statistics: UndefinedObject(Object)>>doesNotUnderstand: AlignmentMorph(Morph)>>openInWindowLabeled:inWorld: AlignmentMorph>>openInWindowLabeled:inWorld: AlignmentMorph(Morph)>>openInWindowLabeled: FreeCellStatistics>>display FreeCell>>statistics [] in SimpleButtonMorph>>doButtonAction BlockContext>>ensure: CursorWithMask(Cursor)>>showWhile: SimpleButtonMorph>>doButtonActionIn Morph,  openInWindowLabeled: aString        ^self openInWindowLabeled: aString inWorld: WorldWorld in nil. Changing 'World' to 'self currentWorld' fixes the symptom.I have no idea what the problem is, since I understand very littleMorphic.This is probably a general problem.  There're are some 106 users of the global 'World' I'm too tired to check them all. Most of them should be tranlated into a test whether we're running a full screen world or an MVC world.  The others might be candidates for #currentWorld. --sma"!!Morph methodsFor: 'initialization' stamp: 'sma 4/22/2000 20:28'!openInWindowLabeled: aString	^self openInWindowLabeled: aString inWorld: self currentWorld! !