'From Squeak3.6beta of ''4 July 2003'' [latest update: #5331] on 13 July 2003 at 2:28:21 pm'!"Change Set:		HandMorphCursorDate:			13 July 2003Author:			Karl RambergWhen morphs are picked up with the hand the cursor changes from a black arrow with a white outline to a black arrow. This single line fix changes that so the cursor look the same when picking up stuff."!!HandMorph class methodsFor: 'class initialization' stamp: 'kfr 7/13/2003 14:15'!initialize	"HandMorph initialize"	PasteBuffer _ nil.	DoubleClickTime _ 350.	NormalCursor _ CursorWithMask normal asCursorForm.! !HandMorph initialize!"Postscript:HandMorph initialize"!