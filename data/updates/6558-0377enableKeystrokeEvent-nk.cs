'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #371] on 13 December 2004 at 6:25:29 pm'!"Change Set:		EnableKeystrokeEvent-nkDate:			13 December 2004Author:			Ned KonzCS 0359keyboardEvents-nk did not register the keystroke event trigger properly (or rather, 0367specificEvents-nk didn't change PasteUpMorph>>initialize as it should).This change set restores the 'keyStroke' event for PasteUpMorphs including the World."!!PasteUpMorph class methodsFor: 'class initialization' stamp: 'nk 12/13/2004 18:22'!initialize	"Initialize the class"	self registerInFlapsRegistry.		ScriptingSystem addCustomEventFor: self named: #keyStroke help: 'when a keystroke happens and nobody heard it' targetMorphClass: PasteUpMorph.! !PasteUpMorph initialize!"Postscript:Now make sure that PasteUpMorph is properly registered."PasteUpMorph initialize.!