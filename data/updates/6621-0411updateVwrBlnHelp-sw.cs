'From Squeakland 3.8.5976 of 25 August 2004 [latest update: #410] on 24 March 2005 at 2:00:33 am'!"Change Set:		updateVwrBlnHelp-swDate:			24 March 2005Author:			Scott WallaceFix for Mantis Squeakland Bug #991 -- when you edit the balloon help for a script, any existing viewer panes looking at the object's scripts will now feel the change."!!ScriptEditorMorph methodsFor: 'buttons' stamp: 'sw 3/24/2005 01:55'!editMethodDescription	"Edit the balloon help associated with the script"	self userScriptObject editDescription.	playerScripted updateAllViewers! !