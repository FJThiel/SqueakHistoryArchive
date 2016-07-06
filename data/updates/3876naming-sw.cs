'From Squeak3.1alpha of 28 February 2001 [latest update: #3886] on 28 March 2001 at 3:14:50 pm'!"Change Set:		naming-swDate:			28 March 2001Author:			Scott WallaceImprovements relating both to quality and speed of some aspects of external naming.  Removes some damaging naming-relating behavior from MorphicModel subclasses.  Improves wording of undo-related menu items.Also, urelated, code to bulletproof against a nasty low-space condition that we see arising from time to time when a TransformationMorph has somehow lost its transformee."!!CommandHistory methodsFor: 'menu' stamp: 'sw 3/28/2001 09:50'!redoMenuWording	"Answer the wording to be used in a menu offering the current Redo command"	| nextCommand |	((nextCommand _ self nextCommand) == nil or: [Preferences useUndo not]) ifTrue:  [^ 'can''t redo'].	^ String streamContents:		[:aStream | 			aStream nextPutAll: 'redo "'.			aStream nextPutAll: (nextCommand cmdWording truncateWithElipsisTo: 20).			aStream nextPut: $".			lastCommand phase == #done ifFalse:				[aStream nextPutAll: ' (z)']]! !!CommandHistory methodsFor: 'menu' stamp: 'sw 3/26/2001 23:13'!undoMenuWording	"Answer the wording to be used in an 'undo' menu item"	(((lastCommand == nil or: [Preferences useUndo not]) or:		[Preferences infiniteUndo not and: [lastCommand phase == #undone]])  or: 			[self nextCommandToUndo == nil]) ifTrue: [^ 'can''t undo'].	^ String streamContents:		[:aStream | 			aStream nextPutAll: 'undo "'.			aStream nextPutAll: (self nextCommandToUndo cmdWording truncateWithElipsisTo: 20).			aStream nextPut: $".			lastCommand phase == #done ifTrue:				[aStream nextPutAll: ' (z)']]! !!CommandHistory methodsFor: 'menu' stamp: 'sw 3/28/2001 09:52'!undoOrRedoMenuWording	"Answer the wording to be used in a menu item offering undo/redo (i.e., the form used when the #infiniteUndo preference is false)"	| pre |	lastCommand ifNil: [^ 'can''t undo'].	pre _ lastCommand phase == #done		ifTrue:		['undo']		ifFalse:		['redo'].	^ pre, ' "', (lastCommand cmdWording truncateWithElipsisTo: 20), '" (z)'! !!CommandHistory methodsFor: 'called from the ui' stamp: 'sw 3/28/2001 10:03'!undoTo	"Not yet functional, and not yet sent.  Allow the user to choose a point somewhere in the undo/redo tape, and undo his way to there.   Applicable only if infiniteUndo is set. "	| anIndex commandList aMenu reply |	(anIndex _ self historyIndexOfLastCommand) == 0 ifTrue: [^ self beep].	commandList _ history		copyFrom:	((anIndex - 10) max: 1)		to:			((anIndex + 10) min: history size).	aMenu _ SelectionMenu labels:  (commandList collect: [:cmd | cmd cmdWording truncateWithElipsisTo: 20]) selections: commandList.	reply _ aMenu startUpWithCaption: 'undo or redo to...'.	reply ifNotNil: [self inform: #deferred]	"ActiveWorld commandHistory undoTo"! !!Morph methodsFor: 'submorphs-accessing' stamp: 'sw 3/28/2001 15:12'!allKnownNames	"Return a list of all known names based on the scope of the receiver.  Does not include the name of the receiver itself.  Items in parts bins are excluded.  Reimplementors (q.v.) can extend the list"	^ self knownNamesOfSubmorphs! !!Morph methodsFor: 'submorphs-accessing' stamp: 'sw 3/28/2001 15:01'!knownNamesOfSubmorphs	"Return a list of all known names of submorphs and nested submorphs of the receiver, based on the scope of the receiver.  Items in parts bins are excluded"	| allNames theName |	self isPartsBin ifTrue:[^ #()]. "Don't report names from parts bins"	allNames _ WriteStream on: #().	self submorphsDo: 		[:m |			(theName _ m knownName) ifNotNil:				[allNames nextPut: theName].			allNames nextPutAll: m allKnownNames].	^ allNames contents! !!Morph methodsFor: 'dropping/grabbing' stamp: 'sw 3/27/2001 11:52'!nameForUndoWording	"Return wording appropriate to the receiver for use in an undo-related menu item (and perhaps elsewhere)"	| aName |	aName _ self knownName ifNil: [self renderedMorph class name].	^ aName truncateTo: 24! !!Morph methodsFor: 'dropping/grabbing' stamp: 'sw 3/28/2001 10:00'!undoGrabCommand	"Return an undo command for grabbing the receiver"	| cmd |	owner ifNil:		[^ nil]. "no owner - no undo"	^ (cmd _ Command new)		cmdWording: 'move ', self nameForUndoWording;		undoTarget: self		selector: #undoMove:redo:owner:bounds:predecessor:		arguments: {cmd. false. owner. self bounds. (owner morphPreceding: self)};		yourself! !!MorphicModel methodsFor: 'access' stamp: 'sw 3/28/2001 10:01'!allKnownNames	"Return a list of all known names based on the scope of the receiver.  If the receiver is a member of a uniclass, incorporate the original 1997 logic that queries the known names of the values of all the instance variables."	| superNames |	superNames _ super allKnownNames.  "gather them from submorph tree"	^ self belongsToUniClass		ifTrue:			[superNames, (self instanceVariableValues select: [:e | (e ~~ nil) and: [e knownName ~~ nil]] thenCollect: [:e | e knownName])]		ifFalse:			[superNames]! !!TransformationMorph methodsFor: 'accessing' stamp: 'sw 3/28/2001 14:24'!referencePosition	"Answer the  receiver's reference position, bullet-proofed against infinite recursion in the unlikely but occasionally-seen case that I am my own renderee"	| rendered |	^ (rendered _ self renderedMorph) == self		ifTrue:			[super referencePosition]		ifFalse:			[transform localPointToGlobal: rendered referencePosition]! !