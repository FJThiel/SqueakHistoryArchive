'From Squeak2.9alpha of 5 August 2000 [latest update: #3013] on 16 November 2000 at 2:45:19 pm'!"Change Set:		genericRefine-swDate:			16 November 2000Author:			Scott WallaceElevates the refineUndo... and refineRedo.. methods to class Object, so that arbitrary objects can implement undo."!!Object methodsFor: 'undo' stamp: 'sw 11/16/2000 14:42'!refineRedoTarget: target selector: aSymbol arguments: arguments in: refineBlock 	"Any object can override this method to refine its redo specification"	^ refineBlock		value: target		value: aSymbol		value: arguments! !!Object methodsFor: 'undo' stamp: 'sw 11/16/2000 14:42'!refineUndoTarget: target selector: aSymbol arguments: arguments in: refineBlock 	"Any object can override this method to refine its undo specification"	^ refineBlock		value: target		value: aSymbol		value: arguments! !Morph removeSelector: #refineRedoTarget:selector:arguments:in:!Morph removeSelector: #refineUndoTarget:selector:arguments:in:!FillStyle removeSelector: #refineRedoTarget:selector:arguments:in:!FillStyle removeSelector: #refineUndoTarget:selector:arguments:in:!