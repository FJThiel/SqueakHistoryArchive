'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5548] on 16 November 2003 at 3:48:21 pm'!"Change Set:		CCP001-SyntaxFix2-mdDate:			16 November 2003Author:			Marcus Denker			I just recompiled the latest 3.7a with the ClosureCompiler:only one method didn't compile. This changeset fixes theproblem"!!MagnifierMorph methodsFor: 'menu' stamp: 'md 11/16/2003 15:14'!chooseMagnification	| result |	result _ (SelectionMenu selections: #(1.5 2 4 8))		startUpWithCaption: ('Choose magnification(currently {1})' translated format:{magnification}).	(result isNil or: [result = magnification]) ifTrue: [^ self].	magnification _ result.	self extent: self extent. "round to new magnification"	self changed. "redraw even if extent wasn't changed"! !