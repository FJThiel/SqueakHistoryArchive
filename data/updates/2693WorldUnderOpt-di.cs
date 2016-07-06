'From Squeak2.9alpha of 12 June 2000 [latest update: #2688] on 24 September 2000 at 1:19:49 pm'!"Change Set:		WorldUnderOptDate:			24 September 2000Author:			Dan IngallsIntroduces wordlMorphsDo: so that certain structures such as PluggableLists and Viewers don't need to pass over every submorph looking for worlds under the cursor.  Saves *lots* of morphic traversal."!!DisplayScreen methodsFor: 'other' stamp: 'di 9/24/2000 12:49'!morphicWorldAt: aPoint 	| roots outer worldCount |	outer _ self getOuterMorphicWorld ifNil: [^ nil].	worldCount _ 0.	outer worldMorphsDo: [ :each | worldCount _ worldCount + 1].	worldCount < 2 ifTrue: [^outer].	roots _ outer rootMorphsAt: aPoint.	roots isEmpty ifTrue: [^ outer].	^ ((roots first morphsAt: aPoint)		detect: [:each | true]		ifNone: [roots first]) world! !!Morph methodsFor: 'WiW support' stamp: 'di 9/24/2000 12:48'!worldMorphsDo: aBlock	"Evaluate the given block for all worlds in this composite morph (including the receiver).	May be overriden to save looking in useless places."	submorphs size > 0 ifTrue:		[submorphs do: [:m | m worldMorphsDo: aBlock]].	self isWorldMorph ifTrue: [aBlock value: self]! !!PluggableListMorph methodsFor: 'events' stamp: 'di 9/24/2000 12:51'!worldMorphsDo: aBlock  "No worlds hiding here"	^ self! !!ScriptEditorMorph methodsFor: 'other' stamp: 'di 9/24/2000 12:54'!worldMorphsDo: aBlock  "No worlds hiding here"	^ self! !!Viewer methodsFor: 'as yet unclassified' stamp: 'di 9/24/2000 12:55'!worldMorphsDo: aBlock  "No worlds hiding here"	^ self! !