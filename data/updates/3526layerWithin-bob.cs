'From Squeak3.1alpha of 5 February 2001 [latest update: #3614] on 16 February 2001 at 2:01:21 pm'!"Change Set:		layerWithinDate:			16 February 2001Author:			Bob Arningwhen adding morphs to front of a layer, consider only their layer number within the new owner"!!Morph methodsFor: 'WiW support' stamp: 'RAA 2/16/2001 13:57'!addMorphInFrontOfLayer: aMorph	| targetLayer layerHere |	targetLayer _ aMorph morphicLayerNumberWithin: self.	submorphs do: [ :each |		each == aMorph ifTrue: [^self].		layerHere _ each morphicLayerNumberWithin: self.		"the <= is the difference - it insures we go to the front of our layer"		targetLayer <= layerHere ifTrue: [			^self addMorph: aMorph inFrontOf: each		].	].	self addMorphBack: aMorph.! !!Morph methodsFor: 'WiW support' stamp: 'RAA 2/16/2001 13:54'!morphicLayerNumberWithin: anOwner	"helpful for insuring some morphs always appear in front of or behind others.	smaller numbers are in front"	^(owner isNil or: [owner isWorldMorph or: [anOwner == owner]]) ifTrue: [		self valueOfProperty: #morphicLayerNumber ifAbsent: [100]	] ifFalse: [		owner morphicLayerNumber	].	"leave lots of room for special things"! !