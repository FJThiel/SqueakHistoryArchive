'From Squeak2.6 of 11 October 1999 [latest update: #1633] on 24 November 1999 at 8:51:48 pm'!"Change Set:		SequenceableCollectionHashDate:			24 November 1999Author:			TFIE, Tweaked by Dan IngallsTwo changes to SequenceableCollection:A slightly faster =A hash function that will be = for = collections.The tweak makes it good enough to serve as a replacement for Array hash.The postscript rehashes any sets affected by changing Array hash."!!SequenceableCollection methodsFor: 'comparing' stamp: 'di 11/24/1999 20:26'!= otherCollection    "Answer true if the receiver is equivalent to the <otherCollection>.    First test for identity, then rule out different species and sizes of collections.    As a last resort, examine each element of the receiver and the <otherCollection>."    | size |    self == otherCollection ifTrue: [^ true].    (self species == otherCollection species) ifFalse: [^ false].    	(size _ self size) = otherCollection size ifFalse: [^ false].	1 to: size do:		[:index |		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].	^ true! !!SequenceableCollection methodsFor: 'comparing' stamp: 'di 11/24/1999 20:30'!hash"Answer an integer hash value for the receiver such that,  -- the hash value of an unchanged object is constant over time, and  -- two equal objects have equal hash values."    | size |	(size _ self size) = 0 ifTrue: [^ 17171].	^ size + (self at: 1) hash + (self at: size) hash! !Array removeSelector: #hash!"Postscript:Rehash any sets that would be affected by changing the hash function for Arrays."  | ok |Set allSubInstances do:	[:x | (x isKindOf: Dictionary)		ifTrue: [ok _ true.				x keysDo: [:y | (y isMemberOf: Array) ifTrue: [ok _ false]].				ok ifFalse: [x rehash]]		ifFalse: [ok _ true.				x do: [:y | (y isMemberOf: Array) ifTrue: [ok _ false]].				ok ifFalse: [x rehash]]]!