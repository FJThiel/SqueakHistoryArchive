'From SqueakLight|II of 31 May 2008 [latest update: #7077] on 10 June 2008 at 7:43:32 am'!!PositionableStream methodsFor: 'testing' stamp: 'dc 2/27/2007 16:16'!isEmpty	"Answer whether the receiver's contents has no elements."	"Returns true if both the set of past and future sequence values ofthe receiver are empty. Otherwise returns false"	^ self atEnd and: [position = 0]! !