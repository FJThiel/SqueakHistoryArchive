'From Squeak3.1alpha of 28 February 2001 [latest update: #4188] on 9 July 2001 at 3:36:18 pm'!"Change Set:		OopsSetFix-tkDate:			9 July 2001Author:			Ted KaehlerMy previous addition of #species to Set caused bad things to happen.  Dictionary select: did not return a Dictionary.  This undoes that change."!!Set methodsFor: 'testing'!= aSet	(aSet isKindOf: Set) ifFalse: [^ false].	self size = aSet size ifFalse: [^ false].	self do: [:each | (aSet includes: each) ifFalse: [^ false]].	^ true! !Set removeSelector: #species!