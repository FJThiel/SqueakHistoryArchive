'From Squeak 3.2 of 11 July 2002 [latest update: #4917] on 13 August 2002 at 10:53:39 am'!"Change Set:		HashChanges2Date:			13 August 2002Author:			Andres ValloudPass 2.  Implement LargePositiveInteger>>hash.Rehash sets as appropriate."!!LargePositiveInteger methodsFor: 'comparing' stamp: 'SqR 8/13/2002 10:52'!hash	^ByteArray		hashBytes: self		startingWith: self species hash! !Set quickRehashAllSets.!