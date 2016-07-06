'From Squeak3.3alpha of 18 January 2002 [latest update: #4989] on 20 October 2002 at 2:54:12 am'!"Change Set:		stampsFromChgList-swDate:			20 October 2002Author:			Scott WallaceFixes the very-long-standing bug that methods in fileouts created from a ChangeList browser were devoid of time-stamps."!!ChangeRecord methodsFor: 'access' stamp: 'sw 10/20/2002 02:53'!fileOutOn: aFileStream	"File the receiver out on the given file stream"	| aString |	type == #method		ifTrue:			[aFileStream nextPut: $!!.			aString _  class asString							, (meta ifTrue: [' class methodsFor: ']									ifFalse: [' methodsFor: '])							, category asString printString.			stamp ifNotNil:				[aString _ aString, ' stamp: ''', stamp, ''''].			aFileStream nextChunkPut: aString.			aFileStream cr].	type == #preamble ifTrue: [aFileStream nextPut: $!!].	type == #classComment		ifTrue:			[aFileStream nextPut: $!!.			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.			aFileStream cr].	aFileStream nextChunkPut: self string.	type == #method ifTrue: [aFileStream nextChunkPut: ' '].	aFileStream cr! !