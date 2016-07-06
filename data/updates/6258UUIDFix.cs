'From Squeak3.8alpha of ''17 July 2004'' [latest update: #5976] on 10 September 2004 at 11:18:27 pm'!"Change Set:		UUIDFixDate:			10 September 2004Author:			Daniel VainsencherRemove performance bug where UUID was generate unique new IDs even when it was asked merely to instantiate a known UUID from a string. v2: some improvements suggested by John Macintosh. Thanks"!!UUID class methodsFor: 'instance creation' stamp: 'dvf 9/10/2004 23:10'!fromString: aString	| object |	aString size ~= 36 ifTrue: [Error signal].	object := self nilUUID. 	object asUUID: aString.	^object! !!UUID class methodsFor: '*smbase-macsafe' stamp: 'dvf 9/10/2004 23:11'!fromString36: aString	"Decode the UUID from a base 36 string using 0-9 and lowercase a-z.	This is the shortest representation still being able to work as	filenames etc since it does not depend on case nor characters	that might cause problems."	| object num |	object := self nilUUID.	num := Integer readFrom: aString asUppercase readStream base: 36.	16 to: 1 by: -1 do: [:i |		num size < i			ifTrue: [object at: i put: 0]			ifFalse: [object at: i put: (num digitAt: i)]].	^object! !