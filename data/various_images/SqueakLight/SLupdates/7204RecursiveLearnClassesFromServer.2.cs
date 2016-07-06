'From SqueakLight|II of 31 May 2008 [latest update: #7201] on 6 September 2008 at 10:54:40 am'!!Object class methodsFor: 'instance creation' stamp: 'edc 9/6/2008 08:57'!lookForClass: aClass 	| path inputStream fcb superPseudo pseudo |	path := self lookForClassIn3dot10: aClass.	inputStream := HTTPLoader default retrieveContentsFor: path.	inputStream := RWBinaryOrTextStream with: inputStream content unzipped.	fcb := FilePackage new fullName: aClass;								fileInFrom: (MultiByteBinaryOrTextStream with: inputStream contents).	pseudo := fcb classes at: aClass.	superPseudo := pseudo definition copyUpTo: Character space.	Smalltalk		at: superPseudo asSymbol		ifAbsent: [self lookForClass: superPseudo].	ChangeSorter newChangesFromStream: inputStream named: aClass asString! !!Object class methodsFor: 'instance creation' stamp: 'edc 9/6/2008 10:54'!lookForClassIn3dot10: aClass 	| inputStream cat path |		Missing3dot10		ifNil: [inputStream := HTTPLoader default retrieveContentsFor: 'ftp.squeak.org/various_images/SqueakLight//SLupdates/Organizer3dot10.obj'.			inputStream := (MultiByteBinaryOrTextStream with: inputStream contents) reset.			inputStream setConverterForCode.			Smalltalk at: #Missing3dot10 put: inputStream fileInObjectAndCode].	cat := Missing3dot10				at: aClass				ifAbsent: [^ self lookForClassIn3dot9: aClass].	^ path := 'http://squeakros.atspace.com/3dot10/' , cat , '/' , aClass asString , '.sqz'! !!Object class methodsFor: 'instance creation' stamp: 'edc 9/6/2008 10:52'!lookForClassIn3dot9: aClass 	| inputStream cat path |	Missing3dot9		ifNil: [inputStream := HTTPLoader default retrieveContentsFor: 'ftp.squeak.org/various_images/SqueakLight//SLupdates/Organizer3dot9.obj'.			inputStream := (MultiByteBinaryOrTextStream with: inputStream contents) reset.			inputStream setConverterForCode.			Smalltalk at: #Missing3dot9 put: inputStream fileInObjectAndCode].	cat := Missing3dot9				at: aClass				ifAbsent: [^ self error: aClass , ' is not on  server '].	^path := 'http://squeakros.atspace.com/3dot9/' , cat , '/' , aClass asString , '.sqz'.	! !