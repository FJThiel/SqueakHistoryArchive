'From Squeak3.2alpha of 3 October 2001 [latest update: #4470] on 5 November 2001 at 12:05:03 pm'!"Change Set:		commentInRecent-swDate:			5 November 2001Author:			Scott WallaceChanges to class comments will now show up in the Recent-Submissions browser."!!ClassDescription methodsFor: 'accessing' stamp: 'sw 11/5/2001 00:53'!comment: aStringOrText	"Set the receiver's comment to be the argument, aStringOrText."	self theNonMetaClass classComment: aStringOrText.	Smalltalk changes commentClass: self.	Utilities noteMethodSubmission: #Comment forClass: self theNonMetaClass! !!ClassDescription methodsFor: 'accessing' stamp: 'sw 11/5/2001 00:55'!comment: aStringOrText stamp: aStamp	"Set the receiver's comment to be the argument, aStringOrText."	self theNonMetaClass classComment: aStringOrText stamp: aStamp.	Smalltalk changes commentClass: self theNonMetaClass.	Utilities noteMethodSubmission: #Comment forClass: self theNonMetaClass! !!MethodReference methodsFor: 'queries' stamp: 'sw 11/5/2001 00:53'!printOn: aStream	"Print the receiver on a stream"	super printOn: aStream.	aStream nextPutAll: ' ', self actualClass name, ' >> ', methodSymbol! !!Utilities class methodsFor: 'recent method submissions' stamp: 'sw 11/5/2001 01:16'!browseRecentSubmissions	"Open up a browser on the most recent methods submitted in the image.  5/96 sw."	"Utilities browseRecentSubmissions"	| recentMessages |	self recentMethodSubmissions size == 0 ifTrue:		[^ self inform: 'There are no recent submissions'].		recentMessages _ RecentSubmissions copy reversed.	RecentMessageSet 		openMessageList: recentMessages 		name: 'Recent submissions -- youngest first ' 		autoSelect: nil! !!Utilities class methodsFor: 'recent method submissions' stamp: 'sw 11/5/2001 12:04'!purgeRecentSubmissionsOfMissingMethods	"Utilities purgeRecentSubmissionsOfMissingMethods"	| keep |	self flag: #mref.	"fix for faster references to methods"	RecentSubmissions _ RecentSubmissions select:		[:aSubmission | 			Utilities setClassAndSelectorFrom: aSubmission in:				[:aClass :aSelector |					keep _ (aClass == nil) not						and: [aClass isInMemory						and: [aSelector == #Comment or: [(aClass compiledMethodAt: aSelector ifAbsent: [nil]) notNil]]]].			keep]! !!Utilities class methodsFor: 'recent method submissions' stamp: 'sw 11/5/2001 01:16'!recentSubmissionsWindow	"Answer a SystemWindow holding recent submissions"	| recentMessages messageSet |	recentMessages _ RecentSubmissions copy reversed.	messageSet _ RecentMessageSet messageList: recentMessages.	messageSet autoSelectString: nil.	^ (messageSet inMorphicWindowLabeled: 'Recent submissions -- youngest first') applyModelExtent	"Utilities recentSubmissionsWindow openInHand"! !