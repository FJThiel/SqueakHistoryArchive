'From Squeak2.9alpha of 13 June 2000 [latest update: #2988] on 26 November 2000 at 10:20:38 am'!"Change Set:		257BrowserFix-akDate:			24 November 2000Author:			Andreas KuckartzSelecting 'class' button while '?' was selected resulted in both buttons being active."!!Browser methodsFor: 'metaclass' stamp: 'ak 11/24/2000 21:46'!classMessagesIndicated	"Answer whether the messages to be presented should come from the 	metaclass."	^ self metaClassIndicated and: [self classCommentIndicated not]! !