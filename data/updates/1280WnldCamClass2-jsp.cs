'From Squeak 2.4b of April 23, 1999 on 29 June 1999 at 1:05:08 am'!"Change Set:		WnldCamClass2Date:			29 June 1999Author:			Jeff PierceOk, this actually does what the last change set should've done.  =)"!!WonderlandCamera class methodsFor: 'change logging'!acceptsLoggingOfCompilation	^ (self == WonderlandCamera) or: [ self inheritsFrom: WonderlandCamera ].! !!WonderlandCamera class methodsFor: 'change logging' stamp: 'jsp 6/29/1999 01:00'!wantsChangeSetLogging	^ (self == WonderlandCamera) or: [ self inheritsFrom: WonderlandCamera ].! !