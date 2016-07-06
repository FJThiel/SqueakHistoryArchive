'From Squeak3.1alpha of 5 February 2001 [latest update: #3538] on 7 February 2001 at 7:13:52 am'!"Change Set:		moreLockingDate:			7 February 2001Author:			Bob ArningFurther tweak to locking parts of SystemWindow necessitated by recent changes moving some morphs down a layer"!!SystemWindow methodsFor: 'top window' stamp: 'RAA 2/7/2001 07:11'!lockInactivePortions	"Make me unable to respond to mouse and keyboard"	"Control boxes remain active, except in novice mode"	self submorphsDo: [:m |		m == labelArea ifFalse: [			m lock		]	].	labelArea submorphsDo: [:m |		(m == closeBox or: [m == collapseBox]) ifTrue: [			Preferences noviceMode ifTrue: [m lock]		] ifFalse: [			m lock		]	].! !!SystemWindow methodsFor: 'events' stamp: 'RAA 2/7/2001 07:11'!handleListenEvent: evt	"Make sure we lock our contents after DnD has finished"	evt isMouse ifFalse:[^self].	evt hand hasSubmorphs ifTrue:[^self]. "still dragging"	self == TopWindow ifFalse:[self lockInactivePortions].	evt hand removeMouseListener: self.! !!SystemWindow methodsFor: 'events' stamp: 'RAA 2/7/2001 07:12'!mouseLeaveDragging: evt	"lock children after drop operations"	(self ~~ TopWindow and:[evt hand hasSubmorphs]) ifTrue:[		self lockInactivePortions.		evt hand removeMouseListener: self.	].! !