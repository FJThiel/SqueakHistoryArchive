'From Squeak3.1alpha of 5 February 2001 [latest update: #3546] on 8 February 2001 at 1:20:05 pm'!"Change Set:		hibernateDate:			8 February 2001Author:			Bob Arninghibernate project thumbnails to save some memory"!!Project methodsFor: 'menu messages' stamp: 'raa 2/8/2001 10:39'!saveState	"Save the current state in me prior to leaving this project"	changeSet _ Smalltalk changes.	thumbnail ifNotNil: [thumbnail hibernate].	Smalltalk isMorphic		ifTrue:			[world _ Display bestGuessOfCurrentWorld.			world sleep]		ifFalse:			[world _ ScheduledControllers.			ScheduledControllers unCacheWindows].	Sensor eventQueue: nil. "Will be reinstalled by World>>install"	transcript _ Transcript.! !!ProjectViewMorph methodsFor: 'drawing' stamp: 'raa 2/8/2001 10:40'!ensureImageReady	self isTheRealProjectPresent ifFalse: [^self].	project thumbnail ifNil: [		image fill: image boundingBox rule: Form over 			fillColor: project defaultBackgroundColor.		^self	].	project thumbnail ~~ lastProjectThumbnail ifTrue: ["scale thumbnail to fit my bounds"		lastProjectThumbnail _ project thumbnail.		self updateImageFrom: lastProjectThumbnail.		project thumbnail ifNotNil: [project thumbnail hibernate].		image borderWidth: 1	].! !