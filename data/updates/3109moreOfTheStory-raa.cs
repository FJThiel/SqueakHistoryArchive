'From Squeak2.9alpha of 17 July 2000 [latest update: #3165] on 12 December 2000 at 2:00:46 pm'!"Change Set:		moreOfTheStoryDate:			12 December 2000Author:			Bob Arning- make page image larger in camera mark and move numeric vaues to balloon help.- force coordinate displays back to integer"!!ZASMCameraMarkMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/12/2000 13:57'!cameraPoint: aPoint cameraScale: aNumber controller: aController page: aBookPage 	self setProperty: #cameraPoint toValue: aPoint.	self setProperty: #cameraScale toValue: aNumber.	self setProperty: #cameraController toValue: aController.	self setProperty: #bookPage toValue: aBookPage.	self addMorphBack: (ImageMorph new image: (aBookPage imageForm scaledToSize: 80@80)) lock.	self setBalloonText: aPoint rounded printString,'  ',(aNumber roundTo: 0.001) printString! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/12/2000 13:55'!keyStroke: anEvent	"hack.. use this as an opportunity to fix old versions"	self patchOldVersion1.! !!ZoomAndScrollControllerMorph methodsFor: 'as yet unclassified' stamp: 'RAA 12/12/2000 13:55'!patchOldVersion1	"hack.. use this as an opportunity to fix old versions"	self allMorphsDo: [:m |		((m isKindOf: UpdatingStringMorph) and: [m getSelector == #cameraPoint]) ifTrue: [			m getSelector: #cameraPointRounded		].	].! !"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."ZoomAndScrollControllerMorph allInstances do: [ :each | each patchOldVersion1].!