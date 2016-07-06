'From Squeak3.5 of ''11 April 2003'' [latest update: #5180] on 13 August 2004 at 5:51:59 pm'!"Change Set:		HaloSize-dgdDate:			17 May 2004Author:			Diego Gomez Deck"Preferences	addPreference: #biggerHandles	category: #halos	default: true	balloonHelp: 'If true, use bigger handles in halo.'.!!HaloMorph methodsFor: 'private' stamp: 'dgd 5/17/2004 19:53'!addHandle: handleSpec on: eventName send: selector to: recipient	"Add a handle within the halo box as per the haloSpec, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."	| handle aPoint iconName colorToUse |	aPoint _ self positionIn: haloBox horizontalPlacement: handleSpec horizontalPlacement verticalPlacement: handleSpec verticalPlacement.	handle _ EllipseMorph		newBounds: (Rectangle center: aPoint extent: self handleSize asPoint)		color: (colorToUse _ Color colorFrom: handleSpec color).	handle borderColor: colorToUse muchDarker.	self addMorph: handle.	(iconName _ handleSpec iconSymbol) ifNotNil:		[ | form |		form _ ScriptingSystem formAtKey: iconName.		form ifNotNil:			[handle addMorphCentered: (ImageMorph new				image: form; 				color: colorToUse makeForegroundColor;				lock)]].	handle on: #mouseUp send: #endInteraction to: self.	handle on: eventName send: selector to: recipient.	self isMagicHalo ifTrue:[		handle on: #mouseEnter send: #handleEntered to: self.		handle on: #mouseLeave send: #handleLeft to: self].	handle setBalloonText: (target balloonHelpTextForHandle: handle) translated.	^ handle! !!HaloMorph methodsFor: 'private' stamp: 'dgd 5/17/2004 19:48'!addHandleAt: aPoint color: aColor icon: iconName on: eventName send: selector to: recipient	"Add a handle centered at the given point with the given color, and set it up to respond to the given event by sending the given selector to the given recipient.  Return the handle."	| handle |	handle _ EllipseMorph		newBounds: (Rectangle center: aPoint extent: self handleSize asPoint)		color: aColor.	handle borderColor: aColor muchDarker.	self addMorph: handle.	iconName ifNotNil:		[ | form |		form _ ScriptingSystem formAtKey: iconName.		form ifNotNil:			[handle addMorphCentered: (ImageMorph new				image: form; 				color: aColor makeForegroundColor;				lock)]].	handle on: #mouseUp send: #endInteraction to: self.	handle on: eventName send: selector to: recipient.	handle setBalloonText: (target balloonHelpTextForHandle: handle) translated.	^ handle! !!HaloMorph methodsFor: 'private' stamp: 'dgd 5/17/2004 20:18'!handleSize	^ Preferences biggerHandles		ifTrue: [20]		ifFalse: [16]! !HaloMorph class removeSelector: #initialize!Morph subclass: #HaloMorph	instanceVariableNames: 'target innerTarget positionOffset angleOffset minExtent growingOrRotating directionArrowAnchor haloBox simpleMode '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Widgets'!