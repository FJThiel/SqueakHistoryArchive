'From Squeak3.1alpha of 5 February 2001 [latest update: #3812] on 8 March 2001 at 6:26:47 am'!"Change Set:		sendWithoutCopyDate:			8 March 2001Author:			Bob Arning- drop #veryDeepCopy when sending morphs with players to another image- also add #bytesPerElement to ColorArray so that they arrive safely"!!ColorArray methodsFor: 'converting' stamp: 'RAA 3/8/2001 06:24'!bytesPerElement	^4! !!EToyFridgeMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/7/2001 22:31'!acceptDroppingMorph: morphToDrop event: evt	| outData |	(morphToDrop isKindOf: NewHandleMorph) ifTrue: [		"don't send these"		^morphToDrop rejectDropMorphEvent: evt	].	self eToyRejectDropMorph: morphToDrop event: evt.		"we will keep a copy"	(morphToDrop isKindOf: EToySenderMorph) ifTrue: [		self class addRecipient: morphToDrop.		^self rebuild	].	self stopFlashing.	"7 mar 2001 - remove #veryDeepCopy"	outData _ morphToDrop eToyStreamedRepresentationNotifying: self.	self resetIndicator: #working.	self class fridgeRecipients do: [ :each |		self transmitStreamedObject: outData to: each ipAddress	].! !!EToySenderMorph methodsFor: 'as yet unclassified' stamp: 'RAA 3/7/2001 22:31'!acceptDroppingMorph: morphToDrop event: evt	| myCopy outData |	(morphToDrop isKindOf: NewHandleMorph) ifTrue: [			"don't send these"		^morphToDrop rejectDropMorphEvent: evt.	].	self eToyRejectDropMorph: morphToDrop event: evt.		"we don't really want it"	"7 mar 2001 - remove #veryDeepCopy"	myCopy _ morphToDrop.	"gradient fills require doing this second"	myCopy setProperty: #positionInOriginatingWorld toValue: morphToDrop position.	self stopFlashing.	outData _ myCopy eToyStreamedRepresentationNotifying: self.	self resetIndicator: #working.	self transmitStreamedObject: outData to: self ipAddress.! !!NetworkTerminalMorph methodsFor: 'event handling' stamp: 'RAA 3/7/2001 22:32'!acceptDroppingMorph: morphToDrop event: evt	| myCopy outData null |	(morphToDrop isKindOf: NewHandleMorph) ifTrue: [			"don't send these"		^morphToDrop rejectDropMorphEvent: evt.	].	self eToyRejectDropMorph: morphToDrop event: evt.		"we don't really want it"	"7 mar 2001 - remove #veryDeepCopy"	myCopy _ morphToDrop.	"gradient fills require doing this second"	myCopy setProperty: #positionInOriginatingWorld toValue: morphToDrop position.	outData _ myCopy eToyStreamedRepresentationNotifying: nil.	null _ String with: 0 asCharacter.	EToyPeerToPeer new 		sendSomeData: {			EToyIncomingMessage typeMorph,null. 			Preferences defaultAuthorName,null.			outData		}		to: (NetNameResolver stringFromAddress: connection remoteAddress)		for: self.! !