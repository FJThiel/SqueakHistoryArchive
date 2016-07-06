'From Squeak2.9alpha of 17 July 2000 [latest update: #3008] on 13 November 2000 at 2:42:15 pm'!"Change Set:		etoycom.csDate:			10 November 2000Author:			Michael RuegerThis change set includes two small cleanups:- Moves the transmit call to EToyPeerToPeer.- introduces a class instance variable to hold the known message types. New application now can register their message type without modifying code in class EToyincomingMessage."!Object subclass: #EToyIncomingMessage	instanceVariableNames: ''	classVariableNames: 'MessageHandlers MessageTypes '	poolDictionaries: ''	category: 'Morphic-Experimental'!EToyIncomingMessage class	instanceVariableNames: ''!!EToyCommunicatorMorph methodsFor: 'as yet unclassified' stamp: 'mir 10/12/2000 14:55'!transmitStreamedObject: outData as: objectCategory to: anIPAddress	EToyPeerToPeer transmitStreamedObject: outData as: objectCategory to: anIPAddress for: self! !!EToyCommunicatorMorph methodsFor: 'as yet unclassified' stamp: 'mir 10/10/2000 12:47'!transmitStreamedObject: outData to: anIPAddress	self transmitStreamedObject: outData as: self transmittedObjectCategory to: anIPAddress! !!EToyIncomingMessage class methodsFor: 'message types' stamp: 'RAA 11/13/2000 14:39'!allTypes	^MessageTypes ifNil: [		MessageTypes _ {			self typeKeyboardChat.			self typeMorph.			self typeFridge.			self typeStatusRequest.			self typeStatusReply.			self typeSeeDesktop.			self typeAudioChat.			self typeAudioChatContinuous.			self typeMultiChat.		}	]! !!EToyIncomingMessage class methodsFor: 'message types' stamp: 'RAA 11/13/2000 14:39'!registerType: aMessageType	MessageTypes _ self allTypes copyWith: aMessageType! !!EToyIncomingMessage class methodsFor: 'message types' stamp: 'RAA 11/13/2000 14:39'!unregisterType: aMessageType	MessageTypes _ self allTypes copyWithout: aMessageType! !!EToyPeerToPeer class methodsFor: 'as yet unclassified' stamp: 'mir 10/10/2000 12:51'!transmitStreamedObject: outData as: objectCategory to: anIPAddress for: aCommunicator	| null |	null _ String with: 0 asCharacter.	self new 		sendSomeData: {			objectCategory,null. 			Preferences defaultAuthorName,null.			outData		}		to: anIPAddress		for: aCommunicator! !!EToySenderMorph methodsFor: 'as yet unclassified' stamp: 'mir 10/12/2000 14:54'!acceptDroppingMorph: morphToDrop event: evt	| myCopy outData |	(morphToDrop isKindOf: NewHandleMorph) ifTrue: [			"don't send these"		^morphToDrop rejectDropMorphEvent: evt.	].	self eToyRejectDropMorph: morphToDrop event: evt.		"we don't really want it"	myCopy _ morphToDrop veryDeepCopy.	"gradient fills require doing this second"	myCopy setProperty: #positionInOriginatingWorld toValue: morphToDrop position.	self stopFlashing.	outData _ myCopy eToyStreamedRepresentationNotifying: self.	self resetIndicator: #working.	self transmitStreamedObject: outData to: self ipAddress.! !