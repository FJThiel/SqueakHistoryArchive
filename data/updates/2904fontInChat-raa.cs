'From Squeak2.9alpha of 17 July 2000 [latest update: #2951] on 5 November 2000 at 2:17:11 pm'!"Change Set:		fontInChatDate:			5 November 2000Author:			Bob Arning- include basic font info in chat messages so that font changes are preserved at recipient"!!EToyChatMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/5/2000 14:14'!acceptTo: someText forMorph: aMorph	| betterText |	betterText _ self improveText: someText forMorph: aMorph.	self 		transmitStreamedObject: (betterText eToyStreamedRepresentationNotifying: self) 		to: self ipAddress.	aMorph setText: '' asText.	self appendMessage: 		self startOfMessageFromMe,		' - ',		betterText,		String cr.	^true! !!EToyChatMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/5/2000 14:14'!improveText: someText forMorph: aMorph	| betterText conversions newAttr fontForAll |	fontForAll _ aMorph eToyGetMainFont.	betterText _ someText veryDeepCopy.	conversions _ OrderedCollection new.	betterText runs withStartStopAndValueDo: [:start :stop :attributes |		attributes do: [:att |			(att isMemberOf: TextFontChange) ifTrue: [				conversions add: {att. start. stop}			]		]	].	conversions do: [ :old |		betterText removeAttribute: old first from: old second to: old third.		newAttr _ TextFontReference toFont: (fontForAll fontAt: old first fontNumber).		newAttr fontNumber: old first fontNumber.		betterText addAttribute: newAttr from: old second to: old third.	].	^betterText! !!EToyMultiChatMorph methodsFor: 'as yet unclassified' stamp: 'RAA 11/5/2000 14:15'!acceptTo: someText forMorph: aMorph	| streamedMessage betterText |	betterText _ self improveText: someText forMorph: aMorph.	streamedMessage _ {targetIPAddresses. betterText} eToyStreamedRepresentationNotifying: self.	targetIPAddresses do: [ :each |		self 			transmitStreamedObject: streamedMessage			to: each.	].	aMorph setText: '' asText.	self appendMessage: 		self startOfMessageFromMe,		' - ',		betterText,		String cr.	^true! !!PluggableTextMorph methodsFor: 'model access' stamp: 'RAA 11/5/2000 14:10'!eToyGetMainFont	^ textMorph textStyle! !