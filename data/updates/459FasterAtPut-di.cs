'From Squeak 2.3 beta of Nov 25, 1998 on 5 December 1998 at 12:49:00 pm'!"Change Set:		FasterAtPutDate:			2 December 1998Author:			Dan IngallsSpeeds up at: and at:put: by eliminating logic for the 4 classes serviced.NOTE: These changes were developed together with several others and so may not work in isolation."!!Interpreter methodsFor: 'common selector sends' stamp: 'di 12/2/1998 14:10'!bytecodePrimAt	| index rcvr result rcvrClass |	index _ self internalStackTop.	rcvr _ self internalStackValue: 1.	successFlag _ self isIntegerObject: index.	successFlag ifTrue:		[rcvrClass _ self fetchClassOf: rcvr.		index _ self integerValueOf: index.		rcvrClass = (self splObj: ClassString) ifTrue:			[result _ self splObject: rcvr at: index.			successFlag ifTrue: [^ self internalPop: 2 thenPush: (self characterForAscii: result)]].		rcvrClass = (self splObj: ClassArray) ifTrue:			[result _ self splObject: rcvr at: index.			successFlag ifTrue: [^ self internalPop: 2 thenPush: result]].		rcvrClass = (self splObj: ClassBitmap) ifTrue:			[self externalizeIPandSP.			result _ self splObject: rcvr at: index.			self internalizeIPandSP.			successFlag ifTrue: [^ self internalPop: 2 thenPush: result]].		rcvrClass = (self splObj: ClassByteArray) ifTrue:			[result _ self splObject: rcvr at: index.			successFlag ifTrue: [^ self internalPop: 2 thenPush: result]]].		messageSelector _ self specialSelector: 16.		argumentCount _ 1.		self normalSend.! !!Interpreter methodsFor: 'common selector sends' stamp: 'di 12/2/1998 14:24'!bytecodePrimAtPut	| value index rcvr rcvrClass ascii |	value _ self internalStackTop.	index _ self internalStackValue: 1.	rcvr _ self internalStackValue: 2.	successFlag _ self isIntegerObject: index.	successFlag ifTrue:		[rcvrClass _ self fetchClassOf: rcvr.		index _ self integerValueOf: index.		rcvrClass = (self splObj: ClassString) ifTrue:			[ascii _ self asciiOfCharacter: value.			successFlag ifTrue: [self splObject: rcvr at: index put: ascii].			successFlag ifTrue: [^ self internalPop: 3 thenPush: value]].		rcvrClass = (self splObj: ClassArray) ifTrue:			[self splObject: rcvr at: index put: value.			successFlag ifTrue: [^ self internalPop: 3 thenPush: value]].		rcvrClass = (self splObj: ClassBitmap) ifTrue:			[self splObject: rcvr at: index put: value.			successFlag ifTrue: [^ self internalPop: 3 thenPush: value]].		rcvrClass = (self splObj: ClassByteArray) ifTrue:			[self splObject: rcvr at: index put: value.			successFlag ifTrue: [^ self internalPop: 3 thenPush: value]]].	messageSelector _ self specialSelector: 17.	argumentCount _ 2.	self normalSend! !!Interpreter methodsFor: 'array and stream primitives' stamp: 'di 12/2/1998 14:13'!splObject: array at: index	"This is a special copy of stObject:at: for the special classes only.	There are no fixed fields, and no context check."	| hdr fmt stSize |	self inline: false.	hdr _ self baseHeader: array.	fmt _ (hdr >> 8) bitAnd: 16rF.	stSize _ self lengthOf: array baseHeader: hdr format: fmt.	(index >= 1 and: [index <= stSize])		ifTrue: [^ self subscript: array with: index format: fmt]		ifFalse: [successFlag _ false.  ^ 0].! !!Interpreter methodsFor: 'array and stream primitives' stamp: 'di 12/2/1998 14:24'!splObject: array at: index put: value	"This is a special copy of stObject:at: for the special classes only.	There are no fixed fields, and no context check."	| hdr fmt stSize |	self inline: false.	hdr _ self baseHeader: array.	fmt _ (hdr >> 8) bitAnd: 16rF.	stSize _ self lengthOf: array baseHeader: hdr format: fmt.	(index >= 1 and: [index <= stSize])		ifTrue: [self subscript: array with: index storing: value format: fmt]		ifFalse: [successFlag _ false]! !