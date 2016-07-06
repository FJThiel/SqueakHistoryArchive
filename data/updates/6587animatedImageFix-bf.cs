'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6569] on 25 February 2005 at 11:19:28 am'!"Change Set:		AnimatedImageFix-bfDate:			25 February 2005Author:			Bert FreudenbergFixes incremental animated images as reported by Scott Wallace"!GIFReadWriter subclass: #AnimatedGIFReadWriter	instanceVariableNames: 'forms offsets delays comments '	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Files'!ImageMorph subclass: #AnimatedImageMorph	instanceVariableNames: 'images offsets delays stepTime nextTime imageIndex previousOffset '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Basic'!!AnimatedGIFReadWriter methodsFor: 'accessing' stamp: 'bf 2/25/2005 11:11'!allImages	| body colorTable |	stream class == ReadWriteStream ifFalse: [		stream binary.		self on: (ReadWriteStream with: (stream contentsOfEntireFile))].	localColorTable _ nil.	forms _ OrderedCollection new.	delays _ OrderedCollection new.	comments _ OrderedCollection new.	self readHeader.	[(body _ self readBody) == nil]		whileFalse: [colorTable _ localColorTable						ifNil: [colorPalette].			transparentIndex				ifNotNil: [transparentIndex + 1 > colorTable size						ifTrue: [colorTable _ colorTable forceTo: transparentIndex + 1 paddingWith: Color white].					colorTable at: transparentIndex + 1 put: Color transparent].			body colors: colorTable.			forms add: body.			delays add: delay].	^ forms! !!AnimatedImageMorph methodsFor: 'stepping and presenter' stamp: 'bf 2/25/2005 11:06'!step	| f d |	images isEmpty		ifTrue: [^ self].	nextTime > Time millisecondClockValue		ifTrue: [^self].	imageIndex _ imageIndex \\ images size + 1.	f _ images at: imageIndex.	f displayOn: self image at: 0@0 rule: Form paint.	self invalidRect: (self position + f offset extent: f extent).	d _ (delays at: imageIndex) ifNil: [0].	nextTime := Time millisecondClockValue + d! !!AnimatedImageMorph methodsFor: 'private' stamp: 'bf 2/25/2005 11:18'!fromReader: reader	images _ reader forms.	delays _ reader delays.	imageIndex _ 0.	self image: (Form extent: images first extent depth: 32).	self step! !!AnimatedImageMorph methodsFor: 'private' stamp: 'bf 2/25/2005 11:09'!initialize	nextTime := Time millisecondClockValue.	imageIndex := 1.	stepTime := 10.	super initialize! !ImageMorph subclass: #AnimatedImageMorph	instanceVariableNames: 'images delays stepTime nextTime imageIndex'	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-Basic'!AnimatedGIFReadWriter removeSelector: #offsets!GIFReadWriter subclass: #AnimatedGIFReadWriter	instanceVariableNames: 'forms delays comments'	classVariableNames: ''	poolDictionaries: ''	category: 'Graphics-Files'!