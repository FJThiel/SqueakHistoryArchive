'From Squeak 2.1 of June 30, 1998 on 12 July 1998 at 1:33:48 pm'!!DataStream methodsFor: 'all' stamp: 'tk 7/12/1998 13:32'!readShortRef	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."	^ self objectAt: (byteStream nextNumber: 2)! !!DataStream methodsFor: 'all' stamp: 'tk 7/12/1998 13:16'!vacantRef	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference	 position' to identify a reference that's not yet filled in. This must be a	 value that won't be used as an ordinary reference. Cf. outputReference: and	 readReference. -- 	 NOTE: We could use a different type ID for vacant-refs rather than writing		object-references with a magic value. (The type ID and value are		overwritten by ordinary object-references when weak refs are fullfilled.)"	^ SmallInteger maxVal! !!DummyStream methodsFor: 'all' stamp: 'tk 7/12/1998 12:51'!position: anOffset	"Pretend to position wherever the caller says!!"! !!Morph commentStamp: 'tk 7/12/1998 13:33' prior: 0!A morph (from the Greek "shape" or "form") is an interactive graphical object.All morphs owned by a morph are held in submorphs.  All coordinates are global screen coordinates.  (Except those that are flexed, contained inside a FlexMorph.)All show unless they are 'hidden'.  Hidden morphs:� are still in submorphs� have a position of about 1000000@100000� have a property #relPos that is its relative position inside its owner.� has a property #hidden set to true.� when fullBounds of a morph is computed, don't include morphs that have the property hidden = true.� "Stop" must not move hidden morphs back onto the playfield (PasteUpMorph)!!Morph methodsFor: 'geometry' stamp: 'tk 7/9/1998 13:56'!fullBounds	fullBounds ifNil: [		fullBounds _ self bounds.		submorphs size > 0 ifTrue: [			submorphs do: [:m | 				m bounds origin x < 50000 "quick test for not hidden"					ifTrue: [fullBounds _ fullBounds quickMerge: m fullBounds]					ifFalse: [(self valueOfProperty: #hidden) ifNil: [						"real test says not hidden"						fullBounds _ fullBounds quickMerge: m fullBounds]]]]].	^ fullBounds! !!Morph methodsFor: 'e-toy commands' stamp: 'tk 7/9/1998 13:44'!hide	"Move this morph way, way offstage!!  Remember its coordinates in relative form, in case window moves while hidden."	owner ifNil: [^ self].	self position < (5000@5000) ifTrue: [		self setProperty: #relPosition toValue: (self position - owner position).		self position: self position + (1000000@100000).		self setProperty: #hidden toValue: true].! !!Morph methodsFor: 'e-toy commands' stamp: 'tk 7/9/1998 13:57'!show	"Make sure this morph is on-stage."	| saved |	self world ifNil: [^ self].	"would like to set bounds, but we must be in a world"	(self valueOfProperty: #hidden) ifNil: [^ self].	(saved _ self valueOfProperty: #relPosition) ifNotNil: [		self position: saved + owner position.		self setProperty: #hidden toValue: nil.	"showing"		self wrap.	"be sure I'm on-stage"		"self bottomRight = owner bottomRight ifTrue: [			self error: 'Why did object lose position?']"		].! !!Morph methodsFor: 'e-toy commands' stamp: 'tk 7/8/1998 23:47'!wrap	| myBox box newX newY wrapped |	owner ifNil: [^ self].	myBox _ self fullBounds.	myBox corner < (50000@50000) ifFalse: [		self inform: 'Who is trying to wrap a hidden object?'. ^ self].	box _ owner bounds.	newX _ self position x.	newY _ self position y.	wrapped _ false.	((myBox right < box left) or: [myBox left > box right]) ifTrue: [		newX _ box left + ((self position x - box left) \\ box width).		wrapped _ true].	((myBox bottom < box top) or: [myBox top > box bottom]) ifTrue: [		newY _ box top + ((self position y - box top) \\ box height).		wrapped _ true].	self position: newX@newY.	(wrapped and: [owner isPlayfieldLike])		ifTrue: [owner changed].  "redraw all turtle trails if wrapped"! !!Morph methodsFor: 'e-toy support' stamp: 'tk 7/9/1998 14:06'!goHome	| box close |	owner ifNotNil: [		close _ bounds origin x < 50000. "quick test for not hidden"		close ifFalse: ["real but slow test"			close _ (self valueOfProperty: #hidden) ~~ true].		close ifTrue: [			box _ owner.			self left < box left ifTrue: [self position: box left@self position y].			self right > box right ifTrue: [self position: (box right - self width)@self position y].			self top < box top ifTrue: [self position: self position x@box top].			self bottom > box bottom ifTrue: [				self position: self position x@(box bottom - self height)]]].! !!Morph methodsFor: 'other' stamp: 'tk 7/11/1998 18:53'!storeDataOn: aDataStream	"Let all Morphs be written out.  All owners are weak references.  They only go out if the owner is in the tree being written."	| cntInstVars cntIndexedVars ti localInstVars |	"block my owner unless he is written out by someone else"	cntInstVars _ self class instSize.	cntIndexedVars _ self basicSize.	localInstVars _ Morph instVarNames.	ti _ 2.  	((localInstVars at: ti) = 'owner') & (Morph superclass == Object) ifFalse:			[self error: 'this method is out of date'].	aDataStream		beginInstance: self class		size: cntInstVars + cntIndexedVars.	1 to: ti-1 do:		[:i | aDataStream nextPut: (self instVarAt: i)].	aDataStream nextPutWeak: owner.	"owner only written if in our tree"	ti+1 to: cntInstVars do:		[:i | aDataStream nextPut: (self instVarAt: i)].	1 to: cntIndexedVars do:		[:i | aDataStream nextPut: (self basicAt: i)]! !!ReferenceStream methodsFor: 'all' stamp: 'tk 7/12/1998 13:30'!beginInstance: aClass size: anInteger	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.	In ReferenceStream, class is just 5 bytes for shared symbol.	SmartRefStream puts out the names and number of class's instances variables for checking.6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "	| short ref |	short _ true.	"All tests for object header that can be written in 4 bytes"	anInteger <= 254 ifFalse: [short _ false].	"one byte size"	ref _ references at: aClass name ifAbsent: [short _ false. nil].	ref isInteger ifFalse: [short _ false].	short ifTrue: [short _ (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"	short ifTrue: [		byteStream skip: -1.		short _ byteStream next = 9.		byteStream skip: 0].	"ugly workaround"	short 		ifTrue: ["passed all the tests!!"			byteStream skip: -1; nextPut: 16; "type = short header"				nextPut: anInteger + 1;	"size is short"				nextNumber: 2 put: ref]		ifFalse: [			"default to normal longer object header"			byteStream nextNumber: 4 put: anInteger + 1.			self nextPut: aClass name].! !