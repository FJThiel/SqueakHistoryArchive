'From Squeak 2.1 of June 30, 1998 on 9 September 1998 at 5:17:21 pm'!!DataStream methodsFor: 'all' stamp: 'tk 9/9/1998 14:10'!next	"Answer the next object in the stream."	| type selector anObject isARefType pos |	type _ byteStream next.	type ifNil: [pos _ byteStream position.	"absolute!!!!"		byteStream close.	"clean up"		byteStream position = 0 			ifTrue: [self error: 'The file did not exist in this directory'] 			ifFalse: [self error: 'Unexpected end of object file'].		pos.	"so can see it in debugger"		^ nil].	type = 0 ifTrue: [pos _ byteStream position.	"absolute!!!!"		byteStream close.	"clean up"		self error: 'Expected start of object, but found 0'.		^ nil].	isARefType _ self noteCurrentReference: type.	selector _ #(readNil readTrue readFalse readInteger			readStringOld readSymbol readByteArray			readArray readInstance readReference readBitmap			readClass readUser readFloat readRectangle readShortInst 			readString) at: type.	anObject _ self perform: selector. "A method that recursively		calls next (readArray, readInstance, objectAt:) must save &		restore the current reference position."	isARefType ifTrue: [self beginReference: anObject].	"After reading the externalObject, internalize it.	 #readReference is a special case. Either:	   (1) We actually have to read the object, recursively calling		   next, which internalizes the object.	   (2) We just read a reference to an object already read and		   thus already interalized.	 Either way, we must not re-internalize the object here."	selector == #readReference ifFalse:		[anObject _ self internalize: anObject].	^ anObject! !!EventHandler methodsFor: 'object fileIn' stamp: 'ack 8/27/1998 13:32'!convertmmmmmmmmmmkkv0: varDict mmmmmmmmmmmmmmkkv0: smartRefStrm	"These variables are automatically stored into the new instance ('mouseDownRecipient' 'mouseDownSelector' 'mouseStillDownRecipient' 'mouseStillDownSelector' 'mouseUpRecipient' 'mouseUpSelector' 'mouseEnterRecipient' 'mouseEnterSelector' 'mouseLeaveRecipient' 'mouseLeaveSelector' 'keyStrokeRecipient' 'keyStrokeSelector' 'valueParameter' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"Be sure to to fill in ('mouseEnterLadenRecipient' 'mouseEnterLadenSelector' 'mouseLeaveLadenRecipient' 'mouseLeaveLadenSelector' ) and deal with the information in ()"! !!MorphExtension methodsFor: 'object fileIn' stamp: 'tk 9/9/1998 14:19'!comeFullyUpOnReload	"inst vars have default booplean values."	locked ifNil: [locked _ false].	visible ifNil: [visible _ true].	sticky ifNil: [sticky _ false].	isPartsDonor ifNil: [isPartsDonor _ false].	^ self! !!Player methodsFor: 'object fileIn' stamp: 'ack 8/27/1998 13:33'!convertdc0: varDict dcc0: smartRefStrm	"These variables are automatically stored into the new instance ('costume' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"Be sure to to fill in ('costumes' ) and deal with the information in ()"! !!SmartRefStream methodsFor: 'read write' stamp: 'tk 9/9/1998 14:01'!restoreClassInstVars	"Install the values of the class instance variables of UniClasses (i.e. scripts slotInfo).  classInstVars is ((#Player25 scripts slotInfo) ...)"	| normal aName newName newCls trans rList |	self moreObjects ifFalse: [^ self]. 	"are no UniClasses with class inst vars" 	classInstVars _ super next.	"Array of arrays"	normal _ Object class instSize.	"might give trouble if Player class superclass changes size"	(structures at: #Player ifAbsent: #()) = #(0 'dependents' 'costume') ifTrue: [		trans _ 1].	"now (0 costume costumes).  Do the conversion of Player class 			inst vars in Update 509."	classInstVars do: [:list |		aName _ (list at: 1) asSymbol.		rList _ list.		newName _ renamed at: aName ifAbsent: [aName].		newCls _ Smalltalk at: newName 				ifAbsent: [self error: 'UniClass definition missing'].		(trans == 1 and: [newCls inheritsFrom: Player]) ifTrue: [			"remove costumeDictionary from Player class inst vars"			rList _ rList asOrderedCollection.			rList removeAt: 4].	"costumeDictionary's value"		newCls class instSize = (normal+(rList size)-1) ifFalse: [			self error: 'UniClass superclass class has changed size'].			"Need to install a conversion method mechanism"		2 to: rList size do: [:ii |			newCls instVarAt: normal+ii-1 put: (rList at: ii)]].! !!SoundRecorder methodsFor: 'object fileIn' stamp: 'tk 9/9/1998 13:37'!convertssrrbpmmscn0: varDict ssrrrbpmmscn0: smartRefStrm	"These variables are automatically stored into the new instance ('stereo' 'samplingRate' 'recordedBuffers' 'recordProcess' 'bufferAvailableSema' 'paused' 'meteringBuffer' 'meterLevel' 'soundPlaying' 'currentBuffer' 'nextIndex' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"Be sure to to fill in ('recordLevel' ) and deal with the information in ()"! !!WorldMorph methodsFor: 'object fileIn' stamp: 'tk 9/9/1998 17:11'!convertbosfcepcbbfgccpmcpbttloiairfidcuwhavcdslp0: varDict bosfcebbfgccpmcpbttloiairfidcuwhavcdsllb0: smartRefStrm	"These variables are automatically stored into the new instance ('hands' 'activeHand' 'viewBox' 'canvas' 'damageRecorder' 'stepList' 'lastStepTime' ).	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."	"Be sure to to fill in ('lastCycleTime' 'balloonHelpEnabled' ) and deal with the information in ('playerList' )"	balloonHelpEnabled _ true! !DataStream removeSelector: #checkForPaths:!