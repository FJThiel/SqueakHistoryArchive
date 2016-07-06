'From Squeak2.9alpha of 13 June 2000 [latest update: #2949] on 5 November 2000 at 5:21:09 pm'!"Change Set:		VRMLProgressDate:			5 November 2000Author:			Andreas RaabBetter progress reporting when reading VRML streams."!!VRMLNodeParser methodsFor: 'private' stamp: 'ar 11/5/2000 17:18'!progressUpdate: aVRMLStream	infoBar ifNil:[^self].	infoBar value: (aVRMLStream position * 100 // aVRMLStream size).! !!VRMLNodeParser methodsFor: 'scene parsing' stamp: 'ar 11/5/2000 17:19'!parseStatement: aVRMLStream	| token |	aVRMLStream skipSeparators.	token := aVRMLStream readName.	(VRMLStatements includesKey: token) ifTrue:[		^self dispatchOn: token in: VRMLStatements with: aVRMLStream ifNone:[]].	(nodeTypes includesKey: token) ifTrue:[		self progressUpdate: aVRMLStream.		^(nodeTypes at: token) readFrom: aVRMLStream in: self].	self error:'Unkown token'.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldColorFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (B3DColor4Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldColorFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldColorFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldFloatFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (B3DFloatArray new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldFloatFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldFloatFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldInt32From: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (IntegerArray new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldInt32From: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldInt32From: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldNodeFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldNodeFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldNodeFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldRotationFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (B3DRotationArray new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldRotationFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldRotationFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldStringFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldStringFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldStringFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldTimeFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldTimeFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldTimeFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldVec2fFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (B3DVector2Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldVec2fFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldVec2fFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser methodsFor: 'multi field parsing' stamp: 'ar 11/5/2000 17:19'!readMultiFieldVec3fFrom: aVRMLStream	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: (B3DVector3Array new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self readSingleFieldVec3fFrom: aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self readSingleFieldVec3fFrom: aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.! !!VRMLNodeParser class methodsFor: 'private' stamp: 'ar 11/5/2000 17:18'!compileMultiFieldMethod: selString single: singleString type: mfFieldType	| source |	source := String streamContents:[:s|		s nextPutAll: selString.		s nextPutAll:' aVRMLStream'.		s nextPutAll:(('	"This method was automatically generated"	| fields nFields |	fields := WriteStream on: ($MF_FIELD_TYPE$ new: 100).	aVRMLStream skipSeparators.	aVRMLStream backup.	(aVRMLStream nextChar = $[) ifFalse:[		aVRMLStream restore.		fields nextPut: (self $READSINGLE$ aVRMLStream).		^fields contents].	aVRMLStream discard.	nFields _ 0.	[aVRMLStream skipSeparators.	aVRMLStream peekChar = $] ] whileFalse:[		(nFields bitAnd: 255) = 0 ifTrue:[self progressUpdate: aVRMLStream].		fields nextPut: (self $READSINGLE$ aVRMLStream).		nFields _ nFields + 1.	].	aVRMLStream nextChar.	^fields contents.' 		copyReplaceAll:'$READSINGLE$' with: singleString)		copyReplaceAll:'$MF_FIELD_TYPE$' with: mfFieldType).	].	self compile: source classified:'multi field parsing'! !VRMLNodeParser class removeSelector: #compileMultiFieldMethod:single:!VRMLNodeParser removeSelector: #progressUpdate!