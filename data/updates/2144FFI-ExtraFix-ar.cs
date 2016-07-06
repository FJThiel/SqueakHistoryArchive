'From Squeak2.8alpha of 16 February 2000 [latest update: #2139] on 16 May 2000 at 1:45:06 pm'!"Change Set:		FFI-ExtraFix-arDate:			16 May 2000Author:			Andreas RaabThe last fix wasn't quite correct. This one is."!!FFIPlugin methodsFor: 'callout support' stamp: 'ar 5/16/2000 13:39'!ffiCreateReturnOop: retVal	"Callout support. Return the appropriate oop for the given atomic value"	| atomicType shift value mask byteSize |	atomicType _ self atomicTypeOf: ffiRetHeader.	atomicType = FFITypeBool ifTrue:[			"Make sure bool honors the byte size requested"			byteSize _ ffiRetHeader bitAnd: FFIStructSizeMask.			byteSize = 4				ifTrue:[value _ retVal]				ifFalse:[value _ retVal bitAnd: 1 << (byteSize * 8) - 1].			value = 0				ifTrue:[^interpreterProxy falseObject]				ifFalse:[^interpreterProxy trueObject]].	atomicType <= FFITypeSignedInt ifTrue:[		"these are all generall integer returns"		atomicType <= FFITypeSignedShort ifTrue:[			"byte/short. first extract partial word, then sign extend"			shift _ (atomicType >> 1) * 8. "# of significant bits"			value _ retVal bitAnd: (1 << shift - 1). 			(atomicType anyMask: 1) ifTrue:[				"make the guy signed"				mask _ 1 << (shift-1).				value _ (value bitAnd: mask-1) - (value bitAnd: mask)].			^interpreterProxy integerObjectOf: value].		"32bit integer return"		(atomicType anyMask: 1)			ifTrue:[^(interpreterProxy signed32BitIntegerFor: retVal)] "signed return"			ifFalse:[^(interpreterProxy positive32BitIntegerFor: retVal)]]. "unsigned return"	atomicType < FFITypeSingleFloat ifTrue:[		"longlong, char"		(atomicType >> 1) = (FFITypeSignedLongLong >> 1) 			ifTrue:[^self ffiCreateLongLongReturn: (atomicType anyMask: 1)]			ifFalse:[^(interpreterProxy 						fetchPointer: (retVal bitAnd: 255)						ofObject: interpreterProxy characterTable)]].	"float return"	^interpreterProxy floatObjectOf: (self ffiReturnFloatValue).! !