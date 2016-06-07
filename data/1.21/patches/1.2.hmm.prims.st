Date:	97 Sep 05 9:22:33 am
From:	Hans-Martin Mosner <hmm@heeg.de>
To:		Squeak Mailing List <squeak@create.ucsb.edu>
Subject:	primitives (again) and 2 fixes

This is a multi-part message in MIME format.

--------------446B9B3D794BDF3215FB7483
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit

Hello,
in the last few days I have been working on better Smalltalk->C
translation for primitives. My current code (attached to this mail) can
create primitives that work with Ian Piumarta's VMs, and the following
additional properties:
1. FloatArrays can be used in primitives (C type: double *).
2. 'self' can be used and declared in primitives.
   I used these features to implement the class FloatArray with
   subclasses for homogeneous coordinates and transformation matrices.
3. A set of translated primitives can include a dispatch function which
   can be used to connect a variable set of primitives to the VM
   without needing to re-translating and recompiling the VM proper.
   I have not yet used this feature, but it should save some compile
   time on the old and slow machines I'm using.
   I would like to suggest that this mechanism be used for the sound
   primitives, too, and for other subsystems that might need converted
   primitives.

In addition, I've attached 2 bug fixes for the Form editor / Bit editor.
To support the float array accesses, two functions/macros floatAt() and
floatAtput() must be defined in sq.h. Their definition is as follows.

Hans-Martin

#if defined(DOUBLE_WORD_ALIGNMENT) || defined(DOUBLE_WORD_ORDER)
#  ifdef DOUBLE_WORD_ORDER
/* word-based copy with swapping for non-PowerPC order */
#    define storeFloatAtfrom(i, floatVarName) \
	*((int *) (i) + 0) = *((int *) &(floatVarName) + 1); \
	*((int *) (i) + 1) = *((int *) &(floatVarName) + 0);
#    define fetchFloatAtinto(i, floatVarName) \
	*((int *) &(floatVarName) + 0) = *((int *) (i) + 1); \
	*((int *) &(floatVarName) + 1) = *((int *) (i) + 0);
#  else /*!DOUBLE_WORD_ORDER*/
/* word-based copy for machines with alignment restrictions */
#    define storeFloatAtfrom(i, floatVarName) \
	*((int *) (i) + 0) = *((int *) &(floatVarName) + 0); \
	*((int *) (i) + 1) = *((int *) &(floatVarName) + 1);
#    define fetchFloatAtinto(i, floatVarName) \
	*((int *) &(floatVarName) + 0) = *((int *) (i) + 0); \
	*((int *) &(floatVarName) + 1) = *((int *) (i) + 1);
#  endif /*!DOUBLE_WORD_ORDER*/
static double floatAt(double * floatArray, int i)
{
	double tmpFloat;
	int address = (int) floatArray + (i-1)*8;
	fetchFloatAtinto(address, tmpFloat);
	return tmpFloat;
}
static void floatAtput(double * floatArray, int i, double value)
{
	int address = (int) floatArray + (i-1)*8;
	storeFloatAtfrom(address, value);
}
#else /*!(DOUBLE_WORD_ORDER||DOUBLE_WORD_ALIGNMENT)*/
/* for machines that allow doubles to be on any word boundary */
#  define storeFloatAtfrom(i, floatVarName) \
	*((double *) (i)) = (floatVarName);
#  define fetchFloatAtinto(i, floatVarName) \
	(floatVarName) = *((double *) (i));
#define floatAt(floatArray, i) \
	(((double *)(floatArray))[(i)-1])
#define floatAtput(floatArray, i, value) \
	((double *)(floatArray))[(i)-1] = (value)
#endif

--------------446B9B3D794BDF3215FB7483
Content-Type: text/plain; charset=iso-8859-1; name="FPTranslation.cs"
Content-Transfer-Encoding: quoted-printable
Content-Disposition: inline; filename="FPTranslation.cs"

'From Squeak 1.21 of July 17, 1997 on 5 September 1997 at 4:21:57 pm'!
"Change Set:		FPTranslation
Date:			5 September 1997
Author:			Hans-Martin Mosner <hmm@heeg.de>

This change set modifies the primitive translation mechanism used for sub=
systems (such as the sound primitives) to support 2 more features:
1. Arrays of doubles can be declared. This is only useful in combination =
with the FloatArray class.
2. The type of 'self' can be declared, and accesses are modified accordin=
gly. For example, Matrix transformations can be written where the receive=
r is an instance of a subclass of FloatArray.

To be done: support for primitive failing within the body of the primitiv=
e, and support for returning values other than self from the primitive."!=



!CCodeGenerator reorganize!
('public' addClass: codeString codeStringForPrimitives: codeStringForPrim=
itives:dispatchFunction: globalsAsSet initialize storeCodeOnFile:doInlini=
ng: storeCodeOnFile:doInlining:doAssertions: var:declareC:)
('error notification' checkClassForNameConflicts: printUnboundCallWarning=
s printUnboundVariableReferenceWarnings)
('inlining' collectInlineList doInlining inlineDispatchesInMethodNamed:lo=
calizingVars: mayInline: methodStatsString removeAssertions removeMethods=
ReferingToGlobals:except:)
('utilities' addMethod: builtin: cCodeForMethod: emitBuiltinConstructFor:=
on:level: methodNamed: methodsReferringToGlobal: methodsThatCanInvoke: pr=
epareMethods reportRecursiveMethods unreachableMethods)
('C code generator' cFunctionNameFor: cLiteralFor: emitCCodeOn:doInlining=
:doAssertions: emitCExpression:on: emitCFunctionPrototypesOn: emitCHeader=
ForPrimitivesOn: emitCHeaderOn: emitCTestBlock:on: emitCVariablesOn: emit=
PrimitiveDispatch:prims:on:)
('C translation' generateAnd:on:indent: generateAt:on:indent: generateAtP=
ut:on:indent: generateBitAnd:on:indent: generateBitInvert32:on:indent: ge=
nerateBitOr:on:indent: generateBitShift:on:indent: generateBitXor:on:inde=
nt: generateCCoercion:on:indent: generateDivide:on:indent: generateEqual:=
on:indent: generateGreaterThan:on:indent: generateGreaterThanOrEqual:on:i=
ndent: generateIfFalse:on:indent: generateIfFalseIfTrue:on:indent: genera=
teIfTrue:on:indent: generateIfTrueIfFalse:on:indent: generateInlineCCode:=
on:indent: generateInlineDirective:on:indent: generateIntegerObjectOf:on:=
indent: generateIntegerValueOf:on:indent: generateIsIntegerObject:on:inde=
nt: generateIsNil:on:indent: generateLessThan:on:indent: generateLessThan=
OrEqual:on:indent: generateMax:on:indent: generateMin:on:indent: generate=
Minus:on:indent: generateModulo:on:indent: generateNot:on:indent: generat=
eNotEqual:on:indent: generateNotNil:on:indent: generateOr:on:indent: gene=
ratePlus:on:indent: generatePreDecrement:on:indent: generatePreIncrement:=
on:indent: generateSequentialAnd:on:indent: generateSequentialOr:on:inden=
t: generateSharedCodeDirective:on:indent: generateShiftLeft:on:indent: ge=
nerateShiftRight:on:indent: generateTimes:on:indent: generateToByDo:on:in=
dent: generateToDo:on:indent: generateWhileFalse:on:indent: generateWhile=
True:on:indent: initializeCTranslationDictionary)
!


!CCodeGenerator methodsFor: 'public' stamp: 'hmm 9/5/97 15:59'!
codeStringForPrimitives: classAndSelectorList dispatchFunction: dispatchF=
unctionName
	"CCodeGenerator new codeStringForPrimitives: #(
		(FMSound mixSampleCount:into:startingAt:)
	)"

	| sel aClass source s verbose meth switch prim |
	self initialize.
	switch _ Dictionary new.
	classAndSelectorList do: [ :classAndSelector |
		aClass _ Smalltalk at: (classAndSelector at: 1).
		sel _ classAndSelector at: 2.
		source _ aClass sourceCodeAt: sel.
		meth _ (Compiler new parse: source in: aClass notifying: nil).
		prim _ meth primitive.
		meth _ meth asTMethodFromClass: aClass.
		meth preparePrimitiveInClass: aClass.
		self addMethod: meth.
		prim =3D 0 ifFalse: [switch at: prim put: meth].
	].
	s _ ReadWriteStream on: (String new: 1000).

	"method preparation"
	verbose _ false.
	self prepareMethods.
	verbose ifTrue: [
		self printUnboundCallWarnings.
		self printUnboundVariableReferenceWarnings.
		Transcript cr.
	].

	"code generation"
	methods _ methods asSortedCollection: [ :m1 :m2 | m1 selector < m2 selec=
tor ].
	self emitCHeaderForPrimitivesOn: s.
	self emitCVariablesOn: s.
	self emitCFunctionPrototypesOn: s.
	self emitPrimitiveDispatch: dispatchFunctionName prims: switch on: s.
	methods do: [ :m | m emitCCodeOn: s generator: self ].
	^ s contents! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'hmm 9/5/97 16:11'!=

emitCHeaderForPrimitivesOn: aStream
	"Write a C file header for compiled primitives onto the given stream."

	aStream nextPutAll: '/* Automatically generated from Squeak on '.
	aStream nextPutAll: Time dateAndTimeNow printString.
	aStream nextPutAll: ' */'; cr; cr.

	aStream nextPutAll: '#include "sq.h"'; cr; cr.

	aStream nextPutAll: '
/* Memory Access Macros */
#define byteAt(i) (*((unsigned char *) (i)))
#define byteAtput(i, val) (*((unsigned char *) (i)) =3D val)
#define longAt(i) (*((int *) (i)))
#define longAtput(i, val) (*((int *) (i)) =3D val)

/*** Imported Variables ***/
extern int stackPointer(void);
extern int primitiveIndex;
extern int successFlag;
'.
	aStream cr.! !

!CCodeGenerator methodsFor: 'C code generator' stamp: 'hmm 9/5/97 16:19'!=

emitPrimitiveDispatch: aString prims: aDictionary on: aStream
	"create a dispatch function for the primitives in aDictionary"
	aStream cr.
	aStream nextPutAll: 'int '; nextPutAll: aString; nextPutAll: '(void)'; c=
r.
	aStream nextPut: ${; cr.
	aStream tab; nextPutAll: 'switch (primitiveIndex) {'; cr.
	aDictionary keys asSortedCollection do: [:prim |
		aStream tab; nextPutAll: 'case '; print: prim; nextPutAll: ': '.
		aStream nextPutAll: (self cFunctionNameFor: (aDictionary at: prim) sele=
ctor); nextPutAll: '(); break;'; cr].
	aStream tab; nextPutAll: 'default: success(false);'; cr.
	aStream tab; nextPut: $}; cr.
	aStream nextPut: $}; cr! !

!CCodeGenerator methodsFor: 'C translation' stamp: 'hmm 9/5/97 09:20'!
initializeCTranslationDictionary =

	"Initialize the dictionary mapping message names to actions for C code g=
eneration."
	"Modified by HMM to translate the / operator, too (non-truncating divisi=
on needed for matrix primitives)"

	| pairs |
	translationDict _ Dictionary new: 200.
	pairs _ #(
	#&				#generateAnd:on:indent:
	#|				#generateOr:on:indent:
	#and:			#generateSequentialAnd:on:indent:
	#or:			#generateSequentialOr:on:indent:
	#not			#generateNot:on:indent:

	#+				#generatePlus:on:indent:
	#-				#generateMinus:on:indent:
	#*				#generateTimes:on:indent:
	#//				#generateDivide:on:indent:
	#/				#generateDivide:on:indent:	"added for float division, use with car=
e"
	#\\				#generateModulo:on:indent:
	#<<				#generateShiftLeft:on:indent:
	#>>				#generateShiftRight:on:indent:
	#min:			#generateMin:on:indent:
	#max:			#generateMax:on:indent:

	#bitAnd:		#generateBitAnd:on:indent:
	#bitOr:			#generateBitOr:on:indent:
	#bitXor:			#generateBitXor:on:indent:
	#bitShift:		#generateBitShift:on:indent:
	#bitInvert32	#generateBitInvert32:on:indent:

	#<				#generateLessThan:on:indent:
	#<=3D				#generateLessThanOrEqual:on:indent:
	#=3D				#generateEqual:on:indent:
	#>				#generateGreaterThan:on:indent:
	#>=3D				#generateGreaterThanOrEqual:on:indent:
	#~=3D				#generateNotEqual:on:indent:
	#=3D=3D				#generateEqual:on:indent:
	#isNil			#generateIsNil:on:indent:
	#notNil			#generateNotNil:on:indent:

	#whileTrue: 	#generateWhileTrue:on:indent:
	#whileFalse:	#generateWhileFalse:on:indent:
	#to:do:			#generateToDo:on:indent:
	#to:by:do:		#generateToByDo:on:indent:

	#ifTrue:		#generateIfTrue:on:indent:
	#ifFalse:		#generateIfFalse:on:indent:
	#ifTrue:ifFalse:	#generateIfTrueIfFalse:on:indent:
	#ifFalse:ifTrue:	#generateIfFalseIfTrue:on:indent:

	#at:				#generateAt:on:indent:
	#at:put:			#generateAtPut:on:indent:

	#integerValueOf:	#generateIntegerValueOf:on:indent:
	#integerObjectOf:	#generateIntegerObjectOf:on:indent:
	#isIntegerObject: 	#generateIsIntegerObject:on:indent:
	#cCode:				#generateInlineCCode:on:indent:
	#cCoerce:to:			#generateCCoercion:on:indent:
	#preIncrement		#generatePreIncrement:on:indent:
	#preDecrement		#generatePreDecrement:on:indent:
	#inline:				#generateInlineDirective:on:indent:
	#sharedCodeNamed:inCase:	#generateSharedCodeDirective:on:indent:
	).

	1 to: pairs size by: 2 do: [ :i |
		translationDict at: (pairs at: i) put: (pairs at: i + 1).
	].! !


!MethodNode methodsFor: 'C translation' stamp: 'hmm 9/5/97 16:09'!
primitive
	"return the primitive number"
	^primitive! !


!TMethod methodsFor: 'primitive compilation' stamp: 'hmm 9/5/97 16:17'!
argConversionExprFor: varName stackIndex: stackIndex
	"Return the parse tree for an expression that fetches and converts the p=
rimitive argument at the given stack offset."
	"HMM: translate 'double *' into the correct type. Note: accesses to Arra=
y of doubles must be done using #floatAt: and #floatAt:put: since the nor=
mal #at: and #at:put: generate ordinary array references which are not co=
mpatible with Squeak's way of storing floats."

	| expr decl |
	expr _ stackIndex =3D args size
			ifTrue: ['rcvr']
			ifFalse: ['(self longAt: self stackPointer - ( ', stackIndex printStri=
ng, ' * 4))'].
	(declarations includesKey: varName) ifTrue: [  "array"
		decl _ declarations at: varName.
		(decl includes: $*) ifTrue: [
			expr _ varName, ' _ self arrayValueOf: ', expr.
		] ifFalse: [  "must be a double"
			((decl findString: 'double' startingAt: 1) =3D 0)
				ifTrue: [ self error: 'unsupported type declaration in a primitive me=
thod' ].
			expr _ varName, ' _ self floatValueOf: ', expr.
		].
	] ifFalse: [  "undeclared variables are taken to be integer"
		expr _ varName, ' _ self checkedIntegerValueOf: ', expr.
	].
	^ self statementsFor: expr varName: varName
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'hmm 9/5/97 09:22'!
convertReferencesToSelf
	"Replace references to self by the variable name 'mySelf' so they are no=
t lost in translation."

	parseTree nodesDo: [ :n |
		(n isVariable and: [n name =3D 'self'])
			ifTrue: [n setName: 'mySelf'].
	].
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'hmm 9/5/97 09:21'!
convertToZeroBasedArrayReferences
	"Replace the index expressions in at: and at:put: messages with (<expr> =
- 1), since C uses zero-based array indexing."
	"HMM: renamed the method (was #covert...)"

	| oldIndexExpr newIndexExpr |
	parseTree nodesDo: [ :n |
		(n isSend and: [(n selector =3D #at:) or: [ n selector =3D #at:put: ]])=
 ifTrue: [
			oldIndexExpr _ n args first.
			oldIndexExpr isConstant ifTrue: [
				"index expression is a constant: decrement the constant now"
				newIndexExpr _ TConstantNode new setValue: (n args first value - 1).
			] ifFalse: [
				"index expression is complex: build an expression to decrement result=
 at runtime"
				newIndexExpr _ TSendNode new
					setSelector: #-
					receiver: oldIndexExpr
					arguments: (Array with: (TConstantNode new setValue: 1)).
			].
			n args at: 1 put: newIndexExpr.
		].
	].
! !

!TMethod methodsFor: 'primitive compilation' stamp: 'hmm 9/5/97 16:18'!
fetchRcvrExpr
	"Return the parse tree for an expression that fetches the receiver from =
the stack."
	"HMM: Modified to take declarations into account."

	| expr stmts |
	expr _ 'rcvr _ self longAt: self stackPointer - (', args size printStrin=
g, ' * 4)'.
	stmts _ self statementsFor: expr varName: ''.
	(declarations includesKey: 'self') ifTrue: [
		"self is declared as having a C type"
		"rename to 'mySelf' to avoid problems with send manipulations."
		locals addFirst: 'mySelf'.
		declarations at: 'mySelf' put:
			((declarations removeKey: 'self') copyReplaceAll: 'self' with: 'mySelf=
').
		stmts addAll: (self argConversionExprFor: 'mySelf' stackIndex: args siz=
e)].
	^stmts! !

!TMethod methodsFor: 'primitive compilation' stamp: 'hmm 9/5/97 09:22'!
preparePrimitiveInClass: aClass
	"HMM: modified to convert accesses to 'self'"
	"Add a prolog and postlog to a primitive method. The prolog copies any i=
nstance variables referenced by this primitive method into local variable=
s. The postlog copies values of assigned-to variables back into the insta=
nce. The names of the new locals are added to the local variables list.

The declarations dictionary defines the types of any non-integer variable=
s (locals, arguments, or instance variables). In particular, it may speci=
fy the types:

	int *		-- an array of 32-bit values (e.g., a BitMap)
	short *		-- an array of 16-bit values (e.g., a SoundBuffer)
	char *		-- an array of unsigned bytes (e.g., a String)
	double		-- a double precision floating point number (e.g., 3.14159)
	double *	-- an array of doubles

Undeclared variables are taken to be integers and will be converted from =
Smalltalk to C ints."

"Current restrictions:
	o method must not contain explicit returns
	o method must not contain message sends
	o method must not allocate objects
	o method must not manipulate raw oops
	o method cannot access class variables
	o compiled primitives can only return self"

	| prolog postlog instVarsUsed varsAssignedTo instVarList varName |
	prolog _ OrderedCollection new.
	postlog _ OrderedCollection new.
	instVarsUsed _ self freeVariableReferences asSet.
	varsAssignedTo _ self variablesAssignedTo asSet.
	instVarList _ aClass allInstVarNames.

	"convert references to 'self'"
	self convertReferencesToSelf.

	"add receiver fetch to prolog"
	prolog addAll: self fetchRcvrExpr.

	"add arg conversions to prolog"
	1 to: args size do: [ :argIndex |
		varName _ args at: argIndex.
		prolog addAll:
			(self argConversionExprFor: varName stackIndex: args size - argIndex).=

	].

	"add instance variable fetches to prolog and instance variable stores to=
 postlog"
	1 to: instVarList size do: [ :varIndex |
		varName _ instVarList at: varIndex.
		(instVarsUsed includes: varName) ifTrue: [
			locals add: varName.
			prolog addAll: (self instVarGetExprFor: varName offset: varIndex - 1).=

			(varsAssignedTo includes: varName) ifTrue: [
				postlog addAll: (self instVarPutExprFor: varName offset: varIndex - 1=
).
			].
		].
	].

	prolog addAll: self checkSuccessExpr.
	postlog addAll: self popArgsExpr.

	locals addAllFirst: args.
	locals addFirst: 'rcvr'.
	args _ args class new.
	locals asSet size =3D locals size
		ifFalse: [ self error: 'local name conflicts with instance variable nam=
e' ].
	self hasReturn
		ifTrue: [ self error: 'returns in primitive methods are not yet support=
ed' ].

	selector _ 'prim', aClass name, selector.
	parseTree setStatements: prolog, parseTree statements, postlog.
	self convertToZeroBasedArrayReferences.
! !


TMethod removeSelector: #covertToZeroBasedArrayReferences!

--------------446B9B3D794BDF3215FB7483
Content-Type: text/plain; charset=iso-8859-1; name="Sq3d-Transformations.st"
Content-Transfer-Encoding: quoted-printable
Content-Disposition: inline; filename="Sq3d-Transformations.st"

ArrayedCollection variableWordSubclass: #FloatArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sq3d-Transformations'!
FloatArray comment:
'Instances of FloatArray can hold Float values only (8-byte doubles in C)=
=2E They are intended to be used by primitives, and their basic access me=
thods are fairly inefficient.'!

!FloatArray methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:08'!
at: index
	"Create a new float to hold the result."
	| i2 result |
	i2 _ index*2-1.
	result _ 0.0 copy.
	result basicAt: 1 put: (self basicAt: i2).
	result basicAt: 2 put: (self basicAt: i2+1).
	^result! !

!FloatArray methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:21'!
at: index put: aNumber
	"Create a new float to hold the result."
	| i2 f |
	i2 _ index*2-1.
	f _ aNumber asFloat.
	self basicAt: i2 put: (f basicAt: 1).
	self basicAt: i2+1 put: (f basicAt: 2).
	^aNumber! !

!FloatArray methodsFor: 'accessing' stamp: 'hmm 9/5/97 09:28'!
floatAt: index
	"Create a new float to hold the result."
	"The method is provided under this additional name to support primitives=
=2E
	See comment in TMethod>>argConversionExprFor:stackIndex:"

	| i2 result |
	i2 _ index*2-1.
	result _ 0.0 copy.
	result basicAt: 1 put: (self basicAt: i2).
	result basicAt: 2 put: (self basicAt: i2+1).
	^result! !

!FloatArray methodsFor: 'accessing' stamp: 'hmm 9/5/97 09:29'!
floatAt: index put: aNumber
	"Create a new float to hold the result."
	"The method is provided under this additional name to support primitives=
=2E
	See comment in TMethod>>argConversionExprFor:stackIndex:"

	| i2 f |
	i2 _ index*2-1.
	f _ aNumber asFloat.
	self basicAt: i2 put: (f basicAt: 1).
	self basicAt: i2+1 put: (f basicAt: 2).
	^aNumber! !

!FloatArray methodsFor: 'accessing' stamp: 'hmm 9/5/97 09:29'!
size
	"For each Float, the receiver has 2 words."

	^super size//2! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

FloatArray class
	instanceVariableNames: ''!

!FloatArray class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:=
06'!
new: size
	^super new: size*2! !

=0C
FloatArray variableWordSubclass: #Sq3dPoint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sq3d-Transformations'!
Sq3dPoint comment:
'Instances of this class represent homogeneous points with x, y, z and w =
coordinates,
Homogeneous points are a convenient way to describe perspective transform=
ations in terms of ordinary matrix operations.'!

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
w
	^self at: 4! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
w: aNumber
	^self at: 4 put: aNumber! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:13'!
x
	^self at: 1! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
x: aNumber
	^self at: 1 put: aNumber! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:13'!
y
	^self at: 2! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
y: aNumber
	^self at: 2 put: aNumber! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
z
	^self at: 3! !

!Sq3dPoint methodsFor: 'accessing' stamp: 'hmm 7/27/97 14:14'!
z: aNumber
	^self at: 3 put: aNumber! !


!Sq3dPoint methodsFor: 'arithmetic' stamp: 'hmm 7/27/97 14:17'!
+ aSq3dPoint
	^self class
		x: self x + aSq3dPoint x
		y: self y + aSq3dPoint y
		z: self z + aSq3dPoint z! !


!Sq3dPoint methodsFor: 'testing' stamp: 'hmm 8/4/97 23:06'!
isInViewingVolume
	| x y z |
	self divideByW.
	x _ self x.
	x < -1.0 ifTrue: [^false].
	x > 1.0 ifTrue: [^false].
	y _ self y.
	y < -1.0 ifTrue: [^false].
	y > 1.0 ifTrue: [^false].
	z _ self z.
	z < -1.0 ifTrue: [^false].
	z > 1.0 ifTrue: [^false].
	^true! !

!Sq3dPoint methodsFor: 'testing' stamp: 'hmm 8/4/97 20:26'!
isPoint
	^true! !

!Sq3dPoint methodsFor: 'testing' stamp: 'hmm 9/4/97 20:22'!
isSq3dPoint
	^true! !


!Sq3dPoint methodsFor: 'point functions' stamp: 'hmm 7/27/97 14:15'!
r
	^(self x squared + self y squared + self z squared) sqrt! !


!Sq3dPoint methodsFor: 'converting' stamp: 'hmm 7/27/97 14:40'!
asSq3dPoint
	^self! !

!Sq3dPoint methodsFor: 'converting' stamp: 'hmm 9/5/97 11:07'!
divideByW
	"CCodeGenerator new codeStringForPrimitives: #(
		(Sq3dPoint divideByW)
	)."
	| w |
	<primitive: 233>
	self var: #self declareC: 'double * self'.
	self var: #w declareC: 'double w'.
	w _ self floatAt: 4.
	w =3D 1.0 ifFalse: [
		w =3D 0.0 ifFalse: [
			self floatAt: 1 put: (self floatAt: 1) / w.
			self floatAt: 2 put: (self floatAt: 2) / w.
			self floatAt: 3 put: (self floatAt: 3) / w.
			self floatAt: 4 put: 1.0]]! !

!Sq3dPoint methodsFor: 'converting' stamp: 'hmm 9/5/97 09:34'!
normalize
	"normalize this point, utilizing the divideByW primitive"
	self w: self r.
	self divideByW! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Sq3dPoint class
	instanceVariableNames: ''!

!Sq3dPoint class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:3=
3'!
new
	^super new: 4! !

!Sq3dPoint class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:1=
2'!
new: size
	^self error: 'Sq3dPoints always have 4 elements.'! !

!Sq3dPoint class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:4=
1'!
x: x y: y
	| result |
	result _ self new.
	result x: x; y: y; w: 1.0.
	^result! !

!Sq3dPoint class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:3=
3'!
x: x y: y z: z
	| result |
	result _ self new.
	result x: x; y: y; z: z; w: 1.0.
	^result! !

!Sq3dPoint class methodsFor: 'instance creation' stamp: 'hmm 7/27/97 14:1=
9'!
x: x y: y z: z w: w
	"Sometimes points need to be created with a W value unequal 1, for examp=
le to specify directional lights."
	| result |
	result _ self new.
	result x: x; y: y; z: z; w: w.
	^result! !

=0C
FloatArray variableWordSubclass: #Sq3dTransformationMatrix
	instanceVariableNames: ''
	classVariableNames: 'Identity '
	poolDictionaries: ''
	category: 'Sq3d-Transformations'!
Sq3dTransformationMatrix comment:
'Sq3dTransformationMatrixes alway carry their inverse with them, to facil=
itate lighting and other inverse transformations. Whenever standard opera=
tions are used, such as translation, scaling or rotation, the inverse ope=
ration can easily be applied to the inverse, so that explicit computation=
 ofthe inverse is avoided. '!

!Sq3dTransformationMatrix methodsFor: 'printing' stamp: 'hmm 9/4/97 21:02=
'!
printOn: aStream
	aStream nextPut: $(.
	1 to: 16 do: [:i |
		i \\ 4 =3D 1 ifTrue: [aStream cr] ifFalse: [aStream tab].
		(self at: i) printOn: aStream.
	].
	aStream nextPut: $)! !


!Sq3dTransformationMatrix methodsFor: 'accessing' stamp: 'hmm 9/4/97 21:0=
2'!
atRow: row column: column
	^self at: row-1*4+column! !

!Sq3dTransformationMatrix methodsFor: 'accessing' stamp: 'hmm 9/4/97 21:0=
2'!
atRow: row column: column put: aValue
	^self at: row-1*4+column put: aValue! !


!Sq3dTransformationMatrix methodsFor: 'arithmetic' stamp: 'hmm 9/4/97 21:=
39'!
* aNumberOrMatrixOrPoint
	^aNumberOrMatrixOrPoint class =3D=3D self class
		ifTrue: [self matrixMult: aNumberOrMatrixOrPoint]
		ifFalse: [aNumberOrMatrixOrPoint isPoint
					ifTrue: [self transform: aNumberOrMatrixOrPoint]
					ifFalse: [self scaledBy: aNumberOrMatrixOrPoint]]! !

!Sq3dTransformationMatrix methodsFor: 'arithmetic' stamp: 'hmm 9/4/97 21:=
39'!
matrixMult: aMatrix
	"multiply by aMatrix, which must have the same class as myself."
	| result |
	result _ self class new.
	self matrixMult: aMatrix into: result.
	^result! !

!Sq3dTransformationMatrix methodsFor: 'arithmetic' stamp: 'hmm 9/4/97 21:=
13'!
scaledBy: aNumber
	| result x y z |
	result :=3D self clone.
	aNumber isPoint
		ifTrue: [
			x _ aNumber x.
			1 to: 4 do: [:i | result at: i put: (self at: i) * x].
			y _ aNumber y.
			5 to: 8 do: [:i | result at: i put: (self at: i) * y].
			z _ aNumber z.
			9 to: 12 do: [:i | result at: i put: (self at: i) * z].
		] ifFalse: [
			1 to: 12 do: [:i | result at: i put: (self at: i) * aNumber]].
	^result! !

!Sq3dTransformationMatrix methodsFor: 'arithmetic' stamp: 'hmm 9/4/97 21:=
13'!
translatedBy: aPoint
	| result |
	result :=3D self clone.
	result at: 4 put: (result at: 4) + aPoint x.
	result at: 8 put: (result at: 8) + aPoint y.
	result at: 12 put: (result at: 12) + aPoint z.
	^result! !


!Sq3dTransformationMatrix methodsFor: 'transforming' stamp: 'hmm 9/5/97 0=
9:12'!
transform: aPoint
	| p |
	p _ aPoint clone.	"preserve attributes beyond the 4 coordinates"
	self transform: aPoint into: p.
	^p! !


!Sq3dTransformationMatrix methodsFor: 'testing' stamp: 'hmm 7/31/97 21:12=
'!
isMatrix
	^true! !


!Sq3dTransformationMatrix methodsFor: 'primitives' stamp: 'hmm 9/5/97 11:=
06'!
matrixMult: aSq3dFloatArray into: resultSq3dFloatArray
	"CCodeGenerator new codeStringForPrimitives: #(
		(Sq3dTransformationMatrix matrixMult:into:)
	)."
	| f1 f2 sum |
	<primitive: 231>
	self var: #self declareC: 'double *self'.
	self var: #values declareC: 'double *values'.
	self var: #aSq3dFloatArray declareC: 'double *aSq3dFloatArray'.
	self var: #resultSq3dFloatArray declareC: 'double *resultSq3dFloatArray'=
=2E
	self var: #f1 declareC: 'double f1'.
	self var: #f2 declareC: 'double f2'.
	self var: #sum declareC: 'double sum'.
	1 to: 4 do: [:row |
		1 to: 4 do: [:column |
			f1 _ self floatAt: row-1 * 4 + 1.
			f2 _ aSq3dFloatArray floatAt: 1-1 * 4 + column.
			sum _ f1 * f2.
			f1 _ self floatAt: row-1 * 4 + 2.
			f2 _ aSq3dFloatArray floatAt: 2-1 * 4 + column.
			sum _ sum + (f1 * f2).
			f1 _ self floatAt: row-1 * 4 + 3.
			f2 _ aSq3dFloatArray floatAt: 3-1 * 4 + column.
			sum _ sum + (f1 * f2).
			f1 _ self floatAt: row-1 * 4 + 4.
			f2 _ aSq3dFloatArray floatAt: 4-1 * 4 + column.
			sum _ sum + (f1 * f2).
			resultSq3dFloatArray
				floatAt: row-1 * 4 + column
				put: sum]]! !

!Sq3dTransformationMatrix methodsFor: 'primitives' stamp: 'hmm 9/5/97 16:=
31'!
transform: aPoint into: transformedPoint
	"CCodeGenerator new codeStringForPrimitives: #(
		(Sq3dTransformationMatrix transform:into:)
	)."
	| x y z w i4 |
	<primitive: 232>
	self var: #self declareC: 'double *self'.
	self var: #aPoint declareC: 'double *aPoint'.
	self var: #transformedPoint declareC: 'double *transformedPoint'.
	self var: #x declareC: 'double x'.
	self var: #y declareC: 'double y'.
	self var: #z declareC: 'double z'.
	self var: #w declareC: 'double w'.
	x _ aPoint floatAt: 1.
	y _ aPoint floatAt: 2.
	z _ aPoint floatAt: 3.
	w _ aPoint floatAt: 4.
	1 to: 4 do: [:i |
		i4 _ i*4.
		transformedPoint floatAt: i put:
			(x * (self floatAt: i4-3)) +
			(y * (self floatAt: i4-2)) +
			(z * (self floatAt: i4-1)) +
			(w * (self floatAt: i4))]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Sq3dTransformationMatrix class
	instanceVariableNames: ''!

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 9/4/97 21:45'!
frustumLeft: left right: right bottom: bottom top: top near: near far: fa=
r
	"Sq3dTransformationMatrix
		frustumLeft: -1 right: 1
		bottom: -1 top: 1
		near: 0.5 far: 10"
	| matrix |
	matrix _ self new.
	matrix at: 1 put: 2*near / (right-left).
	matrix at: 3 put: (right+left) / (right-left).
	matrix at: 6 put: 2*near / (top-bottom).
	matrix at: 7 put: (top+bottom) / (top-bottom).
	matrix at: 11 put: (0-far-near) / (far-near).
	matrix at: 12 put: (-2*far*near) / (far-near).
	matrix at: 15 put: -1.
	^matrix! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 9/4/97 21:43'!
identity
	| result |
	result _ self new.
	1 to: 16 by: 5 do: [:i | result at: i put: 1].
	^result! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 9/4/97 21:44'!
new
	^super new: 32! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 7/27/97 14:28'!
new: aNumber
	^self error: 'Matrixes are created with new.'! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 9/4/97 21:45'!
toRotateBy: angle around: axisPoint
	| rad cos sin result sign |
	rad _ angle degreesToRadians.
	cos _ rad cos.
	sin _ rad sin.
	result _ self new.
	result at: 16 put: 1.
	axisPoint x =3D 0.0 ifTrue: [
		axisPoint y =3D 0.0 ifTrue: [
			sign _ axisPoint z sign.
			result atRow: 3 column: 3 put: 1.
			result atRow: 1 column: 1 put: cos * sign.
			result atRow: 1 column: 2 put: sin * sign negated.
			result atRow: 2 column: 1 put: sin * sign.
			result atRow: 2 column: 2 put: cos * sign.
			^result].
		axisPoint z =3D 0.0 ifTrue: [
			sign _ axisPoint y sign.
			result atRow: 2 column: 2 put: 1.
			result atRow: 1 column: 1 put: cos * sign.
			result atRow: 3 column: 1 put: sin * sign negated.
			result atRow: 1 column: 3 put: sin * sign.
			result atRow: 3 column: 3 put: cos * sign.
			^result]
	]
	ifFalse: [
		(axisPoint y =3D 0.0 and: [axisPoint z =3D 0.0]) ifTrue: [
			sign _ axisPoint x sign.
			result atRow: 1 column: 1 put: 1.
			result atRow: 2 column: 2 put: cos * sign.
			result atRow: 2 column: 3 put: sin * sign negated.
			result atRow: 3 column: 2 put: sin * sign.
			result atRow: 3 column: 3 put: cos * sign.
			^result]
	].
	self error: 'rotation around arbitrary axes not yet implemented.'! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 7/31/97 21:16'!
toScaleBy: aNumber
	^self identity scaledBy: aNumber! !

!Sq3dTransformationMatrix class methodsFor: 'instance creation' stamp: 'h=
mm 7/31/97 21:16'!
toTranslateBy: aSq3dPoint
	^self identity translatedBy: aSq3dPoint! !


!Sq3dTransformationMatrix class methodsFor: 'class initialization' stamp:=
 'hmm 7/27/97 14:27'!
initialize
	"Sq3dTransformationMatrix initialize"
	Identity _ self new.
	1 to: 16 by: 5 do: [:i | Identity at: i put: 1]! !


!Sq3dTransformationMatrix class methodsFor: 'primitive generation' stamp:=
 'hmm 9/5/97 16:17'!
generateCCodeForMatrixPrimitives
	"InterpreterSupportCode storeString: Sq3dTransformationMatrix generateCC=
odeForMatrixPrimitives
							onFileNamed: 'sqMatrixPrims.c'"
	=

	^CCodeGenerator new codeStringForPrimitives: #(
		(Sq3dTransformationMatrix matrixMult:into:)
		(Sq3dTransformationMatrix transform:into:)
		(Sq3dPoint divideByW)
	) dispatchFunction: #dispatchMatrixPrim.
! !


Sq3dTransformationMatrix initialize!

--------------446B9B3D794BDF3215FB7483
Content-Type: text/plain; charset=us-ascii; name="MiscFixes.cs"
Content-Transfer-Encoding: 7bit
Content-Disposition: inline; filename="MiscFixes.cs"

'From Squeak 1.21 of July 17, 1997 on 5 September 1997 at 4:22:14 pm'!

!BitEditor class methodsFor: 'private' stamp: 'hmm 7/21/97 20:42'!
bitEdit: aForm at: magnifiedFormLocation scale: scaleFactor remoteView: remoteView
	"Create a BitEditor on aForm. That is, aForm is a small image that will 
	change as a result of the BitEditor changing a second and magnified 
	view of me. magnifiedFormLocation is where the magnified form is to be 
	located on the screen. scaleFactor is the amount of magnification. This 
	method implements a scheduled view containing both a small and 
	magnified view of aForm. Upon accept, aForm is updated."

	| aFormView scaledFormView bitEditor topView extent menuView lowerRightExtent |
	scaledFormView _ FormHolderView new model: aForm.
	scaledFormView scaleBy: scaleFactor.
	bitEditor _ self new.
	scaledFormView controller: bitEditor.
	bitEditor setColor: Color black.
	topView _ ColorSystemView new.
	remoteView == nil ifTrue: [topView label: 'Bit Editor'].
	topView borderWidth: 2.

	topView addSubView: scaledFormView.
	remoteView == nil
		ifTrue:  "If no remote view, then provide a local view of the form"
			[aFormView _ FormView new model: scaledFormView workingForm.
			aFormView controller: NoController new.
			aForm height < 50
				ifTrue: [aFormView borderWidthLeft: 0 right: 2 top: 2 bottom: 2]
				ifFalse: [aFormView borderWidthLeft: 0 right: 2 top: 2 bottom: 0].
			topView addSubView: aFormView below: scaledFormView]
		 ifFalse:  "Otherwise, the remote one should view the same form"
			[remoteView model: scaledFormView workingForm].
	lowerRightExtent _ remoteView == nil
			ifTrue:
				[(scaledFormView viewport width - aFormView viewport width) @
					(aFormView viewport height max: 50)]
			ifFalse:
				[scaledFormView viewport width @ 50].
	menuView _ self buildColorMenu: lowerRightExtent colorCount: 1.
	menuView model: bitEditor.
	menuView borderWidthLeft: 0 right: 0 top: 2 bottom: 0.
	topView
		addSubView: menuView
		align: menuView viewport topRight
		with: scaledFormView viewport bottomRight.
	extent _ scaledFormView viewport extent + (0 @ lowerRightExtent y)
			+ (4 @ 4).  "+4 for borders"
	topView minimumSize: extent.
	topView maximumSize: extent.
	topView translateBy: magnifiedFormLocation.
	topView insideColor: Color white.
	^topView! !


!FormView methodsFor: 'displaying' stamp: 'hmm 7/21/97 20:45'!
displayView 
	"Refer to the comment in View|displayView."

	| oldOffset |
	super displayView.
	insideColor == nil ifFalse: [Display fill: self insetDisplayBox fillColor: insideColor].
	oldOffset _ model offset.
	model offset: "borderWidth origin" 0@0.
	model
		displayOn: Display
		transformation: self displayTransformation
		clippingBox: self insetDisplayBox
		rule: self rule
		fillColor: self fillColor.
	model offset: oldOffset! !



--------------446B9B3D794BDF3215FB7483--


