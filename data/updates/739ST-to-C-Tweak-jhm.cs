'From Squeak 2.3 of January 14, 1999 on 15 March 1999 at 10:42:33 pm'!"Change Set:		ST-to-C-Tweak-jhmDate:			15 March 1999Author:			John MaloneyAdds the construct:	self cCode: '...C code...' inSmalltalk: <a block>to the Smalltalk-to-C translator and support for it in Object. Thisallows C code and its Smalltalk equivalent to be kept together. Whenthe code is translated, the C code is output, but when it is simulatedin Smalltalk, the block is run. Requested by Dan Ingalls."!!Object methodsFor: 'translation support' stamp: 'jm 2/15/1999 13:11'!cCode: codeString inSmalltalk: aBlock	"Support for Smalltalk-to-C translation. The given string is output literally when generating C code. If this code is being simulated in Smalltalk, answer the result of evaluating the given block."	^ aBlock value! !!CCodeGenerator methodsFor: 'C translation' stamp: 'jm 3/15/1999 22:40'!initializeCTranslationDictionary 	"Initialize the dictionary mapping message names to actions for C code generation."	| pairs |	translationDict _ Dictionary new: 200.	pairs _ #(	#&				#generateAnd:on:indent:	#|				#generateOr:on:indent:	#and:			#generateSequentialAnd:on:indent:	#or:			#generateSequentialOr:on:indent:	#not			#generateNot:on:indent:	#+				#generatePlus:on:indent:	#-				#generateMinus:on:indent:	#*				#generateTimes:on:indent:	#/				#generateDivide:on:indent:	#//				#generateDivide:on:indent:	#\\				#generateModulo:on:indent:	#<<				#generateShiftLeft:on:indent:	#>>				#generateShiftRight:on:indent:	#min:			#generateMin:on:indent:	#max:			#generateMax:on:indent:	#bitAnd:		#generateBitAnd:on:indent:	#bitOr:			#generateBitOr:on:indent:	#bitXor:			#generateBitXor:on:indent:	#bitShift:		#generateBitShift:on:indent:	#bitInvert32	#generateBitInvert32:on:indent:	#<				#generateLessThan:on:indent:	#<=				#generateLessThanOrEqual:on:indent:	#=				#generateEqual:on:indent:	#>				#generateGreaterThan:on:indent:	#>=				#generateGreaterThanOrEqual:on:indent:	#~=				#generateNotEqual:on:indent:	#==				#generateEqual:on:indent:	#~~				#generateNotEqual:on:indent:	#isNil			#generateIsNil:on:indent:	#notNil			#generateNotNil:on:indent:	#whileTrue: 	#generateWhileTrue:on:indent:	#whileFalse:	#generateWhileFalse:on:indent:	#to:do:			#generateToDo:on:indent:	#to:by:do:		#generateToByDo:on:indent:	#ifTrue:		#generateIfTrue:on:indent:	#ifFalse:		#generateIfFalse:on:indent:	#ifTrue:ifFalse:	#generateIfTrueIfFalse:on:indent:	#ifFalse:ifTrue:	#generateIfFalseIfTrue:on:indent:	#at:				#generateAt:on:indent:	#at:put:			#generateAtPut:on:indent:	#basicAt:		#generateAt:on:indent:	#basicAt:put:	#generateAtPut:on:indent:	#integerValueOf:	#generateIntegerValueOf:on:indent:	#integerObjectOf:	#generateIntegerObjectOf:on:indent:	#isIntegerObject: 	#generateIsIntegerObject:on:indent:	#cCode:				#generateInlineCCode:on:indent:	#cCode:inSmalltalk:	#generateInlineCCode:on:indent:	#cCoerce:to:			#generateCCoercion:on:indent:	#preIncrement		#generatePreIncrement:on:indent:	#preDecrement		#generatePreDecrement:on:indent:	#inline:				#generateInlineDirective:on:indent:	#sharedCodeNamed:inCase:	#generateSharedCodeDirective:on:indent:	#asFloat				#generateAsFloat:on:indent:	#asInteger			#generateAsInteger:on:indent:	#anyMask:			#generateBitAnd:on:indent:	#raisedTo:			#generateRaisedTo:on:indent:	).	1 to: pairs size by: 2 do: [:i |		translationDict at: (pairs at: i) put: (pairs at: i + 1)].! !!TestCClass1 methodsFor: 'all' stamp: 'jm 2/15/1999 13:14'!method8: arg	| a |	self returnTypeC: 'float'.	self var: #a    declareC: 'float a = 0'.	self var: #arg declareC: 'float arg'.	self cCode: 'a = arg * 3.14159'.	self cCode: 'a = arg * 2.0' inSmalltalk: [a _ arg * 2.0].	^ a! !