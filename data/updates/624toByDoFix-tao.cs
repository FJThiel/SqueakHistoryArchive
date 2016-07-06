'From Squeak 2.3 of January 14, 1999 on 1 February 1999 at 8:37:45 pm'!"Change Set:		toByDoFixDate:			1 February 1999Author:			Tim OlsonThis causes the compiler to compile a send when the step of to:by:do: is zero, and also causes the method in Number to raise an apporpriate error in this case."!!MessageNode methodsFor: 'macro transformations' stamp: 'tao 1/30/1999 08:56'!transformToDo: encoder	" var _ rcvr. L1: [var <= arg1] Bfp(L2) [block body. var _ var + inc] Jmp(L1) L2: "	| limit increment block initStmt test incStmt limitInit blockVar |	"First check for valid arguments"	((arguments last isMemberOf: BlockNode)			and: [arguments last numberOfArguments = 1])		ifFalse: [^ false].	arguments last firstArgument isVariableReference		ifFalse: [^ false]. "As with debugger remote vars"	arguments size = 3		ifTrue: [increment _ arguments at: 2.				(increment isConstantNumber and:					[increment literalValue ~= 0]) ifFalse: [^ false]]		ifFalse: [increment _ encoder encodeLiteral: 1].	arguments size < 3 ifTrue:   "transform to full form"		[selector _ SelectorNode new key: #to:by:do: code: #macro].	"Now generate auxiliary structures"	block _ arguments last.	blockVar _ block firstArgument.	initStmt _ AssignmentNode new variable: blockVar value: receiver.	limit _ arguments at: 1.	limit isVariableReference | limit isConstantNumber		ifTrue: [limitInit _ nil]		ifFalse:  "Need to store limit in a var"			[limit _ encoder autoBind: blockVar key , 'LimiT'.			limit scope: -2.  "Already done parsing block"			limitInit _ AssignmentNode new					variable: limit					value: (arguments at: 1)].	test _ MessageNode new receiver: blockVar			selector: (increment key > 0 ifTrue: [#<=] ifFalse: [#>=])			arguments: (Array with: limit)			precedence: precedence from: encoder.	incStmt _ AssignmentNode new			variable: blockVar			value: (MessageNode new				receiver: blockVar selector: #+				arguments: (Array with: increment)				precedence: precedence from: encoder).	arguments _ (Array with: limit with: increment with: block)		, (Array with: initStmt with: test with: incStmt with: limitInit).	^ true! !!Number methodsFor: 'intervals' stamp: 'tao 1/30/1999 08:58'!to: stop by: step do: aBlock 	"Normally compiled in-line, and therefore not overridable.	Evaluate aBlock for each element of the interval (self to: stop by: step)."	| nextValue |	nextValue _ self.	step = 0 ifTrue: [self error: 'step must be non-zero'].	step < 0		ifTrue: [[stop <= nextValue]				whileTrue: 					[aBlock value: nextValue.					nextValue _ nextValue + step]]		ifFalse: [[stop >= nextValue]				whileTrue: 					[aBlock value: nextValue.					nextValue _ nextValue + step]]! !