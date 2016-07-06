'From Squeak2.8alpha of 4 February 2000 [latest update: #2210] on 1 June 2000 at 3:07:56 pm'!"Change Set:		105IntSpeedUp-smaDate:			1 June 2000Author:			Stefan Matthias Aust[Tweaked by Dan I. to fix a bug (last method in changeSet]From the profiles posted earlier, you could see that        Interpreter translate: 'interp.c' doInlining: falsespend about 20% in method Symbol class>>intern:.  About 17% of these are caused by TVariableNode>>bindVariablesIn:.  Here, we've a case where a dictionary of strings would perform much better than a dictionary of symbols.  I first tried to reduce the number of #intern: calls by only using symbols but that fired back, making the code even slower.  So I tried to remove the need to use asSymbol (which calls intern:) in that bind... method, using only string keys in all passed dictionaries.  That worked!!I hope I found all places where the dictionaries are initialized, modified or accessed and made sure that strings are used as keys - not symbols anymore.  The change is attached.With that change, the #intern: calls are reduced to 1.9% and the code runs about 30% faster (56 seconds instead of 73).  It still generates all 22 files without errors but I didn't try to compile the C source.  So it might be the case that I optimized too much."!!CCodeGenerator methodsFor: 'public'!addClassVarsFor: aClass	"Add the class variables for the given class to the code base as constants."	aClass classPool associationsDo: [:assoc | 		constants at: assoc key asString			put: (TConstantNode new setValue: assoc value)]! !!CCodeGenerator methodsFor: 'public'!addPoolVarsFor: aClass 	"Add the pool variables for the given class to the code base as constants."	aClass sharedPools do: [:pool |		pool associationsDo: [:assoc |			constants at: assoc key asString 				put: (TConstantNode new setValue: assoc value)]]! !!CCodeGenerator methodsFor: 'error notification'!checkClassForNameConflicts: aClass	"Verify that the given class does not have constant, variable, or method names that conflict with those of previously added classes. Raise an error if a conflict is found, otherwise just return."	"check for constant name collisions"	aClass classPool associationsDo: [ :assoc |		(constants includesKey: assoc key asString) ifTrue: [			self error: 'Constant was defined in a previously added class: ', assoc key.		].	].	"ikp..."	aClass sharedPools do: [:pool |		pool associationsDo: [ :assoc |			(constants includesKey: assoc key asString) ifTrue: [				self error: 'Constant was defined in a previously added class: ', assoc key.			].		].	].	"check for instance variable name collisions"	aClass instVarNames do: [ :varName |		(variables includes: varName) ifTrue: [			self error: 'Instance variable was defined in a previously added class: ', varName.		].	].	"check for method name collisions"	aClass selectors do: [ :sel |		(methods includesKey: sel) ifTrue: [			self error: 'Method was defined in a previously added class: ', sel.		].	].! !!CCodeGenerator methodsFor: 'inlining'!inlineDispatchesInMethodNamed: selector localizingVars: varsList	"Inline dispatches (case statements) in the method with the given name."	| m varString |	m _ self methodNamed: selector.	m = nil ifFalse: [		m inlineCaseStatementBranchesIn: self localizingVars: varsList.		m parseTree nodesDo: [ :n |			n isCaseStmt ifTrue: [				n customizeShortCasesForDispatchVar: 'currentBytecode'.			].		].	].	variables _ variables asOrderedCollection.	varsList do: [ :v |		varString _ v asString.		variables remove: varString ifAbsent: [].		(variableDeclarations includesKey: varString) ifTrue: [			m declarations at: v asString put: (variableDeclarations at: varString).			variableDeclarations removeKey: varString.		].	].! !!TMethod methodsFor: 'inlining'!argAssignmentsFor: meth args: argList in: aCodeGen	"Return a collection of assignment nodes that assign the given argument expressions to the formal parameter variables of the given method."	"Optimization: If the actual parameters are either constants or local variables in the target method (the receiver), substitute them directly into the body of meth. Note that global variables cannot be subsituted because the inlined method might depend on the exact ordering of side effects to the globals."	| stmtList substitutionDict |	stmtList _ OrderedCollection new: 100.	substitutionDict _ Dictionary new: 100.	meth args with: argList do: [ :argName :exprNode |		(self isSubstitutableNode: exprNode intoMethod: meth in: aCodeGen) ifTrue: [			substitutionDict at: argName put: exprNode.			locals remove: argName.		] ifFalse: [			stmtList add: (TAssignmentNode new				setVariable: (TVariableNode new setName: argName)				expression: exprNode copyTree).		].	].	meth parseTree: (meth parseTree bindVariablesIn: substitutionDict).	^stmtList! !!TMethod methodsFor: 'inlining'!inlineFunctionCall: aSendNode in: aCodeGen	"Answer the body of the called function, substituting the actual parameters for the formal argument variables in the method body."	"Assume caller has established that:		1. the method arguments are all substitutable nodes, and		2. the method to be inlined contains no additional embedded returns."	| sel meth substitutionDict |	sel _ aSendNode selector.	meth _ (aCodeGen methodNamed: sel) copy.	meth renameVarsForInliningInto: self in: aCodeGen.	meth renameLabelsForInliningInto: self.	self addVarsDeclarationsAndLabelsOf: meth.	substitutionDict _ Dictionary new: 100.	meth args with: aSendNode args do: [ :argName :exprNode |		substitutionDict at: argName put: exprNode.		locals remove: argName].	meth parseTree bindVariablesIn: substitutionDict.	^ meth statements first expression! !!TVariableNode methodsFor: 'as yet unclassified' stamp: 'sma 5/24/2000 23:42'!bindVariablesIn: aDictionary 	^ (aDictionary at: name ifAbsent: [^ self]) copyTree! !!TVariableNode methodsFor: 'as yet unclassified' stamp: 'di 6/5/2000 16:56'!bindVariableUsesIn: aDictionary	^ (aDictionary at: name ifAbsent: [^ self]) copyTree! !