'From Squeak 2.3 of January 14, 1999 on 25 January 1999 at 10:15:28 am'!
"Change Set:		ThingLab
Date:			23 January 1999
Last-changed-by:			Dan Ingalls

This file includes all the independent ThingLab classes.  The parts of ThingLab that alter existing Squeak classes are in the file ThingLab-Predefs, which should be filed in before this file.

ThingLab is a system for designing and programming with constraints.  It was originally written by Alan Borning around 1976 as part of his PhD thesis at Stanford University.  Alan is now at the University of Washington <borning@cs.washington.edu>.  These sources are almost unchanged from the publicly available sources to which Alan directed me as follows...

[connect to ftp.cs.washington.edu log in as anonymous.  Connect to the
subdirectory pub/constraints/code/old-ThingLab.  There are three files:
README, old-thinglab.tar.Z or hierarchies.tar.Z.  The latter two are 
compressed Unix tar files.

The old-thinglab.tar.Z is the original, pre-constraint hierarchies version.
This should work fine modulo the problems with simultaneous equations,
inequalities, and such (that we tried to fix with the newer constraint solvers)].
"!

Object subclass: #AccessPath
	instanceVariableNames: 'names '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #Constraint
	instanceVariableNames: 'ruleTree testTree errorTree methodDescriptions methodTrees '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #ConstraintMethodDescription
	instanceVariableNames: 'receiver uniqueState referenceOnly compileTimeOnly '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #ConstraintSatisfactionPlanner
	instanceVariableNames: 'methods mergeSet unalterable semialterable checkedLabels temps encoder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
DisplayMedium subclass: #DisplayRegionHack
	instanceVariableNames: 'clippingRectangle translation bitBlt '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
Object subclass: #FieldDescription
	instanceVariableNames: 'name index '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
FieldDescription subclass: #AbsoluteReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
Object subclass: #Format
	instanceVariableNames: 'name showSelector editSelector paneSpecies '
	classVariableNames: 'DefaultFormats '
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
WriteStream subclass: #IndentingStream
	instanceVariableNames: 'ntabs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
Constraint subclass: #InstanceConstraint
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #LabelConstraint
	instanceVariableNames: 'labelPath valuePath '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #MergeConstraint
	instanceVariableNames: 'paths '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #MergeMessage
	instanceVariableNames: 'context paths constraint owner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
OrderedCollection variableSubclass: #MergeSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Object subclass: #MessagePlan
	instanceVariableNames: 'context receiver constraint owner keywords arguments uniqueState referenceOnly compileTimeOnly '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
FieldDescription subclass: #PartDescription
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
MouseMenuController subclass: #PictureController
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
View subclass: #PictureView
	instanceVariableNames: 'buffer aspect changeMsg menuMsg '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
FieldDescription subclass: #PrimitiveDescription
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
ConstraintMethodDescription subclass: #QueuedMethodDescription
	instanceVariableNames: 'context constraint owner constraintMethod testMethod '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
FieldDescription subclass: #RelativeReference
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
Object subclass: #Relaxer
	instanceVariableNames: 'context objectOwners retrieveSelectors primitiveSetSelectors constraintOwnerVecs errorVecs testVecs methods objectOwner retrieveSelector primitiveSetSelector constraintOwners errors tests method showImageFlag '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Relaxer subclass: #BridgeRelaxer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Constraint subclass: #SetConstraint
	instanceVariableNames: 'setNode elementNode elementMessageTree '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Constraint subclass: #SetMembershipConstraint
	instanceVariableNames: 'set element '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Constraint subclass: #ConditionalConstraint
	instanceVariableNames: 'condition trueValue falseValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Constraints'!
Set variableSubclass: #SetOfPaths
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Dictionaries'!
FieldDescription subclass: #SpareField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
PartDescription subclass: #SuperclassDescription
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Fields'!
Controller subclass: #TextThingController
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
View subclass: #TextThingView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
Model subclass: #ThingLabBrowser
	instanceVariableNames: 'prototypeClass prototypeClassNameList prototypeClassListIndex prototype aspect aspectListIndex tool toolListIndex filter filterListIndex text displayObject '
	classVariableNames: 'TextMenu '
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
ThingLabBrowser subclass: #ObjectDefiner
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-ObjectDefiner'!
StandardSystemController subclass: #ThingLabBrowserController
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
StandardSystemView subclass: #ThingLabBrowserView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Windows'!
ThingLabBrowserView subclass: #ObjectDefinerView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-ObjectDefiner'!
Object subclass: #ThingLabObject
	instanceVariableNames: ''
	classVariableNames: 'Definitions Instances '
	poolDictionaries: ''
	category: 'ThingLab-Objects'!
ThingLabObject subclass: #BitImage
	instanceVariableNames: 'frame form '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Graphics'!
ThingLabObject subclass: #ConstraintPicture
	instanceVariableNames: 'center name leftNodes rightNodes topNodes bottomNodes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Graphics'!
ThingLabObject subclass: #InstancePartDeclaration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-ObjectDefiner'!
ThingLabObject subclass: #LineEquation
	instanceVariableNames: 'A B C '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Graphics'!
ThingLabObject subclass: #TextThing
	instanceVariableNames: 'frame text alignment leftInset topInset rightInset bottomInset textForm '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Graphics'!
ThingLabObject subclass: #ThingLabDefinition
	instanceVariableNames: 'instance '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Objects'!
ThingLabObject subclass: #ThingLabInstance
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Objects'!
ThingLabObject subclass: #ThingLabLine
	instanceVariableNames: 'point1 point2 '
	classVariableNames: 'BorderDot LineDrawingDot '
	poolDictionaries: ''
	category: 'ThingLab-Graphics'!
ThingLabObject class
	instanceVariableNames: 'prototype prototypeConstraints prototypeMerges prototypeMethods prototypeFieldDescriptions prototypeInstancePathsDict prototypeInserters prototypeConstrainers '!
ThingLabInstance class
	instanceVariableNames: 'prototypeInstanceConstraints prototypeScratchDefinition '!
Object subclass: #Tool
	instanceVariableNames: 'context editedObject editedObjectPath editedView otherViews carrier carrierShowSelector attachers attacherObjects attacher attacherNum carrierMove carrierReceiver selectionPart selectionPath selectionMove selectionReceiver stickyParts stickyPaths selectionMerges showForeground '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Tool subclass: #Deleter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Tool subclass: #Inserter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Inserter subclass: #Constrainer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Tool subclass: #MergingMover
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Tool subclass: #Mover
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThingLab-Editing'!
Tool subclass: #TextEditor
	instanceVariableNames: ''
	classVariableNames: 'TextEditForce '
	poolDictionaries: ''
	category: 'ThingLab-Editing'!

!AccessPath commentStamp: '<historical>' prior: 0!
A path is a way to get to one object from another by following a vector of field names.!

!AccessPath methodsFor: 'access to parts'!
names | |
	^names! !

!AccessPath methodsFor: 'access to parts'!
names: t1 | |
	names _ t1! !

!AccessPath methodsFor: 'hashing and tests'!
= other
	^names = other names! !

!AccessPath methodsFor: 'hashing and tests'!
hash
		| h |
	h _ 0.
	names do: [:n | h _ h bitXor: n hash].
	^h! !

!AccessPath methodsFor: 'hashing and tests'!
isEmpty
	^names size = 0! !

!AccessPath methodsFor: 'hashing and tests'!
overlaps: other
	"Return true if my image is within other's image or
		other's image is within my image."
	| othernames |
	othernames _ other names.
	1 to: (names size min: othernames size) do:
		[:i | (names at: i) = (othernames at: i)
		  ifFalse: [^false]].
	^true! !

!AccessPath methodsFor: 'hashing and tests'!
overlapsOneOf: vec
	vec do:
		[:p |  "Return true if my image overlaps one of the paths in vec"
		(self overlaps: p)
		  ifTrue: [^true]].
	^false! !

!AccessPath methodsFor: 'hashing and tests'!
validPathFor: object | image |
	 "return true if I still point to an existing subpart ... kludgey message for
	use when deleting in the presence of merges"
	image _ object.
	names do: [:pName |
				image==nil ifTrue: [^false].
				(image class canUnderstand: pName) ifFalse: [^false].
				image _ image perform: pName].
	^true! !

!AccessPath methodsFor: 'hashing and tests'!
within: other
	"Return the difference if my image is within other's image,
		i.e. if my list of names is an extension of other's list of names;
		otherwise return nil"
	| length othernames otherlength |
	othernames _ other names.
	length _ names size.
	otherlength _ othernames size.
	otherlength > length ifTrue: [^ nil].
	1 to: otherlength do:
		[:i | (names at: i) = (othernames at: i) ifFalse: [^nil]].
	^AccessPath new names: (names copyFrom: otherlength + 1 to: length)! !

!AccessPath methodsFor: 'hashing and tests'!
withinOneOf: vec
	"Return the difference if my image is one of the paths in vec,
		otherwise return nil"
	| difference |
	vec do:
		[:p | (difference _ self within: p) notNil ifTrue: [^difference]].
	^nil! !

!AccessPath methodsFor: 'operations'!
- other | |
	(self within: other) notNil
	  ifTrue: [^AccessPath new names: (names copyFrom: other names size + 1 to: names size)].
	^ nil! !

!AccessPath methodsFor: 'operations'!
add: pName
	^AccessPath new names: (names copyWith: pName)! !

!AccessPath methodsFor: 'operations'!
allButLast
	 "return a new path with all but the last name"
	^AccessPath new names: (names copyFrom: 1 to: names size-1)! !

!AccessPath methodsFor: 'operations'!
applyTo: object | image |
	 "return the image of the object through me"
	image _ object.
	names do: [:pName | image _ image perform: pName].
	^image! !

!AccessPath methodsFor: 'operations'!
concat: other | |
	^AccessPath new names: (names , other names)! !

!AccessPath methodsFor: 'operations'!
first
	^AccessPath new names: (Array with: names first)! !

!AccessPath methodsFor: 'operations'!
firstName
	^names at: 1! !

!AccessPath methodsFor: 'operations'!
globalRef | x |
	self isEmpty
	  ifFalse:
		[x _ Smalltalk at: (names at: 1).
		(x isKindOf: Behavior)
		  ifTrue: [x _ x prototype].
		^self tail applyTo: x]! !

!AccessPath methodsFor: 'operations'!
lastName
	^names at: names size! !

!AccessPath methodsFor: 'operations'!
tail
	 "return a new path with all but the first name"
	^AccessPath new names: (names copyFrom: 2 to: names size)! !

!AccessPath methodsFor: 'compiling methods'!
code: encoder
	"return code in parse tree form to access the subpart that I reference"
	names size=0 ifTrue: [^encoder encodeVariable: 'self'].
	names size=1 ifTrue: [^encoder encodeVariable: names first asString].
	^MessageNode new
		receiver: (self allButLast code: encoder)
		selector: names last
		arguments: Array new
		precedence: names last precedence
		from: encoder! !

!AccessPath methodsFor: 'compiling methods'!
code: encoder context: contextNode
	"return code in parse tree form to access the subpart that I reference starting at context"
	names size=0 ifTrue: [^contextNode].
	^MessageNode new
		receiver: (self allButLast code: encoder context: contextNode)
		selector: names last
		arguments: Array new
		precedence: names last precedence
		from: encoder! !

!AccessPath methodsFor: 'compiling methods'!
codeTo: strm
		| context |
	context _ (names size=0 ifTrue: ['self'] ifFalse: ['']).
	self codeTo: strm context: context! !

!AccessPath methodsFor: 'compiling methods'!
codeTo: strm context: context
	strm nextPutAll: context; space.
	names do: [:pName | strm nextPutAll: pName; space]! !

!AccessPath methodsFor: 'compiling methods'!
dottedNameString
		| strm |
	strm _ WriteStream on: (String new: 200).
	1 to: names size-1 do: [:i | strm nextPutAll: (names at: i).  strm nextPutAll: '.'].
	names size>0 ifTrue: [strm nextPutAll: names last].
	^strm contents! !

!AccessPath methodsFor: 'compiling methods'!
nameString
		| strm |
	strm _ WriteStream on: (String new: 200).
	names do: [:n | strm nextPutAll: n.  strm space].
	^strm contents! !

!AccessPath methodsFor: 'compiling methods'!
primitiveSetLast
		| strm |
	strm _ WriteStream on: (String new: 200).
	(names copyFrom: 1 to: names size-1) do:
		[:n | strm nextPutAll: n.  strm space].
	strm nextPutAll: 'primitiveSet.'.
	strm nextPutAll: names last.
	strm nextPut: $:.
	^strm contents! !

!AccessPath methodsFor: 'printing'!
printNamesOn: strm
	1 to: names size-1 do:
		[:i | strm nextPutAll: (names at: i); space].
	names size>0 ifTrue:
		[strm nextPutAll: names last]! !

!AccessPath methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: '<'.
	self printNamesOn: strm.
	strm nextPutAll: '>'! !

!AccessPath methodsFor: 'conversions'!
asPath
	^ self! !


!AccessPath class methodsFor: 'initialization'!
initialize
	Smalltalk at: #EmptyPath put: (AccessPath new names: Array new)! !


!Constraint commentStamp: '<historical>' prior: 0!
Instance Variables:
	ruleTree	<MessageNode> -- the rule that must hold
	testTree	<MessageNode> -- a test to see if the constraint is satisfied
	errorTree	<MessageNode> -- returns a number indicating how close to
		being satisfied is the constraint (0.0 means completely satisfied)
	methodDescriptions	<Array of ConstraintMethodDescription>
	methodTrees 	<Array of MessageNode> -- methods for making the constraint hold!

!Constraint methodsFor: 'initialization'!
methodsNotUniqueState
	"set flags that methods are not unique state (for non-reference only methods)"
	methodDescriptions do:
		[:m | m referenceOnly ifFalse: [m uniqueState: false]]! !

!Constraint methodsFor: 'initialization'!
ruleTree: r testTree: t errorTree: e methodDescriptions: descrs methodTrees: m
	ruleTree _ r.
	testTree _ t.
	errorTree _ e.
	methodDescriptions _ descrs.
	methodTrees _ m! !

!Constraint methodsFor: 'access to parts'!
methodDescriptions
	^methodDescriptions! !

!Constraint methodsFor: 'access to parts'!
testTree
	^testTree! !

!Constraint methodsFor: 'tests'!
hasPath: path
	methodDescriptions do:
		[:descr | path = descr receiver ifTrue: [^true]].
	^false! !

!Constraint methodsFor: 'tests'!
isLabelConstraint
	^false! !

!Constraint methodsFor: 'tests'!
overlaps: path
	 "return true if any of my messages' receivers overlap path"
	methodDescriptions do:
		[:descr | (descr receiver overlaps: path) ifTrue: [^true]].
	^false! !

!Constraint methodsFor: 'compiling'!
addLabelMethods: label context: context owner: owner rootPart: rootPart planner: planner
	"default is to do nothing"! !

!Constraint methodsFor: 'compiling'!
addMethods: queue context: context owner: owner receiver: receiver
	"add my messages to the queue"

	"changed from oldAddMethods: to also add reference only methods ...
	 will this work right?"
		| queuedDescr queuedTest |
	(self overlaps: receiver) ifFalse: [^nil].
	"see if I have already been told to add messages to the queue"
	(queue hasConstraint: self owner: owner) ifTrue: [^nil].
	queuedTest _ self testCode: queue encoder owner: owner.
	methodDescriptions with: methodTrees do:
		[:descr :method | "make a message plan with the correct
			  context, receiver, and owner"
			queuedDescr _ QueuedMethodDescription new
				context: context
				receiver: (owner concat: descr receiver)
				constraint: self
				owner: owner
				uniqueState: descr uniqueState
				referenceOnly: descr referenceOnly
				compileTimeOnly: descr compileTimeOnly
				constraintMethod: (method relocatePaths: owner encoder: queue encoder)
				testMethod: queuedTest.
			queue addMethod: queuedDescr.
			queuedDescr addMethods: queue]! !

!Constraint methodsFor: 'compiling'!
deleteFrom: prototype
	prototype deleteConstraint: self.
	prototype forgetConstraintMethods! !

!Constraint methodsFor: 'compiling'!
deleteFrom: prototype ifOverlaps: path
	(self overlaps: path)
	  ifTrue: [self deleteFrom: prototype]! !

!Constraint methodsFor: 'compiling'!
errorCode: encoder owner: owner
	"return a method (in parse tree form) to return my error, in the
	  context of owner"
	^errorTree relocatePaths: owner encoder: encoder! !

!Constraint methodsFor: 'compiling'!
insertIn: prototype
	prototype addConstraint: self.
	prototype forgetConstraintMethods! !

!Constraint methodsFor: 'compiling'!
oldAddMethods: queue context: context owner: owner receiver: receiver
	"add my messages to the queue"
		| queuedDescr queuedTest |
	(self overlaps: receiver) ifFalse: [^nil].
	"see if I have already been told to add messages to the queue"
	(queue hasConstraint: self owner: owner) ifTrue: [^nil].
	queuedTest _ testTree relocatePaths: owner encoder: queue encoder.
	methodDescriptions with: methodTrees do:
		[:descr :method |
		descr referenceOnly ifFalse:
			["make a message plan with the correct
			  context, receiver, and owner"
			queuedDescr _ QueuedMethodDescription new
				context: context
				receiver: (owner concat: descr receiver)
				constraint: self
				owner: owner
				uniqueState: descr uniqueState
				referenceOnly: descr referenceOnly
				compileTimeOnly: descr compileTimeOnly
				constraintMethod: (method relocatePaths: owner encoder: queue encoder)
				testMethod: queuedTest.
			queue addMethod: queuedDescr.
			queuedDescr addMethods: queue]]! !

!Constraint methodsFor: 'compiling'!
testCode: encoder owner: owner
	^ (methodDescriptions at: 1) compileTimeOnly
		ifTrue: [encoder encodeVariable: 'true']
		ifFalse: [testTree relocatePaths: owner encoder: encoder]! !

!Constraint methodsFor: 'printing'!
fileOutOn: strm forOwner: owner
	strm nextPutAll: 'Constraint owner: ' , owner class name , ' prototype'.
	strm cr; tab; nextPutAll: 'rule: '''.
	ruleTree printOn: strm indent: 0.
	strm nextPut: $'.
	strm cr; tab; nextPutAll: 'methods: #('.
	methodTrees do:
		[:m | strm cr; tab; tab; nextPut: $'. 
			m printOn: strm indent: 0.
			strm nextPut: $'].
	strm nextPutAll: ')!!'; cr; cr! !

!Constraint methodsFor: 'printing'!
printOn: strm
	ruleTree printOn: strm indent: 0! !


!Constraint class methodsFor: 'initialization'!
makeErrorFromRule: rTree owner: owner encoder: encoder
	"Generate an error method given a rule"

	"If the rule is of the form xx = yy, then the error is xx constraintDifference: yy.
	 Otherwise, the error is 'test asError' "
	((rTree isKindOf: MessageNode) and: [rTree selector key = #=])
		ifTrue: [^MessageNode new
			receiver: rTree receiver
			selector: #constraintDifference:
			arguments: rTree arguments
			precedence: #constraintDifference: precedence
			from: encoder]
		ifFalse: [^MessageNode new
			receiver: rTree
			selector: #asError
			arguments: #()
			precedence: #asError precedence
			from: encoder]! !

!Constraint class methodsFor: 'initialization'!
makeTestFromRule: rTree owner: owner encoder: encoder
	"Generate a test given a rule.  If the rule is of the form
		xx = yy, then the error is xx withinTolerance: yy.
	 Otherwise, the test is the same as the rule"
	^ ((rTree isKindOf: MessageNode) and: [rTree selector key = #=])
		ifTrue: [MessageNode new
			receiver: rTree receiver
			selector: #withinTolerance:
			arguments: rTree arguments
			precedence: #withinTolerance: precedence
			from: encoder]
		ifFalse: [rTree]! !

!Constraint class methodsFor: 'initialization'!
noInsertOwner: owner ruleTree: rTree testTree: tTreeOrNil errorTree: eTreeOrNil methodTrees: mTrees
	"Make up a new constraint, initializing its rule, test and methods from parse trees.
	 test and error may be nil, in which case they are defaulted."

		| tTree eTree descrs encoder |
	encoder _ Encoder new init: owner class context: nil notifying: nil.
	eTree _ eTreeOrNil isNil
		ifTrue: [self makeErrorFromRule: rTree owner: owner encoder: encoder]
		ifFalse: [eTreeOrNil].
	tTree _ tTreeOrNil isNil
		ifTrue: [self makeTestFromRule: rTree owner: owner encoder: encoder]
		ifFalse: [tTreeOrNil].
	descrs_ mTrees collect:
		[:tree | ConstraintMethodDescription new
					receiver: (tree makePathFor: owner)
					selector: tree selector key].
	^self new ruleTree: rTree testTree: tTree errorTree: eTree
		methodDescriptions: descrs methodTrees: mTrees! !

!Constraint class methodsFor: 'initialization'!
owner: owner rule: rString error: eString methods: mStrings
	^self owner: owner rule: rString test: nil error: eString methods: mStrings! !

!Constraint class methodsFor: 'initialization'!
owner: owner rule: rString methods: mStrings
	^self owner: owner rule: rString test: nil error: nil methods: mStrings! !

!Constraint class methodsFor: 'initialization'!
owner: owner rule: rString test: tString error: eString methods: mStrings
	"Make up a new constraint, initializing its rule, test and methods from strings.
	 test and error may be nil, in which case they are defaulted."

		| rMethod rTree tMethod tTree eMethod eTree mMethods mTrees aConstraint |
	rMethod _ Compiler new parse: 'rule ' , rString in: owner class notifying: nil.
	rTree _ rMethod block statements at: 1.
	eString isNil ifFalse:
		[eMethod _ Compiler new parse: 'error ' , eString in: owner class notifying: nil.
		eTree _ eMethod block statements at: 1].
	tString isNil ifFalse:
		[tMethod _ Compiler new parse: 'test ' , tString in: owner class notifying: nil.
		tTree _ tMethod block statements at: 1].
	mMethods_ mStrings collect:
		[:s | Compiler new parse: 'satisfy ' , s in: owner class notifying: nil].
	mTrees_ mMethods collect:
		[:m | m block statements at: 1].
	aConstraint _ self noInsertOwner: owner ruleTree: rTree testTree: tTree
		errorTree: eTree methodTrees: mTrees.
	aConstraint insertIn: owner.
	^aConstraint! !

!Constraint class methodsFor: 'initialization'!
owner: owner rule: rString test: tString methods: mStrings
	^self owner: owner rule: rString test: tString error: nil methods: mStrings! !


!ConstraintMethodDescription commentStamp: '<historical>' prior: 0!
I describe one of the methods of a constraint.

Instance Variables:
	receiver	<Path> -- a path from the owner of the constraint to the subpart affected by the method
	uniqueState	<Boolean> -- true iff only one state of the receiver will satisfy the constraint
	referenceOnly	<Boolean> -- true iff the receiver is only referenced
	compileTimeOnly	<Boolean> -- true iff there is no method that executes at run time
!

!ConstraintMethodDescription methodsFor: 'initialization'!
receiver: rcvr selector: sel
	"set flags for some common selectors"
	receiver _ rcvr.
	sel = #reference ifTrue: 
			[uniqueState _ false.
			referenceOnly _ true.
			compileTimeOnly _ true.
			^self].
	sel = #check ifTrue: 
			[uniqueState _ false.
			referenceOnly _ true.
			compileTimeOnly _ true.
			^self].
	sel = #alter ifTrue: 
			[uniqueState _ false.
			referenceOnly _ false.
			compileTimeOnly _ false.
			^self].
	sel = #fixed ifTrue: 
			[uniqueState _ true.
			referenceOnly _ false.
			compileTimeOnly _ true.
			^self].
	"not a special selector - set flags to the defaults"
	uniqueState _ true.
	referenceOnly _ false.
	compileTimeOnly _ false! !

!ConstraintMethodDescription methodsFor: 'initialization'!
receiver: rcvr uniqueState: uState referenceOnly: refOnly compileTimeOnly: cOnly
	receiver _ rcvr.
	uniqueState _ uState.
	referenceOnly _ refOnly.
	compileTimeOnly _ cOnly! !

!ConstraintMethodDescription methodsFor: 'initialization'!
uniqueState: uState
	uniqueState _ uState! !

!ConstraintMethodDescription methodsFor: 'access to parts'!
compileTimeOnly
	^compileTimeOnly! !

!ConstraintMethodDescription methodsFor: 'access to parts'!
receiver
	^receiver! !

!ConstraintMethodDescription methodsFor: 'access to parts'!
referenceOnly
	^referenceOnly! !

!ConstraintMethodDescription methodsFor: 'access to parts'!
uniqueState
	^uniqueState! !

!ConstraintMethodDescription methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: self class name.
	strm nextPutAll: ' for '.
	strm print: receiver.! !


!ConstraintSatisfactionPlanner commentStamp: '<historical>' prior: 0!
Holds state while compiling a constraint satisfaction method.  I include a collection of methods waiting to be processed.

Instance variables:
	methods -- a collection of methods from constraints
	merges -- a MergeSet
	unalterable -- collection of paths to objects which cannot be altered
	semialterable -- collection of paths to objects which should not be altered 
		unless necessary
	checkedLabels -- collection of labels (Symbols) which have already been processed
	temps -- collection of names of temporaries
	encoder -- an Encoder!

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
checkedLabels
	^checkedLabels! !

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
encoder
	^encoder! !

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
semialterable
	^semialterable! !

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
temps
	^temps! !

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
unalterable
	^unalterable! !

!ConstraintSatisfactionPlanner methodsFor: 'access to parts'!
unalterable: u
	unalterable _ u! !

!ConstraintSatisfactionPlanner methodsFor: 'initialization'!
initFor: object
	methods _ OrderedCollection new.
	mergeSet _ MergeSet new.
	unalterable _ Array new.
	semialterable _ Array new.
	checkedLabels _ OrderedCollection new.
	temps _ OrderedCollection new.
	encoder _ Encoder new init: object class context: nil notifying: object! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
allPathsTo: p
	^mergeSet allPathsTo: p! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
findConstraint: constraint owner: owner
	"return a vector of all messages from the constraint to the owner"
	| paths |
	paths _ mergeSet allPathsTo: owner.
	^ methods select:
		[:message | constraint = message constraint and: [paths includes: message owner]]! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
findEqualMessages: message
	"return a vector of messages equal to message"
	| paths owner constraint keywords |
	paths _ mergeSet allPathsTo: message receiver.
	owner _ message owner.
	constraint _ message constraint.
	keywords _ message keywords.
	^methods select:
		[:m |
		((m owner = owner
		  and: [m constraint = constraint])
		  and: [m keywords = keywords])
		  and: [paths includes: m receiver]]! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
findPostponable
	"return a postponable message or nil"
	methods do:
		[:message | (message shouldBePostponed: self) ifTrue: [^ message]].
	methods do:
		[:message | (message canBePostponed: self) ifTrue: [^ message]].
	^ nil! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
findReceiver: receiver
	"return a vector of all messages affecting receiver"
	| paths |	
	paths _ mergeSet allPathsTo: receiver.
	^ methods select: [:message | message receiver overlapsOneOf: paths]! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
findSendable
	"return a message that can be sent next, or nil if none"
	methods do:
		[:message | (message canBeSentNext: self) ifTrue: [^ message]].
	^ nil! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
firstMethod
	^methods first! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
hasConstraint: constraint owner: owner
	| paths |
	paths _ mergeSet allPathsTo: owner.
	methods do:
		[:message |
		(constraint = message constraint and: [paths includes: message owner])
			ifTrue: [^true]].
	^false! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
hasMerge: merge
	^ mergeSet hasMerge: merge! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
methodsDo: block
	methods copy do: block! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
nConstraintsOn: receiver
	"return the number of constraints on receiver"
	| paths nConstraints |
	paths _ mergeSet allPathsTo: receiver.
	nConstraints _ 0.
	methods do:
		[:message |
		message referenceOnly "doesn't count"
		  ifFalse:
			[(message receiver overlapsOneOf: paths)
			  ifTrue: [nConstraints _ nConstraints + 1]]].
	^nConstraints! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
nMethods
	^methods size! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
noAlteringMessagesTo: receiver
		| vec |
	vec _ self findReceiver: receiver.
	vec do: [:m | m referenceOnly ifFalse: [^false]].
	^true! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
noMethods
	^methods size=0! !

!ConstraintSatisfactionPlanner methodsFor: 'searching'!
pickUnknown
	"Return a path to an unknown.  Try not to alter stuff in semialterable.
	Also look for the receiver with the most constraints.  See Sussman and Stallman."
		| receiver bestYet isSemiAlterable nConstraints n |
	bestYet _ nil.
	isSemiAlterable _ true.
	nConstraints _ 0.
	methods do:
		[:message | receiver _ message receiver.
		(self testForUnalterablePart: receiver)
		  ifFalse: "reject"
			[(self testForSemialterablePart: receiver)
			  ifTrue:
				[isSemiAlterable
				  ifTrue: "reject-worse than bestYet"
					[n _ self nConstraintsOn: receiver.
					n > nConstraints
					  ifTrue:
						[bestYet _ receiver.
						nConstraints _ n]]]
			  ifFalse:
				[n _ self nConstraintsOn: receiver.
				n > nConstraints
				  ifTrue:
					[bestYet _ receiver.
					nConstraints _ n.
					isSemiAlterable _ false]]]].
	^ bestYet! !

!ConstraintSatisfactionPlanner methodsFor: 'adding and removing'!
addMerge: message
	mergeSet add: message! !

!ConstraintSatisfactionPlanner methodsFor: 'adding and removing'!
addMethod: m
	methods add: m! !

!ConstraintSatisfactionPlanner methodsFor: 'adding and removing'!
deleteDuplicates
	"delete duplicate messages"
		| duplicates message paths |
	duplicates _ OrderedCollection new.
	1 to: methods size do:
		[:i | message _ methods at: i.
		paths _ mergeSet allPathsTo: message receiver.
		i + 1 to: methods size do:
			[:j |
			((methods at: j) sameAs: message receiverPaths: paths)
				ifTrue: [duplicates add: (methods at: j)]]].
	duplicates do: [:msg | methods remove: msg ifAbsent: []]! !

!ConstraintSatisfactionPlanner methodsFor: 'adding and removing'!
removeMethod: m
	methods remove: m! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
makeSemialterable: path
	(mergeSet allPathsTo: path) do:
		[:p | (p withinOneOf: semialterable) notNil
		  ifFalse: [semialterable _ semialterable copyWith: p]]! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
makeUnalterable: path
	(mergeSet allPathsTo: path) do:
		[:p | (p withinOneOf: unalterable) notNil
		  ifFalse: [unalterable _ unalterable copyWith: p]]! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
testForSemialterablePart: path
	"return true if the object at the end of path or one of its parts is semialterable"
	(mergeSet allPathsTo: path) do:
		[:p | (p overlapsOneOf: semialterable) ifTrue: [^true]].
	^false! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
testForUnalterablePart: path
	"return true if the object at the end of path or one of its parts is unalterable"
	(mergeSet allPathsTo: path) do:
		[:p | (p overlapsOneOf: unalterable) ifTrue: [^true]].
	^false! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
testIfSemialterable: path
	(mergeSet allPathsTo: path) do:
		[:p | (p withinOneOf: semialterable) notNil ifTrue: [^true]].
	^false! !

!ConstraintSatisfactionPlanner methodsFor: 'alterable paths'!
testIfUnalterable: path
	(mergeSet allPathsTo: path) do:
		[:p | (p withinOneOf: unalterable) notNil ifTrue: [^true]].
	^false! !

!ConstraintSatisfactionPlanner methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: 'MethodQueue'; cr.
	methods printOn: strm.
	strm cr.
	mergeSet printOn: strm.
	strm cr.
	strm nextPutAll: 'unalterable: '.
	strm print: unalterable.
	strm cr.
	strm nextPutAll: 'semialterable: '.
	strm print: semialterable! !


!ConstraintSatisfactionPlanner class methodsFor: 'initialization'!
for: object
	^ self new initFor: object! !


!DisplayRegionHack commentStamp: '<historical>' prior: 0!
I am similar to the RegionMedium class in the Xerox research image; I just have the minimum
functionality needed to do the job.!

!DisplayRegionHack methodsFor: 'initialization'!
clippingRectangle: c translation: t
	clippingRectangle _ c.
	translation _ t.
	bitBlt _ BitBlt destForm: Display sourceForm: nil halftoneForm: nil combinationRule: nil destOrigin: t
		sourceOrigin: 0@0 extent: 0@0 clipRect: c! !

!DisplayRegionHack methodsFor: 'initialization' stamp: 'di 1/21/1999 20:10'!
depth
	^ bitBlt destForm depth! !

!DisplayRegionHack methodsFor: 'display medium protocol'!
computeBoundingBox
	^clippingRectangle! !

!DisplayRegionHack methodsFor: 'display medium protocol' stamp: 'di 1/21/1999 20:15'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: ignore rule: rule fillColor: aForm map: colorMapOrNil
	bitBlt sourceForm: sourceForm;
		sourceRect: sourceRect;
		destOrigin: destOrigin+translation;
		combinationRule: rule;
		fillColor: aForm;
		colorMap: colorMapOrNil.
	bitBlt copyBits! !

!DisplayRegionHack methodsFor: 'display medium protocol'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: ignore rule: rule mask: mask 
	bitBlt sourceForm: sourceForm;
		sourceRect: sourceRect;
		destOrigin: destOrigin+translation;
		combinationRule: rule;
		mask: mask.
	bitBlt copyBits! !

!DisplayRegionHack methodsFor: 'display medium protocol' stamp: 'di 1/21/1999 20:34'!
drawLine: sourceForm from: beginPoint to: endPoint rule: rule fillColor: aColor
	bitBlt sourceForm: sourceForm;
		sourceRect: sourceForm computeBoundingBox;
		combinationRule: rule;
		fillColor: aColor.
	bitBlt drawFrom: beginPoint+translation to: endPoint+translation! !

!DisplayRegionHack methodsFor: 'display medium protocol' stamp: 'di 1/21/1999 20:16'!
fill: rect rule: rule fillColor: aColor
	bitBlt sourceForm: nil;
		sourceOrigin: 0@0;
		destRect: (rect translateBy: translation);
		combinationRule: rule;
		fillColor: aColor.
	bitBlt copyBits! !

!DisplayRegionHack methodsFor: 'display medium protocol'!
fill: rect rule: rule mask: mask
	bitBlt sourceForm: nil;
		sourceOrigin: 0@0;
		destRect: (rect translateBy: translation);
		combinationRule: rule;
		mask: mask.
	bitBlt copyBits! !

!DisplayRegionHack methodsFor: 'paragraph hack'!
displayParagraph: para at: pt
	para displayOn: Display at: pt+translation clippingBox: clippingRectangle! !


!FieldDescription commentStamp: '<historical>' prior: 0!
I describe an instance field of a prototype and its copies

Instance variables:
	index <Integer> -- my index in my owner's instance fields!

!FieldDescription methodsFor: 'initialization'!
name: n index: i
	name _ n.
	index _ i! !

!FieldDescription methodsFor: 'access to parts'!
index
	^index! !

!FieldDescription methodsFor: 'access to parts'!
name
	^name! !

!FieldDescription methodsFor: 'testing'!
isAbsoluteReference
	"return true if I hold an absolute reference - default is that I do not"
	^ false! !

!FieldDescription methodsFor: 'testing'!
isPart
	"return true if I hold a part - default is that I do not"
	^ false! !

!FieldDescription methodsFor: 'testing'!
isPrimitive
	"return true if I hold a primitive part - default is that I do not"
	^ false! !

!FieldDescription methodsFor: 'testing'!
isRelativeReference
	"return true if I hold a relative reference - default is that I do not"
	^ false! !

!FieldDescription methodsFor: 'testing'!
isSpare
	"return true if I am a spare field - default is that I am not spare"
	^ false! !

!FieldDescription methodsFor: 'testing'!
isSuperclassPart
	"return true if I hold a part representing a superclass - default is that I do not"
	^ false! !

!FieldDescription methodsFor: 'conversions'!
asPath
	^AccessPath new names: (Array with: name)! !

!FieldDescription methodsFor: 'showing'!
printField: strm field: field
	self isSpare ifFalse:
	  [strm cr; nextPutAll: name.
	  strm nextPutAll: ': '.
	  self isPart
		ifTrue: [field printTableOfFields: strm]
		ifFalse: [field printOn: strm]]! !

!FieldDescription methodsFor: 'showing'!
printFieldStructure: strm field: field
	| title |
	self isSpare ifFalse:
		[strm cr; nextPutAll: name.
		title _ field class name.
		strm nextPutAll: (('AEIOU' includes: (title at: 1))
		  ifTrue: [': an ']
		  ifFalse: [': a ']).
		strm nextPutAll: title]! !

!FieldDescription methodsFor: 'compiling'!
deleteFrom: prototype
	prototype fieldDescriptions at: index put: (SpareField name: name index: index)! !

!FieldDescription methodsFor: 'compiling'!
insertIn: prototype
	prototype fieldDescriptions at: index put: self! !

!FieldDescription methodsFor: 'compiling'!
makeAssignNode: newValueNode encoder: encoder
	"return a message node of the form
			myname _ newValue
	"

	^AssignmentNode new
		variable: (encoder encodeVariable: name asString)
		value: newValueNode! !

!FieldDescription methodsFor: 'compiling'!
makePrimitiveSetNode: newValueNode sample: sample encoder: encoder
	self subclassResponsibility! !

!FieldDescription methodsFor: 'compiling'!
makeRetrieveNode: encoder
	self subclassResponsibility! !

!FieldDescription methodsFor: 'relative references'!
relocatePathFor: obj prefix: p
	 "default is no-op - RelativeReference will overrride this"! !

!FieldDescription methodsFor: 'printing'!
printOn: strm
	strm nextPut: $(.
	strm nextPutAll: self class name.
	strm nextPutAll: ' for '.
	strm nextPutAll: name.
	strm nextPut: $)! !


!AbsoluteReference commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!AbsoluteReference methodsFor: 'testing'!
isAbsoluteReference
	^ true! !


!FieldDescription class methodsFor: 'instance creation'!
name: n index: i
	^self new name: n index: i! !


!Format commentStamp: '<historical>' prior: 0!
I describe a way an object can display itself.

Instance variables:
	name -- descriptive string
	showSelector -- selector for method for showing
	editSelector -- selector for method for editing
	paneSpecies -- class of pane for showing in my format!

!Format methodsFor: 'initialization'!
name: t1 showSelector: t2 editSelector: t3 paneSpecies: t4 | |
	name _ t1.
	showSelector _ t2.
	editSelector _ t3.
	paneSpecies _ t4! !

!Format methodsFor: 'access to parts'!
editSelector
	^editSelector! !

!Format methodsFor: 'access to parts'!
name
	^name! !

!Format methodsFor: 'access to parts'!
showSelector
	^showSelector! !

!Format methodsFor: 'classification'!
isPicture 
	^showSelector == #showPicture:! !


!Format class methodsFor: 'initialization' stamp: 'di 1/23/1999 05:14'!
initialize
	"Format initialize"
	DefaultFormats _ Array
		with: (Format new name: 'picture' showSelector: #showPicture:
					editSelector: #editPicture: paneSpecies: PictureView)
		with: (Format new name: 'structure' showSelector: #showStructure:
					editSelector: #editStructure: paneSpecies: PluggableTextView)
		with: (Format new name: 'values' showSelector: #showValues:
					editSelector: #editValues: paneSpecies: PluggableTextView)! !

!Format class methodsFor: 'default formats'!
defaultFormats
	^DefaultFormats! !


!IndentingStream commentStamp: '<historical>' prior: 0!
An indenting stream appends some tabs after each cr!

!IndentingStream methodsFor: 'indenting'!
cr
	super cr.
	1 to: ntabs do: [:i | self tab]! !

!IndentingStream methodsFor: 'indenting'!
indent | |
	ntabs _ ntabs + 1! !

!IndentingStream methodsFor: 'indenting'!
reset | |
	super reset.
	ntabs _ 0! !

!IndentingStream methodsFor: 'indenting'!
unindent | |
	ntabs _ ntabs - 1! !


!IndentingStream class methodsFor: 'instance creation'!
new
	^ (self basicNew on: (String new: 100)) reset! !


!InstanceConstraint methodsFor: 'compiling'!
addMethods: queue context: context owner: owner receiver: receiver
	"kludgy override - if I'm the instance in a definition, don't wake me up"
	(owner names includes: #instance) ifTrue: [^nil].
	^super addMethods: queue context: context owner: owner receiver: receiver! !

!InstanceConstraint methodsFor: 'compiling'!
insertIn: prototype
	"don't do anything ... InstanceConstraints are indexed in a separate way"! !

!InstanceConstraint methodsFor: 'compiling'!
testCode: encoder owner: owner
	^encoder encodeVariable: 'true'! !

!InstanceConstraint methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: '{InstanceConstraint on'.
	methodDescriptions do:
		[:m | strm space.  m receiver printOn: strm].
	strm nextPut: $}! !


!LabelConstraint methodsFor: 'initialization'!
setLabelPath: l valuePath: v
	labelPath _ l.
	valuePath _v! !

!LabelConstraint methodsFor: 'tests'!
hasPath: path
	^valuePath=path! !

!LabelConstraint methodsFor: 'tests'!
isLabelConstraint
	^true! !

!LabelConstraint methodsFor: 'tests'!
overlaps: path
	^valuePath overlaps: path! !

!LabelConstraint methodsFor: 'compiling'!
addLabelMethods: label context: context owner: owner rootPart: rootPart planner: planner
	"If the path 'label' overlaps my label, add methods for it.  Arguments are as follows:
		context: top-level owner
		owner: path from context to the owner of this constraint
		rootPart: path from context to some (arbitrary) value labelled with 'label'
		planner: a constraint satisfaction planner "
	  | myLabel fullValuePath realRootPart realFullValuePath d |
	myLabel _ (labelPath applyTo: (owner applyTo: context)) asPath.
	(label overlaps: myLabel) ifFalse: [^nil].
	fullValuePath _ owner concat: valuePath.
	"if root is my own value, then don't add any methods"
	rootPart=fullValuePath ifTrue: [^nil ].
	"myLabel may be a path that is some extension of label, or vice versa.  Compute
	 the real root part and real value path using the extension"
	d _ myLabel within: label.
	d notNil
		ifTrue: [realRootPart _ rootPart concat: d.
				realFullValuePath _ fullValuePath]
		ifFalse: [d _ label within: myLabel.
				realRootPart _ rootPart.
				realFullValuePath _ fullValuePath concat: d].
	"add a method for setting rootPart equal to the value I constrain"
	self addMethodToPlanner: planner context: context owner: owner
		receiver: realRootPart argument: realFullValuePath.
	"now a method for the other direction"
	self addMethodToPlanner: planner context: context owner: owner
		receiver: realFullValuePath argument: realRootPart! !

!LabelConstraint methodsFor: 'compiling'!
addMethodToPlanner: planner context: context owner: owner receiver: receiver argument: argument
	"auxiliary method for 'addLabelMethods' -- makes up an equality method to set
	 the receiver equal to the argument, and adds it to the planner"
		| encoder sel method testMethod descr |
	encoder _ planner encoder.
	sel _ ('primitiveSet.' , receiver lastName , ':') asSymbol.
	method _ MessageNode new
		receiver: (receiver allButLast code: encoder)
		selector: sel
		arguments: (Array with: (argument code: encoder))
		precedence: sel precedence
		from: encoder.
	"make up a test of the form  (receiver withinTolerance: argument)  "
	testMethod _ MessageNode new
		receiver: (receiver code: encoder)
		selector: #withinTolerance:
		arguments: (Array with: (argument code: encoder))
		precedence: #withinTolerance: precedence
		from: encoder.
	descr _ QueuedMethodDescription new
		context: context
		receiver: receiver
		constraint: self
		owner: owner
		uniqueState: true
		referenceOnly: false
		compileTimeOnly: false
		constraintMethod: method
		testMethod: testMethod.
	planner addMethod: descr.
	descr addMethods: planner! !

!LabelConstraint methodsFor: 'compiling'!
addMethods: planner context: context owner: owner receiver: receiver
		| label |
	"add my messages to the planner"
	(self overlaps: receiver) ifFalse: [^nil].
	label _ (labelPath applyTo: (owner applyTo: context)) asPath.
	"ignore null labels"
	label isEmpty ifTrue: [^nil].
	"see if my label has already been checked"
	(planner checkedLabels includes: label firstName) ifTrue: [^nil].
	planner checkedLabels add: label firstName.
	"wake up all label constraints for this label"
	context activateLabelConstraints: label
		context: context
		path: EmptyPath
		rootPart: (owner concat: valuePath)
		planner: planner! !

!LabelConstraint methodsFor: 'compiling'!
deleteFrom: prototype
	prototype deleteConstraint: self.
	prototype forgetConstraintMethods! !

!LabelConstraint methodsFor: 'compiling'!
deleteFrom: prototype ifOverlaps: path
	(self overlaps: path)
	  ifTrue: [self deleteFrom: prototype]! !

!LabelConstraint methodsFor: 'compiling'!
insertIn: prototype
	prototype addConstraint: self.
	prototype forgetConstraintMethods! !

!LabelConstraint methodsFor: 'compiling'!
methodDescriptions
	"return a method description for my value ... note that editing the
	 label is handled in a separate and kludgiferous way"
	^Array with: (ConstraintMethodDescription new
					receiver: valuePath
					uniqueState: true
					referenceOnly: false
					compileTimeOnly: false)! !

!LabelConstraint methodsFor: 'compiling'!
testCode: encoder owner: owner
	^encoder encodeVariable: 'true'! !

!LabelConstraint methodsFor: 'printing'!
printOn: strm
	labelPath printNamesOn: strm.
	strm nextPutAll: ' labels: '.
	valuePath printNamesOn: strm! !

!LabelConstraint methodsFor: 'access to parts'!
labelPath
	^labelPath! !

!LabelConstraint methodsFor: 'access to parts'!
valuePath
	^valuePath! !


!LabelConstraint class methodsFor: 'initialization'!
owner: owner label: labelString value: valueString
		| aConstraint |
	aConstraint _ self new
		setLabelPath: labelString asPath valuePath: valueString asPath.
	aConstraint insertIn: owner.
	^aConstraint! !


!MergeConstraint commentStamp: '<historical>' prior: 0!
A merge constraint means that the merged objects are really the same - not just equal.  'name' is nil.!

!MergeConstraint methodsFor: 'access to parts'!
paths
	^paths! !

!MergeConstraint methodsFor: 'access to parts'!
pathsGet: vec
	paths _ vec collect: [:each | each asPath]! !

!MergeConstraint methodsFor: 'tests and operations'!
= other
	^paths = other paths! !

!MergeConstraint methodsFor: 'tests and operations'!
hasPath: path
	^paths includes: path! !

!MergeConstraint methodsFor: 'tests and operations'!
overlaps: path
	 "return true if any of my paths overlap path"
	paths do:
		[:p | (p overlaps: path)
		  ifTrue: [^true]].
	^false! !

!MergeConstraint methodsFor: 'adding and deleting'!
deleteFrom: prototype
	prototype deleteMerge: self.
	prototype forgetConstraintMethods.
	self unmergePartsFor: prototype! !

!MergeConstraint methodsFor: 'adding and deleting'!
deleteFrom: prototype ifOverlaps: path
	(self overlaps: path)
	  ifTrue: [self deleteFrom: prototype]! !

!MergeConstraint methodsFor: 'adding and deleting'!
insertIn: prototype
	prototype addMerge: self.
	prototype forgetConstraintMethods.
	self mergePartsFor: prototype! !

!MergeConstraint methodsFor: 'adding and deleting'!
mergePartsFor: owner 
	"Replace all the old objects with the one at the end of the first path. 
	This assumes that I have been indexed in owner's merge table"
		| newPart |
	newPart _ (paths at: 1) applyTo: owner.
	2 to: paths size do: [:i | newPart _ newPart mergeWith: ((paths at: i) applyTo: owner)].
	owner replace: (paths at: 1) with: newPart! !

!MergeConstraint methodsFor: 'adding and deleting'!
unmergePartsFor: owner
	"This method assumes that all the merged parts were of the same class,
	and that I am not indexed in owner's merge table.  Also has a kludge to
	prevent blowing up if the merged object has already been deleted via
	another route.

	If the merged parts were not originally of the same class,
	should restore to instances of the original classes ... this isn't handled yet."
		| mergedObject |
	paths do:
		[:p | (p validPathFor: owner) ifTrue:
			[mergedObject _ p applyTo: owner.
			owner replace: p with: mergedObject recopy]]! !

!MergeConstraint methodsFor: 'compiling'!
addMethods: queue context: context owner: owner receiver: receiver
	"see if the receiver is an extension of one of my paths.  If so, make
	 up a MergeMessage for the extensions only, to prevent waking up
	 constraints unnecessarily"
	| extension message fullPaths |
	(self overlaps: receiver) ifTrue:
		[extension _ nil.
		paths do:
			[:p | extension==nil ifTrue: [extension _ receiver within: p]].
		extension==nil ifTrue: [extension _ EmptyPath].
		fullPaths _ paths collect:
			[:path | (owner concat: path) concat: extension].
		message _ MergeMessage new context: context paths: fullPaths constraint: self owner: owner.
		(queue hasMerge: message)
		  ifFalse:
			[queue addMerge: message.
			message addMethods: queue]]! !

!MergeConstraint methodsFor: 'printing'!
printOn: strm
	paths do:
		[:path | path names do:
			[:n | n printOn: strm.  strm space].
		strm nextPutAll: '== '].
	strm skip: -3! !

!MergeConstraint methodsFor: 'filout'!
mergeCodeTo: strm
	strm nextPutAll: 'self merge: '.
	paths do:
		[:path | strm nextPutAll: ''''.
		path names do:
			[:n | strm print: n.
			strm space].
		strm nextPutAll: '''']
	  andBetweenDo:
		[strm nextPutAll: ', '].
	strm nextPutAll: '.'.
	strm cr! !

!MergeConstraint methodsFor: 'filout'!
mergeCodeTo: strm for: owner
	strm nextPutAll: owner class name.
	strm nextPutAll: ' prototype merge: #('.
	paths do:
		[:path | strm space; nextPut: $'.
		path printNamesOn: strm.
		strm nextPut: $'].
	strm nextPutAll: ')!!'; cr; cr! !


!MergeMessage commentStamp: '<historical>' prior: 0!
Instance variables:
		context -- object in which method will be compiled
		paths -- paths to merged objects
		constraint -- pointer to the constraint which sends me
		owner -- path from context to owner of the constraint!

!MergeMessage methodsFor: 'access to parts'!
constraint | |
	^constraint! !

!MergeMessage methodsFor: 'access to parts'!
context | |
	^context! !

!MergeMessage methodsFor: 'access to parts'!
context: t1 paths: t2 constraint: t3 owner: t4 | |
	context _ t1.
	paths _ t2.
	constraint _ t3.
	owner _ t4! !

!MergeMessage methodsFor: 'access to parts'!
owner | |
	^owner! !

!MergeMessage methodsFor: 'access to parts'!
paths | |
	^paths! !

!MergeMessage methodsFor: 'compiling Methods'!
addMethods: queue
	"If any of my paths were edited, find all the constraints which
	 might no longer be satisfied.  Add the messages from such
	 constraints to the queue."
		| prefixPath suffixPath object pName |
	paths do:
		[:receiver |  
		object _ context.
		prefixPath _ EmptyPath.
		suffixPath _ receiver.
		[suffixPath isEmpty] whileFalse: 
			[pName _ suffixPath firstName.
			object constraints do: 
				[:c | c addMethods: queue context: context owner: prefixPath receiver: suffixPath].
			object merges do:
				[:m | m addMethods: queue context: context owner: prefixPath receiver: suffixPath].
			object _ object perform: pName.
			prefixPath _ prefixPath add: pName.
			suffixPath _ suffixPath tail]]! !

!MergeMessage methodsFor: 'printing'!
printOn: strm | |
	strm nextPutAll: 'Message for '.
	strm print: paths.
	strm nextPutAll: ' by '.
	strm print: constraint! !


!MergeSet commentStamp: '<historical>' prior: 0!
Merge sets hold a set of merge messages.!

!MergeSet methodsFor: 'accessing'!
allPathsTo: path | set |
	 "return an array of all paths to the object indicated by path"
	set _ Set new.
	self addPathsTo: path set: set.
	^set asOrderedCollection asArray! !

!MergeSet methodsFor: 'accessing'!
hasMerge: merge
	self do:
		[:m |
		((merge constraint = m constraint
		and: [merge owner = m owner])
		and: [merge paths = m paths])
		  ifTrue: [^true]].
	^false! !

!MergeSet methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: 'MergeSet'.
	self do:
		[:merge | strm cr; tab; print: merge]! !

!MergeSet methodsFor: 'private'!
addPathsTo: path set: set
	"add to set all paths to the object indicated by path"
	| paths suffixPath |
	(set includes: path) ifTrue: [^nil].
	set add: path.
	self do:
		[:merge | paths _ merge paths.
		suffixPath _ path withinOneOf: paths.
		suffixPath notNil ifTrue:
			"add the suffixPath to each merge path, and add to the set"
			[paths do: [:p | self addPathsTo: (p concat: suffixPath) set: set]]]! !


!MessagePlan commentStamp: '<historical>' prior: 0!
I describe a Smalltalk message that might be sent at run-time.

Instance variables:
	context -- object in which method will be compiled
	receiver -- path from context to ultimate receiver
	constraint -- pointer to the constraint which sends me, or nil
	owner -- path from context to owner of the constraint, or 
			EmptyPath if constraint is nil.
			If not nil, message is actually sent to the owner
	keywords -- vector of keywords for message (strings)
	arguments -- vector of arguments for message (strings)
	uniqueState -- true iff only one state of my receiver
			will satisfy my constraint
	referenceOnly -- true iff my receiver is only referenced
	compileTimeOnly!

!MessagePlan methodsFor: 'access to parts'!
arguments | |
	^arguments! !

!MessagePlan methodsFor: 'access to parts'!
argumentsGet: t1 | |
	arguments _ t1! !

!MessagePlan methodsFor: 'access to parts'!
compileTimeOnly | |
	^compileTimeOnly! !

!MessagePlan methodsFor: 'access to parts'!
compileTimeOnlyGet: t1 | |
	compileTimeOnly _ t1! !

!MessagePlan methodsFor: 'access to parts'!
constraint | |
	^constraint! !

!MessagePlan methodsFor: 'access to parts'!
context | |
	^context! !

!MessagePlan methodsFor: 'access to parts'!
context: t1 receiver: t2 constraint: t3 owner: t4 keywords: t5 arguments: t6 uniqueState: t7 referenceOnly: t8 compileTimeOnly: t9 | |
	context _ t1.
	receiver _ t2.
	constraint _ t3.
	owner _ t4.
	keywords _ t5.
	arguments _ t6.
	uniqueState _ t7.
	referenceOnly _ t8.
	compileTimeOnly _ t9! !

!MessagePlan methodsFor: 'access to parts'!
contextGet: t1 | |
	context _ t1! !

!MessagePlan methodsFor: 'access to parts'!
fromString: s | strm token path a |
         "handles some simple cases"
        strm _ s asTokens readStream.
        path _ OrderedCollection new.
        token _ strm next.
        [(token isMemberOf: Symbol) and: [token numArgs = 0]] whileTrue:
                [path addLast: token.
                token _ strm next].
        token == nil  ifTrue:
                [token _ path removeLast.
                receiver _ path asArray asPath.
                keywords _ Array with: token.
                arguments _ Array new: 0.
                ^self setFlags: token].

        ((token isMemberOf: Symbol) and: [token numArgs = 1])
                  ifFalse: [self error: 'string not of proper form'].
        receiver _ path asArray asPath.
        owner _ EmptyPath.
        keywords _ Array with: token.
        a _ WriteStream on: (String new: 200).
        [strm atEnd] whileFalse:
                [token _ strm next.
                a print: token.
                a space].
        arguments _ Array with: a contents.
        uniqueState _ true.
        referenceOnly _ compileTimeOnly _ false.! !

!MessagePlan methodsFor: 'access to parts'!
keywords | |
	^keywords! !

!MessagePlan methodsFor: 'access to parts'!
keywordsGet: t1 | |
	keywords _ t1! !

!MessagePlan methodsFor: 'access to parts'!
owner | |
	^owner! !

!MessagePlan methodsFor: 'access to parts'!
receiver | |
	^receiver! !

!MessagePlan methodsFor: 'access to parts'!
receiverGet: t1 | |
	receiver _ t1! !

!MessagePlan methodsFor: 'access to parts'!
receivingObject | |
	^receiver applyTo: context! !

!MessagePlan methodsFor: 'access to parts'!
referenceOnly | |
	^referenceOnly! !

!MessagePlan methodsFor: 'access to parts'!
referenceOnlyGet: t1 | |
	referenceOnly _ t1! !

!MessagePlan methodsFor: 'access to parts'!
setFlags: token 
	token = #reference ifTrue: 
			[uniqueState _ false.
			referenceOnly _ true.
			compileTimeOnly _ true.
			^self].
	token = #check ifTrue: 
			[uniqueState _ false.
			referenceOnly _ true.
			compileTimeOnly _ true.
			^self].
	token = #alter ifTrue: 
			[uniqueState _ false.
			referenceOnly _ false.
			compileTimeOnly _ false.
			^self].
	token = #fixed ifTrue: 
			[uniqueState _ true.
			referenceOnly _ false.
			compileTimeOnly _ true.
			^self].
	self error: 'unknown keyword'! !

!MessagePlan methodsFor: 'access to parts'!
uniqueState | |
	^uniqueState! !

!MessagePlan methodsFor: 'access to parts'!
uniqueStateGet: t1 | |
	uniqueState _ t1! !

!MessagePlan methodsFor: 'hashing and tests'!
= m
	"for looking up methods in method dictionaries and building message queues"
	^(receiver = m receiver  and: [keywords = m keywords]) and: 
		[(owner = m owner and: [constraint = m constraint])]! !

!MessagePlan methodsFor: 'hashing and tests'!
hash 
		| h |
	h _ receiver hash.
	keywords do: [:k | h _ h bitXor: k hash].
	^h! !

!MessagePlan methodsFor: 'hashing and tests'!
sameAs: message receiverPaths: receiverPaths
	"return true if I am the same as message.
	ReceiverPaths is an array of all paths to its receiver"
	message owner = owner ifFalse: [^false].
	message constraint = constraint ifFalse: [^false].
	message keywords = keywords ifFalse: [^false].
	message arguments = arguments ifFalse: [^false].
	(receiverPaths includes: receiver) ifTrue: [^true].
	^false! !

!MessagePlan methodsFor: 'compiling methods'!
addMethods: planner
	"If my receiver were edited, find all the constraints which might no
	 longer be satisfied.  Add the messages from such constraints to the planner's queue."
		| prefixPath suffixPath object pName |
	object _ context.
	prefixPath _ EmptyPath.
	suffixPath _ receiver.
	[suffixPath isEmpty] whileFalse: 
		[pName _ suffixPath firstName.
		object constraints do:
			[:c | c addMethods: planner context: context owner: prefixPath receiver: suffixPath].
		object merges do:
			[:m | m addMethods: planner context: context owner: prefixPath receiver: suffixPath].
		object _ object perform: pName.
		prefixPath _ prefixPath add: pName.
		suffixPath _ suffixPath tail]! !

!MessagePlan methodsFor: 'compiling methods'!
canBePostponed: queue
	"return true if I can be sent last.  To be sent last, neither my receiver
		nor any of its parts can be unalterable.  Also, either I must be the
		only message to my receiver, or all messages to my receiver must
		come from the same constraint and owner."
	(queue testForUnalterablePart: receiver) ifTrue: [^false].
	(queue findReceiver: receiver) do:
		[:m | self == m
		  ifFalse:
			[constraint == nil
			  ifTrue: [^false].
			(constraint = m constraint and: [owner = m owner])
			  ifFalse: [^false]]].
	^true! !

!MessagePlan methodsFor: 'compiling methods'!
canBeSentNext: queue | path |
	 "return true if I can be sent next"
	"The following conditions must be true for me to be sent next:
		my constraint uniquely determines the state of my receiver;
		my receiver or one of its parts isn't already unalterable;
		and all the objects that I reference are unalterable.
		Or: (kludge) If I am sent by a SetMembershipConstraint."
	(constraint isMemberOf: SetMembershipConstraint) ifTrue: [^true].
	uniqueState == false ifTrue: [^false].
	(queue testForUnalterablePart: receiver) ifTrue: [^false].
	constraint == nil ifTrue: [^true].
	 "If any of the objects that I reference may still change, return false"
	constraint messages do:
		[:message | path _ owner concat: message receiver.
		path = receiver
		  ifFalse:
			[ "it's the one I alter"
			(queue testIfUnalterable: path)
			  ifFalse:
				[ "last chance!!  If this message is reference only, and there are 
			no altering messages to it in the queue, it won't change."
				(message referenceOnly and: [(queue noAlteringMessagesTo: path)])
				  ifFalse: [^false]]]].
	"Kludge!!  If I am sent by a set constraint, check actual messages in queue"
	(constraint isKindOf: SetConstraint)
	  ifTrue: [(queue findConstraint: constraint owner: owner) do:
			[:message | message == self
			  ifFalse:
				[(queue testIfUnalterable: message receiver)
				  ifFalse: [^false]]]].
	^true! !

!MessagePlan methodsFor: 'compiling methods'!
checked: queue 
	"Delete myself from the queue.  Since I will check my constraint, 
	none of the other messages from my constraint need to be sent."
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
					do: [:m | queue removeMethod: m]]! !

!MessagePlan methodsFor: 'compiling methods'!
code: encoder
		| sel |
	compileTimeOnly ifTrue: [^encoder encodeVariable: 'nil'].
	sel _ self selector.
	^MessageNode new
		receiver: (receiver code: encoder)
		selector: sel
		arguments: (arguments collect: [:a | encoder encodeVariable: a])  "is this right???"
		precedence: sel precedence
		from: encoder! !

!MessagePlan methodsFor: 'compiling methods'!
longSelector 
	"return a new selector composed of my receiver path names before my keywords"
		| strm |
	strm _ WriteStream on: (String new: 50).
	receiver names do: [:n | strm nextPutAll: n].
	keywords do: [:k | strm nextPutAll: k].
	^strm contents asSymbol! !

!MessagePlan methodsFor: 'compiling methods'!
makeMethod: statements temps: temps encoder: encoder
	"return a method for editing my receiver"
		| sel argNodes returnNode returningStatements blockNode |
	sel _ self longSelector.
	argNodes _ arguments collect: [:a | encoder encodeVariable: a].
	"add a ^self at the end of the method"
	returnNode _ ReturnNode new expr: (encoder encodeVariable: 'self').
	returningStatements _ statements copyWith: returnNode.
	blockNode _ BlockNode new statements: returningStatements returns: true.
	^MethodNode new
		selector: sel
		arguments: argNodes
		precedence: sel precedence
		temporaries: #()
		block: blockNode
		encoder: encoder
		primitive: 0! !

!MessagePlan methodsFor: 'compiling methods'!
noBackPointerCopy
	 "return a copy of me without backpointers"
	^MessagePlan new
		context: nil
		receiver: receiver
		constraint: nil
		owner: EmptyPath
		keywords: keywords
		arguments: arguments
		uniqueState: uniqueState
		referenceOnly: referenceOnly
		compileTimeOnly: compileTimeOnly! !

!MessagePlan methodsFor: 'compiling methods'!
postponed: queue 
	"Delete myself from the queue.  Since I have been postponed, 
	none of the other messages from my constraint need to be sent. 
	Nothing needs to be done with unalterable or semialterable, 
	since the receiver of this message doesnt affect anyone else 
	(I hope ...)"
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
				do: [:m | queue removeMethod: m]]! !

!MessagePlan methodsFor: 'compiling methods'!
selector | strm |
	strm _ WriteStream on: (String new: 50).
	keywords do: [:k | strm nextPutAll: k].
	^strm contents asSymbol! !

!MessagePlan methodsFor: 'compiling methods'!
sentNext: queue 
	"Delete myself from the queue.  Since I have been sent, none of the other 
	messages from my constraint need to be sent.  If my constraint must be 
	satisfied, alteration of my receiver.  KLUDGE - this is currently done by 
	seeing if the keyword is moveby: .  Eventually, the constraints should 
	indicate this.  Some possible classifications are:    
	must be satisfied satisfy if possible (but not an error if not satisfied - 
	e.g., moveby:) minimize sum of squares of errors ( as in bridges)"
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
				do: [:m | queue removeMethod: m]].
	"eventually, in making my receiver unalterable or semialterable, the 
	queue should check if several unalterable subparts can be combined into 
	a larger unalterable part.  KLUDGE!! decide whether receiver is 
	unalterable or semialterable based on keywords.  Eventually, this should 
	be specified by an instance variable."
	( #( 'moveby:' 'primitiveMoveby:' 'changeValue:' ) includes: (keywords at: 1))
		ifTrue: [queue makeSemialterable: receiver]
		ifFalse: [queue makeUnalterable: receiver]! !

!MessagePlan methodsFor: 'compiling methods'!
shouldBePostponed: queue
	"return true if I should be sent last.  I should be sent last if I can be
	 sent last, and if my receiver or one of its parts isn't semialterable"
	(queue testForSemialterablePart: receiver) 
	  ifTrue: [^false].
	^self canBePostponed: queue! !

!MessagePlan methodsFor: 'printing'!
printOn: strm
	strm nextPutAll: 'MessagePlan for ('.
	strm print: context.
	strm nextPut: $).
	receiver names do: [:n | strm space; nextPutAll: n].
	strm nextPutAll: ' by '.
	strm print: constraint! !


!ObjectDefinerView class methodsFor: 'initialization' stamp: 'di 1/21/1999 20:01'!
open		"Create and schedule a new ObjectDefinerView."

	"ObjectDefinerView open"
	| topView browser |
	browser _ ObjectDefiner new.
	topView _ self new
				model: browser;
				label: 'ThingLab Object Definer';
				minimumSize: 300 @ 200.
	topView addSubView:
			((PluggableListView on: browser list: #prototypeClassList
				selected: #prototypeClassListIndex changeSelected: #prototypeClassListIndex:
				menu: #prototypeClassListMenu)
					window: (0@0 extent: 75@50)).
	topView addSubView:
			((PluggableListView on: browser list: #aspectList
				selected: #aspectListIndex changeSelected: #aspectListIndex:
				menu: #aspectListMenu)
					window: (75@0 extent: 75@50)).
	topView addSubView:
			((PluggableListView on: browser list: #toolList
				selected: #toolListIndex changeSelected: #toolListIndex:
				menu: #toolListMenu)
					window: (150@0 extent: 75@50)).
	topView addSubView:
			((PluggableListView on: browser list: #filterList
				selected: #filterListIndex changeSelected: #filterListIndex:
				menu: #filterListMenu)
					window: (225@0 extent: 75@50)).
	topView addSubView:
			((PluggableTextView on: browser text: #text accept: #acceptText:from:
				readSelection: nil menu: #textMenu)
					window: (0@50 extent: 300@90)).
	topView addSubView:
			((PluggableTextView on: browser text: #instanceText accept: #acceptText:from:
				readSelection: nil menu: #textMenu)
					window: (0@140 extent: 300@60)).
	topView controller open! !


!PartDescription commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!PartDescription methodsFor: 'testing'!
isPart
	^ true! !

!PartDescription methodsFor: 'compiling'!
makePrimitiveSetNode: newValueNode sample: sample encoder: encoder
	"return a message node of the form
			myname primitiveSet.field1: newValue field1 field2: newValue field2 	"

		| sampleFields args selString sel |
	"check for a nil sample (if it's a part, it shouldn't  be nil)"
	sample==nil ifTrue: [self error: 'field not initialized'].
	sampleFields _ sample fieldDescriptions reject: [:f | f isSpare].
	args _ sampleFields collect: [:f | MessageNode new
		receiver: newValueNode
		selector: f name
		arguments: #()
		precedence: f name precedence
		from: encoder].
	selString _ 'primitiveSet.' .
	sampleFields do:
		[:f | selString _ selString , f name , ':'].
	sel _ selString asSymbol.
	^MessageNode new
		receiver: (encoder encodeVariable: name asString)
		selector: sel
		arguments: args
		precedence: sel precedence
		from: encoder! !

!PartDescription methodsFor: 'compiling'!
makeRetrieveNode: encoder
	^encoder encodeVariable: name asString! !


!PictureController methodsFor: 'control defaults'!
controlActivity
	| tool | 
	super controlActivity.
	tool _ model toolObject.
	tool == nil ifFalse:
		[tool context: model context
			editedObject: view viewedObject
			editedObjectPath: view viewedObjectPath
			editedView: view
			otherViews: (view superView viewsOtherThan: view)
			carrierShowSelector: model showSelector.
		tool startUp]! !

!PictureController methodsFor: 'control defaults'!
isControlActive
	Sensor blueButtonPressed ifTrue: [^false].
	^ super isControlActive! !


!PictureView methodsFor: 'viewed object'!
viewedObject
	^model perform: aspect! !

!PictureView methodsFor: 'viewed object'!
viewedObjectPath
	"return a path from the context to the part being edited"
	"kludge - this is currently specialized to work only with the prototype itself or the instance"
		| inst |
	inst _ 'instance'.
	(aspect size> inst size and: [(aspect copyFrom: 1 to: inst size) = inst])
		ifTrue: [^#(instance) asPath]
		ifFalse: [^EmptyPath]! !

!PictureView methodsFor: 'displaying'!
cursorlocGet: pt
	Sensor cursorPoint: insetDisplayBox origin + pt! !

!PictureView methodsFor: 'displaying' stamp: 'di 1/21/1999 19:18'!
displayView
	"this is the license version of displayViewOn: "
		| obj box |
	obj _ self viewedObject.
	obj == nil
		ifFalse: [box _ self insetDisplayBox.  "force newBuffer if necess"
				buffer fillWhite.
				obj showPicture: buffer.
				self copyFromBuffer]! !

!PictureView methodsFor: 'displaying'!
displayViewOn: aMedium
	| obj box | 
	obj _ self viewedObject.
	obj == nil
		ifFalse: [box _ self insetDisplayBox.  "force newBuffer if necess"
				buffer white.
				obj showPicture: buffer.
				buffer displayOn: aMedium at: box origin]! !

!PictureView methodsFor: 'updating'!
update: aSymbol
	aSymbol == aspect
		ifTrue: [self displayView]
		ifFalse: [super update: aSymbol]! !

!PictureView methodsFor: 'buffering'!
copyFromBuffer
	buffer displayOn: Display at: self insetDisplayBox origin! !

!PictureView methodsFor: 'buffering'!
copyToBuffer! !

!PictureView methodsFor: 'buffering'!
insetDisplayBox
	insetDisplayBox == nil
		ifTrue: [self newBuffer].		"New buffer and pen when change size"
	^ insetDisplayBox! !

!PictureView methodsFor: 'buffering'!
newBuffer 
	| newBox | 
	newBox _ super insetDisplayBox.
	buffer _ Form extent: newBox extent.! !

!PictureView methodsFor: 'buffering' stamp: 'di 1/23/1999 05:12'!
topMedium
		| box | 
	box _ self insetDisplayBox.
	^ DisplayRegionHack new
				clippingRectangle: box
				translation: box origin! !

!PictureView methodsFor: 'buffering' stamp: 'di 1/21/1999 20:17'!
updateBuffer
	buffer fillWhite.
	self viewedObject showPicture: buffer! !

!PictureView methodsFor: 'aspect testing'!
viewsDefinition
	"aspect will be #picture if I'm looking at the definition, and will be #instancePicture if
		I'm looking at the instance"
	^aspect==#picture! !

!PictureView methodsFor: 'private'!
defaultControllerClass
	^ PictureController! !

!PictureView methodsFor: 'private' stamp: 'di 1/21/1999 19:17'!
on: anObject aspect: aspectSymbol
	self model: anObject.
	aspect _ aspectSymbol.
	changeMsg _ (aspectSymbol , ':') asSymbol.
	menuMsg _ (aspectSymbol , 'Menu') asSymbol.
	self insideColor: Color white.
	self initialize! !


!PictureView class methodsFor: 'instance creation'!
on: anObject aspect: aspectSymbol
	"Create a 'pluggable' View viewing a DisplayObject.
	aspectSymbol is sent to read the displayObject value in the model.
	It is also concatenated with ':' and 'Menu' to yield change and
	menu messages for this view of the model."

	^ self new on: anObject aspect: aspectSymbol! !


!PrimitiveDescription methodsFor: 'testing'!
isPrimitive
	^ true! !

!PrimitiveDescription methodsFor: 'compiling'!
makePrimitiveSetNode: newValueNode sample: sample encoder: encoder
	"return a message node of the form
			myname _ newValue
	"

	^AssignmentNode new
		variable: (encoder encodeVariable: name asString)
		value: newValueNode! !

!PrimitiveDescription methodsFor: 'compiling'!
makeRetrieveNode: encoder
	^encoder encodeVariable: name asString! !


!QueuedMethodDescription commentStamp: '<historical>' prior: 0!
I describe a constraint satisfaction method ... I'm used in MethodQueues

Instance Variables:
	context	<Object> -- object in which the constraint satisfaction method will be compiled
	constraint	<Constraint> -- constraint that has my method
	owner	<AccessPath> -- path from context to owner of the constraint
	constraintMethod	<MessageNode> -- the method in the constraint that I describe,
											relocated for owner
!

!QueuedMethodDescription methodsFor: 'initialization'!
context: ct receiver: r constraint: constr owner: o uniqueState: uState referenceOnly: refOnly compileTimeOnly: cOnly constraintMethod: m testMethod: tm

	context _ ct.
	receiver _ r.
	constraint _ constr.
	owner _ o.
	uniqueState _ uState.
	referenceOnly _ refOnly.
	compileTimeOnly _ cOnly.
	constraintMethod _ m.
	testMethod _ tm! !

!QueuedMethodDescription methodsFor: 'access to parts'!
constraint
	^constraint! !

!QueuedMethodDescription methodsFor: 'access to parts'!
constraintMethod
	^constraintMethod! !

!QueuedMethodDescription methodsFor: 'access to parts'!
context
	^context! !

!QueuedMethodDescription methodsFor: 'access to parts'!
owner
	^owner! !

!QueuedMethodDescription methodsFor: 'access to parts'!
receivingObject
	^receiver applyTo: context! !

!QueuedMethodDescription methodsFor: 'tests'!
= q
	"for looking up methods in method dictionaries and building message queues"
	^(receiver = q receiver  and: [constraintMethod = q constraintMethod]) and: 
		[(owner = q owner and: [constraint = q constraint])]! !

!QueuedMethodDescription methodsFor: 'tests'!
sameAs: descr receiverPaths: receiverPaths
	"return true if I am the same as descr.
	ReceiverPaths is an array of all paths to its receiver"
	descr owner = owner ifFalse: [^false].
	descr constraint = constraint ifFalse: [^false].
	descr constraintMethod = constraintMethod ifFalse: [^false].
	(receiverPaths includes: receiver) ifTrue: [^true].
	^false! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
addMethods: queue
	"If my receiver were edited, find all the constraints which might no
	 longer be satisfied.  Add the methods from such constraints
	 to the queue."
		| prefixPath suffixPath object pName |
	object _ context.
	prefixPath _ EmptyPath.
	suffixPath _ receiver.
	[suffixPath isEmpty] whileFalse: 
		[pName _ suffixPath firstName.
		object constraints do:
			[:c | c addMethods: queue context: context owner: prefixPath receiver: suffixPath].
		object merges do:
			[:m | m addMethods: queue context: context owner: prefixPath receiver: suffixPath].
		object _ object perform: pName.
		prefixPath _ prefixPath add: pName.
		suffixPath _ suffixPath tail]! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
canBePostponed: queue
	"return true if I can be sent last.  To be sent last, neither my receiver
		nor any of its parts can be unalterable.  Also, either I must be the
		only method applying to my receiver, or all methods applying to my receiver must
		come from the same constraint and owner."
	(queue testForUnalterablePart: receiver) ifTrue: [^false].
	(queue findReceiver: receiver) do:
		[:m | self == m
		  ifFalse:
			[constraint == nil
			  ifTrue: [^false].
			(constraint = m constraint and: [owner = m owner])
			  ifFalse: [^false]]].
	^true! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
canBeSentNext: queue
	"return true if I can be sent next"
	"The following conditions must be true for me to be sent next:
	 my constraint uniquely determines the state of my receiver;
	 my receiver or one of its parts isn't already unalterable;
	 and all the objects that I reference are unalterable. 
	 Or: (kludge) if I am sent by a SetMembershipConstraint."
		| path |
	(constraint isMemberOf: SetMembershipConstraint) ifTrue: [^true].
	uniqueState ifFalse: [^false].
	(queue testForUnalterablePart: receiver) ifTrue: [^false].
	constraint == nil ifTrue: [^true].
	 "If any of the objects that I reference may still change, return false"
	constraint methodDescriptions do:
		[:descr | path _ owner concat: descr receiver.
		path = receiver ifFalse:
			["it's not the one I alter"
			(queue testIfUnalterable: path) ifFalse:
				["If this description is reference only, and there are 
				no altering descriptions to it in the queue, it won't change."
				(descr referenceOnly and: [(queue noAlteringMessagesTo: path)])
				  ifFalse: [^false]]]].
 	"Kludge!!  If I am sent by a set or label constraint, check actual descriptions in queue"
	((constraint isKindOf: SetConstraint) or: [constraint isKindOf: LabelConstraint])
	  ifTrue: [(queue findConstraint: constraint owner: owner) do:
			[:descr | descr == self
			  ifFalse:
				[(queue testIfUnalterable: descr receiver)
				  ifFalse: [^false]]]].
	^true! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
checked: queue 
	"Delete myself from the queue.  Since I will check my constraint, 
	none of the other methods from my constraint need to be used"
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
					do: [:m | queue removeMethod: m]]! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
code: encoder
	"return a method for satisfying my constraint (in parse tree form)
	 that can be installed in owner"
	compileTimeOnly
		ifTrue: [^encoder encodeVariable: 'nil']
		ifFalse: [^constraintMethod recode: encoder]! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
postponed: queue 
	"Delete myself from the queue.  Since I have been postponed, 
	none of the other methods for my constraint need to be used. 
	Nothing needs to be done with unalterable or semialterable, 
	since the receiver of this message doesnt affect anyone else 
	(I hope ...)"
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
				do: [:m | queue removeMethod: m]]! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
sentNext: queue 
	"Delete myself from the queue.  Since I have been sent, none of the other 
	methods for my constraint need to be used.  If my constraint must be 
	satisfied, then add my receiver to unalterable.  Otherwise, discourage
	alteration of my receiver.  KLUDGE - this is currently done by 
	seeing if the keyword is moveby: .  Eventually, the constraints should 
	indicate this.  Some possible classifications are:    
		must be satisfied
		satisfy if possible (but not an error if not satisfied - e.g., moveby:)
		minimize sum of squares of errors ( as in bridges)"
	constraint == nil
		ifTrue: [queue removeMethod: self]
		ifFalse: [(queue findConstraint: constraint owner: owner)
				do: [:m | queue removeMethod: m]].
	"eventually, in making my receiver unalterable or semialterable, the 
	queue should check if several unalterable subparts can be combined into 
	a larger unalterable part.  KLUDGE!! decide whether receiver is 
	unalterable or semialterable based on keywords.  Eventually, this should 
	be specified by an instance variable."

	"FIX THIS UP -- old code:

	(keywords = #('moveby:' ) or: [keywords = #('changeValue:' )])
		ifTrue: [queue makeSemialterable: receiver]
		ifFalse: [queue makeUnalterable: receiver]

	also see MessagePlan sentNext:

	"

	queue makeUnalterable: receiver! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
shouldBePostponed: queue
	"return true if I should be sent last.  I should be sent last if I can be
		sent last, and if my receiver or one of its parts isn't semialterable"
	(queue testForSemialterablePart: receiver)
		ifTrue: [^false]
		ifFalse: [^self canBePostponed: queue]! !

!QueuedMethodDescription methodsFor: 'compiling methods'!
testCode: encoder
	"return a method (in parse tree form) to test whether my constraint is satisfied"
	compileTimeOnly ifTrue:
		[self error: 'I am compile time only'.
		"if the user chooses to proceed, return a vacuous test"
		^encoder encodeVariable: 'true'].
	^testMethod recode: encoder! !


!RelativeReference commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!RelativeReference methodsFor: 'testing'!
isRelativeReference
	^ true! !

!RelativeReference methodsFor: 'compiling'!
relocatePathFor: obj prefix: prefix
	"prefix the path in my field with prefix"
	obj instVarAt: index put: (prefix concat: (obj instVarAt: index))! !


!Relaxer commentStamp: '<historical>' prior: 0!
This is the relaxation method for solving simultaneous constraints.  Instances of this class are used both at compilation time and execution time.  At compilation time, all the instance variables will use paths to objects rather than pointers.

During execution, an iteration consists of computing some new contents for each object to satisfy its constraints as well as possible.  In this version, the constraints are declared unsatisfiable if the sum of the squares of the errors changes by less than minChange after an iteration.  Relaxation stops when all the constraints are satisfied.  Ugh.

Instance variables:
	context -- object which creates this relaxer
	objectOwners  -- a collection of the immediate owners of the objects being relaxed
	retrieveSelectors -- a collection of selectors for retrieving the objects being relaxed
						from their corresponding objectOwner
	primitiveSetSelectors -- a collection of selectors for setting the value of the objects being relaxed
						(sent to the corresponding objectOwner)
	constraintOwnerVecs -- collection of collection of owners of constraints
	errorVecs -- collection of collection of selectors for constraint error message
	testVecs -- collection of collection of selectors for constraint satisfaction test message
	methods -- collection of selectors for methods to
			be executed after each change to an object owned by 1 of objectOwners
	objectOwner retrieveSelector primitiveSetSelector constraintOwners errors tests method --
			particular elements of above collections under consideration
	showImage -- true iff image should be updated after each cycle!

!Relaxer methodsFor: 'compiling'!
circularMethod: c planner: planner
	"return code to invoke the relaxation method"
		| unknown subpartPath |
	context _ c.
	objectOwners _ OrderedCollection new. 
	retrieveSelectors _ OrderedCollection new. 
	primitiveSetSelectors _ OrderedCollection new. 
	constraintOwnerVecs _ OrderedCollection new.
	errorVecs _ OrderedCollection new.
	testVecs _ OrderedCollection new.
	methods _ OrderedCollection new.
	unknown _ planner pickUnknown.
	"find the largest container of the unknown that can be relaxed"
	[unknown==nil] whileFalse:
		[subpartPath _ context largestRelaxablePart: unknown.
		objectOwner _ subpartPath allButLast.
		retrieveSelector _ subpartPath names last.
		primitiveSetSelector _ ('primitiveSet.' , retrieveSelector , ':') asSymbol.
		self propagate: planner.
		unknown _ planner pickUnknown].
	^self relaxationCode: planner! !

!Relaxer methodsFor: 'compiling'!
compileError: message encoder: encoder
	"compile an error method for message's constraint, and return the selector"
		| sel returnNode blockNode methodNode |
	sel _ context class generateSelector: 'error'.
	returnNode _ ReturnNode new 
		expr: (message constraint errorCode: encoder owner: message owner).
	blockNode _ BlockNode new
		statements: (Array with: returnNode)
		returns: true.
	methodNode _ MethodNode new
		selector: sel
		arguments: #()
		precedence: sel precedence
		temporaries: #()
		block: blockNode
		encoder: encoder
		primitive: 0.
	methodNode compileIn: context class.
	^sel! !

!Relaxer methodsFor: 'compiling'!
compileTest: message encoder: encoder
	"compile a test for message's constraint, and return the selector"
		| sel returnNode blockNode methodNode |
	sel _ context class generateSelector: 'test'.
	returnNode _ ReturnNode new expr: (message testCode: encoder).
	blockNode _ BlockNode new
		statements: (Array with: returnNode)
		returns: true.
	methodNode _ MethodNode new
		selector: sel
		arguments: #()
		precedence: sel precedence
		temporaries: #()
		block: blockNode
		encoder: encoder
		primitive: 0.
	methodNode compileIn: context class.
	^sel! !

!Relaxer methodsFor: 'compiling'!
pickErrors: planner oldUnalterable: oldUnalterable
	"Find the vectors of constraint owners, errors and tests.
	 Delete these messages from the queue.
	 Choose only one error and test per constraint"
		| picked |
	picked _ ConstraintSatisfactionPlanner for: context.
	constraintOwners _ OrderedCollection new.
	errors _ OrderedCollection new.
	tests _ OrderedCollection new.
	planner unalterable do:
		[:path | (oldUnalterable includes: path)
		  ifFalse: [(planner findReceiver: path) do:
				[:message | planner removeMethod: message.
				(picked hasConstraint: message constraint owner: message owner)
				  ifFalse:
					["delete messages from this constraint to objects in oldUnalterable,
				since the errors for their constraint will be taken care of by the relaxer"
					picked addMethod: message.
					constraintOwners add: message owner.
					errors add: (self compileError: message encoder: planner encoder).
					tests add: (self compileTest: message encoder: planner encoder).
					(planner findConstraint: message constraint owner: message owner) do:
						[:m | (oldUnalterable includes: m receiver)
						  ifTrue: [planner removeMethod: m]]]]]].
	constraintOwnerVecs add: constraintOwners asArray.
	errorVecs add: errors asArray.
	testVecs add: tests asArray! !

!Relaxer methodsFor: 'compiling'!
propagate: planner
		| statements returnNode returningStatements blockNode methodNode sel oldUnalterable |
	objectOwners add: objectOwner.
	retrieveSelectors add: retrieveSelector.
	primitiveSetSelectors add: primitiveSetSelector.
	"save old vector of unalterable paths"
	oldUnalterable _ planner unalterable.
	planner makeUnalterable: (objectOwner add: retrieveSelector).
	statements _ context workForwards: planner.
	statements isEmpty
	  ifTrue: [methods add: #none]
	  ifFalse:
		["create a method, compile it, and save the selector"
		sel _ context class generateSelector: 'relax'.
		"add a ^self at the end of the method"
		returnNode _ ReturnNode new expr: (planner encoder encodeVariable: 'self').
		returningStatements _ statements copyWith: returnNode.
		blockNode _ BlockNode new statements: returningStatements returns: true.
		methodNode _ MethodNode new
			selector: sel
			arguments: #()
			precedence: sel precedence
			temporaries: #()
			block: blockNode
			encoder: planner encoder
			primitive: 0.
		methodNode compileIn: context class.
		methods add: sel].
	"pick errors to be considered"
	self pickErrors: planner oldUnalterable: oldUnalterable.
	"restore vector of unalterable paths"
	planner unalterable: oldUnalterable! !

!Relaxer methodsFor: 'compiling'!
relaxSelector
	"return the selector to use when invoking me during execution -
		should be either #relax or #relaxQuiet"
	objectOwners size = 1
		ifTrue: [^#relaxQuiet]
		ifFalse: [^#relax]! !

!Relaxer methodsFor: 'compiling'!
relaxationCode: planner
	"If the number of objects to be relaxed is 0, then don't generate any code.
	 This may occur if there are constraints that need to be checked."
		| nObjects nConstraints encoder objectOwnerPathArray constraintOwnerPathArray statements 
			relaxerTemp assignRelaxerNode initArgs initRelaxerNode doItNode |
	nObjects _ objectOwners size.
	nObjects = 0 ifTrue: [^#()].
	Transcript cr; show: 'WARNING - relaxation method being compiled in ' , context class name.
	encoder _ planner encoder.
	relaxerTemp _ encoder bindTemp: 'relaxer'.
	objectOwnerPathArray _ Array new: nObjects.
	constraintOwnerPathArray _ Array new: nObjects.
	1 to: nObjects do:
		[:i | objectOwnerPathArray at: i put: (objectOwners at: i) names.
		nConstraints _ (constraintOwnerVecs at: i) size.
		constraintOwnerPathArray at: i put: (Array new: nConstraints).
		1 to: nConstraints do: [:j |
			(constraintOwnerPathArray at: i) at: j put: ((constraintOwnerVecs at: i) at: j) names]].
	errorVecs _ errorVecs asArray.
	testVecs _ testVecs asArray.
	methods _ methods asArray.
	statements_ OrderedCollection new.
	assignRelaxerNode _ AssignmentNode new
		variable: relaxerTemp
		value: (MessageNode new
			receiver: (encoder encodeVariable: 'self')
			selector: #newRelaxer
			arguments: Array new
			precedence: #newRelaxer precedence
			from: encoder).
	statements add: assignRelaxerNode.
	initArgs _ OrderedCollection new.
	initArgs add: (encoder encodeVariable: 'self').   " context: "
	initArgs add: (encoder encodeLiteral: objectOwnerPathArray). 
	initArgs add: (encoder encodeLiteral: retrieveSelectors asArray). 
	initArgs add: (encoder encodeLiteral: primitiveSetSelectors asArray). 
	initArgs add: (encoder encodeLiteral: constraintOwnerPathArray).
	initArgs add: (encoder encodeLiteral: errorVecs).
	initArgs add: (encoder encodeLiteral: testVecs).
	initArgs add: (encoder encodeLiteral: methods).
	initRelaxerNode _ MessageNode new
		receiver: relaxerTemp
		selector: #context:objectOwners:retrieveSelectors:primitiveSetSelectors:constraintOwnerVecs:errorVecs:testVecs:methods:
		arguments: initArgs
		precedence: #context:objectOwners:retrieveSelectors:primitiveSetSelectors:constraintOwnerVecs:errorVecs:testVecs:methods: precedence
		from: encoder.
	statements add: initRelaxerNode.
	doItNode _ MessageNode new
		receiver: relaxerTemp
		selector: self relaxSelector
		arguments: #()
		precedence: self relaxSelector precedence
		from: encoder.
	statements add: doItNode.
	^statements! !

!Relaxer methodsFor: 'execution initialization'!
context: c objectOwners: objectOwnerPaths retrieveSelectors: retSels primitiveSetSelectors: primitiveSetSels constraintOwnerVecs: constraintOwnerPaths errorVecs: e testVecs: t methods: m
	context _ c.
	retrieveSelectors _ retSels.
	primitiveSetSelectors _ primitiveSetSels.
	errorVecs _ e.
	testVecs _ t.
	methods _ m.
	"convert object paths into pointers"
	objectOwners _ objectOwnerPaths collect:
		[:path | (AccessPath new names: path) applyTo: context].
	"convert vectors of constraint owner paths into vectors of owners"
	constraintOwnerVecs _ constraintOwnerPaths collect:
		[:o | o collect:
			[:owner | (AccessPath new names: owner) applyTo: context]].
	showImageFlag _ true! !

!Relaxer methodsFor: 'execution'!
add: delta
	"add delta to the value under consideration"
	objectOwner perform: primitiveSetSelector with:
		(objectOwner perform: retrieveSelector) + delta.
	method == #none
	  ifFalse: [context perform: method]! !

!Relaxer methodsFor: 'execution'!
checkError: toterror old: old
	 "Return true if relaxation should stop"
	toterror > old "If the total error has increased, something is wrong."
	  ifTrue:
		[Transcript cr; show: 'error increased during relaxation!!!!!!'.
		^true].
	"If the total error has decreased by less than minChange,
		the constraints aren't satisfiable."
	context minChange * toterror > (old - toterror) abs
	  ifTrue:
		[Transcript cr; show: 'constraints not satisfied after relaxation'.
		^true].
	^false! !

!Relaxer methodsFor: 'execution'!
checkKBD: toterror stop: stopArg
	 | flag char stop |
	stop _ stopArg.
	"maybe print error"
	"true if the toterror should be printed"
	"always print error when stopping"
	"Check if the user typed e, q, s, or n.  Discard other characters.
		Use rawkbck to prevent purging with each check."
	flag _ stop.
	[Sensor keyboardPressed] whileTrue:
		[(char _ Sensor keyboard) = $e
		  ifTrue: [flag _ true]
		  ifFalse:
			[char = $q
			  ifTrue: [flag _ stop _ true]
			  ifFalse:
				[char = $s
				  ifTrue:
					[showImageFlag
					  ifFalse:
						[showImageFlag _ true.
						"mem at: 55 put: mem067.
						mem067 _ nil"]]
				  ifFalse:
					[char = $n
					  ifTrue:
						[showImageFlag
						  ifTrue:
							[showImageFlag _ false.
							"mem067 _ mem at: 55.
							mem at: 55 put: 58"]]]]]].
	flag
	  ifTrue: [self printError: toterror].
	(stop and: ["mem067 == nil == "false])
	  ifTrue: ["mem at: 55 put: mem067"].
	^stop! !

!Relaxer methodsFor: 'execution'!
cycle
	"relax each object"
		| sel |
	1 to: objectOwners size do:
		[:i | objectOwner _ objectOwners at: i.
		retrieveSelector _ retrieveSelectors at: i.
		primitiveSetSelector _ primitiveSetSelectors at: i.
		constraintOwners _ constraintOwnerVecs at: i.
		errors _ errorVecs at: i.
		method _ methods at: i.
		sel _ (objectOwner perform: retrieveSelector) relaxationSolveSelector.
		self add: (self perform: sel)]! !

!Relaxer methodsFor: 'execution'!
printError: error
	Transcript cr; show: 'error = ' , error printString! !

!Relaxer methodsFor: 'execution'!
reducePointMatrix: m 
	"reduce m to a 2 by 3 matrix"
		| new sum |
	new _ Array new: 2.
	new at: 1 put: (Array new: 3).
	new at: 2 put: (Array new: 3).
	1 to: 2 do: [:i | 1 to: 3 do:
			[:j | sum _ 0.0.
			1 to: m size do: [:k | sum _ sum + (((m at: k) at: i) * ((m at: k) at: j))].
			(new at: i) at: j put: sum]].
	^new! !

!Relaxer methodsFor: 'execution'!
relax  "relax objects until constraints are satisfied"
		|	stop "true if relaxation should stop"
			toterror "the sum of the squares of the errors in the constraints"
			old "the toterror from the previous iteration"   |
	Transcript cr; cr; show: 'relaxing - type ''e'' to see error; ''q'' to quit;
''s'' to show image; ''n'' to not show it'.
	stop _ false.
	old _ 1.0e30.
	[stop] whileFalse: 
		[self cycle.  "relax each object"
		(stop _ self testConstraints)
		  ifFalse:
			[toterror _ self toterror.
			(self checkError: toterror old: old)
			  ifTrue: [stop _ true]
			  ifFalse: [old _ toterror]].
		stop _ self checkKBD: toterror stop: stop.
		showImageFlag ifTrue: [self showImage]]! !

!Relaxer methodsFor: 'execution'!
relaxQuiet  "like relax, but no display stuff"
		|	stop "true if relaxation should stop"
			toterror "the sum of the squares of the errors in the constraints"
			old "the toterror from the previous iteration"   |
	stop _ false.
	old _ 1.0e30.
	[stop] whileFalse: 
		[self cycle.  "relax each object"
		(stop _ self testConstraints)
		  ifFalse:
			[toterror _ self toterror.
			(self checkError: toterror old: old)
			  ifTrue: [stop _ true]
			  ifFalse: [old _ toterror]]]! !

!Relaxer methodsFor: 'execution'!
showImage
"show the current picture of the thing being relaxed.  It is inconvenient to find the window in which this thing is being displayed.  This is done with a kludge: namely, looking up the stack for a Tool and telling it to showBuffered"
		| cxt |
	cxt _ thisContext.
	[cxt isNil or: [cxt receiver isKindOf: Tool]] whileFalse:
		[cxt _ cxt sender].
	cxt isNil ifFalse: [cxt receiver showBuffered]! !

!Relaxer methodsFor: 'execution'!
solve1PointEqn: m | other matrix |
	other _ Array new: 3 "m should length 3".
	other at: 1 put: (m at: 2).
	other at: 2 put: 0.0 - (m at: 1).
	other at: 3 put: 0.0.
	matrix _ Array new: 2.
	matrix at: 1 put: m.
	matrix at: 2 put: other.
	^self solve2PointEqns: matrix! !

!Relaxer methodsFor: 'execution'!
solve2PointEqns: m | det x y |
	det _ ((m at: 1) "m should be 2 by 3" at: 1) * ((m at: 2) at: 2) - (((m at: 1) at: 2) * ((m at: 2) at: 1)).
	det = 0.0
	  ifTrue:
		[0.0 = ((m at: 1) at: 1)
		  ifTrue: [^self solve1PointEqn: (m at: 2)].
		^self solve1PointEqn: (m at: 1)]
	  ifFalse:
		[x _ ((m at: 1) at: 3) * ((m at: 2) at: 2) - (((m at: 2) at: 3) * ((m at: 1) at: 2)) / det.
		y _ ((m at: 1) at: 1) * ((m at: 2) at: 3) - (((m at: 2) at: 1) * ((m at: 1) at: 3)) / det.
		^Point x: x y: y]! !

!Relaxer methodsFor: 'execution'!
solveNumber
	"This message returns the change to be made to a nmber
		to satisfy its constraints (1 degree of freedom).  The change is
	   determined by finding linear equations which best approximate the
	   possibly nonlinear constraints, given the current values of the
	   variables.  Then a least-mean-squares fit to these equations is
	   found.  See Ivan Sutherland's thesis Sketchpad for details of
	   this technique.

	   The global variable Incremented is set to true when the value of
	   the object being relaxed has been incremented."
		| matrix delta nConstraints value negerrsum errchange x |
	nConstraints _ constraintOwners size.
	value _ objectOwner perform: retrieveSelector.
	delta _ (1.0e-4 > value abs
			  ifTrue: [1.0e-4]
			  ifFalse: [0.001 * value]).
	1 = nConstraints
	  ifTrue:
		[Incremented _ false.
		negerrsum _ 0.0 - (context perform: (errors at: 1)).
		self add: delta.
		Incremented _ true.
		errchange _ negerrsum + (context perform: (errors at: 1)) / delta.
		self sub: delta.
		Incremented _ false.
		errchange = 0.0 ifTrue: [^0.0].
		^negerrsum / errchange]
	  ifFalse:
		[Incremented _ false.
		matrix _ Array new: nConstraints.
		1 to: nConstraints do:
			[:i | matrix at: i put: (Array new: 2).
			(matrix at: i) at: 2 put: 0.0 - (context perform: (errors at: i))].
		self add: delta.
		Incremented _ true.
		1 to: nConstraints do: [:i | (matrix at: i) at: 1 put: ((matrix at: i) at: 2) + (context perform: (errors at: i)) / delta].
		self sub: delta.
		Incremented _ false.
		negerrsum _ errchange _ 0.0.
		1 to: nConstraints do:
			[:i | negerrsum _ ((matrix at: i) at: 2) * (x _ (matrix at: i) at: 1) + negerrsum.
			errchange _ x * x + errchange].
		errchange = 0.0
		  ifTrue: [^0.0].
		^negerrsum / errchange]! !

!Relaxer methodsFor: 'execution'!
solvePoint | matrix nConstraints |
	"This message returns the change to be made to a Point
		to satisfy its constraints (2 degrees of freedom).  The change is
	   determined by finding linear equations which best approximate the
	   possibly nonlinear constraints, given the current values of the
	   variables.  Then a least-mean-squares fit to these equations is
	   found.  See Ivan Sutherland's thesis Sketchpad for details of
	   this technique.

	   The global variable Incremented is set to true when the value of
	   the object being relaxed has been incremented."
	nConstraints _ constraintOwners size.
	Incremented _ false.
	matrix _ Array new: nConstraints.
	1 to: nConstraints do:
		[:i | matrix at: i put: (Array new: 3).
		(matrix at: i) at: 3 put: 0.0 - (context perform: (errors at: i))].
	self add: 1.0 @ 0.0 .
	Incremented _ true.
	1 to: nConstraints do: [:i | (matrix at: i) at: 1 put: ((matrix at: i) at: 3) + (context perform: (errors at: i))].
	self add: -1.0 @ 1.0 .
	1 to: nConstraints do: [:i | (matrix at: i) at: 2 put: ((matrix at: i) at: 3) + (context perform: (errors at: i))].
	self add: 0.0 @ -1.0 .
	Incremented _ false.
	nConstraints = 1
	  ifTrue: [^self solve1PointEqn: (matrix at: 1)].
	nConstraints = 2
	  ifTrue: [^self solve2PointEqns: matrix].
	^self solve2PointEqns: (self reducePointMatrix: matrix)! !

!Relaxer methodsFor: 'execution'!
sub: delta
	"subtract delta from the value under consideration"
	objectOwner perform: primitiveSetSelector with:
		(objectOwner perform: retrieveSelector) - delta.
	method == #none
	  ifFalse: [context perform: method]! !

!Relaxer methodsFor: 'execution'!
testConstraints
	"return true if constraints are satisfied"
	1 to: objectOwners size do:
		[:i | constraintOwners _ constraintOwnerVecs at: i.
		tests _ testVecs at: i.
		1 to: constraintOwners size do:
			[:j | (context perform: (tests at: j))
			  ifFalse: [^false]]].
	^true! !

!Relaxer methodsFor: 'execution'!
toterror
	"compute the sum of the squares of the errors"
	| toterror e |
	toterror _ 0.0.
	1 to: objectOwners size do:
		[:i | constraintOwners _ constraintOwnerVecs at: i.
		errors _ errorVecs at: i.
		1 to: constraintOwners size do:
			[:j | e _context perform: (errors at: j).
			toterror _ e * e + toterror]].
	^toterror! !


!BridgeRelaxer commentStamp: '<historical>' prior: 0!
A BridgeRelaxer doesn't expect the error in the constraints to go to 0!

!BridgeRelaxer methodsFor: 'execution'!
checkError: toterror old: old
	"Return true if relaxation should stop"
	"this replaces the inherited method - doesn't put out a message
		if constraints are not satisfied, and doesn't check for a decrease
		in the total error"

	^context minChange * toterror > (old - toterror) abs! !

!BridgeRelaxer methodsFor: 'execution'!
printError: error
	"In bridges, the error is actually the sum of the squares
		of the changes in lengths of the beams"
	Transcript cr; show: 'deformation: ' , error printString! !

!BridgeRelaxer methodsFor: 'execution'!
testConstraints
	^false! !


!Relaxer class methodsFor: 'initialization'!
initialize
	Smalltalk at: #Incremented put: false! !


!SetConstraint methodsFor: 'initialization'!
setNode: sN elementNode: eN elementMessageTree: eT
	setNode _ sN.
	elementNode _ eN.
	elementMessageTree _ eT! !

!SetConstraint methodsFor: 'compiling'!
addMethods: queue context: context owner: owner receiver: receiver
	"add my messages to the queue"
		| encoder setPath specialVars fixInstPath elementPath appliedSetNode
			m1 m2 sel queuedDescr queuedTest |
	(self overlaps: receiver) ifFalse: [^nil].
	"see if I have already been told to add messages to the queue"
	(queue hasConstraint: self owner: owner) ifTrue: [^nil].
	encoder _ queue encoder.
	queuedTest _ self testCode: encoder owner: owner.
	"find path to set in current context"
	setPath _ owner concat: setNode asPath.
	"weird fix - if the set is in the instance part of a definition, its paths must be prefixed
	 with 'instance', since they otherwise won't start in the right place"
	fixInstPath _ self instancePathPrefix: context class owner: owner.
	"iterate through the paths in the set"
	(setPath applyTo: context) do:
		[:e | elementPath _ fixInstPath concat: e.
		"make a message node (in the correct context) that turns the set of paths
			into a set of objects"
		appliedSetNode _ MessageNode new
			receiver: (setNode relocatePaths: owner encoder: encoder)
			selector: #applyTo:
			arguments: (Array with: (fixInstPath code: encoder))
			precedence: #applyTo: precedence
			from: encoder.
		"specialVars is a dictionary whose keys are the names of the element and primitiveSet.  Occurrences
			of nodes for these variables are to be replaced by references to the actual element and
			to the set of objects, respectively."
		specialVars _ Dictionary new.
		specialVars at: setNode key put: appliedSetNode.
		specialVars at: elementNode key put: (elementPath code: encoder).
		"relocate paths in the element message, also replacing special vars"
		m1 _ elementMessageTree relocatePaths: owner encoder: encoder specialVars: specialVars.
		"make a message node to set the actual element to its new value"
		sel _ ('primitiveSet.' , elementPath names last , ':') asSymbol.
		m2 _ MessageNode new
			receiver: (elementPath allButLast code: encoder)
			selector: sel
			arguments: (Array with: m1)
			precedence: sel precedence
			from: encoder.
		"make a message plan with the correct context, receiver, and owner"
		queuedDescr _ QueuedMethodDescription new
			context: context
			receiver: elementPath
			constraint: self
			owner: owner
			uniqueState: true
			referenceOnly: false
			compileTimeOnly: false
			constraintMethod: m2
			testMethod: queuedTest.
		queue addMethod: queuedDescr.
		queuedDescr addMethods: queue]! !

!SetConstraint methodsFor: 'compiling'!
errorCode: encoder owner: owner
	"return a method (in parse tree form) to return my error, in the
	  context of owner"
		| startContext specialVars appliedSetNode |
		"specialVars is a dictionary whose sole key is the name of the set.  Occurrences
			of nodes for this variable are to be replaced by references to the set of objects."
	startContext _ self instancePathPrefix: encoder targetClass owner: owner.
	appliedSetNode _ MessageNode new
		receiver: (setNode relocatePaths: owner encoder: encoder)
		selector: #applyTo:
		arguments: (Array with: (startContext code: encoder))
		precedence: #applyTo: precedence
		from: encoder.
	specialVars _ Dictionary new.
	specialVars at: setNode key put: appliedSetNode.
	"relocate paths in the error tree, also replacing special vars"
	^errorTree relocatePaths: owner encoder: encoder specialVars: specialVars! !

!SetConstraint methodsFor: 'compiling'!
instancePathPrefix: contextClass owner: owner
	"weird fix - if the set is in the instance part of a definition, its paths must be prefixed
	 with 'instance', since they otherwise won't start in the right place"

	"temporary fix - ignore owner"
	(contextClass inheritsFrom: ThingLabDefinition)
		ifTrue: [^ #(instance) asPath]
		ifFalse: [^ EmptyPath]! !

!SetConstraint methodsFor: 'compiling'!
oldInstancePathPrefix: contextClass owner: owner
	"weird fix - if the set is in the instance part of a definition, its paths must be prefixed
	 with 'instance', since they otherwise won't start in the right place"
	((owner firstName==#instance) and: [contextClass inheritsFrom: ThingLabDefinition])
		ifTrue: [^ #(instance) asPath]
		ifFalse: [^ EmptyPath]! !

!SetConstraint methodsFor: 'compiling'!
testCode: encoder owner: owner
	"return a method (in parse tree form) to test whether I am satisfied in the
	  context of owner"
		| startContext specialVars appliedSetNode |
	"specialVars is a dictionary whose sole key is the name of the set.  Occurrences
	 of nodes for this variable are to be replaced by references to the set of objects."
	startContext _ self instancePathPrefix: encoder targetClass owner: owner.
	appliedSetNode _ MessageNode new
		receiver: (setNode relocatePaths: owner encoder: encoder)
		selector: #applyTo:
		arguments: (Array with: (startContext code: encoder))
		precedence: #applyTo: precedence
		from: encoder.
	specialVars _ Dictionary new.
	specialVars at: setNode key put: appliedSetNode.
	"relocate paths in the test, also replacing special vars"
	^testTree relocatePaths: owner encoder: encoder specialVars: specialVars! !


!SetConstraint class methodsFor: 'initialization'!
owner: owner rule: ruleString set: setString element: elementString elementMethod: mString
	^self owner: owner rule: ruleString test: nil error: nil set: setString element: elementString elementMethod: mString! !

!SetConstraint class methodsFor: 'initialization'!
owner: owner rule: ruleString test: testString error: errorString set: setString element: elementString elementMethod: mString
		| superMethods aConstraint s elMethod elTree encoder setNode elNode |
	"Make a dummy message to the set for detecting overlap of constraints.  The set is really altered rather
	 than referenced, but this message will be enough to cause this constraint to be activated."
	superMethods _ Array with: (setString , ' reference').
	aConstraint _ self owner: owner rule: ruleString test: testString error: errorString methods: superMethods.
	s _ 'satisfy | ' , elementString , ' | ' , mString.
	elMethod _ Compiler new parse: s in: owner class notifying: nil.
	elTree _ elMethod block statements at: 1.
	encoder _ elMethod encoder.
	setNode _ setString asPath code: encoder.
	elNode _ encoder encodeVariable: elementString.
	aConstraint setNode: setNode elementNode: elNode elementMessageTree: elTree.
	^aConstraint! !


!SetMembershipConstraint commentStamp: '<historical>' prior: 0!
I constrain something to be an element of a set.  Kludge - doesn't handle deletion correctly.

Instance Variables:
	set	<Path> -- path to set element
	element	<Path> -- path to set!

!SetMembershipConstraint methodsFor: 'initialization'!
set: s element: e
	set _ s.
	element _ e! !

!SetMembershipConstraint methodsFor: 'compiling'!
addMethods: queue context: context owner: owner receiver: receiver
		| queuedDescr |  
	"add my messages to the queue"
	(self overlaps: receiver) ifFalse: [^nil].
	"see if I have already been told to add messages to the queue"
	(queue hasConstraint: self owner: owner) ifTrue: [^nil].
	methodDescriptions do: 
		[:descr | 
		"make a message plan with the correct
		 context, receiver, and owner"
		queuedDescr _ QueuedMethodDescription new
			context: context
			receiver: (owner concat: descr receiver)
			constraint: self
			owner: owner
			uniqueState: descr uniqueState
			referenceOnly: descr referenceOnly
			compileTimeOnly: descr compileTimeOnly
			constraintMethod: nil
			testMethod: nil.
		"kludge - only add first message"
		descr == (methodDescriptions at: 1) ifTrue: [queue addMethod: queuedDescr].
		queuedDescr addMethods: queue]! !


!SetMembershipConstraint class methodsFor: 'initialization'!
owner: owner rule: ruleString set: setString element: elementString
		| superMethods aConstraint |
	"Make a dummy message to the set and element for detecting overlap of constraints"
	superMethods _ (Array with: (setString , ' reference') with: (elementString , ' reference')).
	aConstraint _ self owner: owner rule: ruleString test: nil error: nil methods: superMethods.
	aConstraint set: setString asPath element: elementString asPath.
	^aConstraint! !

!ConditionalConstraint class methodsFor: 'initialization'!
owner: owner condition: conditionString ifTrue: trueArray ifFalse: falseArray
	^ self new! !


!SetOfPaths commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!SetOfPaths methodsFor: 'copying/merging/deleting'!
deletePathFromSets: deletePath 
	"The object at the end of path is being deleted.  Check sets of paths and remove
		paths that would point to nonexistent parts."
	self copy do:
		[:p | (p within: deletePath) notNil ifTrue: [self remove: p]]! !

!SetOfPaths methodsFor: 'copying/merging/deleting'!
mergeWith: other 
	| s |
	s _ SetOfPaths new.
	self do: [:p | s add: p].
	other do: [:p | s add: p].
	^s! !

!SetOfPaths methodsFor: 'copying/merging/deleting'!
relocatePaths: prefix 
	"prefix all my paths with prefix"
	self copy do:
		[:path | self remove: path; add: (prefix concat: path)]! !

!SetOfPaths methodsFor: 'operations'!
applyTo: owner
		| c |
	c _ OrderedCollection new.  "want to allow duplicate elements"
	self do: [:each | c add: (each applyTo: owner)].
	^c! !

!SetOfPaths methodsFor: 'showing'!
printTableOfFields: strm
	strm nextPut: ${.
	self do: [:p | p printOn: strm.  strm space].
	strm nextPut: $}! !


!SpareField methodsFor: 'testing'!
isSpare
	^ true! !


!SuperclassDescription commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!SuperclassDescription methodsFor: 'testing'!
isSuperclassPart
	^ true! !


!TextThingController methodsFor: 'control defaults'!
isControlActive
	Sensor blueButtonPressed ifTrue: [^ false].
	self viewHasCursor ifTrue: [^ true].
	^ Sensor noButtonPressed! !

!TextThingController methodsFor: 'control defaults'!
processRedButton
	self viewHasCursor ifTrue: [super processRedButton]! !


!TextThingView methodsFor: 'controller access'!
defaultControllerClass
	^ TextThingController! !

!TextThingView methodsFor: 'controller access'!
innerMedium
	^ Display medium! !

!TextThingView methodsFor: 'displaying'!
displayOn: aMedium
	self isUnlocked
		ifTrue: 
			[self controller
				wrappingBox: self insetDisplayBox  "Inherited x-offset not used here"
				clippingBox: self insetDisplayBox.
			"ugly initialization hack: "
			(controller text isEmpty and: [controller textHasChanged not])
				ifTrue: [self newText: self getText]].
	super displayOn: aMedium! !


!ThingLabBrowser commentStamp: '<historical>' prior: 0!
A ThingLab browser allows access to ThingLab objects.  The aspect list determines what aspect is accessible, and therefore what the nature of the access is.

Many aspects are accessible as text, but the most significant, namely the protoype itself, is edited as a graphical structure.  In the first case, text is defined, and the bottom subView is a TextView;  in the latter case, displayObject is defined, and the bottom subView is a DisplayObjectView.

Instance Variables:
	prototypeClass	<Class selected from Things>
	prototype		<ThingLabObject>
	aspect			<Format>
	tool				<Tool>
	filter			<Class relevant to prototypeClass>
	displayObject	<DisplayObject representing the prototype>
	text				<Text representing the selected aspect of the prototype>
!

!ThingLabBrowser methodsFor: 'prototypeClass'!
browsedDictionary
	^ThingLabObject instances! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
browsedPrototype
	^ prototype! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
buildPrototypeClass: name
	ThingLabObject subclass: name
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Prototypes'! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
defineNewThing
		| nameString name sup nSpares strm newFields newClass proto | 
	nSpares _ 10.  "number of spare fields to generate"
	nameString _ FillInTheBlank request: 'Enter name of new thing' initialAnswer: 'NameOfThing'.
	nameString size=0 ifTrue: [^self].
	nameString at: 1 put: (nameString at: 1) asUppercase.
	name _ self makeClassName: nameString.
	(Smalltalk includesKey: name) ifTrue: [^self notify: name , ' is a name used elsewhere'].
	sup _ self prototypeSuperclass.
	strm _ WriteStream on: (String new: 100).
	1 to: nSpares do: [:i | strm nextPutAll: ' part'; print: i+sup instSize].
	newFields _ strm contents.
	sup subclass: name
			instanceVariableNames: newFields
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Prototypes'.
	newClass _ (Smalltalk at: name).
	proto _ newClass prototype.  "causes prototype to be created"
	"Mark new fields as spare"
	newClass instSize-nSpares+1 to: newClass instSize do:
		[:i | proto markSpareField: i].
	"leave text editing mode, and update the selection pane"
	prototypeClass _ name.
	self changed: #prototypeClassList! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
fileOutClass
	self browsedPrototype fileOutPrototype! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
makeClassName: str
	^str asSymbol! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
prototypeClass
	^ prototypeClass! !

!ThingLabBrowser methodsFor: 'prototypeClass' stamp: 'di 1/20/1999 20:45'!
prototypeClass: selection
	prototypeClass _ selection.
	prototype _ selection == nil
		ifTrue: [nil]
		ifFalse: [self browsedDictionary at: selection].
	self changed: #picture.
	self changed: #aspectList.
	self changed: #text.
	self changed: #instancePicture.
	self changed: #instanceText! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
prototypeClassList
	prototypeClassNameList _ self browsedDictionary keys asSortedCollection.
	self changed: #filter.
^ prototypeClassNameList! !

!ThingLabBrowser methodsFor: 'prototypeClass' stamp: 'di 1/20/1999 20:42'!
prototypeClassListIndex: newIndex
	prototypeClassListIndex _ newIndex.
	self prototypeClass: (newIndex = 0
							ifTrue: [nil]
							ifFalse: [self prototypeClassList at: newIndex]).
	self changed: #prototypeClassListIndex! !

!ThingLabBrowser methodsFor: 'prototypeClass' stamp: 'di 1/21/1999 20:59'!
prototypeClassListMenu
	prototypeClass == nil
		ifTrue: [SelectionMenu labels: 'define new thing' withCRs
					lines: #()
					selections: #(defineNewThing)].
	^ SelectionMenu
			labels: 'define new thing\file out\remove' withCRs
			lines: #(1 2)
			selections: #(defineNewThing fileOutClass removeThing)! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
prototypeSuperclass
	^ThingLabObject! !

!ThingLabBrowser methodsFor: 'prototypeClass'!
removeThing
	(self confirm: 'Do you really want to remove
' , prototypeClass , ' from the system?') ifFalse: [^ self].
	self browsedDictionary removeKey: prototypeClass ifAbsent: [].
	self browsedPrototype class removeFromSystem.
	"update the selection pane"
	prototypeClass _ nil.
	self changed: #prototypeClassList! !

!ThingLabBrowser methodsFor: 'aspect'!
aspect
	^ aspect! !

!ThingLabBrowser methodsFor: 'aspect'!
aspect: selection
	| wasPicture | 
	wasPicture _ self isPicture.
	aspect _ selection.
	wasPicture == self isPicture
		ifFalse: [self changed: #viewSpecies].
	self changed: #text.
	self changed: #instanceText.
	self changed: #picture.
	self changed: #instancePicture.
	self changed: #toolList.
	self changed: #filterList! !

!ThingLabBrowser methodsFor: 'aspect' stamp: 'di 1/20/1999 20:54'!
aspectList
	prototype == nil ifTrue: [^ Array new].
	^ prototype formats collect: [:x | x name]! !

!ThingLabBrowser methodsFor: 'aspect' stamp: 'di 1/20/1999 20:51'!
aspectListIndex: newIndex
	aspectListIndex _ newIndex.
	self aspect: (newIndex = 0
					ifTrue: [nil]
					ifFalse: [prototype formats at: newIndex])! !

!ThingLabBrowser methodsFor: 'aspect'!
aspectListMenu
	^ nil  "a no-op"! !

!ThingLabBrowser methodsFor: 'aspect'!
isPicture
	^ aspect notNil and: [aspect isPicture]! !

!ThingLabBrowser methodsFor: 'aspect'!
showSelector
	^ aspect showSelector! !

!ThingLabBrowser methodsFor: 'tool'!
tool
	^ tool! !

!ThingLabBrowser methodsFor: 'tool'!
tool: selection
	tool _ selection! !

!ThingLabBrowser methodsFor: 'tool' stamp: 'di 1/20/1999 20:56'!
toolList
	(prototype == nil or: [aspect == nil]) ifTrue: [^ Array new].
	self showSelector == #showPicture: ifFalse: [^ Array new].
	^ (prototype editingToolsFor: aspect) collect: [:x | x new action]! !

!ThingLabBrowser methodsFor: 'tool' stamp: 'di 1/21/1999 20:07'!
toolListIndex: newIndex
	toolListIndex _ newIndex.
	self tool: (newIndex = 0
					ifTrue: [nil]
					ifFalse: [(prototype editingToolsFor: aspect) at: newIndex])! !

!ThingLabBrowser methodsFor: 'tool'!
toolListMenu
	^ nil  "a no-op"! !

!ThingLabBrowser methodsFor: 'tool'!
toolObject
	(tool == nil) | (filter == nil) ifTrue: [^ nil].
	^ tool new carrier: (ThingLabObject instances at: filter) recopy! !

!ThingLabBrowser methodsFor: 'filter'!
filter
	^ filter! !

!ThingLabBrowser methodsFor: 'filter'!
filter: selection
	filter _ selection! !

!ThingLabBrowser methodsFor: 'filter'!
filterList
"(prototype == nil or: [aspect == nil]) ifTrue: [^ nil]." "this was the old code"
	aspect == nil ifTrue: [^ nil].
	self showSelector == #showPicture: ifFalse: [^ nil].
	^ prototype usefulClasses! !

!ThingLabBrowser methodsFor: 'filter' stamp: 'di 1/20/1999 20:39'!
filterListIndex: newIndex
	filterListIndex _ newIndex.
	self filter: (newIndex = 0
					ifTrue: [nil]
					ifFalse: [self filterList at: newIndex])! !

!ThingLabBrowser methodsFor: 'filter'!
filterListMenu
	^ nil  "a no-op"! !

!ThingLabBrowser methodsFor: 'context'!
context
	"return the top-level owner of the object being edited"
	^prototype! !

!ThingLabBrowser methodsFor: 'picture'!
instancePicture
	aspect == nil ifTrue: [^ nil].
	prototype == nil ifTrue: [^ nil].
	^ prototype instance! !

!ThingLabBrowser methodsFor: 'picture'!
picture
	aspect == nil ifTrue: [^ nil].
	^ prototype! !

!ThingLabBrowser methodsFor: 'picture'!
pictureMenu
	^ nil! !

!ThingLabBrowser methodsFor: 'text'!
acceptText: aText from: aController
	^ false! !

!ThingLabBrowser methodsFor: 'text'!
instanceText
	aspect isNil | prototype isNil ifTrue: [^Text new].
	^prototype instance perform: aspect showSelector with: self! !

!ThingLabBrowser methodsFor: 'text' stamp: 'di 1/20/1999 21:46'!
text
	aspect == nil ifTrue: [^ Text new].
	^ prototype perform: aspect showSelector with: self thingView! !

!ThingLabBrowser methodsFor: 'text' stamp: 'di 1/23/1999 05:06'!
textMenu
	"ThingLabBrowser classPool at: #TextMenu put: nil"
	TextMenu == nil ifTrue:
		[TextMenu _ SelectionMenu
			labels: 'again\undo\copy\cut\paste\do it\print it\accept\cancel' withCRs
			lines: #(2 5 7)
			selections: #(again undo copySelection cut paste doIt printIt accept cancel)].
	^ TextMenu! !

!ThingLabBrowser methodsFor: 'update'!
checkNewThings
	(prototypeClassNameList == nil or:
		[self browsedDictionary size ~= prototypeClassNameList size])
	ifTrue:
		[self changed: #prototypeClassList.
		self changed: #filterList]! !

!ThingLabBrowser methodsFor: 'squeak compatibility' stamp: 'di 1/21/1999 21:03'!
perform: selector orSendTo: otherTarget
	(self respondsTo: selector)
		ifTrue: [self perform: selector]
		ifFalse: [otherTarget perform: selector]! !

!ThingLabBrowser methodsFor: 'squeak compatibility' stamp: 'di 1/20/1999 21:45'!
thingView
	^ dependents detect: [:v | (v isMemberOf: PluggableTextView)
						or: [v isMemberOf: PluggableTextMorph]]
				ifNone: [dependents last]! !


!ObjectDefiner methodsFor: 'new definitions'!
browsedDictionary
	^ThingLabObject definitions! !

!ObjectDefiner methodsFor: 'new definitions'!
makeClassName: str
	^(str , 'Definition') asSymbol! !

!ObjectDefiner methodsFor: 'new definitions'!
prototypeSuperclass
	^ThingLabDefinition! !


!ThingLabBrowser class reorganize!
('instance creation' open openAsMorph)
!


!ThingLabBrowser class methodsFor: 'instance creation' stamp: 'di 1/20/1999 21:56'!
open		"ThingLabBrowser open"
	"Open a new ThingLabBrowser."

	| browser topView |
	World ifNotNil: [^ self openAsMorph].

	browser _ self new.
	topView _ ThingLabBrowserView new
		model: browser;
		label: 'ThingLab Browser';
		minimumSize: 300@200.
	topView borderWidth: 1.

	topView addSubView:
			((PluggableListView on: browser list: #prototypeClassList
				selected: #prototypeClassListIndex changeSelected: #prototypeClassListIndex:
				menu: #prototypeClassListMenu)
					window: (0@0 extent: 75@66)).
	topView addSubView:
			((PluggableListView on: browser list: #aspectList
				selected: #aspectListIndex changeSelected: #aspectListIndex:
				menu: #aspectListMenu)
					window: (75@0 extent: 75@66)).
	topView addSubView:
			((PluggableListView on: browser list: #toolList
				selected: #toolListIndex changeSelected: #toolListIndex:
				menu: #toolListMenu)
					window: (150@0 extent: 75@66)).
	topView addSubView:
			((PluggableListView on: browser list: #filterList
				selected: #filterListIndex changeSelected: #filterListIndex:
				menu: #filterListMenu)
					window: (225@0 extent: 75@66)).
	topView addSubView:
			((PluggableTextView on: browser text: #text accept: #acceptText:
				readSelection: nil menu: #textMenu)
					window: (0@66 extent: 300@134)).
	topView controller open! !

!ThingLabBrowser class methodsFor: 'instance creation' stamp: 'di 1/20/1999 20:36'!
openAsMorph		"ThingLabBrowser openAsMorph"
	"Open a new ThingLabBrowser."

	| browser window |
	browser _ self new.
	window _ (SystemWindow labelled: 'ThingLab Browser') model: browser.
	window addMorph:
			(PluggableListMorph on: browser list: #prototypeClassList
				selected: #prototypeClassListIndex changeSelected: #prototypeClassListIndex:
				menu: #prototypeClassListMenu)
			frame: (0@0 extent: 0.25@0.3).
	window addMorph:
			(PluggableListMorph on: browser list: #aspectList
				selected: #aspectListIndex changeSelected: #aspectListIndex:
				menu: #aspectListMenu)
			frame: (0.25@0 extent: 0.25@0.3).
	window addMorph:
			(PluggableListMorph on: browser list: #toolList
				selected: #toolListIndex changeSelected: #toolListIndex:
				menu: #toolListMenu)
			frame: (0.5@0 extent: 0.25@0.3).
	window addMorph:
			(PluggableListMorph on: browser list: #filterList
				selected: #filterListIndex changeSelected: #filterListIndex:
				menu: #filterListMenu)
			frame: (0.75@0 extent: 0.25@0.3).
	window addMorph:
			(PluggableTextMorph on: browser text: #text accept: #acceptText:
				readSelection: nil menu: #textMenu)
			frame: (0@0.3 extent: 1@0.7).

	window openInWorld! !


!ThingLabBrowserController methodsFor: 'control defaults'!
controlInitialize
	super controlInitialize.
	model checkNewThings! !


!ThingLabBrowserView reorganize!
('views and updating' changeView: defaultControllerClass update: viewsOtherThan:)
!


!ThingLabBrowserView methodsFor: 'views and updating' stamp: 'di 1/21/1999 19:20'!
changeView: oldView
	| viewsD aspectSymbol newView oldWindow |
	oldWindow _ oldView window.
	viewsD _ (oldView isMemberOf: PluggableTextView)
			ifTrue: [oldView getTextSelector == #text]
			ifFalse: [oldView viewsDefinition].
	self releaseSubView: oldView.
	model isPicture
		ifTrue: [aspectSymbol _ viewsD ifTrue: [#picture] ifFalse: [#instancePicture].
				newView _ PictureView on: model aspect: aspectSymbol]
		ifFalse: [aspectSymbol _ viewsD ifTrue: [#text] ifFalse: [#instanceText].
				newView _ PluggableTextView on: model
						text: aspectSymbol accept: #acceptText:from:
						readSelection: nil menu: #textMenu].
	self addSubView: ((newView borderWidth: 1) window: oldWindow).
! !

!ThingLabBrowserView methodsFor: 'views and updating'!
defaultControllerClass
	^ ThingLabBrowserController! !

!ThingLabBrowserView methodsFor: 'views and updating'!
update: aSymbol
	| oldView |
	aSymbol == #viewSpecies
		ifFalse: [^super update: aSymbol].
	oldView _ subViews last.
	self changeView: oldView! !

!ThingLabBrowserView methodsFor: 'views and updating'!
viewsOtherThan: v
	v==subViews last ifTrue: [^Array new].
	self error: 'I don''t know about this view'! !


!ObjectDefinerView methodsFor: 'views and updating'!
definitionView
	"return the view looking at the definition"
	^subViews at: (subViews size-1)! !

!ObjectDefinerView methodsFor: 'views and updating'!
instanceView
	"return the view looking at the instance"
	^subViews last! !

!ObjectDefinerView methodsFor: 'views and updating'!
update: aSymbol
	| oldView1 oldView2 |
	aSymbol == #viewSpecies
		ifFalse: [^super update: aSymbol].
	oldView1 _self definitionView.
	oldView2 _ self instanceView.
	self changeView: oldView1.
	self changeView: oldView2! !

!ObjectDefinerView methodsFor: 'views and updating'!
viewsOtherThan: v
		| d i |
	d _ self definitionView.
	i _ self instanceView.
	v==d ifTrue: [^Array with: i].
	v==i ifTrue: [^Array with: d].
	self error: 'I don''t know about this view'! !


!ThingLabObject methodsFor: 'fields'!
markSpareField: i
	"mark my i-th field as spare.  Not defined up in Object, since fieldDescriptions not
	 saved for Objects in general."
	self fieldDescriptions at: i put: (SpareField name: ('part' , i printString) asSymbol index: i)! !

!ThingLabObject methodsFor: 'fields'!
storeOn: aStream 
	"like Object.storeOn: but doesn't save state for trailing spare fields"
	self class isVariable ifTrue: [^super storeOn: aStream].
	aStream nextPut: $(.
	aStream nextPutAll: self class name, ' basicNew'.
	1 to: self realFieldDescriptions size do:
		[:i | aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'! !

!ThingLabObject methodsFor: 'checking constraints'!
checkConstraints
	"check if all my constraints are satisfied.  This doesn't work right if subclasses 
	 are used, since the checkConstraints method in a superclass might be found.
	 However, the only ThingLab things that use superclasses are Expressions and
	 electrical things, and checkConstraints methods are built in the subclasses for
	 these objects."
	self class==ThingLabObject ifTrue: [^true].
	"make up a new method and resend the message"
	self makeCheckConstraintsMethod.
	^self checkConstraints! !

!ThingLabObject methodsFor: 'checking constraints'!
makeCheckConstraintsMethod
		| encoder returnFalseArg statements test ifNode checkNode
			methodNode |
	encoder _ Encoder new init: self class context: nil notifying: self.
	returnFalseArg _ Array with: (BlockNode new
		statements: (Array with:
			(ReturnNode new expr: (encoder encodeVariable: 'false')))
		returns: true).
	statements _ OrderedCollection new.
	self constraints do: [:c |
		test _ c testCode: encoder owner: EmptyPath.
		ifNode _ MessageNode new
			receiver: (c testCode: encoder owner: EmptyPath)
			selector: #ifFalse:
			arguments: returnFalseArg
			precedence: #ifFalse: precedence
			from: encoder.
		statements add: ifNode].
	(self fieldDescriptions select: [:f | f isPart]) do: [:p |
		checkNode _ MessageNode new
			receiver: (p makeRetrieveNode: encoder)
			selector: #checkConstraints
			arguments: Array new
			precedence: #checkConstraints precedence
			from: encoder.
		ifNode _ MessageNode new
			receiver: checkNode
			selector: #ifFalse:
			arguments: returnFalseArg
			precedence: #ifFalse: precedence
			from: encoder.
		statements add: ifNode].
	"passed all tests - return true"
	statements add: (ReturnNode new expr: (encoder encodeVariable: 'true')).
	methodNode _ MethodNode new
		selector: #checkConstraints
		arguments: Array new
		precedence: #checkConstraints precedence
		temporaries: #()
		block: (BlockNode new statements: statements returns: true)
		encoder: encoder
		primitive: 0.
	methodNode compileIn: self class! !


!BitImage methodsFor: 'as yet unclassified'!
showPicture: medium
	form displayOn: medium at: frame origin rule: Form paint! !


!ConstraintPicture methodsFor: 'as yet unclassified'!
center: c name: n leftNodes: l rightNodes: r topNodes: t bottomNodes: b
	center _ c.
	name _ n.
	leftNodes _ l.
	rightNodes _ r.
	topNodes _ t.
	bottomNodes _ b! !

!ConstraintPicture methodsFor: 'as yet unclassified'!
showNodesAndLeads: medium nodes: nodes start: start stop: stop
		| step pt node |
	step _ stop-start / (nodes size+1) asFloat.
	pt _ start+step.
	1 to: nodes size do: 
		[:i | node _ nodes at: i.
		node showPicture: medium.
		(ThingLabLine point1: (pt truncateTo: 1) point2: node location) showPicture: medium.
		pt _ pt+step]! !

!ConstraintPicture methodsFor: 'as yet unclassified'!
showPicture: medium
		| para extent frame |
	"find the frame for the constraint name"
	para _ name asParagraph.
	extent _ para compositionRectangle extent max: (0@para lineGrid).
	frame _ (center - (extent//2) extent: extent) insetBy: (-6 @ -2).
	"show leads coming from each side of the frame"
	self showNodesAndLeads: medium nodes: leftNodes
		start: frame topLeft stop: frame bottomLeft.
	self showNodesAndLeads: medium nodes: rightNodes
		start: frame topRight stop: frame bottomRight.
	self showNodesAndLeads: medium nodes: topNodes
		start: frame topLeft stop: frame topRight.
	self showNodesAndLeads: medium nodes: bottomNodes
		start: frame bottomLeft stop: frame bottomRight.
	"show the frame and constraint name"
	frame showPicture: medium.
	para displayOn: medium at: frame origin + (6@2).! !


!InstancePartDeclaration methodsFor: 'as yet unclassified'!
instancePartPath
	"return a path to the part of me that should be in each instance"
	self subclassResponsibility! !

!InstancePartDeclaration methodsFor: 'as yet unclassified'!
addTo: definition selectionMerges: selectionMerges attachers: attachers
	"I am being added to a definition.  Add an appropriate field to its instance.
	 Bug: this way of adding instance part declarations doesn't do the right thing
	 when an existing declaration is edited."
		| insertPath subPartPath fieldNameOrNil iPart iPath |
	insertPath _ super addTo: definition selectionMerges: selectionMerges attachers: attachers.
	subPartPath _ insertPath concat: self instancePartPath.
	fieldNameOrNil _ definition nameFor: subPartPath.
	iPart _ self instancePartPath applyTo: self.
	iPath _ definition instance addPart: iPart fieldNameOrNil: fieldNameOrNil.
	definition merge: (Array
		with: subPartPath
		with: ( #(instance) asPath concat: iPath))! !


!LineEquation methodsFor: 'as yet unclassified'!
intersect: L | t1 t2 |
	 "return my intersection with another LineEquation"
	t1 _ (B = 0
			  ifTrue: [A * L C - (C * L A)]
			  ifFalse: [C * L B - (B * L C)]).
	t2 _ B * L A - (A * L B).
	t2 = 0
	  ifTrue: [self error: 'lines are parallel']
	  ifFalse:
		[B = 0.0
		  ifTrue: [^self pointAtY: t1 / t2].
		^self pointAtX: t1 / t2]! !

!LineEquation methodsFor: 'as yet unclassified'!
pointNearest: pt
	 "return the point on me nearest pt"
	^self intersect: (self perpendicular: pt)! !

!LineEquation methodsFor: 'as yet unclassified'!
parallel: p | |
	^LineEquation new A: A B: B through: p! !

!LineEquation methodsFor: 'as yet unclassified'!
dist: p
	"distance from a line to a point"
	^(C + (A * p x asFloat) + (B * p y asFloat)) abs! !

!LineEquation methodsFor: 'as yet unclassified'!
A | |
	^A! !

!LineEquation methodsFor: 'as yet unclassified'!
perpendicular: p
	^LineEquation new A: (A = 0.0
	  ifTrue: [B negated]
	  ifFalse: [B]) B: A negated through: p! !

!LineEquation methodsFor: 'as yet unclassified'!
C | |
	^C! !

!LineEquation methodsFor: 'as yet unclassified'!
A: t1 B: t2 C: t3 | t |
	A _ t1.
	B _ t2.
	C _ t3.
	t _ (A * A "general form: A*x + B*y + C = 0
	normalize coefficients" + (B * B)) sqrt.
	A _ A / t.
	B _ B / t.
	C _ C / t! !

!LineEquation methodsFor: 'as yet unclassified'!
slope: t1 intercept: t2 | |
	A _ t1.
	C _ t2.
	self A: A B: -1.0 C: C "slope-intercept form: y = A*x + C"! !

!LineEquation methodsFor: 'as yet unclassified'!
point: p slope: t2 | |
	A _ t2.
	self A: A B: -1.0 through: p "point-slope form: y-y1 = A*(x-x1)"! !

!LineEquation methodsFor: 'as yet unclassified'!
A: a B: b through: p
	A _ a.
	B _ b.
	self A: A B: B C: (A * p x + (B * p y)) negated! !

!LineEquation methodsFor: 'as yet unclassified'!
B | |
	^B! !

!LineEquation methodsFor: 'as yet unclassified'!
pointAtX: x
	 "return the point on me with x value = x"
	B = 0.0
	  ifTrue: [self error: 'undefined']
	  ifFalse: [^x rounded @ (C + (A * x) / B negated) rounded]! !

!LineEquation methodsFor: 'as yet unclassified'!
reflect: pt
	 "return the reflection of pt about me"
	^(self pointNearest: pt) - pt * 2 + pt! !

!LineEquation methodsFor: 'as yet unclassified'!
pointAtY: y
	 "return the point on me with y value = y"
	A = 0.0
	  ifTrue: [self error: 'undefined']
	  ifFalse: [^(C + (B * y) / A negated) rounded @ y rounded]! !


!TextThing methodsFor: 'as yet unclassified'!
textExtent
	"assuming no wraparound, return the extent of my text"
		| lastNonSeparator para |
	"delete trailing separators"
	lastNonSeparator _ text string size.
	[(text string at: lastNonSeparator) isSeparator] whileTrue:
		[lastNonSeparator _ lastNonSeparator-1].
	para _ Paragraph
			withText: (text copyFrom: 1 to: lastNonSeparator)
			style: (TextStyle default alignment: alignment).
	^para compositionRectangle extent! !

!TextThing methodsFor: 'as yet unclassified'!
printTableOfFields: strm
	text string printOn: strm.
	strm nextPutAll: ' asTextThing'! !

!TextThing methodsFor: 'as yet unclassified' stamp: 'di 1/23/1999 12:15'!
textHeight
	"given my current width, return the height of my text"
		| lastNonSeparator para compRectOrigin compRectCorner |
	"delete trailing separators"
	lastNonSeparator _ text string size.
	[(text string at: lastNonSeparator) isSeparator] whileTrue:
		[lastNonSeparator _ lastNonSeparator-1].
	"make sure the composition rectangle is non-empty"
	compRectOrigin _ frame origin + (leftInset@topInset).
	compRectCorner _ (frame corner - (rightInset@bottomInset)) max: compRectOrigin+(1@1).
	para _ (Paragraph withText: (text copyFrom: 1 to: lastNonSeparator)
			style: (TextStyle default copy alignment: alignment)
			compositionRectangle: (compRectOrigin corner: compRectCorner)
			clippingRectangle: frame).
	para fit.
	^para height! !

!TextThing methodsFor: 'as yet unclassified'!
center
	alignment _ 2.
	^self! !

!TextThing methodsFor: 'as yet unclassified'!
findSelectionIndex: selectionRect list: list
	"I am being used to hold a menu list.  Return the selection indicated by selectionRect."
		| top lineGrid index |
	top _ frame origin y + topInset.
	lineGrid _ TextStyle default lineGrid.
	index _ selectionRect origin y - top // lineGrid + 1.
	"kludge - check for weird index"
	^(index max: 1) min: list size! !

!TextThing methodsFor: 'as yet unclassified'!
justify
	alignment _ 3.
	^self! !

!TextThing methodsFor: 'as yet unclassified'!
findSelectionRect: pt
	"I am being used to hold a menu list.  Return the selectionRect nearest to pt."
		| lineGrid textHeight textTop textBottom rectTop |
	lineGrid _ TextStyle default lineGrid.
	textHeight _ self textHeight.
	textTop _ frame origin y + topInset.
	textBottom _ textTop + textHeight - lineGrid.
	rectTop _ pt y < textTop ifTrue: [textTop] ifFalse:
		[pt y > textBottom ifTrue: [textBottom]
							ifFalse: [textTop + (pt y - textTop truncateTo: lineGrid)]].
	^(frame origin x+3) @ rectTop corner:
		(frame corner x-2) @ (rectTop+lineGrid-2)! !

!TextThing methodsFor: 'as yet unclassified'!
flushLeft
	alignment _ 0.
	^self! !

!TextThing methodsFor: 'as yet unclassified' stamp: 'di 1/23/1999 12:15'!
showPicture: medium
		| compRectOrigin compRectCorner |
	medium fillWhite: (frame insetBy: (2@2 corner: 1@1)).
	"make sure the composition rectangle is non-empty"
	compRectOrigin _ frame origin + (leftInset@topInset).
	compRectCorner _ (frame corner - (rightInset@bottomInset)) max: compRectOrigin+(1@1).
"
	para _ Paragraph withText: text
			style: (TextStyle default copy alignment: alignment)
			compositionRectangle: (compRectOrigin corner: compRectCorner)
			clippingRectangle: frame.
	medium displayParagraph: para at: frame origin + (leftInset@topInset)
"

	(Paragraph withText: text
			style: (TextStyle default copy alignment: alignment)
			compositionRectangle: (compRectOrigin corner: compRectCorner)
			clippingRectangle: frame)
		displayOn: medium at: frame origin + (leftInset@topInset)
! !

!TextThing methodsFor: 'as yet unclassified'!
flushRight
	alignment _ 1.
	^self! !


!ThingLabDefinition methodsFor: 'as yet unclassified'!
findInstanceInserters
	"return a list of paths to the inserters in my instance; if none are indicated, the instance
	 as a whole is its inserter"
		| inserterParts part inserterPart inserterPaths |
	inserterParts _ Array new.
	1 to: self class instSize do:
		[:i | part _ self instVarAt: i.
			part isInserter ifTrue:
				[inserterPart _ (part inserters at: 1) applyTo: part.
				inserterParts _ inserterParts copyWith: inserterPart]].
	inserterPaths _ inserterParts collect: [:x | instance pathTo: x].
	inserterPaths size=0
		ifTrue: [^Array with: EmptyPath]
		ifFalse: [^inserterPaths]! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
forgetConstraintMethods
	self forgetOwnConstraintMethods.
	instance forgetOwnConstraintMethods! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
prototypeName
		| tail className | 
	tail _ 'Definition'.
	className _ self class name.
	tail = (className copyFrom: className size-tail size+1 to: className size) ifFalse:
		[self error: 'bad class name for definition class'].
	^(className copyFrom: 1 to: className size-tail size) asSymbol! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
testSimple: planner
	"add additional test to inherited method - make sure the message isn't to my instance"
		| method |
	(super testSimple: planner) ifFalse: [^false].
	"passed inherited test - now check the method in the planner"
	method _ planner firstMethod.
	^(method receiver overlaps: (#instance) asPath) not! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
instanceConstraintPaths
	"return a collection of all paths to subparts of my instance that have constraints, 
	 sorted so that those that should be changed in preference to others come first"
		| paths |
	paths _ OrderedCollection new.
	self constraints do:
		[:c | c methodDescriptions do:
			[:m | m receiver firstName = #instance ifTrue: [paths add: m receiver]]].
	self merges do:
		[:m | m paths do:
			[:p | p firstName = #instance ifTrue: [paths add: p]]].
	"sort the paths by preference as to which part should be changed when there is a choice,
	 and then strip off the 'instance' name on the front of each path"
	^(self preferenceSort: paths) collect: [:p | p tail]! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
instance
	^instance! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
fileOutPrototype
	"file out the definition and instance classes"
		| fileStream |
	fileStream _ self makeFileOutFile.
	instance fileOutClassDefinitionOn: fileStream.
	self fileOutClassDefinitionOn: fileStream.
	instance fileOutStateOn: fileStream.
	self fileOutStateOn: fileStream.
	fileStream close.
	self class removeFromChanges.
	instance class removeFromChanges! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
setInstance: i
	instance _ i! !

!ThingLabDefinition methodsFor: 'as yet unclassified'!
isRecursive
	"return true if I am a recursive definition.  This will be the case if there is more than one
	 occurrence of my instance in my fields"
	^(self instancePaths: self instance class) size > 1! !


!ThingLabInstance methodsFor: 'as yet unclassified'!
forgetConstraintMethods
	self forgetOwnConstraintMethods.
	self definition forgetOwnConstraintMethods! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
constraintEnabled
	"temporary kludge - return true if I am enabled (overridden by recursive objects)"
	^true! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
constraints
	^super constraints , self class prototypeInstanceConstraints! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
nonRecursiveCompileSetCodeFor: sel fields: fields
	"version of compileSetCodefor: that does not allow for recursive definitions
	 (faster than recursive version, since definition doesn't need to be copied)

	 If for example sel is
		set.end2:
	then compile a method of the form
		set.end2: t1
			self copyFieldsFrom: 
				((self scratchDefinition primitiveSet.instance: self) set.instance.end2: t1) instance

	PROBLEM?  This used to be set.instance: instead of primitiveSet.instance:
	However, the way the constraint satisfier currently works, the instance will be in
	 a temporarily inconsistent state, which was getting a constraint-not-satisfied error.
	Does this fix always work?  TIME TO REWRITE THE CONSTRAINT SASTISFIER!!"

		| encoder args tempName arg defNode setDefNode newSel setDefSubPartNode
			getInstNode copyFieldsNode block methodNode |
	encoder _ Encoder new init: self class context: nil notifying: self.
	"make up argument nodes"
	args _ OrderedCollection new.
	1 to: fields size do: 
		[:i | tempName _ 't' , i printString.
		arg _ encoder bindTemp: tempName.
		args add: arg].
	defNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #scratchDefinition
		arguments: Array new
		precedence: #scratchDefinition precedence
		from: encoder.
	setDefNode _ MessageNode new
		receiver: defNode
		selector: #primitiveSet.instance:
		arguments: (Array with: (encoder encodeVariable: 'self'))
		precedence: #primitiveSet.instance: precedence
		from: encoder.
	"make a new selector that sets a subpart of the instance part of the definition"
	newSel _ 'set.'.
	(sel copyFrom: 7 to: sel size) asSymbol keywords do:
		[:k | newSel _ newSel , 'instance.' , k].
	newSel _ newSel asSymbol.
	setDefSubPartNode _ MessageNode new
		receiver: setDefNode
		selector: newSel
		arguments: args
		precedence: newSel precedence
		from: encoder.
	getInstNode _ MessageNode new
		receiver: setDefSubPartNode
		selector: #instance
		arguments: Array new
		precedence: #instance precedence
		from: encoder.
	copyFieldsNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #copyFieldsFrom:
		arguments: (Array with: getInstNode)
		precedence: #copyFieldsFrom: precedence
		from: encoder.
	block _ BlockNode new
		statements: (Array with: (ReturnNode new expr: copyFieldsNode))
		returns: true.
	methodNode _ MethodNode new selector: sel
		arguments: args precedence: sel precedence temporaries: #()
		block: block encoder: encoder primitive: 0.
	methodNode compileIn: self class! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
definition
	^ ThingLabDefinition prototype! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
forgetOwnConstraintMethods
	super forgetOwnConstraintMethods.
	self class resetPrototypeInstanceConstraints.
	self class resetPrototypeScratchDefinition.
	self class resetPrototypeInserters! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
compileSetCodeFor: sel fields: fields
	"overrides method inherited from Object to go to definition for correct behavior.
	 Checks if definition is recursive, to optimize for non-recursive case"
	self definition isRecursive
		ifTrue: [self recursiveCompileSetCodeFor: sel fields: fields]
		ifFalse: [self nonRecursiveCompileSetCodeFor: sel fields: fields]! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
oldNonRecursiveCompileSetCodeFor: sel fields: fields
	"version of compileSetCodefor: that does not allow for recursive definitions
	 (faster than recursive version, since definition doesn't need to be copied)

	 If for example sel is
		set.end2:
	then compile a method of the form
		set.end2: t1
			self copyFieldsFrom: 
				((self scratchDefinition set.instance: self) set.instance.end2: t1) instance"

		| encoder args tempName arg defNode setDefNode newSel setDefSubPartNode
			getInstNode copyFieldsNode block methodNode |
	encoder _ Encoder new init: self class context: nil notifying: self.
	"make up argument nodes"
	args _ OrderedCollection new.
	1 to: fields size do: 
		[:i | tempName _ 't' , i printString.
		arg _ encoder bindTemp: tempName.
		args add: arg].
	defNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #scratchDefinition
		arguments: Array new
		precedence: #scratchDefinition precedence
		from: encoder.
	setDefNode _ MessageNode new
		receiver: defNode
		selector: #set.instance:
		arguments: (Array with: (encoder encodeVariable: 'self'))
		precedence: #set.instance: precedence
		from: encoder.
	"make a new selector that sets a subpart of the instance part of the definition"
	newSel _ 'set.'.
	(sel copyFrom: 7 to: sel size) asSymbol keywords do:
		[:k | newSel _ newSel , 'instance.' , k].
	newSel _ newSel asSymbol.
	setDefSubPartNode _ MessageNode new
		receiver: setDefNode
		selector: newSel
		arguments: args
		precedence: newSel precedence
		from: encoder.
	getInstNode _ MessageNode new
		receiver: setDefSubPartNode
		selector: #instance
		arguments: Array new
		precedence: #instance precedence
		from: encoder.
	copyFieldsNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #copyFieldsFrom:
		arguments: (Array with: getInstNode)
		precedence: #copyFieldsFrom: precedence
		from: encoder.
	block _ BlockNode new
		statements: (Array with: (ReturnNode new expr: copyFieldsNode))
		returns: true.
	methodNode _ MethodNode new selector: sel
		arguments: args precedence: sel precedence temporaries: #()
		block: block encoder: encoder primitive: 0.
	methodNode compileIn: self class! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
recursiveCompileSetCodeFor: sel fields: fields
	"version of compileSetCodefor: that allows for recursive definitions

	 If for example sel is
		set.end2:
	then compile a method of the form
		set.end2: t1
			self copyFieldsFrom: 
				((self definition recopy primitiveSet.instance: self) set.instance.end2: t1) instance.

KLUDGED - see comment in nonrecursive version"

		| encoder args tempName arg defNode copyNode primitiveSetNode newSel setDefNode
			getInstNode copyFieldsNode block methodNode |
	encoder _ Encoder new init: self class context: nil notifying: self.
	"make up argument nodes"
	args _ OrderedCollection new.
	1 to: fields size do: 
		[:i | tempName _ 't' , i printString.
		arg _ encoder bindTemp: tempName.
		args add: arg].
	defNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #definition
		arguments: Array new
		precedence: #definition precedence
		from: encoder.
	copyNode _ MessageNode new
		receiver: defNode
		selector: #recopy
		arguments: Array new
		precedence: #recopy precedence
		from: encoder.
	primitiveSetNode _ MessageNode new
		receiver: copyNode
		selector: #primitiveSet.instance:
		arguments: (Array with: (encoder encodeVariable: 'self'))
		precedence: #primitiveSet.instance: precedence
		from: encoder.
	"make a new selector that sets the instance part of the definition"
	newSel _ 'set.'.
	(sel copyFrom: 7 to: sel size) asSymbol keywords do:
		[:k | newSel _ newSel , 'instance.' , k].
	newSel _ newSel asSymbol.
	setDefNode _ MessageNode new
		receiver: primitiveSetNode
		selector: newSel
		arguments: args
		precedence: newSel precedence
		from: encoder.
	getInstNode _ MessageNode new
		receiver: setDefNode
		selector: #instance
		arguments: Array new
		precedence: #instance precedence
		from: encoder.
	copyFieldsNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #copyFieldsFrom:
		arguments: (Array with: getInstNode)
		precedence: #copyFieldsFrom: precedence
		from: encoder.
	block _ BlockNode new
		statements: (Array with: (ReturnNode new expr: copyFieldsNode))
		returns: true.
	methodNode _ MethodNode new selector: sel
		arguments: args precedence: sel precedence temporaries: #()
		block: block encoder: encoder primitive: 0.
	methodNode compileIn: self class! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
makeInstanceConstraint
"Synthesize a new constraint that embodies the constraints that my definition puts on me.

The right way to do this isn't clear.  Other researchers (Steele, Gosling) do an exhaustive enumeration of all possible input and output sets, but they use a somewhat different formulation of what a constraint is.  For the moment, a simple approach is taken.  

The rule for the constraint is simply the 'and' of the rules for each of the constraints of the definition (what about parts that aren't there???)

The constraint methods are found as follows.  First, all the subparts of the instance that are constrained in some way by the definition are ennumerated.  Then a method for each subpart is constructed that just does a 'set' using all the other subparts as parameters."

		| constrainedParts encoder ruleTree methodTrees otherParts
		  primitiveSetSel setSel selfNode setNode enabledNode setBlock selfBlock
		  conditionalNode partValueNode |
	constrainedParts _ self definition instanceConstraintPaths.
	encoder _ Encoder new init: self class context: nil notifying: nil.
	"temporary kludge - don't include a test"
	ruleTree _ encoder encodeLiteral: true.
	"another kludge ... if the constrained parts are 
			(<part1 sub3 subsub8> <part2>) 
	 then the methods are:
			part1 sub3 primitiveSet.subsub8: 
				(self constraintEnabled ifTrue: [self set.part2 part2] ifFalse: [self])
					part1 sub3 subsub8
			self primitiveSet.part2:
				(self constraintEnabled ifTrue: [self set.part1.sub3.subsub8: part1 sub3 subsub8]
										ifFalse: [self]) part2"
	methodTrees _ constrainedParts collect: [:target |
		otherParts _ constrainedParts copyWithout: target.
		setSel _ 'set.' .
		otherParts do: [:p | setSel _ setSel , p dottedNameString , ':'].
		setSel _ setSel asSymbol.
		selfNode _ encoder encodeVariable: 'self'.
		setNode _ MessageNode new
			receiver: selfNode
			selector: setSel
			arguments: (otherParts collect: [:p | p code: encoder])
			precedence: setSel precedence
			from: encoder.
		enabledNode _ MessageNode new
			receiver: selfNode
			selector: #constraintEnabled
			arguments: Array new
			precedence: #constraintEnabled precedence
			from: encoder.
		setBlock _ BlockNode new statements: (Array with: setNode) returns: false.
		selfBlock _ BlockNode new statements: (Array with: selfNode) returns: false.
		conditionalNode _ MessageNode new
			receiver: enabledNode
			selector: #ifTrue:ifFalse:
			arguments: (Array with: setBlock with: selfBlock)
			precedence: #ifTrue:ifFalse: precedence
			from: encoder.
		partValueNode _ target code: encoder context: conditionalNode.
		primitiveSetSel _ ('primitiveSet.' , target names last , ':') asSymbol.
		MessageNode new
			receiver: (target allButLast code: encoder)
			selector: primitiveSetSel
			arguments: (Array with: partValueNode)
			precedence: primitiveSetSel precedence
			from: encoder].
	^InstanceConstraint
		noInsertOwner: self
		ruleTree: ruleTree
		testTree: nil
		errorTree: nil
		methodTrees: methodTrees! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
makeMethodFor: message
	"overrides method inherited from Object to go to definition for correct behavior"

	"this is redundant with the 'set.' method maker ... eventually this should be deleted"
		| definition defMessage defTransform encoder argNodes defNode copyNode changeItNode
			getInstNode copyFieldsNode method messageCopy transform recursive |
	definition _ self definition.
	"make up a message for the definition and make a plan for the definition to receive it"
	defMessage _ MessagePlan new
		context: definition
		receiver: (#(instance) asPath concat: message receiver)
		constraint:nil
		owner: EmptyPath
		keywords: message keywords
		arguments: message arguments
		uniqueState: message uniqueState
		referenceOnly: message referenceOnly
		compileTimeOnly: message compileTimeOnly.
	defTransform _ definition transform: defMessage.
	"build an encoder and declare the arguments"
	encoder _ Encoder new init: self class context: nil notifying: self.
	argNodes _ message arguments collect: [:a | encoder bindTemp: a].
	"make up a parse tree that gets my definition, copies it, and sends it the message just planned"
	recursive _ self definition isRecursive.
	defNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: (recursive ifTrue: [#definition] ifFalse: [#scratchDefinition])
		arguments: Array new
		precedence: #definition precedence
		from: encoder.
	copyNode _ MessageNode new
		receiver: defNode
		selector: (recursive ifTrue: [#copy.instance:] ifFalse: [#set.instance:])
		arguments: (Array with: (encoder encodeVariable: 'self'))
		precedence: #copy.instance: precedence
		from: encoder.
	changeItNode _ MessageNode new
		receiver: (defTransform receiver code: encoder context: copyNode)
		selector: defTransform selector
		arguments: argNodes
		precedence: defTransform selector precedence
		from: encoder.
	getInstNode _ MessageNode new
		receiver: changeItNode
		selector: #instance
		arguments: Array new
		precedence: #instance precedence
		from: encoder.
	copyFieldsNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #copyFieldsFrom:
		arguments: (Array with: getInstNode)
		precedence: #copyFieldsFrom: precedence
		from: encoder.
	method _ message makeMethod: (Array with: copyFieldsNode) temps: #() encoder: encoder.
	method compileIn: self class.
	messageCopy _ message noBackPointerCopy.
	"make a transformed message from the method"
	transform _ MessagePlan new
		context: nil
		receiver: EmptyPath
		constraint: nil
		owner: EmptyPath
		keywords: method selector keywords
		arguments: message arguments
		uniqueState: message uniqueState
		referenceOnly: message referenceOnly
		compileTimeOnly: message compileTimeOnly.
	"save this message in my dictionary of constraint satisfaction methods"
	self methods at: messageCopy put: transform.
	^transform! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
constraintCodeTo: fileStream
	"don't try to file out instance constraints - they will be regenerated automatically"! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
oldRecursiveCompileSetCodeFor: sel fields: fields
	"version of compileSetCodefor: that allows for recursive definitions

	 If for example sel is
		set.end2:
	then compile a method of the form
		set.end2: t1
			self copyFieldsFrom: 
				((self definition copy.instance: self) set.instance.end2: t1) instance"

		| encoder args tempName arg defNode copyNode newSel setDefNode
			getInstNode copyFieldsNode block methodNode |
	encoder _ Encoder new init: self class context: nil notifying: self.
	"make up argument nodes"
	args _ OrderedCollection new.
	1 to: fields size do: 
		[:i | tempName _ 't' , i printString.
		arg _ encoder bindTemp: tempName.
		args add: arg].
	defNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #definition
		arguments: Array new
		precedence: #definition precedence
		from: encoder.
	copyNode _ MessageNode new
		receiver: defNode
		selector: #copy.instance:
		arguments: (Array with: (encoder encodeVariable: 'self'))
		precedence: #copy.instance: precedence
		from: encoder.
	"make a new selector that sets the instance part of the definition"
	newSel _ 'set.'.
	(sel copyFrom: 7 to: sel size) asSymbol keywords do:
		[:k | newSel _ newSel , 'instance.' , k].
	newSel _ newSel asSymbol.
	setDefNode _ MessageNode new
		receiver: copyNode
		selector: newSel
		arguments: args
		precedence: newSel precedence
		from: encoder.
	getInstNode _ MessageNode new
		receiver: setDefNode
		selector: #instance
		arguments: Array new
		precedence: #instance precedence
		from: encoder.
	copyFieldsNode _ MessageNode new
		receiver: (encoder encodeVariable: 'self')
		selector: #copyFieldsFrom:
		arguments: (Array with: getInstNode)
		precedence: #copyFieldsFrom: precedence
		from: encoder.
	block _ BlockNode new
		statements: (Array with: (ReturnNode new expr: copyFieldsNode))
		returns: true.
	methodNode _ MethodNode new selector: sel
		arguments: args precedence: sel precedence temporaries: #()
		block: block encoder: encoder primitive: 0.
	methodNode compileIn: self class! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
scratchDefinition
	^self class prototypeScratchDefinition! !

!ThingLabInstance methodsFor: 'as yet unclassified'!
prototypeName
		| tail className | 
	tail _ 'Instance'.
	className _ self class name.
	tail = (className copyFrom: className size-tail size+1 to: className size) ifFalse:
		[self error: 'bad class name for instance class'].
	^(className copyFrom: 1 to: className size-tail size) asSymbol! !


!ThingLabLine methodsFor: 'as yet unclassified'!
pointNearest: pt
		| n |
	"insist that n be on me as a segment"
	n _ self asLineEquation pointNearest: pt.
	^(n max: (point1 min: point2)) min: (point1 max: point2)! !

!ThingLabLine methodsFor: 'as yet unclassified'!
printOn: strm
	strm print: point1.
	strm nextPutAll: ' line: '.
	strm print: point2! !

!ThingLabLine methodsFor: 'as yet unclassified'!
dist: pt
	 "return the shortest distance from me to pt"
	"this method takes care of the case of points near me as a LineEquation,
		but outside my endpoints.  See LineEquation dist: for a
		cheaper method"
	^(self pointNearest: pt) dist: pt! !

!ThingLabLine methodsFor: 'as yet unclassified'!
enclosingFrameOrNil
	^(point1 min: point2) - 1 corner: (point1 max: point2) + 1! !

!ThingLabLine methodsFor: 'as yet unclassified' stamp: 'di 1/21/1999 20:35'!
showPicture: medium
	medium
		drawLine: LineDrawingDot
		from: point1
		to: point2
		rule: Form over
		fillColor: nil! !

!ThingLabLine methodsFor: 'as yet unclassified'!
containsPoint: pt
	"cheap initial test"
	(self enclosingFrame containsPoint: pt) ifFalse: [^false].
	^(self dist: pt) < (6@6)! !

!ThingLabLine methodsFor: 'as yet unclassified'!
setPoint1: p1 point2: p2
	point1 _ p1.
	point2 _ p2! !

!ThingLabLine methodsFor: 'as yet unclassified'!
asLineEquation | A B |
	A _ point2 y asFloat - point1 y asFloat.
	B _ point1 x asFloat - point2 x asFloat.
	^LineEquation new A: A B: B through: point1! !

!ThingLabLine methodsFor: 'as yet unclassified' stamp: 'di 1/21/1999 20:34'!
showGrayBorder: medium
	medium drawLine: BorderDot
		from: point1
		to: point2
		rule: Form under
		fillColor: Color gray! !

!ThingLabLine methodsFor: 'as yet unclassified'!
length
	^point1 dist: point2! !

!ThingLabLine methodsFor: 'as yet unclassified'!
location
	"do computation in floating point"
	^ ((point1 x + point2 x) asFloat / 2) @
		((point1 y + point2 y) asFloat / 2)! !


!ThingLabObject class methodsFor: 'initialization'!
initialize  "ThingLabObject initialize"
	Instances _ Dictionary new.
	Definitions _ Dictionary new! !

!ThingLabObject class methodsFor: 'initialization'!
resetThingLabFields
	"reset constraints, merges, and fields - used when filing in a definition for the n-th time"
	prototype _ nil.
	prototypeConstraints _ nil.
	prototypeMerges _ nil.
	prototypeFieldDescriptions _ nil.
	prototypeInstancePathsDict _ nil! !

!ThingLabObject class methodsFor: 'instance creation' stamp: 'di 1/24/1999 11:14'!
readThing: stringOrText
        "read in a new thing from stringOrText."
                | strm nameStream char cl |
        strm _ ReadStream on: stringOrText asString.
        nameStream _ WriteStream on: (String new: 20).
        "the first token may be the name of the new thing's class.  If so, use
         the readFrom: message for that class; otherwise just evaluate the string"
        [strm atEnd or: [(char _ strm next) isSeparator]] whileFalse:
                [nameStream nextPut: char].
        nameStream isEmpty ifFalse:
                [cl _ Smalltalk at: nameStream contents asSymbol ifAbsent: [nil]].
        (cl isKindOf: Class)
                ifTrue: [^cl readFrom: strm]
                ifFalse: [^Compiler evaluate: stringOrText]! !

!ThingLabObject class methodsFor: 'ThingLab overrides'!
cleanDefinition
	"Like super.definition, but excludes Thinglab classvars and spare parts (ha ha)"
	| aStream |
	aStream _ WriteStream on: (String new: 300).
	aStream nextPutAll: (superclass == nil ifTrue: ['nil'] ifFalse: [superclass name]).
	aStream nextPutAll: self kindOfSubclass.
	self name storeOn: aStream.
	aStream cr; tab; nextPutAll: 'instanceVariableNames: '.
	aStream store: self cleanInstanceVariablesString.
	aStream cr; tab; nextPutAll: 'classVariableNames: '.
	aStream store: self classVariablesString.
	aStream cr; tab; nextPutAll: 'poolDictionaries: '.
	aStream store: self sharedPoolsString.
	aStream cr; tab; nextPutAll: 'category: '.
	(SystemOrganization categoryOfElement: self name) asString storeOn: aStream.
	^aStream contents! !

!ThingLabObject class methodsFor: 'ThingLab overrides'!
cleanInstanceVariablesString
	"Answer a string of my instance variable names separated by spaces."
		| strm descrs |
	strm _ WriteStream on: (String new: 100).
	descrs _ self prototype realFieldDescriptions.
	self superclass instSize+1 to: descrs size do:
		[:i | strm nextPutAll: (descrs at: i) name; space].
	^strm contents! !

!ThingLabObject class methodsFor: 'ThingLab overrides'!
compileAllFrom: oldClass
	| oldNames |
	"Thinglab overrides for fast growing of classes"
	oldNames _ oldClass instVarNames.
	oldNames = (self instVarNames copyFrom: 1 to: oldNames size)
		ifTrue: [^self].  "Do nothing if just adding a field"
	super compileAllFrom: oldClass! !

!ThingLabObject class methodsFor: 'ThingLab overrides'!
subclass: cl instanceVariableNames: instNames classVariableNames: classNames poolDictionaries: pools category: cat
		| newClass |
	newClass _ super subclass: cl instanceVariableNames: instNames
		classVariableNames: classNames poolDictionaries: pools category: cat.
	newClass resetThingLabFields.
	^newClass! !

!ThingLabObject class methodsFor: 'prototypes'!
definitions
	^Definitions! !

!ThingLabObject class methodsFor: 'prototypes'!
instances
	^Instances! !

!ThingLabObject class methodsFor: 'prototypes'!
makeNewPrototype
		| p |
	p _ self new.
	p bePrototypeInstance.
	^p! !

!ThingLabObject class methodsFor: 'prototypes'!
prototype
	prototype==nil ifTrue: [prototype _ self makeNewPrototype].
	^prototype! !

!ThingLabObject class methodsFor: 'fields'!
markNewPrototypeFieldsAsSpare
"I may have additional fields as a result of performing a 'grow' operation.  Mark them as spare"
		| oldDescriptions |
	oldDescriptions _ self prototypeFieldDescriptions.
	prototypeFieldDescriptions _ oldDescriptions ,
		((oldDescriptions size+1 to: self instSize) collect:
			[:i | SpareField name: ('part' , i printString) asSymbol index: i])! !

!ThingLabObject class methodsFor: 'fields'!
prototypeFieldDescriptions
	prototypeFieldDescriptions==nil ifTrue:
		[prototypeFieldDescriptions _ super prototypeFieldDescriptions].
	^prototypeFieldDescriptions! !

!ThingLabObject class methodsFor: 'fields'!
prototypeInstancePathsDict
	prototypeInstancePathsDict==nil ifTrue: [prototypeInstancePathsDict _ Dictionary new].
	^prototypeInstancePathsDict! !

!ThingLabObject class methodsFor: 'constraints and merges'!
addPrototypeConstraint: c
	prototypeConstraints _ self prototypeConstraints copyWith: c! !

!ThingLabObject class methodsFor: 'constraints and merges'!
addPrototypeMerge: m
	prototypeMerges _ self prototypeMerges copyWith: m! !

!ThingLabObject class methodsFor: 'constraints and merges'!
deletePrototypeConstraint: c
	prototypeConstraints _ self prototypeConstraints copyWithout: c! !

!ThingLabObject class methodsFor: 'constraints and merges'!
deletePrototypeMerge: m
	prototypeMerges _ self prototypeMerges copyWithout: m! !

!ThingLabObject class methodsFor: 'constraints and merges'!
prototypeConstraints
	prototypeConstraints==nil ifTrue: [prototypeConstraints _ superclass prototypeConstraints].
	^prototypeConstraints! !

!ThingLabObject class methodsFor: 'constraints and merges'!
prototypeMerges
	prototypeMerges==nil ifTrue: [prototypeMerges _ superclass prototypeMerges].
	^prototypeMerges! !

!ThingLabObject class methodsFor: 'constraints and merges'!
prototypeMethods
	prototypeMethods==nil ifTrue: [prototypeMethods _ Dictionary new].
	^prototypeMethods! !

!ThingLabObject class methodsFor: 'inserters and constrainers'!
prototypeConstrainers
	prototypeConstrainers==nil ifTrue: [prototypeConstrainers _ superclass prototypeConstrainers].
	^prototypeConstrainers! !

!ThingLabObject class methodsFor: 'inserters and constrainers'!
prototypeInserters
	prototypeInserters==nil ifTrue: [prototypeInserters _ superclass prototypeInserters].
	^prototypeInserters! !

!ThingLabObject class methodsFor: 'inserters and constrainers'!
setPrototypeConstrainers: c
	prototypeConstrainers _ c! !

!ThingLabObject class methodsFor: 'inserters and constrainers'!
setPrototypeInserters: i
	prototypeInserters _ i! !

!ThingLabObject class methodsFor: 'file out'!
fileOutThingLab   "ThingLabObject fileOutThingLab"
	"File out all classes and changes categorized as related to Thinglab.
	 This method doesn't file out prototypes, which must be handled separately."
		| sourceFile toInitialize meta |
	"toInitialize is a collection of classes that need to have their prototypes initialized"
	toInitialize _ OrderedCollection new.
	"file out changes to existing classes (should all be in protocols that start with 'Thing') "
	sourceFile _ FileStream newFileNamed: 'thinglab.st'.
	sourceFile timeStamp.
	"don't file out changes for prototype classes or classes in ThingLab categories"
	Smalltalk allClassesDo:
		[:class |
		('ThingLab*' match: class category) | (#Prototypes = class category) ifFalse:
			[(Array with: class with: class class) do:  "Both class and meta"
				[:each | each organization categories do:
					[:cat | ('ThingLab*' match: cat) ifTrue:  "File out ThingLab categories"
							[each fileOutCategory: cat on: sourceFile moveSource: false toFile: 0.
							"check if this class needs to have its prototype initialized"
							each isMeta & (each includesSelector: #initializePrototype) ifTrue:
								[toInitialize add: each]]]]]].
	sourceFile cr.  sourceFile cr.
	"file out new ThingLab classes (should all be in categories that start with 'Thing')  "
	(SystemOrganization categories select: [:x | 'ThingLab*' match: x]) do:
		[:cat | 	sourceFile cr.  sourceFile cr.
		SystemOrganization fileOutCategory: cat on: sourceFile.
		sourceFile cr].
	"add messages to initialize prototypes.  Initializations of existing system classes
		are done first to avoid ordering problems"
	sourceFile cr.  sourceFile cr.
	Smalltalk allClassesDo:
		[:each | meta _ each class.  
			(meta includesSelector: #initializePrototype) & (toInitialize includes: meta) not
				ifTrue: [toInitialize add: meta]].
	toInitialize do:
		[:c | sourceFile nextPutAll: c soleInstance name , ' initializePrototype!!'.
		sourceFile cr].
	"add code to change the system menu"
	sourceFile nextPutAll: 'ScreenController addThingLabItems!!'; cr.
	sourceFile close! !

!ThingLabObject class methodsFor: 'file out'!
forgetPrototypeChanges
	"remove all classes and changes in the 'Prototypes' category from Changes"
	(SystemOrganization listAtCategoryNamed: #Prototypes) do:
		[:n | (Smalltalk at: n) removeFromChanges]


	"	ThingLabObject forgetPrototypeChanges.
		(FileStream newFileNamed: 'thinglab-changes.st') fileOutChanges.   "! !


!BitImage class methodsFor: 'class initialization'!
initializePrototype  "BitImage initializePrototype"
		| p |
	p _ self prototype.
	p parts: 'frame'.
	p primitives: 'form'.
	p inserters: #('frame origin' 'frame corner').
	p assign.frame: (10@10 extent: 76@102).
	"set up a constraint between my frame and form extent"
	Constraint owner: p
		rule: 'form extent = frame extent'
		methods:  #('form changeExtent: frame extent'
					'frame reference').
	p assign.form:  (Form extent: 76@102
		fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 768 0 0 0 0 1920 0 0 0 0 4032 0 0 0 0 7008 0 0 0 0 13104 0 0 0 0 25368 0 0 0 0 49932 0 0 0 0 774 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 0 0 0 0 768 6 0 0 0 768 3 0 0 0 768 1 32768 0 0 768 0 49152 0 0 1023 65535 57344 0 0 1023 65535 57344 0 0 0 0 49152 0 0 0 1 32768 0 0 0 3 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 24640 512 0 0 0 8196 0 0 0 448 11470 1627 3610 28672 32 12868 548 33060 34816 480 8772 548 36644 63488 544 8772 548 37144 32768 544 8773 548 37152 34816 464 7394 1902 52892 28672 0 0 0 34 0 0 0 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	offset: 0@0)! !


!TextThing class methodsFor: 'class initialization'!
initializePrototype  "TextThing initializePrototype"
		| p |
	p _ self prototype.
	p parts: 'frame text'.
	p primitives: 'leftInset rightInset topInset bottomInset alignment'.
	p assign.frame: (10 @ 10 corner: 40 @ 28)
		text: 'text' asText
		leftInset: 2
		rightInset: 2
		topInset: 2
		bottomInset: -1000.
	p flushLeft.
	p inserters: #('frame origin' 'frame corner').
	p constrainers: #('frame')! !


!ThingLabDefinition class methodsFor: 'initialization'!
initializePrototype   "ThingLabDefinition initializePrototype"
	self prototype parts: 'instance'! !

!ThingLabDefinition class methodsFor: 'prototypes'!
makeNewPrototype
		| myName tail instanceClassName instanceClass p |
	myName _ self name.
	tail _ 'Definition'.
	tail = (myName copyFrom: myName size-tail size+1 to: myName size) ifFalse:
		[self error: 'bad name for definition class'].
	instanceClassName _ ((myName copyFrom: 1 to: myName size-tail size) , 'Instance') asSymbol.
	(Smalltalk includesKey: instanceClassName) ifFalse:
		[ThingLabInstance subclass: instanceClassName
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: 'Prototypes'].
	p _  self new.
	p bePrototypeDefinition.
	instanceClass _ Smalltalk at: instanceClassName.
	p setInstance: instanceClass prototype.
	instanceClass quickCompile: 'definition  ^' , myName , ' prototype'.
	^p! !


!ThingLabInstance class methodsFor: 'constraints'!
prototypeInstanceConstraints
	"return the single constraint on my instance that embodies the requirements of its definition"
	prototypeInstanceConstraints==nil ifTrue:
		["hack - to prevent infinite recursion temporarily set prototypeInstanceConstraints to 
		  the empty array"
		prototypeInstanceConstraints _ Array new.
		prototypeInstanceConstraints _ Array with: self prototype makeInstanceConstraint].
	^prototypeInstanceConstraints! !

!ThingLabInstance class methodsFor: 'constraints'!
resetPrototypeInstanceConstraints
	"set my instance constraint to nil (may no longer be valid)"
	prototypeInstanceConstraints _ nil! !

!ThingLabInstance class methodsFor: 'scratch definition'!
prototypeScratchDefinition
	"return a scratch instance of my definition"
	prototypeScratchDefinition==nil ifTrue:
		[prototypeScratchDefinition _ self prototype definition recopy].
	^prototypeScratchDefinition! !

!ThingLabInstance class methodsFor: 'scratch definition'!
resetPrototypeScratchDefinition
	"set my scratch definition to nil (may no longer be valid)"
	prototypeScratchDefinition _ nil! !

!ThingLabInstance class methodsFor: 'inserters and constrainers'!
prototypeInserters
	"check for graphically defined inserters in my definition"
	prototypeInserters==nil ifTrue:
		[prototypeInserters _ self prototype definition findInstanceInserters].
	^prototypeInserters! !

!ThingLabInstance class methodsFor: 'inserters and constrainers'!
resetPrototypeInserters
	"set my inserters to nil (may no longer be valid)"
	prototypeInserters _ nil! !


!ThingLabLine class methodsFor: 'class initialization'!
initialize   "ThingLabLine initialize"
	LineDrawingDot _ Form dotOfSize: 1	.
	BorderDot _ Form dotOfSize: 5! !

!ThingLabLine class methodsFor: 'class initialization'!
initializePrototype   "ThingLabLine initializePrototype"
		| p |
	p _ self prototype.
	p parts: 'point1 point2'.
	p setPoint1: 30 @ 30 point2: 50 @ 20.
	p inserters: #('point1' 'point2')! !

!ThingLabLine class methodsFor: 'instance creation'!
point1: p1 point2: p2
	^ self new setPoint1: p1 point2: p2! !


!Tool commentStamp: '<historical>' prior: 0!
I am a superclass for ThingLab editing tools.

Instance variables:
	context -- top-level owner of the thing being edited
	editedObject  -- thing being edited
	editedObjectPath -- path from context to editedPart
	editedView -- window in which the selection is shown and being edited
	otherViews -- other windows in which the selection is shown
	carrier -- sample of object to be inserted, deleted, or moved
	carrierShowCarrier -- selector for showing the carrier
	attachers -- vector of paths to attachers
	attacherObjects -- vector of attacher objects of the carrier (not the paths)
	attacher -- the current attacher
	attacherNum -- index of the current attacher in attachers
	carrierMove -- selector for a move method for the current attacher, or nil
	carrierReceiver -- object to which carrierMove is sent (this is a part of the carrier)
	selectionPart -- moving part in selection
	selectionPath -- path to moving part in selection, or nil
	selectionMove -- selector for a move method for moving parts of
			selection, or nil (this part is pointed to by movingPath)
	selectionReceiver -- object to which selectionMove is sent
	stickyParts -- parts of the selection which might merge with the current attacher
	stickyPaths -- vector of paths to sticky parts
	selectionMerges -- a vector, the same length as attachers.
			If the i-th attacher should merge with a part of the selection,
			(selectionMerges at: i) will be a path to the part in the selection
			 that it should merge with.  Otherwise (selectionMerges at: i) is nil.
	showForeground -- selector for showing the moving foreground of the selection, or nil!

!Tool methodsFor: 'initialization'!
carrier: thing
	carrier _ thing! !

!Tool methodsFor: 'initialization'!
context: c editedObject: eObject editedObjectPath: ePath editedView: v otherViews: ov carrierShowSelector: defaultSelector
	context _ c.
	editedObject _ eObject.
	editedObjectPath _ ePath.
	editedView _ v.
	otherViews _ ov.
	self setCarrierShowSelector: defaultSelector! !

!Tool methodsFor: 'initialization'!
setCarrierShowSelector: defaultSelector
	carrierShowSelector _ defaultSelector! !

!Tool methodsFor: 'scheduling'!
eachtime | offset firstBug |
	"each eachtime, allow the user to position the next attacher"
	self nextAttacher.
	attacher == false ifTrue: [^false].  "no more attachers"
	offset _ editedView insetDisplayBox topLeft.
	"keep showing the carrier until redbug is pressed or
		the mouse is outside the picture"
	[Sensor redButtonPressed] whileTrue: [].
	[Sensor redButtonPressed] whileFalse: 
		[firstBug _ Sensor cursorPoint.
		(editedView containsPoint: firstBug)
			ifFalse: [carrier _ nil. ^false].
		Sensor blueButtonPressed
			ifTrue: [carrier _ nil. ^false].
		self moveto: firstBug - offset].
	self positionAttacher: firstBug offset: offset.
	^ true! !

!Tool methodsFor: 'scheduling'!
firsttime
	self findAttachers.
	attacherNum _ 0.
	attacherObjects _ attachers collect: [:att | att applyTo: carrier].
	selectionMerges _ Array new: attachers size.
	"make sure the buffers have a copy of the picture"
	self copyToBuffers.
	^ true! !

!Tool methodsFor: 'scheduling'!
lasttime
	carrier ==  nil
	  ifTrue: ["don't perform action"  self copyFromBuffers. ^false]
	  ifFalse: [self performAction. ^true]! !

!Tool methodsFor: 'scheduling'!
startUp
	self firsttime ifFalse: [^false].
	[self eachtime] whileTrue.
	^self lasttime! !

!Tool methodsFor: 'structural changes'!
willChangeStructure
	"The structure of the selection is about to be changed.  Make sure its a prototype"
	editedObject isPrototype ifFalse:
		[self error: 'not a prototype'.

		"following code is obsolete: "
		editedObject _ editedObject asPrototype.
		editedView picturePane update.
		editedView buffer update]! !

!Tool methodsFor: 'moving'!
findSelectionMove
"If the current attacher should merge with a part in the selection, put that part in selectionPart, and a path to it in selectionPath.  Delete the part from stickyParts, and set selectionMove to a selector for moving that part in the selection.  Put the object which should receive this selector in selectionReceiver.  Find a selector for showing the moving foreground of the selection (if any)."
		| i message transform |
	i _ 1.
	[selectionPath == nil and: [i <= stickyParts size]] whileTrue:
		[((stickyParts at: i) containsPoint: attacher computedLocation)
		  ifTrue:
			[selectionPath _ stickyPaths at: i.
			selectionPart _ stickyParts at: i.
			stickyParts _ stickyParts copyWithout: selectionPart.
			stickyPaths _ stickyPaths copyWithout: selectionPath]
		  ifFalse: [i _ i + 1]].
	selectionPath == nil
	  ifFalse:
		[message _ MessagePlan new
			context: context
			receiver: (editedObjectPath concat: selectionPath)
			constraint: nil owner: EmptyPath
			keywords: #('moveby:') arguments: #('delta')
			uniqueState: true referenceOnly: false compileTimeOnly: false.
		transform _ context transform: message.
		selectionReceiver _ transform receiver applyTo: context.
		selectionMove _ transform selector.
		showForeground _ context
			foregroundSelector: editedView model showSelector
			message: message]! !

!Tool methodsFor: 'moving'!
moveto: pt
	"Move the current attacher to pt, and drag along the other attachers.
		Stick to any nearby sticky part.  If I stuck to something, don't
		blink the picture.  If selectionMove isn't nil, then also tell
		the selection to move one of its parts by performing this selector."
		| part newpt |
	part _ self stickyPartOrNil: pt.
	part==nil
		ifTrue: [newpt _ pt]
		ifFalse:
			[newpt _ part computedLocation.
			newpt=attacher computedLocation ifTrue:
				["I'm sticking to something, and haven't moved.  Don't
					blink the picture, and draw a box around the selected part"
				part showGrayBorder: editedView topMedium.
				^self]].
	carrierReceiver perform: carrierMove with: newpt - attacher computedLocation.
	selectionMove==nil ifTrue:
		[editedView copyFromBuffer.  self showCarrier.  ^self].
	selectionReceiver perform: selectionMove with: newpt - selectionPart computedLocation.
	self updatePictures.
	self showMovingCarrier! !

!Tool methodsFor: 'moving'!
nextAttacher
	"Get the next attacher from attacherObjects, and update sticky parts.
		Find the selector carrierMove for moving the new attacher.
		For the first attacher, this is simply a moveby: .
		If this isn't the first attacher, move the cursor to the next
		attacher."
	| message transform keywords |
	attacherNum _ attacherNum + 1.
	attacherNum > attacherObjects size
	  ifTrue: [attacher _ false]
	  ifFalse:
		[attacher _ attacherObjects at: attacherNum.
		self findStickyParts.
		keywords _ (attacherNum = 1
				  ifTrue: [#('moveby:' )]
				  ifFalse: [self attacherKeywords]).
		message _ MessagePlan new context: carrier receiver: EmptyPath constraint: nil owner: EmptyPath keywords: keywords arguments: #('delta' ) uniqueState: true referenceOnly: false compileTimeOnly: false.
		transform _ carrier transform: message.
		carrierMove _ transform selector.
		carrierReceiver _ transform receiver applyTo: carrier.
		attacherNum > 1
		  ifTrue: [editedView cursorlocGet: attacher computedLocation]]! !

!Tool methodsFor: 'moving'!
showCarrier
	carrier perform: carrierShowSelector with: editedView topMedium! !

!Tool methodsFor: 'moving'!
stickyPartOrNil: pt
	"I want to  move to pt.  Return nil if I didn't stick to anything, otherwise
	 the part to which I am sticking.  Also, keep track of whether I'm sticking
	 to something by setting the appropriate element of selectionMerges."
		| part |
	1 to: stickyParts size do:
		[:i | part _ stickyParts at: i.
		(part containsPoint: pt) ifTrue:
			[selectionMerges at: attacherNum put: (stickyPaths at: i).
			^part]].
	"I didn't stick to anything"
	selectionMerges at: attacherNum put: nil.
	^nil! !

!Tool methodsFor: 'showing'!
copyFromBuffers
	editedView copyFromBuffer.
	otherViews do: [:v | v copyFromBuffer]! !

!Tool methodsFor: 'showing'!
copyToBuffers
	editedView copyToBuffer.
	otherViews do: [:v | v copyToBuffer]! !

!Tool methodsFor: 'showing'!
showBuffered
	editedView updateBuffer.
	otherViews do: [:v | v updateBuffer].
	editedView copyFromBuffer.
	otherViews do: [:v | v copyFromBuffer]! !

!Tool methodsFor: 'showing'!
updatePictures
	"If there is a selector for showing the moving foreground
	 separately from the unchanging background, use it"
	showForeground==nil 
	   ifTrue: [self showBuffered]
	   ifFalse:
		[context perform: showForeground with: editedView buffer.
		otherViews do: [:v | context perform: showForeground with: v buffer].
		editedView copyFromBuffer.
		otherViews do: [:v | v copyFromBuffer]]! !

!Tool methodsFor: 'subclass defaults'!
action
	"return a string describing my action"
	self subclassResponsibility! !

!Tool methodsFor: 'subclass defaults'!
attacherKeywords 
	"return the keyword to use in positioning attachers other than the first"
	self subclassResponsibility! !

!Tool methodsFor: 'subclass defaults'!
findAttachers
	attachers _ Array with: EmptyPath! !

!Tool methodsFor: 'subclass defaults'!
findStickyParts
	"find parts that want to merge with the attacher"
	stickyPaths _attacher stickyPaths: editedObject.
	stickyParts _ stickyPaths collect: [:path | path applyTo: editedObject]! !

!Tool methodsFor: 'subclass defaults'!
performAction
	self copyFromBuffers! !

!Tool methodsFor: 'subclass defaults'!
positionAttacher: position offset: offset! !

!Tool methodsFor: 'subclass defaults'!
showMovingCarrier
	self showCarrier! !


!Deleter commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!Deleter methodsFor: 'tool protocol'!
action
	^'delete'! !

!Deleter methodsFor: 'tool protocol'!
performAction
	"delete anything the carrier stuck to"
	self willChangeStructure.
	selectionMerges do: [:m | m == nil ifFalse: [editedObject deletePath: m]].
	self showBuffered! !

!Deleter methodsFor: 'tool protocol'!
setCarrierShowSelector: defaultSelector
	carrierShowSelector _ #showDeleted:! !


!Inserter commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!Inserter methodsFor: 'tool protocol'!
action
	^'insert'! !

!Inserter methodsFor: 'tool protocol'!
attacherKeywords
	^Array with: ('moveInserter' , attacherNum printString , ':')! !

!Inserter methodsFor: 'tool protocol'!
findAttachers
	attachers _ carrier inserters! !

!Inserter methodsFor: 'tool protocol'!
findStickyParts
	"find parts that want to merge with the attacher"
	stickyPaths _attacher stickyPathsForInsert: editedObject.
	stickyParts _ stickyPaths collect: [:path | path applyTo: editedObject]! !

!Inserter methodsFor: 'tool protocol'!
performAction
		| path |
	self willChangeStructure.
	path _ carrier addTo: editedObject
		selectionMerges: selectionMerges attachers: attachers.
	"Check that all constraints are still satisfied.  If not, invoke constraint satisfaction.
	 This can be slow, so (hack) disable this if left shift key is pressed."
	Sensor leftShiftDown ifFalse: [editedObject checkAndSatisfyConstraints: path].
	self showBuffered! !

!Inserter methodsFor: 'tool protocol'!
positionAttacher: position offset: offset
	self moveto: Sensor cursorPoint - offset.
	[Sensor redButtonPressed] whileTrue: [].! !


!Constrainer commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!Constrainer methodsFor: 'tool protocol'!
action
	^'constrain'! !

!Constrainer methodsFor: 'tool protocol'!
attacherKeywords
	^Array with: ('moveConstrainer' + attacherNum printString + ':')! !

!Constrainer methodsFor: 'tool protocol'!
findAttachers
	attachers _ carrier constrainers! !


!MergingMover commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!MergingMover methodsFor: 'tool protocol'!
action
	^'merge'! !

!MergingMover methodsFor: 'tool protocol'!
performAction
	editedView copyFromBuffer.  "image is ok"
	selectionPath == nil
	  ifFalse:
		[ "merge the moving part with nearby parts of the selection"
		self willChangeStructure.
		selectionMerges do:
			[:m | m == nil
			  ifFalse: [editedObject merge: (Array with: m with: selectionPath)]]]! !

!MergingMover methodsFor: 'tool protocol'!
positionAttacher: position offset: offset
	self findSelectionMove.
	[Sensor redButtonPressed] whileTrue: [self moveto: Sensor cursorPoint - offset]! !

!MergingMover methodsFor: 'tool protocol'!
showMovingCarrier
	 "don't show carrier after moving starts"! !


!Mover commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!Mover methodsFor: 'tool protocol'!
action
	^'move'! !

!Mover methodsFor: 'tool protocol'!
positionAttacher: position offset: offset
	self findSelectionMove.
	stickyParts _ stickyPaths _ #().  "disable sticking"
	[Sensor redButtonPressed] whileTrue: [self moveto: Sensor cursorPoint - offset]! !

!Mover methodsFor: 'tool protocol'!
showMovingCarrier
	 "don't show carrier after moving starts"! !


!TextEditor commentStamp: '<historical>' prior: 0!
This class has not yet been commented!

!TextEditor methodsFor: 'text' stamp: 'di 1/23/1999 05:24'!
acceptText: newText from: controller 
	"the newly edited text has been accepted - check constraints"
		| t message transform |
	"NOTE: put in messages to change editedObject's margins here"

	"If left shift button is down, just change text; don't try to satisfy constraints"
	Sensor leftShiftDown ifTrue:
		[t _ (editedObjectPath concat: selectionPath) applyTo: context.
		t primitiveSet.text: newText.
		self updatePictures.
		^true].

"Kludge!!  Check the flag TextEditForce.  If it's true, then use 'primitiveSet.text' so that the edit is required; otherwise use changeValue: (see oldAcceptText:from:) which makes the edit a default instead"
	TextEditForce==true
		ifTrue: [message _ MessagePlan new context: context
			receiver: (editedObjectPath concat: selectionPath)
			constraint: nil owner: EmptyPath
			keywords: #('primitiveSet.text:') arguments: #('newtext')
			uniqueState: true referenceOnly: false compileTimeOnly: false]
		ifFalse: [message _ MessagePlan new context: context
			receiver: ((editedObjectPath concat: selectionPath) concat: #(text) asPath)
			constraint: nil owner: EmptyPath
			keywords: #('changeValue:') arguments: #('newtext')
			uniqueState: true referenceOnly: false compileTimeOnly: false].
	transform _ context transform: message.
	(transform receiver applyTo: context) perform: transform selector with: newText.
	"KLUDGE - try to find a showForeground selector.  If one can be found,
	assume background in buffer is OK, and just perform that selector"
	showForeground _ context
		foregroundSelector: editedView model showSelector
		message: message.
	self updatePictures.
	^true! !

!TextEditor methodsFor: 'text'!
oldAcceptText: newText from: controller
	"the newly edited text has been accepted - check constraints"
		| message transform |
	"NOTE: put in messages to change editedObject's margins here"
	"use changeValue rather than copyFrom, so that para is semialterable
	rather than unalterable"
	message _ MessagePlan new context: context
		receiver: ((editedObjectPath concat: selectionPath) concat: #(text) asPath)
		constraint: nil owner: EmptyPath
		keywords: #('changeValue:') arguments: #('newtext')
		uniqueState: true referenceOnly: false compileTimeOnly: false.
	transform _ context transform: message.
	(transform receiver applyTo: context) perform: transform selector with: newText.
	"KLUDGE - try to find a showForeground selector.  If one can be found,
	assume background in buffer is OK, and just perform that selector"
	showForeground _ context
		foregroundSelector: editedView model showSelector
		message: message.
	self updatePictures.
	^ true! !

!TextEditor methodsFor: 'text'!
text
	^ selectionPart text copy! !

!TextEditor methodsFor: 'text' stamp: 'di 1/23/1999 05:22'!
textMenu
	"TextEditor flushMenus"
	^ SelectionMenu
			labels: 'again\undo\copy\cut\paste\do it\print it\accept\cancel' withCRs
			lines: #(2 5 7 9)
			selections: #(again undo copySelection cut paste doIt printIt accept cancel)! !

!TextEditor methodsFor: 'tool protocol'!
action
	^'edit'! !

!TextEditor methodsFor: 'tool protocol'!
positionAttacher: position offset: offset
	"if the current attacher should merge with a part in the selection,
		start up its text editor."
	| i textView |
	i _ 1.
	[selectionPath == nil and: [i <= stickyParts size]] whileTrue:
		[((stickyParts at: i) enclosingFrame containsPoint: attacher computedLocation)
		  ifTrue:
			[selectionPath _ stickyPaths at: i.
			selectionPart _ stickyParts at: i.
			stickyParts _ stickyParts copyWithout: selectionPart.
			stickyPaths _ stickyPaths copyWithout: selectionPath]
		  ifFalse: [i _ i + 1]].
	selectionPath == nil ifTrue: [^self].
	"temporary kludge - ask the edited thing what sort of beast it is, and open an editor
	 accordingly.  This should instead be done by sending it an 'edit' message."
	(selectionPart isKindOf: TextThing) ifTrue:
		["start up the text editor in the sticky part of the selection"
		editedView copyFromBuffer.
		textView _ TextThingView new on: self aspect: #text change: #acceptText:from: menu: #textMenu.
		textView controller paragraph textStyle alignment: selectionPart alignment.
		textView borderWidth: 2.
		textView window: (selectionPart frame translateBy: editedView insetDisplayBox origin).
		textView display.
		textView controller startUp.
		textView release.
		^self].
	(selectionPart isKindOf: BitImage) ifTrue:
		[selectionPart form edit.
		^self].
	selectionPart inspect! !


AccessPath initialize!
Format initialize!
Relaxer initialize!
ThingLabObject initialize!
ThingLabLine initialize!
"Postscript:
Initialize and install prototypes for the basic thinglab objects."
Point initializePrototype.
Rectangle initializePrototype.
BitImage initializePrototype.
ThingLabLine initializePrototype.
TextThing initializePrototype.
ThingLabDefinition initializePrototype.
!

