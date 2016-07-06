"      NAME	TreeWorks80
       AUTHOR	ssadams@us.ibm.com (Lynn Fogwell and Ken Auer (Supported by Sam Adams))
       URL	(none)
       FUNCTION	A reusable tree node and hierarchy model complete with editors and views
       KEYWORDS	tree hierarchy editor st80 Squeak
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 1.23
       CONFLICTS	may need additional components from old KSC library
       DISTRIBUTION	world
       VERSION	1.1.23
       DATE	22-Sep-98

SUMMARY

This is the old TreeWorks code orignallywritten at Knowledge Systems Corporationby Lynn Fogwell and Ken Auer.This code was released by KSC in 1997.It has been ported from the original ST80to Squeak 1.23 by Sam Adams.

				Lynn Fogwell and Ken Auer (Supported by Sam Adams)
"!

'From Squeak 1.23 of October 4, 1997 on 22 September 1998 at 9:19:23 am'!
Object subclass: #KSCHierarchy
	instanceVariableNames: ''
	classVariableNames: 'Seed '
	poolDictionaries: ''
	category: 'KSC-Tree Structures'!
Object subclass: #KSCNodeGroup
	instanceVariableNames: 'nodeDictionary childrenMsg parentMsg '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Structures'!
MouseMenuController subclass: #KSCTreeController
	instanceVariableNames: ''
	classVariableNames: 'DownArrowCursor LeftArrowCursor RightArrowCursor UpArrowCursor '
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
Model subclass: #KSCTreeEditor
	instanceVariableNames: 'roots selectedNodes model aspectMsg changeMsg cutMsg pasteMsg updateMsg requestTreeMsg menuMsg isSingleSelection '
	classVariableNames: 'TreeEditorPasteBuffer '
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
KSCTreeEditor subclass: #KSCHierarchyEditor
	instanceVariableNames: 'restrictedNodes nodeGroup '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
KSCTreeEditor subclass: #KSCTreeEditorLocal
	instanceVariableNames: 'acceptTreeMsg treeOwner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
Object subclass: #KSCTreeExamples
	instanceVariableNames: 'modelMenu modelRoots modelSelection '
	classVariableNames: 'SampleTree '
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
KSCHierarchy subclass: #KSCTreeNode
	instanceVariableNames: 'parent children '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Structures'!
KSCTreeNode subclass: #KSCLinkedTreeNode
	instanceVariableNames: 'reference '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Structures'!
KSCHierarchy subclass: #KSCTreeNodeByReference
	instanceVariableNames: 'reference nodeGroup facade '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Structures'!
KSCLinkedTreeNode subclass: #KSCTreeNodeImage
	instanceVariableNames: 'nodeForm labelForm subtreeSize myOffset parentOffset selectArea isHidden displayMsg branchLength '
	classVariableNames: 'HideIndicator '
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
KSCTreeNodeImage subclass: #KSCTreeNodeAlignableImage
	instanceVariableNames: 'alignedWidth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!
nil subclass: #KSCTreePrunerSpecification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KSC-WindowDesigner v0.51'!
View subclass: #KSCTreeView
	instanceVariableNames: 'metaRoot canvas branchLength isVerticalTree isSingleSelection levelWidths selectedNodes fixedOffsets canvasOffset cutImages configMenu initializeHidden displaySelector '
	classVariableNames: 'RootsChecked '
	poolDictionaries: ''
	category: 'KSC-Tree Editor'!

!KSCHierarchy methodsFor: 'initialize-release'!
release
	"kwa -- 14 March 1989, unlink the tree and release all children in addition to the receiver."

	self removeFromParent. 
	self releaseChildren.
	super release.! !

!KSCHierarchy methodsFor: 'initialize-release'!
releaseChildren
	"kwa -- 14 March 1989, Remove and release all my children. Use copies of the children for the 'do:', since they will be releasing themselves."

	self children copy do:[:node| node release]! !

!KSCHierarchy methodsFor: 'initialize-release'!
unlinkChildren
	"kwa -- 14 March 1989, Remove and release all my children. Use a shallowCopy of the children for the 'do:', since they will be unlinking themselves (which removes them from the current collection)."

	self children shallowCopy do: [:node| node unlinkTree]! !

!KSCHierarchy methodsFor: 'initialize-release'!
unlinkTree
	"kwa -- 14 March 1989, unlink the receiver and all children from their parents."

	self removeFromParent. 
	self unlinkChildren.! !

!KSCHierarchy methodsFor: 'structure changes'!
addChild: aTreeNode
"add a TreeNode under me. Back-link it to me. Return boolean indication of success"

	(aTreeNode causesCyclicReference:self)  ifTrue:[^false].
	self children addLast: aTreeNode.
	aTreeNode parent: self.
	^true! !

!KSCHierarchy methodsFor: 'structure changes'!
addChild: aTreeNode before: aSiblingTreeNode
"add a TreeNode under me before the existing child aSiblingTreeNode. If aSiblingTreeNode is nil or not my kid, add aTreeNode last. Back-link aTreeNode to me. Return boolean indication of success"

	(aTreeNode causesCyclicReference:self)  ifTrue:[^false].
	(aSiblingTreeNode isNil or:[(self children includes: aSiblingTreeNode) not])
		ifTrue:[self children addLast: aTreeNode]
		ifFalse:[self children add: aTreeNode before: aSiblingTreeNode].
	aTreeNode parent: self.
	^true! !

!KSCHierarchy methodsFor: 'structure changes'!
addChildren: aCollectionOfTreeNodes
"add aCollectionOfTreeNodes under me. Back-link them to me. Return boolean indication of success"

	| oldChildren |
	oldChildren _ self children copy.
	aCollectionOfTreeNodes do:
		[:child| (self addChild: child) ifFalse:[self children: oldChildren.
										^false]].
	^true! !

!KSCHierarchy methodsFor: 'structure changes'!
removeChild:aTreeNode
"remove a TreeNode under me. Return a boolean of operation's success"

	self children remove:aTreeNode ifAbsent:[^false].
	aTreeNode parent:nil.
	self children isEmpty ifTrue:[self children: nil].
	^true! !

!KSCHierarchy methodsFor: 'structure changes'!
removeFromParent
"remove me from my parent TreeNode. Return a boolean of operation's success"

	self parent isNil
		ifTrue:[^false].
	self parent children remove:self ifAbsent:[^false].
	self parent:nil.
	^true! !

!KSCHierarchy methodsFor: 'private'!
causesCyclicReference:aTreeNode
"Return a boolean indicating any subtree reference to aTreeNode. Method is used to insure that new additions to a tree don't produce circular references" 

	self == aTreeNode
		ifTrue:[^true].
	self children
		do:[:child| child == aTreeNode
					ifTrue:[^true].
				(child causesCyclicReference:aTreeNode)
					ifTrue:[^true]].
	^false! !

!KSCHierarchy methodsFor: 'accessing'!
children
"return a collection of TreeNodes under me"

	self subclassResponsibility! !

!KSCHierarchy methodsFor: 'accessing'!
children: anObject
	"Set children to be anObject"

	self subclassResponsibility! !

!KSCHierarchy methodsFor: 'accessing'!
doesNotUnderstand: aMessage
	"If I don't understand, maybe my children will..."

	^self children perform: aMessage selector
				withArguments: aMessage arguments! !

!KSCHierarchy methodsFor: 'accessing'!
parent
"return my super node"

	self subclassResponsibility! !

!KSCHierarchy methodsFor: 'accessing'!
parent: aTreeNode
"save my super node."

	self subclassResponsibility! !

!KSCHierarchy methodsFor: 'accessing'!
root
"return the root node of any treeNode (i.e. the node that has no parent)"

	self parent isNil
		ifTrue:[^self]
		ifFalse:[^self parent root].! !

!KSCHierarchy methodsFor: 'customized accessing'!
ancestors
	"Answer all TreeNodes which are ancestors of the receiver."
	
	| ancestors |
	ancestors _ OrderedCollection new.
	self ancestorsDo: [:each | ancestors add: each].
	^ancestors! !

!KSCHierarchy methodsFor: 'customized accessing'!
ancestorsDetect: detectionBlock
	"kwa -- 15 April 1989,
	Answer the first ancestor in the hierarchy of the receiver for which detectionBlock evaluates to true.
	If none, answer nil."
	
	^self ancestorsDetect: detectionBlock ifNone: [nil]! !

!KSCHierarchy methodsFor: 'customized accessing'!
ancestorsDetect: detectionBlock ifNone: exceptionBlock
	"kwa -- 15 April 1989,
	Answer the first ancestor in the hierarchy of the receiver for which detectionBlock evaluates to true.
	If none, answer the value of exceptionBlock."
	
	self ancestorsDo: [:each | 
		(detectionBlock value: each)
			ifTrue: [^each]].
	^exceptionBlock value! !

!KSCHierarchy methodsFor: 'customized accessing'!
ancestorsDo: aBlock
	"kwa -- 15 April 1989,
	perform aBlock for every ancestor of the receiver."

	| myParent |
	myParent _ self parent.
	myParent == nil
		ifTrue: [^self].
	aBlock value: myParent.
	myParent ancestorsDo: aBlock.! !

!KSCHierarchy methodsFor: 'customized accessing'!
breadthDetect: detectionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the tree (where the receiver is the root) for which detectionBlock evaluates to true.  Traverse in breadthFirst order.
	If none, answer the nil."
	
	^self breadthDetect: detectionBlock ifNone: [nil]! !

!KSCHierarchy methodsFor: 'customized accessing'!
breadthDetect: detectionBlock ifNone: exceptionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the tree (where the receiver is the root) for which detectionBlock evaluates to true.  Traverse in breadthFirst order.
	If none, answer the value of exceptionBlock."
	
	self breadthDo: [:each | 
		(detectionBlock value: each)
			ifTrue: [^each]].
	^exceptionBlock value! !

!KSCHierarchy methodsFor: 'customized accessing'!
collect: aBlock
	"kwa -- 15 April 1989,
	Evaluate aBlock with each of the elements of the tree (where the receiver is the root) as the argument.  Collect the resulting values into a Set.  Answer the new set."

	| collectedSet |
	collectedSet _ Set new.
	self do: [:each | collectedSet add: (aBlock value: each)].
	^collectedSet! !

!KSCHierarchy methodsFor: 'customized accessing'!
collect: aBlock if: aQualifyingBlock
	"kwa -- 15 April 1989,
	Evaluate aQualifyingBlock with each of the elements of the tree (where the receiver is the root) as the argument.  If the result is true, collect the resulting values of aBlock (using the same element as the argument) into a new Set.  Answer the new Set."

	| collectedSet |
	collectedSet _ Set new.
	self do: [:each | 
		(aQualifyingBlock value: each)
			ifTrue: [collectedSet add: (aBlock value: each)]].
	^collectedSet! !

!KSCHierarchy methodsFor: 'customized accessing'!
commonSeedWith: aTreeNode
	"kwa -- 15 April 1989,
	Answer the node at which the ancestry of the receiver and aTreeNode meets.  If none, answer nil."
	
	| hisAncestors |
	(aTreeNode isOrInheritsFrom: self)
		ifTrue: [^self].
	(aTreeNode isAncestorOf: self)
		ifTrue: [^aTreeNode].
	hisAncestors _ aTreeNode ancestors.
	^self ancestorsDetect: [:eachAncestor |
		hisAncestors includes: eachAncestor]! !

!KSCHierarchy methodsFor: 'customized accessing'!
commonSeedWithAll: aCollectionOfTreeNodes
	"kwa -- 15 April 1989,
	Answer the type at which the ancestry of the receiver and each of the TreeNodes in aCollectionOfTreeNodes meet.  If none, answer nil."
	
	| potentialSeed |
	potentialSeed _ self.
	aCollectionOfTreeNodes do: [:eachNode |
		potentialSeed _ potentialSeed commonSeedWith: eachNode.
		potentialSeed == nil
			ifTrue: [^nil]].
	^potentialSeed! !

!KSCHierarchy methodsFor: 'customized accessing'!
descendants
	"Answer all TreeNodes which are descendants of the receiver."
	
	| descendants |
	descendants _ OrderedCollection new.
	self descendantsDo: [:each | descendants add: each].
	^descendants! !

!KSCHierarchy methodsFor: 'customized accessing'!
descendantsDetect: detectionBlock
	"kwa -- 15 April 1989,
	Answer the first descendant in the tree (where the receiver is the root) for which detectionBlock evaluates to true.
	If none, answer nil."
	
	^self descendantsDetect: detectionBlock ifNone: [nil]! !

!KSCHierarchy methodsFor: 'customized accessing'!
descendantsDetect: detectionBlock ifNone: exceptionBlock
	"kwa -- 15 April 1989,
	Answer the first descendant in the tree (where the receiver is the root) for which detectionBlock evaluates to true.
	If none, answer the value of exceptionBlock."
	
	self descendantsDo: [:each | 
		(detectionBlock value: each)
			ifTrue: [^each]].
	^exceptionBlock value! !

!KSCHierarchy methodsFor: 'customized accessing'!
descendantsDo: aBlock
	"kwa -- 15 April 1989,
	perform a depth-first operation (aBlock) on all the receiver's descendants."

	self children do: [:node | 
		aBlock value: node.
		node descendantsDo: aBlock]! !

!KSCHierarchy methodsFor: 'customized accessing'!
detect: detectionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the tree (where the receiver is the root) for which detectionBlock evaluates to true.
	If none, answer the nil."
	
	^self detect: detectionBlock ifNone: [nil]! !

!KSCHierarchy methodsFor: 'customized accessing'!
detect: detectionBlock ifNone: exceptionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the tree (where the receiver is the root) for which detectionBlock evaluates to true.
	If none, answer the value of exceptionBlock."
	
	self do: [:each | 
		(detectionBlock value: each)
			ifTrue: [^each]].
	^exceptionBlock value! !

!KSCHierarchy methodsFor: 'customized accessing'!
hierarchyDetect: detectionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the hierarchy of the receiver for which detectionBlock evaluates to true.
	If none, answer nil."
	
	^self hierarchyDetect: detectionBlock ifNone: [nil]! !

!KSCHierarchy methodsFor: 'customized accessing'!
hierarchyDetect: detectionBlock ifNone: exceptionBlock
	"kwa -- 15 April 1989,
	Answer the first node in the hierarchy of the receiver for which detectionBlock evaluates to true.
	If none, answer the value of exceptionBlock."
	
	self hierarchyDo: [:each | 
		(detectionBlock value: each)
			ifTrue: [^each]].
	^exceptionBlock value! !

!KSCHierarchy methodsFor: 'customized accessing'!
hierarchyDo: aBlock
	"kwa -- 15 April 1989,
	perform aBlock for every node in the hierarchy of the receiver, starting at the root."

	self ancestors reverseDo: aBlock.
	self depthDo: aBlock.! !

!KSCHierarchy methodsFor: 'customized accessing'!
select: aBlock
	"kwa -- 15 April 1989,
	Answer a set of all elements of the tree (where the receiver is the root) which return true when evaluated with aBlock."

	| selectSet |
	selectSet _ Set new.
	self do: [:each | (aBlock value: each) ifTrue: [selectSet add: each]].
	^selectSet! !

!KSCHierarchy methodsFor: 'customized accessing'!
withAncestors
	"kwa -- 15 April 1989,
	Answer all TreeNodes which are descendants of the receiver.  Also include the receiver."
	
	| withAncestors |
	withAncestors _ OrderedCollection new.
	self withAncestorsDo: [:each | withAncestors add: each].
	^withAncestors! !

!KSCHierarchy methodsFor: 'customized accessing'!
withAncestorsDo: aBlock
	"kwa -- 15 April 1989,
	perform aBlock for self and every ancestor of self."

	aBlock value: self.
	self ancestorsDo: aBlock! !

!KSCHierarchy methodsFor: 'customized accessing'!
withDescendants
	"kwa -- 15 April 1989,
	Answer all TreeNodes which are descendants of the receiver.  Also include the receiver."
	
	| withDescendants |
	withDescendants _ OrderedCollection new.
	self do: [:each | withDescendants add: each].
	^withDescendants! !

!KSCHierarchy methodsFor: 'customized accessing'!
withDescendantsDo: aBlock
	"kwa -- 15 April 1989,
	perform aBlock for self and every descendant of self."

	self do: aBlock! !

!KSCHierarchy methodsFor: 'copying'!
copyEmpty
	"Answer a new instance of the receiver without a parent or children.  Subclasses may wish to override."

	| copy |
	copy _ super copy.
	copy parent: nil.
	copy children: nil.
	^copy! !

!KSCHierarchy methodsFor: 'copying'!
copyTree
"return a copy of this node and all of its sub-nodes."

| copy | 
	copy _ self  copyEmpty.
	self children
		do:[:child| copy addChild: child copyTree].
	^copy! !

!KSCHierarchy methodsFor: 'deriving hierarchy'!
deriveChildrenFrom: aModel using: aSymbol
	"Add child nodes based on some current hierarchical structure already in place where the childModels of aModel (and, assumably, each of its childModels) can be retrieved by performing aSymbol"

	| childNode | 
	(aModel perform: aSymbol) do: [:childModel |
		childNode _ self copyEmpty.
		self addChild: childNode.
		childNode deriveChildrenFrom: childModel using: aSymbol].! !

!KSCHierarchy methodsFor: 'deriving hierarchy'!
deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol
	"Create the receiver's hierarchy by modelling the hierarchy of aModel whose parents and children are found using parentSymbol and childrenSymbol respectively.  Answer the node which is the root of the hierarchy."
	"KSCTreeNodeReference deriveHierarchyFrom: Magnitude parents: #superclass children: #subclasses"

	| root | 
	root _ self.
	parentSymbol == nil
		ifFalse:
			[root _ self deriveParentFrom: aModel using: parentSymbol].
	childrenSymbol == nil
		ifFalse:
			[self deriveChildrenFrom: aModel using: childrenSymbol].
	^root! !

!KSCHierarchy methodsFor: 'deriving hierarchy'!
deriveParentFrom: aModel using: aSymbol
	"Create a node and make the receiver a child of it, based on some current hierarchical structure already in place where the parentModel of aModel (and, assumably, its parentModel) can be retrieved by performing aSymbol.  Answer the root node of the entire resulting tree (the recursion stops when a nil parentModel is found"

	| parentModel parentNode | 
	parentModel _ aModel perform: aSymbol.
	parentModel == nil
		ifFalse:
			[parentNode _ self copyEmpty.
			parentNode addChild: self.
			^parentNode deriveParentFrom: parentModel using: aSymbol].
	^self! !

!KSCHierarchy methodsFor: 'traversing'!
breadthDo: aBlock
"Perform a breadth first operation on the receiver."

	| kids parents |
	aBlock value: self.
	kids _ self children.
	[kids isEmpty] whileFalse:
		[kids do: [:kid| aBlock value: kid].
		 parents _ kids.
		 kids _ OrderedCollection new.
		 parents do:[:dad| kids addAll: dad children]].! !

!KSCHierarchy methodsFor: 'traversing'!
depthDo:aBlock
"perform a depth-first operation (aBlock) on a (sub)tree node"

	aBlock value: self. 
	self children do:[:node| node depthDo:aBlock]! !

!KSCHierarchy methodsFor: 'traversing'!
do:aBlock
"default with a depth-first operation."

	^self depthDo: aBlock! !

!KSCHierarchy methodsFor: 'traversing'!
do:aBlock atLevel:anInteger
"Perform aBlock only for the children anInteger levels below this node. Level zero implies this node"

	anInteger == 0
		ifTrue:[aBlock value:self]
		ifFalse:[self children 
				do:[:child| child do:aBlock atLevel:anInteger - 1]].! !

!KSCHierarchy methodsFor: 'traversing'!
do: aBlock atLevel: anInteger onCondition: aTestBlock
"Perform aBlock only for the children anInteger levels below this node. If this node is a parent,  only continue towards level zero if aTestBlock evaluates to true."

	anInteger == 0
		ifTrue:[aBlock value:self]
		ifFalse:[(aTestBlock value: self)
					ifTrue:[self children 
							do:[:child| child do: aBlock 
										  atLevel: anInteger - 1 
										  onCondition: aTestBlock]]].! !

!KSCHierarchy methodsFor: 'traversing'!
do: aBlock toLevel: anInteger 
"Perform aBlock (two parameters) for all nodes down to anInteger levels below this node. A seperate method is used to track the current level of the nodes which is the first parameter to aBlock. The second parameter is the node itself"

	self do: aBlock toLevel: anInteger atLevel: 0! !

!KSCHierarchy methodsFor: 'traversing'!
do: aBlock toLevel: final atLevel: current
"Perform (two parameter) aBlock for all nodes down to 'final'.  'current' is the current level of this node and is included as the first parameter to aBlock. The second parameter is the node itself"

	aBlock value: current value: self.
	final <= current
		ifFalse:[self children 
				do:[:child| child do: aBlock
							  toLevel: final
							  atLevel: current + 1]].! !

!KSCHierarchy methodsFor: 'traversing'!
doToRoot: aBlock
"perform aBlock for self and every direct ancestor of self "
	aBlock value: self.
	self parent ~~ nil
		ifTrue:[self parent doToRoot: aBlock]! !

!KSCHierarchy methodsFor: 'traversing'!
traverseFalse: aBlock
"evaluate (two parameter) aBlock at node. If it returns false, evaluate it for each child. aBlock's parameters are (anInteger) level and self, respectively"

	self traverseFalse: aBlock level: 0.
	^self! !

!KSCHierarchy methodsFor: 'traversing'!
traverseFalse: aBlock level: anInteger
"evaluate aBlock with self and anInteger. If it returns false, continue with its children"

	(aBlock value: anInteger value: self)
		ifFalse:[self children
				do:[:child| child traverseFalse: aBlock level: anInteger + 1]].! !

!KSCHierarchy methodsFor: 'traversing'!
traverseTrue: aBlock
"evaluate (two parameter) aBlock at node. If it returns true, evaluate it for each child. aBlock's parameters are (anInteger) level and self, respectively"

	self traverseTrue: aBlock level: 0.
	^self! !

!KSCHierarchy methodsFor: 'traversing'!
traverseTrue: aBlock level: anInteger
"evaluate aBlock with anInteger and self, respectively. If it returns true, continue with its children"

	(aBlock value: anInteger value: self)
		ifTrue:[self children
				do:[:child| child traverseTrue: aBlock level: anInteger + 1]].! !

!KSCHierarchy methodsFor: 'traversing'!
upTo: aParentNode do: aBlock
"Perform aBlock for this node and all of its parents up to and including aParentNode"

	aBlock value:self.
	self == aParentNode
		ifFalse:[self parent upTo: aParentNode do: aBlock].! !

!KSCHierarchy methodsFor: 'testing'!
depth
	"kwa -- 15 April 1989,
	Answer the depth of the receiver."

	^self children inject: 1 into: [:maxDepth :child | maxDepth max: child depth + 1]! !

!KSCHierarchy methodsFor: 'testing'!
depthBelow
	"Answer the number of levels below the receiver."

	^self children inject: 0 into: [:maxDepth :child | maxDepth max: child depth]! !

!KSCHierarchy methodsFor: 'testing'!
hasChildren

	^self children isEmpty not! !

!KSCHierarchy methodsFor: 'testing'!
hasParent

	^self parent notNil! !

!KSCHierarchy methodsFor: 'testing'!
inheritsFrom: aTreeNode
	"kwa -- 15 April 1989,
	Answer whether aTreeNode inherits from the receiver."
	
	^aTreeNode isAncestorOf: self! !

!KSCHierarchy methodsFor: 'testing'!
isAncestorOf: aTreeNode
	"kwa -- 15 April 1989,
	Answer whether aTreeNode the receiver inherits from the receiver."
	
	^(aTreeNode ancestorsDetect: [:each | each == self]) ~~ nil! !

!KSCHierarchy methodsFor: 'testing'!
isLeaf

	^self children isEmpty! !

!KSCHierarchy methodsFor: 'testing'!
isOrInheritsFrom: aTreeNode
	"kwa -- 15 April 1989,
	Answer whether aTreeNode is or inherits from the receiver."
	
	^self == aTreeNode or: [self inheritsFrom: aTreeNode]! !

!KSCHierarchy methodsFor: 'testing'!
isRoot
"Return boolean indicating whether this node has a parent."

	^self parent isNil! !

!KSCHierarchy methodsFor: 'testing'!
level
	"Answer anInteger which indicates my level within the tree (where root is 1)."

	self parent isNil
		ifTrue:[^1]
		ifFalse:[^self parent level + 1]! !

!KSCHierarchy methodsFor: 'testing'!
levelId
	"Answer a collection which indicates the receiver's levelId with respect to its root."
	
	| levelIdCollection |
	levelIdCollection _ OrderedCollection new.
	self doToRoot: [:node |
		levelIdCollection addFirst: node localLevelId].
	levelIdCollection size > 1
		ifTrue: [levelIdCollection removeFirst].
	^levelIdCollection! !

!KSCHierarchy methodsFor: 'testing'!
localLevelId
	"kwa -- 15 April 1989,
	Answer the levelId or index (child number) of the receiver local to its parent.  If no parent, answer 0."
	
	| myParent |
	myParent _ self parent.
	^myParent == nil
		ifTrue: [0]
		ifFalse: [myParent children indexOf: self]! !

!KSCHierarchy methodsFor: 'printing'!
printHierarchy
	"kwa -- 15 April 1989,
	Answer a description of the receiver's hierarchy using tabs to show levels"
	"((KSCTreeNode deriveHierarchyFrom: Collection parents: #superclass children: #subclasses) detect: [:each | each level == 3]) printHierarchy
"

	| descriptionStream |
	descriptionStream _ (String new: 32) writeStream.
	self printHierarchyOn: descriptionStream.
	^descriptionStream contents! !

!KSCHierarchy methodsFor: 'printing'!
printHierarchyOn: aStream
	"kwa -- 15 April 1989,
	Put a description of the receiver's hierarchy on aStream.  Use the format:
root
	child1
		receiver
			receiver's child1
				(nextLevel)
			receiver's child2
			..."

	self hierarchyDo: [:eachNode |
		eachNode level - 1 timesRepeat: [aStream tab].
		eachNode printOn: aStream.
		aStream cr].
	aStream skip: -1.		"Strip off last cr."! !

!KSCHierarchy methodsFor: 'printing'!
printOn: aStream
	"kwa -- 15 April 1989,
	Put a description of the receiver on aStream.  Use the format:
		className ( level id )  -->  TreeNode ( 1 2 5 )."

	aStream nextPutAll: self class name.
	aStream nextPutAll: ' ( '.
	self levelId do: [:eachIndex |
		eachIndex printOn: aStream.
		aStream space].
	aStream nextPut: $).! !

!KSCHierarchy methodsFor: 'printing'!
printTree
	"kwa -- 15 April 1989,
	Answer a description of the receiver's tree (with receiver as root) using tabs to show levels"
	"((KSCTreeNode deriveHierarchyFrom: Collection parents: #superclass children: #subclasses) detect: [:each | each level == 3]) printTree"

	| descriptionStream |
	descriptionStream _ (String new: 32) writeStream.
	self printTreeOn: descriptionStream.
	^descriptionStream contents! !

!KSCHierarchy methodsFor: 'printing'!
printTreeOn: aStream
	"kwa -- 15 April 1989,
	Put a description of the receiver's tree (with the receiver as the root) on aStream.  Use the format:
receiver
	child1
		child's child1
		child's child2
	child2
..."

	| myLevel |
	myLevel _ self level.
	self do: [:eachNode |
		eachNode level - myLevel timesRepeat: [aStream tab].
		eachNode printOn: aStream.
		aStream cr].
	aStream skip: -1.		"Strip off last cr."! !


!KSCHierarchy class methodsFor: 'instance creation'!
deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol
	"Answer the root node of a hierarchy derived by copying the hierarchy of aModel.  Use parentSymbol and childrenSymbol to derive the node hierarchy."
	"KSCTreeNode deriveHierarchyFrom: Magnitude parents: #superclass children: #subclasses"

	^self new deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol! !

!KSCHierarchy class methodsFor: 'instance creation'!
newTreeFrom: aTreeNode inject: aBlock
"Build a new tree based on the structure of aTreeNode and execute the two parameter block at each node. The first parameter is a node from aTreeNode and the second is the corresponding new treeNode. The new tree dosen't necessarily need to be the class as aTreeNode"

| new | 
	new _ self  new.
	aBlock value: aTreeNode value: new.
	aTreeNode children
		do:[:child| new addChild: (self newTreeFrom: child inject: aBlock)].
	^new! !


!KSCNodeGroup methodsFor: 'accessing'!
childrenMsg
	"DEFAULT - Answer the instance variable, childrenMsg"

	^childrenMsg! !

!KSCNodeGroup methodsFor: 'accessing'!
childrenMsg: aSymbol
	"DEFAULT - Set childrenMsg to be aSymbol."

	childrenMsg _ aSymbol! !

!KSCNodeGroup methodsFor: 'accessing'!
nodeDictionary
"DEFAULT - Answer the instance variable, nodeDictionary"

	nodeDictionary == nil
		ifTrue:[nodeDictionary _ IdentityDictionary new].
	^nodeDictionary! !

!KSCNodeGroup methodsFor: 'accessing'!
nodeDictionary: aDictionary
"Set nodeDictionary to be aDictionary (or nil). Release any previously held nodes (I don't remove nodes from the dictionary because I assume that I am the only one using it and it gets replaced with aDictionary)."

	nodeDictionary ~~ nil
		ifTrue:[nodeDictionary keys do: [:anObj|
				(nodeDictionary at: anObj) nodeGroup: nil; reference: nil.
				nodeDictionary removeKey: anObj]].
	nodeDictionary _ aDictionary! !

!KSCNodeGroup methodsFor: 'accessing'!
parentMsg
	"DEFAULT - Answer the instance variable, parentMsg"

	^parentMsg! !

!KSCNodeGroup methodsFor: 'accessing'!
parentMsg: aSymbol
	"DEFAULT - Set parentMsg to be aSymbol."

	parentMsg _ aSymbol! !

!KSCNodeGroup methodsFor: 'private'!
children: childrenSymbol parent: parentSymbol nodeDictionary: aDictionary

	childrenMsg _ childrenSymbol.
	parentMsg _ parentSymbol.
	nodeDictionary _ aDictionary.! !


!KSCNodeGroup class methodsFor: 'instance creation'!
children: childrenSymbol parent: parentSymbol nodeDictionary: aDictionary

	^self new
		children: childrenSymbol
		parent: parentSymbol
		nodeDictionary: aDictionary! !


KSCTreeController comment:
'KSCTreeController  sends all view specific menu functions (hide, show, findNode...) to the view (KSCTreeView) and all default editing functions to the model (KSCTreeEditor). Any selectors it dosen''t respond to, go to the REAL model (TreeEditor''s model). The KSCTreeController will also build a default menu (from the default menus of the view and the editor) if no menu was specified from the REAL model.

instance variables:

(none)

class variables:

DownArrowCursor <Cursor> Arrow for rolling scroll.
LeftArrowCursor <Cursor> Arrow for rolling scroll.
RightArrowCursor <Cursor> Arrow for rolling scroll.
UpArrowCursor <Cursor> Arrow for rolling scroll.

issues:

1.  Vertical tree display hasn''t been implemented yet.
'!

!KSCTreeController methodsFor: 'scrolling'!
bottom: aPoint of: aRect
"Returns a relative point for an upward rolling scroll when the cursor is below the view."

| dist slope1 slope2 tmp |

	(dist _ aPoint y - aRect bottom) <= 0 ifTrue:[^nil].
	tmp _ aRect bottomLeft - aPoint. 
	slope1 _ tmp y negated / (self neverZero: tmp x).
	tmp _ aRect bottomRight - aPoint.
	slope2 _ tmp y negated / (self neverZero: tmp x).
	(slope1 between:0 and:1) not & (slope2 between:-1 and:0) not
		ifTrue:[self class arrowDown show.
			    ^0 @ ((dist // 6) + 1)]
		ifFalse:[^nil].! !

!KSCTreeController methodsFor: 'scrolling'!
doHandScroll: maxOffset inside: aRect
"Do a hand scroll while red button pressed and cursor is inside of tree view."

| oldPoint point relPoint newOffset |

	Cursor hand
		showWhile:
		    [oldPoint _ Sensor cursorPoint.
			[Sensor redButtonPressed & (aRect containsPoint:(point _ Sensor cursorPoint))]
				whileTrue:
				      [oldPoint = point
						ifFalse:
						     [relPoint _ point - oldPoint.
					 		newOffset _ (view canvas offset + relPoint min: 0 @ 0)
											max: (0@0) - maxOffset.
							view canvas offset:newOffset.
							view displayView.
							Processor yield.
							oldPoint _ point]]]! !

!KSCTreeController methodsFor: 'scrolling'!
doRollingScroll: maxOffset outside: aRect
"Do a rolling scroll while cursor is outside view with the red button pressed."

| oldCursor point relPoint newOffset |

	oldCursor _ Sensor currentCursor.
	[Sensor redButtonPressed & (aRect containsPoint:(point _ Sensor cursorPoint)) not]
		whileTrue:
		     [		(relPoint _ self top: point of: aRect) isNil
			ifTrue:[	(relPoint _ self left: point of: aRect) isNil
			ifTrue:[	(relPoint _ self right: point of: aRect) isNil
			ifTrue:[	(relPoint _ self bottom: point of: aRect) isNil
			ifTrue:[  relPoint _ 0 @ 0]]]].			
			newOffset _ (view canvas offset + relPoint min: 0 @ 0)
							max: (0@0) - maxOffset.
			view canvas offset:newOffset.
			view displayView.
			Processor yield].
	oldCursor show.! !

!KSCTreeController methodsFor: 'scrolling'!
left: aPoint of: aRect
"Returns a relative point for a right rolling scroll when the cursor is to the outside left of the view."

| dist slope1 slope2 tmp |

	(dist _ aRect left - aPoint x) < 0 ifTrue:[^nil].
	tmp _ aRect topLeft - aPoint.
	slope1 _ tmp y negated / (self neverZero: tmp x).
	tmp _ aRect bottomLeft - aPoint.
	slope2 _ tmp y negated / (self neverZero: tmp x).
	(slope1 >= -1) & (slope2 <= 1)
		ifTrue:[self class arrowLeft show.
			    ^((dist // -6) - 1) @ 0]
		ifFalse:[^nil].! !

!KSCTreeController methodsFor: 'scrolling'!
neverZero: aNum
"Never return a zero (value used in denominator)."
	
	aNum = 0
		ifTrue:[^1].
	^aNum! !

!KSCTreeController methodsFor: 'scrolling'!
right: aPoint of: aRect
"Returns a relative point for a left rolling scroll when the cursor is to the outside right of the view."

| dist slope1 slope2 tmp |

	(dist _ aPoint x - aRect right) <= 0 ifTrue:[^nil].
	tmp _ aRect topRight - aPoint.
	slope1 _ tmp y negated / (self neverZero: tmp x).
	tmp _ aRect bottomRight - aPoint.
	slope2 _ tmp y negated / (self neverZero: tmp x).
	(slope1 <= 1) & (slope2 >= -1)
		ifTrue:[self class arrowRight show.
			    ^((dist // 6) + 1) @ 0]
		ifFalse:[^nil].! !

!KSCTreeController methodsFor: 'scrolling'!
scroll
"A scroll has been requested. Perform scrolling as long as the red button is pressed"

| box maxOffset currentOffset |

	box _ view insetDisplayBox.
	maxOffset _ view canvas boundingBox extent - box extent.
	maxOffset x < 0 ifTrue:[maxOffset x:0].
	maxOffset y < 0 ifTrue:[maxOffset y:0].
	currentOffset _ view canvas offset.

	"if treeView canvas is out of bounds, jump it back into position..."
	currentOffset x > 0 | (currentOffset y > 0)
		ifTrue:[view	canvas offset: (currentOffset x min: 0) @ (currentOffset y min:0).
			    view	clearInside: Form white;
					displayView].
	[Sensor redButtonPressed]
		whileTrue:
			[self doHandScroll:   maxOffset inside:  box.
			 self doRollingScroll: maxOffset outside: box].! !

!KSCTreeController methodsFor: 'scrolling'!
top: aPoint of: aRect
"Returns a relative point for an downward rolling scroll when the cursor is above the view."

| dist slope1 slope2 tmp |

	(dist _ aRect top - aPoint y) < 0 ifTrue:[^nil].
	tmp _ aRect topLeft - aPoint. 
	slope1 _ tmp y negated / (self neverZero: tmp x).
	tmp _ aRect topRight - aPoint.
	slope2 _ tmp y negated / (self neverZero: tmp x).
	(slope1 between:-1 and:0) not & (slope2 between:0 and:1) not
		ifTrue:[self class arrowUp show.
			    ^0 @ ((dist // -6) - 1)]
		ifFalse:[^nil].! !

!KSCTreeController methodsFor: 'selecting'!
selecting: aTreeNodeImage

	| previousArea point | 
	view pickNode: aTreeNodeImage.
	previousArea _ aTreeNodeImage selectArea.
	[sensor redButtonPressed]
		whileTrue:[ (previousArea containsPoint: self translateCursor)
						ifFalse:[point _ self translateCursor.
							     view metaRoot traverseFalse:
									[:level :each| (each selectArea containsPoint: point)
													ifTrue:[view pickNode: each.
														    previousArea _ each selectArea].
												each isHidden]]]! !

!KSCTreeController methodsFor: 'control activity'!
redButtonActivity
	"red button activity indicates either a (de)selection or a scrolling request.
	kwa -- 25 April 1989, make sure its ok to change the selection before we do it."

| point |
	sensor redButtonPressed
		ifTrue:[	point _ self translateCursor.
				view metaRoot traverseFalse:
					[:level :each| 
					((each selectArea containsPoint: point) and: [model changeRequestFrom: view])
						ifTrue:[^self selecting: each].
					each isHidden].
				self scroll].! !

!KSCTreeController methodsFor: 'control activity'!
yellowButtonActivity
	"Determine which item in the yellow button hierarchical pop-up menu is selected. If the TreeEditor's model has a selector, perform it, otherwise build my own menu. For a selection, check if I can answer it, otherwise, forward it to the treeEditor's model."

	|  msg menu |
	(msg _ self model menuMsg) ~~ nil
		ifTrue:
			[menu _ (self model treeOwner perform: msg).
			 menu isNil
				ifTrue:[^self view flash]
				ifFalse:[msg _ menu startUpYellowButton]]
		ifFalse: 
			[msg _ self yellowButtonMenu startUp].
		msg ~= 0 
			ifTrue: [ (self localMenuItem: msg)
						ifTrue:[self perform: msg]
						ifFalse:[self model treeOwner perform: msg]]! !

!KSCTreeController methodsFor: 'private' stamp: 'ssa 12/3/97 12:25'!
translateCursor
"get the current cursor position and translate it to the canvas coordinates. Clamp position to the limits of the view"

	| viewOrigin viewCorner | 
	viewOrigin  _ (0@0) - view canvas offset.
	viewCorner _  view insetDisplayBox extent - view canvas offset.
	^(sensor cursorPoint - view insetDisplayBox origin - view canvas offset max: viewOrigin)
		min: viewCorner! !

!KSCTreeController methodsFor: 'private'!
yellowButtonMenu
	"build my hierarchical menu from view menus and model (editor) menus."

	yellowButtonMenu == nil
		ifTrue:[(self model cutMsg == #yourself) & (self model pasteMsg == #yourself)
					ifTrue:[yellowButtonMenu _ self view treeViewMenu]
					ifFalse:[yellowButtonMenu _ self view treeViewMenu joinWith: self model class editMenu]].
	^yellowButtonMenu! !

!KSCTreeController methodsFor: 'menu messages'!
accept

	self model accept! !

!KSCTreeController methodsFor: 'menu messages'!
align 

	view isThereAlignment
		ifTrue:[nil inform: 'tree is already aligned by levels']
		ifFalse:[Cursor execute
					showWhile: [view alignment: true]]! !

!KSCTreeController methodsFor: 'menu messages'!
cancel

	self model cancel! !

!KSCTreeController methodsFor: 'menu messages'!
copy
"copy all selected nodes to the paste buffer"

	Cursor execute
		showWhile:
			[(view fixOffsets)
				ifTrue:
					[model copyChildren.
					 self view flash]]! !

!KSCTreeController methodsFor: 'menu messages'!
create
"Fix offsets and notify the editor model that the user would like to create something."

	Cursor execute
		showWhile:
			[(view fixOffsets)
				ifTrue:
					[model model create]]! !

!KSCTreeController methodsFor: 'menu messages'!
cut
"cut all selected nodes"

	Cursor execute
		showWhile:
			[(view fixOffsets)
				ifTrue:
					[model cutChildren]]! !

!KSCTreeController methodsFor: 'menu messages'!
deAlign
"Removes tree alignment by levels, allowing faster browser response time."

	view isThereAlignment
		ifTrue:[Cursor execute
					showWhile: [view alignment: false]]
		ifFalse:[nil inform: 'Tree is not currently aligned by levels']! !

!KSCTreeController methodsFor: 'menu messages'!
deselectAll
	Sensor leftShiftDown
		ifTrue:[self model selectedNodes first inspect].
	view clearSelectedNodes! !

!KSCTreeController methodsFor: 'menu messages'!
findDisplayedNode
"The user is prompted for a pattern. DISPLAYED nodes are checked against the pattern and a menu is presented with all qualifying nodes. A selection here will present the node in the view."

	| string sortedNodes labels menu index |
	string _ FillInTheBlank request: 'Pattern match for node name?' initialAnswer: '*'.
	string = ''
		ifTrue:[^self].
	Cursor execute
		showWhile:[
			sortedNodes _ SortedCollection sortBlock: [:a :b| a labelString < b labelString].
			self view metaRoot children
				do: [:root| root traverseFalse:
							[:x :image| (string match: image labelString ignoreCase: false)
											ifTrue:[sortedNodes add: image].
										image isHidden]].
			sortedNodes isEmpty
				ifTrue:[Cursor normal show.
					    ^self inform: 'No matching nodes found'].
			sortedNodes size = 1 & (string includes: $*) not
				ifTrue:[Cursor normal show.
					    ^self view findNode: sortedNodes first].
			labels _ ''.
			sortedNodes do: [:each | labels _ labels , each labelString, '\' withCRs].
			"strip the last cr"
			labels _ labels copyFrom: 1 to: labels size - 1.
			menu _ PopUpMenu labels: labels].
	index _ menu startUp.
	index = 0
		ifFalse:[self view findNode: (sortedNodes at: index)].! !

!KSCTreeController methodsFor: 'menu messages'!
findNode
"The user is prompted for a pattern. ALL nodes are checked against the pattern and a menu is presented with all qualifying nodes. A selection here will present the node in the view."

	| string sortedNodes labels menu index |
	string _ FillInTheBlank request: 'Pattern match on ALL node names:' initialAnswer: '*'.
	string = ''  ifTrue:[^self].
	Cursor execute
		showWhile:[
			sortedNodes _ SortedCollection sortBlock: [:a :b| a labelString < b labelString].
			self view metaRoot children
				do: [:root| root depthDo:
							[:image| (string match: image labelString ignoreCase: false)
										ifTrue:[sortedNodes add: image]]].
			sortedNodes isEmpty
				ifTrue:[Cursor normal show.
					    ^self inform: 'No matching nodes found'].
			sortedNodes size = 1 & (string includes: $*) not
				ifTrue:[Cursor normal show.
					    ^self view findNode: sortedNodes first].
			labels _ ''.
			sortedNodes do: [:each | labels _ labels , each labelString, '\' withCRs].
			"strip the last cr"
			labels _ labels copyFrom: 1 to: labels size - 1.
			menu _ PopUpMenu labels: labels].
	index _ menu startUp.
	index = 0
		ifFalse:[self view findNode: (sortedNodes at: index)].! !

!KSCTreeController methodsFor: 'menu messages'!
hide
	Cursor execute
		showWhile:[view hide].! !

!KSCTreeController methodsFor: 'menu messages'!
hideAll
	Cursor execute
		showWhile:[view hideAll].! !

!KSCTreeController methodsFor: 'menu messages'!
hideAllAtLevel
"hide all of the nodes (displayed or not) at the input level"

| input |
	input _ FillInTheBlank request: 'hide all nodes at level:'.
	(input _ input asNumber) ~= 0
		ifTrue:[Cursor execute
					showWhile:[view hideAllAtLevel: input]].! !

!KSCTreeController methodsFor: 'menu messages'!
hideAllUnder
	Cursor execute
		showWhile:[view hideAllUnder].! !

!KSCTreeController methodsFor: 'menu messages'!
hideAtLevel
"hide all of the nodes currently displayed at the input level."

| input |
	input _ FillInTheBlank request: 'hide all displayed nodes at level:'.
	(input _ input asNumber) ~= 0
		ifTrue:[Cursor execute
					showWhile:[view hideAtLevel: input]].! !

!KSCTreeController methodsFor: 'menu messages'!
localMenuItem: aSymbol
	"Answer whether aSymbol is a menu item which is handled directly by the receiver."
	
	^#(accept align cancel copy create cut deAlign deselectAll findDisplayedNode findNode hide hideAll hideAllAtLevel hideAllUnder hideAtLevel newBranchLength paste printTreeBitmap selectionSwitch setVertHoriz show showAll showAllToLevel showAllUnder statDisplayed statTotal) includes: aSymbol! !

!KSCTreeController methodsFor: 'menu messages'!
newBranchLength
"prompt for and display a new branch length."

| ans | 
	ans _ FillInTheBlank request:'(minimum) branch length?'
					   initialAnswer: view branchLength printString.
	ans isEmpty
		ifFalse:[Cursor execute
				 showWhile:
				  [ans _ ans asNumber.
				   view branchLength: ans.
				   view displayView]].! !

!KSCTreeController methodsFor: 'menu messages'!
paste
"add the paste buffer under the currently selected node of add as new root(s) if nothing is selected."

| nodes parent | 
	(nodes _ view selectedNodesOfModel) size > 1
		ifTrue:[^nil inform: 'You can only paste under a single tree node\' withCRs,
						  'or add as root(s) if nothing is selected...'].
	Cursor execute
		showWhile:
			[nodes isEmpty
				ifTrue:[parent _ nil]
				ifFalse:[parent _ nodes at:1].
			 (view fixOffsets)
				ifTrue:[model pasteChildren]].! !

!KSCTreeController methodsFor: 'menu messages'!
printTreeBitmap
	"Put the tree bitmap out to the printer."

	| myPrinter |
	myPrinter _ KSCConfiguration defaultPrinter.
	(myPrinter open)
		ifTrue: 
			[self view putTreeBitmapFullPageOn: myPrinter.
			myPrinter close].! !

!KSCTreeController methodsFor: 'menu messages'!
selectionSwitch

	self view isSingleSelection
		ifTrue:[self view isSingleSelection: false]
		ifFalse:[self view isSingleSelection: true].! !

!KSCTreeController methodsFor: 'menu messages'!
setVertHoriz
"Switch between a vertical or horizontally displayed tree. not implemented yet..."

"	| ptr |  paintJet printout of tree.... expriment
	Cursor wait
		showWhile: 
			[ptr _ PrinterDriver default openPort.
			 ptr printMonochromeForm: self view canvas.
			 ptr interface release close]."! !

!KSCTreeController methodsFor: 'menu messages'!
show
	Cursor execute
		showWhile:[view show].! !

!KSCTreeController methodsFor: 'menu messages'!
showAll
	Cursor execute
		showWhile:[view showAll].! !

!KSCTreeController methodsFor: 'menu messages'!
showAllToLevel
"Prompt for a level, before which everything will be shown."

| input |
	input _ FillInTheBlank request: 'show all nodes out to level:'.
	(input _ input asNumber) ~= 0
		ifTrue:[Cursor execute
					showWhile:[view showAllToLevel: input]].! !

!KSCTreeController methodsFor: 'menu messages'!
showAllUnder
	Cursor execute
		showWhile:[view showAllUnder].! !

!KSCTreeController methodsFor: 'menu messages'!
statDisplayed
"inform the user of the displayed (sub)tree"

| root deep nodes |
	view selectedNodes size > 1
		ifTrue:[^nil inform: 'Select a SINGLE (sub)tree or\' withCRs,
						  'no selection for stats on all displayed nodes'].
	view selectedNodes isEmpty
		ifTrue:[root _ view metaRoot.
			    root isHidden: false.
			    deep _ 0.
			    nodes _ -1]
		ifFalse:[root _ view selectedNodes at: 1.
			    deep _ 1.
			    nodes _ 0].
	root traverseTrue: [:level :node| deep _ level max: deep.
								 nodes _ nodes + 1.
								 node isHidden not]
		level: deep.
	nil inform: 'Displayed tree(s) consists of\' withCRs, nodes printString,
			  ' nodes and ', deep printString, ' levels'.! !

!KSCTreeController methodsFor: 'menu messages'!
statTotal
"inform the user of the total (sub)tree."

| root deep nodes |
	view selectedNodes size > 1
		ifTrue:[^nil inform: 'Select a SINGLE (sub)tree or\' withCRs,
						  'no selection for stats on all nodes'].
	view selectedNodes isEmpty
		ifTrue:[root _ view metaRoot.
			    deep _ 0.
			    nodes _ -1]
		ifFalse:[root _ view selectedNodes at: 1.
			    deep _ 1.
			    nodes _ 0].
	root traverseTrue: [:level :node| deep _ level max: deep.
								 nodes _ nodes + 1. true]
		level: deep.
	nil inform: 'Tree(s) consists of\' withCRs, nodes printString,
			  ' nodes and ', deep printString, ' levels'.! !

!KSCTreeController methodsFor: 'control defaults'!
isControlActive

	^self viewHasCursor & Sensor blueButtonPressed not! !


!KSCTreeController class methodsFor: 'cursor access'!
arrowDown
	"Answer the instance of the receiver that is displayed when scrolling down."

	DownArrowCursor isNil
		ifTrue:[self initDownArrow].
	^DownArrowCursor! !

!KSCTreeController class methodsFor: 'cursor access'!
arrowLeft
	"Answer the instance of the receiver that is displayed when scrolling left."

	LeftArrowCursor isNil
		ifTrue:[self initLeftArrow].
	^LeftArrowCursor! !

!KSCTreeController class methodsFor: 'cursor access'!
arrowRight
	"Answer the instance of the receiver that is displayed when scrolling right."

	RightArrowCursor isNil
		ifTrue:[self initRightArrow].
	^RightArrowCursor! !

!KSCTreeController class methodsFor: 'cursor access'!
arrowUp
	"Answer the instance of the receiver that is displayed when scrolling up."

	UpArrowCursor isNil
		ifTrue:[self initUpArrow].
	^UpArrowCursor! !

!KSCTreeController class methodsFor: 'class initialization'!
initDownArrow
	DownArrowCursor _ (Cursor
			extent: 16@16
			fromArray: #(
		2r0
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r1111111111111110
		2r0111111111111100
		2r0011111111111000
		2r0001111111110000
		2r0000111111100000
		2r0000011111000000
		2r0000001110000000
		2r0000000100000000
		2r0)
			offset: -7@-15 name: 'down')! !

!KSCTreeController class methodsFor: 'class initialization'!
initialize
"define my yellow button menu and (re) initialize cursors"
"KSCTreeController initialize. 	KSCTreeController allInstancesDo:[:each| each initialize]"

	DownArrowCursor _ nil.
	UpArrowCursor _ nil.
	LeftArrowCursor _ nil.
	RightArrowCursor _ nil.! !

!KSCTreeController class methodsFor: 'class initialization'!
initLeftArrow
	LeftArrowCursor _ (Cursor
			extent: 16@16
			fromArray: #(
		2r0000000010000000
		2r0000000110000000
		2r0000001110000000
		2r0000011110000000
		2r0000111110000000
		2r0001111111111111
		2r0011111111111111
		2r0111111111111111
		2r0011111111111111
		2r0001111111111111
		2r0000111110000000
		2r0000011110000000
		2r0000001110000000
		2r0000000110000000
		2r0000000010000000
		2r0)
			offset: -1 @ -7 name: 'left')! !

!KSCTreeController class methodsFor: 'class initialization'!
initRightArrow
	RightArrowCursor _ (Cursor
			extent: 16@16
			fromArray: #(
		2r0000000100000000
		2r0000000110000000
		2r0000000111000000
		2r0000000111100000
		2r0000000111110000
		2r1111111111111000
		2r1111111111111100
		2r1111111111111110
		2r1111111111111100
		2r1111111111111000
		2r0000000111110000
		2r0000000111100000
		2r0000000111000000
		2r0000000110000000
		2r0000000100000000
		2r0)
			offset: -15 @ -7 name: 'right')! !

!KSCTreeController class methodsFor: 'class initialization'!
initUpArrow
	UpArrowCursor _ (Cursor
			extent: 16@16
			fromArray: #(
		2r0
		2r0000000100000000
		2r0000001110000000
		2r0000011111000000
		2r0000111111100000
		2r0001111111110000
		2r0011111111111000
		2r0111111111111100
		2r1111111111111110
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0000011111000000
		2r0)
			offset: -7@-1 name: 'up')! !


KSCTreeEditor comment:
'KSCTreeEditor is a fully interactive hierarchical editor. The model communicates with the editor through a generic hierarchy representation (KSCTreeNodeReference). The model receives requests  from the user (through the editor), makes any desired changes and, then notifies the editor of changes (#changed:with:) using the updateMsg and the changed generic tree nodes. The model is expected to actually create, cut and, add the nodes.

The selectors for communication are established when the TreeView is first opened by the parameters of the initial message (on:tree:...). Each selector is used for communication to or from the editor. When received by the model, it is a request (from the user). After the model performs its functions, it broadcasts any hierarchy or selection changes (#changed:with:) using the ''updateMsg'' selector. On an update, the view will check the passed node(s) for changes in its children or its parent. If no structure changes are found, the view assumes that the node display has changed and redisplays the node.  All parameters used in the broadcast messages are in terms of the generic tree (KSCTreeNodeReferences). 

To better understand the editor''s interface, run the examples found in the class methods.

instance variables:

roots <OrderedCollection> All root nodes of the generic trees (KSCTreeNodeReference) 
		   				 being edited.
model <your class> The model to whom all of the following messages will be sent.
aspectMsg <Symbol> No parameter message, requesting all of the model''s current selections 					(OrderedCollection). If single select is active (aKSCTreeView singleSelect: true),
					the editor expects a single node.
changedMsg <Symbol> Single parameter message sent to the model suggesting enclosed
					 orderedCollection as the new selections. If single select is active, this will be
					a single node.
selectedNodes <OrderedCollection> currently selected nodes.
cutMsg <Symbol>	No parameter message sent to the model. Indicates that the user has requseted a cut.
pasteMsg <Symbol> A 1 or no parameter message indicates that the user is requesting a paste. The
					parameter will be an ordered collection from the editor''s paste buffer.
createMsg <Symbol> No parameter message sent to the model from the create menu item. 
					This message is sent to the model when the user wants a new node.
					The model will likely send a subsequent updateMsg.
updateMsg <Symbol> Single parameter message broadcast by the application to the view whenever
					a tree node has somehow changed. The parameter can be a node or a
					collection of nodes. If no structure changes are found for the changed node,
					it is simply redisplayed by the view (eg. node was renamed).
requestTreeMsg <Symbol> No parameter message. Editor requests a generic tree to use. Model
					returns either a single generic node or a Collection of generic nodes. If
					the model ever wishes to send a completely new tree ( self changed:
					requestTreeMsg), the editor will respond with a ''perform: requestTreeMsg''
					to the model.
menuMsg <Symbol>    If ~~ nil, it is a selector to send to the model for all menus, otherwise
					default treeVew menus will be used.

class variables:

PasteBuffer <OrderedCollection> This is the paste buffer for all of the TreeEditors. Allows cross view pasting and buffer remains after all windows are closed. If the application would like to use the services of this paste buffer, he must make the pasteMsg a single parameter message and add the cut messages to the paste buffer himself (KSCTreeView pasteBuffer: aCollectionOfTreeNodes).

issues:

1.  interface cuts and adds should all be ordered collections. Minimizes treeView redisplays (i.e. cut four nodes and redisplay at end).
2. when tree editor window is closed no indication needs to be given to the model.
3. '!

!KSCTreeEditor methodsFor: 'accessing'!
aspectMsg
	"DEFAULT - Answer the instance variable, aspectMsg"

	^aspectMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
aspectMsg: anObject
	"DEFAULT - Set aspectMsg to be anObject."

	aspectMsg _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
changeMsg
	"DEFAULT - Answer the instance variable, changeMsg"

	^changeMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
changeMsg: anObject
	"DEFAULT - Set changeMsg to be anObject."

	changeMsg _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
cutMsg
	"DEFAULT - Answer the instance variable, cutMsg"

	cutMsg == nil
		ifTrue:[^#yourself].
	^cutMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
cutMsg: anObject
	"DEFAULT - Set cutMsg to be anObject."

	cutMsg _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
isSingleSelection
	"DEFAULT - Answer the instance variable, isSingleSelection"

	isSingleSelection == nil
		ifTrue:[isSingleSelection _ true].
	^isSingleSelection! !

!KSCTreeEditor methodsFor: 'accessing'!
isSingleSelection: aBoolean
	"DEFAULT - Set isSingleSelection to be anaBoolean."

	isSingleSelection _ aBoolean! !

!KSCTreeEditor methodsFor: 'accessing'!
menuMsg
	"DEFAULT - Answer the instance variable, menuMsg"

	^menuMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
menuMsg: aSymbol
	"The model wants total control of the menus. The tree controller performs aSymbol (on yellowButtonActivity) to revrieve the current model menu."

	menuMsg _ aSymbol! !

!KSCTreeEditor methodsFor: 'accessing'!
model
	model isNil
		ifTrue:[^self].
	^model! !

!KSCTreeEditor methodsFor: 'accessing'!
model: anObject

	model == nil
		ifFalse: [model removeDependent: self].
	model _ anObject.
	anObject notNil & anObject ~~ self
		ifTrue:[model addDependent: self].! !

!KSCTreeEditor methodsFor: 'accessing'!
pasteMsg
"Answer the instance variable, pasteMsg"

	pasteMsg == nil
		ifTrue:[^#yourself].
	^pasteMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
pasteMsg: anObject
"DEFAULT - Set pasteMsg to be anObject."

	pasteMsg _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
requestTreeMsg
	"DEFAULT - Answer the instance variable, requestTreeMsg"

	^requestTreeMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
requestTreeMsg: anObject
	"DEFAULT - Set requestTreeMsg to be anObject."

	requestTreeMsg _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
roots

	roots isNil
		ifTrue:[self roots: self getRoots].
	^roots! !

!KSCTreeEditor methodsFor: 'accessing'!
roots: anObject
	"DEFAULT - Set roots to be anObject."

	roots _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
selectedNodes
"If I don't have selectedNodes, ask my model what they are. selectedNodes will always be an OrderedCollection. This will also indicate that nothing is selected. In single select, a nil indicates nothing is selected. In multiple select, an empty collection means nothing is selected."

	selectedNodes == nil
		ifTrue:[selectedNodes _ self getSelections].
	^selectedNodes! !

!KSCTreeEditor methodsFor: 'accessing'!
selectedNodes: anObject
	"DEFAULT - Set selectedNodes to be anObject."

	selectedNodes _ anObject! !

!KSCTreeEditor methodsFor: 'accessing'!
updateMsg
	"DEFAULT - Answer the instance variable, updateMsg"

	^updateMsg! !

!KSCTreeEditor methodsFor: 'accessing'!
updateMsg: aSymbol
	"DEFAULT - Set updateMsg to be aSymbol."

	updateMsg _ aSymbol! !

!KSCTreeEditor methodsFor: 'scheduling'!
open
	"Open a tree browser window with the default display options"
	self openWithLabel: 'Tree Browser'! !

!KSCTreeEditor methodsFor: 'scheduling'!
openWithLabel: aLabel
	"Open a tree browser window with the default display options"
	| topView theTreeView |
	topView _ StandardSystemView
				model: self
				label: aLabel
				minimumSize: 100 @ 100.
	theTreeView _ KSCTreeView
						on: self
						aspect: #roots
						change: #treeStructure.
	topView
		addSubView: theTreeView
		in: (0 @ 0 extent: 1 @ 1)
		borderWidth: 1.
	topView controller open! !

!KSCTreeEditor methodsFor: 'tree functions'!
copyChildren
"copy the selected subtrees to the pasteBuffer without changing anything else"

	self pasteBuffer: self selectedNodes.! !

!KSCTreeEditor methodsFor: 'tree functions'!
cutChildren
	"Cut aCollectionOfNodes from their parents."

	self model perform: self cutMsg.! !

!KSCTreeEditor methodsFor: 'tree functions'!
doUpdate: anObject
"anObject can be aTreeNode, aCollection or, nil. A nil indicates that the roots have changed"

	| nodes | 
	(anObject isKindOf: Collection)
		ifTrue:[nodes _ anObject asOrderedCollection]
		ifFalse:[nodes _ OrderedCollection with: anObject].

"When there is a change, I always make sure I have the latest update of the roots"
	self roots: nil.

	self changed:#updateNodes: with: nodes.
	self changed: #treeStructure.! !

!KSCTreeEditor methodsFor: 'tree functions'!
pasteChildren
	"Add the pasteBuffer to whatever is selected."

	self pasteMsg numArgs = 0
		ifTrue:[self model perform: self pasteMsg]
		ifFalse:[self model perform: self pasteMsg
						 with: self takePasteBuffer].! !

!KSCTreeEditor methodsFor: 'selecting'!
changeRequest
	"Pass changeRequests on to the receiver's model."

	self model ~~ self
		ifTrue: [^self model changeRequest].
	^true! !

!KSCTreeEditor methodsFor: 'selecting'!
changeRequest: anAspectSymbol 
	"Pass changeRequests on to the receiver's model."

	self model ~~ self
		ifTrue: [^self model changeRequest: anAspectSymbol].
	^true! !

!KSCTreeEditor methodsFor: 'selecting'!
changeRequestFrom: aRequestorObject
	"Pass changeRequests on to the receiver's model."

	self model ~~ self
		ifTrue: [^true "self model changeRequestFrom: aRequestorObject"].
	^true! !

!KSCTreeEditor methodsFor: 'selecting'!
deselectNodes: aCollectionOfTreeNodes
"The view is asking me to deselect aCollectionOfTreeNodes. If all of them are already deselected, I won't bother the model."

	| notifyModel |
	notifyModel _ false.
	aCollectionOfTreeNodes
		do:[:node| (self selectedNodes includes: node)
					ifTrue:[self selectedNodes remove: node.
							notifyModel _ true]].
	notifyModel
		ifTrue:[self isSingleSelection
				ifTrue:[self model perform: self changeMsg with: nil]
				ifFalse:[self model perform: self changeMsg with: self selectedNodes copy]].! !

!KSCTreeEditor methodsFor: 'selecting'!
doSelections
"compare the model's current selection(s) with his old selection(s). Tell the view to deselect and/or select any differences"

	|  newSelections oldSelections differences | 
	oldSelections _ self selectedNodes.
	newSelections _ self getSelections.

	self selectedNodes: newSelections copy.

	differences _ oldSelections reject: [:old| newSelections includes: old].
	differences isEmpty
		ifFalse:[self changed: #deselectNodes: with: differences].

	differences _ newSelections reject: [:new| oldSelections includes: new].
	differences isEmpty
		ifFalse:[self changed: #selectNodes: with: differences].! !

!KSCTreeEditor methodsFor: 'selecting'!
getSelectedNodes
"Asked for by the view to (re)initialize his selections"

	^self selectedNodes! !

!KSCTreeEditor methodsFor: 'selecting'!
selectNodes: aCollectionOfTreeNodes
"I have the model's latest selection(s). If the view is asking me to select something that the model already has selected, I won't bother the model with it."

	| notifyModel |
	notifyModel _ false.
	aCollectionOfTreeNodes
		do:[:node| (self selectedNodes includes: node)
						ifFalse:[self isSingleSelection
									ifTrue:[self selectedNodes:
													(OrderedCollection with: node)]
									ifFalse:[self selectedNodes add: node].
								notifyModel _ true]].
	notifyModel
		ifTrue:[self isSingleSelection
					ifTrue:[self model perform: self changeMsg with: self selectedNodes first]
					ifFalse:[self model perform: self changeMsg with: self selectedNodes copy]].! !

!KSCTreeEditor methodsFor: 'updating'!
update: aSymbol with: aParm

	aSymbol == self aspectMsg
		ifTrue:[self doSelections].
	aSymbol == self updateMsg
		ifTrue:[self doUpdate: aParm].
	aSymbol == self requestTreeMsg
		ifTrue:[self roots: nil.
			    self changed: #newTree].! !

!KSCTreeEditor methodsFor: 'private'!
breakDependents
	"Since I serve as an indirect model, and no one is dependent on me anymore, break the dependency on my model."

	super breakDependents.
	self model ~~ self
		ifTrue:[self model removeDependent: self].
	model _ nil.! !

!KSCTreeEditor methodsFor: 'private'!
getRoots

	| modelTree | 
	modelTree _ self model perform: self requestTreeMsg.
	(modelTree isKindOf: Collection)
		ifTrue:[^modelTree copy]
		ifFalse:[^OrderedCollection with: modelTree].! !

!KSCTreeEditor methodsFor: 'private'!
getSelections
"Get the model's selections"

	| modelResponse |
	modelResponse _ self model perform: self aspectMsg.
	self isSingleSelection
		ifTrue:[modelResponse isNil
					ifTrue:[^OrderedCollection new]
					ifFalse:[^OrderedCollection with: modelResponse]]
		ifFalse:[(modelResponse isKindOf: Collection)
					ifTrue:[^modelResponse asOrderedCollection]
					ifFalse:[^OrderedCollection with: modelResponse]].! !

!KSCTreeEditor methodsFor: 'private'!
nodeClass

	^self class nodeClass! !

!KSCTreeEditor methodsFor: 'private'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol cutRequest: cutReqSymbol  pasteRequest: pasteReqSymbol updateNode: updateSymbol menu: menuSymbol

	self model: aModel.
	self requestTreeMsg: treeSymbol.
	self aspectMsg: aspectSymbol.
	self changeMsg: changeSymbol.
	updateSymbol == nil
		ifFalse: [	self cutMsg: cutReqSymbol.
				self pasteMsg: pasteReqSymbol.
				self updateMsg: updateSymbol].
	self menuMsg: menuSymbol.! !

!KSCTreeEditor methodsFor: 'private'!
treeOwner
"My model is the owner of the tree. This message is for the treeController when he is trying to forward a menu selection to my model."

	^self model! !

!KSCTreeEditor methodsFor: 'buffer access'!
pasteBuffer

	^self class pasteBuffer! !

!KSCTreeEditor methodsFor: 'buffer access'!
pasteBuffer: aCollection

	self class pasteBuffer: aCollection! !

!KSCTreeEditor methodsFor: 'buffer access'!
takePasteBuffer

	^self class takePasteBuffer! !


KSCHierarchyEditor comment:
'Uses KSCTreeNodeAbstractions to communicate with the model in terms of its own objects. This editor is selected when a treeView is plugged in as: on:seeds:parent:children:updateNode:menu: .

instance variables:

restrictedNodes <ExpectedClass> not used yet.
childrenMsg <Symbol> selector to send to get the model node''s children.
parentMsg <Symbol> selector to send to get the model node''s parent.
nodeDictionary <Dictionary> holds all of the treeNodeAbstractions for this view.

class variables:

(none)

issues:'!

!KSCHierarchyEditor methodsFor: 'accessing'!
nodeGroup
"Answer a nodeGroup (defined at Editor creation)"

	^nodeGroup! !

!KSCHierarchyEditor methodsFor: 'accessing'!
nodeGroup: aKSCNodeGroup
	"DEFAULT - Set nodeGroup to be aKSCNodeGroup."

	nodeGroup _ aKSCNodeGroup! !

!KSCHierarchyEditor methodsFor: 'accessing'!
restrictedNodes
	"DEFAULT - Answer the instance variable, restrictedNodes"

	restrictedNodes == nil
		ifTrue:[restrictedNodes _ Set new].
	^restrictedNodes! !

!KSCHierarchyEditor methodsFor: 'accessing'!
restrictedNodes: aCollection
	"DEFAULT - Set restrictedNodes to be aCollection."

	restrictedNodes _ aCollection! !

!KSCHierarchyEditor methodsFor: 'tree functions'!
copyChildren
"I won't copy abstract nodes..."

	^self! !

!KSCHierarchyEditor methodsFor: 'tree functions'!
doUpdate: anObject
"anObject can be aTreeNode, aCollection or, nil. A nil indicates that the roots have changed"

	| nodes | 
	(anObject isKindOf: Collection)
		ifTrue:[nodes _ anObject asOrderedCollection]
		ifFalse:[nodes _ OrderedCollection with: anObject].

	nodes _ nodes collect: [:obj| obj isNil "request for root update"
									ifTrue:[nil]
									ifFalse:[self findAbstractionFor: obj]].

"When there is any change, I always make sure I have the latest update of the roots"
	self roots: nil.

	self changed:#updateNodes: with: nodes.
	self changed: #treeStructure.! !

!KSCHierarchyEditor methodsFor: 'selecting'!
deselectNodes: aCollectionOfTreeNodes
"The view is asking me to deselect aCollectionOfTreeNodes. If all of them are already deselected, I won't bother the model."

	| notifyModel |
	notifyModel _ false.
	aCollectionOfTreeNodes
		do:[:node| (self selectedNodes includes: node)
					ifTrue:[self selectedNodes remove: node.
							notifyModel _ true]].
	notifyModel
		ifTrue:[self isSingleSelection
				ifTrue:[self model perform: self changeMsg with: nil]
				ifFalse:[self model perform: self changeMsg
								 with: (self selectedNodes collect:[:n| n reference])]].! !

!KSCHierarchyEditor methodsFor: 'selecting'!
selectNodes: aCollectionOfTreeNodes
"I have the model's latest selection(s). If the view is asking me to select something that the model already has selected, I won't bother the model with it."

	| notifyModel newSelection |
	notifyModel _ false.
	aCollectionOfTreeNodes
		do:[:node| (self selectedNodes includes: node)
						ifFalse:[self isSingleSelection
									ifTrue:[self selectedNodes:
													(OrderedCollection with: node)]
									ifFalse:[self selectedNodes add: node].
								notifyModel _ true]].
	notifyModel
		ifTrue:[self isSingleSelection
					ifTrue:[self model perform: self changeMsg with: self selectedNodes first reference]
					ifFalse:[self model perform: self changeMsg
									 with: (self selectedNodes collect:[:n| n reference])]].! !

!KSCHierarchyEditor methodsFor: 'private'!
breakDependents
	"Since I manage my own abstract tree, release my nodes."

	super breakDependents.
	nodeGroup == nil
		ifFalse:[nodeGroup nodeDictionary: nil].
	nodeGroup _ nil! !

!KSCHierarchyEditor methodsFor: 'private'!
findAbstractionFor: anObject
"find or create the treeNodeAbstraction for anObject."

	^self nodeGroup nodeDictionary
		at: anObject
		ifAbsent:[^self nodeGroup nodeDictionary
					at: anObject
					put: (KSCTreeNodeByReference
							deriveHierarchyFrom: anObject
							nodeGroup: self nodeGroup)]! !

!KSCHierarchyEditor methodsFor: 'private'!
getRoots
"Build hierarchy of treeNodeReferences parallel to the model's hierarchy. I expect a Collection of roots"

	| realRoots myRoots abstraction |
	realRoots _ self treeOwner perform: self requestTreeMsg.
	myRoots _ OrderedCollection new.

	realRoots do:[:realRoot|
		abstraction _ self findAbstractionFor: realRoot.
		abstraction isFakeRoot: true.
		myRoots add: abstraction].
	^myRoots! !

!KSCHierarchyEditor methodsFor: 'private'!
getSelections
"Get the model's selections"

	| modelResponse |
	modelResponse _ self model perform: self aspectMsg.
	self isSingleSelection
		ifTrue:[modelResponse isNil
					ifTrue:[^OrderedCollection new]
					ifFalse:[^OrderedCollection
								with:(self findAbstractionFor: modelResponse)]]
		ifFalse:[^modelResponse collect: [:obj| self findAbstractionFor: obj]].! !

!KSCHierarchyEditor methodsFor: 'private'!
parent: parentSymbol children: childrenSymbol
"Establish all common treeNodeByReference parameters (can be overridden by individual nodes...)"

	self nodeGroup: (KSCNodeGroup
						children: childrenSymbol
						parent: parentSymbol
						nodeDictionary: IdentityDictionary new)! !


!KSCTreeEditor class methodsFor: 'buffer access'!
clearPasteBuffer
	"Empty the PasteBuffer"
	"KSCTreeEditor clearPasteBuffer"

	TreeEditorPasteBuffer == nil
		ifFalse: [TreeEditorPasteBuffer do: [:node | node release]].
	TreeEditorPasteBuffer _ nil! !

!KSCTreeEditor class methodsFor: 'buffer access'!
pasteBuffer
	"look at the PasteBuffer without altering it."
	"KSCTreeEditor pasteBuffer"

	TreeEditorPasteBuffer == nil
		ifTrue: [TreeEditorPasteBuffer _ OrderedCollection new].
	^TreeEditorPasteBuffer! !

!KSCTreeEditor class methodsFor: 'buffer access'!
pasteBuffer: oldNodes
	"Set the PasteBuffer to oldNodes. Release any previous nodes held in buffer. Keep the old paste buffer contents around if oldNodes is empty. Everything that goes to the paste buffer is a copy!!"

	| keep |
	(oldNodes isKindOf: Collection)
		ifTrue:[oldNodes isEmpty
				ifTrue:[^self]
				ifFalse:[keep _ oldNodes]]
		ifFalse:[oldNodes isNil
				ifTrue:[^self]
				ifFalse:[keep _ OrderedCollection with: oldNodes]].
	self pasteBuffer isEmpty
		ifFalse:[ (TreeEditorPasteBuffer at:1) == (keep at:1)
					ifTrue:[^self].
				TreeEditorPasteBuffer do: [:node | node release]].

	TreeEditorPasteBuffer _ keep collect:[:root| root copyTree]! !

!KSCTreeEditor class methodsFor: 'buffer access'!
takePasteBuffer
	"Answer the PasteBuffer and set it to nil."

	| hold |
	hold _ self pasteBuffer.
	TreeEditorPasteBuffer _ nil.
	^hold! !

!KSCTreeEditor class methodsFor: 'instance creation'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol cutRequest: cutReqSymbol   pasteRequest: pasteReqSymbol  updateNode: updateSymbol menu: menuSymbol

	^self new on: aModel
			tree: treeSymbol
			aspect: aspectSymbol
			change: changeSymbol
			cutRequest: cutReqSymbol
			pasteRequest: pasteReqSymbol
			updateNode: updateSymbol
			menu: menuSymbol! !

!KSCTreeEditor class methodsFor: 'defaults'!
editMenu
"Return the standard edit menu"

	^self menuClass
		labelList: #(('cut' 'copy' 'paste under'))
		selectors: #(cut copy paste)"
		helpIdentifier: self menuHelpIdentifier"! !

!KSCTreeEditor class methodsFor: 'defaults'!
menuClass

	^ActionMenu! !

!KSCTreeEditor class methodsFor: 'defaults'!
nodeClass

	^KSCTreeNode! !


!KSCHierarchyEditor class methodsFor: 'instance creation'!
on: aModel tree: treeSym aspect: aspectSym change: changeSym cutRequest: cutReqSym   pasteRequest: pasteReqSym  updateNode: updateSym menu: menuSym parent: parentSym children: childrenSym

	| editor |
	editor _ self new.
	editor	on: aModel
			tree: treeSym
			aspect: aspectSym
			change: changeSym
			cutRequest: cutReqSym
			pasteRequest: pasteReqSym
			updateNode: updateSym
			menu: menuSym.
	editor	parent: parentSym
			children: childrenSym.
	^editor! !


!KSCTreeEditorLocal methodsFor: 'accessing'!
acceptTreeMsg
	"DEFAULT - Answer the instance variable, acceptTreeMsg"

	acceptTreeMsg == nil
		ifTrue:[^#update:].
	^acceptTreeMsg! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
acceptTreeMsg: anObject
	"DEFAULT - Set acceptTreeMsg to be anObject."

	acceptTreeMsg _ anObject! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
roots

	roots isNil
		ifTrue:[self roots: self getCopyOfRoots].
	^roots! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
roots: anObject
	"Since I have my own private copy of a tree as a local editor, release any previously held tree."

	roots ~~ nil
		ifTrue:[ roots do: [:each| each release]].

	roots _ anObject! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
selectedNodes
	"DEFAULT - Answer the instance variable, selectedNodes"

	selectedNodes == nil
		ifTrue:[selectedNodes _ OrderedCollection new].
	^selectedNodes! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
treeOwner
	"DEFAULT - Answer the instance variable, treeOwner"

	^treeOwner! !

!KSCTreeEditorLocal methodsFor: 'accessing'!
treeOwner: anObject
	"DEFAULT - Set treeOwner to be anObject."

	treeOwner == nil
		ifFalse: [self treeOwner removeDependent: self].
	treeOwner _ anObject.
	treeOwner addDependent: self.! !

!KSCTreeEditorLocal methodsFor: 'private'!
breakDependents
	"Since I serve as a direct model, release my copy of the roots."

	super breakDependents.
	self treeOwner ~~ nil
		ifTrue: [self treeOwner removeDependent: self].
	self roots do: [:root | root release].! !

!KSCTreeEditorLocal methodsFor: 'private'!
getCopyOfRoots

	| fromOwner myRoots |
	myRoots _ OrderedCollection new.
	fromOwner _ self treeOwner perform: self requestTreeMsg.

	(fromOwner isKindOf: Collection)
		ifFalse:[fromOwner _ OrderedCollection with: fromOwner].

	fromOwner do:[:root| myRoots add: root copyTree].
	^myRoots! !

!KSCTreeEditorLocal methodsFor: 'private'!
getSelections

	selectedNodes isNil
		ifTrue:[^OrderedCollection new]
		ifFalse:[^selectedNodes]! !

!KSCTreeEditorLocal methodsFor: 'private'!
on: aTreeOwner tree: requestSymbol accept: acceptSymbol menu: menuSelector

	self treeOwner: aTreeOwner.
	self requestTreeMsg: requestSymbol.
	self acceptTreeMsg: acceptSymbol.
	self menuMsg: menuSelector.

"I am the model for editing the tree"
	self model: self.
	self aspectMsg: #selectedNodes.
	self changeMsg: #newSelections:.
	self cutMsg: #localCut.
	self pasteMsg: #localAdd:.
	self updateMsg: #localUpdate:.! !

!KSCTreeEditorLocal methodsFor: 'private'!
on: aTreeOwner tree: requestSymbol menu: menuSelector
" This view will act as a tree browser with no changes made to the treeOwner's tree"

	self treeOwner: aTreeOwner.
	self requestTreeMsg: requestSymbol.
	self menuMsg: menuSelector.
	self acceptTreeMsg: nil.

"I am the model for editing the tree"
	self model: self.
	self aspectMsg: #localSelections.
	self changeMsg: #newSelections:.
	self updateMsg: #localUpdate:.! !

!KSCTreeEditorLocal methodsFor: 'tree functions'!
doUpdate: anObject
"anObject can be aTreeNode, aCollection or, nil. A nil indicates that the roots have changed"

	| nodes | 
	(anObject isKindOf: Collection)
		ifTrue:[nodes _ anObject asOrderedCollection]
		ifFalse:[nodes _ OrderedCollection with: anObject].

	self changed:#updateNodes: with: nodes.
	self changed: #treeStructure.! !

!KSCTreeEditorLocal methodsFor: 'model functions'!
localAdd: aCollectionOfNodes

	| dad |
	self selectedNodes isEmpty
		ifTrue:[dad _ nil]
		ifFalse:[dad _ self selectedNodes at:1].

	dad ~~ nil
		ifTrue:[dad addChildren: aCollectionOfNodes]
		ifFalse:[self roots addAll: aCollectionOfNodes].

	self update: #localUpdate: with: dad.! !

!KSCTreeEditorLocal methodsFor: 'model functions'!
localCreate
"The user has requested a new node. Add to aTreeNode or roots."
	| new name |
	name _ FillInTheBlank request:'name of new node?'.
	name isEmpty
		ifTrue:[^self].
	new _ self class nodeClass reference: name.

	self selectedNodes isEmpty
		ifTrue:[self roots add: new]
		ifFalse:[(self selectedNodes at: 1) addChild: new].

	self update: #localUpdate: with: new parent.! !

!KSCTreeEditorLocal methodsFor: 'model functions'!
localCut

	self selectedNodes
		do: [:node| node removeFromParent].
	self pasteBuffer: self selectedNodes.
	self update: #localUpdate: with: self selectedNodes.
	self selectedNodes: nil.! !

!KSCTreeEditorLocal methodsFor: 'model functions'!
newSelections: anOrderedCollection

	^self! !

!KSCTreeEditorLocal methodsFor: 'accept - cancel'!
accept
"send the model his message with the current tree configuration as its parameter"

	(self treeOwner perform: self acceptTreeMsg with: self roots)
		ifTrue:
			["I just gave the model MY ROOTS (don't release)"
			roots _ self getCopyOfRoots]! !

!KSCTreeEditorLocal methodsFor: 'accept - cancel'!
cancel
"Verify cancel. Get another copy from the treeOwner (my model)."

	(BinaryChoice message:'Cancel all changes?')
		ifTrue:[ Cursor execute
					showWhile:[self roots: nil.
							    self changed: #newTree]].! !


!KSCTreeEditorLocal class methodsFor: 'instance creation'!
on: aModel tree: treeSymbol accept: acceptSymbol menu: anObject

	^self new on: aModel
			  tree: treeSymbol
			  accept: acceptSymbol
			  menu: anObject! !

!KSCTreeEditorLocal class methodsFor: 'instance creation'!
on: aModel tree: treeSymbol menu: anObject

	^self new on: aModel
			  tree: treeSymbol
			  menu: anObject! !

!KSCTreeEditorLocal class methodsFor: 'defaults'!
editMenu
"Return the standard edit menu"

	^self menuClass
		labelList: #(('cut' 'copy' 'paste under') ('accept' 'cancel'))
		selectors: #(cut copy paste accept cancel)
		helpIdentifier: self menuHelpIdentifier! !

!KSCTreeEditorLocal class methodsFor: 'defaults'!
nodeClass

	^KSCLinkedTreeNode! !


!KSCTreeExamples methodsFor: 'accessing'!
modelMenu
"Build a new menu from a standard treeView menu and add my application specific stuff.
KSCTreeView sub-menus can also be retrieved individually..."

	| egMenu applicationSpecificMenu |
	modelMenu == nil
		ifTrue:[	egMenu _ self menuClass
						labelList: #(('create' 'cut' 'as 1st root' 'select'))
						selectors: #(modelCreate modelCut modelMove modelSelect).
				applicationSpecificMenu _ self menuClass
					labelList: #(('THRU EDITOR' 'THRU MODEL' 'rebuild tree'))
			 		selectors: (Array with: KSCTreeEditor editMenu with: egMenu with: #userWantsRebuild)"
			 		helpIdentifier: self menuHelpIdentifier".
				modelMenu _ KSCTreeView treeViewMenu
								"joinWith: applicationSpecificMenu"].
	^modelMenu! !

!KSCTreeExamples methodsFor: 'accessing'!
modelMenu: aKSCDisableMenu
	"DEFAULT - Set modelMenu to be aKSCDisableMenu."

	modelMenu _ aKSCDisableMenu! !

!KSCTreeExamples methodsFor: 'accessing'!
modelRoots
"used by the tree editor to initially get the entire hierarchy - 
  I can also trigger the tree editor to update the whole hierarchy with a changed: #modelRoots  "

	modelRoots isNil
		ifTrue:
			[modelRoots _ OrderedCollection
							with:(KSCLinkedTreeNode 
									deriveHierarchyFrom: Collection
									parents: #superclass
									children: #subclasses)].
	^modelRoots! !

!KSCTreeExamples methodsFor: 'accessing'!
modelRoots: anObject
	"Git rid of the nodes!!!!!!"

	modelRoots == nil
		ifFalse:[modelRoots do:[:root | root release]].
	modelRoots _ anObject! !

!KSCTreeExamples methodsFor: 'accessing'!
modelSelection
"Often requested by the Tree Editor..."

	^modelSelection! !

!KSCTreeExamples methodsFor: 'accessing'!
modelSelection: aNode
"Node selection coming from the treeBrowser..."

	|val |
	modelSelection _ aNode.
	modelSelection isNil
		ifTrue:[	val _ 'nil']
		ifFalse:[	val _ modelSelection asString].
	Transcript show: ('\Selection: ', val) withCRs.! !

!KSCTreeExamples methodsFor: 'view support'!
acceptRoots: newRoots
	"This selector is used for the accept cancel example4."

	self modelRoots: newRoots.
	^true! !

!KSCTreeExamples methodsFor: 'view support'!
cut
"changed:with: method can use cut node or parent. If cut node is used, don't release it until AFTER you've notified treeEditor. On a cut, tree editor will reset the modelSelection automatically."
	| dad |
	self modelSelection isNil
		ifTrue:[^nil].
	dad _ self modelSelection parent.
	self modelSelection isRoot
		ifTrue:[self modelRoots remove: self modelSelection]
		ifFalse:[self modelSelection removeFromParent].
	Transcript show: ('\CUT   - ' , self modelSelection asString) withCRs.

"optional insertion into paste buffer..."
	KSCTreeEditor pasteBuffer: self modelSelection.
"need to release the tree..."
	self modelSelection release.
"tell tree editor..."
	self changed: #different: with: dad.! !

!KSCTreeExamples methodsFor: 'view support'!
paste: aCollectionOfNodes
"sent by a treeEditor, the user has selected 'paste under'. aCollectionOfNodes is the paste buffer.
This example will add nodes under the selected node or add as roots if nothing is selected."

	self modelSelection isNil
		ifTrue:[aCollectionOfNodes do:[:node|
					Transcript show:('\PASTE- ', node asString,' as new root') withCRs].
			     self modelRoots addAll: aCollectionOfNodes]
		ifFalse:[aCollectionOfNodes do:[:node|
					Transcript show:('\PASTE- ', node asString,' to ',
									self modelSelection asString) withCRs.
					self modelSelection addChild: node]].

	self changed: #different: with: self modelSelection.! !

!KSCTreeExamples methodsFor: 'changes thru model'!
modelCreate

	| new label | 
	label _ FillInTheBlank request:'Name of new root model will create?'.
	label = '' ifTrue:[^self].
	new _ KSCLinkedTreeNode reference: label.
	Transcript show:('\CREATE- ', new asString,' as a new root by model') withCRs.

	self modelRoots addFirst: new.

"Any update will automatically note root change"
	self changed: #different: with: nil.! !

!KSCTreeExamples methodsFor: 'changes thru model'!
modelCut
"Simulate the case where the model spontaneously wants to cut some node. It will not be placed in the paste buffer"

	| inputName | 
	inputName _ FillInTheBlank request: 'name of node model will cut:'.
	inputName = ''	ifTrue:[^self].
	self modelRoots do: [:root|
		root depthDo: [:node|
			node asString = inputName
				ifTrue:[node parent isNil
						ifTrue:[self modelRoots remove: node.
								^self changed: #different: with: node]
						ifFalse:[node removeFromParent.
								^self changed: #different: with: node]]]].
	^self inform: 'node not found...'! !

!KSCTreeExamples methodsFor: 'changes thru model'!
modelMove
"Simulate the case where the model spontaneously wants to move some node. Selected node will become a root. Node automatically stays selected..."

	self modelSelection == nil
		ifTrue:[^self inform: 'nothing selected'].
	self modelSelection hasParent
		ifTrue:[self modelSelection removeFromParent]
		ifFalse:[self modelRoots remove: self modelSelection].
	self modelRoots addFirst: self modelSelection.
	^self changed: #different: with: self modelSelection! !

!KSCTreeExamples methodsFor: 'changes thru model'!
modelSelect

	| inputName | 
	inputName _ FillInTheBlank request: 'name of node model will select:'.
	self modelRoots do: [:root|
		root depthDo: [:node|
			node asString = inputName
				ifTrue:[self modelSelection = node
							ifFalse:[self modelSelection: node.
								    ^self changed: #modelSelection]]]].
	^self inform: 'node not found or already selected...'! !

!KSCTreeExamples methodsFor: 'changes thru model'!
userWantsRebuild
"This causes the treeEditor to again ask for the modelRoots, and redisplay the entire tree."

	self changed: #modelRoots! !

!KSCTreeExamples methodsFor: 'private'!
breakDependents
	"Since I serve as an indirect model, and no one is dependent on me anymore, break the dependency on my model."

	super breakDependents.
	self modelRoots: nil.
	modelSelection _ nil.! !

!KSCTreeExamples methodsFor: 'menu class accessing'!
menuClass

	^ActionMenu! !


KSCTreeExamples class comment:
'(Renamed from #KSCTreeExamplesA)
I am a Metaclass... please see my instance, "KSCTreeExamples", for a description.'!

!KSCTreeExamples class methodsFor: 'examples'!
example1
"open a fully interactive tree editor on my hierarchy of treeNodes. I will have control of the menu. I can select specific KSCTreeView menus as desired (see class comments).
NOTE - Open a System Transcript Window."

"KSCTreeExamples example1"

	(KSCTreeView
			on: self new
			tree: #modelRoots
			aspect: #modelSelection
			change: #modelSelection:		
			cutRequest: #cut				"from treeView to model."
			pasteRequest: #paste:			"from treeView to model."
			updateNode: #different:		"from model to treeView."
			menu: #modelMenu) open.! !

!KSCTreeExamples class methodsFor: 'examples'!
example2
"open an interactive tree editor on my hierarchy of treeNodes themselves. I will have control of the menu. I can select specific KSCTreeView menus as desired (see class comments)."

"KSCTreeExamples example2"
	| aView |
	aView _KSCTreeView
				on: self new
				seeds: #modelRoots
				parent: #parent
				children: #children
				aspect: #modelSelection
				change: #modelSelection:
				cutRequest: #cut			"can be nil"
				pasteRequest: #paste:		"can be nil"
				updateNode: #different:	"can be nil"
				menu: nil					"can be nil".
	aView initializeHidden:3.
	aView openWithLabel: 'Foreign Hierarchy Example'.! !

!KSCTreeExamples class methodsFor: 'examples'!
example3
"open a BROWSE ONLY VIEW on my hierarchy of treeNodes."

"KSCTreeExamples example3"

	(KSCTreeView
		on: self new
		tree: #modelRoots
		menu: nil) openWithLabel: 'Browse Only Example'.! !

!KSCTreeExamples class methodsFor: 'examples'!
example4
"open an editable view on my hierarchy of treeNodes. All editing is done independently from the model. The model will recieve the new tree on 'accept' "

"KSCTreeExamples example4"

	(KSCTreeView
		on: self new
		tree: #modelRoots
		accept: #acceptRoots:
		menu: nil) openWithLabel: 'Accept/Cancel Example'.! !

!KSCTreeExamples class methodsFor: 'examples'!
example5
"WITH GRAPHICS - open a fully interactive tree editor similar to example1.
NOTE - you need 'KSC-Archiving' to do this."

"KSCTreeExamples example5"

	| model |
	SampleTree _ nil.
	model _ self new.
	model modelRoots: (OrderedCollection with: self sampleTree).
	(KSCTreeView
			on: model
			tree: #modelRoots
			aspect: #modelSelection
			change: #modelSelection:		
			cutRequest: #cut				"from treeView to model."
			pasteRequest: #paste:			"from treeView to model."
			updateNode: #different:		"from model to treeView."
			menu: #modelMenu) open.! !

!KSCTreeExamples class methodsFor: 'class variable'!
sampleTree
	"sample tree is an archived tree that is dearchived when a view is opened."
	"ArchiveWriter archive: KSCTreeExamples sampleTree to: 'egtree.arc'"

	SampleTree == nil
		ifTrue:[SampleTree _ ArchiveReader dearchive: 'egtree.arc'].
	^SampleTree! !


!KSCTreeNode methodsFor: 'accessing'!
children
	"return a collection of TreeNodes under me.
	kwa -- 15 April 1989, use newChildrenCollection instead of hardCoded 'OrderedCollection new: 2'"

	children isNil
		ifTrue: [children _ self newChildrenCollection].
	^children! !

!KSCTreeNode methodsFor: 'accessing'!
children: anObject
	"Set children to be anObject"

	children _ anObject.! !

!KSCTreeNode methodsFor: 'accessing'!
parent
"return my super node"

	^parent! !

!KSCTreeNode methodsFor: 'accessing'!
parent: aTreeNode
	"save my super node.
	kwa -- 14 March 1989, WARNING:  This should only be called internally.  Normally a node's parent should be set by making it a child of the proposed parent (see addChild: )."

	parent _ aTreeNode.! !

!KSCTreeNode methodsFor: 'private'!
newChildrenCollection
	"kwa -- 15 April 1989,
	Answer a collection with which to initialize the receiver's children."

	^OrderedCollection new: 2! !


!KSCLinkedTreeNode methodsFor: 'initialize-release'!
release
"remove all references to parent, children and 'node reference' ."

	super release.
	self reference: nil.! !

!KSCLinkedTreeNode methodsFor: 'accessing'!
reference

	^reference! !

!KSCLinkedTreeNode methodsFor: 'accessing'!
reference:anObject
"set anObject as the reference for this node"

	reference _ anObject.! !

!KSCLinkedTreeNode methodsFor: 'printing'!
printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	self reference printOn: aStream.
	aStream nextPut: $).! !

!KSCLinkedTreeNode methodsFor: 'deriving hierarchy'!
deriveChildrenFrom: aModel using: aSymbol
	"Use aModel as the receiver's reference.  Add child nodes based on some current hierarchical structure already in place where the childModels of aModel (and, assumably, each of its childModels) can be retrieved by performing aSymbol.  Use aModel as the receiver's reference."

	self reference: aModel.
	super deriveChildrenFrom: aModel using: aSymbol.! !

!KSCLinkedTreeNode methodsFor: 'deriving hierarchy'!
deriveParentFrom: aModel using: aSymbol
	"Use aModel as the receiver's reference.  Create a node and make the receiver a child of it, based on some current hierarchical structure already in place where the parentModel of aModel (and, assumably, its parentModel) can be retrieved by performing aSymbol.  Answer the root node of the entire resulting tree (the recursion stops when a nil parentModel is found"

	self reference: aModel.
	^super deriveParentFrom: aModel using: aSymbol! !

!KSCLinkedTreeNode methodsFor: 'converting'!
asDisplayObject
	"use the displayObject of my reference."

	^self reference asDisplayObject! !

!KSCLinkedTreeNode methodsFor: 'converting'!
asString
	"use the string of my reference."

	^self reference asString! !


KSCTreeNode class comment:
'(Renamed from #NEWTreeNode)
I am a Metaclass... please see my instance, "KSCTreeNode", for a description.'!

!KSCLinkedTreeNode class methodsFor: 'instance creation'!
deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol
	"Answer the root node of a hierarchy derived by copying the hierarchy of aModel.  Use parentSymbol and childrenSymbol to derive the node hierarchy.  Use aModel as the reference (and the objects in its hierarchy for the rest of the nodes in the new hierarchy) and nameSymbol as the symbol used to name each of the nodes."
	"KSCTreeNodeReference deriveHierarchyFrom: Magnitude parents: #superclass children: #subclasses name: #name"

	| root | 
	root _ self new.
	^root deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol! !

!KSCLinkedTreeNode class methodsFor: 'instance creation'!
reference:anObject
"create a TreeNodeReference to anObject using aSymbol as the referencing selector"

	^self new reference: anObject! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
buildTestTree:anArray
	"Build a test tree specified by anOrderedCollection where the numbers represent the number of  leaves and the parenthesis specify the rest of the hierarchy"

	"KSCTreeNode buildTestTree:#((( 2  5)  (3) ) ( 4 (1(1 2)) 6 ) (1 ) (6 )  ) "

	| tmp |
	tmp _ self reference:'root' name:#yourself.
	self subTree:anArray 
		of:tmp 
		label:'sub'.
	^tmp! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
buildTestTree:anArray maxLabel: anInteger
	"same as build test tree but adds variable letter length labels at random"

	"KSCTreeNode buildTestTree:#((( 2  5)  (3) ) ( 4 (1(1 2)) 6 ) (1 ) (6 )  )
				  maxLabel:10 "

	| tmp |
	tmp _ self reference:'root' name:#yourself.
	self subTree: anArray 
		of: tmp
		maxLabel: anInteger. 
	^tmp! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
getBigTree
"185 nodes"  
 ^self  	buildTestTree: #( 2 (3 (2)) ( 4 (3)) ((1((2((3((2(4 (3)(3)))1))4)1)3))1) ( 4 (3)) 					((1((12(14)(1((2(4))2))1)13))5) ((1(1 (2)) (2) ((1((1((2(4)1)3))5)(3)1))3 4) 1) 5  )
	 	maxLabel: 8! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
randomLabel: labelSize
"Generate and return a random set of letters >= 1 and <= labelSize "
| lab |
	Seed isNil
		ifTrue:[Seed _ Random new].
	lab _ String new:((Seed next * 100) truncated \\ labelSize) + 1.
	1 to: lab size
		do:[:i| lab at: i
				put: ((Seed next * 100) truncated \\ 26 + $a asciiValue) asCharacter].
	^lab! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
subTree:anArray of:aTreeNode label:aString
"Recursively analyze the elements of anArray and build standard labels using aString"
	| lab new i |
	i _ 0.
	anArray 
		do:[ :e | 	(e isKindOf:Array)
					ifTrue:[lab _ aString , '.' , (i _ i + 1) printString.
						    new _ self reference: lab 
									 name:#yourself.
						    aTreeNode addChild:new. 
						    self subTree:e of:new label: lab ]
					ifFalse:[1 to: e
							do:[:r| aTreeNode
									addChild:(self reference: aString,'.',(i _ i + 1) printString 
												 name: #yourself)]]].! !

!KSCLinkedTreeNode class methodsFor: 'test tree generation'!
subTree:anArray of:aTreeNode maxLabel: labelSize
"Recursively analyze the elements of anArray, building random labels for each node"
| lab new |
	anArray  do:
		[ :e |  (e isKindOf:Array)
				ifTrue:[new _ self reference: (self randomLabel: labelSize) 
								 name: #yourself.
					    aTreeNode addChild:new. 
					    self subTree:e of:new maxLabel: labelSize]
				ifFalse:[1 to: e
					    do:[:r| aTreeNode
							addChild:(self reference: (self randomLabel: labelSize) 
										 name: #yourself)]]].! !


!KSCTreeNodeByReference methodsFor: 'initialize-release'!
release
"Abstract trees are released by removing them from the dictionary 'nodeGroup' and then reseting their pointers"

	self releaseChildren.
	nodeGroup == nil ifTrue:[^nil].
	nodeGroup nodeDictionary
		removeKey: self reference
		ifAbsent:[].
	self reference: nil.
	self nodeGroup: nil! !

!KSCTreeNodeByReference methodsFor: 'structure changes'!
addChild: aTreeNode
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'structure changes'!
addChild: aTreeNode before: aBrotherTreeNode
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'structure changes'!
addChildren: aCollectionOfTreeNodes
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'structure changes'!
removeChild:aTreeNode
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'structure changes'!
removeFromParent
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
facade
"This single instance variable handles all 4 possible states of fake roots and leaves:
#none - a typical node
#leaf - will appear as a leaf when questioned, whether it has children or not
#root - will appear as a root when questioned, whether it has a parent or not
#rootAndLeaf - will appear as a single node no matter where it is in a hierarchy."

	facade == nil
		ifTrue:[^#none].
	^facade! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
facade: aSymbol
	"DEFAULT - Set facade to be aSymbol."

	facade _ aSymbol! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
nodeGroup
"There should always be a node group. The node gets info common to the tree from nodeGroup"

	^nodeGroup! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
nodeGroup: aKSCNodeGroup
	"DEFAULT - Set nodeGroup to be anObject."

	nodeGroup _ aKSCNodeGroup! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
reference
	"DEFAULT - Answer the instance variable, reference"

	^reference! !

!KSCTreeNodeByReference methodsFor: 'accessing'!
reference: anObject
	"DEFAULT - Set reference to be anObject."

	reference _ anObject! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
children
"return a collection of TreeNodes under me. If I havent seen the child before, create an abstract node for it"

	| kids |
	kids _ OrderedCollection new.
	self isFakeLeaf
		ifTrue: [^kids].
	(self reference perform: nodeGroup childrenMsg)
		do:[:realKid|
			kids	add: (nodeGroup nodeDictionary
						at: realKid
						ifAbsent:[self class reference: realKid
										  nodeGroup: self nodeGroup])].
	^kids! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
children: anObject
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
isFakeLeaf
"check both possible fake leaf states"

	^(self facade == #leaf) | (self facade == #rootAndLeaf)! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
isFakeLeaf: aBoolean
"Adjust the state of the facade"

	aBoolean
		ifTrue:[	self facade == #none
					ifTrue:[^self facade: #leaf].
				self facade == #root
					ifTrue:[^self facade: #rootAndLeaf]]
		ifFalse:[	self facade == #leaf
					ifTrue:[^self facade: #none].
				self facade == #rootAndLeaf
					ifTrue:[^self facade: #root]]! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
isFakeRoot
"check both possible fake root states"

	^(self facade == #root) | (self facade == #rootAndLeaf)! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
isFakeRoot: aBoolean
"Adjust the state of the facade"

	aBoolean
		ifTrue:[	self facade == #none
					ifTrue:[^self facade: #root].
				self facade == #leaf
					ifTrue:[^self facade: #rootAndLeaf]]
		ifFalse:[	self facade == #root
					ifTrue:[^self facade: #none].
				self facade == #rootAndLeaf
					ifTrue:[^self facade: #leaf]]! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
parent
"return my supernode"

	| dad |
	self isFakeRoot
		ifTrue: [^nil]
		ifFalse:[dad _ self reference perform: nodeGroup parentMsg].
	dad isNil
		ifTrue:[^nil]
		ifFalse:[^nodeGroup nodeDictionary
					at: dad
					 ifAbsent:[self class
								reference: dad
								nodeGroup: nodeGroup]].! !

!KSCTreeNodeByReference methodsFor: 'customized accessing'!
parent: anObject
"abstract trees can't be altered directly"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'copying'!
copyEmpty
"not needed for abatract trees"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'copying'!
copyTree
"abstract trees are not copied this way"

	self shouldNotImplement! !

!KSCTreeNodeByReference methodsFor: 'deriving hierarchy'!
deriveHierarchy
"Create the receiver's hierarchy by modelling the hierarchy of this node.  Answer this node as the root."

	self children
		do:[:child| child deriveHierarchy].
	^self! !

!KSCTreeNodeByReference methodsFor: 'printing'!
printOn: aStream
"print my reference along with myself."

	super printOn: aStream.
	aStream nextPut: $(.
	self reference printOn: aStream.
	aStream nextPut: $).! !

!KSCTreeNodeByReference methodsFor: 'private'!
doesNotUnderstand: aMessage
"Pass on to the nodeGroup anything I don't understand"

	self nodeGroup
		perform: aMessage selector
		withArguments: aMessage arguments! !

!KSCTreeNodeByReference methodsFor: 'private'!
reference: anObject nodeGroup: aKSCNodeGroup
"Create a new abstract node and build its nodeGroup."

	reference _ anObject.
	nodeGroup _ aKSCNodeGroup.
	nodeGroup nodeDictionary at: anObject put: self.
	^self! !

!KSCTreeNodeByReference methodsFor: 'converting'!
asDisplayObject
	"use the displayObject of my reference."

	^self reference asDisplayObject! !

!KSCTreeNodeByReference methodsFor: 'converting'!
asString
	"use the string of my reference."

	^self reference asString! !


KSCTreeNodeByReference class comment:
'(Renamed from #KSCUserHierarchy)
I am a Metaclass... please see my instance, "KSCTreeNodeByReference", for a description.'!

!KSCTreeNodeByReference class methodsFor: 'instance creation'!
deriveHierarchyFrom: aModel nodeGroup: aKSCNodeGroup
	"Answer the root node of a hierarchy derived by copying the hierarchy of aModel.  Use parentSymbol and childrenSymbol to derive the node hierarchy.  Use aModel as the reference and the objects in its hierarchy for the rest of the nodes in the new hierarchy. All of the nodes of this hierarchy will use the same nodeGroup."

	| root | 
	root _ self new
			reference: aModel
			nodeGroup: aKSCNodeGroup.

	root deriveHierarchy.
"	root isFakeRoot: true.  This will now always bee seen as the root"
	^root! !

!KSCTreeNodeByReference class methodsFor: 'instance creation'!
deriveHierarchyFrom: aModel parents: parentSymbol children: childrenSymbol nodeDictionary: aDictionary
	"Answer the root node of a hierarchy derived by copying the hierarchy of aModel.  Use parentSymbol and childrenSymbol to derive the node hierarchy.  Use aModel as the reference and the objects in its hierarchy for the rest of the nodes in the new hierarchy. This node will create a new nodeGroup."

	| root | 
	root _ self new
		reference: aModel
		nodeGroup:(KSCNodeGroup
					children: childrenSymbol
					parent: parentSymbol
					nodeDictionary: aDictionary).
	root deriveHierarchy.
	^root! !

!KSCTreeNodeByReference class methodsFor: 'instance creation'!
reference: anObject childrenMsg: childrenSymbol parentMsg: parentSymbol nodeDictionary: aDictionary
"Create a new instance of myself and add me to aDictionary"

	^self new
		reference: anObject
		nodeGroup:(KSCNodeGroup
					children: childrenSymbol
					parent: parentSymbol
					nodeDictionary: aDictionary)! !

!KSCTreeNodeByReference class methodsFor: 'instance creation'!
reference: anObject nodeGroup: aKSCNodeGroup
"Create a new instance of myself and add me to the given nodeGroup"

	^self new
		reference: anObject
		nodeGroup: aKSCNodeGroup! !


!KSCTreeNodeImage methodsFor: 'initialize-release'!
release

	super release.! !

!KSCTreeNodeImage methodsFor: 'structure changes'!
addChild: aTreeNodeImage
"add a TreeNode under me. Back-link it to me. If success, reset display info"

	(super addChild: aTreeNodeImage)
		ifTrue:[self doToRoot: [:each| each resetDisplayInformation]]! !

!KSCTreeNodeImage methodsFor: 'structure changes'!
addChild: aTreeNodeImage before: aBrotherNode
"add a TreeNode under me. Back-link aTreeNodeImage to me. If success, reset display info"

	(super addChild: aTreeNodeImage before: aBrotherNode)
		ifTrue:[self doToRoot: [:each| each resetDisplayInformation]]! !

!KSCTreeNodeImage methodsFor: 'structure changes'!
removeChild: aTreeNodeImage
"remove a TreeNode under me. If success, disable cut nodes' selection areas and reset parents' images."

	(super removeChild: aTreeNodeImage)
		ifTrue:[aTreeNodeImage depthDo: [:node| node selectArea: nil].
			    self doToRoot: [:node| node resetDisplayInformation]].! !

!KSCTreeNodeImage methodsFor: 'structure changes'!
removeFromParent
"remove me from my parent TreeNode. If success, update image info"

	| dad |
	dad _ parent.
	super removeFromParent
		ifTrue:[dad doToRoot: [:node| node resetDisplayInformation]].
	self depthDo: [:node| node selectArea: nil].! !

!KSCTreeNodeImage methodsFor: 'calculating'!
drawBranches
"draws the branch lines. Assumes labelForm and nodeForm have already been initialized."

	|myBitBlt source|
	myBitBlt _ BitBlt destForm: nodeForm
				sourceForm: (Form dotOfSize:1)
				halftoneForm: nil
				combinationRule: Form over
				destOrigin: 0 @ (source _ labelForm width @ (nodeForm height // 2))
				sourceOrigin: 0 @ 0
				extent: nodeForm extent
				clipRect: nodeForm boundingBox.
	self children
		do:[:child| myBitBlt drawFrom: source
						  to: nodeForm extent x - 1 @ child parentOffset].! !

!KSCTreeNodeImage methodsFor: 'calculating'!
formSize
"return or calculate the size of the nodeForm"

	| x y |
	nodeForm notNil
		ifTrue:[^nodeForm boundingBox extent].
	x _ self getFormWidth.
	self children size <= 1 | self isHidden
		ifTrue: [y _ self labelForm height]
		ifFalse:[y _ self labelForm height max: self children last parentOffset].
	^x @ y! !

!KSCTreeNodeImage methodsFor: 'calculating'!
getFormWidth
"The requested nodeForm width is determined from the width of the node and possibly: branchLength or hide indicator (for hidden nodes ...)"

| suffixWidth |
	self isLeaf
		ifTrue:[suffixWidth _ 0]
		ifFalse:[ self isHidden
					ifTrue:[suffixWidth _ self hideIndicator width]
					ifFalse:[suffixWidth _ self branchLength]].
	^self nodeWidth + suffixWidth! !

!KSCTreeNodeImage methodsFor: 'calculating'!
hideShowReset
"reset all parent node images up to the root"

	nodeForm _ nil.
	parentOffset _ nil.
	myOffset _ nil.
	subtreeSize _ nil.
	selectArea _ nil.
	self parent isNil
		ifFalse:[self parent hideShowReset].! !

!KSCTreeNodeImage methodsFor: 'calculating'!
isOnCanvas
"Is the current image displayed in the tree browser?"

	^self selectArea origin >= (0 @ 0)! !

!KSCTreeNodeImage methodsFor: 'calculating' stamp: 'ssa 12/4/97 17:30'!
makeForm
"draws the image nodeForm from all the current information"

	nodeForm _ Form extent: self formSize depth: Display depth.
	self labelForm
		displayOn: nodeForm
		at: 0 @ (nodeForm height // 2 - (labelForm height // 2))
		clippingBox: (0 @ 0 extent: nodeForm extent)
		rule: Form over
		fillColor: Form black.
	self hasChildren & self isHidden
		ifTrue:[self hideIndicator
					displayOn: nodeForm
					at: labelForm width @ (nodeForm height - self hideIndicator height)
					clippingBox: nodeForm boundingBox
					rule: Form over
					fillColor: Form black]
		ifFalse:[self drawBranches].! !

!KSCTreeNodeImage methodsFor: 'calculating'!
nodeWidth
" These nodeImages don't worry about alignment. Can be reImplemented by a subclass"

	^self labelForm width! !

!KSCTreeNodeImage methodsFor: 'calculating'!
notOnCanvas
"reset the input area of this treeNodeImage because it is no longer displayed"

	selectArea _ nil.! !

!KSCTreeNodeImage methodsFor: 'calculating'!
parentYoffset

	^self parentOffset - (self formSize y // 2)! !

!KSCTreeNodeImage methodsFor: 'calculating'!
resetDisplayInformation
"reset all display information related to the rest of the rest of the tree."

	nodeForm _ nil.
	parentOffset _ nil.
	myOffset _ nil.
	subtreeSize _ nil.
	selectArea _ nil.! !

!KSCTreeNodeImage methodsFor: 'calculating'!
resetImageOfNode
"reset all display information."

	labelForm _ nil.
	nodeForm _ nil.
	parentOffset _ nil.
	myOffset _ nil.
	subtreeSize _ nil.
	selectArea _ nil.! !

!KSCTreeNodeImage methodsFor: 'calculating'!
resetNodeForm
"This will force the node image to be redrawn"

	nodeForm _ nil.! !

!KSCTreeNodeImage methodsFor: 'calculating'!
selectPoint:aPoint
"The given point defines the treeView canvas location of the selection area."

	self selectArea moveTo: aPoint x  @ (aPoint y + (nodeForm height // 2) - (self selectArea height // 2))! !

!KSCTreeNodeImage methodsFor: 'calculating' stamp: 'ssa 12/3/97 12:24'!
sizeAndLinkSubtree
"this method combines two functions: size tree and link nodes. They both must recursively traverse the tree and are combined to save time (i.e. one traversal of the tree)"
	| wd offset kid1 kid2 myMiddle adjust child1 |
	myMiddle _  self labelForm height // 2.
	self hasChildren not | self isHidden
		ifTrue:[	subtreeSize _ 0 @ self labelForm height.
			    	^myOffset _ myMiddle].
	self children size == 1
		ifTrue:[	kid1 _ self children first.
				kid1 parentOffset: myMiddle.
				myOffset _ kid1 treeOffsetTop max: myMiddle.
				^subtreeSize _ kid1 treeWidth @
							  (myOffset + (kid1 treeOffsetBottom max: myMiddle))].
	(kid1 _ self children first) parentOffset: 0.
	offset _ kid1 treeOffsetBottom.
	wd _ kid1 treeWidth.
	2 to: self children size do:
		[:i|	child1 _ self children at: i.
			child1 parentOffset: offset + child1 treeOffsetTop.
			offset _ offset + child1 treeHeight.
			wd _ wd max: child1 treeWidth].
	offset _ offset - (kid2 _ self children last) treeOffsetBottom.  "adjustment for last child"
	offset < self labelForm height
		ifTrue:[	adjust _ (self labelForm height - offset) // 2.
				self children do: [:child| child parentOffset: child parentOffset + adjust].
				myOffset _ kid1 treeOffsetTop - adjust + myMiddle max: myMiddle.
				subtreeSize _ wd @ (myOffset + (kid2 treeOffsetBottom - adjust + myMiddle max: myMiddle))]
		ifFalse:[	subtreeSize _ wd @ (kid1 treeOffsetTop + offset + kid2 treeOffsetBottom).
				myOffset _ kid1 treeOffsetTop + (offset // 2)]! !

!KSCTreeNodeImage methodsFor: 'calculating'!
sizeSubtree
"this method (recursively) sizes the subTrees. It is used for re-alignment, when linking has already been performed"
	| ht wd |
	ht _ wd _ 0.
	self hasChildren & self isHidden
		ifTrue:[subtreeSize _ 0 @ self labelForm height.
			    ^myOffset _ self labelForm height // 2].
	self children do:
		[:child|	ht _ ht + child treeHeight.
				wd _ wd max: child treeWidth].
	subtreeSize _ wd @ ht.
	subtreeSize y: (ht max: self labelForm height).! !

!KSCTreeNodeImage methodsFor: 'calculating'!
treeHeight
"tree height involves the subtree size. If myOffset (or parentOffset) are nil, both size and link the subtree. Otherwise, just size the subtree if subtreeSize is nil."

	myOffset isNil
		ifTrue:[self sizeAndLinkSubtree]
		ifFalse:[subtreeSize isNil
				ifTrue:[self sizeSubtree]].
	^subtreeSize y! !

!KSCTreeNodeImage methodsFor: 'calculating'!
treeOffsetBottom
"If myOffset (or parentOffset) is nil, both size and link the subtree. Otherwise, just size the subtree if subtreeSize is nil."

	myOffset isNil
		ifTrue:[self sizeAndLinkSubtree]
		ifFalse:[subtreeSize isNil
					ifTrue:[self sizeSubtree]].
	^subtreeSize y - myOffset! !

!KSCTreeNodeImage methodsFor: 'calculating'!
treeOffsetTop

	myOffset isNil
		ifTrue:[self sizeAndLinkSubtree].
	^myOffset! !

!KSCTreeNodeImage methodsFor: 'calculating'!
treeWidth
"If myOffset (or parentOffset) is nil, both size and link the subtree. Otherwise, just size the subtree if subtreeSize is nil."

	myOffset isNil
		ifTrue:[self sizeAndLinkSubtree]
		ifFalse:[subtreeSize isNil
					ifTrue:[self sizeSubtree]].
	^self getFormWidth + subtreeSize x! !

!KSCTreeNodeImage methodsFor: 'printing'!
labelString
"return a string of what this node references"

	^self reference asString! !

!KSCTreeNodeImage methodsFor: 'accessing'!
branchLength
	branchLength isNil
		ifTrue:[parent isNil
				ifTrue:[branchLength _ self defaultBranchLength]
				ifFalse:[branchLength _ parent branchLength]].
	^branchLength! !

!KSCTreeNodeImage methodsFor: 'accessing'!
branchLength: anInteger
" Set branchLength to be anInteger and reset."

	self branchLength = anInteger
		ifFalse: [branchLength _ anInteger.
				self resetDisplayInformation].! !

!KSCTreeNodeImage methodsFor: 'accessing'!
displayMsg
	"DEFAULT - Answer the instance variable, displayMsg"

	displayMsg == nil
		ifTrue:[parent isNil
				ifTrue:[displayMsg _ self defaultDisplayMsg]
				ifFalse:[displayMsg _ parent displayMsg]].
	^displayMsg! !

!KSCTreeNodeImage methodsFor: 'accessing'!
displayMsg: aSymbol
	"DEFAULT - Set displayMsg to be aSymbol."

	displayMsg _ aSymbol! !

!KSCTreeNodeImage methodsFor: 'accessing'!
isHidden
"isHidden defaults to false at initialization"

	isHidden isNil
		ifTrue:[isHidden _ self defaultHidden].
	^isHidden! !

!KSCTreeNodeImage methodsFor: 'accessing'!
isHidden: aBoolean
"perform all functions necessary to the tree, to hide or show a node"

	(self isHidden = aBoolean) | self isLeaf
		ifTrue:[^false].
	isHidden _ aBoolean.
	isHidden
		ifTrue:[ self depthDo: [:each| each  notOnCanvas]].
 	self hideShowReset.
	^true! !

!KSCTreeNodeImage methodsFor: 'accessing'!
isInitiallyHidden: aBoolean
"set isHidden to aBoolean, for initialization"

	isHidden  _ aBoolean.! !

!KSCTreeNodeImage methodsFor: 'accessing'!
labelForm
"Label form is (more generally) a displayObject. It represents the image of this node."

	labelForm isNil
		ifTrue:[labelForm _ self reference perform: self displayMsg].
	^labelForm! !

!KSCTreeNodeImage methodsFor: 'accessing'!
labelForm: anObject
	"DEFAULT - Set labelForm to be anObject."

	labelForm _ anObject! !

!KSCTreeNodeImage methodsFor: 'accessing'!
myOffset

	myOffset isNil
		ifTrue:[self sizeAndLinkSubtree].
	^myOffset! !

!KSCTreeNodeImage methodsFor: 'accessing'!
myOffset: anObject
	"DEFAULT - Set myOffset to be anObject."

	myOffset _ anObject! !

!KSCTreeNodeImage methodsFor: 'accessing'!
nodeForm
"draw the node form if there isn't one"

	nodeForm == nil 
		ifTrue:[self makeForm].
	^nodeForm! !

!KSCTreeNodeImage methodsFor: 'accessing'!
nodeForm: anObject
	"DEFAULT - Set nodeForm to be anObject."

	nodeForm _ anObject! !

!KSCTreeNodeImage methodsFor: 'accessing'!
parentOffset
"This instance variable specifies the Y offset (of the top left corner) of this nodeForm in relation to (the top right corner of) its parent's nodeForm. It is set by the parent."

	parentOffset isNil
		ifTrue:[self parent sizeAndLinkSubtree].
	^parentOffset! !

!KSCTreeNodeImage methodsFor: 'accessing'!
parentOffset: aSmallInteger
"See 'parentOffset' message for description of instance variable"

	parentOffset _ aSmallInteger.! !

!KSCTreeNodeImage methodsFor: 'accessing' stamp: 'ssa 12/3/97 12:24'!
selectArea
"return the select area of the node on the screen."

	selectArea isNil
		ifTrue:[selectArea _ Rectangle origin: (0@0) - self labelForm extent
									extent: self labelForm extent].
	^selectArea! !

!KSCTreeNodeImage methodsFor: 'accessing'!
selectArea: anObject
	"DEFAULT - Set selectArea to be anObject."

	selectArea _ anObject! !

!KSCTreeNodeImage methodsFor: 'accessing'!
subtreeSize
	"DEFAULT - Answer the instance variable, subtreeSize"

	^subtreeSize! !

!KSCTreeNodeImage methodsFor: 'accessing'!
subtreeSize: anObject
	"DEFAULT - Set subtreeSize to be anObject."

	subtreeSize _ anObject! !

!KSCTreeNodeImage methodsFor: 'default behavior'!
defaultBranchLength
"This is the (minimum) X distance of branch lines, in pixels."

	^32! !

!KSCTreeNodeImage methodsFor: 'default behavior'!
defaultDisplayMsg
"Default is to use an object's normal protocol to display itself."

	^#asDisplayObject! !

!KSCTreeNodeImage methodsFor: 'default behavior'!
defaultHidden
"Nodes will not normally present themselves as hidden."

	^false! !

!KSCTreeNodeImage methodsFor: 'default behavior'!
hideIndicator
"This form will be appended to all node images that currently have their subtrees hidden"

	HideIndicator isNil
		ifTrue:[HideIndicator _ '...' asDisplayText].
	^HideIndicator! !

!KSCTreeNodeImage methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint
"display yourself and your underlings (recursively)"

"	self nodeForm height == 42
		ifTrue:[self halt.].
"	self nodeForm
		displayOn: aDisplayMedium
		at: aPoint
		clippingBox: aDisplayMedium boundingBox
		rule: Form over
		fillColor: nil.
	self selectPoint: aPoint.
	self isHidden
		ifFalse:[self children do:
				[:child|  child	displayOn: aDisplayMedium
						   	at: aPoint + (nodeForm width @ child parentYoffset)]]! !

!KSCTreeNodeImage methodsFor: 'private'!
reference: anObject displayMsg: aSymbol

	reference _ anObject.
	displayMsg _ aSymbol.! !


!KSCTreeNodeAlignableImage methodsFor: 'accessing'!
alignedWidth
"A alignedWidth of zero means that alignment is not activated (default)"

	alignedWidth isNil
		ifTrue:[alignedWidth _ 0].
	^alignedWidth! !

!KSCTreeNodeAlignableImage methodsFor: 'accessing'!
alignedWidth: anInteger

	alignedWidth = anInteger
		ifFalse:[	alignedWidth _ anInteger.
				self resetNodeForm].! !

!KSCTreeNodeAlignableImage methodsFor: 'calculating'!
hideShowReset
"reset all parent node images up to the root"

	super hideShowReset.
	alignedWidth _ nil.! !

!KSCTreeNodeAlignableImage methodsFor: 'calculating'!
nodeWidth
"if the alignedWidth = 0, then alignment is not activated"

	self alignedWidth = 0
		ifTrue:[^self labelForm width]
		ifFalse:[^alignedWidth].! !

!KSCTreeNodeAlignableImage methodsFor: 'calculating'!
resetDisplayInformation
"reset all display information related to the rest of the rest of the tree."

	super resetDisplayInformation.
	alignedWidth _ nil.! !

!KSCTreeNodeAlignableImage methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint withAlignment: aCollection
"display yourself and your underlings (recursively) with or without alignment"

	aCollection isNil
		ifTrue: [self 	displayOn: aDisplayMedium
					at: aPoint]
		ifFalse:[self 	displayOn: aDisplayMedium
					at: aPoint
					withAlignment: aCollection
					level: 1].! !

!KSCTreeNodeAlignableImage methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aPoint withAlignment: aCollection level: anInteger
"display yourself and your underlings (recursively)"

	self alignedWidth: (aCollection at: anInteger).
	self nodeForm
		displayOn: aDisplayMedium
		at: aPoint
		clippingBox: aDisplayMedium boundingBox
		rule: Form over
		fillColor: nil.
	self selectPoint: aPoint.
	self isHidden
		ifFalse:[self children do:
				[:child|  child	displayOn: aDisplayMedium
						   	at: aPoint + (nodeForm width @ child parentYoffset)
							withAlignment: aCollection
							level: anInteger + 1]]! !


!KSCTreeNodeImage class methodsFor: 'class variables'!
resetHideIndicator
"KSCTreeNodeImage resetHideIndicator"

	HideIndicator _ nil! !

!KSCTreeNodeImage class methodsFor: 'instance creation'!
of:anObject display: aSymbol
"create a new image which must reference anObject and a tree view."

	^super new reference: anObject
			    displayMsg: aSymbol! !


!KSCTreePrunerSpecification methodsFor: 'code generation'!
addAcceptMethodIn: aClass
	"generate and compile a method into the class aClass that will allow accept the new tree when signalled to do so by an action on the menu."

	| methodString instVarName |
	instVarName _ self adaptorMessages at: #tree.
	methodString _ 
(self adaptorMessages at: #accept), 'aTree
	"Accept aTree as the receiver''s new ', instVarName, ' if it is acceptable.  	Answer a Boolean denoting its acceptability."

	self ', instVarName, ': aTree.
	^true'.

	(KSCWindowDesigner browserClassNamed: aClass)
		compile: methodString classified: 'tree accessing'! !

!KSCTreePrunerSpecification methodsFor: 'code generation'!
addDefaultTreeMethodIn: aClass 
	"Generate and compile a method into the class aClass that will answer a default tree structure."

	| treeName methodString methodName |
	treeName _ (self adaptorMessages at: #tree) asString.
	methodName _ 'default', treeName.
	methodName at: ('default' size + 1) put: (methodName at: ('default' size + 1)) asUppercase.
	methodString _ 
methodName, '
	"create a tree to use as the default value for, ', treeName, '"

	^KSCTreeNodeReference deriveHierarchyFrom: Magnitude parents: #superclass children: #subclasses name: #name'.
	(KSCWindowDesigner browserClassNamed: aClass)
		compile: methodString classified: 'private'.! !

!KSCTreePrunerSpecification methodsFor: 'code generation'!
addTreeMethodIn: aClass 
	"Generate and compile a method into the class aClass that will allow access to 
	the browser model's tree that I view when invoked."

	| treeName methodString |
	treeName _ self adaptorMessages at: #tree.
	(KSCWindowDesigner browserClassNamed: aClass)
		addInstVarName: treeName.
	KSCWindowDesigner updateBrowserClassNamed: aClass.
	methodString _ 
treeName, '
	"access and return the receiver''s ' , treeName, '"

	' , treeName, ' == nil
		ifTrue: [self ', treeName, ': self default', (self stripAndCap: treeName), '].
	^', treeName.
	(KSCWindowDesigner browserClassNamed: aClass)
		compile: methodString classified: 'tree accessing'.
	(KSCWindowDesigner browserClassNamed: aClass) createDefaultAccessMethods.
	self addDefaultTreeMethodIn: aClass.! !

!KSCTreePrunerSpecification methodsFor: 'code generation'!
addViewSupportMethodsIn: aClass 
	"using my specification, generate and install the code needed to support my 
	viewClass into aClass"

	super addViewSupportMethodsIn: aClass.
	self addAcceptMethodIn: aClass.
	self addMenuMethodIn: aClass.! !

!KSCTreePrunerSpecification methodsFor: 'adding adaptors'!
createAdaptorMessagesUsing: treeSymbol 
	"using treeSymbol as a basis, create and install the adaptor methods.  I 
	expect to be invoked by my subclasses when (if) they need to override me to 
	install their own adaptors"

	super createAdaptorMessagesUsing: treeSymbol. 
	self adaptorMessages at: #accept put: ('new', (self stripAndCap: (self adaptorMessages at: #tree), ':')) asSymbol.
	self adaptorMessages at: #menu put: (treeSymbol, 'Menu') asSymbol.! !

!KSCTreePrunerSpecification methodsFor: 'private'!
defaultCreationMessageSelector
	"Answer the default creationMessageSelector of the receiver."

	^#on:tree:accept:menu:! !


KSCTreeView comment:
'Instances of this class work very closley with KSCTreeNodeImages in displaying trees associated with a KSCTreeEditor (or its subclasses). They build and maintain the KSCTreeNodeImages from a ''metaRoot'' for the KSCTreeEditor trees (multiple roots).


Whenever the canvas is nil, the treeView references the size of the root node images to determine the canvas size. It then copies each appropriate tree node image to the canvas. When changes to the tree nodes occur, the associated images may be reset and/or its input area disabled.

The KSCTreeView works with the KSCTreeController for hides, shows and, selections. Otherwise, the TreeController deals with the KSCTreeEditor (i.e. for tree editing, accept, cancel). The user need only know about KSCTreeView (for creation or paste buffer accessing).

Instance variable definitions:

selectedNodes <OrderedCollection> holds treeNodeImages that are currently selected by the user. 
canvas <Form> where all TreeNodeImages are drawn.
canvasOffset <Point> for redisplay - normaly fixedOffsets will determine the new offset but if there is a redisplay with nothing selected, this instance variable is used.
fixedOffsets <OrderedCollection> holds associations of a node and its previous offset relative to the view. This structure is built by fixOffsets and referenced and reset by findOffset.
branchLength <Integer> x length (in pixels) for all branches drawn in the view.
isVerticalTree <Boolean> state (later....
levelWidths <OrderedCollection or nil> if nil, level alignment is disabled. Else:
					holds integers representing the width at the corresponding level.
aspectMsg <Symbol> to get collection of trees from the model (a TreeBrowser).			
changeMsg <Symbol> canvas is reset and trees redrawn when the model (a TreeBrowser) notifies.
configMenu <KSCDisableMenu> This variable holds the configMenu in order to keep track of the current options in effect.

class variables:

(none)

issues:

1.  This currently only handles horizontal trees, vertical trees will come later.'!

!KSCTreeView methodsFor: 'initialize-release'!
initialize
"Create a new imageDictionary. The dictionary keys are TreeNode instances and the values are TreeNodeImage instances"
	
	super initialize.
	self insideColor: Form white.! !

!KSCTreeView methodsFor: 'initialize-release'!
initializeAlignment

	self levelWidths: (Array new: self maxDepth withAll:0).
	self configMenu disable: 1;
				    enable: 2.
	self resetAllNodeImages.! !

!KSCTreeView methodsFor: 'initialize-release'!
initializeMultipleSelect
"Initialize this view to handle multiple selections. "

	self isSingleSelection: false.! !

!KSCTreeView methodsFor: 'initialize-release'!
release

 	self selectedNodes: nil.
	self metaRoot release.
	self cutImages do: [:image| image release].
	cutImages _ nil.
	super release.! !

!KSCTreeView methodsFor: 'accessing'!
branchLength
"use the default branch length if nil"

	branchLength isNil
		ifTrue:[branchLength _ self defaultBranchLength].
	^branchLength! !

!KSCTreeView methodsFor: 'accessing'!
branchLength: anInteger
"must be within a reasonable range"

	anInteger <= 10 | (anInteger >= 300)
		ifTrue:[nil inform: ' Try again with length between 10 and 300']
		ifFalse:[ branchLength _ anInteger.
				self metaRoot depthDo:[:image| image branchLength: branchLength].
				self canvas: nil].! !

!KSCTreeView methodsFor: 'accessing'!
canvas
"If singleSelection, make sure to expose selected node."

	| dad |
	canvas isNil
		ifTrue:[Cursor execute
				showWhile:
				     ["initialize possibly hidden selection before the nodes are drawn"
					self selectedNodes isEmpty not & self isSingleSelection
						ifTrue:[(dad _ selectedNodes first parent) isNil
									ifFalse:[dad doToRoot:[:node| node isHidden: false]]].
					self sizeAndCreateCanvas.
					self drawRoots.
					self clearInside: Form white.
					canvas offset: self findNewOffset.
					self updateSelectedNodes]].
	canvasOffset _ canvas offset.
	^canvas! !

!KSCTreeView methodsFor: 'accessing'!
canvas: aForm 
	canvas _ aForm! !

!KSCTreeView methodsFor: 'accessing'!
canvasOffset
	"DEFAULT - Answer the instance variable, canvasOffset"

	canvasOffset == nil
		ifTrue: [self canvasOffset: 0@0].
	^canvasOffset! !

!KSCTreeView methodsFor: 'accessing'!
canvasOffset: anObject
	"DEFAULT - Set canvasOffset to be anObject."

	canvasOffset _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
configMenu
	"This menu stays the same throughout the life of the view. It is enabled and disabled as required."

	configMenu == nil
		ifTrue:[configMenu _ self menuClass
								labelList: #(('align levels'
											'remove alignment'
											'vertical tree'
											'horizontal tree'
											'set branch length'))
								selectors: #(align deAlign setVertHoriz setVertHoriz newBranchLength)"
								helpIdentifier: self menuHelpIdentifier, '.configuration'".
			 configMenu deemphasizeFlag: false.
			 self isThereAlignment
				ifTrue:[configMenu disable: 1]
				ifFalse:[configMenu disable: 2].
			 self isVerticalTree
				ifTrue:[configMenu disable: 3]
				ifFalse:[configMenu disable: 4]].
	^configMenu! !

!KSCTreeView methodsFor: 'accessing'!
configMenu: anObject
	"DEFAULT - Set configMenu to be anObject."

	configMenu _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
cutImages
"Answer the instance variable, cutImages"

	cutImages == nil
		ifTrue:[cutImages _ OrderedCollection new]
		ifFalse:[cutImages _ cutImages select:
							[:image | image reference isNil
										ifTrue:[false].
									 image parent == nil]].
	 ^cutImages! !

!KSCTreeView methodsFor: 'accessing'!
cutImages: anObject
	"All cutImages will be replaced. Release the current cutImages that are still cut."

	cutImages == nil
		ifFalse:
			[cutImages do:
				[:image | image parent == nil
							ifTrue: [image release]]].
	cutImages _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
displaySelector
"Answer the instance variable, displaySelector"

	displaySelector == nil
		ifTrue:[displaySelector _ self defaultDisplayMsg].
	^displaySelector! !

!KSCTreeView methodsFor: 'accessing'!
displaySelector: aSymbol
"Set displaySelector to be aSymbol. OPTIONAL - Reset the entire tree to the new selector"

	displaySelector _ aSymbol.

	"self metaRoot depthDo:
		[:node| 	node resetImageOfNode.
				node displayMsg: displaySelector].
	self canvas: nil.
	self displayView."! !

!KSCTreeView methodsFor: 'accessing'!
fixedOffsets
	"DEFAULT - Answer the instance variable, fixedOffsets"

	fixedOffsets == nil
		ifTrue: [self fixedOffsets: OrderedCollection new].
	^fixedOffsets! !

!KSCTreeView methodsFor: 'accessing'!
fixedOffsets: anObject
	"DEFAULT - Set fixedOffsets to be anObject."

	fixedOffsets _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
initializeHidden
	"DEFAULT - Answer the instance variable, initializeHidden"

	initializeHidden isNil
		ifTrue:[initializeHidden _ 1000].
	^initializeHidden! !

!KSCTreeView methodsFor: 'accessing'!
initializeHidden: anObject
	"DEFAULT - Set initializeHidden to be anObject."

	initializeHidden _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
isSingleSelection
	"DEFAULT - tree views will normally be single select..."

	isSingleSelection == nil
		ifTrue:[	self model isSingleSelection: true.
				isSingleSelection _ true].
	^isSingleSelection! !

!KSCTreeView methodsFor: 'accessing'!
isSingleSelection: aBoolean
	"DEFAULT - Set isSingleSelection to be aBoolean."

	self isSingleSelection == aBoolean
		ifTrue:[^self].
	aBoolean
		ifTrue:[	self clearSelectedNodes.
				self selectedNodes size < 2
					ifFalse:[^self inform: 'Still multiple selections, model won''t release nodes']].
	self model isSingleSelection: aBoolean.
	isSingleSelection _ aBoolean! !

!KSCTreeView methodsFor: 'accessing'!
isVerticalTree
"true indicates a vertical tree, false indicates a horizontal tree"

	isVerticalTree isNil
		ifTrue:[isVerticalTree _ false].
	^isVerticalTree! !

!KSCTreeView methodsFor: 'accessing'!
isVerticalTree:aBoolean
"true displays a vertical tree, false displays a horizontal tree"

	self isVerticalTree = aBoolean
		ifFalse:[isVerticalTree _ aBoolean.
				"reset TreeNode display forms"
				"redisplay view"].! !

!KSCTreeView methodsFor: 'accessing'!
levelWidths
	"DEFAULT - Answer the instance variable, levelWidths"

	^levelWidths! !

!KSCTreeView methodsFor: 'accessing'!
levelWidths: anObject
	"DEFAULT - Set levelWidths to be anObject."

	levelWidths _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
metaRoot
"on initialization, create the metaRoot and decendents from the TreeEditor's roots"

	| myClass initLevel |	
	metaRoot isNil
		 ifTrue:[	myClass _ self class nodeClass.
				initLevel _ self initializeHidden.
				metaRoot _ myClass 	of: (KSCLinkedTreeNode reference: self)
									display: self displaySelector.
				self model roots
				do:[:root| metaRoot
							addChild:(myClass
										newTreeFrom: root
										 inject: [:old :new| new reference: old.
														 old level >= initLevel
															ifTrue:[new isInitiallyHidden:true]])]].
	^metaRoot! !

!KSCTreeView methodsFor: 'accessing'!
metaRoot: anObject
	"DEFAULT - Set metaRoot to be anObject."

	metaRoot ~~ nil
		ifTrue:[metaRoot release].
	self selectedNodes: nil.		"There will be none of the old node images selected!!"
	metaRoot _ anObject! !

!KSCTreeView methodsFor: 'accessing'!
selectedNodes
	"DEFAULT - Answer the instance variable, selectedNodes"

	selectedNodes == nil
		ifTrue: [self selectedNodes: (self model getSelectedNodes
									collect: [:node| self findImageOf: node])].
	^selectedNodes! !

!KSCTreeView methodsFor: 'accessing'!
selectedNodes: anObject
	"DEFAULT - Set selectedNodes to be anObject."

	selectedNodes _ anObject! !

!KSCTreeView methodsFor: 'scheduling'!
open
	"Open a standard system view with the receiver as its only subview."
	|topView|
	topView _ StandardSystemView
				model: self model
				label: 'Tree Browser'
				minimumSize: 150 @ 75.
	topView
		addSubView: self
		in: (0 @ 0 extent: 1 @ 1)
		borderWidth: 1.

	topView controller open! !

!KSCTreeView methodsFor: 'scheduling'!
openWithLabel: aString
	"Open a standard system view with the receiver as its only subview."
	|topView|
	topView _ StandardSystemView
				model: self model
				label: aString
				minimumSize: 150 @ 75.
	topView
		addSubView: self
		in: (0 @ 0 extent: 1 @ 1)
		borderWidth: 1.

	topView controller open! !

!KSCTreeView methodsFor: 'tree grooming'!
alignment: aBoolean

	aBoolean
		ifTrue:[self levelWidths: (Array new: self maxDepth withAll:0).
			    self configMenu disable: 1;
							  enable: 2]
		ifFalse:[self levelWidths: nil.
			     self configMenu disable: 2;
							  enable: 1].
	self resetAllNodeImages.
	self displayView.! !

!KSCTreeView methodsFor: 'tree grooming'!
isThereAlignment
"Find if alignment is active by checking if there is a 'levels array' present"

	^self levelWidths notNil! !

!KSCTreeView methodsFor: 'tree grooming'!
reAlign
"sample all node level widths. Values will be obeyed by each node image when his form is requested. Return the width of the whole tree"

	| myLevelWidths i width |
	self levelWidths: (Array new: self maxDepth withAll: 0).
	myLevelWidths _ self levelWidths.
	self metaRoot children
		do:[:child| child traverseFalse:
					[:level :node|	myLevelWidths at: level
										   put: ((myLevelWidths at: level)
												max: node labelForm width).
							     	node isHidden]
			level: 1].
	i _ 1.
	width _ 0.
	[(myLevelWidths at:i) == 0]
		whileFalse:
			[width _ width + (myLevelWidths at:i).
			  i _ i + 1].
	  ^width + (self branchLength * (i - 2 max: 0)) +
			self metaRoot hideIndicator width! !

!KSCTreeView methodsFor: 'tree grooming'!
resetAllNodeImages

	self metaRoot depthDo:[:image| image resetDisplayInformation].
	self canvas: nil.! !

!KSCTreeView methodsFor: 'model requests'!
checkChildrenOf: image with: node
"Make the image's children reflect the node's children. If image is restructured return true. If the structures are the same, return false."

	| modified realKids imageKids kid |
	modified _ false.
	realKids _ node children.
	imageKids _ image children.
	1 to: realKids size do:
		[:x| x > imageKids size
				ifTrue:[	modified _ true.
						kid _ self findImageOf: (realKids at:x).
						kid removeFromParent.
						image addChild: kid]
				ifFalse:[(realKids at:x) == (imageKids at:x) reference
						ifFalse:[modified _ true.
							    kid _ self findImageOf: (realKids at:x).
							    kid removeFromParent.
							    image addChild: kid before: (imageKids at:x)]]].
	realKids size + 1 to: imageKids size do:
		[:x| 	modified _ true.
			(kid _ imageKids last) removeFromParent.
			self cutImages add: kid].
	^modified! !

!KSCTreeView methodsFor: 'model requests'!
checkParentOf: image with: node
"Make the image's parent reflect the node's parent. If image is restructured return true. If the structures are the same, return false."

	| dad |
	(node parent == nil and:[image parent == self metaRoot]) "roots?"
		ifTrue:[^self restructureRoots "may be in different order..."].
	node parent == image parent reference
		ifTrue:[^false "same parent..."].
	image removeFromParent.
	self cutImages add: image. "image may be used later..."
	node parent isNil
		ifTrue:[self restructureRoots "should find 'node' as a new root!!..."]
		ifFalse:[	dad _ self findExistingImageOf: node parent.
				dad isNil
					ifFalse:[self 	checkChildrenOf: dad
								with: node parent]].
	^true! !

!KSCTreeView methodsFor: 'model requests'!
findUpdatedImageOf: aTreeNode
"Find the corresponding image. Return nil to ignore."

	| dad | 
	self metaRoot
		depthDo:[:image|  image reference == aTreeNode
							ifTrue:[^image "existing node..."]].
	aTreeNode hasParent
		ifFalse:[self restructureRoots. "new root..."
				^nil].
	
	dad _ aTreeNode parent.
	self metaRoot
		depthDo:[:image|  image reference == dad
							ifTrue:[^image "existing parent of new node..."]].
	^nil  "aTreeNode isn't associated with any of my trees..."! !

!KSCTreeView methodsFor: 'model requests'!
restructureRoots
"Make the image's roots reflect the model's roots."

	| realKids imageKids kid modified |
	RootsChecked ifTrue:[^false].
	modified _ false.
	RootsChecked _ true.
	realKids _ self model roots.
	imageKids _ self metaRoot children.
	1 to: realKids size do:
		[:x| x > imageKids size
				ifTrue:[	modified _ true.
						kid _ self findImageOf: (realKids at:x).
						kid removeFromParent.
						self metaRoot addChild: kid]
				ifFalse:[(realKids at:x) == (imageKids at:x) reference
						ifFalse:[modified _ true.
							     kid _ self findImageOf: (realKids at:x).
							     kid removeFromParent.
							     self metaRoot addChild: kid before: (imageKids at:x)]]].
	realKids size + 1 to: imageKids size do:
		[:x| 	modified _ true.
			(kid _ imageKids last) removeFromParent.
			self cutImages add: kid].
	^modified! !

!KSCTreeView methodsFor: 'model requests'!
updateNodes: aCollection
"Look at all nodes in the collection. Check for (and process) new or obsolete children. If the are none, simply reset the node display."

	| image modifiedKids modifiedParent |
	self offsetsAreFixed
		ifFalse:[self fixOffsetsNoWarning].
	RootsChecked _ false.
	aCollection do: [:changed|
		changed isNil
			ifTrue:[self restructureRoots]
			ifFalse:[image _ self findUpdatedImageOf: changed.
				     image notNil
						ifTrue:[modifiedKids _ self checkChildrenOf: image with: changed.
							    modifiedParent _ self checkParentOf: image with: changed.
							    modifiedKids | modifiedParent
								ifFalse:["redisplay node"
										image resetImageOfNode.
										image parent isNil
										ifFalse:[image parent
												doToRoot: [:dad| dad
																resetDisplayInformation]]]]]].! !

!KSCTreeView methodsFor: 'selecting'!
clearSelectedNodes
"User is asking to deselect any selected nodes."

	Cursor execute showWhile:
		[ self selectedNodes isEmpty
			ifFalse:[ self deselectNodeImages: self selectedNodes copy]].! !

!KSCTreeView methodsFor: 'selecting'!
deselectNodeImages: treeNodeImages
"Deselect any of the treeNodes currently selected and tell the editor what was actually deselected."

	| deselected |
	treeNodeImages isEmpty
		ifTrue:[^self]
		ifFalse:[deselected _ OrderedCollection new].
	treeNodeImages do: [:image|
		(self selectedNodes includes: image)
			ifTrue:[self selectedNodes remove: image.
				    deselected add: image reference.
				    self canvas fill: image selectArea 
							  rule: Form reverse 
							  fillColor: nil]].
	self displayView.
	deselected isEmpty ifFalse:[self model deselectNodes: deselected].! !

!KSCTreeView methodsFor: 'selecting'!
deselectNodes: treeNodes
"From the editor, make sure these nodes are deselected."

	treeNodes isEmpty
		ifFalse:[self deselectNodeImages:
						(treeNodes collect:[:node| self findImageOf: node])].! !

!KSCTreeView methodsFor: 'selecting'!
pickNode: aTreeNodeImage

	(self selectedNodes includes: aTreeNodeImage)
		ifTrue:[self deselectNodeImages: (OrderedCollection with: aTreeNodeImage)]
		ifFalse:[self selectNodeImages: (OrderedCollection with: aTreeNodeImage)].! !

!KSCTreeView methodsFor: 'selecting'!
selectNodeImages: treeNodeImages
"Request to select the treeNodeImages. If single selection is active, remove any previous selection and unHide the node if necessary."

	| newSelections |
	treeNodeImages isEmpty
		ifTrue:[^nil].
	self isSingleSelection
		ifTrue:[(self selectedNodes includes: treeNodeImages first)
					ifTrue:[^nil].
			    self selectedNodes isEmpty
					ifFalse:[self canvas  fill: (self selectedNodes removeFirst) selectArea 
									    rule: Form reverse 
									    fillColor: nil]].
	newSelections _ OrderedCollection new.
	treeNodeImages do:[:image|
		(self selectedNodes includes: image)
			ifFalse:[	newSelections add: image reference.
					self selectedNodes add: image.
					image root == self metaRoot
						ifFalse:[	self metaRoot: nil. "force an update of the tree"
								self canvas: nil.
								^self displayView].
					self isSingleSelection
						ifTrue:[image isOnCanvas
								ifTrue:[self updateSelectedNodes]
								ifFalse:[	image doToRoot:[:parent| parent isHidden: false].
										self canvas: nil]]
						ifFalse:[self canvas 	fill: image selectArea 
											rule: Form reverse 
											fillColor: nil.]]].
	self displayView.
	newSelections isEmpty
		ifFalse:[self model selectNodes: newSelections].! !

!KSCTreeView methodsFor: 'selecting'!
selectNodes: treeNodes
"Selection request from the editor."

	treeNodes isEmpty
		ifFalse:[self selectNodeImages:
					(treeNodes collect: [:node| self findImageOf: node])].! !

!KSCTreeView methodsFor: 'selecting'!
updateSelectedNodes

	| image hold absentNodes |
	self selectedNodes isEmpty
		ifTrue:[^self].
	self isSingleSelection
		ifTrue:[image _ self selectedNodes first.
				image root ~~ self metaRoot
	 				ifTrue:[self selectedNodes remove: image.
						    ^self model
							   deselectNodes: (OrderedCollection with: image reference)].
				self canvas	fill: image selectArea 
							rule: Form reverse 
							fillColor: nil.
				self bringIntoView: image]
		ifFalse:[hold _ self selectedNodes copy.
				absentNodes _ OrderedCollection new.
				hold do:[:img| img isOnCanvas
								ifTrue:[self canvas fill: img selectArea 
												  rule: Form reverse 
												  fillColor: nil]
								 ifFalse:[self selectedNodes remove: img.
										absentNodes add: img reference]].
				absentNodes isEmpty
					ifFalse: [self model deselectNodes: absentNodes]].! !

!KSCTreeView methodsFor: 'controller requests'!
findNode: aTreeNodeImage
"put this node in view. Un hide it if needed"

	| dad |
	aTreeNodeImage isOnCanvas
		ifTrue:[self bringIntoView: aTreeNodeImage]
		ifFalse:[(dad _ aTreeNodeImage parent) isNil
					ifFalse:[dad doToRoot:[:node| node isHidden: false]].
				self canvas: nil].
	(self selectedNodes includes: aTreeNodeImage)
		ifTrue:[^self displayView]
		ifFalse:[^self pickNode: aTreeNodeImage].! !

!KSCTreeView methodsFor: 'controller requests'!
findNodeNamed: aString
"find a node (that references a string) that matches aString"

	metaRoot depthDo:
		[:image| (image labelString = aString) & image isOnCanvas
					ifTrue:[	self bringIntoView: image.
							(self selectedNodes includes: image)
								ifTrue:[^self displayView]
								ifFalse:[^self pickNode: image]]].
	nil inform: 'node not found'.! !

!KSCTreeView methodsFor: 'controller requests'!
hide

	self fixOffsets.
	self selectedNodes
		do:[:node| node isHidden: true].
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests'!
hideAll

	self metaRoot depthDo: [:image| image reference hasChildren
									ifTrue:[image isHidden: true]].
	self canvasOffset: nil.
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests'!
hideAllAtLevel: anInteger

	self fixOffsets.
	self metaRoot	do: [:node| node isLeaf
									ifFalse:[node isHidden: true]]
					atLevel: anInteger.
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests'!
hideAllUnder
"Show all nodes under and including the selected node(s)"

	self fixOffsets.
	self selectedNodes do: [ :selected| selected depthDo: [:node | node isHidden: true]].
	self canvas: nil.
	self displayView! !

!KSCTreeView methodsFor: 'controller requests'!
hideAtLevel: anInteger

	self fixOffsets.
	self metaRoot	do: [:node| node isHidden: true]
					atLevel: anInteger
					onCondition: [ :node| node isHidden not].
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests' stamp: 'ssa 12/4/97 17:31'!
putTreeBitmapFullPageOn: aPrinter
	"Print the tree bitmap on a page on aPrinter."

	| pageSize treeResolution desiredDpi printerDpi pageFormExtent maxBytes pageForm chunkWidth |
	pageSize _ 10 @ 8. "in inches..."
	treeResolution _  self canvas extent.
	desiredDpi _ treeResolution // pageSize.
	desiredDpi _ desiredDpi x max: desiredDpi y.
	printerDpi _ aPrinter bestResolutionFor: desiredDpi.
	aPrinter dpiResolution: printerDpi.
	pageFormExtent _ (pageSize * printerDpi) rounded.
	maxBytes _ Smalltalk coreLeft // 2.
	(pageFormExtent x * pageFormExtent y // 8) < maxBytes
		ifTrue:
			[pageForm _ (Form extent: pageFormExtent depth: Display depth).
			self canvas
				displayOn: pageForm
				transformation: (WindowingTransformation scale: nil translation: self canvas offset negated)
				clippingBox: pageForm boundingBox
				align: self canvas boundingBox center
				with: pageForm boundingBox center
				rule: Form over
				fillColor: nil.
			aPrinter nextPutFormUpright: pageForm.
			pageForm _ nil]
		ifFalse:
			[chunkWidth _ (maxBytes * 8 // pageFormExtent y truncateTo: 16) max: 16.
			1 to: pageFormExtent x by: chunkWidth do: [:x |
				pageForm _ (Form extent: (chunkWidth min: (pageFormExtent x - x + 1)) @ pageFormExtent y depth: Display depth).
				self canvas
					displayOn: pageForm
					transformation: (WindowingTransformation scale: nil translation: self canvas offset negated + ((x - 1) negated @ 0))
					clippingBox: pageForm boundingBox
					align: self canvas boundingBox center
					with: pageFormExtent // 2
					rule: Form over
					fillColor: nil.
				aPrinter nextPutFormUpright: pageForm.
				pageForm _ nil]].
	aPrinter ff! !

!KSCTreeView methodsFor: 'controller requests'!
resetAndDisableSelections

	self selectedNodes
		do:[:node| node upTo: self metaRoot
					   	do: [:each| each resetDisplayInformation; selectArea: nil]].! !

!KSCTreeView methodsFor: 'controller requests'!
resetSelections

	self selectedNodes
		do:[:node| node upTo: self metaRoot
					   	do: [:each| each resetDisplayInformation]].! !

!KSCTreeView methodsFor: 'controller requests'!
selectedNodesOfModel

	^self selectedNodes collect:[:image| image reference]! !

!KSCTreeView methodsFor: 'controller requests'!
show

	self fixOffsets.
	self selectedNodes do: [:node | node isHidden: false].
	self canvas: nil.
	self displayView! !

!KSCTreeView methodsFor: 'controller requests'!
showAll

	self metaRoot depthDo:[:image| image isHidden: false].
	self canvasOffset: nil.
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests'!
showAllToLevel: anInteger

	self fixOffsets.
	self metaRoot 	do: [:level :node| node isHidden: false]
					toLevel: anInteger - 1.
	self canvas: nil.
	self displayView.! !

!KSCTreeView methodsFor: 'controller requests'!
showAllUnder
"Show all nodes under and including the selected node(s)"

	self fixOffsets.
	self selectedNodes do: [ :selected| selected depthDo: [:node | node isHidden: false]].
	self canvas: nil.
	self displayView! !

!KSCTreeView methodsFor: 'controller access'!
defaultControllerClass
	"Answer the class of the default controller. "

	^KSCTreeController! !

!KSCTreeView methodsFor: 'menus'!
treeViewMenu
"Return a new joinable helpMenu for tree views."

	| menuItems | 
	menuItems _ Array new:6.
	menuItems
		at: 1 put: self configMenu;
		at: 2 put: self class statsMenu;
		at: 3 put: self class hideMenu;
		at: 4 put: self class showMenu;
		at: 5 put: #deselectAll;
		at: 6 put: self class findNodeMenu.

	^self menuClass
		labelList: #(('configure view' 'tree stats')('hide' 'show')('deselect all' 'find node'))
		selectors: menuItems
		helpIdentifier: self menuHelpIdentifier! !

!KSCTreeView methodsFor: 'offsets'!
findNewOffset
"Called to determine a new display offset based on any previously determined fixedOffsets. Assumes new canvas is created."

	| fix |
	((self insetDisplayBox copy moveTo: 0@0) contains: canvas boundingBox)
		ifTrue:[self fixedOffsets: nil.
			    ^0@0].
	self fixedOffsets isEmpty
		ifTrue:[self fixedOffsets: nil.
			    ^self canvasOffset].

	fix _ self fixedOffsets detect:[:ass|  ass key isOnCanvas]
					  ifNone:[self fixedOffsets: nil.
							^0@0].
	self fixedOffsets: nil.
	^((0@0) - fix key selectArea origin) + fix value! !

!KSCTreeView methodsFor: 'offsets'!
fixOffsets
"find the selected node that is closest to the current canvas offset (view origin) and fix this node's (and its parents') position relative to the view. If no selected node is currently in view, the tree will be redisplayed from 0 @ 0. If there are no selected nodes, retain current offset"

	| area min box fix outOfView |
	self selectedNodes isEmpty
		ifTrue:[self fixedOffsets: nil.
			   ^true].
	box _ Rectangle origin: (0@0) - canvasOffset 
				    extent: self insetDisplayBox extent.
	min _ 0 @ 0 dist: self canvas extent.
	outOfView _ 0.
	self selectedNodes
		do:[:node|  area _ node selectArea.
				  (area intersects: box)
					ifTrue:[(box origin dist: area origin) < min
							ifTrue:[min _ box origin dist: area origin.
								    fix _ node]]
					ifFalse:[outOfView _ outOfView + 1]].
	(self heedWarning: outOfView)
		ifTrue:[^false].
	self fixedOffsets: nil.
	fix isNil
		ifFalse:[	fix
				upTo: metaRoot
				do:[:node| self fixedOffsets 
							addLast:(Association key: node
											  value: node selectArea origin + self canvasOffset)]].
	^true! !

!KSCTreeView methodsFor: 'offsets'!
fixOffsetsNoWarning
"find the selected node that is closest to the current canvas offset (view origin) and fix this node's (and its parents') position relative to the view. If no selected node is currently in view, the tree will be redisplayed from 0 @ 0. If there are no selected nodes, retain current offset"

	| area min box fix outOfView |
	self selectedNodes isEmpty
		ifTrue:[self fixedOffsets: nil.
			   ^true].
	box _ Rectangle origin: (0@0) - canvasOffset 
				    extent: self insetDisplayBox extent.
	min _ 0 @ 0 dist: self canvas extent.
	outOfView _ 0.
	self selectedNodes
		do:[:node|  area _ node selectArea.
				  (area intersects: box)
					ifTrue:[(box origin dist: area origin) < min
							ifTrue:[min _ box origin dist: area origin.
								    fix _ node]]
					ifFalse:[outOfView _ outOfView + 1]].
	self fixedOffsets: nil.
	fix isNil
		ifFalse:[	fix
				upTo: metaRoot
				do:[:node| self fixedOffsets 
							addLast:(Association key: node
											  value: node selectArea origin + self canvasOffset)]].
	^true! !

!KSCTreeView methodsFor: 'offsets'!
offsetsAreFixed
"answer a boolean to indicate whether or not offsets are already fixed"

	^fixedOffsets notNil! !

!KSCTreeView methodsFor: 'displaying'!
displayView
	"Display the canvas only if I am in place and my top view isn't collapsed"

	(self topView == self or:[self topView isCollapsed])
		ifTrue:[^self].

	self canvas
		displayOn: Display
		at: self insetDisplayBox origin
		clippingBox: self insetDisplayBox
		rule: Form over
		fillColor: Form black.! !

!KSCTreeView methodsFor: 'updating'!
update: aSymbol with: aParm

	aSymbol == #selectNodes: 
		ifTrue:[self perform: aSymbol with: aParm].
	aSymbol == #deselectNodes:
		ifTrue:[self perform: aSymbol with: aParm].
	aSymbol == #updateNodes:
		ifTrue:[self perform: aSymbol with: aParm].
	aSymbol == #treeStructure
		ifTrue:[self canvas: nil.
			    self displayView].
	aSymbol == #newTree
		ifTrue:[self metaRoot: nil.
			    self canvas: nil.
			    self displayView]! !

!KSCTreeView methodsFor: 'defaults'!
defaultBranchLength

	^32! !

!KSCTreeView methodsFor: 'defaults'!
defaultDisplayMsg
"The default will be the normal object display mechanism."

	^#asDisplayObject! !

!KSCTreeView methodsFor: 'defaults'!
maxDepth
"Define the maximum alignment depth of a tree displayed by treeView (cheap!!!! costs 2 bytes level)"

	^100! !

!KSCTreeView methodsFor: 'defaults'!
menuClass

	^ActionMenu! !

!KSCTreeView methodsFor: 'defaults'!
treeBorderMask
"Answer the mask to use for the border surrounding the tree on the canvas."

	^Color lightGray! !

!KSCTreeView methodsFor: 'defaults'!
treeBorderWidth
"Answer the number of pixels to use for the border surrounding the tree on the canvas."

	^10! !

!KSCTreeView methodsFor: 'private'!
bringIntoView: anImage
"Bring anImage into view if it currently can't be seen."

	| box delta |

	box _ Rectangle origin: (0@0) - self canvasOffset 
					extent: self insetDisplayBox extent.

	(box contains: anImage selectArea)
		ifTrue:[^self].

	self  clearInside: Form white.
	delta _ anImage selectArea amountToTranslateWithin: box.

	self canvas offset: self canvasOffset + delta.! !

!KSCTreeView methodsFor: 'private'!
deselectAllNodes
"request deselect of all nodes currently selected (model can override by reselecting)"

	| oldNodes | 
	oldNodes _ self selectedNodes.
	self selectedNodes: nil.
	self model deselectNodes: oldNodes.! !

!KSCTreeView methodsFor: 'private'!
doesNotUnderstand: aMessage
"I am the reference of the meta root. If I am plugged into some funky displaySelector, I likely won't understand when asked. Intercept any display selector and return a dummy displayObject."

	aMessage selector == self displaySelector
		ifTrue:[^'metaRoot' asText asDisplayText].
	^super doesNotUnderstand: aMessage! !

!KSCTreeView methodsFor: 'private'!
drawRoots
"Ask each root to display itself."

	| y |
	y _ self treeBorderWidth.
	self metaRoot children do: 
		[:root | root 	displayOn: self canvas
					at: self treeBorderWidth @ (y + (root myOffset - (root formSize y // 2)))
					withAlignment: levelWidths.
			    y _ y + root treeHeight].! !

!KSCTreeView methodsFor: 'private'!
findCutImageOf: aTreeNode
"search for an old corresponding image then a current image. If not found, make a new one."

	| myCuts | 
	myCuts _ self cutImages.
	myCuts   do:[:image| image reference == aTreeNode
							ifTrue:[^image]].

	^self class nodeClass	newTreeFrom: aTreeNode
						inject: [:old :new| new reference: old]! !

!KSCTreeView methodsFor: 'private'!
findExistingImageOf: aTreeNode
"search for the corresponding image. If not found, return nil."

	self metaRoot
		depthDo:[:image|  image reference == aTreeNode
							ifTrue:[^image]].
	^nil! !

!KSCTreeView methodsFor: 'private'!
findImageOf: aTreeNode
"search for the corresponding image. If not found, make a new one."

	| myCuts | 
	self metaRoot
		depthDo:[:image|  image reference == aTreeNode
							ifTrue:[^image]].
	myCuts _ self cutImages.
	myCuts   do:[:root| root depthDo:[:image| image reference == aTreeNode
											ifTrue:[^image]]].

	^self class nodeClass	newTreeFrom: aTreeNode
						inject: [:old :new| new reference: old]! !

!KSCTreeView methodsFor: 'private'!
heedWarning: anInteger
"While fixing offsets, anInteger selections were found out of view. Warn the user and return the boolean of a possible abort."

	anInteger == 0
		ifTrue:[^false].
	^(self confirm: anInteger printString,
				' selected node(s) are out of view. Continue?') not
	"The above is the more common way to do the below."
	"^(BinaryChoice
		message: anInteger printString,
				' selected node(s) are out of view. Continue?') not"! !

!KSCTreeView methodsFor: 'private'!
on: aModel

	self model: aModel.! !

!KSCTreeView methodsFor: 'private' stamp: 'ssa 12/4/97 17:32'!
sizeAndCreateCanvas
"size and create the canvas, add the grey border"

	| x y |
	x _ y _ 0.
	self metaRoot children do: [:root | y _ y + root treeHeight].
	self isThereAlignment
		ifTrue: [x _ self reAlign]
		ifFalse: [metaRoot children do: [:root | x _ x max: root treeWidth]].

	canvas _ Form extent: (x + (2 * self treeBorderWidth)) @ (y + (2 * self treeBorderWidth)) depth: Display depth.
	canvas
		border: canvas boundingBox
		width: self treeBorderWidth
		fillColor: self treeBorderMask.! !


!KSCTreeView class methodsFor: 'invoke (any hierarchy)'!
on: aModel seeds: treeSym parent: parentSym children: childrenSym aspect: aspectSym change: changeSym cutRequest: cutSym pasteRequest: pasteSym updateNode: updateSym menu: menuSym 
"Open a fully interactive EDITOR."

	| editor |
	editor _ self hierarchyEditorClass
				on: aModel
				tree: treeSym
				aspect: aspectSym
				change: changeSym
				cutRequest: cutSym
				pasteRequest: pasteSym
				updateNode: updateSym
				menu: menuSym
				parent: parentSym
				children: childrenSym.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (any hierarchy)'!
on: aModel seeds: treeSym parent: parentSym children: childrenSym aspect: aspectSym change: changeSym menu: menuSym 
"Open a BROWSER that allows a model's menu and can communicate selections. childrenSym and parentSym are selectors I can send to get a node's children and parent, respectively."

	| editor |
	editor _ self hierarchyEditorClass
				on: aModel
				tree: treeSym
				aspect: aspectSym
				change: changeSym
				cutRequest: nil
				pasteRequest: nil
				updateNode: nil
				menu: menuSym
				parent: parentSym
				children: childrenSym.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (any hierarchy)'!
on: aModel seeds: treeSym parent: parentSym children: childrenSym aspect: aspectSym change: changeSym  updateNode: updateSym menu: menuSym 
"Open an EDITOR that expects edit requests thru a model's menu."

	| editor |
	editor _ self hierarchyEditorClass
				on: aModel
				tree: treeSym
				aspect: aspectSym
				change: changeSym
				cutRequest: nil
				pasteRequest: nil
				updateNode: updateSym
				menu: menuSym
				parent: parentSym
				children: childrenSym.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on:aModel tree: aspectSymbol accept: acceptSymbol menu: menuSymbol
	"This method will prune or alter trees seperate from the model. When the user accepts the tree, the model is sent the acceptMsg with the modified tree as the parameter"
	"(KSCTreeView on: (KSCTreeNodeReference deriveHierarchyFrom: Magnitude parents: #superclass children: #subclasses) tree: #yourself acceptTree: nil) open"

	| browser | 
	browser _ self localEditorClass
					on: aModel
					tree: aspectSymbol
					accept: acceptSymbol
					menu: menuSymbol.
	^self new on: browser! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol
	"kwa -- 13 January 1989, changed 'changed:' to 'change:'"

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: nil
						 pasteRequest: nil
						 updateNode: nil
						 menu: nil.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol cutRequest: cutReqSymbol pasteRequest: pasteReqSymbol updateNode: updateSymbol

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: cutReqSymbol
						 pasteRequest: pasteReqSymbol
						 updateNode: updateSymbol
						 menu: nil.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol cutRequest: cutReqSymbol pasteRequest: pasteReqSymbol updateNode: updateSymbol menu: menuSymbol

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: cutReqSymbol
						 pasteRequest: pasteReqSymbol
						 updateNode: updateSymbol
						 menu: menuSymbol.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol menu: menuSymbol

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: nil
						 pasteRequest: nil
						 updateNode: nil
						 menu: menuSymbol.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol updateNode: updateSymbol

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: nil
						 pasteRequest: nil
						 updateNode: updateSymbol
						 menu: nil.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol updateNode: updateSymbol menu: menuSymbol

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: nil
						 pasteRequest: nil
						 updateNode: updateSymbol
						 menu: menuSymbol.
	^self new on: editor! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
on: aModel tree: aspectSymbol menu: menuSymbol
	"This method will browse a model's tree. The browser will not affect the tree."

	| browser | 
	browser _ self localEditorClass
					on: aModel
					tree: aspectSymbol
					menu: menuSymbol.
	^self new on: browser! !

!KSCTreeView class methodsFor: 'invoke (KSCHierarchy)'!
openOn: aModel tree: treeSymbol aspect: aspectSymbol change: changeSymbol menu: menuSymbol label: aString

	| editor | 
	editor _ self editorClass on: aModel
						 tree: treeSymbol
						 aspect: aspectSymbol
						 change: changeSymbol
						 cutRequest: nil
						 pasteRequest: nil
						 updateNode: nil
						 menu: menuSymbol.

	(self new on: editor) openWithLabel: aString! !

!KSCTreeView class methodsFor: 'private'!
editorClass

	^ KSCTreeEditor! !

!KSCTreeView class methodsFor: 'private'!
hierarchyEditorClass

	^ KSCHierarchyEditor! !

!KSCTreeView class methodsFor: 'private'!
localEditorClass

	^ KSCTreeEditorLocal! !

!KSCTreeView class methodsFor: 'private'!
nodeClass

	^KSCTreeNodeAlignableImage! !

!KSCTreeView class methodsFor: 'menus'!
configMenu
"This menu is for configure view."

	^self menuClass
		labelList: #(('align levels'
					'remove alignment'
					'vertical tree'
					'horizontal tree'
					'set branch length'))
		selectors: #(align deAlign setVertHoriz setVertHoriz newBranchLength)"
		helpIdentifier: self menuHelpIdentifier, '.configuration'"! !

!KSCTreeView class methodsFor: 'menus'!
findNodeMenu

	^self menuClass
		labelList: #(('search all nodes' 'search displayed nodes'))
		selectors: #(findNode findDisplayedNode)"
		helpIdentifier: self menuHelpIdentifier, '.find'"! !

!KSCTreeView class methodsFor: 'menus'!
hideMenu

	^self menuClass
		labelList: #(('hide all' 'hide all at level' 'hide all under selected' 'hide selected'))
		selectors: #(hideAll hideAllAtLevel hideAllUnder hide)"
		helpIdentifier: self menuHelpIdentifier, '.hide'"! !

!KSCTreeView class methodsFor: 'menus'!
menuClass

	^ActionMenu! !

!KSCTreeView class methodsFor: 'menus'!
showMenu

	^self menuClass
		labelList: #(('show all' 'show all to level' 'show all under selected' 'show selected'))
		selectors: #(showAll showAllToLevel showAllUnder show)"
		helpIdentifier: self menuHelpIdentifier, '.show'"! !

!KSCTreeView class methodsFor: 'menus'!
statsMenu

	^self menuClass
		labelList: #(('total tree' 'displayed tree'))
		selectors: #(statTotal statDisplayed)"
		helpIdentifier: self menuHelpIdentifier, '.stats'"! !

!KSCTreeView class methodsFor: 'menus'!
treeViewMenu
"Return a new joinable helpMenu for tree views."

	| menuItems | 
	menuItems _ Array new:6.
	menuItems
		at: 1 put: self configMenu;
		at: 2 put: self statsMenu;
		at: 3 put: self hideMenu;
		at: 4 put: self showMenu;
		at: 5 put: #deselectAll;
		at: 6 put: self findNodeMenu.

	^self menuClass
		labelList: #(('configure view' 'tree stats')('hide' 'show')('deselect all' 'find node'))
		selectors: menuItems"
		helpIdentifier: self menuHelpIdentifier"! !

!KSCTreeView class methodsFor: 'buffer access'!
pasteBuffer: aCollection

	self editorClass pasteBuffer: aCollection! !


KSCTreeController initialize!

