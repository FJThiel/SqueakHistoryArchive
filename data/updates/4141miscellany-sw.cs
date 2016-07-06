'From Squeak3.1alpha of 28 February 2001 [latest update: #4140] on 6 June 2001 at 3:21:12 pm'!"Change Set:		miscellany-swDate:			6 June 2001Author:			Scott Wallace(1)  Changes the wording of the prototypical TextMorph (such as is found in a Supplies flap) from 'Text for Editing' to 'Abc'.  Won't show up in Supplies flap unless you reinitialize flaps (e.g. by stopping the use of shared flaps and then starting it again).(2)  Fixes bug that could crop up in PluggableListMorphOfMany when #mouseMove: happened to get called while #dragOnOrOff was nil.(3)  Protects various methods against the new convention that message-list entries might have the pseudo-selectors #Comment, #Definition, or #Hierarchy, such as when requesting Versions for such an item.(4)  Arranges for CollapsedMorphs *not* to have zoom boxes.(5)  Restores the health of PluggableListMorph.verifyContents, which was not savvy about the recent shift to MethodReference objects and consequently could drop the user into a debugger.(6)  Modifies the check-for-methods-without-comments check in the change sorter so that only precode comments are considered in the search."!!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 6/6/2001 15:10'!firstPrecodeCommentFor:  selector	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"	| parser source tree |	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"	(MessageSet isPseudoSelector: selector)		ifTrue:			["Not really a selector"			^ nil].	source _ self sourceCodeAt: selector asSymbol.	parser _ self parserClass new.	tree _ 		parser			parse: (ReadStream on: source)			class: self			noPattern: false			context: nil			notifying: nil			ifFail: [^ nil].	^ (tree comment ifNil: [^ nil]) first! !!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 6/6/2001 13:26'!precodeCommentOrInheritedCommentFor:  selector	"Answer a string representing the first comment in the method associated with selector, considering however only comments that occur before the beginning of the actual code.  If the version recorded in the receiver is uncommented, look up the inheritance chain.  Return nil if none found."	| aSuper aComment |	^ (aComment _ self firstPrecodeCommentFor: selector) isEmptyOrNil		ifFalse:			[aComment]		ifTrue:			[(self == Behavior or: [superclass == nil or:							[(aSuper _ superclass classThatUnderstands: selector) == nil]])				ifTrue:					[nil]				ifFalse:					[aSuper precodeCommentOrInheritedCommentFor: selector]]"ActorState precodeCommentOrInheritedCommentFor: #printOn:"! !!ChangeSet methodsFor: 'moving changes' stamp: 'sw 6/6/2001 13:37'!methodsWithoutComments	"Return a collection representing methods in the receiver which have no precode comments"	| slips |	slips _ OrderedCollection new.	self changedClasses do:		[:aClass |		(self methodChangesAtClass: aClass name) associationsDo: 				[:mAssoc | (#(remove addedThenRemoved) includes: mAssoc value) ifFalse:					[(aClass selectors includes:  mAssoc key) ifTrue:						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].	^ slips	"Smalltalk browseMessageList: (Smalltalk changes methodsWithoutComments) name: 'methods lacking comments'"! !!CollapsedMorph methodsFor: 'queries' stamp: 'sw 6/5/2001 22:55'!wantsExpandBox	"Answer whether I'd like an expand box"	^ false! !!PluggableListMorph methodsFor: 'updating' stamp: 'sw 6/6/2001 14:38'!verifyContents	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes.  Also called periodically in morphic if the smartUpdating preference is true"	| newList existingSelection anIndex listOfStrings |	newList _ self getList.	((list == newList) "fastest" or: [list = newList]) ifTrue: [^ self].	self flash.  "list has changed beneath us; give the user a little visual feedback that the contents of the pane are being updated."	existingSelection _ selection.	self list: newList.	listOfStrings _ newList collect: [:el | el asStringOrText].  "the newList itself may have MethodReference objects"	(existingSelection notNil and: [(anIndex _ listOfStrings indexOf: existingSelection asStringOrText ifAbsent: [nil]) notNil])		ifTrue:			[model noteSelectionIndex: anIndex for: getListSelector.			self selectionIndex: anIndex]		ifFalse:			[self changeModelSelection: 0]! !!PluggableListMorphOfMany methodsFor: 'event handling' stamp: 'sw 6/5/2001 00:32'!mouseMove: event	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"	| index oldIndex oldVal aMorph |	event position y < self top		ifTrue:			[scrollBar scrollUp: 1.			aMorph _ self itemFromPoint: scroller topLeft + (1@1)]		ifFalse:			[event position y > self bottom				ifTrue:					[scrollBar scrollDown: 1.					aMorph _ self itemFromPoint: scroller bottomLeft + (1@-1)]				ifFalse:					[aMorph _ self itemFromPoint: event position]].	aMorph ifNil: [^ super mouseDown: event].	model okToChange ifFalse: [^ self].  "No change if model is locked"	index _ scroller submorphs indexOf: aMorph.	index = 0 ifTrue: [^ self  "minimize chance of selecting with a pane border drag"].	"Set meaning for subsequent dragging of selection"	oldIndex _ self getCurrentSelectionIndex.	oldIndex ~= 0 ifTrue: [oldVal _ model listSelectionAt: oldIndex].	"Set or clear new primary selection (listIndex)"	dragOnOrOff == true		ifTrue: [self setSelectedMorph: aMorph]		ifFalse: [self setSelectedMorph: nil].	"Need to restore the old one, due to how model works, and set new one."	oldIndex ~= 0 ifTrue: [model listSelectionAt: oldIndex put: oldVal].	model listSelectionAt: index put: dragOnOrOff.	aMorph changed! !!StringHolder methodsFor: 'message list menu' stamp: 'sw 6/6/2001 15:11'!browseVersions	"Create and schedule a message set browser on all versions of the currently selected message selector."	| class selector |	(selector _ self selectedMessageName) ifNotNil:		[(MessageSet isPseudoSelector: selector) ifTrue:			[self inform: 'Sorry, only actual methods have retrievable versions.'.			^ self].		class _ self selectedClassOrMetaClass.		VersionsBrowser			browseVersionsOf: (class compiledMethodAt: selector)			class: self selectedClass			meta: class isMeta			category: self selectedMessageCategoryName			selector: selector]! !!MessageSet class methodsFor: 'utilities' stamp: 'sw 6/6/2001 15:09'!isPseudoSelector: aSelector	"Answer whether the given selector is a special marker"	^ #(Comment Definition Hierarchy) includes: aSelector! !!TextMorph methodsFor: 'private' stamp: 'sw 5/17/2001 12:30'!setDefaultContentsIfNil	"Set the default contents"	text ifNil: [text _ 'Abc' asText allBold]! !!CollapsedMorph reorganize!('menu' buildWindowMenu)('queries' isMyUncollapsedMorph: wantsExpandBox)('collapse/expand' beReplacementFor: collapseOrExpand uncollapseToHand)!