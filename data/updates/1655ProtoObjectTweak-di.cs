'From Squeak2.6 of 11 October 1999 [latest update: #1653] on 26 November 1999 at 7:46:21 pm'!"Change Set:		ProtoObjectTweak-diDate:			26 November 1999Author:			Dan IngallsA few further tweaks to the ProtoObject change, to use Object where appropriate in traversing the entire Object hirearchy."!!ContextPart methodsFor: 'controlling' stamp: 'di 11/26/1999 19:34'!send: selector to: rcvr with: args super: superFlag 	"Simulate the action of sending a message with selector, selector, and 	arguments, args, to receiver. The argument, superFlag, tells whether the 	receiver of the message was specified with 'super' in the source method."	| class meth val |	class _ superFlag			ifTrue: [(self method literalAt: self method numLiterals) value superclass]			ifFalse: [rcvr class].	meth _ class lookupSelector: selector.	meth == nil		ifTrue: [^ self send: #doesNotUnderstand:					to: rcvr					with: (Array with: (Message selector: selector arguments: args))					super: superFlag]		ifFalse: [val _ self tryPrimitiveFor: meth						receiver: rcvr						args: args.				val == PrimitiveFailToken ifFalse: [^ val].				(selector == #doesNotUnderstand: and: [class == ProtoObject]) ifTrue:					[^ self error: 'Simulated message ' , (args at: 1) selector									, ' not understood'].				^ self activateMethod: meth					withArgs: args					receiver: rcvr					class: class]! !!ProtocolBrowser methodsFor: 'private' stamp: 'di 11/26/1999 19:39'!onSubProtocolOf: aClass 	"Initialize with the entire protocol for the class, aClass,		but excluding those inherited from Object."	| selectors |	selectors := Set new.	aClass withAllSuperclasses do:		[:each | (each == Object or: [each == ProtoObject]) 			ifFalse: [selectors addAll: each selectors]].	self initListFrom: selectors asSortedCollection		highlighting: aClass! !!SystemDictionary methodsFor: 'retrieving' stamp: 'di 11/26/1999 19:23'!allBehaviorsDo: aBlock 	"Evaluate the argument, aBlock, for each kind of Behavior in the system 	(that is, Object and its subclasses).	ar 7/15/1999: The code below will not enumerate any obsolete or anonymous	behaviors for which the following should be executed:		Smalltalk allObjectsDo:[:obj| obj isBehavior ifTrue:[aBlock value: obj]].	but what follows is way faster than enumerating all objects."	aBlock value: ProtoObject.	ProtoObject allSubclassesDoGently: aBlock.		"don't bring in ImageSegments"	"Classes outside the ProtoObject hierarchy"	Class subclassesDo: [:aClass |		aClass == ProtoObject class ifFalse: ["Do not enumerate ProtoObject"			"Enumerate the non-meta class and its subclasses"			aBlock value: aClass soleInstance.			aClass soleInstance allSubclassesDoGently: aBlock]].! !"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."!