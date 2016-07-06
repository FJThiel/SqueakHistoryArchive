'From Squeak3.2 of 22 February 2002 [latest update: #4956] on 5 June 2003 at 1:39:49 pm'!"Change Set:		InspectElement-tkDate:			5 June 2003Author:			Ted KaehlerIn an Inspector, if the selected item is a SequenceableCollection, the menu offers 'inspect element...'.  With this improvement, the 'inspect element...' menu shows the printString of each element along with its index.  This helps the user pick the right element to inspect.  The print string is limited to 25 characters."!!Inspector methodsFor: 'menu commands' stamp: 'tk 6/5/2003 13:34'!inspectElement	| sel selSize countString count nameStrs |	"Create and schedule an Inspector on an element of the receiver's model's currently selected collection."	self selectionIndex = 0 ifTrue: [^ self changed: #flash].	((sel _ self selection) isKindOf: SequenceableCollection) ifFalse:		[(sel isKindOf: MorphExtension) ifTrue: [^ sel inspectElement].		^ sel inspect].	(selSize _ sel size) == 1 ifTrue: [^ sel first inspect].	selSize <= 20 ifTrue:		[nameStrs _ (1 to: selSize) asArray collect: [:ii | 			ii printString, '   ', ((sel at: ii) printStringLimitedTo: 25)].		count _ PopUpMenu withCaption: 'which element?' chooseFrom: nameStrs.		count = 0 ifTrue: [^ self].		^ (sel at: count) inspect].	countString _ FillInTheBlank request: 'Which element? (1 to ', selSize printString, ')' initialAnswer: '1'.	countString isEmptyOrNil ifTrue: [^ self].	count _ Integer readFrom: (ReadStream on: countString).	(count > 0 and: [count <= selSize])		ifTrue: [(sel at: count) inspect]		ifFalse: [self beep]! !