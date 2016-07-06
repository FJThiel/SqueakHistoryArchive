'From Squeak 2.2beta of Sept 11, 1998 on 15 September 1998 at 2:38:27 pm'!"Change Set:		MoreSqueakTweaksDate:			12 September 1998Author:			Dan IngallsMore Tweaks form various authors...Fix for FormEditor frame (Tim Olson).Fix for FormView frame (Dan Ingalls).Fix for simulation of perform: (Ale Reimondo)Block fork now returns the resulting process (suggested by Bob Arning)Tweak to String = (Dan Ingalls)Removed a patch from serverURLs (Dan Ingalls)Removed an unused method from ImageReadWriter class."!!ContextPart methodsFor: 'private' stamp: 'afr 9/11/1998 19:50'!doPrimitive: primitiveIndex receiver: receiver args: arguments 	"Simulate a primitive method whose index is primitiveIndex.  The	simulated receiver and arguments are given as arguments to this message.""	NOTE: In order for perform:WithArguments: to work reliably here,	this method must be forced to invoke a large context.  This is done	by adding extra temps until the following expression evaluates as true:		(ContextPart compiledMethodAt: #doPrimitive:receiver:args:) frameSize > 20"	| value t1 t2 t3 |	<primitive: 19> "Simulation guard"	"If successful, push result and return resuming context,		else ^ #simulatorFail"	(primitiveIndex = 19) ifTrue:[		Debugger 			openContext: self			label:'Code simulation error'			contents: self shortStack].	(primitiveIndex = 80 and: [receiver isKindOf: ContextPart])		ifTrue: [^self push: 					((BlockContext new: receiver size)						home: receiver home						startpc: pc + 2						nargs: (arguments at: 1))].	(primitiveIndex = 81 and: [receiver isMemberOf: BlockContext])		ifTrue: [^receiver pushArgs: arguments from: self].	primitiveIndex = 83 "afr 9/11/1998 19:50"		ifTrue: [^ self send: arguments first to: receiver					with: arguments allButFirst					super: false].	primitiveIndex = 84 "afr 9/11/1998 19:50"		ifTrue: [^ self send: arguments first to: receiver					with: (arguments at: 2)					super: false].	arguments size > 6 ifTrue: [^#simulatorFail].	value _ receiver tryPrimitive: primitiveIndex withArgs: arguments.	value == #simulatorFail		ifTrue: [^ #simulatorFail]		ifFalse: [^ self push: value]! !!BlockContext methodsFor: 'scheduling' stamp: 'di 9/12/1998 11:53'!fork	"Create and schedule a Process running the code in the receiver."	^ self newProcess resume! !!FormEditor class methodsFor: 'private' stamp: 'di 9/12/1998 12:28'!createOnForm: aForm	"Create a StandardSystemView for a FormEditor on the form aForm."	| formView formEditor menuView aView topView extent topViewBorder |	topViewBorder _ 2.	formView _ FormHolderView new model: aForm.	formEditor _ formView controller.	menuView _ FormMenuView new makeFormEditorMenu model: formEditor.	formEditor model: aForm.	aView _ View new.	aView model: aForm.	aView addSubView: formView.	aView 		addSubView: menuView		align: menuView viewport topCenter		with: formView viewport bottomCenter + (0@16).	aView window: 		((formView viewport 			merge: (menuView viewport expandBy: (16 @ 0 corner: 16@16))) 		  expandBy: (0@topViewBorder corner: 0@0)).	topView _ StandardSystemView new.	topView backgroundColor: #veryLightGray.	topView addSubView: aView.	topView label: 'Form Editor'.	topView borderWidth: topViewBorder.	extent _ topView viewport extent.	topView minimumSize: extent.	topView maximumSize: extent.	^topView! !!FormView class methodsFor: 'examples' stamp: 'di 9/12/1998 10:17'!open: aForm named: aString	"FormView open: ((Form extent: 100@100) borderWidth: 1) named: 'Squeak' "	"Open a window whose model is aForm and whose label is aString."	| topView aView |	topView _ StandardSystemView new.	topView model: aForm.	topView label: aString.	topView minimumSize: 80@80.	aView _ FormView new.	aView model: aForm.	aView window: (aForm boundingBox expandBy: 2).	aView borderWidthLeft: 2 right: 2 top: 2 bottom: 2.	topView addSubView: aView.	topView controller open! !!String methodsFor: 'comparing' stamp: 'di 9/14/1998 16:29'!= aString 	"Answer whether the receiver sorts equally as aString.	The collation order is simple ascii (with case differences)."	aString species == String ifFalse: [^ false].	^ (self compare: self with: aString collated: AsciiOrder) = 2! !!Utilities class methodsFor: 'fetching updates' stamp: 'di 9/14/1998 13:27'!serverUrls	"Return the current list of server URLs.  For code updates.  Format of UpdateUrlLists is #( ('squeak updates' ('url1' 'url2'))    ('some other updates' ('url3' 'url4')))"	^ UpdateUrlLists first last! !ImageReadWriter class removeSelector: #formFromFile:!"Postscript:Init the ParagraphEditor command key table."ParagraphEditor initialize.!