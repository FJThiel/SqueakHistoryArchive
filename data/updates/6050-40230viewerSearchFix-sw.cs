'From Squeakland 3.2.4913 of 10 July 2002 [latest update: #225] on 9 August 2004 at 10:12:48 am'!"Change Set:		viewerSearchFix-swDate:			9 August 2004Author:			Scott WallaceFixes the bug that dropped you into a debugger when you requested a search (in a viewer's Search pane) for 'color'.Çaution: this fix is for the Squeakland/3.2-based system in use during school-year 2003-4 only; the new 3.8-based Squeakland image of August 2004 requires a different fix."!!MethodInterface methodsFor: 'initialization' stamp: 'sw 8/9/2004 10:12'!initializeFromEToySlotSpec: tuple	"tuple holds an old etoy slot-item spec, of the form found in #additionsToViewerCategories methods.   Initialize the receiver to hold the same information"	| setter |	selector _ tuple seventh.	self		wording: (ScriptingSystem wordingForOperator: tuple second);		helpMessage: tuple third.	receiverType _ #Player.	resultSpecification _ ResultSpecification new.	resultSpecification resultType: tuple fourth.	(#(getNewClone "seesColor: isOverColor:") includes: selector)  "Test disenfranchised for etoy vocabs sw 8/9/2004"		ifTrue:			[self setNotToRefresh]  "actually should already be nil"		ifFalse:			[self setToRefetch].	((tuple fifth == #readWrite) and: [((tuple size >= 9) and: [(setter _ tuple at: 9) ~~ #unused])]) ifTrue:		[resultSpecification companionSetterSelector: setter].		"An example of an old slot-item spec:(slot numericValue 'A number representing the current position of the knob.' number readWrite Player getNumericValue Player setNumericValue:)	1	#slot	2	wording	3	balloon help	4	type	5	#readOnly or #readWrite	6	#Player (not used -- ignore)	7	getter selector	8	#Player (not used -- ignore)	9	setter selector"	! !"Postscript:"Vocabulary initialize.!