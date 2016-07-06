'From Squeak2.6 of 11 October 1999 [latest update: #1595] on 8 November 1999 at 5:53:20 pm'!"Change Set:		menuConstructor-swDate:			8 November 1999Author:			Scott WallaceProvides a more direct way to construct a SelectionMenu, which keeps the labels and corresponding selectors together, and allows lines to be specified more straightforwardly.The call is:  SelectionMenu fromArray: anArraywhere each of anArray's elements is either a pair of the form     <label string>   <selector>or else, to specify a line, simply a - symbol.Example  SelectionMenu fromArray:	#(	('first label'		first)		('second label'	second)		-		('third label' 	third))is the equivalent of   SelectionMenu labels: #('first label' 'second label' 'third label') lines: #(2) selectors: #(first second third)To see a serious use of this constructor, see method ParagraphEditor class #shiftedYellowButtonMenu, the first method in the system to take advantage of the new construct."!!SelectionMenu class methodsFor: 'instance creation' stamp: 'sw 11/8/1999 17:52'!fromArray: anArray	"Construct a menu from anArray.  The elements of anArray must be either:	*  A pair of the form: <label> <selector>or	*  The 'dash' (or 'minus sign') symbol	Refer to the example at the bottom of the method"	| labelList lines selections anIndex |	labelList _ OrderedCollection new.	lines _ OrderedCollection new.	selections _ OrderedCollection new.	anIndex _ 0.	anArray do:		[:anElement |			anElement size == 1				ifTrue:					[(anElement == #-) ifFalse: [self error: 'badly-formed menu constructor'].					lines add: anIndex]				ifFalse:					[anElement size == 2 ifFalse: [self error: 'badly-formed menu constructor'].					anIndex _ anIndex + 1.					labelList add: anElement first.					selections add: anElement second]].	^ self labelList: labelList lines: lines selections: selections"(SelectionMenu fromArray:	#(	('first label'		moja)		('second label'	mbili)		-		('third label' 	tatu)		-		('fourth label'	nne)		('fifth label'	tano))) startUp"! !!CustomMenu class methodsFor: 'example' stamp: 'sw 11/8/1999 17:27'!example	"CustomMenu example"	| menu |	menu _ CustomMenu new.	menu add: 'apples' action: #apples.	menu add: 'oranges' action: #oranges.	menu addLine.	menu addLine.  "extra lines ignored"	menu add: 'peaches' action: #peaches.	menu addLine.	menu add: 'pears' action: #pears.	menu addLine.	^ menu startUp: #apples"NB:  The following is equivalent to the above, but uses the compact #fromArray: consruct:	(CustomMenu fromArray:		#(	('apples'		apples)			('oranges'		oranges)			-			-			('peaches'		peaches)			-			('pears'			pears)			-))				startUp: #apples"! !