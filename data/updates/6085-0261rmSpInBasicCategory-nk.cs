'From Squeakland 3.8.5976 of 19 August 2004 [latest update: #260] on 23 August 2004 at 6:51:49 pm'!"Change Set:		RemoveSpaceInBasicCategory-nkDate:			23 August 2004Author:			Ned KonzRemoves an annoying stray space in the wording of the 'Basic' category in the Objects tool.Just in case the space was there to force Basic to come first (but who would do that?), also makes sure that Basic (or ' Basic', or translated versions of same) come first anyway."!!EllipseMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!descriptionForPartsBin	^ self partName:	'Ellipse'		categories:		#('Graphics' 'Basic')		documentation:	'An elliptical or circular shape'! !!ImageMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!descriptionForPartsBin	^ self partName:	'Image'		categories:		#('Graphics' 'Basic')		documentation:	'A non-editable picture.  If you use the Paint palette to make a picture, you can edit it afterwards.'! !!ObjectsTool methodsFor: 'categories' stamp: 'nk 8/23/2004 18:18'!tabsForCategories	"Answer a list of buttons which, when hit, will trigger the choice of a category"	| buttonList aButton classes categoryList basic |	classes _ Morph withAllSubclasses.	categoryList _ Set new.	classes do: [:aClass |		(aClass class includesSelector: #descriptionForPartsBin) ifTrue:			[categoryList addAll: aClass descriptionForPartsBin translatedCategories].		(aClass class includesSelector: #supplementaryPartsDescriptions) ifTrue:			[aClass supplementaryPartsDescriptions do:				[:aDescription | categoryList addAll: aDescription translatedCategories]]].	categoryList _ OrderedCollection withAll: (categoryList asSortedArray).		basic := categoryList remove: ' Basic' translated ifAbsent: [ ].	basic ifNotNil: [ categoryList addFirst: basic ].	basic := categoryList remove: 'Basic' translated ifAbsent: [ ].	basic ifNotNil: [ categoryList addFirst: basic ].	buttonList _ categoryList collect:		[:catName |			aButton _ SimpleButtonMorph new label: catName.			aButton actWhen: #buttonDown.			aButton target: self; actionSelector: #showCategory:fromButton:; arguments: {catName. aButton}].	^ buttonList"ObjectsTool new tabsForCategories"! !!PaintInvokingMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!descriptionForPartsBin	^ self partName:	'Paint'		categories:		#('Basic' 'Graphics')		documentation:	'Drop this icon to start painting a new object.'! !!PolygonMorph class methodsFor: 'instance creation' stamp: 'nk 8/23/2004 18:12'!supplementaryPartsDescriptions	^ {DescriptionForPartsBin		formalName: 'Arrow'		categoryList: #('Basic' 'Graphics')		documentation: 'A line with an arrowhead.  Shift-click to get handles and move the ends.'		globalReceiverSymbol: #PolygonMorph		nativitySelector: #arrowPrototype}! !!PolygonMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'Polygon'		categories:		#('Graphics' 'Basic')		documentation:	'A series of connected line segments, which may be a closed solid, or a zig-zag line.  Shift-click to get handles and move the points.'! !!CurveMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!descriptionForPartsBin	^ self partName:	'Curve'		categories:		#('Graphics' 'Basic')		documentation:	'A smooth wiggly curve, or a curved solid.  Shift-click to get handles and move the points.'! !!CurveMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!supplementaryPartsDescriptions	^ {DescriptionForPartsBin		formalName: 'Curvy Arrow'		categoryList: #('Basic' 'Graphics')		documentation: 'A curved line with an arrowhead.  Shift-click to get handles and move the points.'		globalReceiverSymbol: #CurveMorph		nativitySelector: #arrowPrototype}! !!LineMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:11'!descriptionForPartsBin	^ self partName:	'Line'		categories:		#('Graphics' 'Basic')		documentation:	'A straight line.  Shift-click to get handles and move the ends.'! !!RectangleMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'Rectangle'		categories:		#('Graphics' 'Basic')		documentation:	'A rectangular shape, with border and fill style'! !!RectangleMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!supplementaryPartsDescriptions	^ {DescriptionForPartsBin		formalName: 'RoundRect'		categoryList: #('Graphics' 'Basic')		documentation: 'A rectangle with rounded corners'		globalReceiverSymbol: #RectangleMorph		nativitySelector: #roundRectPrototype.	DescriptionForPartsBin		formalName: 'Gradient'		categoryList: #('Graphics' 'Basic')		documentation: 'A rectangle with a horizontal gradient'		globalReceiverSymbol: #RectangleMorph		nativitySelector: #gradientPrototype.	DescriptionForPartsBin		formalName: 'Gradient (slanted)'		categoryList: #('Graphics' 'Basic')		documentation: 'A rectangle with a diagonal gradient'		globalReceiverSymbol: #RectangleMorph		nativitySelector: #diagonalPrototype}! !!NumericReadoutTile class methodsFor: 'instance creation' stamp: 'nk 8/23/2004 18:11'!supplementaryPartsDescriptions	"Answer additional items for the parts bin"	Preferences universalTiles ifFalse: [^ #()].	^ {DescriptionForPartsBin		formalName: 'Number (fancy)'		categoryList: #('Basic')		documentation: 'A number readout for a Stack.  Shows current value.  Click and type the value.  Shift-click on title to edit.'		globalReceiverSymbol: #NumericReadoutTile		nativitySelector: #authoringPrototype.	   DescriptionForPartsBin		formalName: 'Number (bare)'		categoryList: #('Basic')		documentation: 'A number readout for a Stack.  Shows current value.  Click and type the value.'		globalReceiverSymbol: #NumericReadoutTile		nativitySelector: #simplePrototype.	   DescriptionForPartsBin		formalName: 'Number (mid)'		categoryList: #('Basic')		documentation: 'A number readout for a Stack.  Shows current value.  Click and type the value.'		globalReceiverSymbol: #NumericReadoutTile		nativitySelector: #borderedPrototype}! !!ScriptableButton class methodsFor: 'name' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'Button'		categories:		#('Scripting' 'Basic')		documentation:	'A button to use with tile scripting; its script will be a method of its containing playfield'! !!TTSampleStringMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'TrueType banner'		categories:		#('Text' 'Basic')		documentation:	'A short text in a beautiful font.  Use the resize handle to change size.'! !!TextMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'Text'		categories:		#('Text' 'Basic')		documentation:	'A raw piece of text which you can edit into anything you want'! !!TextMorph class methodsFor: 'parts bin' stamp: 'nk 8/23/2004 18:12'!supplementaryPartsDescriptions	^ {DescriptionForPartsBin		formalName: 'Text (border)'		categoryList: #('Basic' 'Text')		documentation: 'A text field with border'		globalReceiverSymbol: #TextMorph		nativitySelector: #borderedPrototype.	DescriptionForPartsBin		formalName: 'Text (fancy)'		categoryList: #('Basic' 'Text')		documentation: 'A text field with a rounded shadowed border, with a fancy font.'		globalReceiverSymbol: #TextMorph		nativitySelector: #fancyPrototype.}! !!TrashCanMorph class methodsFor: 'miscellaneous' stamp: 'nk 8/23/2004 18:12'!descriptionForPartsBin	^ self partName:	'Trash'		categories:		#('Useful' 'Basic')		documentation:	'a tool for discarding objects'! !