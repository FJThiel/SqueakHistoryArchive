"      NAME	CurveButton
       AUTHOR	cgreuter@calum.csclub.uwaterloo.ca (Chris Reuter)
       URL	(none)
       FUNCTION	A fully-functional non-rectangular button
       KEYWORDS	Morphic widget button
       ST-VERSIONS	Squeak
       PREREQUISITES	(none)
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	1.0
       DATE	16-Apr-98

SUMMARY

The CurveButton is a simple Morphic button whoseshape is defined by splines, just like aCurveMorph, yet functions like a button.  It canbe used to put more goo in your GUI.

				Chris Reuter
"!
'From Squeak 1.31 of Feb 4, 1998 on 16 April 1998 at 11:57:04 am'!
CurveMorph subclass: #CurveButton
	instanceVariableNames: 'target actionSelector arguments actWhen oldColor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!CurveButton reorganize!
('initialization' defaultVertices initialize)
('events' doButtonAction handlesMouseDown: mouseDown: mouseMove: mouseUp:)
('menu' addCustomMenuItems:hand: makeClosed makeOpen setActionSelector setActWhen setArguments setLabel setTarget:)
('accessing' actionSelector actionSelector: actWhen: arguments arguments: label label: label:font: target target:)
('copying' updateReferencesUsing:)
('private' centerLabel computeBounds)
!


!CurveButton methodsFor: 'initialization' stamp: 'cr 3/20/98 03:24'!
defaultVertices
	"Return a collection of vertices to initialize self with.  The array was created by cutting from
	 the vertices list a Morph inspector produced."
	| result |
	result := WriteStream on: Array new.
	(#(90@184 147@184 149@211 88@213) 
			reject: [:item | item == #@]
	  ) pairsDo: [:x :y | result nextPut: x @ y].

	^ result contents.! !

!CurveButton methodsFor: 'initialization' stamp: 'cr 3/20/98 03:18'!
initialize
	"Initialize me."
	super initialize.

	self vertices: self defaultVertices asArray
		color: (Color r: 0.4 g: 0.8 b: 0.599)
		borderWidth: 2
		borderColor: #raised.

	target _ nil.
	actionSelector _ #flash.
	arguments _ EmptyArray.
	actWhen _ #buttonUp.
	self label: 'Flash'.
! !

!CurveButton methodsFor: 'events' stamp: 'cr 3/20/98 01:29'!
doButtonAction
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	(target ~~ nil and: [actionSelector ~~ nil]) ifTrue: [
		Cursor normal showWhile: [
			target perform: actionSelector withArguments: arguments]].
! !

!CurveButton methodsFor: 'events' stamp: 'cr 3/20/98 01:25'!
handlesMouseDown: evt
	^  self isPartsDonor not
! !

!CurveButton methodsFor: 'events' stamp: 'cr 3/20/98 01:31'!
mouseDown: evt

	oldColor _ color.
	actWhen == #buttonDown
		ifTrue: [self doButtonAction].
! !

!CurveButton methodsFor: 'events' stamp: 'cr 3/20/98 01:31'!
mouseMove: evt

	(self containsPoint: evt cursorPoint)
		ifTrue: [self color: (oldColor mixed: 1/2 with: Color white).
				(actWhen == #whilePressed and: [evt anyButtonPressed])
					 ifTrue: [self doButtonAction]]
		ifFalse: [self color: oldColor].
! !

!CurveButton methodsFor: 'events' stamp: 'cr 3/20/98 01:31'!
mouseUp: evt

	self color: oldColor.
	(actWhen == #buttonUp and: [self containsPoint: evt cursorPoint])
		ifTrue: [self doButtonAction].
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 02:23'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change label' action: #setLabel.
	aCustomMenu add: 'change action selector' action: #setActionSelector.
	aCustomMenu add: 'change arguments' action: #setArguments.
	aCustomMenu add: 'change when to act' action: #setActWhen.

	((self world rootMorphsAt: aHandMorph targetOffset) size > 1) ifTrue: [
		aCustomMenu add: 'set target' action: #setTarget:].
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 02:07'!
makeClosed
	"Not allowed."
	^self shouldNotImplement! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 02:07'!
makeOpen
	"Not allowed."
	^self shouldNotImplement! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 01:54'!
setActionSelector

	| newSel |
	newSel _ FillInTheBlank
		request:
'Please type the selector to be sent to
the target when this button is pressed'
		initialAnswer: actionSelector.
	newSel isEmpty ifFalse: [self actionSelector: newSel].
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 01:54'!
setActWhen
	actWhen _ (SelectionMenu selections: #(buttonDown buttonUp whilePressed))
		startUpWithCaption: 'Choose one of the following conditions'
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 01:54'!
setArguments

	| s newArgs newArgsArray |
	s _ WriteStream on: ''.
	arguments do: [:arg | arg printOn: s. s nextPutAll: '. '].
	newArgs _ FillInTheBlank
		request:
'Please type the arguments to be sent to the target
when this button is pressed separated by periods'
		initialAnswer: s contents.
	newArgs isEmpty ifFalse: [
		newArgsArray _ Compiler evaluate: '{', newArgs, '}' for: self logged: false.
		self arguments: newArgsArray].
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 01:54'!
setLabel

	| newLabel |
	newLabel _ FillInTheBlank
		request:
'Please a new label for this button'
		initialAnswer: self label.
	newLabel isEmpty ifFalse: [self label: newLabel].
! !

!CurveButton methodsFor: 'menu' stamp: 'cr 3/20/98 01:55'!
setTarget: evt

	| rootMorphs |
	rootMorphs _ self world rootMorphsAt: evt hand targetOffset.
	rootMorphs size > 1
		ifTrue: [target _ rootMorphs at: 2]
		ifFalse: [target _ nil. ^ self].
! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:24'!
actionSelector
	"Return value of instance variable 'actionSelector'"
	^actionSelector! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:46'!
actionSelector: aSymbolOrString

	(nil = aSymbolOrString or:
	 ['nil' = aSymbolOrString or:
	 [aSymbolOrString isEmpty]])
		ifTrue: [^ actionSelector _ nil].

	actionSelector _ aSymbolOrString asSymbol.
! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:46'!
actWhen: condition
	"Accepts symbols:  #buttonDown, #buttonUp, and #whilePressed"
	actWhen _ condition! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:24'!
arguments
	"Return value of instance variable 'arguments'"
	^arguments! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:46'!
arguments: aCollection

	arguments _ aCollection asArray copy.
! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:51'!
label

	| s |
	s _ ''.
	self allMorphsDo: [:m | (m isKindOf: StringMorph) ifTrue: [s _ m contents]].
	^ s! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/22/98 02:17'!
label: aString

	| oldLabel m |

	(oldLabel _ self findA: StringMorph)
		ifNotNil: [oldLabel delete].
	m _ StringMorph new contents: aString.
[	self extent: (m extent * 2) + (borderWidth + 6).].
[	m position: self center - (m extent // 2);
		extent: self extent * 0.8.].
	self addMorph: m.
	m lock.
	self centerLabel.
! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 01:17'!
label: aString font: aFont

	| oldLabel m |
	(oldLabel _ self findA: StringMorph)
		ifNotNil: [oldLabel delete].
	m _ StringMorph contents: aString font: aFont.
	self extent: (m width + 6) @ (m height + 6).
	m position: self center - (m extent // 2).
	self addMorph: m.
	m lock
! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:24'!
target
	"Return value of instance variable 'target'"
	^target! !

!CurveButton methodsFor: 'accessing' stamp: 'cr 3/20/98 00:24'!
target: newValue
	"Set value of instance variable 'target'."
	target := newValue! !

!CurveButton methodsFor: 'copying' stamp: 'cr 3/20/98 01:22'!
updateReferencesUsing: aDictionary
	"If the arguments array points at a morph we are copying, then point at the new copy.  And also copies the array, which is important!!"

	super updateReferencesUsing: aDictionary.
	arguments _ arguments collect:
		[:old | aDictionary at: old ifAbsent: [old]].
! !

!CurveButton methodsFor: 'private' stamp: 'cr 3/22/98 02:38'!
centerLabel
	"Reposition the label after the button has been adjusted."
	| m |

	(m _ self findA: StringMorph)
		ifNil: [^self].
	m position: bounds center - (m extent // 2).

! !

!CurveButton methodsFor: 'private' stamp: 'cr 3/22/98 01:46'!
computeBounds
	"Recenter the label, then pass this message up."
	self centerLabel.
	^super computeBounds.! !



