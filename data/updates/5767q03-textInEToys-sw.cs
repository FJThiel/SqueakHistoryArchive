'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:52 pm'!
"Change Set:		q03-textInEtoys-sw
Date:			22 May 2003
Author:			Scott Wallace

Some extensions affording etoy access to text-handling functions.
Also, some extensions to and rewordings of functions relating to 'telling.'

Adapted from update 0170textInEToys-sw of Squeakland; integrated with Squeak 3.7a by Scott Wallace on 3/3/04."!


!Morph methodsFor: 'e-toy support' stamp: 'sw 2/18/2003 02:54'!
getCharacters
	"obtain a string value from the receiver.  The default generic response is simply the name of the object."

	^ self externalName! !


!Morph class methodsFor: 'scripting' stamp: 'sw 2/19/2003 18:04'!
additionsToViewerCategoryScripting
	"Answer viewer additions for the 'scripting' category"

	^#(
		scripting 
		(

			(command startScript: 'start the given script ticking' ScriptName)
			(command pauseScript: 'make the given script be "paused"' ScriptName)
			(command stopScript: 'make the given script be "normal"' ScriptName)

			(command startAll: 'start the given script ticking in the object and all of its siblings.' ScriptName)
			(command pauseAll: 'make the given script be "paused" in the object and all of its siblings' ScriptName)
			(command stopAll: 'make the given script be "normal" in the object and all of its siblings' ScriptName)

			(command doScript: 'run the given script once, on the next tick' ScriptName)
			(command tellSelfAndAllSiblings: 'run the given script in the object and in all of its siblings' ScriptName)
			(command tellAllSiblings: 'send a message to all siblings' ScriptName)))! !


!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 3/3/2004 17:52'!
addPlayfieldMenuItems: menu hand: aHandMorph
	"Add playfield-related items to the menu"

	menu add: 'playfield options...' translated target: self action: #presentPlayfieldMenu.
	(self hasProperty: #donorTextMorph) ifTrue:
		[menu add: 'send contents back to donor' translated action: #sendTextContentsBackToDonor]! !

!PasteUpMorph methodsFor: 'menu & halo' stamp: 'sw 2/18/2003 03:08'!
sendTextContentsBackToDonor
	"Send my string contents back to the Text Morph from whence I came"

	(self valueOfProperty: #donorTextMorph) ifNotNilDo:
		[:aDonor | aDonor setCharacters: self assuredPlayer getStringContents]! !

!PasteUpMorph methodsFor: 'scripting' stamp: 'sw 2/18/2003 02:56'!
elementCount
	"Answer how many objects are contained within me"

	^ submorphs size! !

!PasteUpMorph methodsFor: 'scripting' stamp: 'sw 2/18/2003 01:46'!
getCharacters
	"obtain a string value from the receiver"

	^ String streamContents:
		[:aStream |
			submorphs do:
				[:m | aStream nextPutAll: m getCharacters]]! !

!PasteUpMorph methodsFor: 'scripting' stamp: 'sw 2/20/2003 13:06'!
tellAllContents: aMessageSelector
	"Send the given message selector to all the objects within the receiver"

	self submorphs do:
		[:m |
			m player ifNotNilDo:
				[:p | p performScriptIfCan: aMessageSelector]]! !


!PasteUpMorph class methodsFor: 'scripting' stamp: 'sw 3/4/2004 10:09'!
additionsToViewerCategories
	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."

	^ # (

(playfield (
(command initiatePainting 'Initiate painting of a new object in the standard playfield.')
(slot mouseX 'The x coordinate of the mouse pointer' Number readWrite Player getMouseX  unused unused)
(slot mouseY 'The y coordinate of the mouse pointer' Number readWrite Player getMouseY  unused unused)
(command roundUpStrays 'Bring all out-of-container subparts back into view.')
(slot graphic 'The graphic shown in the background of this object' Graphic readWrite Player getGraphic Player setGraphic:)
(command unhideHiddenObjects 'Unhide all hidden objects.')))

(scripting (
(command tellAllContents: 'Send a message to all the objects inside the playfield' ScriptName)))

(collections (
(slot cursor 'The index of the chosen element' Number readWrite Player getCursor Player setCursorWrapped:)
(slot count 'How many elements are within me' Number readOnly Player getCount unused unused)
(slot stringContents 'The characters of the objects inside me, laid end to end' String readOnly Player getStringContents unused unused)
(slot playerAtCursor 'the object currently at the cursor' Player readWrite Player getValueAtCursor  unused unused)
(slot firstElement  'The first object in my contents' Player  readWrite Player getFirstElement  Player  setFirstElement:)
(slot numberAtCursor 'the number at the cursor' Number readWrite Player getNumberAtCursor Player setNumberAtCursor: )
(slot graphicAtCursor 'the graphic worn by the object at the cursor' Graphic readOnly Player getGraphicAtCursor  unused unused)
(command tellAllContents: 'Send a message to all the objects inside the playfield' ScriptName)
(command removeAll 'Remove all elements from the playfield')
(command shuffleContents 'Shuffle the contents of the playfield')
(command append: 'Add the object to the end of my contents list.' Player)
(command prepend: 'Add the object at the beginning of my contents list.' Player)
(command include: 'Add the object to my contents' Player)
))

(#'stack navigation' (
(command goToNextCardInStack 'Go to the next card')
(command goToPreviousCardInStack  'Go to the previous card')
(command goToFirstCardInBackground 'Go to the first card of the current background')
(command goToFirstCardOfStack 'Go to the first card of the entire stack')
(command goToLastCardInBackground 'Go to the last card of the current background')
(command goToLastCardOfStack 'Go to the last card of the entire stack')
(command deleteCard 'Delete the current card')
(command insertCard 'Create a new card')))

"(viewing (
(slot viewingNormally 'whether contents are viewed normally' Boolean readWrite Player getViewingByIcon Player setViewingByIcon: )))"

(#'pen trails' (
(command liftAllPens 'Lift the pens on all the objects in my interior.')
(command lowerAllPens  'Lower the pens on all the objects in my interior.')
(command trailStyleForAllPens:  'Set the trail style for pens of all objects within' TrailStyle)
(command clearTurtleTrails 'Clear all the pen trails in the interior.'))))
! !


!Player methodsFor: 'scripts-standard' stamp: 'sw 2/18/2003 02:57'!
insertCharacters: aString
	"Insert the given characters at my current cursor position"

	self costume renderedMorph insertCharacters: aString! !

!Player methodsFor: 'scripts-standard' stamp: 'sw 2/18/2003 02:57'!
insertContentsOf: aPlayer
	"Insert the string contents of the given player at my given cursor position"

	self costume renderedMorph insertContentsOf: aPlayer! !

!Player methodsFor: 'scripts-standard' stamp: 'sw 2/20/2003 13:05'!
performScriptIfCan: scriptNameString
	"If I understand the given script name, perform it now"

	Symbol hasInterned: scriptNameString ifTrue:
		[:sym | (self class includesSelector: sym) ifTrue:
			[self perform: sym]]! !

!Player methodsFor: 'scripts-standard' stamp: 'sw 2/20/2003 13:14'!
tellAllContents: aMessageSelector
	"Send the given message selector to all the content players within the receiver's morph"

	costume renderedMorph tellAllContents: aMessageSelector! !

!Player methodsFor: 'scripts-standard' stamp: 'sw 2/19/2003 18:19'!
tellSelfAndAllSiblings: aMessageSelector
	"Send the given message selector to myself and to all my sibling instances"

	self belongsToUniClass ifTrue:
		[self class allSubInstances do:
			[:anInstance | anInstance perform: aMessageSelector asSymbol]]! !

!Player methodsFor: 'slot getters/setters' stamp: 'sw 2/17/2003 18:09'!
getCharacterAtCursor
	"Answer the value of the text cursor"

	| aLoc aTextMorph aString |
	aLoc _ (aTextMorph _ self costume renderedMorph) cursor.
	aString _ aTextMorph text string.
	^ (aString at: aLoc ifAbsent: ['¥']) asString! !

!Player methodsFor: 'slot getters/setters' stamp: 'sw 2/17/2003 11:46'!
getCount
	"Answer the number of elements"

	^ self costume renderedMorph elementCount! !

!Player methodsFor: 'slot getters/setters' stamp: 'sw 2/18/2003 02:20'!
getStringContents
	"Answer the String contents"

	^ self costume renderedMorph getCharacters! !

!Player methodsFor: 'slot getters/setters' stamp: 'sw 2/17/2003 17:35'!
setCharacterAtCursor: aCharOrString
	"Insert the given character at my cursor position"

	| aLoc aTextMorph aString charToUse |
	aLoc _ (aTextMorph _ self costume renderedMorph) cursor.
	charToUse _ (aString _ aCharOrString asString) size > 0
		ifTrue:
			[aString first]
		ifFalse:
			['¥'].
	aTextMorph paragraph replaceFrom: aLoc to: aLoc with: charToUse asString asText displaying: true.
	aTextMorph updateFromParagraph  ! !


!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 2/19/2003 18:11'!
wordingForOperator: aString
	"Answer the wording to be seen by the user for the given operator symbol/string"

	| toTest |
	toTest _ aString asString.
	#(	(append:				'include at end')
		(beep:					'make sound')
		(bounce:				'bounce')
		(clearTurtleTrails		'clear pen trails')
		(clearOwnersPenTrails	'clear all pen trails')
		(colorSees				'color  sees')
		(color:sees:				'color sees')
		(doMenuItem:			'do menu item')
		(doScript:				'do')
		(forward:				'forward by')
		(moveToward:			'move toward')
		(goToRightOf:			'align after')
		(isDivisibleBy:			'is divisible by')
		(liftAllPens				'lift all pens')
		(lowerAllPens			'lower all pens')
		(arrowheadsOnAllPens	'arrowheads on all pens')
		(noArrowheadsOnAllPens	'no arrowheads on pens')
		(pauseAll:				'pause all')
		(pauseScript:			'pause script')
		(max:					'max')
		(min:					'min')
		(seesColor:				'is over color')
		(makeNewDrawingIn:	'start painting in')
		(prepend:				'include at beginning')
		(startAll:				'start all')
		(startScript:				'start script')
		(stopProgramatically	'stop')
		(stopAll:					'stop all')
		(stopScript:				'stop script')
		(tellAllSiblings:			'tell all siblings')
		(tellSelfAndAllSiblings:	'send to all')
		(turn:					'turn by')
		(wearCostumeOf:		'look like'))

	do:
		[:pair | toTest = pair first ifTrue: [^ pair second]].

	^ toTest

	"StandardScriptingSystem initialize"

! !


!StringMorph methodsFor: 'accessing' stamp: 'sw 2/18/2003 02:55'!
getCharacters
	"obtain a string value from the receiver."

	^ self contents! !


!TextMorph methodsFor: 'accessing' stamp: 'sw 5/22/2003 02:39'!
cursor
	"Answer the receiver's logical cursor position"

	| loc |
	loc _ self valueOfProperty: #textCursorLocation  ifAbsentPut: [1].
	loc _ loc min: text string size.
	^ loc rounded
	! !

!TextMorph methodsFor: 'accessing' stamp: 'sw 2/17/2003 18:20'!
cursorWrapped: aNumber
	"Set the cursor as indicated"

	self setProperty: #textCursorLocation toValue: (((aNumber rounded - 1) \\  text string size) + 1)

	! !

!TextMorph methodsFor: 'accessing' stamp: 'sw 2/18/2003 02:58'!
elementCount
	"Answer how many sub-objects are within me"

	^ self text string size ! !

!TextMorph methodsFor: 'menu' stamp: 'sw 3/3/2004 17:56'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	"Add text-related menu items to the menu"

	| outer |
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'text color...' translated action: #changeTextColor.
	aCustomMenu addUpdating: #autoFitString target: self action: #autoFitOnOff.
	aCustomMenu addUpdating: #wrapString target: self action: #wrapOnOff.
	aCustomMenu add: 'text margins...' translated action: #changeMargins:.
	aCustomMenu add: 'add predecessor' translated action: #addPredecessor:.
	aCustomMenu add: 'add successor' translated action: #addSuccessor:.
	(Preferences noviceMode or: [Preferences simpleMenus])
		ifFalse:
			[aCustomMenu add: 'code pane menu...' translated action: #yellowButtonActivity.
			aCustomMenu add: 'code pane shift menu....' translated action: #shiftedYellowButtonActivity].

	outer _ self owner.
	((outer isKindOf: PolygonMorph) and: [outer isOpen])
		ifTrue:
			[container isNil
				ifTrue: [aCustomMenu add: 'follow owner''s curve' translated action: #followCurve]
				ifFalse: [aCustomMenu add: 'reverse direction' translated action: #reverseCurveDirection.
						aCustomMenu add: 'set baseline' translated action: #setCurveBaseline:]]
		ifFalse:
			[(container isNil or: [container fillsOwner not])
				ifTrue: [aCustomMenu add: 'fill owner''s shape' translated action: #fillingOnOff]
				ifFalse: [aCustomMenu add: 'rectangluar bounds' translated action: #fillingOnOff].
			(container isNil or: [container avoidsOcclusions not])
				ifTrue: [aCustomMenu add: 'avoid occlusions' translated action: #occlusionsOnOff]
				ifFalse: [aCustomMenu add: 'ignore occlusions' translated action: #occlusionsOnOff]].
		
	aCustomMenu addLine.
	aCustomMenu add: 'holder for characters' translated action: #holderForCharacters

! !

!TextMorph methodsFor: 'menu' stamp: 'sw 2/18/2003 03:20'!
holderForCharacters
	"Hand the user a Holder that is populated with individual text morphs representing my characters"

	| aHolder |
	aHolder _ ScriptingSystem prototypicalHolder.
	aHolder setNameTo: 'H', self externalName.
	text string do:
		[:aChar |
			aHolder addMorphBack: (TextMorph new contents: aChar asText)].
	aHolder setProperty: #donorTextMorph toValue: self.
	aHolder fullBounds.
	aHolder openInHand! !

!TextMorph methodsFor: 'scripting access' stamp: 'sw 2/18/2003 02:45'!
insertCharacters: aSource
	"Insert the characters from the given source at my current cursor position"

	| aLoc |
	aLoc _ self cursor.
	paragraph replaceFrom: aLoc to: (aLoc - 1) with: aSource asText displaying: true.
	self updateFromParagraph  ! !

!TextMorph methodsFor: 'scripting access' stamp: 'sw 2/18/2003 02:46'!
insertContentsOf: aPlayer
	"Insert the characters from the given player at my current cursor position"

	| aLoc |
	aLoc _ self cursor.
	paragraph replaceFrom: aLoc to: (aLoc - 1) with: aPlayer getStringContents displaying: true.
	self updateFromParagraph  ! !


!TextMorph class methodsFor: 'scripting' stamp: 'sw 2/18/2003 03:21'!
additionsToViewerCategories
	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."

	^ #(
(text (
(slot characters	'The characters in my contents' String	readWrite Player getCharacters Player setCharacters:)

(slot cursor 'The position among my characters that replacement text would go' Number readWrite Player getCursor Player setCursor:)
(slot characterAtCursor 'The character at the my cursor position' String readWrite Player getCharacterAtCursor Player setCharacterAtCursor:)
(slot count 'How many characters I have' Number readOnly Player getCount unused unused)

(slot firstCharacter  'The first character in my contents' String  readWrite Player getFirstCharacter  Player  setFirstCharacter:)
(slot allButFirst 'All my characters except the first one' String readWrite Player getAllButFirstCharacter Player  setAllButFirstCharacter:)
(command insertCharacters: 'insert the given string at my cursor position' String)
(command insertContentsOf: 'insert the characters from another object at my cursor position' Player)
(slot numericValue 'The number represented by my contents' Number readWrite Player getNumericValue Player  setNumericValue:)))

(basic (
(slot characters	'The characters in my contents' String	readWrite Player getCharacters Player setCharacters:))))


! !

