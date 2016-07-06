'From Squeak 2.4c of May 10, 1999 on 15 June 1999 at 11:42:51 pm'!"Change Set:		scriptCleanup-swDate:			15 June 1999Author:			Scott WallaceVarious improvements that make it much more straightforward to extend the scripting system to handle new kinds of morphs.  In each case, it is no longer necessary to edit a large method to add information for a new scriptable morph type.  Instead, the morph itself, or its class, is all that needs be changed.(1) Categories for Viewers:  Specific classes are no longer hard-coded into the list-preparation mechanism, but rather individual costumes instantiated on behalf of a player are asked to assist in building up the category list.(2) ScriptInfo:  when a new morph subclass wishes to have scripting commands unique to itself shown on its behalf in a Viewer, it now does this by reimplementing #scriptInfo in its class.(3) SlotInfo: when a new morph subclass wishes to have pseudo-slots uniques to itself shown on its behalf in a Viewer, it now does this by reimplementing #standardSlotInfo in its class.(4) Category contents: the determination of which elements (which slots, which scripts) belong to each category is now made by via a cached structure that is built up by polling for reimplementors of #categoryContributions whenever a call to reinitialize the structure is made. (5) Help strings: the dictionary of standard help strings is now also constructed by a similar mechanism, this time polling for implementors of #helpContributions.(6)  Combined two overlapping and competing mechanisms for maintaining a directory of help information."StandardScriptingSystem removeSelector: #scriptHelpOrNilFor:.StandardScriptingSystem removeSelector: #scriptHelp:.!Object subclass: #StandardScriptingSystem	instanceVariableNames: ''	classVariableNames: 'CategoryElementDictionary CustomPartsBin FormDictionary HelpStrings StandardPartsBin StandardScriptInfo StandardSlotInfo SystemSlotDictionary TypeColorDictionary '	poolDictionaries: ''	category: 'Morphic-Scripting'!!Morph methodsFor: 'e-toy support' stamp: 'sw 6/15/1999 09:33'!addCostumeSpecificCategoriesTo: aCategoryList	"If the receiver has further sripting categories particular to itself to add to aCategoryList, add them now"! !!BookMorph methodsFor: 'other' stamp: 'sw 6/15/1999 09:36'!addCostumeSpecificCategoriesTo: aCategoryList	aCategoryList addIfNotPresent: 'book navigation'! !!JoystickMorph methodsFor: 'other' stamp: 'sw 6/15/1999 09:35'!addCostumeSpecificCategoriesTo: aCategoryList	aCategoryList addIfNotPresent: 'joystick'! !!Morph class methodsFor: 'misc' stamp: 'sw 6/15/1999 14:35'!categoryContributions	"Answer a list of arrays which characterize the elements in various viewer categories for the etoy system.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change.	Each array returned has two elements.  The first is the category name, and the second is a an array of <elementType> <elementName> pairs, where <elementType is #slot or #script"	^ #(		('basic' ((slot x) (slot y) (slot heading) (slot colorUnder) (script forward:)				 (script turn:) (script beep:)))		('color & border'( (slot color) (slot colorUnder) (slot borderColor) (slot borderWidth)))		('geometry'  ((slot scaleFactor) (slot left) (slot right) (slot top) (slot bottom) (slot width) (slot height) (slot x) (slot y) (slot heading)))		('miscellaneous' ((script show) (script hide) (script wearCostumeOf:) (script startScript:) (script stopScript:) (script pauseScript:)))		('motion' ((slot x) (slot y) (slot heading) (script forward:) (script moveToward:) (script turn:) (script bounce:) (script wrap) (script followPath) (script goToRightOf:)))		('pen use' ((slot penColor) (slot penSize) (slot penDown)))		('tests' ((slot isOverColor) (slot isUnderMouse) (slot colorSees))))! !!Morph class methodsFor: 'misc' stamp: 'sw 6/15/1999 23:35'!helpContributions	"Answer a list of pairs of the form		<symbol> <help message> 	to contribute to the system help dictionary"	^ #(		(acceptScript:for:			'submit the contents of the given script editor as the code defining the given selector')		(actorState			'return the ActorState object for the receiver, creating it if necessary')		(addInstanceVariable			'start the interaction for adding a new instance variable to the receiver')		(addPlayerMenuItemsTo:hand:			'add player-specific menu items to the given menu, on behalf of the given hand.  At present, these are only commands relating to the turtle')		(addSlotNamedLike:withValue:			'add a slot with a unique name derived from the first parameter, giving it the second parameter as its initial value')		(addYesNoToHand			'Press here to tear off a  TEST/YES/NO unit which you can drop into your script')		(allScriptEditors			'answer a list off the extant ScriptEditors for the receiver')		(amount			'The amount of displacement')		(angle				'The angular displacement')		(anonymousScriptEditorFor:			'answer a new ScriptEditor object to serve as the place for scripting an anonymous (unnamed, unsaved) script for the receiver')		(assignDecrGetter:setter:amt:			'evaluate the decrement variant of assignment')		(assignGetter:setter:amt:			'evaluate the vanilla variant of assignment')		(assignIncrGetter:setter:amt:			'evalute the increment version of assignment')		(assignMultGetter:setter:amt:			'evaluate the multiplicative version of assignment')		(assureEventHandlerRepresentsStatus			'make certain that the event handler associated with my current costume is set up to conform to my current script-status')		(assureExternalName			'If I do not currently have an external name assigned, get one now')		(assureUniClass			'make certain that I am a member a uniclass (i.e. a unique subclass); if I am not, create one now and become me into an instance of it')		(availableCostumeNames			'answer a list of strings representing the names of all costumes currently available for me')		(availableCostumesForArrows			'answer a list of actual, instantiated costumes for me, which can be cycled through as the user hits a next-costume or previous-costume button in a viewer')		(beep:			'make the specified sound')		(borderColor			'The color of the object''s border')		(borderWidth			'The width of the object''s border')		(bottom			'My bottom edge, measured downward from the top edge of the world')		(bounce:			'If object strayed beyond the boundaries of its container, make it reflect back into it, making the specified noise while doing so.')		(chooseTrigger'When this script should run."normal" means "only when called".See "explain status alternatives" at leftfor more information.')		(clearTurtleTrails			'Clear all the pen trails in the interior.')		(color				'The object''s interior color')		(colorSees			'Whether a given color in the object is over another given color')		(colorUnder			'The color under the center of the object')		(cursor				'The index of the chosen element')		(deleteCard			'Delete the current card.')		(dismiss			'Click here to dismiss me')		(firstPage			'Go to first page of book')		(followPath				'Retrace the path the object has memorized, if any.')		(forward:			'Moves the object forward in the direction it is heading') 		(goto:			'Go to the specfied book page')		(goToNextCard			'Go to the next card')		(goToPreviousCard			'Go to the previous card.')		(goToRightOf:			'Align the object just to the right of any specified object.')		(heading			'Which direction the object is facing.  0 is straight up') 		(height				'The distance between the top and bottom edges of the object')		(hide			'Make the object so that it does not display and cannot handle input')		(initiatePainting				'Initiate painting of a new object in the standard playfield.')		(initiatePaintingIn:			'Initiate painting of a new object in the given place.')		(isOverColor			'Whether any part of this object is directly over the specified color')		(isUnderMouse			'Whether any part of this object is beneath the current mouse-cursor position')		(lastPage			'Go to the last page of the book.')		(left			'My left edge, measured from the left edge of the World')		(leftRight			'The horizontal displacement')		(liftAllPens			'Lift the pens on all the objects in my interior.')		(lowerAllPens			'Lower the pens on all the objects in my interior.')		(mouseX			'The x coordinate of the mouse pointer')		(mouseY			'The y coordinate of the mouse pointer')		(moveToward:			'Move in the direction of another object.')		(newCard			'Create a new card.')		(nextPage			'Go to next page.')		(objectNameInHalo			'Object''s name -- To change: click here; backspace over old name, type in new name; hit ENTER')		(offerScriptorMenu			'Press here to get a menu of options for this Scriptor')		(pauseScript:			'Make a running script become paused.')		(penDown			'Whether the object''s pen is down (true) or up (false)')		(penColor			'The color of the object''s pen')		(penSize				'The size of the object''s pen')		(previousPage			'Go to previous page')		(show			'If object was hidden, make it show itself again.')		(startScript:			'Make a script start running.')		(stopScript:			'Make a script stop running.')		(top			'My top edge, measured downward from the top edge of the world')		(right			'My right edge, measured from the left edge of the world')		(roundUpStrays			'Bring all out-of-container subparts back into view.')		(scaleFactor			'The amount by which the object is scaled')		(stopScript:			'make the specified script stop running')		(try			'Run this command once.')		(tryMe			'Click here to run this script once; hold button down to run repeatedly.')		(turn:							'Change the heading of the object by the specified amount')		(unhideHiddenObjects			'Unhide all hidden objects.')		(upDown			'The vertical displacement')		(userScript			'This is a script defined by you.  Click here to rename or delete it')		(userSlot			'This is an instance variable defined by you.  Click here to change its type')		(valueAtCursor			'The chosen element')		(wearCostumeOf:			'Wear the same kind of costume as the other object')		(width				'The distance between the left and right edges of the object')		(wrap			'If object has strayed beond the boundaries of its container, make it reappear from the opposite edge.')		(x			'The x coordinate, measured from the left of the container')		(y			'The y-coordinate, measured upward from the bottom of the container')		)! !!Morph class methodsFor: 'misc' stamp: 'sw 6/15/1999 12:49'!scriptInfo	"Answer a list of arrays which characterize etoy script commands understood by this kind of morph -- in addition to those already defined by superclasses.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change."	^ #((command beep: sound)		(command bounce: sound)		(command forward: number)		(command followPath)		(command goToRightOf: player)		(command hide)		(command makeNewDrawingIn: player)		(command moveToward: player)		(command pauseScript: string)		(command show)		(command startScript: string)		(command stopScript: string)		(command turn: number)		(command wearCostumeOf: player)		(command wrap))! !!Morph class methodsFor: 'misc' stamp: 'sw 6/15/1999 13:41'!standardSlotInfo	"Answer a list of arrays which characterize etoy slots borne by this kind of morph -- in addition to those already defined by superclasses.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change."	^ #((borderWidth 	number		readWrite	getBorderWidth		setBorderWidth:)		(borderColor		color		readWrite	getBorderColor		setBorderColor:)		(bottom 			number		readWrite	getBottom			setBottom:)		(color			color		readWrite	getColor				setColor:)		(colorSees		boolean		readOnly	dummy				unused)		(colorUnder		color		readOnly	getColorUnder		unused)		(heading		number		readWrite	getHeading			setHeading:)		(height 			number		readWrite	getHeight			setHeight:)		(isOverColor		boolean		readOnly	dummy				unused)		(isUnderMouse	boolean		readOnly	getIsUnderMouse		unused)		(left 			number		readWrite	getLeft				setLeft:)		(penDown		boolean		readWrite	getPenDown			setPenDown:)		(penColor		color		readWrite	getPenColor			setPenColor:)		(penSize 		number		readWrite	getPenSize			setPenSize:)		(right 			number		readWrite	getRight			setRight:)		(scaleFactor		number		readWrite	getScaleFactor		setScaleFactor:)		(top 			number		readWrite	getTop				setTop:)		(width 			number		readWrite	getWidth			setWidth:)		(x 				number		readWrite	getX					setX:)		(y				number		readWrite	getY				setY:))! !!BookMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 14:34'!categoryContributions	^ #(('book navigation' ((script nextPage) (script previousPage) (script firstPage) (script lastPage) (script goto:))))! !!BookMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 12:36'!scriptInfo	"Answer a list of arrays which characterize new etoy script commands understood by this kind of morph -- in addition to those already defined by superclasses.  This is used only for initializing an effectively global structure."	^ #((command goto: player)		(command nextPage)		(command previousPage)		(command firstPage)		(command lastPage))! !!JoystickMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 14:35'!categoryContributions	^ #(('joystick' ((slot amount) (slot angle) (slot leftRight) (slot upDown))))! !!JoystickMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 12:57'!standardSlotInfo	"Answer a list of arrays which characterize etoy slots borne by this kind of morph -- in addition to those already defined by superclasses.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change."	^ #((leftRight	number		readOnly	getLeftRight				unused)		(upDown	number		readOnly	getUpDown				unused)		(angle		number		readOnly	getAngle				unused)		(amount	number		readOnly	getAmount				unused))! !!PaintBoxMorph methodsFor: 'other' stamp: 'sw 6/15/1999 09:36'!addCostumeSpecificCategoriesTo: aCategoryList	aCategoryList addIfNotPresent: 'paintbox'! !!PasteUpMorph methodsFor: 'scripting' stamp: 'sw 6/15/1999 09:37'!addCostumeSpecificCategoriesTo: aCategoryList	#('pen trails' 'card/stack' 'playfield') do:		[:cat | aCategoryList addIfNotPresent: cat]! !!PasteUpMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 14:35'!categoryContributions	"Answer a list of arrays which characterize the elements in various viewer categories for the etoy system.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change.	Each array returned has two elements.  The first is the category name, and the second is a an array of <elementType> <elementName> pairs, where <elementType is #slot or #script"	^ #(		('playfield' ((script initiatePainting) (slot cursor) (slot valueAtCursor) (slot mouseX) (slot mouseY)(script roundUpStrays) (script unhideHiddenObjects)))		('card/stack' ((script goToNextCard) (script goToPreviousCard) (script deleteCard) (script newCard)))		('pen trails' ((script liftAllPens) (script lowerAllPens) (script clearTurtleTrails))))! !!PasteUpMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 12:36'!scriptInfo	"Answer a list of arrays which characterize new etoy script commands understood by this kind of morph -- in addition to those already defined by superclasses.  This is used only for initializing an effectively global structure."	^ #((command clearTurtleTrails)		(command deleteCard)		(command goToNextCard)		(command goToPreviousCard)		(command initiatePainting)		(command liftAllPens)		(command lowerAllPens)		(command newCard)		(command roundUpStrays)		(command unhideHiddenObjects))! !!PasteUpMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 13:41'!standardSlotInfo	"Answer a list of arrays which characterize etoy slots borne by this kind of morph -- in addition to those already defined by superclasses.  Implementors of this method are statically polled to contribute this information when the scripting system reinitializes its scripting info, which typically only happens after a structural change."	^ #((mouseX			number		readOnly	getMouseX			unused)		(mouseY		number		readOnly	getMouseY			unused)		(cursor 			number		readWrite	getCursor			setCursor:)		(valueAtCursor	player		readOnly	getValueAtCursor	unused))! !!Player methodsFor: 'slots-kernel' stamp: 'sw 6/15/1999 09:42'!categories	"Answer a list of categories appropriate to the the receiver and its costumes"	| aList |	(self hasCostumeThatIsAWorld)		ifTrue:	[^ self categoriesForWorld].	aList _ #('basic' ) asOrderedCollection.	self slotNames size > 0 ifTrue:		[aList add: 'instance variables'].	self class scripts size > 0 ifTrue:		[aList add: 'scripts'].	aList addAll: #('tests' 'color & border' 'geometry' 'motion' 'pen use' 'miscellaneous' ).	self costumesDo:		[:aCostume | aCostume addCostumeSpecificCategoriesTo: aList].	^ aList! !!Player methodsFor: 'slots-kernel' stamp: 'sw 6/15/1999 15:08'!tilePhrasesSpecsForCategory: aCategory	"Return an array of slot and script names and info for use in a viewer on the receiver.  These can be of two flavors - script and slot.		(slot		heading		number				readWrite	getHeading		setHeading:)		(script		command 	wearCostumeOf: 	player)"	| aList nameString categoryString |	categoryString _ aCategory asString.	(categoryString = 'instance variables') ifTrue:		[^ self slotNames collect: [:aName |		nameString _ aName asString capitalized.		Array			with:	#slot			with: 	aName 								"name"			with: 	(self typeForSlot: aName asSymbol)	"type"			with:	#readWrite							"r/w"			with:	('get', nameString) asSymbol		"get selector"			with:	('set', nameString, ':') asSymbol]].	"set selector"	(categoryString = 'scripts') ifTrue:		[^ self tileScriptCommands].	self hasCostumeThatIsAWorld ifTrue: [^ self worldTilePhrasesSpecsForCategory: aCategory].	aList _ ScriptingSystem categoryElementsFor: categoryString.	aList ifNil: [self error: 'oops, missing category info for ', categoryString].	^ aList collect: [:aPair | self phraseSpecFor: aPair]! !!Player methodsFor: 'slots-kernel' stamp: 'sw 6/15/1999 14:49'!worldTilePhrasesSpecsForCategory: aCategory	"Return an array of slot and script names and info for use in a viewer on the receiver, in the situation where the receiver's costume is the World.   Categories 'instance variables' and 'scripts'  will already have been taken care of and need not be dealt with here."	| aList categoryString |	categoryString _ aCategory asString.	aList _ #().	(categoryString = 'basic') ifTrue:		[aList _ #((script beep:))].	(categoryString = 'color & border') ifTrue:		[aList _ #((slot color))].	(categoryString = 'miscellaneous') ifTrue:		[aList _ #((script startScript:) (script stopScript:) (script pauseScript:))].	(categoryString = 'pen trails') ifTrue:		[aList _ #((script liftAllPens) (script lowerAllPens) (script clearTurtleTrails))].	(categoryString = 'playfield') ifTrue:		[aList _ #((script initiatePainting) (slot cursor) (slot valueAtCursor) (slot mouseX) (slot mouseY)(script roundUpStrays) (script unhideHiddenObjects))].	^ aList collect: [:aPair | self phraseSpecFor: aPair]! !!ReferenceMorph methodsFor: 'misc' stamp: 'sw 6/15/1999 09:40'!addCostumeSpecificCategoriesTo: aCategoryList	(self referent isKindOf: PaintBoxMorph)			ifTrue:	[aCategoryList addIfNotPresent: 'paintbox']! !!ReferenceMorph class methodsFor: 'as yet unclassified' stamp: 'sw 6/15/1999 14:25'!categoryContributions	^ #(('paintbox' ((script makeNewDrawingIn:))))! !!StandardScriptingSystem methodsFor: 'help dictionary' stamp: 'sw 6/15/1999 17:03'!initializeHelpStrings	"Initialize the data structure that determines, for the etoy system, help messages for various scripting elements.  The structure is built up by letting every Morph subclass contribute elements simply by implementing method #helpContributions.  Consult implementors of #helpContributions for examples of how this goes."	"ScriptingSystem initializeHelpStrings"	| aDictionary |	aDictionary _ IdentityDictionary new.  	"For safety, the new copy is built up in this temp first, so that if an error occurs during the creation of the structure, the old version will remain remain in place"	Morph withAllSubclasses do:		[:aClass | (aClass class selectors includes: #helpContributions)			ifTrue:				[aClass helpContributions do:					[:pair | aDictionary at: pair first put: pair second]]].		HelpStrings _ aDictionary! !!StandardScriptingSystem methodsFor: 'help dictionary' stamp: 'sw 6/15/1999 16:15'!initializeSystemSlotDictionary	"ScriptingSystem initializeSystemSlotDictionary"	SystemSlotDictionary _ IdentityDictionary new.	StandardSlotInfo keysDo:		[:aKey |			SystemSlotDictionary at: aKey put: (StandardSlotInfo at: aKey) second]! !!StandardScriptingSystem methodsFor: 'universal slots & scripts' stamp: 'sw 6/15/1999 14:52'!categoryElementsFor: aCategoryName	^ CategoryElementDictionary at: aCategoryName ifAbsent: [nil]! !!StandardScriptingSystem methodsFor: 'universal slots & scripts' stamp: 'sw 6/15/1999 14:40'!initCategoryElementDictionary	"Initialize the data structure that characterizes, for the etoy system, which elements fall into which categories in the Viewer.The structure is built up by letting every Morph subclass contribute elements simply by implementing method #categoryContributions.  Consult implementors of #categoryContributions for examples of how this goes."	"ScriptingSystem initCategoryElementDictionary"	| aDictionary |	aDictionary _ Dictionary new.  	"For safety, the new copy is built up in this temp first, so that if an error occurs during the creation of the structure, the old version will remain remain in place"	Morph withAllSubclasses do:		[:aClass | (aClass class selectors includes: #categoryContributions)			ifTrue:				[aClass categoryContributions do:					[:pair | aDictionary at: pair first put: pair second]]].	CategoryElementDictionary _ aDictionary! !!StandardScriptingSystem methodsFor: 'universal slots & scripts' stamp: 'sw 6/15/1999 12:44'!initStandardScriptInfo	"Initialize the data structure that characterizes, for the etoy system, the built-in system scripts.  The structure is a dictionary whose keys are the script selectors and whose values are arrays of one of the forms:              (command   <selector>)              (command   <selector>  <argType>)The structure is built up by letting every Morph subclass contribute elements simply by implementing method #scriptInfo.  Consult implementors of #scriptInfo for examples of how this goes."	"ScriptingSystem initStandardScriptInfo"	| aDictionary |	aDictionary _ Dictionary new.  	"For safety, the new copy is built up in this temp first, so that if an error occurs during the creation of the structure, the old version will remain remain in place"	Morph withAllSubclasses do:		[:aClass | (aClass class selectors includes: #scriptInfo)			ifTrue:				[aClass scriptInfo do:					[:anArray | aDictionary at: anArray second put: anArray]]].	StandardScriptInfo _ aDictionary! !!StandardScriptingSystem methodsFor: 'universal slots & scripts' stamp: 'sw 6/15/1999 15:26'!initStandardSlotInfo	"Initialize the data structure that characterizes, for the etoy system, the built-in pseudoslots.  The structure is a dictionary whose keys are the slot names and whose values are arrays of quintuplets exemplified by:	(borderColor		color		readWrite	getBorderColor	setBorderColor:)The structure is built up by letting every Morph subclass contribute elements simply by implementing method #standardSlotInfo.  Consult implementors of #standardSlotInfo for examples of how this goes."	"ScriptingSystem initStandardSlotInfo"	| aDictionary |	aDictionary _ Dictionary new.  	"For safety, the new copy is built up in this temp first, so that if an error occurs during the creation of the structure, the old version will remain remain in place"	Morph withAllSubclasses do:		[:aClass | (aClass class selectors includes: #standardSlotInfo)			ifTrue:				[aClass standardSlotInfo do:					[:anArray | aDictionary at: anArray first put: anArray]]].	StandardSlotInfo _ aDictionary! !!StandardScriptingSystem class methodsFor: 'class initialization' stamp: 'sw 6/15/1999 16:22'!initialize	"StandardScriptingSystem initialize"	"Sometimes this method is vacuously changed just to get it in a changeset so that its invocation will occur as part of an update"	(Smalltalk at: #ScriptingSystem ifAbsent: [nil]) ifNil:		[Smalltalk at: #ScriptingSystem put: self new].	ScriptingSystem		initStandardSlotInfo;		initCategoryElementDictionary;		initStandardScriptInfo;		initializeSystemSlotDictionary;		initializeHelpStrings! !!String class methodsFor: 'instance creation' stamp: 'sw 6/15/1999 22:59'!tab	"Answer a string containing a single tab character."	^ self with: Character tab! !!ViewerEntry methodsFor: 'contents' stamp: 'sw 6/15/1999 23:23'!contents	| aType  newText sel info |	(info _ self slotDocumentation) ifNotNil:  "handles system and user slots"		[^ info].	((aType _ self entryType) == #userScript or: [aType == #systemScript]) ifTrue:		[sel _ self viewerRow elementSymbol.		newText _ (Preferences showScriptSource or: [self playerBearingCode class tileScriptNames includes: sel])			ifTrue:				[self playerBearingCode sourceCodeFor: sel]			ifFalse:				[ScriptingSystem helpStringFor: sel]].	^ newText ifNil: ['?????']! !StandardScriptingSystem removeSelector: #runningPlayfieldBorderColor!StandardScriptingSystem removeSelector: #scriptHelpFor:!StandardScriptingSystem removeSelector: #fixUniclassAccessors!StandardScriptingSystem removeSelector: #setHelpStringsForSystemScripts!StandardScriptingSystem initialize!