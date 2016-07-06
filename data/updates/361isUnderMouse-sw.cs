'From Squeak 2.2 of Sept 23, 1998 on 13 October 1998 at 12:38:25 pm'!"Change Set:		isUnderMouse-swDate:			13 October 1998Author:			Scott WallaceAdds an #isUnderMouse slot to the Viewer, for use in scripting.  isUnderMouse returns true if any part of the player's current costume is under the current mouse location, whether the object is occluded at that point or not.  The implementation is 'flex-aware'.Also, moves initialization of the scripting help-dictionary to the instance side of StandardScriptingSystem, remedying an anomaly attributable to historical accident."!!Player methodsFor: 'slots-kernel' stamp: 'sw 10/7/1998 09:12'!standardSlotsForBank: aBank	"Return an array of slot names and slot info for use in a viewer on the receiver""		name		type		r/w			get selector			put selector		-----------	---------		-----------	---------------------	-------------   "	(self costume isKindOf: WorldMorph) ifTrue: [^ self standardWorldSlotsForBank: aBank].	aBank = 1 ifTrue: [^ #(		(heading	number		readWrite	getHeading			setHeading:)		(x 			number		readWrite	getX					setX:)		(y			number		readWrite	getY				setY:)		(colorUnder	color		readOnly	getColorUnder		unused))].	aBank = 3 ifTrue: [^ #(		(penDown	boolean		readWrite	getPenDown			setPenDown:)		(penColor	color		readWrite	getPenColor			setPenColor:)		(penSize 	number		readWrite	getPenSize			setPenSize:))].	aBank = 4 ifTrue: [^ #(		(isOverColor	boolean		readOnly	dummy				unused)		(colorSees	boolean		readOnly	dummy				unused)		(isUnderMouse					boolean		readOnly	getIsUnderMouse		unused)		(scaleFactor	number		readWrite	getScaleFactor		setScaleFactor:)		(width 		number		readWrite	getWidth			setWidth:)		(height 		number		readWrite	getHeight			setHeight:)		(left 		number		readWrite	getLeft				setLeft:)		(right 		number		readWrite	getRight			setRight:)		(top 		number		readWrite	getTop				setTop:)		(bottom 		number		readWrite	getBottom			setBottom:)		)].	^ #()! !!Player methodsFor: 'slots-standard-get/set' stamp: 'sw 10/7/1998 09:31'!getIsUnderMouse	costume isInWorld ifFalse: [^ false].	^ costume containsPoint: (costume pointFromWorld: costume primaryHand lastEvent cursorPoint)! !!StandardScriptingSystem methodsFor: 'help dictionary' stamp: 'sw 10/7/1998 09:49'!initializeHelpStrings	"ScriptingSystem initializeHelpStrings"	HelpStrings _ IdentityDictionary new.	#((heading		number		'Which direction the object isfacing.  0 is straight up') (x				number		'The x coordinate, measured from the left of the container')(y				number'The y-coordinate, measured upwardfrom the bottom of the container')(colorUnder		color'The color under thecenter of the object')(penDown		boolean'Whether the object''s penis down (true) or up (false)')(penColor		color'The color of the object''s pen')(penSize			number'The size of the object''s pen')(colorSees		boolean'Whether a given color in theobject is over another given color')(isOverColor		boolean'Whether any part of thisobject is directly over thespecified color')(isUnderMouse	boolean'Whether any part of thisobject is beneath the currentmouse-cursor position')(scaleFactor		number'The amount by whichthe object is scaled')(width			number'The distance between theleft and right edges of the object')(height			number'The distance between thetop and bottom edges of the object')(isOverColor		color'Whether the object isover the given color')(color			color'The object''s interior color')(borderWidth	number'The width of the object''s border')(borderColor		color'The color of the object''s border')(cursor			number'The index of the chosen element')(valueAtCursor	player'The chosen element')(leftRight		number'The horizontal displacement')(upDown		number'The vertical displacement')(angle			number'The angular displacement')(amount		number'The amount of displacement')(mouseX		number'The x coordinate ofthe mouse pointer')(mouseY		number'The y coordinate ofthe mouse pointer')(left		number'My left edge, measured fromthe left edge of the World')(right		number'My right edge, measured fromthe left edge of the world')(top		number'My top edge, measured downwardfrom the top edge of the world')(bottom		number'My bottom edge, measured downwardfrom the top edge of the world')(tryMe			command'Click here to run this script once;hold button down to run repeatedly.')(try			command'Click here to run this command once,with parameters as seen right here.Hold button down to run repeatedly')(dismiss			command'Click here to dismiss me')(addYesNoToHand	command'Press here to tear off a TEST/YES/NO unit whichyou can drop into your script')(chooseTrigger	command'Press here to choose whenthis script should be run')(offerScriptorMenu	command'Press here to get a menu ofoptions for this Scriptor')(objectNameInHalo  control'Object''s name -- To change:click here; backspace over old name,type in new name; hit ENTER')(userSlot		control'This is an instance variabledefined by you.  Click here tochange its type')		) do: [:triplet | HelpStrings at: triplet first put: triplet third]! !!StandardScriptingSystem methodsFor: 'help dictionary' stamp: 'sw 10/7/1998 09:32'!initializeSystemSlotDictionary	"ScriptingSystem initializeSystemSlotDictionary"	SystemSlotDictionary _ IdentityDictionary new.	#(		(heading		number)		(x				number)		(y				number)		(colorUnder		color)		(penDown		boolean)		(penColor		color)		(penSize			number)		(colorSees		boolean)		(seesColor		boolean)		(isUnderMouse	boolean)		(scaleFactor		number)		(width			number)		(height			number)		(isOverColor		color)		(color			color)		(borderWidth	number)		(borderColor		color)		(cursor			number)		(valueAtCursor	player)		(leftRight		number)		(upDown		number)		(angle			number)		(amount		number)		(left			number)		(right			number)		(top				number)		(bottom			number)		(mouseX			number)		(mouseY		number)) do:	[:pair | SystemSlotDictionary at: pair first put: pair second]! !!StandardScriptingSystem class methodsFor: 'class initialization' stamp: 'sw 10/13/1998 12:32'!initialize	"StandardScriptingSystem initialize"	"Sometimes this method is vacuously changed just to get it in a changeset so that its invocation will occur as part of an update"	Smalltalk at: #ScriptingSystem put: (self new initializeSystemSlotDictionary; initializeHelpStrings; yourself)! !StandardScriptingSystem class removeSelector: #initializeSystemSlotDictionary!StandardScriptingSystem initialize!