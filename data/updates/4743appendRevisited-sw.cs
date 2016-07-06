'From Squeak3.3alpha of 12 January 2002 [latest update: #4742] on 4 February 2002 at 1:32:07 am'!"Change Set:		appendRevisited-swDate:			3 February 2002Author:			Scott WallaceSome adjustments to the choices of which 'append' variants are available in etoy viewers in which situations.  All users will see 'include', which is generic; the other two choices, 'append' and 'prepend', are shown as well when *not* in eToyFriendly mode.  The balloon help for these commands is improved."!!EToyVocabulary methodsFor: 'method list' stamp: 'sw 12/12/2001 11:57'!masterOrderingOfPhraseSymbols	"Answer a dictatorially-imposed presentation list of phrase-symbols.  This governs the order in which suitable phrases are presented in etoy viewers using the etoy vocabulary.  For any given category, the default implementation is that any items that are in this list will occur first, in the order specified here; after that, all other items will come, in alphabetic order by formal selector."	^ #(beep: forward: turn: getX getY  getHeading		startScript: pauseScript: stopScript: startAll: pauseAll: stopAll: tellAllSiblings: doScript:		getColor getUseGradientFill getSecondColor  getRadialGradientFill  getBorderWidth getBorderColor getBorderStyle getRoundedCorners getDropShadow getShadowColor 		getGraphic getBaseGraphic)! !!EToyVocabulary methodsFor: 'method list' stamp: 'sw 2/3/2002 23:26'!phraseSymbolsToSuppress	"Answer a dictatorially-imposed list of phrase-symbols that are to be suppressed from viewers when the eToyFriendly preference is set to true.  This list at the moment corresponds to the wishes of Alan and Kim and the LA teachers using Squeak in school-year 2001-2"	^ Preferences eToyFriendly		ifTrue:			[#(moveToward: followPath goToRightOf:				getViewingByIcon initiatePainting				append: prepend:)]		ifFalse:			[#()]! !!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 2/3/2002 22:58'!templateForLanguageTranslation	"Edit this method such that the second element of each triplet has the translated wording and the third element has the translated help-message; give the edited method a name of the form #addLangVocabulary, and be sure to change the language name in the three places that it occurs, as #YourLanguage, below.A complete translation consists, as in #addKiswahiliVocabulary, of calls to three methods, namely:	translateMethodInterfaceWordings:language: 	translateCategories:language:	addToTranslationTableFrom:language: After editing this method into the one that holds your language translations, the next step is to edit #assureTranslationsAvailableFor: so that it calls the method you just created when appropriate.   Consult #addKiswahiliVocabulary and its sender for a complete example to emulate."	self translateMethodInterfaceWordings: #((append: 'include at end' 'Add the object to my content, placing it after all the other objects currently within me.')(beep: 'make sound' 'Make the specified sound')(bounce: 'bounce' 'bounce off the edge if hit')(cameraPoint #cameraPoint 'the camera point')(clear 'clear' 'Clear the graph of current contents')(clearOwnersPenTrails 'clear all pen trails' 'clear all pen trails in my containing playfield')(clearTurtleTrails 'clear pen trails' 'Clear all the pen trails in the interior.')(color:sees: 'color  sees' 'whether the given color sees the given color')(deleteCard 'deleteCard' 'Delete the current card')(doMenuItem: 'do menu item' 'do the menu item')(doScript: 'do' 'run the given script once, on the next tick')(emptyScript 'emptyScript' 'an empty script')(fire 'fire' 'trigger any and all of this object''s button actions')(firstPage 'firstPage' 'go to first page')(followPath 'followPath' 'follow the yellow brick road')(forward: 'forward by' 'Moves the object forward in the direction it is heading')(getActWhen #actWhen 'When the script should fire')(getAllButFirstCharacter #allButFirst 'All my characters except the first one')(getAmount #amount 'The amount of displacement')(getAngle #angle 'The angular displacement')(getBorderColor #borderColor 'The color of the object''s border')(getBorderWidth #borderWidth 'The width of the object''s border')(getBottom #bottom 'The bottom edge')(getBrightnessUnder #brightnessUnder 'The brightness under the center of the object')(getCharacters #characters 'The characters in my contents')(getColor #color 'The color of the object')(getColorUnder #colorUnder 'The color under the center of the object')(getConePosition #conePosition 'the position of the speaker cone')(getCursor #cursor 'The current cursor location, wrapped back to the beginning if appropriate')(getDescending #descending 'Tells whether the smallest value is at the top/left (descending = false) or at the bottom/right (descending = true)')(getDistance #distance 'The length of the vector connecting the origin to the object''s position')(getFirstCharacter #firstCharacter 'The first character in my contents')(getFirstElement #firstElement 'The first object in my contents')(getFogColor #fogColor 'The color of fog being applied')(getFogDensity #fogDensity 'The density of fog being applied')(getFogRangeEnd #fogRangeEnd 'The range start of fog being applied')(getFogRangeStart #fogRangeStart 'The range start of fog being applied')(getFogType #fogType 'The type of fog being applied')(getGraphic #graphic 'The picture currently being worn')(getGraphicAtCursor #graphicAtCursor 'the graphic worn by the object at the cursor')(getHeading #heading 'Which direction the object is facing.  0 is straight up')(getHeight #height 'The height')(getHolder #holder 'the object''s container')(getIndexInOwner #elementNumber 'my index in my container')(getIsUnderMouse #isUnderMouse 'whether the object is under the current mouse position')(getKnobColor #knobColor 'The color of the slider')(getLabel #label 'The wording on the button')(getLastValue #lastValue 'The last value obtained')(getLeft #left 'The left edge')(getLeftRight #leftRight 'The horizontal displacement')(getLuminanceUnder #luminanceUnder 'The luminance under the center of the object')(getMaxVal #maxVal 'The number represented when the knob is at the right or bottom of the slider; the largest value returned by the slider.')(getMinVal #minVal 'The number represented when the knob is at the left or top of the slider; the smallest value returned by the slider.')(getMouseX #mouseX 'The x coordinate of the mouse pointer')(getMouseY #mouseY 'The y coordinate of the mouse pointer')(getNewClone #copy 'returns a copy of this object')(getNumberAtCursor #numberAtCursor 'the number at the cursor')(getNumericValue #numericValue 'A number representing the current position of the knob.')(getObtrudes #obtrudes 'whether the object sticks out over its container''s edge')(getPenColor #penColor 'the color of ink used by the pen')(getPenDown #penDown 'whether the pen is currently down')(getPenSize #penSize 'the width of the pen')(getRight #right 'The right edge')(getRoundedCorners #roundedCorners 'Whether corners should be rounded')(getSampleAtCursor #sampleAtCursor 'The sample value at the current cursor location')(getSaturationUnder #saturationUnder 'The saturation under the center of the object')(getScaleFactor #scaleFactor 'The factor by which the object is magnified')(getTheta #theta 'The angle between the positive x-axis and the vector connecting the origin to the object''s position')(getTop #top 'The top edge')(getTruncate #truncate 'If true, only whole numbers are used as values; if false, fractional values are allowed.')(getUpDown #upDown 'The vertical displacement')(getValueAtCursor #playerAtCursor 'the object currently at the cursor')(getViewingByIcon #viewingNormally 'whether contents are viewed normally')(getWidth #width 'The width')(getX #x 'The x coordinate')(getY #y 'The y coordinate')(goToFirstCardInBackground 'goToFirstCardInBackground' 'Go to the first card of the current background')(goToFirstCardOfStack 'goToFirstCardOfStack' 'Go to the first card of the entire stack')(goToLastCardInBackground 'goToLastCardInBackground' 'Go to the last card of the current background')(goToLastCardOfStack 'goToLastCardOfStack' 'Go to the last card of the entire stack')(goToNextCardInStack 'goToNextCardInStack' 'Go to the next card')(goToPreviousCardInStack 'goToPreviousCardInStack' 'Go to the previous card')(goToRightOf: 'align after' 'place this object to the right of another')(goto: 'goto:' 'go to the given page')(hide 'hide' 'make the object invisible')(include: 'include' 'Add the object to my content')(initiatePainting 'initiatePainting' 'Initiate painting of a new object in the standard playfield.')(insertCard 'insertCard' 'Create a new card')(lastPage 'lastPage' 'go to last page')(liftAllPens 'lift all pens' 'Lift the pens on all the objects in my interior.')(loadSineWave 'loadSineWave' 'Load a sine wave as the current graph')(loadSound: 'loadSound:' 'Load the specified sound into the current graph')(lowerAllPens 'lower all pens' 'Lower the pens on all the objects in my interior.')(makeNewDrawingIn: 'start painting in' 'make a new drawing in the specified playfield')(moveToward: 'move toward' 'move toward the given object')(nextPage 'nextPage' 'go to next page')(pauseAll: 'pause all' 'make the given script be "paused" in the object and all of its siblings')(pauseScript: 'pause script' 'make the given script be "paused"')(play 'play' 'Play the current graph as a sound')(prepend: 'prepend' 'Add the object to my content, placing it before all the other objects currently within me.')(previousPage 'previousPage' 'go to previous page')(removeAll 'removeAll' 'Remove all elements from the playfield')(reverse 'reverse' 'Reverse the graph')(roundUpStrays 'roundUpStrays' 'Bring all out-of-container subparts back into view.')(seesColor: #isOverColor 'whether any part of the object is over the given color')(show 'show' 'make the object visible')(shuffleContents 'shuffleContents' 'Shuffle the contents of the playfield')(stampAndErase 'stampAndErase' 'add my image to the pen trails and go away')(startAll: 'start All' 'start the given script ticking in the object and all of its siblings.')(startScript: 'start script' 'start the given script ticking')(stopAll: 'stop all' 'make the given script be "normal" in the object and all of its siblings')(stopScript: 'stop script' 'make the given script be "normal"')(tellAllSiblings: 'tell all siblings' 'send a message to all siblings')(touchesA: #touchesA 'whether I touch something that looks like...')(turn: 'turn by' 'Change the heading of the object by the specified amount')(unhideHiddenObjects 'unhideHiddenObjects' 'Unhide all hidden objects.')(wearCostumeOf: 'look like' 'wear the costume of...')(wrap 'wrap' 'wrap off the edge if appropriate')) language: #YourLanguage.	self translateCategories: #((basic					'basic'					'a few important things')(#'book navigation'		'book navigation'		'relating to book, stacks, etc')(button					'button'					'for thinking of this object as a push-button control')(collections				'collections'				'for thinking of this object as a collection')(fog					'fog'					'3D fog')(geometry				'geometry' 				'measurements and coordinates')(#'color & border'		'color & border'			'matters concerning the colors and borders of objects')(graphics				'graphics'				'for thinking of this object as a picture')(#'instance variables'	'instance variables'		'instance variables added by this object')(joystick				'joystick	'				'the object as a Joystick')(miscellaneous			'miscellaneous' 			'various commands')(scripting				'scripting'				'commands to start and stop scripts, etc.')(motion					'motion' 				'matters relating to moving and turning')(paintbox				'paintbox'				'the painting palette')(#'pen trails'			'pen trails'				'relating to trails put down by pens')(#'pen use'				'pen use' 				'use of an object''s "pen"')(playfield				'playfield'				'the object as a container for other visible objects')(sampling				'sampling'				'sampling')(scripts					'scripts'					'methods added by this object')(slider					'slider'					'functions useful to sliders')(speaker				'speaker'				'the object as an audio Speaker')(#'stack navigation'		'stack navigation'		'navigation within a stck')(storyboard				'storyboard'				'storyboard')(tests					'tests'					'yes/no tests, to use in "Test" panes of scripts')(text					'text'					'The object as text')(viewing				'viewing'				'matters relating to viewing')(vector					'vector'					'The object as a vector') ) language: #YourLanguage.	self addToTranslationTableFrom: #((:						'_'						'assign value')(Incr:					'increase by'			'increase value by')(Decr:					'decrease by'			'decrease value by')(Mult:					'multiply by'			'multiply value by')) language: #YourLanguage! !!PasteUpMorph class methodsFor: 'scripting' stamp: 'sw 2/4/2002 01:15'!additionsToViewerCategories	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."	^ # ((playfield ((command initiatePainting 'Initiate painting of a new object in the standard playfield.')(slot mouseX 'The x coordinate of the mouse pointer' Number readWrite Player getMouseX  unused unused)(slot mouseY 'The y coordinate of the mouse pointer' Number readWrite Player getMouseY  unused unused)(command roundUpStrays 'Bring all out-of-container subparts back into view.')(slot numberAtCursor 'the Number at the cursor' Number readWrite Player getNumberAtCursor Player setNumberAtCursor: )(slot playerAtCursor 'the object currently at the cursor' Player readWrite Player getValueAtCursor  unused unused)(slot graphicAtCursor 'the graphic worn by the object at the cursor' Graphic readOnly Player getGraphicAtCursor  unused unused)(command unhideHiddenObjects 'Unhide all hidden objects.')))(collections ((slot cursor 'The index of the chosen element' Number readWrite Player getCursor Player setCursorWrapped:)(slot playerAtCursor 'the object currently at the cursor' Player readWrite Player getValueAtCursor  unused unused)(slot firstElement  'The first object in my contents' Player  readWrite Player getFirstElement  Player  setFirstElement:)(slot numberAtCursor 'the number at the cursor' Number readWrite Player getNumberAtCursor Player setNumberAtCursor: )(slot graphicAtCursor 'the graphic worn by the object at the cursor' Graphic readOnly Player getGraphicAtCursor  unused unused)(command removeAll 'Remove all elements from the playfield')(command shuffleContents 'Shuffle the contents of the playfield')(command append: 'Add the object to the end of my contents list.' Player)(command prepend: 'Add the object at the beginning of my contents list.' Player)(command include: 'Add the object to my contents' Player)))(#'stack navigation' ((command goToNextCardInStack 'Go to the next card')(command goToPreviousCardInStack  'Go to the previous card')(command goToFirstCardInBackground 'Go to the first card of the current background')(command goToFirstCardOfStack 'Go to the first card of the entire stack')(command goToLastCardInBackground 'Go to the last card of the current background')(command goToLastCardOfStack 'Go to the last card of the entire stack')(command deleteCard 'Delete the current card')(command insertCard 'Create a new card')))(viewing ((slot viewingNormally 'whether contents are viewed normally' Boolean readWrite Player getViewingByIcon Player setViewingByIcon: )))(#'pen trails' ((command liftAllPens 'Lift the pens on all the objects in my interior.')(command lowerAllPens  'Lower the pens on all the objects in my interior.')(command arrowheadsOnAllPens  'Put arrowheads on the ends of strokes of pens on all objects.')(command noArrowheadsOnAllPens  'Stop putting arrowheads on the ends of strokes of pens on all objects.')(command clearTurtleTrails 'Clear all the pen trails in the interior.'))))! !!Player methodsFor: 'scripts-standard' stamp: 'sw 2/3/2002 23:09'!include: anObject	"Add the object to my content"	^ self append: anObject! !!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 2/3/2002 23:02'!wordingForOperator: aString	"Answer the wording to be seen by the user for the given operator symbol/string"	| toTest |	toTest _ aString asString.	#(	(append:				'append')		(beep:					'make sound')		(bounce:				'bounce')		(clearTurtleTrails		'clear pen trails')		(clearOwnersPenTrails	'clear all pen trails')		(colorSees				'color  sees')		(color:sees:				'color sees')		(doMenuItem:			'do menu item')		(doScript:				'do')		(forward:				'forward by')		(moveToward:			'move toward')		(goToRightOf:			'align after')		(include:				'include')		(isDivisibleBy:			'is divisible by')		(liftAllPens				'lift all pens')		(lowerAllPens			'lower all pens')		(arrowheadsOnAllPens	'arrowheads on all pens')		(noArrowheadsOnAllPens	'no arrowheads on pens')		(pauseAll:				'pause all')		(pauseScript:			'pause script')		(max:					'max')		(min:					'min')		(seesColor:				'is over color')		(makeNewDrawingIn:	'start painting in')		(prepend:				'prepend')		(startAll:				'start all')		(startScript:				'start script')		(stopProgramatically	'stop')		(stopAll:					'stop all')		(stopScript:				'stop script')		(tellAllSiblings:			'tell all')		(turn:					'turn by')		(wearCostumeOf:		'look like'))	do:		[:pair | toTest = pair first ifTrue: [^ pair second]].	^ toTest	"StandardScriptingSystem initialize"! !"Postscript:"Vocabulary changeMadeToViewerAdditions.!