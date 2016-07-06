'From Squeak2.9alpha of 13 June 2000 [latest update: #3304] on 25 January 2001 at 5:09:02 pm'!"Change Set:		FixEmptyScript-arDate:			25 January 2001Author:			Andreas RaabFixes various problems with the #emptyScript."!!Morph methodsFor: 'scripting' stamp: 'ar 1/25/2001 12:50'!asEmptyPermanentScriptor	"Answer a new empty permanent scriptor derived from info deftly secreted in the receiver.  Good grief"	| aScriptor aPlayer |	aPlayer _ self valueOfProperty: #newPermanentPlayer.	aPlayer assureUniClass.	aScriptor _  aPlayer newScriptorAround: nil.	aScriptor position: (self world primaryHand position - (10 @ 10)).	aPlayer updateAllViewersAndForceToShow: #scripts.	^ aScriptor! !!Morph class methodsFor: 'scripting' stamp: 'ar 1/20/2001 12:38'!additionsToViewerCategories	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."	^ #(		(#basic (			(slot x 'The x coordinate' number readWrite player getX player setX:)			(slot y  	'The y coordinate' number readWrite	player 	getY player setY:)			(slot heading  'Which direction the object is facing.  0 is straight up' number readWrite player getHeading player setHeading:)			(command forward: 'Moves the object forward in the direction it is heading' number)			(command turn: 'Change the heading of the object by the specified amount' number)			(command beep: 'Make the specified sound' sound)))		"note: if you change the thing below you also need to change #tileScriptCommands."		(#scripts (			(command emptyScript 'an empty script'))		)		(#'color & border' (			(slot color 'The color of the object' color readWrite player getColor  player  setColor:)			(slot colorUnder 'The color under the center of the object' color readOnly player getColorUnder unused  unused )			(slot borderColor 'The color of the object''s border' color readWrite player getBorderColor player  setBorderColor:)			(slot borderWidth 'The width of the object''s border' number readWrite player getBorderWidth player setBorderWidth:)			(slot roundedCorners 'Whether corners should be rounded' boolean readWrite player getRoundedCorners player setRoundedCorners:)))		(geometry (			(slot  scaleFactor 'Yeah, the scale factor' number readWrite player getScaleFactor player setScaleFactor:)			(slot  left   'The left edge, yeah' number readWrite player getLeft  player  setLeft:)			(slot right  'The right edge, yeah' number readWrite player getRight  player  setRight:)			(slot  top  'The top edge' number readWrite player getTop  player  setTop:) 			(slot  bottom  'The bottom edge' number readWrite player getBottom  player  setBottom:) 			(slot  width  'The width' number readWrite player getWidth  player  setWidth:)			(slot  height  'The height' number readWrite player getHeight  player  setHeight:) 			(slot x   'The x coordinate' number readWrite player  getX   player setX:)			(slot y   'The y coordinate' number readWrite player  getY  player setY:)			(slot heading  'Which direction the object is facing.  0 is straight up' number readWrite player getHeading  player setHeading:)))		(miscellaneous (			(command doMenuItem: 'do the menu item' menu) 			(command show 'show the guy')			(command hide 'hide the guy')			(command wearCostumeOf: 'wear the costume of...' player)			(command startScript: 'start the given script ticking' string)			(command stopScript: 'make the given script be "normal"' string)			(command pauseScript: 'make the given script be "paused"' string)			(slot copy 'returns a copy of this object' player readOnly player getNewClone	 unused unused)			(slot elementNumber 'my index in my container' number readWrite player getIndexInOwner player setIndexInOwner:)))		(motion (			(slot x 'The x coordinate' number readWrite player getX player setX:)			(slot y  	'The y coordinate' number readWrite	player 	getY player setY:)			(slot heading  'Which direction the object is facing.  0 is straight up' number readWrite player getHeading player setHeading:)			(command forward: 'Moves the object forward in the direction it is heading' number)			(slot obtrudes 'whether the object sticks out over its container''s edge' boolean readOnly player getObtrudes unused unused) 			(command moveToward: 'move toward the given object' player) 			(command turn: 'Change the heading of the object by the specified amount' number)			(command bounce: 'bounce off the edge if hit' sound) 			(command wrap 'wrap off the edge if appropriate') 			(command followPath 'follow the yellow brick road') 			(command goToRightOf: 'place this object to the right of another' player)))		(#'pen use' (			(slot penColor 'the color of ink used by the pen' color readWrite player getPenColor player setPenColor:) 			(slot penSize 'the width of the pen' number readWrite player getPenSize player setPenSize:) 			(slot penDown 'whether the pen is currently down' boolean readWrite player getPenDown player setPenDown:)))		(#tests (			(slot isOverColor 'whether any part of the object is over the given color' boolean	readOnly player dummy unused unused) 			(slot isUnderMouse 'whether the object is under the current mouse position' boolean readOnly	player getIsUnderMouse unused unused)			(slot colorSees	'whether the given color sees the given color' boolean readOnly	player dummy	unused	unused)			(slot touchesA	'whether I touch something that looks like...' boolean readOnly	player dummy	unused	unused)			(slot obtrudes 'whether the object sticks out over its container''s edge' boolean readOnly player getObtrudes unused unused))))! !!MorphExtension methodsFor: 'copying' stamp: 'ar 1/25/2001 12:49'!copyWeakly	"list of names of properties whose values should be weak-copied when veryDeepCopying a morph.  See DeepCopier."	^ #(formerOwner newPermanentPlayer)	"add yours to this list" "formerOwner should really be nil at the time of the copy, but this will work just fine."!]style[(10 101 10 157)f1b,f1,f1LDeepCopier Comment;,f1! !!Player methodsFor: 'scripts-kernel' stamp: 'ar 1/25/2001 12:50'!commandPhraseFor: commandSpec inViewer: aViewer	"Translate commandSpec into a PhraseTileMorph.  Put appropriate balloon help into the phrase"	| aRow resultType cmd names argType argTile selfTile aPhrase balloonTextSelector stat inst ut |	names _ self class namedTileScriptSelectors.	resultType _ (commandSpec at: 1).	cmd _ (commandSpec at: 2).	ut _ costume world valueOfProperty: #universalTiles ifAbsent: [false].	ut ifTrue: [aPhrase _ (CategoryViewer new) newTilesFor: self command: commandSpec]		ifFalse: [commandSpec size = 3			ifTrue:				[aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player]			ifFalse: "commandSpec size is four"				[argType _ commandSpec at: 4.				aPhrase _ PhraseTileMorph new setOperator: cmd					type: resultType					rcvrType: #player					argType: argType.				argTile _ self tileForArgType: argType inViewer: aViewer.				argTile position: aPhrase lastSubmorph position.				aPhrase lastSubmorph addMorph: argTile]].	(self slotInfo includesKey: cmd)		ifTrue: [balloonTextSelector _ #userSlot].	(self belongsToUniClass and:			[self class includesSelector: cmd])		ifTrue: [balloonTextSelector _ #userScript].	(ut ifTrue: [aPhrase submorphs second] ifFalse: [aPhrase operatorTile]) balloonTextSelector: 			(balloonTextSelector ifNil: [cmd]).	aPhrase markAsPartsDonor.	cmd == #emptyScript ifTrue:[		aPhrase setProperty: #newPermanentScript toValue: true.		aPhrase setProperty: #newPermanentPlayer toValue: self].	ut ifFalse: [		selfTile _ aViewer tileForSelf.		selfTile position: aPhrase firstSubmorph position.		aPhrase firstSubmorph addMorph: selfTile].	aRow _ ViewerRow newRow borderWidth: 0; color: aViewer color.	aRow elementSymbol: cmd asSymbol.	aRow addMorphBack: (ScriptingSystem tryButtonFor: aPhrase).	aRow addMorphBack: (Morph new extent: 4@2; beTransparent).	aRow addMorphBack: (aViewer infoButtonFor: cmd).	aRow addMorphBack: aPhrase.	(names includes: cmd) ifTrue:		[aPhrase userScriptSelector: cmd.		aPhrase beTransparent.		aRow addMorphBack: AlignmentMorph newVariableTransparentSpacer.		aRow addMorphBack: (stat _ (inst _ self scriptInstantiationForSelector: cmd) statusControlMorph).		inst updateStatusMorph: stat.].	aRow beSticky; disableDragNDrop.	^ aRow! !!Viewer methodsFor: 'as yet unclassified' stamp: 'ar 1/25/2001 12:51'!newPermanentScript	| aMorph |	self scriptedPlayer assureUniClass.	self scriptedPlayer isFlagshipForClass ifFalse: [		self flag: #deferred.  "This really has to change, esp in card world"		self primaryHand attachMorph: (StringMorph contents: 'YOU CANNOT MAKE A NEW SCRIPT IF YOU ARE NOT THE FLAGSHIP FOR THE CLASS').		^ self	].	aMorph _ ImageMorph new image: (ScriptingSystem formAtKey: 'newScript').	aMorph setProperty: #newPermanentScript toValue: true.	aMorph setProperty: #newPermanentPlayer toValue: self scriptedPlayer.	self primaryHand attachMorph: aMorph! !"Postscript:Re-initialize reserved names and fix up old #player properties."Smalltalk at: #ScriptingSystem ifPresent:[:s| s initReservedScriptNames].Smalltalk allObjectsDo:[:o| o isMorph ifTrue:[	(o hasProperty: #newPermanentScript) ifTrue:[		o setProperty: #newPermanentPlayer toValue: (o valueOfProperty: #player).		o removeProperty: #player]]].!