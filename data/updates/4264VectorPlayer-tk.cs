'From Squeak3.1alpha of 28 February 2001 [latest update: #4263] on 19 August 2001 at 11:07:48 am'!"Change Set:		VectorPlayer-tkDate:			18 August 2001Author:			Ted KaehlerBy setting the preference #playersAreVectors to true, Players are treated like vectors.  Their viewers have the operations + - * /  incr: decr: multBy: dividedBy:.  They already have get/set distance and get/set theta.	For the moment, once a project is set to #playersAreVectors, it can't be set to not have vectors.	+ - * / return a Player and should be used for an argument.  	incr: decr: multBy: dividedBy: are commands that take their own line in a script.  Each changes the position of the receiving Player."!!Preferences commentStamp: 'tk 8/18/2001 23:27' prior: 0!A general mechanism to store preference choices.  The default setup treats any symbol as a potential boolean flag; flags unknown to the preference dictionary are always returned as false.  	To open the control panel:		Preferences openFactoredPanel	To read how to use the panel (and how to make a preference be per-project):		 Preferences giveHelpWithPreferencesAll messages are on the class side.To query a a preference:	Preferences logDebuggerStackToFileor some people prefer the more verbose	Preferences valueOfFlag: #logDebuggerStackToFileYou can make up a new preference any time.  Do not define a new message in Preferences class. Accessor methods are compiled automatically when you add a preference as illustrated below:To add a preference (e.g. in the Postscript of a fileout):	Preferences addPreference: #samplePreference categories: #(general browsing)		default: true balloonHelp: 'This is an example of a preference added by a do-it'		projectLocal: false changeInformee: nil changeSelector: nil.To change a preference programatically:	Preferences disable: #logDebuggerStackToFile.Or to turn it on,	Preferences enable: #logDebuggerStackToFile.!]style[(220 29 81 35 812)f1,f1dPreferences openFactoredPanel;;,f1,f1dPreferences giveHelpWithPreferences;;,f1!!Morph methodsFor: 'scripting' stamp: 'tk 8/19/2001 09:46'!selectorsForViewer	"Answer a list of symbols representing all the selectors available in all my viewer categories"	| aClass aList itsAdditions |	aClass _ self renderedMorph class.	aList _ OrderedCollection new.	[aClass == Morph superclass] whileFalse:		[(aClass class includesSelector: #additionsToViewerCategories) ifTrue:			[itsAdditions _ aClass additionsToViewerCategories.			itsAdditions do:				[:anAddition | anAddition second "the spec list" do:					[:aSpec |						 aSpec first == #command ifTrue: [aList add: aSpec second].						aSpec first == #slot ifTrue:							[aList add: (aSpec at: 7).							aList add: (aSpec at: 9)]]]].		aClass _ aClass superclass]. 	^ aList asSet copyWithoutAll: #(unused dummy)"SimpleSliderMorph basicNew selectorsForViewer"! !!Morph class methodsFor: 'scripting' stamp: 'tk 8/19/2001 09:44'!additionsToViewerCategoryGeometry	| list |	list _ #(geometry 		(			(slot  scaleFactor 'The factor by which the object is magnified' number readWrite player getScaleFactor player setScaleFactor:)			(slot  left   'The left edge' number readWrite player getLeft  player  setLeft:)			(slot right  'The right edge' number readWrite player getRight  player  setRight:)			(slot  top  'The top edge' number readWrite player getTop  player  setTop:) 			(slot  bottom  'The bottom edge' number readWrite player getBottom  player  setBottom:) 			(slot  width  'The width' number readWrite player getWidth  player  setWidth:)			(slot  height  'The height' number readWrite player getHeight  player  setHeight:) 			(slot x   'The x coordinate' number readWrite player  getX   player setX:)			(slot y   'The y coordinate' number readWrite player  getY  player setY:)			(slot heading  'Which direction the object is facing.  0 is straight up' number readWrite player getHeading  player setHeading:)			(slot distance 'The length of the vector connecting the origin to the object''s position' number readWrite player getDistance player setDistance:)			(slot theta 'The angle between the positive x-axis and the vector connecting the origin to the object''s position' number readWrite player getTheta player setTheta: )		)	).	Preferences playersAreVectors ifTrue: [		list at: 2 put: (list second, #((command + 'Adds two players together, treating each as a vector from the origin.' player)(command - nil ((aVector  player)) player (geometry) 'Subtracts one player from another, treating each as a vector from the origin.' player)(command * 'Multiply a player by a number, treating the Player as a vector from the origin.' number)(command / 'Divide a player by a number, treating the Player as a vector from the origin.' number)(command incr: 'Each Player is a vector from the origin.  Increase one by the amount of the other.' player)(command decr: 'Each Player is a vector from the origin.  Decrease one by the amount of the other.' player)(command multBy: 'A Player is a vector from the origin.  Multiply its length by the factor.' number)(command dividedBy: 'A Player is a vector from the origin.  Divide its length by the factor.' number)	))].	^ list! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/19/2001 10:55'!* aNumber	"Treating Players like vectors, return a new Player that is myself scaled by the number"	| new |	new _ costume usableSiblingInstance player.	new setX: self getX * aNumber asPoint x.	new setY: self getY * aNumber asPoint y.	^ new! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/19/2001 10:56'!+ aPlayer	"Treating Players like vectors, add aPlayer to me and return a new Player"	| new |	new _ costume usableSiblingInstance player.	new setX: self getX + aPlayer asPoint x.	new setY: self getY + aPlayer asPoint y.	^ new! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/19/2001 10:56'!- aPlayer	"Treating Players like vectors, subtract aPlayer from me and return a new Player"	| new |	new _ costume usableSiblingInstance player.	new setX: self getX - aPlayer asPoint x.	new setY: self getY - aPlayer asPoint y.	^ new! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/19/2001 10:56'!/ aNumber	"Treating Players like vectors, return a new Player that is myself divided by the number"	| new |	new _ costume usableSiblingInstance player.	new setX: self getX / aNumber asPoint x.	new setY: self getY / aNumber asPoint y.	^ new! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/18/2001 22:41'!asPoint	^ self getX @ self getY! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/18/2001 22:46'!decr: aPlayer	"Treating Players like vectors, subtract aPlayer from me"	self setX: self getX - aPlayer asPoint x.	self setY: self getY - aPlayer asPoint y.! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/18/2001 22:51'!dividedBy: aNumber	"Treating Players like vectors, divide myself by aNumber"	self setX: self getX / aNumber asPoint x.	self setY: self getY / aNumber asPoint y.! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/18/2001 22:49'!incr: aPlayer	"Treating Players like vectors, add aPlayer to me"	self setX: self getX + aPlayer asPoint x.	self setY: self getY + aPlayer asPoint y.! !!Player methodsFor: 'scripts-vector' stamp: 'tk 8/18/2001 22:51'!multBy: aNumber	"Treating Players like vectors, scale myself by aNumber"	self setX: self getX * aNumber asPoint x.	self setY: self getY * aNumber asPoint y.! !!Preferences class methodsFor: 'reacting to change' stamp: 'tk 8/18/2001 23:37'!playersAreVectorsToggled	"The current value of the playersAreVectors flag has changed; now react"	self playersAreVectors ifTrue: [Vocabulary addEToyVectorVocabulary]		ifFalse: [Vocabulary removeEToyVectorVocabulary].! !!Preferences class methodsFor: 'reacting to change' stamp: 'tk 8/18/2001 23:00'!setNotificationParametersForStandardPreferences	"Set up the notification parameters for the standard preferences that require need them.  When adding new Preferences that require use of the notification mechanism, users declare the notifcation info as part of the call that adds the preference, or afterwards -- the two relevant methods for doing that are: 	Preferences.addPreference:categories:default:balloonHelp:projectLocal:changeInformee:changeSelector:   and	Preference changeInformee:changeSelector:"		"Preferences setNotificationParametersForStandardPreferences"	| aPreference |	#(			(annotationPanes		annotationPanesChanged)		(eToyFriendly			eToyFriendlyChanged)		(infiniteUndo			infiniteUndoChanged)		(uniTilesClassic			classicTilesSettingToggled)		(optionalButtons			optionalButtonsChanged)		(playersAreVectors		playersAreVectorsToggled)		(roundedWindowCorners	roundedWindowCornersChanged)		(showProjectNavigator	showProjectNavigatorChanged)		(smartUpdating			smartUpdatingChanged)		(universalTiles			universalTilesSettingToggled)		(showSharedFlaps		sharedFlapsSettingChanged))  do:			[:pair |				aPreference _ self preferenceAt: pair first.				aPreference changeInformee: self changeSelector: pair second]! !!Vocabulary methodsFor: 'initialization' stamp: 'tk 8/18/2001 20:10'!addFromTable: aTable	"Add each method-specification tuples, each of the form:		(1)	selector		(2)	companion setter selector (#none or nil indicate none)		(3)  argument specification array, each element being an array of the form				<arg name>  <arg type>		(4)  result type, (#none or nil indicate none)		(5)  array of category symbols, i.e. the categories in which this element should appear.		(6)  help message. (optional)		(7)  wording (optional)		(8)  auto update flag (optional) - if #updating, set readout to refetch automatically	Make new categories as needed.	Consult Vocabulary class.initializeTestVocabulary for an example of use"					| aMethodCategory aMethodInterface aSelector |	aTable do:		[:tuple |   tuple fifth do: [:aCategorySymbol |			(aMethodCategory _ self categoryAt: aCategorySymbol) ifNil: [ 					aMethodCategory _ ElementCategory new categoryName: aCategorySymbol.					self addCategory: aMethodCategory].							aMethodInterface _ MethodInterface new.			aSelector _ tuple first.			aMethodInterface selector: aSelector type: tuple fourth setter: tuple second.			aMethodCategory elementAt: aSelector put: aMethodInterface.			self atKey: aSelector putMethodInterface: aMethodInterface.			((tuple third ~~ #none) and: [tuple third isEmptyOrNil not])				ifTrue:					[aMethodInterface argumentVariables: (tuple third collect:						[:pair | Variable new name: pair first type: pair second])].			tuple size >= 6 ifTrue: [(#(nil none unused) includes: tuple sixth) ifFalse:				[aMethodInterface documentation: tuple sixth]].			tuple size >= 7 ifTrue: [(#(nil none unused) includes: tuple seventh) ifFalse:				[aMethodInterface elementWording: tuple seventh]].			tuple size >= 8 ifTrue:				[aMethodInterface setToRefetch]]].! !!Vocabulary class methodsFor: 'type vocabularies' stamp: 'tk 8/19/2001 09:54'!addEToyVectorVocabulary	"Add vector operations to Players in the EToyVocabulary.  Only wanted when we are doing vectors, since it allows player tiles to be dropped on numbers tiles."	"(selector setterOrNil ((arg name  arg type)...) resultType (category ...) 'help msg' 'wording' autoUpdate)"| table |table _ #((+ nil ((aVector  player)) player (geometry) 'Adds two players together, treating each as a vector from the origin.')(- nil ((aVector  player)) player (geometry) 'Subtracts one player from another, treating each as a vector from the origin.')(* nil ((aVector  number)) player (geometry) 'Multiply a player by a number, treating the Player as a vector from the origin.')(/ nil ((aVector  number)) player (geometry) 'Divide a player by a number, treating the Player as a vector from the origin.')(incr: nil ((aVector  player)) unknown (geometry) 'Each Player is a vector from the origin.  Increase one by the amount of the other.' 'increase by')(decr: nil ((aVector  player)) unknown (geometry) 'Each Player is a vector from the origin.  Decrease one by the amount of the other.' 'decrease by')(multBy: nil ((factor  number)) unknown (geometry) 'A Player is a vector from the origin.  Multiply its length by the factor.' 'multiplied by')(dividedBy: nil ((factor  number)) unknown (geometry) 'A Player is a vector from the origin.  Divide its length by the factor.' 'divided by')"distance and theta are already in player.  See additionsToViewerCategoryGeometry").	Vocabulary eToyVocabulary addFromTable: table.! !!Vocabulary class methodsFor: 'type vocabularies' stamp: 'tk 8/18/2001 21:14'!removeEToyVectorVocabulary	"Add vector operations to Players in the EToyVocabulary.  Only wanted when we are doing vectors, since it allows player tiles to be dropped on numbers tiles."	"(selector setterOrNil ((arg name  arg type)...) resultType (category ...) 'help msg' 'wording' autoUpdate)"	"Later figure out how to remove selectors from a category"#(+ - * / incr: decr: multBy: dividedBy: ).! !Vocabulary class removeSelector: #newVectorVocabulary!"Postscript:Install playersAreVectors as a local preference."Preferences preferenceAt: #playersAreVectors ifAbsent: [	Preferences addPreference: #playersAreVectors categories: #(scripting)		default: false 		balloonHelp:  'Set Players to act as vectors from the origin and do vector arithmetic.'].(Preferences preferenceAt: #playersAreVectors) localToProject ifFalse:			[(Preferences preferenceAt: #playersAreVectors) toggleProjectLocalness].Preferences setNotificationParametersForStandardPreferences.!