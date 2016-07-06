'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:52 pm'!
"Change Set:		q04-variables-sw
Date:			3 March 2004
Author:			Scott Wallace

Derived for 3.7a from Squeakland updates: 0151variables-sw.cs, dated 18 March 2003
0152bolderV-sw, dated 20 March 2003
0165variablesFixes-sw,  dated 15 April 2003

There were numerous collisions with recent 3.7a code.

Use the term 'variable' rather than 'instance variable' in the etoy UI.  Also parameterize the term for used for both this and the user-scripts category, so that the next time we want to make a change like this it will be much easier.

Add a 'v' button added to the header of a Viewer that serves to add a variable, and remove the corresponding menu item  from the Viewer menu.

All of these changes made at the Kim's request.'"!


!Object methodsFor: 'scripting' stamp: 'sw 2/6/2003 18:05'!
methodInterfacesForCategory: aCategorySymbol inVocabulary: aVocabulary limitClass: aLimitClass
	"Return a list of methodInterfaces for the receiver in the given category, given a vocabulary.  aCategorySymbol is the inherent category symbol, not necessarily the wording as expressed in the vocabulary."

	| categorySymbol |
	categorySymbol _ aCategorySymbol asSymbol.

	(categorySymbol == ScriptingSystem nameForInstanceVariablesCategory) ifTrue:		"user-defined instance variables"
		[^ self methodInterfacesForInstanceVariablesCategoryIn: aVocabulary].

	(categorySymbol == ScriptingSystem nameForScriptsCategory) ifTrue:						"user-defined scripts"
		[^ self methodInterfacesForScriptsCategoryIn: aVocabulary].

	^ (self usableMethodInterfacesIn: (aVocabulary methodInterfacesInCategory: (aVocabulary translatedWordingFor: categorySymbol) forInstance: self ofClass: self class limitClass: aLimitClass))   "all others"! !


!CategoryViewer methodsFor: 'categories' stamp: 'sw 3/2/2004 23:53'!
chooseCategory
	"The mouse went down on my category-list control; pop up a list of category choices"

	| aList aMenu reply aLinePosition lineList |
	aList _ scriptedPlayer categoriesForViewer: self.

	aLinePosition _ aList indexOf: #miscellaneous ifAbsent: [nil].
	aList _ aList collect:	
		[:aCatSymbol | self currentVocabulary categoryWordingAt: aCatSymbol].

	lineList _ aLinePosition ifNil: [#()] ifNotNil: [Array with: aLinePosition].
	aList size == 0 ifTrue: [aList add: ScriptingSystem nameForInstanceVariablesCategory translated].
	aMenu _ CustomMenu labels: aList lines: lineList selections: aList.
	reply _ aMenu startUpWithCaption: 'category' translated.
	reply ifNil: [^ self].
	self chooseCategoryWhoseTranslatedWordingIs: reply asSymbol
! !


!EToyVocabulary methodsFor: 'initialization' stamp: 'sw 2/6/2003 18:00'!
initialize
	"Initialize the receiver (automatically called when instances are created via 'new')"

	|   classes aMethodCategory selector selectors categorySymbols aMethodInterface |
	super initialize.
	self vocabularyName: #eToy.
	self documentation: '"EToy" is a vocabulary that provides the equivalent of the 1997-2000 etoy prototype'.
	categorySymbols _ Set new.
	classes _ self morphClassesDeclaringViewerAdditions.
	classes do:
		[:aMorphClass | categorySymbols addAll: aMorphClass basicNew categoriesForViewer].
	self addCustomCategoriesTo: categorySymbols.  "For benefit, e.g., of EToyVectorVocabulary"

	categorySymbols asOrderedCollection do:
		[:aCategorySymbol |
			aMethodCategory _ ElementCategory new categoryName: aCategorySymbol.
			selectors _ Set new.
			classes do:
				[:aMorphClass |
					 (aMorphClass additionsToViewerCategory: aCategorySymbol) do:
						[:anElement |
						aMethodInterface _ self methodInterfaceFrom: anElement.
						selectors add: (selector _ aMethodInterface selector).
						(methodInterfaces includesKey: selector) ifFalse:
							[methodInterfaces at: selector put: aMethodInterface].
						self flag: #deffered.
						"NB at present, the *setter* does not get its own method interface.  Need to revisit"].

			(selectors copyWithout: #unused) asSortedArray do:
				[:aSelector |
					aMethodCategory elementAt: aSelector put: (methodInterfaces at: aSelector)]].
				 
			self addCategory: aMethodCategory].

	self addCategoryNamed: ScriptingSystem nameForInstanceVariablesCategory.
	self addCategoryNamed: ScriptingSystem nameForScriptsCategory.
	self setCategoryDocumentationStrings.

	self addToTranslationTableFrom: #(
(:						'_'						'assign value')
(Incr:					'increase by'			'increase value by')
(Decr:					'decrease by'			'decrease value by')
(Mult:					'multiply by'			'multiply value by')) language: #English
! !

!EToyVocabulary methodsFor: 'initialization' stamp: 'sw 2/26/2003 23:08'!
setCategoryDocumentationStrings
	"Initialize the documentation strings associated with the old etoy categories, in English"

	self translateCategories: #(
(basic					'basic'					'a few important things')
(#'book navigation'		'book navigation'		'relating to book, stacks, etc')
(button					'button'					'for thinking of this object as a push-button control')
(collections				'collections'				'for thinking of this object as a collection')
(fog					'fog'					'3D fog')
(geometry				'geometry' 				'measurements and coordinates')
(#'color & border'		'color & border'			'matters concerning the colors and borders of objects')
(graphics				'graphics'				'for thinking of this object as a picture')
(variables				'variables'				'variables added by this object')
(joystick				'joystick	'				'the object as a Joystick')
(miscellaneous			'miscellaneous' 			'various commands')
(motion					'motion' 				'matters relating to moving and turning')
(paintbox				'paintbox'				'the painting palette')
(#'pen trails'			'pen trails'				'relating to trails put down by pens')
(#'pen use'				'pen use' 				'use of an object''s "pen"')
(playfield				'playfield'				'the object as a container for other visible objects')
(sampling				'sampling'				'sampling')
(scripting				'scripting'				'commands to start and stop scripts')
(scripts					'scripts'					'methods added by this object')
(slider					'slider'					'functions useful to sliders')
(speaker				'speaker'				'the object as an audio Speaker')
(#'stack navigation'		'stack navigation'		'navigation within a stck')
(storyboard				'storyboard'				'storyboard')
(tests					'tests'					'yes/no tests, to use in "Test" panes of scripts')
(text					'text'					'The object as text')
(vector					'vector'					'The object as a vector')
(viewing				'viewing'				'matters relating to viewing')
 ) language: #English
! !

!EToyVocabulary methodsFor: 'category list' stamp: 'sw 4/15/2003 23:49'!
categoryListForInstance: anObject ofClass: aClass limitClass: mostGenericClass
	"Answer the category list for the given object, considering only code implemented in aClass and lower"

	^ (anObject isKindOf: Player)
		ifTrue:
			[self flag: #deferred.  "The bit commented out on next line is desirable but not yet workable, because it delivers categories that are not relevant to the costume in question"
			"#(scripts #'instance variables'), (super categoryListForInstance: anObject ofClass: aClass limitClass: mostGenericClass)]"

			self translatedWordingsFor: ((mostGenericClass == aClass)
				ifFalse:
					[anObject categoriesForVocabulary: self]
				ifTrue:
					[{ScriptingSystem nameForScriptsCategory.  ScriptingSystem nameForInstanceVariablesCategory}])]
		ifFalse:
			[super categoryListForInstance: anObject ofClass: aClass limitClass: mostGenericClass]! !

!EToyVocabulary methodsFor: 'method list' stamp: 'sw 4/15/2003 23:42'!
allMethodsInCategory: aCategoryName forInstance: anObject ofClass: aClass
	"Answer a list of all methods in the etoy interface which are in the given category, on behalf of anObject, or if it is nil, aClass"

	| aCategory unfiltered suitableSelectors isAll |

	aCategoryName ifNil: [^ OrderedCollection new].
	aClass isUniClass ifTrue:
		[aCategoryName = ScriptingSystem nameForScriptsCategory ifTrue:
			[^ aClass namedTileScriptSelectors].
		aCategoryName = ScriptingSystem nameForInstanceVariablesCategory ifTrue:
			[^ aClass slotInfo keys asSortedArray collect:
				[:anInstVarName | Utilities getterSelectorFor: anInstVarName]]].
	unfiltered _ (isAll _ aCategoryName = self allCategoryName)
		ifTrue:
			[methodInterfaces collect: [:anInterface | anInterface selector]]
		ifFalse:
			[aCategory _ categories detect: [:cat | cat categoryName == aCategoryName] 
							ifNone: [^ OrderedCollection new].
			aCategory elementsInOrder collect: [:anElement | anElement selector]].

	(anObject isKindOf: Player) ifTrue:
		[suitableSelectors _ anObject costume selectorsForViewer.
		unfiltered _ unfiltered  select:
			[:aSelector | suitableSelectors includes: aSelector]].
	(isAll and: [aClass isUniClass]) ifTrue:
		[unfiltered addAll: aClass namedTileScriptSelectors.
		unfiltered addAll: (aClass slotInfo keys asSortedArray collect:
			[:anInstVarName | Utilities getterSelectorFor: anInstVarName])].

	^ (unfiltered copyWithoutAll: #(dummy unused)) asSortedArray! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:45'!
addDutchVocabulary
	"Add a Dutch etoy vocabulary

		EToyVocabulary assureTranslationsAvailableFor: #Nederlands
     "
	self translateMethodInterfaceWordings: #(

(append:					'voeg toe'				'Voegt een objekt aan deze houder toe')
(prepend:					'voeg in'				'Voegt een objekt aan het begin van deze houder toe')
(beep:						'maak geluid'  				'Maak het aangegeven geluid')
(bounce:					'stuiter weg'					'Laat het objekt van de rand wegstuiteren en speel het aangegeven geluid wanneer dit gebeurt')
(cameraPoint 				'camerastandpunt'				'Het camerastandpunt van het objekt') 
(clear 						'wis grafiek'						'Wist de huidige grafiek')
(clearOwnersPenTrails		'wis pensporen in eigenaar'	'Wist alle pennesporen in de eigenaar')
(clearTurtleTrails 			'wis pensporen' 		'Wist alle pennesporen in het object')
(color:sees: 					'kleur ziet'			'Test of de aangegeven kleur van het objekt de andere kleur kan zien')
(deleteCard 					'wis kaart'				'Wist deze kaar uit de stapel')
(doMenuItem: 				'voer menu-item uit'	'Voert het aangegeven menupunt uit')
(emptyScript 				'leeg script'				'Een leeg script')
(fire						'vuur'						'Vuurt alle acties die bij deze knop horen af')
(firstPage 					'ga naar eerste pagina'		'Ga naar eerste pagina')
(followPath					'volg pad'					'Volg het gedefineerde pad')
(forward: 					'ga vooruit met'		'Beweegt het objekt vooruit in de huidige richting')
(goToFirstCardInBackground	'ga naar eerste kaart in achtergrond'	'Gaat naar de eerste kaart in de huidige achtergrond')
(goToFirstCardOfStack		'ga naar eerste kaart op stapel'	'Gaat naar de eerste kaart op de stapel')
(goToLastCardInBackground	'ga naar laatste kaart in achtergrond'	'Gaat naar de laatste kaart in de huidige achtergrond')
(goToLastCardOfStack		'ga naar laatste kaart op stapel'		'Gaat naar de laatste kaart op de stapel')
(goToNextCardInStack		'ga naar volgende kaart op stapel'		'Gaat naar de volgende kaart op de stapel')
(goToPreviousCardInStack	'ga naar vorige kaart op stapel'	'Gaat naar de vorige kaart op de stapel')
(goToRightOf:				'plaats rechts van'		'Zet het object rechts naast een ander object neer')
(goto:						'ga naar pagina'		'Gaat naar de aangegeven pagina')
(hide					'verstop je'					'Verstopt het object')
(initiatePainting			'begin met nieuwe tekening'		'Start met een nieuwe tekening')
(insertCard				'maak kaart aan'				'Maakt een nieuwe kaart aan en voegt deze in')
(lastPage				'laatste pagina'			'Gaat naar de laatste pagina')
(liftAllPens				'pak alle pennen op'			'Pakt alle pennen op die zich in het binnenste bevinden')
(loadSineWave			'laad sinusgolf'			'Laadt een sinusgolf als de huidige grafiek')
(loadSound:				'laad geluid'					'Laadt een bepaald geluid als de huidige grafiek')
(lowerAllPens			'zet alle pennen neer'			'Zet alle pennen neer die zich in het binnenste bevinden')
(makeNewDrawingIn:	'start nieuwe tekening in'	'Begint een nieuwe tekening in het aangegeven objekt')
(moveToward:			'ga in richting '				'Beweegt het objekt in de richting van het andere objekt')
(nextPage				'ga naar volgende pagina'		'Gaat naar de volgende pagina')
(pauseScript:			'stop script'					'Stopt het uitvoeren van een script')
(play					'speel geluid af'				'Speelt de huidige grafiek als geluid af')
(previousPage			'ga naar vorige pagina' 	'Gaat naar de vorige pagina')
(removeAll				'verwijder alles'				'Verwijdert en wist alle elementen')
(reverse				'omdraaien'						'Draait de inhoud van de grafiek om')
(roundUpStrays			'haal terug'					'Haalt alle objekten terug, als ze zich ergens verstoppen')
(seesColor:				'ziet de kleur'					'Test, of het objekt de aangegeven kleur ziet')
(show					'toon jezelf'						'Toont het objekt')
(shuffleContents			'mix inhoud'					'Mixt alle objekten in een willekeurige volgorde')
(stampAndErase			'stempel en verwijder je'		'Voegt de afbeelding van het objekt aan het pennespoor toe en verwijdert het vervolgens.')
(startScript:				'start script'					'Begint met het uitvoeren van een script')
(stopScript:				'stop script'					'Stopt met het uitvoeren van een script')
(tellAllSiblings:			'zeg tegen alle broers en zussen'		'Stuurt een bericht aan alle broers/zussen van het objekt')
(touchesA:				'raakt aan'					'Test, of een objekt van het aangegeven type aangeraakt wordt')
(turn:					'draai je'				'Verandert de richting van het objekt met het aangegeven aantal graden')
(unhideHiddenObjects	'toon verstopte objekten' 		'Laat alle verstopte objekten zien')
(wearCostumeOf: 		'draag kostuum van'				'Laat het kostuum van het aangegeven objekt zien')
(wrap					'wikkel je om'				'Wikkelt het objekt om de rand van zijn houder')

(getActWhen 			'Uitvoeringsstatus'	'Bepaalt wanneer het script uitgevoerd wordt')
(getAllButFirstCharacter	'Alle tekens behalve de eerste' 'Bevat alle tekens behalve het eerste teken')
(getAmount				'grootte'					'De grootte van de afwijking ten op zichte van het centrum')
(getAngle				'hoek'					'De hoek van de afwijking ten op zichte van het centrum')
(getBorderColor			'randkleur'		'De kleur van de rand')
(getBorderWidth			'randbreedte'		'De breedte van de rand')
(getBottom 				'onderkant'					'De onderkant van het objekt')
(getBrightnessUnder		'helderheid onder'			'De helderheid onder het midden van het objekt')
(getCharacters			'tekens'					'De tekens van de inhoud')
(getColor				'kleur' 					'De kleur van het objekt')
(getColorUnder 			'kleur eronder'			'De kleur onder het midden van het objekt')
(getConePosition			'Conuspositie'			'De positie van de conus van de luidspreker')
(getCursor				'Cursor'					'De huidige tekenaanwijzing')
(getDescending			'afdalend'				'Bepaalt of de kleinste waarde eerste getoond moet worden')
(getDistance				'afstand'				'De afstand tot de oorsprong van de houder')
(getFirstCharacter		'eerste teken'		'Het eerste teken van de inhoud')
(getFirstElement			'eerste element'			'Het eerste element van de inhoud')
(getFogColor				'nevelkleur'				'De kleur van de nevel')
(getFogDensity			'neveldichtheid'			'De dichtheid van de nevel')
(getFogRangeEnd		'nevelbegin'			'De beginafstand van de nevel')
(getFogRangeStart		'neveleinde'				'De eindafstand van de nevel')
(getFogType				'nevelsoort'				'Het soort nevel')
(getGraphic				'beeld'					'De afbeelding van het objekt')
(getGraphicAtCursor		'beeld bij cursor'			'De afbeelding van het objekt bij de cursor')
(getHeading				'richting'				'De richting waar het objekt in staat')
(getHeight				'hoogte'					'De hoogte van het objekt')
(getHolder				'houder'				'De houder van het objekt')
(getIndexInOwner		'index in eigenaar' 			'De positie van het objekt in zijn eigenaar')
"@@@: Folgendes sollte vermutlich die Hand und nicht die Maus referenzieren :@@@"
(getIsUnderMouse		'is muis erover'		'Test, of de mous op het objekt staat')
"@@@: Sollte vielleicht 'Griff' heissen, aber ich mag Knubbel :-) :@@@"
(getKnobColor			'greepkleur'			'De kleur van de greep')
(getLabel				'opschrift'			'Het opschrift van het objekt')
(getLastValue			'laatste waarde'			'De laatste invoerwaarde')
(getLeft					'linkerkant'			'De linkerkant van het objekt')
(getLeftRight			'links-rechts'			'De horizontale afwijking ten opzichte van het centrum')
(getLuminanceUnder	'lichtsterkte eronder'	'De lichtsterkte onder het objekt')
(getMaxVal				'maximale waarde'			'De maximale waarde van de regelaar')
(getMinVal				'minimale waarde'			'De minimale waarde van de regelaar')
(getMouseX				'muis x-positie'		'De X koordinaat van de muispositie')
(getMouseY				'muis y-positie'		'De Y koordinaat van de muispositie')
(getNewClone			'kopieer je'			'Maakt een kopie van het objekt')
(getNumberAtCursor		'aantal bij cursor'			'Het aantal bij de cursor')
(getNumericValue		'waarde van regelaar'				'De huidige waarde van de regelaar')
(getObtrudes				'steekt uit'			'Test of het object uit zijn houder uitsteekt')
(getPenColor				'penkleur'				'De kleur van de pen')
(getPenDown			'pen neer'				'De status van de pen (op/neer)')
(getPenSize				'pengrootte'			'De doorsnede van de pen')
(getRight				'rechterkant'			'De rechterkant van het objekt')
(getRoundedCorners		'afgeronde hoeken'			'Test of hoeken afgerond moeten worden')
(getSampleAtCursor		'steekproef'				'Een steekproef van de waarde bij de cursor')
(getSaturationUnder		'verzadiging eronder'	'De verzadiging van de kleur onder het objekt')
(getScaleFactor			'schalingsfaktor'		'De schalingsfaktor van het objekt')
(getTheta				'theta'					'De hoek met de X-as')
(getTop					'bovenkant'			'De bovenkant van het objekt')
(getTruncate			'afronden'				'Bepaalt of slechts gehele getallen gebruikt worden')
(getUpDown				'erboven-onder'			'De verticale afwijking ten opzichte van het centrum')
(getValueAtCursor		'objekt bij cursor'		'Het objekt bij de huidige cursorpositie')
(getViewingByIcon 		'symboolweergave'		'Bepaalt of het objekt met zijn symbool moet worden weergegeven')
(getX					'x'						'De X koordinaat van het objekt')
(getY					'y'						'De Y koordinaat van het objekt')
(getWidth				'breedte'					'De breedte van het objekt')) language: #Nederlands.


	self translateCategories: #(
(basic					Simpel					'Standaardzicht')
(#'book navigation'		#'Boeknavigatie'		'Navigatie in boeken')
(button					Knop					'Het objekt als knop')
(collections				Houder				'Het objekt als houder')
(fog					Nevel					'3D Nevel eigenschappen')
(geometry				Geometrie				'Over de geometrief van het objekt')
(#'color & border'		#'Kleur & Rand'			'Over kleuren en randen')
(graphics				Grafisch					'Grafische eigenschappen')
(variables				Variabelen				'Variabelen van het objekt')
(joystick				Joystick					'Het objekt als joystick')
(miscellaneous			Overige			'Alles dat nergens anders bij past')
(motion					Beweging				'Bewegingseigenschappen')
(paintbox				Tekenpalet				'Het tekenpalet')
(#'pen trails'			Pennesporen				'Alles over het achterlaten van pennesporen')
(#'pen use'				Pengebruik					'Het gebruik van pennen')
(playfield				Speelveld				'Het speelveld')
(sampling				Meten					'Het meten van waarden')
(scripts					Scripts					'Al je scripts')
(slider					Regelaar					'Het objekt als regelaar')
(speaker				Luidspreker			'Het objekt als luidspreker')
(#'stack navigation'		Stapelnavigatie		'Navigatie in stapels')
(storyboard				Storyboard				'Storyboard')
(tests					Tests					'Verscheidene tests')
(text					Tekst						'Het objekt als tekst')
(viewing				Observatie				'Zoals je het objekt ziet')
(vector					Vector					'Het objekt als vector')
 ) language: #Nederlands "nou ja, nederlands... ;-) - CdG"
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:45'!
addGermanVocabulary
	"Add a German etoy vocabulary"
	self translateMethodInterfaceWordings: #(

(append:					'hänge an'				'Fügt ein Objekt in diesen Behälter ein')
(prepend:					'hänge davor'				'Fügt ein Objekt in diesen Behälter ein')
(beep:						'mache Geräusch'  				'Macht das angegebene Geräusch')
(bounce:					'pralle ab'					'Läßt das Objekt vom Rand seines Behälters abprallen und spielt das angegebene Geräusch, wenn es außerhalb ist')
(cameraPoint 				'Kamerapunkt'				'Der Kamerapunkt des Objektes') 
(clear 						'lösche Graph'						'Löscht den momentanen Graphen')
(clearOwnersPenTrails		'lösche Stiftspuren im Eigner'	'Löscht alle Stiftspuren im Eigner')
(clearTurtleTrails 			'lösche Stiftspuren' 		'Löscht all Stiftspuren im Objekt')
(color:sees: 					'Farbe sieht'			'Überprüft, ob die angebene Farbe des Objektes die Testfarbe sehen kann')
(deleteCard 					'lösche Karte'				'Löscht diese Karte aus dem Stapel')
(doMenuItem: 				'führe Menüpunkt aus'	'Führt den angebenen Menüpunkt aus')
(emptyScript 				'leeres Skript'				'Ein leeres Skript')
(fire						'feuer'						'Führt alle zugehörigen Aktionen dieses Schalters aus')
(firstPage 					'gehe zur ersten Seite'		'Geht zur ersten Seite')
(followPath					'folge Pfad'					'Folge dem definierten Pfad')
(forward: 					'gehe vorwärts um'		'Bewegt das Objekt vorwärts in seiner momentanen Richtung')
(goToFirstCardInBackground	'gehe zur ersten Karte im Hintergund'	'Geht zur ersten Karte im momentanen Hintergrund')
(goToFirstCardOfStack		'gehe zur ersten Karte im Stapel'	'Geht zur ersten Karte im Stapel')
(goToLastCardInBackground	'gehe zur letzten Karte im Hintergund'	'Geht zur letzten Karte im momentanen Hintergrund')
(goToLastCardOfStack		'gehe zur letzten Karte im Stapel'		'Geht zur letzten Karte im Stapel')
(goToNextCardInStack		'gehe zur nächsten Karte im Stapel'		'Geht zur nächsten Karte im momentanen Hintergrund')
(goToPreviousCardInStack	'gehe zur vorherigen Karte im Stapel'	'Geht zur vorherigen Karte im momentanen Hintergrund')
(goToRightOf:				'plaziere rechts von'		'Setzt das Objekt rechts neben ein anderes')
(goto:						'gehe zur Seite'		'Geht zur angegebenen Seite')
(hide					'verstecke Dich'					'Versteckt das Objekt')
(initiatePainting			'beginne neue Zeichnung'		'Beginnt eine neue Zeichnung')
(insertCard				'erzeuge Karte'				'Erzeugt eine neue Karte und fügt sie ein.')
(lastPage				'gehe zur letzten Seite'			'Geht zur letzten Seite')
(liftAllPens				'nimm alle Stifte hoch'			'Nimmt alle Stifte hoch, die sich im Inneren befinden')
(loadSineWave			'lade Sinuswelle'			'Lädt eine Sinuswelle als momentanen Graph')
(loadSound:				'lade Geräusch'					'Lädt das angegebene Geräusch als momentanen Graph')
(lowerAllPens			'setze alle Stifte ab'			'Setzt alle Stifte ab, die sich im Inneren befinden')
(makeNewDrawingIn:	'beginne neue Zeichnung in'	'Beginnt eine neue Zeichnung im angegebenen Objekt')
(moveToward:			'gehe in Richtung '				'Bewegt das Objekt in Richtung eines anderen Objektes')
(nextPage				'gehe zur nächsten Seite'		'Geht zur nächsten Seite')
(pauseScript:			'stoppe Skript'					'Hält ein Skript an')
(play					'spiele Gerausch ab'				'Spielt den momentanen Graphen als Geräusch ab')
(previousPage			'gehe zur vorherigen Seite'	'Geht zur vorherigen Seite')
(removeAll				'entferne alles'				'Entfernt und löscht alle Elemente')
(reverse				'umdrehen'						'Dreht den Inhalt des Graphen um')
(roundUpStrays			'hole zurück'					'Holt alle Objekte zurück, falls sie sich irgendwo verstecken')
(seesColor:				'sieht die Farbe'					'Überprüft, ob das Objekt die angegebene Farbe sieht')
(show					'zeige Dich'						'Zeigt das Objekt')
(shuffleContents			'mische Inhalt'					'Mischt alle Objekte zufällig')
(stampAndErase			'stanze und lösche Dich'		'Fügt das Abbild des Objektes den Stiftspurent hinzu und löscht es anschließend.')
(startScript:				'starte Skript'					'Beginnt die wiederholte Ausführung eines Skriptes')
(stopScript:				'stoppe Script'				'Beendet die wiederholte Ausführung eines Skriptes')
(tellAllSiblings:			'sage den Geschwistern'		'Sendet eine Nachricht zu allen Geschwistern des Objektes')
(touchesA:				'berührt'					'Überprüft, ob ein Objekt des angegebenen Typs berührt wird')
(turn:					'drehe Dich um'				'Ändert die Richtung des Objektes um den angegebenen Winkel')
(unhideHiddenObjects	'zeige versteckte Objekte' 		'Zeigt alle versteckten Objekte an')
(wearCostumeOf: 		'trage Kostüm von'				'Trägt das Kostüm eines anderen Objektes')
(wrap					'wickel Dich rum'				'Wickelt das Objekt um den Rand seines Behälters')

(getActWhen 			'Ausführungsstatus'	'Bestimmt, wann das Skript ausgeführt wird')
(getAllButFirstCharacter	'Alle Buchstaben außer dem Ersten' 'Enthält alle Buchstaben außer dem Ersten')
(getAmount				'Betrag'					'Der Betrag der Abweichung vom Zentrum')
(getAngle				'Winkel'					'Der Winkel der Abweichung vom Zentrum')
(getBorderColor			'Randfarbe'		'Die Farbe des Randes')
(getBorderWidth			'Randbreite'		'Die Breite des Randes')
(getBottom 				'untere Kante'					'Die untere Kante des Objektes')
(getBrightnessUnder		'Helligkeit darunter'			'Die Helligkeit unter dem Zentrum des Objektes')
(getCharacters			'Buchstaben'					'Die Buchstaben des Inhalts')
(getColor				'Farbe' 					'Die Farbe des Objektes')
(getColorUnder 			'Farbe darunter'			'Die Farbe unter dem Zentrum des Objektes')
(getConePosition			'Membranposition'			'Die Position der Membran des Lautsprechers')
(getCursor				'Zeiger'					'Der momentane Zeigerindex')
(getDescending			'Absteigend'				'Bestimmt, ob der kleinste Wert zuerst gezeigt werden soll')
(getDistance				'Distanz'				'Die Distanz zum Ursprung des Behälters')
(getFirstCharacter		'Erster Buchstabe'		'Der erste Buchstabe des Inhalts')
(getFirstElement			'Erstes Element'			'Das erste Element des Inhalts')
(getFogColor				'Nebelfarbe'				'Die Farbe des Nebels')
(getFogDensity			'Nebeldichte'			'Die Dichte des Nebels')
(getFogRangeEnd		'Nebelanfang'			'Die Anfangsdistanz des Nebels')
(getFogRangeStart		'Nebelende'				'Die Enddistanz des Nebels')
(getFogType				'Nebeltyp'				'Der Typ des Nebels')
(getGraphic				'Bild'					'Das Bild des Objektes')
(getGraphicAtCursor		'Bild am Zeiger'			'Das Bild des Objektes am Zeiger')
(getHeading				'Richtung'				'Die Richtung in die das Objekt weist')
(getHeight				'Höhe'					'Die Höhe des Objektes')
(getHolder				'Behälter'				'Der Behälter dieses Objekt')
(getIndexInOwner		'Eignerindex' 			'Der Index des Objektes in seinem Eigner')
"@@@: Folgendes sollte vermutlich die Hand und nicht die Maus referenzieren :@@@"
(getIsUnderMouse		'ist Maus darüber'		'Überprüft, ob die Maus über dem Objekt ist')
"@@@: Sollte vielleicht 'Griff' heissen, aber ich mag Knubbel :-) :@@@"
(getKnobColor			'Knubbelfarbe'			'Die Farbe des Knubbels')
(getLabel				'Beschriftung'			'Die Beschriftung des Objektes')
(getLastValue			'letzter Wert'			'Der letzte Eingabewert')
(getLeft					'linke Kante'			'Die linke Kante des Objektes')
(getLeftRight			'links-rechts'			'Die horizontale Abweichung vom Zentrum')
(getLuminanceUnder	'Leuchtkraft darunter'	'Die Leuchtkraft unter dem Objekt')
(getMaxVal				'Maximalwert'			'Der maximale Wert des Reglers')
(getMinVal				'Minimalwert'			'Der minimale Wert des Reglers')
(getMouseX				'Maus X-Position'		'Die X Koordinate der Mausposition')
(getMouseY				'Maus Y-Position'		'Die Y Koordinate der Mausposition')
(getNewClone			'kopiere Dich'			'Erzeugt eine Kopie des Objektes')
(getNumberAtCursor		'Zahl am Zeiger'			'Die Zahl am Zeiger')
(getNumericValue		'Reglerwert'				'Der momentane Wert des Reglers')
(getObtrudes				'ragt hinaus'			'Überprüft, ob das Objekt aus seinem Eigner herausragt')
(getPenColor				'Stiftfarbe'				'Die Farbe des Stiftes')
(getPenDown			'Stift unten'				'Der Status des Stiftes')
(getPenSize				'Stiftgröße'			'Der Durchmesser des Stiftes')
(getRight				'rechte Kante'			'Die rechte Kante des Objektes')
(getRoundedCorners		'runde Ecken'			'Bestimmt, ob Ecken abgerundet werden')
(getSampleAtCursor		'Stichprobe'				'Eine Stichprobe des Wertes an der momentanen Zeigerposition')
(getSaturationUnder		'Sättigung darunter'	'Die Sättigung der Farbe unter dem Objekt')
(getScaleFactor			'Skalierungsfaktor'		'Der Skalierungsfaktor des Objektes')
(getTheta				'Theta'					'Der ''Zentrums-Ursprungs-Rechts'' Winkel (huh?)')
(getTop					'obere Kante'			'Die obere Kante des Objekts')
(getTruncate			'Abrunden'				'Bestimmt, ob nur ganze Zahlen benutzt werden')
(getUpDown				'hoch-runter'			'Die vertikale Abweichung vom Zentrum')
(getValueAtCursor		'Objekt am Zeiger'		'Das Objekt an der momentanen Zeigerposition')
(getViewingByIcon 		'Symboldarstellung'		'Bestimmt, ob Objekte symbolisch oder normal dargestellt werden')
(getX					'x'						'Die X Koordinate des Objektes')
(getY					'y'						'Die Y Koordinate des Objektes')
(getWidth				'Breite'					'Die Breite des Objektes')) language: #Deutsch.


	self translateCategories: #(
(basic					Einfach					'Standardsicht')
(#'book navigation'		#'Buchnavigation'		'Navigation in Büchern')
(button					Schalter					'Das Objekt als Schalter')
(collections				Behälter				'Das Objekt als Behälter')
(fog					Nebel					'3D Nebel Eigenschaften')
(geometry				Geometrie				'Zur Geometrie des Objektes')
(#'color & border'		#'Farbe & Rand'			'Zum Thema Farben und Ränder')
(graphics				Graphik					'Graphische Eigenschaften')
(variables				Variablen				'Variablen des Objektes')
(joystick				Joystick					'Das Objekt als Joystick')
(miscellaneous			Verschiedenes			'Alles was woanders nicht hinpaßt')
(motion					Bewegung				'Bewegungseigenschaften')
(paintbox				Malpalette				'Die Zeichenpalette')
(#'pen trails'			Stiftspuren				'Alles zum Thema Spuren hinterlassen')
(#'pen use'				Stifte					'Verwendung von Stiften')
(playfield				Spielfeld				'ja ich weiß auch nicht...')
(sampling				Messen					'Messungen von Werten')
(scripts					Skripte					'Alle Deine Skripte')
(slider					Regler					'Das Objekt als Regler')
(speaker				Lautsprecher			'Das Objekt als Lautsprecher')
(#'stack navigation'		Stapelnavigation		'Navigation in Stapeln')
(storyboard				Storyboard				'Später mal...')
(tests					Tests					'Verschiedene Tests')
(text					Text						'Das Objekt als Text')
(viewing				Betrachtung				'Wie man''s so sieht')
(vector					Vektor					'Das Objekt als Vektor')
 ) language: #Deutsch
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addKiswahiliVocabulary
	"Add a Kiswahili vocabulary"

	self translateMethodInterfaceWordings: #(

(append:					'tia mwishoni'				'weka kitu hicho mwishoni')
(beep:						'fanya kelele'  				'piga kelele fulani')
(bounce:					'ruka duta'					'ruka duta kama mpira')
(cameraPoint 				'penye kamera'				'mahali penya kamera') 
(clear 						'kumba'						'ondoa vilivyokwemo')
(clearOwnersPenTrails		'ondoa nyayo'				'ondoa nyayo za wino')
(clearTurtleTrails 			'ondoa nyayo ndani' 		'ondoa nyayo za wino zilzo ndani')
(color:sees: 					'rangi yaona rangi'			'kama rangi fulana yaona rangi nyingine')
(deleteCard 					'tupa karata'				'tupa karata hii')
(doMenuItem: 				'fanya uchaguzi'			'fanya uchaguzi fulani')
(doScript:					'piga script'					'piga script ya jina fulani mara moja')
(emptyScript 				'script tupu'				'tengeneza script mpya tupu')
(fire						'waka'						'waka script, yaani kuianzisha')
(firstPage 					'nenda mwanzoni'			'nenda penye ukurasa wa kwanza')
(followPath					'fuata njia'					'fuata njia iliyofanywa kabla')
(forward: 					'nenda mbele'				'sogea mbela kwa kiasi fulani')
(goToFirstCardInBackground	'endea kwanza ya nyuma'	'endea karata kwanza ya nyuma')
(goToFirstCardOfStack		'endea kwanza ya stack'	'endea karata iliyo ya kwanza ya stack')
(goToLastCardInBackground	'endea mwisho ya nyuma'	'endea karata ya mwisho ya nyuma')
(goToLastCardOfStack		'endea mwisho ya stack'		'endea karata ya mwisho ya stack')
(goToNextCardInStack		'endea karata ifuatayo'		'endea karata itakayofuata penye stack')
(goToPreviousCardInStack	'endea karata itanguliayo'	'endea karata kliyonitangulia penye stack')
(goToRightOf:				'endea karibu ya kulia'		'sogea hata nipo upande wa kulia kuhusu kitu fulani')
(goto:						'endea mahali fulani'		'endea mahali fulani')
(hide					'ficha'							'nifanywe ili nisionekane')
(initiatePainting			'anza kupiga picha'				'anza kupiga picha mpya')
(insertCard				'weka karata mpya'				'weka karata mpya ndani ya stack')
(lastPage				'ukurasa wa mwisho'			'endea ukurasa ya mwisho')
(liftAllPens				'inua kalamu zote'				'inua kalamu zote zilizomo ndani, ili zisipige rangi')
(loadSineWave			'pakia wimbi la sine'			'pakia wimibi (la kitrigonometry) la sine')
(loadSound:				'pakia kelele'					'pakia kelele fulani')
(lowerAllPens			'telemsha kalamu zote'			'telemsha kalamu zote ya vitu vyote vilivyomo ndani')
(makeNewDrawingIn:	'anza kupiga picha kiwanjani'	'anza kupaga picha mpya ndani ya kiwanja')
(moveToward:			'nenda upande wa'				'nenda upande wa kitu fulani')
(nextPage				'endea ukurasa ufuatao'		'nenda ukurasani unaofuata')
(pauseScript:			'pumzisha script'				'pumzisha script fulani')
(pauseAll:				'pumzisha script zote'			'pumzisha script fulani katika mwenyewe na ndugu zangu wote')
(play					'cheza'							'cheza, basi!!')
(previousPage			'endea ukurasa uliotangulia'	'enda ukurasa uliotangulia ukurusa huu')
(removeAll				'ondoa vyote vilivyokuwemo'	'ondoa vitu vyote vilvyomo dani')
(reverse				'kinyume'						'kinyume cha upande')
(roundUpStrays			'kusanya'						'sanya vitu vilovyopotoleka')
(seesColor:				'yaona rangi'					'kama naona rangi fulani')
(show					'onyesha'						'fanya hata naonekana')
(shuffleContents			'changanya'					'changanya orodha ya ndani')
(stampAndErase			'piga chapa na kufuta'			'piga chapa, halafu kufuta')
(startScript:				'anzisha script'					'anzisha script ya jina fulani')
(startAll:				'anzisha script zote'				'anzisha script fulani penye mwenyewe na ndugu zangu wote')
(stopAll:					'simamisha script zote'			'simamisha script fulani penye mwenyewe na ndugu zangu wote')
(stopScript:				'simamisa skriptu'				'simamisha script ya jana fulani')
(tellAllSiblings:			'watangazie ndugu'				'tangaza habari kwa ndugu zangu wote')
(touchesA:				'yagusa'						'kama nagusa kitu cha aina fulani')
(turn:					'geuka'							'geuka kwa pembe fulani')
(unhideHiddenObjects	'onyesha vilivyofichwa' 		'onyesha vitu ndani vilivyofichwa')
(wearCostumeOf: 		'vaa nguo za'					'vaa nguo za mtu mwingine')
(wrap					'zunguka'						'baada ya kutoka, ingia n''gambo')

(getActWhen 			'waka kama'			'lini ya waka')
(getAllButFirstCharacter	'herufi ila ya kwanza' 'herufi zote isipokuwa ile ya kwanza tu')
(getAmount				'kiasi'					'kiasi gani')
(getAngle				'pembe'					'pembe iliyopo (degree)')
(getBorderColor			'rangi ya mpaka'		'rangi ya mpaka wangu')
(getBorderWidth			'upana wa mpaka'		'upana wa mpaka wangu')
(getBottom 				'chini'					'chini yangu')
(getBrightnessUnder		'mng''aro chini'			'mwangaza chini yangu')
(getCharacters			'herufi'					'herufi zangu')
(getColor				'rangi' 					'rangi yangu')
(getColorUnder 			'rangi chini'			'rangi chini yangu')
(getConePosition			'penye cone'			'mahali penye cone')
(getCursor				'kidole'					'namba ya kitu ndani kilichagulwa')
(getDescending			'kama yaenda chini'	'kama naonyesha vitu chini')
(getDistance				'urefu'					'urefu kutoka asili')
(getFirstCharacter		'herufi ya kwanza'		'herufi yangu ya kwanza')
(getFirstElement			'kitu cha kwanza'		'kitu changu cha ndani cha kwanza')
(getFogColor				'rangi ya ukungu'		'rangi ya ukungu wangu')
(getFogDensity			'nguvu wa ukungu'	'nguvu ya ukungu wangu')
(getFogRangeEnd		'mwisho wa ukungu'	'mwisho wa upana wa ukungu wangu')
(getFogRangeStart		'mwanzo wa ukungu'	'mwanzo wa upana wa ukungu wangu')
(getFogType				'aina ya ukungu'		'aina ya ukungu wangu')
(getGraphic				'picha'					'picha ninayonyesha')
(getGraphicAtCursor		'picha penye kidolee'	'picha iliyopo penye kidole changu')
(getHeading				'upande'				'upande gani ninayoelekea')
(getHeight				'urefu'					'urefu wangu')
(getHolder				'mshikoi'				'kitu niliomo ndani yake')
(getIndexInOwner		'namba kataki mwenyeji' 'namba niliyo nayo katika mwenyeji')
(getIsUnderMouse		'chini kipanya'			'kama nipo chini ya kipanya')
(getKnobColor			'rangi ya ndani'		'rangi ya sehemu yangu ya ndani')
(getLabel				'tangazo'				'iliyoandishwa juu yangu')
(getLastValue			'mapimo'				'iliyokuwemo ndani')
(getLeft					'kushoto'				'mpaka wa kushoto')
(getLeftRight			'kiasi cha sawasawa'	'kiasi cha kushoto ama kulia')
(getLuminanceUnder	'uNg''aa chini'			'uNg''aa ya sehemu chini yangu')
(getMaxVal				'kiasi cha juu'			'kiasi cha juu humu ndani')
(getMinVal				'kiasi cha chini'		'kiasi cha chini humu ndani')
(getMouseX				'x ya kipanya'			'mahali pa x pa kipanya')
(getMouseY				'y ya kipanya'			'mahali pa y pa kipanya')
(getNewClone			'nakala'				'fanya nakala yangu')
(getNumberAtCursor		'namba kidoleni'		'namba iliyopo kidoleni')
(getNumericValue		'namba humu'			'namba iliyopo katika kituc hicho')
(getObtrudes				'jiingiliza'				'kama kitu hicho hujiingiliza')
(getPenColor				'rangi ya kalamu'		'rangi ninayotumia kwa kalamu')
(getPenDown			'kalamu chini'			'kama kalamu hukaa chini')
(getPenSize				'upana wa kalamu'		'urefu wa kalamu ninayotumia')
(getRight				'kulia'					'mpaka wa kulia')
(getRoundedCorners		'viringisha'				'tumia pembe zilizoviringishwa')
(getSampleAtCursor		'kiasi kidoleni'			'kiasi kilichopo kidoleni')
(getSaturationUnder		'kunyewesha chini'		'kiasi cha kunyewesha chini ya kati yangu')
(getScaleFactor			'kuzidisha kwa'			'kiasi ninachozidishwa nacho')
(getTheta				'theta'					'pemba kwa x-axis')
(getTop					'juu'					'mpaka wa juu')
(getTruncate			'kata'					'kama kukata ama sivyo')
(getUpDown				'juu/chini'				'kiasi cha juu ama cha chini')
(getValueAtCursor		'mchezaji kidoleni'		'mchechazji aliyepo kidoneni')
(getViewingByIcon 		'angalia kwa picha'		'kama vitu vilivyomo ndani huanagaliwa kwa picha ama sivyo')
(getX					'x'						'mahali pa x')
(getY					'y'						'mahali ya y')
(getWidth				'upana'					'upana wangu')) language: #Kiswahili.

	self translateCategories: #(
(basic					muhimu				'mambo muhimu muhimu')
(#'book navigation'		#'kuongoza vitabu'		'kuhusu kuongozea vitabu')
(button					kifungo					'mambo kuhusu vifungo')
(collections				mikusanyo				'kuhusu mikusanyo ya vitu')
(fog					ukungu					'kuhusu ukungu (3D)')
(geometry				kupimia					'urefu na kadhaliki')
(#'color & border'		#'rangi & mpaka'		'kuhusu rangi na mpaka')
(graphics				picha					'mambo kuhusu picha')
(variables				badiliko					'data zilizoundwa na yule atumiaye')
(joystick				#'fimbo la furaha'		'kuhusu fimbo la furaha, yaani "joystick"')
(miscellaneous			mbalimbali				'mambo mbalimbali')
(motion					kusogea					'kwenda, kuegeuka, etc.')
(paintbox				#'kupiga rangi'			'vitu kuhusu kupigia rangi')
(#'pen trails'			#'nyayo za kalamu'		'kuhusu nyay za kalamu')
(#'pen use'				#'kalamu'				'kuhusu kalamu')
(playfield				kiwanja					'vitu kuhusu kiwanjani')
(sampling				kuchagua				'mambo kuhusu kuchagua')
(scripts					taratibu				'taratibu zilizoundwa na atumiaye')
(slider					telezo					'kitu kionyeshacho kiasi cha namba fulani')
(speaker				spika					'kuhusu spika za kelele')
(#'stack navigation'		#'kuongoza chungu'	'kuhusu kuongozea chungu')
(storyboard				kusimulia				'kusimilia hadithi')
(tests					kama					'amua kama hali fulani i kweli ama sivyo')
(text					maneno					'maandiko ya maneno')
(viewing				kuangaliwa				'kuhusu kuangalia vitu')
(vector					vektor					'kuhusu vektor')
) language: #Kiswahili.

self addToTranslationTableFrom: 

#((:						'_'						'tia ndani')
(Incr:					'pamoja na'				'tia thamani + fulani')
(Decr:					'toa'					'tia thamani - fulani')
(Mult:					'mara'					'tia thamani * fulani')) language: #Kiswahili.
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addNorwegianVocabulary
	"Add a Norwegian vocabulary. "

	self translateMethodInterfaceWordings: #(
(append: 'legg til' 'Legg til objektet i mitt innehold')
(beep: 'spill lyd' 'Spill denne lyden')
(bounce: 'sprett' 'sprett bort fra kanten om den treffes')
(cameraPoint 'kameraposisjon' 'kamerans posisjon')
(clear 'rens' 'Rens bort bildet')
(clearOwnersPenTrails 'rens alla pennestrøk' 'Rens bort alle pennestrøk i pennens lekeplass')
(clearTurtleTrails 'rens skilpaddestrøk' 'Rens bort alle skilpaddestrøk på innsiden')
(color:sees: 'farge syns' 'Om valgte farge ser angitt farge')
(deleteCard 'Ta bort kort' 'Ta bort dette kortet')
(doMenuItem: 'utfør menyvalg' 'utfør menyvalget')
(doScript: 'kjør' 'kjør skriptet en gang, ved nesta tick')
(emptyScript 'tomt skript' 'et tomt skript')
(fire 'starta' 'starter alle knapphendelser for dette objektet')
(firstPage 'førsta siden' 'gå till førsta siden')
(followPath 'følg vei' 'følg denne veien')
(forward: 'framover med' 'Flytter objektet framover i objektets nåværende rettning')
(getActWhen 'kjør når' 'Når skriptet skal kjøres')
(getAllButFirstCharacter 'alle uten om førsta' 'Alle mine tegn uten om det første')
(getAmount 'størrelse' 'Forflyttningens størrelse')
(getAngle 'vinkel' 'Vinkelforflyttningens størrelse')
(getBorderColor 'kantfarge' 'Fargen på objektets kant')
(getBorderWidth 'kantbredde' 'Bredden på objektets kant')
(getBottom 'bunn' 'Den nederste kanten')
(getBrightnessUnder 'kontrast' 'Kontrasten under objektets mittpunkt')
(getCharacters 'tegn' 'Tegnet i mitt innehold')
(getColor 'farge' 'Objektets farge')
(getColorUnder 'farge under' 'Fargen under objektets mittpunkt')
(getConePosition 'høytalerposition' 'høytalerens position')
(getCursor 'markør' 'Markørens nåværende position, ombytt til første om det er mulig')
(getDescending 'fallende' 'Sier om den minste verdien er øverst / til venstre (fallende = false) eller nederst / til høyre (fallande = true)')
(getDistance 'avstand' 'Lengden på vektoren mellom utgangspunktet og objektets posisjon')
(getFirstCharacter 'første tegnet' 'Det første tegnet i mitt innehold')
(getFirstElement 'førsta elementet' 'Det førsta objektet i mitt innehold')
(getFogColor 'tåkens farge' 'Fargen på tåken som benyttes')
(getFogDensity 'tåkens tetthet' 'Tettheten på tåken som benyttes')
(getFogRangeEnd 'tåkens intervalslutt' 'Intervalets slutt på tåken som benyttes')
(getFogRangeStart 'tåkens intervalstart' 'Intervalets start på tåken som benyttes')
(getFogType 'tåkens typ' 'Typen av tåke som benyttes')
(getGraphic 'bilde' 'Bildet som bæres av objektet')
(getGraphicAtCursor 'bilde ved markør' 'Bildet som bæres av objektet ved markøren')
(getHeading 'rettning' 'I vilken rettning objektet peker. 0 er rett opp')
(getHeight 'høyde' 'Høyden')
(getHolder 'beholder' 'objektets beholder')
(getIndexInOwner 'elementnummer' 'mitt indeksnummer i min beholdere')
(getIsUnderMouse 'er under muspekeren' 'om objektet befinner seg under muspekeren')
(getKnobColor 'håndtakets farge' 'Håndtakets farge')
(getLabel 'etikett' 'Teksten på knappen')
(getLastValue 'seneste verdi' 'Seneste beregnede verdi')
(getLeft 'venstre' 'Den venstre kanten')
(getLeftRight 'venstre/høyre' 'Horisontel forflyttning')
(getLuminanceUnder 'lysstyrke under' 'Lysstyrken under objektets mittpunkt')
(getMaxVal 'maxverdi' 'Tallet som representerer når håndtaket er lengst til høyre eller lengst ned, den største verdien som håndtaket gir.')
(getMinVal 'minverdi' 'Tallet som representerer når håndtaket er lengst til venstre eller høyest opp, den minste verdien som håndtaket gir.')
(getMouseX 'mus x' 'Muspekerens x-koordinat')
(getMouseY 'mus y' 'Muspekerens y-koordinat')
(getNewClone 'kopia' 'returnerer en kopi av det her objektet')
(getNumberAtCursor 'tall ved markør' 'tallet ved markøren')
(getNumericValue 'numeriskt verdi' 'Et tall som representerer den aktuelle posisjonen av håndtaket.')
(getObtrudes 'stikker ut' 'om objektet stikker ut over beholderens kant')
(getPenColor 'pennefarge' 'fargen på blekket i pennen')
(getPenDown 'pen nedtrykt' 'om pennen er nedtrykt nå')
(getPenSize 'pennens bredde' 'pennens bredde')
(getRight 'høyre' 'Den høyre kanten')
(getRoundedCorners 'rundede hjørner' 'om hjørnene skal være runde')
(getSampleAtCursor 'verdi ved markør' 'Nåværende verdi ved markørens posisjon')
(getSaturationUnder 'mettning under' 'Fargemettning under objektets mittpunkt')
(getScaleFactor 'skala' 'Objektets skala')
(getTheta 'theta' 'Vinkelen mellom den positive x-axelen og vektoren mellom utgangspunktet og objektets posisjon')
(getTop 'toppen' 'Den øverste kanten')
(getTruncate 'heltall' 'Om bare heltall anvendes som verdi, om bråktal ikke er tillatt.')
(getUpDown 'opp/ner' 'Vertikal forflyttning')
(getValueAtCursor 'spiller ved markør' 'objektet ved markøren')
(getViewingByIcon 'normalt synssett' 'Synsettet på inneholdet er normal')
(getWidth 'bredd' 'Bredden')
(getX 'x' 'X-koordinaten')
(getY 'y' 'Y-koordinaten')
(goToFirstCardInBackground 'gå til første i bakgrunnen' 'Gå til det første kortet i den nåværende bakgrunnen')
(goToFirstCardOfStack 'gå til første kortet i stacken' 'Gå til første korten i hele stacken')
(goToLastCardInBackground 'gå til siste kortet i bakgrunnen' 'Gå til det siste kortet i den nåværende bakgrunnen')
(goToLastCardOfStack 'gå til siste kortet i stacken' 'Gå til det siste kortet i hele stacken')
(goToNextCardInStack 'gå till neste kort' 'Gå til neste kort i stacken')
(goToPreviousCardInStack 'gå til forrige kort' 'Gå til det forrige kortet i stacken')
(goToRightOf: 'plassere etter' 'plassere dette objekt til høyre om et annet')
(goto: 'gå til' 'gå til angitt side')
(hide 'gjem' 'gjør objektet usynligt')
(initiatePainting 'begynn malning' 'Begynn malning av et nytt objekt i den vanlige lekeplatsen.')
(insertCard 'legg inn kort' 'Skap et nytt kort')
(lastPage 'siste siden' 'gå til siste siden')
(liftAllPens 'løft alle penner' 'Løft pennene på mitt inneholds alle objekt.')
(loadSineWave 'åpne sinusbølge' 'Åpne en sinusbølge som nåværende graf')
(loadSound: 'åpne lyd' 'Åpne angivet lyd som nåværende lyd')
(lowerAllPens 'senk alle penner' 'Senk pennene på mitt inneholds alle objekt.')
(makeNewDrawingIn: 'bgynn malning i' 'lag en ny malning i angiven lekeplats')
(moveToward: 'flytte mot' 'flytte mot angivet objekt')
(nextPage 'nesta side' 'gå til neste side')
(pauseAll: 'pause alle' 'pause skriptet i objektet och alle dets slekninger')
(pauseScript: 'pause skript' 'pause skriptet')
(play 'spela' 'Spill nåværende graf som en lyd')
(previousPage 'foregående side' 'gå til foregående side')
(removeAll 'ta bort alle' 'Ta bort alle element fra lekeplatsen')
(reverse 'reversere' 'Reversere grafen')
(roundUpStrays 'samle inn bortsprungne' 'Samle inn alle deler utenfor beholderen så at de blir synlige igen.')
(seesColor: 'er over farge' 'om noen del av objektet er over den angitte fargen')
(show 'vise' 'gjør objektet synlig')
(shuffleContents 'blande innehold' 'Blande lekeplatsens innehold')
(stampAndErase 'stemple og forsvinn' 'legg til mitt bilde som tegnestrek och forsvinn')
(startAll: 'start alle' 'start skriptet tickende i objektet och alla dets slekninger.')
(startScript: 'start skript' 'start skriptet tickende')
(stopAll: 'stopp alle' 'gjør skriptet "normalt" i objektet och alla dets slektninger')
(stopScript: 'stopp skript' 'gjør skriptet "normalt" i objektet')
(tellAllSiblings: 'si til slekninger' 'send et meddelende til alla slekninger')
(touchesA: 'rörer' 'Om jeg rörer noe som ser ut som...')
(turn: 'sving med' 'Endre objektets retning med angitt mengde')
(unhideHiddenObjects 'vise alle gjemte objekt' 'Gjør alle gjemte objekt synlige.')
(wearCostumeOf: 'se ut som' 'bær samme drakt som...')
(wrap 'fold over kant' 'fold over kanten om det er passende')) language: #Norsk.

	self translateCategories: #(
(basic					'elementære'				'et antall viktige saker')
(#'book navigation'		'boknavigering'			'saker som har med bøker och stackar å gjøre')
(button					'knapp'					'objektet betraktet som en knapp man kan trykke på')
(collections				'samlinger'				'objektet betraktet som en samling av andre objekt')
(fog					'tåke'					'3-dimensionell tåke')
(geometry				'geometri' 				'verdier og koordinater')
(#'color & border'		'farger & kanter'		'saker som har med farger og kanter på objekt å gjøre')
(graphics				'grafikk'					'objektet betraktat som et bilde')
(variables				'instansvariabler'		'instansvariabler tilhørende dette objektet')
(joystick				'joystick	'				'objektet som en joystick')
(miscellaneous			'diverse' 				'diverse kommander')
(motion					'rørelse' 				'saker som har med forflyttning och vridning å gjøre')
(paintbox				'maleskrin'				'malepaletten')
(#'pen trails'			'pennestrøk'				'saker som har med strøk som pennen tegner å gjøre)
(#'pen use'				'pen bruk' 		'bruk av et objekts pen')
(playfield				'lekeplats'					'objektet som beholdere for andre synlige objekt')
(sampling				'prøvetagning'			'prøvetagning')
(scripts					'skript'					'metoder lagt til dette objektet')
(slider					'håndtak'			'nyttige funksjoner for håndtak')
(speaker				'høytaler'				'objektet sett som en høytaler')
(#'stack navigation'		'stacknavigering'		'navigering innen en stack')
(storyboard				'storyboard'				'storyboard')
(tests					'tester'					'for å lage ja/nei tester i script')
(text					'tekst'					'Objektet som tekst')
(viewing				'seer'					'Saker som har med olika seeren å gjøre')
(vector					'vektor'					'Objektet som en vektor')
 ) language: #Norsk.

	self addToTranslationTableFrom: #(
(:						'_'						'gi verdi')
(Incr:					'øke med'			'øka verdi med')
(Decr:					'minske med'			'minske verdi med')
(Mult:					'multiplicera med'			'multiplisere verdi med'')) language: #Norsk! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addSpanishVocabulary

   "'Español' translation by Diego Gómez Deck and Germán Morales"
   " á é í ó ú ñ Ñ "

   self translateMethodInterfaceWordings: #(
(append:                   'agregar al final'                    'Agregar el objeto a mi contenido, poniéndolo después de todos los otros objetos actualmente contenidos por mi')
(beep:                     'sonar'                               'Reproducir el sonido especificado')
(bounce:                   'rebotar'                             'Rebotar si toca el borde')
(cameraPoint               'punto de la cámara'                  'El punto de la cámara')
(clear                     'limpiar'                             'Limpia el gráfico del contenido actual')
(clearOwnersPenTrails      'limpiar todos los rastros del lápiz'       'Limpiar todos los rastros del lápiz en mi contenedor')
(clearTurtleTrails         'limpiar los rastros del lápiz'       'Limpiar los rastros del lápiz en el interior')
(color:sees:               'color ve'                            'Si el color dado ve el otro color')
(deleteCard                'borrar tarjeta'                      'Borrar la tarjeta actual')
(doMenuItem:               'realizar la opción del menú'         'Realizar la opción del menú')
(doScript:                 'realizar'                            'Realizar el guión dado una vez, en el próximo instante')
(emptyScript               'guión vacío'                         'Un guión vacío')
(fire                      'activar'                             'Activar todas y cada una de las acciones del botón de este objeto')
(firstPage                 'primera página'                      'Ir a la primera página')
(followPath                'seguir el camino'                    'Seguir el camino')
(forward:                  'avanzar'                             'Mover el objeto hacia adelante en la dirección del objeto')
(getActWhen                'activar cuando'                      'Cuando se debe activar el guión')
(getAllButFirstCharacter   'todos excepto el primero'            'Todos mis caracteres excepto el primero')
(getAmount                 'desplazamiento'                      'La cantidad de desplazamiento')
(getAngle                  'ángulo'                              'El desplazamiento angular')
(getBorderColor            'color del borde'                     'El color del borde del objeto')
(getBorderWidth            'ancho del borde'                     'El ancho del borde del objeto')
(getBottom                 'abajo'                               'El borde de abajo')
(getBrightnessUnder        'brillo debajo'                       'El brillo debajo del centro del objeto')
(getCharacters             'caracteres'                          'Los caracteres en mi contenido')
(getColor                  'color'                               'El color del objeto')
(getColorUnder             'color debajo'                        'El color debajo del centro del objeto')
(getConePosition           'posición del cono'                   'La posición del cono del altavoz')
(getCursor                 'cursor'                              'La posición actual del cursor, trasladado al principio si es apropiado')
(getDescending             'descendiente'                        'Verdadero si el menor valor está arriba o a la izquierda, falso si está abajo o a la derecha')
(getDistance               'distancia'                           'El largo del vector que conecta el origen a la posición del objeto')
(getFirstCharacter         'primer caracter'                     'El primer caracter de mi contenido')
(getFirstElement           'primer elemento'                     'El primer objeto de mi contenido')
(getFogColor               'color de la niebla'                  'El color de la niebla que se está aplicando')
(getFogDensity             'intensidad de la niebla'             'La intensidad de la niebla que se está aplicando')
(getFogRangeEnd            'niebla hasta'                        'Hasta donde se aplica la niebla')
(getFogRangeStart          'niebla desde'                        'Desde donde se aplica la niebla')
(getFogType                'tipo de niebla'                      'El tipo de la niebla que se está aplicando')
(getGraphic                'gráfico'                             'El gráfico usado actualmente')
(getGraphicAtCursor        'gráfico en el cursor'                'El gráfico usado por el objeto en el cursor')
(getHeading                'dirección'                           'En que dirección está mirando el objeto. Cero significa hacia arriba')
(getHeight                 'altura'                              'La altura del objeto')
(getHolder                 'contenedor'                          'El contenedor del objeto')
(getIndexInOwner           'número de elemento'                     'Mi número dentro de mi contenedor')
(getIsUnderMouse           'debajo del ratón'                    'Si el objeto está debajo de la posición actual del ratón')
(getKnobColor              'color de la perilla'                 'El color del deslizador')
(getLabel                  'etiqueta'                            'La etiqueta del botón')
(getLastValue              'último valor'                        'El último valor obtenido')
(getLeft                   'izquierda'                           'El borde de la izquierda')
(getLeftRight              'desplazamiento izquierda - derecha'  'El desplazamiento horizontal')
(getLuminanceUnder         'luminiscencia debajo'                'La luminiscencia debajo del centro del objeto')
(getMaxVal                 'máximo valor'                        'El número representado cuando la perilla está a la derecha o abajo del deslizador. Este es el máximo valor devuelto por el deslizador')
(getMinVal                 'mínimo valor'                        'El número representado cuando la perilla está a la izquierda o arriba  del deslizador. Este es el mínimo valor devuelto por el deslizador')
(getMouseX                 'x del ratón'                         'La coordenada X del puntero del ratón')
(getMouseY                 'y del ratón'                         'La coordenada Y del puntero del ratón')
(getNewClone               'copiar'                              'Devuelve una copia de este objeto')
(getNumberAtCursor         'número en el cursor'                 'El número en el cursor')
(getNumericValue           'valor numérico'                      'Un número representando la posición actual de la perilla')
(getObtrudes               'sobresale'                         'Si el objeto sobresale de los bordes de su contenedor')
(getPenColor               'color del lápiz'                     'El color del lápiz')
(getPenDown                'lápiz bajo'                          'Si el lápiz está bajo')
(getPenSize                'tamaño del lápiz'                    'El ancho del lápiz')
(getRight                  'derecha'                             'El borde derecho')
(getRoundedCorners         'esquinas redondeadas'                'Si las esquinas deberían ser redondeadas')
(getSampleAtCursor         'muestra en el cursor'                'El valor de la muestra en la posición actual del cursor')
(getSaturationUnder        'saturación debajo'                   'La saturación debajo del centro del objeto')
(getScaleFactor            'factor de escala'                    'El factor de escala por el cual el objeto es magnificado')
(getTheta                  'theta'                               'El ángulo entre el eje X positivo y el vector que conecta el origen a la posición del objeto')
(getTop                    'arriba'                              'El borde de arriba')
(getTruncate               'truncado'                            'Si es verdadero, solo números son usados como valores; si es falso, valores fraccionarios son permitidos')
(getUpDown                 'desplazamiento arriba - abajo'       'El desplazamiento vertical')
(getValueAtCursor          'objeto en el cursor'                 'El objeto actualmente en el cursor')
(getViewingByIcon          'vista normal'                        'Si los contenidos son vistos normalmente')
(getWidth                  'ancho'                               'El ancho')
(getX                      'x'                                   'La coordenada X')
(getY                      'y'                                   'La coordenada Y')
(goToFirstCardInBackground 'ir a la primera tarjeta del fondo'   'Ir a la primera tarjeta del fondo actual')
(goToFirstCardOfStack      'ir a la primera tarjeta de la pila'  'Ir a la primera tarjeta de la pila completa')
(goToLastCardInBackground  'ir a la última tarjeta del fondo'    'Ir a la última tarjeta del fondo actual')
(goToLastCardOfStack       'ir a la última tarjeta de la pila'   'Ir a la última tarjeta de la pila completa')
(goToNextCardInStack       'ir a la próxima tarjeta de la pila'  'Ir a la próxima tarjeta')
(goToPreviousCardInStack   'ir a la anterior tarjeta de la pila' 'Ir a la anterior tarjeta')
(goToRightOf:              'alinear después de'                     'Ubicar este objeto a la derecha de otro')
(goto:                     'ir a'                                'Ir a la página dada')
(hide                      'ocultar'                             'Hacer invisible al objeto')
(include:                  'incluir'                             'Agregar el objeto a mi contenido')
(initiatePainting          'iniciar pintado'                     'Iniciar el pintado de nuevos objetos en el campo de juegos estandar')
(insertCard                'insertar nueva tarjeta'              'Insertar una nueva tarjeta')
(lastPage                  'última página'                       'Ir a la última página')
(liftAllPens               'levantar todos los lápices'          'Levantar los lápices de todos los objetos en mi interior')
(loadSineWave              'cargar forma de onda senoidal'       'Cargar una forma de onda senoidal como el gráfico actual')
(loadSound:                'cargar sonido'                       'Cargar el sonido especificado dentro del gráfico actual')
(lowerAllPens              'bajar todos los lápices'             'Bajar los lápices de todos los objetos en mi interior')
(makeNewDrawingIn:         'empezar pintado en'                  'Crear un nuevo dibujo en el campo de juegos especificado')
(moveToward:               'mover hacia'                         'Mover hacia el objeto dado')
(nextPage                  'próxima página'                      'Ir a la próxima página')
(pauseAll:                 'pausar todo'                         'Hacer que todos los guiones se pausen en el objeto y en todos sus hermanos')
(pauseScript:              'pausar guión'                        'Hacer que se pause el guión dado')
(play                      'reproducir'                          'Reproducir el gráfico actual como un sonido')
(prepend:                  'agregar adelante'                    'Agregar el objeto a mi contenido, poniéndolo antes que todos los otros objetos actualmente contenidos por mi')
(previousPage              'página anterior'                     'Ir a la página anterior')
(removeAll                 'remover todo'                        'Remover todos los elementos del campo de juegos')
(reverse                   'invertir'                            'Invertir el gráfico')
(roundUpStrays             'reunir perdidos'                       'Traer todas las partes fuera del contenedor nuevamente a la vista')
(seesColor:                'sobre color'                         'Si alguna parte del objeto esta sobre el color dado')
(show                      'mostrar'                             'Hacer visible el objeto')
(shuffleContents           'mezclar el contenido'                'Mezclar el contenido del campo de juegos')
(stampAndErase             'estampar y borrar'                   'Estampar mi imagen al rastro del lápiz y salir')
(startAll:                 'comenzar todo'                       'Comenzar el latido del guión en el objeto y en todos sus hermanos')
(startScript:              'comenzar guión'                      'Comenzar el latido del guión dado')
(stopAll:                  'parar todos'                         'Hacer que el estado del guión dado sea "normal" en el objeto y en todos sus hermanos')
(stopScript:               'detener guión'                         'Hacer que el estado del guión dado sea "normal"')
(tellAllSiblings:          'decir a todos los hermanos'          'Enviar un mensaje a todos los hermanos')
(touchesA:                 'toca un'                             'Si toco algo que parece como...')
(turn:                     'girar'                               'Cambiar la dirección del objeto en la cantidad especificada')
(unhideHiddenObjects       'mostrar objetos ocultos'             'Mostrar todos los objetos ocultos')
(wearCostumeOf:            'lucir como'                          'Usar el traje de...')

(wrap                      'ajusta'                                'traslada al otro borde si es apropiado')
(penArrowheads             'puntas de flecha en el lápiz'        'Si muestra puntas de flecha al final de cada trazo del lápiz')
(dropShadow                'sombra'                              'Si muestra la sombra')
(shadowColor               'color de la sombra'                  'Color de la sombra')
(clipSubmorphs             'cortar submorphs'                    'Si cortar o no mis submorphs')

) language: #'Español'.

   self translateCategories: #(
(basic                 'básico'                 'Unas pocas cosas importantes')
(#'book navigation'    'navegacion del libro'   'Relativo a libros, pilas, etc.')
(button                'botón'                  'Para pensar este objeto como un botón')
(collections           'colecciones'            'Para pensar este objeto como una colección')
(fog                   'niebla'                 'Niebla 3D')
(geometry              'geometría'              'Medidas y coordenadas')
(#'color & border'     'color y borde'          'Asuntos relacionados con colores y bordes de objetos')
(graphics              'gráficos'               'Para pensar este objeto como una imagen')
(variables			 'variables de instancia' 'Variables de instancia agregadas por este objeto')
(joystick              'joystick'               'El objeto como un Joystick')
(miscellaneous         'misceláneo'             'Comandos varios')
(scripting             'guiones'                'Comandos para  comenzar y parar guiones, etc.')
(motion                'movimiento'             'Asuntos relacionados con el movimiento y el giro')
(paintbox              'paleta de pintor'       'La paleta del pintor')
(#'pen trails'         'rastros del lápiz'      'Relacionados a los rastros dejados por los lápices')
(#'pen use'            'uso del lápiz'          'Uso del lápiz del objeto')
(playfield             'campo de juegos'        'El objeto como un contenedor para otros objetos visibles')
(sampling              'muestreo'               'Muestreo')
(scripts               'guiones'                'Guiones agregados por este objeto')
(slider                'deslizador'             'Funciones útiles para deslizadores')
(speaker               'altavoz'                'El objeto como un altavoz de audio')
(#'stack navigation'   'navegación de pila'     'Navegación dentro de una pila')
(storyboard            'pizarra de historia'    'Pizarra de historia')
(tests                 'pruebas'                'Pruebas por verdadero/falso, para usar en cuadros de "Prueba" de guiones')
(text                  'texto'                  'El objeto como texto')
(viewing               'visualización'          'Asuntos relacionados a visualización')
(vector                'vector'                 'El objeto como un vector')
(layout                'disposición'            'Disposición de los objetos contenidos')
(#'drag & drop'        'arrastrar y soltar'     'Arrastrar y soltar')
(observation           'observación'            'Observación')

 ) language: #'Español'.

        self addToTranslationTableFrom: #(
(:                                                '_'                                                'asignar el valor')
(Incr: 'incrementar por' 'incrementar el valor por')
(Decr: 'reducir por' 'reducir el valor por')
(Mult: 'multiplicar por' 'multiplicar el valor por')) language: #'Español'
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addSwedishVocabulary
	"Add a Swedish vocabulary. Well, ok, perhaps better translations could be made...
	I might have simplified a few word choices to be better suited for kids like 'sudda' instead of 'rensa' (two words for 'clear'). Hmm, this was hard... :-)"

	self translateMethodInterfaceWordings: #(
(append: 'lägg till' 'Lägg till objektet i mitt innehåll')
(beep: 'spela ljud' 'Spela upp angivet ljud')
(bounce: 'studsa' 'studsa bort från kanten ifall den träffas')
(cameraPoint 'kameraposition' 'kamerans position')
(clear 'sudda' 'Sudda bildens innehåll')
(clearOwnersPenTrails 'sudda alla pennstreck' 'Sudda alla pennstreck i pennans lekplats')
(clearTurtleTrails 'sudda pennstreck' 'Sudda alla pennstreck på insidan')
(color:sees: 'färg syns' 'ifall vald färg ser angiven färg')
(deleteCard 'Ta bort kort' 'Ta bort det nuvarande kortet')
(doMenuItem: 'utför menyval' 'utför menyvalet')
(doScript: 'kör' 'kör skriptet en gång, vid nästa tick')
(emptyScript 'tomt skript' 'ett tomt skript')
(fire 'starta' 'startar alla knapphändelser för detta objekt')
(firstPage 'första sidan' 'gå till första sidan')
(followPath 'följ väg' 'följ den utsatta vägen')
(forward: 'framåt med' 'Flyttar objektet framåt i objektets nuvarande riktning')
(getActWhen 'kör när' 'När skriptet skall köras')
(getAllButFirstCharacter 'alla utom första' 'Alla mina tecken utom det första')
(getAmount 'storlek' 'Förflyttningens storlek')
(getAngle 'vinkel' 'Vinkelförflyttningens storlek')
(getBorderColor 'kantfärg' 'Färgen på objektets kant')
(getBorderWidth 'kantbredd' 'Bredden på objektets kant')
(getBottom 'botten' 'Den nedersta kanten')
(getBrightnessUnder 'kontrast' 'Kontrasten under objektets mittpunkt')
(getCharacters 'tecken' 'Tecknen i mitt innehåll')
(getColor 'färg' 'Objektets färg')
(getColorUnder 'färg under' 'Färgen under objektets mittpunkt')
(getConePosition 'högtalarposition' 'högtalarens position')
(getCursor 'markör' 'Markörens nuvarande position, omväxlad till början ifall det är lämpligt')
(getDescending 'fallande' 'Säger ifall det minsta värdet är överst/till vänster (fallande = false) eller nederst/till höger (fallande = true)')
(getDistance 'avstånd' 'Längden på vektorn mellan ursprungspunkten och objektets position')
(getFirstCharacter 'första tecknet' 'Det första tecknet i mitt innehåll')
(getFirstElement 'första elementet' 'Det första objektet i mitt innehåll')
(getFogColor 'dimmans färg' 'Färgen på dimman som appliceras')
(getFogDensity 'dimmans densitet' 'Densiteten på dimman som appliceras')
(getFogRangeEnd 'dimmans intervallslut' 'Intervallets slut på dimman som appliceras')
(getFogRangeStart 'dimmans intervallstart' 'Intervallets start på dimman som appliceras')
(getFogType 'dimmans typ' 'Typen av dimma som appliceras')
(getGraphic 'bild' 'Bilden som bärs av objektet')
(getGraphicAtCursor 'bild vid markör' 'Bilden som bärs av objektet vid markören')
(getHeading 'riktning' 'Åt vilket håll objektet pekar. 0 är rakt upp')
(getHeight 'höjd' 'Höjden')
(getHolder 'behållare' 'objektets behållare')
(getIndexInOwner 'elementnummer' 'mitt index i min behållare')
(getIsUnderMouse 'är under muspekaren' 'ifall objektet befinner sig under muspekaren')
(getKnobColor 'draghandtagets färg' 'Draghandtagets färg')
(getLabel 'etikett' 'Texten på knappen')
(getLastValue 'senaste värde' 'Senast beräknade värde')
(getLeft 'vänster' 'Den vänstra kanten')
(getLeftRight 'vänster/höger' 'Horisontell förflyttning')
(getLuminanceUnder 'ljusstyrka under' 'Ljusstyrkan under objektets mittpunkt')
(getMaxVal 'maxvärde' 'Talet som representerar när draghandtaget är längst till höger eller längst ner, det största värdet som draghandtaget ger.')
(getMinVal 'minvärde' 'Talet som representerar när draghandtaget är längst till vänster eller högst upp, det minsta värdet som draghandtaget ger.')
(getMouseX 'mus x' 'Muspekarens x-koordinat')
(getMouseY 'mus y' 'Muspekarens y-koordinat')
(getNewClone 'kopia' 'returnerar en kopia det här objektet')
(getNumberAtCursor 'tal vid markör' 'talet vid markören')
(getNumericValue 'numeriskt värde' 'Ett tal som representerar den aktuella positionen av draghandtaget.')
(getObtrudes 'sticker ut' 'ifall objektet sticker ut över behållarens kant')
(getPenColor 'pennfärg' 'färgen på bläcket i pennan')
(getPenDown 'penna nedtryckt' 'ifall pennan är nedtryckt just nu')
(getPenSize 'pennans bredd' 'pennans bredd')
(getRight 'höger' 'Den högra kanten')
(getRoundedCorners 'rundade hörn' 'Ifall hörnen skall vara runda')
(getSampleAtCursor 'värde vid markör' 'Nuvarande värde vid markörens position')
(getSaturationUnder 'mättnad under' 'Mättnaden under objektets mittpunkt')
(getScaleFactor 'skalfaktor' 'Objektets förstoringsfaktor')
(getTheta 'theta' 'Vinkeln mellan den positiva x-axeln och vektorn mellan ursprungspunkten och objektets position')
(getTop 'toppen' 'Den översta kanten')
(getTruncate 'trunkera' 'Ifall endast heltal används som värden, om inte är bråktal också tillåtna.')
(getUpDown 'upp/ner' 'Vertikal förflyttning')
(getValueAtCursor 'spelare vid markör' 'objektet vid markören')
(getViewingByIcon 'normalvy' 'vyn på innehållet är normal')
(getWidth 'bredd' 'Bredden')
(getX 'x' 'X-koordinaten')
(getY 'y' 'Y-koordinaten')
(goToFirstCardInBackground 'gå till första i bakgrunden' 'Gå till det första kortet i den nuvarande bakgrunden')
(goToFirstCardOfStack 'gå till första kortet i stacken' 'Gå till första korten i hela stacken')
(goToLastCardInBackground 'gå till sista kortet i bakgrunden' 'Gå till det sista kortet i den nuvarande bakgrunden')
(goToLastCardOfStack 'gå till sista kortet i stacken' 'Gå till det sista kortet i hela stacken')
(goToNextCardInStack 'gå till nästa kort' 'Gå till nästa kort i stacken')
(goToPreviousCardInStack 'gå till förra kortet' 'Gå till det föregående kortet i stacken')
(goToRightOf: 'align after' 'placera detta objekt till höger om ett annat')
(goto: 'gå till' 'gå till angiven sida')
(hide 'göm' 'gör objektet osynligt')
(initiatePainting 'börja målning' 'Påbörja målning av ett nytt objekt i den vanliga lekplatsen.')
(insertCard 'stoppa in kort' 'Skapa ett nytt kort')
(lastPage 'sista sidan' 'gå till sista sidan')
(liftAllPens 'lyft alla pennor' 'Lyft pennorna på mitt innehålls alla objekt.')
(loadSineWave 'ladda sinusvåg' 'Ladda en sinusvåg som nuvarande graf')
(loadSound: 'ladda ljud' 'Ladda angivet ljud som nuvarnde graf')
(lowerAllPens 'sänk alla pennor' 'Sänk pennorna på mitt innehålls alla objekt.')
(makeNewDrawingIn: 'påbörja målning i' 'gör en ny målning i angiven lekplats')
(moveToward: 'flytta mot' 'flytta mot angivet objekt')
(nextPage 'nästa sida' 'gå till nästa sida')
(pauseAll: 'pausa alla' 'pausa skriptet i objektet och alla dess kusiner')
(pauseScript: 'pausa skript' 'pausa skriptet')
(play 'spela' 'Spela nuvarande graf som ett ljud')
(previousPage 'föregående sida' 'gå till föregående sida')
(removeAll 'ta bort alla' 'Ta bort alla element från lekplatsen')
(reverse 'reversera' 'Reversera grafen')
(roundUpStrays 'samla in bortsprungna' 'Samla in alla delar utanför behållaren så att de blir synliga igen.')
(seesColor: 'är över färg' 'ifall någon del av objektet är över den givna färgen')
(show 'visa' 'gör objektet synligt')
(shuffleContents 'blanda innehåll' 'Blanda lekplatsens innehåll')
(stampAndErase 'stämpla och försvinn' 'lägg till min bild som ritstreck och försvinn')
(startAll: 'starta alla' 'starta skriptet tickande i objektet och alla dess kusiner.')
(startScript: 'starta skript' 'starta skriptet tickande')
(stopAll: 'stoppa alla' 'gör skriptet "normalt" i objektet och alla dess kusiner')
(stopScript: 'stoppa skript' 'gör skriptet "normalt" i objektet')
(tellAllSiblings: 'säg till kusiner' 'skicka ett meddelande till alla kusiner')
(touchesA: 'vidrör' 'Ifall jag vidrör något som ser ut som...')
(turn: 'sväng med' 'Ändra objektets riktning med angiven mängd')
(unhideHiddenObjects 'visa alla gömda objekt' 'Gör alla gömda objekt synliga.')
(wearCostumeOf: 'se ut som' 'bär samma dräkt som...')
(wrap 'växla över kant' 'växla över kanten ifall det är lämpligt')) language: #Svenska.

	self translateCategories: #(
(basic					'grunder'				'ett antal viktiga saker')
(#'book navigation'		'boknavigering'			'saker som har att göra med böcker och stackar med mera')
(button					'knapp'					'objektet betraktat som en knapp man kan trycka på')
(collections				'samlingar'				'objektet betraktat som en samling av andra objekt')
(fog					'dimma'					'3-dimensionell dimma')
(geometry				'geometri' 				'mätvärden och koordinater')
(#'color & border'		'färger & kanter'		'saker som har att göra med färger och kanter på objekt')
(graphics				'grafik'					'objektet betraktat som en bild')
(variables				'instansvariabler'		'instansvariabler tillagda i detta objekt')
(joystick				'joystick	'				'objektet som en joystick')
(miscellaneous			'diverse' 				'diverse kommandon')
(motion					'rörelse' 				'saker som har att göra med förflyttning och vridning')
(paintbox				'målarlåda'				'målarpaletten')
(#'pen trails'			'pennstreck'				'saker som har att göra med streck som pennan ritar')
(#'pen use'				'pennanvändning' 		'användning av ett objekts penna')
(playfield				'lekplats'					'objektet som behållare för andra synliga objekt')
(sampling				'provtagning'			'provtagning')
(scripts					'skript'					'metoder tillagda i detta objekt')
(slider					'draghandtag'			'användbara funktioner för draghandtag')
(speaker				'högtalare'				'objektet betraktat som en högtalare')
(#'stack navigation'		'stacknavigering'		'navigering inom en stack')
(storyboard				'storyboard'				'storyboard')
(tests					'tester'					'ja/nej tester, för användning skriptens testytor')
(text					'text'					'Objektet som text')
(viewing				'vyer'					'Saker som har att göra med olika vyer')
(vector					'vektor'					'Objektet som en vektor')
 ) language: #Svenska.

	self addToTranslationTableFrom: #(
(:						'_'						'tilldela värde')
(Incr:					'öka med'			'öka värde med')
(Decr:					'minska med'			'minska värde med')
(Mult:					'multiplicera med'			'multiplicera värde med')) language: #Svenska
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 2/26/2003 23:35'!
templateForLanguageTranslation
	
"Edit this method such that the second element of each triplet has the translated wording and the third element has the translated help-message; give the edited method a name of the form #addLangVocabulary, and be sure to change the language name in the three places that it occurs, as #YourLanguage, below.

A complete translation consists, as in #addKiswahiliVocabulary, of calls to three methods, namely:
	translateMethodInterfaceWordings:language: 
	translateCategories:language:
	addToTranslationTableFrom:language: 

After editing this method into the one that holds your language translations, the next step is to edit #assureTranslationsAvailableFor: so that it calls the method you just created when appropriate.   Consult #addKiswahiliVocabulary and its sender for a complete example to emulate."

	self translateMethodInterfaceWordings: #(
(append: 'include at end' 'Add the object to my content, placing it after all the other objects currently within me.')
(beep: 'make sound' 'Make the specified sound')
(bounce: 'bounce' 'bounce off the edge if hit')
(cameraPoint #cameraPoint 'the camera point')
(clear 'clear' 'Clear the graph of current contents')
(clearOwnersPenTrails 'clear all pen trails' 'clear all pen trails in my containing playfield')
(clearTurtleTrails 'clear pen trails' 'Clear all the pen trails in the interior.')
(color:sees: 'color  sees' 'whether the given color sees the given color')
(deleteCard 'deleteCard' 'Delete the current card')
(doMenuItem: 'do menu item' 'do the menu item')
(doScript: 'do' 'run the given script once, on the next tick')
(emptyScript 'emptyScript' 'an empty script')
(fire 'fire' 'trigger any and all of this object''s button actions')
(firstPage 'firstPage' 'go to first page')
(followPath 'followPath' 'follow the yellow brick road')
(forward: 'forward by' 'Moves the object forward in the direction it is heading')
(getActWhen #actWhen 'When the script should fire')
(getAllButFirstCharacter #allButFirst 'All my characters except the first one')
(getAmount #amount 'The amount of displacement')
(getAngle #angle 'The angular displacement')
(getBorderColor #borderColor 'The color of the object''s border')
(getBorderWidth #borderWidth 'The width of the object''s border')
(getBottom #bottom 'The bottom edge')
(getBrightnessUnder #brightnessUnder 'The brightness under the center of the object')
(getCharacters #characters 'The characters in my contents')
(getColor #color 'The color of the object')
(getColorUnder #colorUnder 'The color under the center of the object')
(getConePosition #conePosition 'the position of the speaker cone')
(getCursor #cursor 'The current cursor location, wrapped back to the beginning if appropriate')
(getDescending #descending 'Tells whether the smallest value is at the top/left (descending = false) or at the bottom/right (descending = true)')
(getDistance #distance 'The length of the vector connecting the origin to the object''s position')
(getFirstCharacter #firstCharacter 'The first character in my contents')
(getFirstElement #firstElement 'The first object in my contents')
(getFogColor #fogColor 'The color of fog being applied')
(getFogDensity #fogDensity 'The density of fog being applied')
(getFogRangeEnd #fogRangeEnd 'The range start of fog being applied')
(getFogRangeStart #fogRangeStart 'The range start of fog being applied')
(getFogType #fogType 'The type of fog being applied')
(getGraphic #graphic 'The picture currently being worn')
(getGraphicAtCursor #graphicAtCursor 'the graphic worn by the object at the cursor')
(getHeading #heading 'Which direction the object is facing.  0 is straight up')
(getHeight #height 'The height')
(getHolder #holder 'the object''s container')
(getIndexInOwner #elementNumber 'my index in my container')
(getIsUnderMouse #isUnderMouse 'whether the object is under the current mouse position')
(getKnobColor #knobColor 'The color of the slider')
(getLabel #label 'The wording on the button')
(getLastValue #lastValue 'The last value obtained')
(getLeft #left 'The left edge')
(getLeftRight #leftRight 'The horizontal displacement')
(getLuminanceUnder #luminanceUnder 'The luminance under the center of the object')
(getMaxVal #maxVal 'The number represented when the knob is at the right or bottom of the slider; the largest value returned by the slider.')
(getMinVal #minVal 'The number represented when the knob is at the left or top of the slider; the smallest value returned by the slider.')
(getMouseX #mouseX 'The x coordinate of the mouse pointer')
(getMouseY #mouseY 'The y coordinate of the mouse pointer')
(getNewClone #copy 'returns a copy of this object')
(getNumberAtCursor #numberAtCursor 'the number at the cursor')
(getNumericValue #numericValue 'A number representing the current position of the knob.')
(getObtrudes #obtrudes 'whether the object sticks out over its container''s edge')
(getPenColor #penColor 'the color of ink used by the pen')
(getPenDown #penDown 'whether the pen is currently down')
(getPenSize #penSize 'the width of the pen')
(getRight #right 'The right edge')
(getRoundedCorners #roundedCorners 'Whether corners should be rounded')
(getSampleAtCursor #sampleAtCursor 'The sample value at the current cursor location')
(getSaturationUnder #saturationUnder 'The saturation under the center of the object')
(getScaleFactor #scaleFactor 'The factor by which the object is magnified')
(getTheta #theta 'The angle between the positive x-axis and the vector connecting the origin to the object''s position')
(getTop #top 'The top edge')
(getTruncate #truncate 'If true, only whole numbers are used as values; if false, fractional values are allowed.')
(getUpDown #upDown 'The vertical displacement')
(getValueAtCursor #playerAtCursor 'the object currently at the cursor')
(getViewingByIcon #viewingNormally 'whether contents are viewed normally')
(getWidth #width 'The width')
(getX #x 'The x coordinate')
(getY #y 'The y coordinate')
(goToFirstCardInBackground 'goToFirstCardInBackground' 'Go to the first card of the current background')
(goToFirstCardOfStack 'goToFirstCardOfStack' 'Go to the first card of the entire stack')
(goToLastCardInBackground 'goToLastCardInBackground' 'Go to the last card of the current background')
(goToLastCardOfStack 'goToLastCardOfStack' 'Go to the last card of the entire stack')
(goToNextCardInStack 'goToNextCardInStack' 'Go to the next card')
(goToPreviousCardInStack 'goToPreviousCardInStack' 'Go to the previous card')
(goToRightOf: 'align after' 'place this object to the right of another')
(goto: 'goto:' 'go to the given page')
(hide 'hide' 'make the object invisible')
(initiatePainting 'initiatePainting' 'Initiate painting of a new object in the standard playfield.')
(insertCard 'insertCard' 'Create a new card')
(lastPage 'lastPage' 'go to last page')
(liftAllPens 'lift all pens' 'Lift the pens on all the objects in my interior.')
(loadSineWave 'loadSineWave' 'Load a sine wave as the current graph')
(loadSound: 'loadSound:' 'Load the specified sound into the current graph')
(lowerAllPens 'lower all pens' 'Lower the pens on all the objects in my interior.')
(makeNewDrawingIn: 'start painting in' 'make a new drawing in the specified playfield')
(moveToward: 'move toward' 'move toward the given object')
(nextPage 'nextPage' 'go to next page')
(pauseAll: 'pause all' 'make the given script be "paused" in the object and all of its siblings')
(pauseScript: 'pause script' 'make the given script be "paused"')
(play 'play' 'Play the current graph as a sound')
(prepend: 'include at beginning' 'Add the object to my content, placing it before all the other objects currently within me.')
(previousPage 'previousPage' 'go to previous page')
(removeAll 'removeAll' 'Remove all elements from the playfield')
(reverse 'reverse' 'Reverse the graph')
(roundUpStrays 'roundUpStrays' 'Bring all out-of-container subparts back into view.')
(seesColor: #isOverColor 'whether any part of the object is over the given color')
(show 'show' 'make the object visible')
(shuffleContents 'shuffleContents' 'Shuffle the contents of the playfield')
(stampAndErase 'stampAndErase' 'add my image to the pen trails and go away')
(startAll: 'start All' 'start the given script ticking in the object and all of its siblings.')
(startScript: 'start script' 'start the given script ticking')
(stopAll: 'stop all' 'make the given script be "normal" in the object and all of its siblings')
(stopScript: 'stop script' 'make the given script be "normal"')
(tellAllSiblings: 'tell all siblings' 'send a message to all siblings')
(touchesA: #touchesA 'whether I touch something that looks like...')
(turn: 'turn by' 'Change the heading of the object by the specified amount')
(unhideHiddenObjects 'unhideHiddenObjects' 'Unhide all hidden objects.')
(wearCostumeOf: 'look like' 'wear the costume of...')
(wrap 'wrap' 'wrap off the edge if appropriate')) language: #YourLanguage.

	self translateCategories: #(
(basic					'basic'					'a few important things')
(#'book navigation'		'book navigation'		'relating to book, stacks, etc')
(button					'button'					'for thinking of this object as a push-button control')
(collections				'collections'				'for thinking of this object as a collection')
(fog					'fog'					'3D fog')
(geometry				'geometry' 				'measurements and coordinates')
(#'color & border'		'color & border'			'matters concerning the colors and borders of objects')
(graphics				'graphics'				'for thinking of this object as a picture')
(variables				'variables'				'variables added by this object')
(joystick				'joystick	'				'the object as a Joystick')
(miscellaneous			'miscellaneous' 			'various commands')
(scripting				'scripting'				'commands to start and stop scripts, etc.')
(motion					'motion' 				'matters relating to moving and turning')
(paintbox				'paintbox'				'the painting palette')
(#'pen trails'			'pen trails'				'relating to trails put down by pens')
(#'pen use'				'pen use' 				'use of an object''s "pen"')
(playfield				'playfield'				'the object as a container for other visible objects')
(sampling				'sampling'				'sampling')
(scripts					'scripts'					'methods added by this object')
(slider					'slider'					'functions useful to sliders')
(speaker				'speaker'				'the object as an audio Speaker')
(#'stack navigation'		'stack navigation'		'navigation within a stck')
(storyboard				'storyboard'				'storyboard')
(tests					'tests'					'yes/no tests, to use in "Test" panes of scripts')
(text					'text'					'The object as text')
(viewing				'viewing'				'matters relating to viewing')
(vector					'vector'					'The object as a vector')
 ) language: #YourLanguage.

	self addToTranslationTableFrom: #(
(:						'_'						'assign value')
(Incr:					'increase by'			'increase value by')
(Decr:					'decrease by'			'decrease value by')
(Mult:					'multiply by'			'multiply value by')) language: #YourLanguage
! !


!Morph class methodsFor: 'scripting' stamp: 'sw 2/26/2003 23:35'!
helpContributions
	"Answer a list of pairs of the form (<symbol> <help message> ) to contribute to the system help dictionary"
	
"NB: Many of the items here are not needed any more since they're specified as part of command definitions now.  Someone needs to take the time to go through the list and remove items no longer needed.  But who's got that kind of time?"

	^ #(
		(acceptScript:for:
			'submit the contents of the given script editor as the code defining the given selector')
		(actorState
			'return the ActorState object for the receiver, creating it if necessary')
		(addInstanceVariable
			'start the interaction for adding a new variable to the object')
		(addPlayerMenuItemsTo:hand:
			'add player-specific menu items to the given menu, on behalf of the given hand.  At present, these are only commands relating to the turtle')
		(addYesNoToHand
			'Press here to tear off a  TEST/YES/NO unit which you can drop into your script')
		(allScriptEditors
			'answer a list off the extant ScriptEditors for the receiver')
		(amount
			'The amount of displacement')
		(angle	
			'The angular displacement')
		(anonymousScriptEditorFor:
			'answer a new ScriptEditor object to serve as the place for scripting an anonymous (unnamed, unsaved) script for the receiver')
		(append:
			'add an object to this container')
		(prepend:
			'add an object to this container')
		(assignDecrGetter:setter:amt:
			'evaluate the decrement variant of assignment')
		(assignGetter:setter:amt:
			'evaluate the vanilla variant of assignment')
		(assignIncrGetter:setter:amt:
			'evalute the increment version of assignment')
		(assignMultGetter:setter:amt:
			'evaluate the multiplicative version of assignment')
		(assureEventHandlerRepresentsStatus
			'make certain that the event handler associated with my current costume is set up to conform to my current script-status')
		(assureExternalName
			'If I do not currently have an external name assigned, get one now')
		(assureUniClass
			'make certain that I am a member a uniclass (i.e. a unique subclass); if I am not, create one now and become me into an instance of it')
		(availableCostumeNames
			'answer a list of strings representing the names of all costumes currently available for me')
		(availableCostumesForArrows
			'answer a list of actual, instantiated costumes for me, which can be cycled through as the user hits a next-costume or previous-costume button in a viewer')
		(beep:
			'make the specified sound')
		(borderColor
			'The color of the object''s border')
		(borderWidth
			'The width of the object''s border')
		(bottom
			'My bottom edge, measured downward from the top edge of the world')
		(bounce:
			'If object strayed beyond the boundaries of its container, make it reflect back into it, making the specified noise while doing so.')
		(bounce
			'If object strayed beyond the boundaries of its container, make it reflect back into it')
		(chooseTrigger
'When this script should run.
"normal" means "only when called"')
		(clearTurtleTrails
			'Clear all the pen trails in the interior.')
		(clearOwnersPenTrails
			'Clear all the pen trails in my container.')
		(color	
			'The object''s interior color')
		(colorSees
			'Whether a given color in the object is over another given color')
		(colorUnder
			'The color under the center of the object')
		(copy
			'Return a new object that is very much like this one')
		(cursor	
			'The index of the chosen element')
		(deleteCard
			'Delete the current card.')
		(dismiss
			'Click here to dismiss me')
		(doMenuItem:
			'Do a menu item, the same way as if it were chosen manually')
		(doScript:
			'Perform the given script once, on the next tick.')
		(elementNumber
			'My element number as seen by my owner')
		(fire
			'Run any and all button-firing scripts of this object')
		(firstPage
			'Go to first page of book')
		(followPath
				'Retrace the path the object has memorized, if any.')
		(forward:
			'Moves the object forward in the direction it is heading') 
		(goto:
			'Go to the specfied book page')
		(goToNextCardInStack
			'Go to the next card')
		(goToPreviousCardInStack
			'Go to the previous card.')
		(goToRightOf:
			'Align the object just to the right of any specified object.')
		(heading
			'Which direction the object is facing.  0 is straight up') 
		(height	
			'The distance between the top and bottom edges of the object')
		(hide
			'Make the object so that it does not display and cannot handle input')
		(initiatePainting	
			'Initiate painting of a new object in the standard playfield.')
		(initiatePaintingIn:
			'Initiate painting of a new object in the given place.')
		(isOverColor
			'Whether any part of this object is directly over the specified color')
		(isUnderMouse
			'Whether any part of this object is beneath the current mouse-cursor position')
		(lastPage
			'Go to the last page of the book.')
		(left
			'My left edge, measured from the left edge of the World')
		(leftRight
			'The horizontal displacement')
		(liftAllPens
			'Lift the pens on all the objects in my interior.')
		(lowerAllPens
			'Lower the pens on all the objects in my interior.')
		(mouseX
			'The x coordinate of the mouse pointer')
		(mouseY
			'The y coordinate of the mouse pointer')
		(moveToward:
			'Move in the direction of another object.')
		(insertCard
			'Create a new card.')
		(nextPage
			'Go to next page.')
		(numberAtCursor
			'The number held by the object at the chosen element')
		(objectNameInHalo
			'Object''s name -- To change: click here, edit, hit ENTER')
		(obtrudes
			'Whether any part of the object sticks out beyond its container''s borders')
		(offerScriptorMenu
			'The Scriptee.
Press here to get a menu')
		(pauseScript:
			'Make a running script become paused.')
		(penDown
			'Whether the object''s pen is down (true) or up (false)')
		(penColor
			'The color of the object''s pen')
		(penSize	
			'The size of the object''s pen')
		(clearPenTrails
			'Clear all pen trails in the current playfield')
		(playerSeeingColorPhrase
			'The player who "sees" a given color')
		(previousPage
			'Go to previous page')

		(show
			'If object was hidden, make it show itself again.')
		(startScript:
			'Make a script start running.')
		(stopScript:
			'Make a script stop running.')
		(top
			'My top edge, measured downward from the top edge of the world')
		(right
			'My right edge, measured from the left edge of the world')
		(roundUpStrays
			'Bring all out-of-container subparts back into view.')
		(scaleFactor
			'The amount by which the object is scaled')
		(stopScript:
			'make the specified script stop running')
		(tellAllSiblings:
			'send a message to all of my sibling instances')
		(try
			'Run this command once.')
		(tryMe
			'Click here to run this script once; hold button down to run repeatedly')
		(turn:				
			'Change the heading of the object by the specified amount')
		(unhideHiddenObjects
			'Unhide all hidden objects.')
		(upDown
			'The vertical displacement')
		(userScript
			'This is a script defined by you.')
		(userSlot
			'This is a variable defined by you.  Click here to change its type')
		(valueAtCursor
			'The chosen element')
		(wearCostumeOf:
			'Wear the same kind of costume as the other object')
		(width	
			'The distance between the left and right edges of the object')
		(wrap
			'If object has strayed beond the boundaries of its container, make it reappear from the opposite edge.')
		(x
			'The x coordinate, measured from the left of the container')
		(y
			'The y-coordinate, measured upward from the bottom of the container')

		)
! !


!Player methodsFor: 'misc' stamp: 'sw 3/3/2004 00:21'!
offerViewerMenuFor: aViewer event: evt
	"Put up the Viewer menu on behalf of the receiver.  If the shift key is held down, put up the alternate menu. The menu omits the 'add a new variable' item when in eToyFriendly mode, as per request from teachers using Squeakland in 2003 once the button for adding a new variable was added to the viewer"

	| aMenu aWorld  |
	(evt notNil and: [evt shiftPressed]) ifTrue:
		[^ self offerAlternateViewerMenuFor: aViewer event: evt].

	aWorld _ aViewer world.
	aMenu _ MenuMorph new defaultTarget: self.
	Preferences eToyFriendly ifFalse: "exclude this from squeakland-like UI "
		[aMenu add: 'add a new variable' translated target: self action: #addInstanceVariable.
		aMenu balloonTextForLastItem: 'Add a new variable to this object and all of its siblings.  You will be asked to supply a name for it.' translated].

	aMenu add: 'add a new script' translated target: aViewer action: #newPermanentScript.
	aMenu balloonTextForLastItem: 'Add a new script that will work for this object and all of its siblings' translated.
	aMenu addLine.
	aMenu add: 'grab me' translated target: self selector: #grabPlayerIn: argument: aWorld.
	aMenu balloonTextForLastItem: 'This will actually pick up the object this Viewer is looking at, and hand it to you.  Click the (left) button to drop it' translated.

	aMenu add: 'reveal me' translated target: self selector: #revealPlayerIn: argument: aWorld.
	aMenu balloonTextForLastItem: 'If you have misplaced the object that this Viewer is looking at, use this item to (try to) make it visible' translated.

	aMenu addLine.
	aMenu add: 'tile representing me' translated action: #tearOffTileForSelf.
	aMenu add: 'add search pane' translated target: aViewer action: #addSearchPane.
	aMenu addLine.
	aMenu add: 'more...' translated target: self selector: #offerAlternateViewerMenuFor:event: argumentList: {aViewer. evt}.

	aMenu popUpEvent: evt in: aWorld
! !

!Player methodsFor: 'slots-kernel' stamp: 'sw 3/3/2004 23:58'!
categories
	"Answer a list of categories appropriate to the the receiver and its costumes"

	| aList |
	(self hasCostumeThatIsAWorld)
		ifTrue:	[^ self categoriesForWorld].

	aList _ OrderedCollection new.
	self slotNames notEmpty ifTrue:
		[aList add: ScriptingSystem nameForInstanceVariablesCategory].
	aList addAll: costume categoriesForViewer.
	aList remove: ScriptingSystem nameForScriptsCategory ifAbsent: [].
	aList add: ScriptingSystem nameForScriptsCategory after: aList first.
	^ aList! !

!Player methodsFor: 'slots-kernel' stamp: 'sw 3/3/2004 00:00'!
categoriesForVocabulary: aVocabulary
	"Answer a list of categories appropriate to the receiver and its costumes, in the given Vocabulary"

	| aList |
	self hasCostumeThatIsAWorld
		ifTrue:
			[aList _ self categoriesForWorld]
		ifFalse:
			[aList _ OrderedCollection new.
			self slotNames ifNotEmpty:
				[aList add: ScriptingSystem nameForInstanceVariablesCategory].
			aList addAll: costume categoriesForViewer].
	aVocabulary addCustomCategoriesTo: aList.
	aList remove: ScriptingSystem nameForScriptsCategory ifAbsent: [].
	aList add: ScriptingSystem nameForScriptsCategory after: aList first.
	^ aList! !

!Player methodsFor: 'slots-kernel' stamp: 'sw 3/3/2004 00:01'!
categoriesForWorld
	"Answer the list of categories given that the receiver is the Player representing a World"

	| aList |
	aList _ #(#'color & border' #'pen trails' playfield collections #'stack navigation') asOrderedCollection.
	self class scripts ifNotEmpty:
		[aList addFirst: ScriptingSystem nameForScriptsCategory].
	self slotNames ifNotEmpty:
		[aList addFirst: ScriptingSystem nameForInstanceVariablesCategory].

	^ aList! !

!Player methodsFor: 'slots-kernel' stamp: 'sw 2/26/2003 23:31'!
methodInterfacesForInstanceVariablesCategoryIn: aVocabulary
	"Return a collection of methodInterfaces for the instance-variables category.  The vocabulary parameter, at present anyway, is not used."

	| aList anInterface itsSlotName |
	aList _ OrderedCollection new.
	self slotInfo associationsDo:
		[:assoc |
			anInterface _ MethodInterface new.
			itsSlotName _ assoc key.
			anInterface absorbTranslation: 
				(ElementTranslation new wording: itsSlotName helpMessage: 'a variable defined by this object' language: #English).

			anInterface selector: (Utilities getterSelectorFor: itsSlotName) type: assoc value type setter: (Utilities setterSelectorFor: itsSlotName).
			anInterface setToRefetch.
			aList add: anInterface].
	^ aList! !

!Player methodsFor: 'slots-user' stamp: 'sw 3/3/2004 23:56'!
addInstanceVariable
	"Offer the user the opportunity to add an instance variable, and if he goes through with it, actually add it"

	| itsName initialValue typeChosen suggestedNames usedNames initialAnswer setterSelector originalString |
	suggestedNames _ #('cargo' 'speed' 'weight' 'mzee' 'friml' 'verp' 'znak').
	usedNames _ self class instVarNames.
	initialAnswer _ suggestedNames detect: [:aName |  (usedNames includes: aName) not] ifNone:
		[Utilities keyLike: 'var1'  satisfying: [:aKey | (usedNames includes: aKey) not]].

	originalString _ FillInTheBlank request: 'name for new variable: ' translated initialAnswer: initialAnswer.
	originalString isEmptyOrNil ifTrue: [^ self].
	itsName _ ScriptingSystem acceptableSlotNameFrom: originalString forSlotCurrentlyNamed: nil asSlotNameIn: self world: self costume world.

 	itsName isEmpty ifTrue: [^ self].	
	self assureUniClass.
	typeChosen _ self initialTypeForSlotNamed: itsName.
	self slotInfo at: itsName put: (SlotInformation new initialize type: typeChosen).
	initialValue _ self initialValueForSlotOfType: typeChosen.
	self addInstanceVarNamed: itsName withValue: initialValue.
	self class compileAccessorsFor: itsName.
	setterSelector _ Utilities setterSelectorFor: itsName.
	(self class allSubInstances copyWithout: self) do:
		[:anInstance | anInstance perform: setterSelector with: initialValue].
	self updateAllViewersAndForceToShow: ScriptingSystem nameForInstanceVariablesCategory! !

!Player methodsFor: 'slots-user' stamp: 'sw 2/6/2003 18:04'!
addInstanceVariableNamed: nameSymbol type: typeChosen value: aValue
	"Add an instance variable of the given name and type, and initialize it to have the given value"

	| initialValue setterSelector |
	self assureUniClass.
	self slotInfo at: nameSymbol put: (SlotInformation new initialize type: typeChosen).
	initialValue _ self initialValueForSlotOfType: typeChosen.
	self addInstanceVarNamed: nameSymbol withValue: aValue.
	self class compileAccessorsFor: nameSymbol.
	setterSelector _ Utilities setterSelectorFor: nameSymbol.
	(self class allSubInstances copyWithout: self) do:
		[:anInstance | anInstance perform: setterSelector with: initialValue].
	self updateAllViewersAndForceToShow: ScriptingSystem nameForInstanceVariablesCategory
! !

!Player methodsFor: 'slots-user' stamp: 'sw 3/3/2004 00:23'!
renameSlot: oldSlotName newSlotName: newSlotName
	"Give an existing instance variable a new name"

	self class renameSilentlyInstVar: oldSlotName to: newSlotName.
	self updateAllViewers.
	self flag: #deferred.
	"Any scripts that formerly sent oldSlotName should now send newSlotName"

	self inform: 
'Caution!!  Any scripts that may reference this
variable by its old name may now be
broken -- you may need to fix them up manually.  
In some future release, we''ll automatically
fix those up, hopefully.' translated.

	self costume world presenter allExtantPlayers do:
		[:aPlayer | (aPlayer hasScriptReferencing: oldSlotName ofPlayer: self)
			ifTrue:
				[^ aPlayer noteRenameOf: oldSlotName to: newSlotName inPlayer: self]].
	^ true! !


!SlotInformation methodsFor: 'access' stamp: 'sw 2/26/2003 23:42'!
documentation
	"Answer the receiver's documentation"

	documentation ifNil: [documentation _ 'This is a variable defined by you.  Please edit this into your own meaningful documentation.'].
	^ documentation! !


!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 2/26/2003 22:44'!
nameForInstanceVariablesCategory
	"Answer the name to use for the viewer category that contains instance variables"

	^ #variables    
	"^ #'instance variables'"

"ScriptingSystem nameForInstanceVariablesCategory"! !

!StandardScriptingSystem methodsFor: 'utilities' stamp: 'sw 2/6/2003 18:00'!
nameForScriptsCategory
	"Answer the name to use for the viewer category that contains scripts"

	^ #scripts! !


!StandardViewer methodsFor: 'categories' stamp: 'sw 3/3/2004 00:43'!
likelyCategoryToShow
	"Choose a category to show based on what's already showing and on some predefined heuristics"

	| possible all aCat currVocab |
	all := (scriptedPlayer categoriesForViewer: self) asOrderedCollection.
	possible := all copy.
	currVocab := self currentVocabulary.
	self categoryMorphs do: 
			[:m | 
			aCat := currVocab categoryWhoseTranslatedWordingIs: m currentCategory.
			aCat ifNotNil: [possible remove: aCat categoryName ifAbsent: []]].
	(currVocab isKindOf: EToyVocabulary) 
		ifTrue: 
			["hateful!!"

			((possible includes: ScriptingSystem nameForInstanceVariablesCategory) 
				and: [scriptedPlayer hasUserDefinedSlots]) ifTrue: [^ ScriptingSystem nameForInstanceVariablesCategory].
			((possible includes: ScriptingSystem nameForScriptsCategory) and: [scriptedPlayer hasUserDefinedScripts]) 
				ifTrue: [^ ScriptingSystem nameForScriptsCategory]].
	#(#basic) 
		do: [:preferred | (possible includes: preferred) ifTrue: [^preferred]].
	((scriptedPlayer isKindOf: Player) 
		and: [scriptedPlayer hasOnlySketchCostumes]) 
			ifTrue: [(possible includes: #tests) ifTrue: [^#tests]].
	#(#'color & border' #tests #color #flagging #comparing) 
		do: [:preferred | (possible includes: preferred) ifTrue: [^preferred]].
	^possible isEmpty ifFalse: [possible first] ifTrue: [all first]! !

!StandardViewer methodsFor: 'initialization' stamp: 'sw 3/3/2004 00:56'!
addHeaderMorphWithBarHeight: anInteger includeDismissButton: aBoolean
	"Add the header morph to the receiver, using anInteger as a guide for its height, and if aBoolean is true, include a dismiss buton for it"

	| header aFont aButton aTextMorph nail wrpr costs headWrapper |
	header _ AlignmentMorph newRow color: self color muchLighter; wrapCentering: #center; cellPositioning: #leftCenter.
	aFont _ Preferences standardButtonFont.
	aBoolean ifTrue:
		[header addMorph: (aButton _ SimpleButtonMorph new label: 'O' font: aFont).
		aButton target: self;
				color:  Color tan;
				actionSelector: #dismiss;
				setBalloonText: 'remove this entire Viewer from the screen
don''t worry -- nothing will be lost!!.' translated.
		header addTransparentSpacerOfSize: 4@1].

	aButton _ IconicButton new borderWidth: 0;
			labelGraphic: (ScriptingSystem formAtKey: #AddCategoryViewer); color: Color transparent; 
			actWhen: #buttonDown;
			target: self;
			actionSelector: #addCategoryViewer;
			setBalloonText: 'click here to add
another category pane' translated;
			shedSelvedge.
	header addMorphBack: aButton.
	header addTransparentSpacerOfSize: 4@1.

	costs _ scriptedPlayer costumes.
	costs ifNotNil:
	[(costs size > 1 or: [costs size = 1 and: [costs first ~~ scriptedPlayer costume]]) ifTrue:
		[header addUpDownArrowsFor: self.
		(wrpr _ header submorphs last) submorphs second setBalloonText: 'switch to previous costume' translated.	
		wrpr submorphs first  setBalloonText: 'switch to next costume' translated]].	

	nail _ (self hasProperty: #noInteriorThumbnail)
		ifFalse:
			[ThumbnailMorph new objectToView: scriptedPlayer viewSelector: #costume]
		ifTrue:
			[ImageMorph new image: Cursor menu].
	nail on: #mouseDown send: #offerViewerMenuForEvt:morph: to: scriptedPlayer.
	header addMorphBack: nail.
	nail setBalloonText: 'click here to get a menu
that will allow you to
add a variable,
tear off a tile, etc..' translated.
	(self hasProperty: #noInteriorThumbnail)
		ifFalse:
			[nail borderWidth: 3; borderColor: #raised].

	header addTransparentSpacerOfSize: 5@5.

"	aButton _ SimpleButtonMorph new target: self; actionSelector: #newEmptyScript; label: 'S' translated font: (aFont _ StrikeFont familyName: #ComicBold size: 16);  color: Color transparent; borderWidth: 0; actWhen: #buttonDown.
	aButton setBalloonText: 'drag from here to
create a new script
for this object' translated.	
	header addMorphBack: aButton.

	header addTransparentSpacerOfSize: 8@5."
	
	aButton _ SimpleButtonMorph new target: scriptedPlayer; actionSelector: #addInstanceVariable; label: 'v' translated font: aFont;  color: Color transparent; borderWidth: 1; actWhen: #buttonUp.
	"aButton firstSubmorph color: Color gray."
	aButton setBalloonText: 'click here to add a variable
to this object.' translated.
	header addMorphBack: aButton.

	header addTransparentSpacerOfSize: 5@5.
	self viewsMorph ifTrue: [scriptedPlayer costume assureExternalName].
	aTextMorph _ UpdatingStringMorph new
		useStringFormat;
		target:  scriptedPlayer;
		getSelector: #nameForViewer;
		setNameTo: 'name';
		font: ScriptingSystem fontForNameEditingInScriptor.
	self viewsMorph ifTrue:
		[aTextMorph putSelector: #setName:.
		aTextMorph setProperty: #okToTextEdit toValue: true].
	aTextMorph step.
	header  addMorphBack: aTextMorph.
	aTextMorph setBalloonText: 'Click here to edit the player''s name.' translated.	

	header beSticky.
	anInteger > 0
		ifTrue:
			[headWrapper _ AlignmentMorph newColumn color: self color.
			headWrapper addTransparentSpacerOfSize: (0 @ anInteger).
			headWrapper addMorphBack: header.
			self addMorph: headWrapper]
		ifFalse:
			[self addMorph: header]! !

