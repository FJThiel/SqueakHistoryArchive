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

(append:					'h�nge an'				'F�gt ein Objekt in diesen Beh�lter ein')
(prepend:					'h�nge davor'				'F�gt ein Objekt in diesen Beh�lter ein')
(beep:						'mache Ger�usch'  				'Macht das angegebene Ger�usch')
(bounce:					'pralle ab'					'L��t das Objekt vom Rand seines Beh�lters abprallen und spielt das angegebene Ger�usch, wenn es au�erhalb ist')
(cameraPoint 				'Kamerapunkt'				'Der Kamerapunkt des Objektes') 
(clear 						'l�sche Graph'						'L�scht den momentanen Graphen')
(clearOwnersPenTrails		'l�sche Stiftspuren im Eigner'	'L�scht alle Stiftspuren im Eigner')
(clearTurtleTrails 			'l�sche Stiftspuren' 		'L�scht all Stiftspuren im Objekt')
(color:sees: 					'Farbe sieht'			'�berpr�ft, ob die angebene Farbe des Objektes die Testfarbe sehen kann')
(deleteCard 					'l�sche Karte'				'L�scht diese Karte aus dem Stapel')
(doMenuItem: 				'f�hre Men�punkt aus'	'F�hrt den angebenen Men�punkt aus')
(emptyScript 				'leeres Skript'				'Ein leeres Skript')
(fire						'feuer'						'F�hrt alle zugeh�rigen Aktionen dieses Schalters aus')
(firstPage 					'gehe zur ersten Seite'		'Geht zur ersten Seite')
(followPath					'folge Pfad'					'Folge dem definierten Pfad')
(forward: 					'gehe vorw�rts um'		'Bewegt das Objekt vorw�rts in seiner momentanen Richtung')
(goToFirstCardInBackground	'gehe zur ersten Karte im Hintergund'	'Geht zur ersten Karte im momentanen Hintergrund')
(goToFirstCardOfStack		'gehe zur ersten Karte im Stapel'	'Geht zur ersten Karte im Stapel')
(goToLastCardInBackground	'gehe zur letzten Karte im Hintergund'	'Geht zur letzten Karte im momentanen Hintergrund')
(goToLastCardOfStack		'gehe zur letzten Karte im Stapel'		'Geht zur letzten Karte im Stapel')
(goToNextCardInStack		'gehe zur n�chsten Karte im Stapel'		'Geht zur n�chsten Karte im momentanen Hintergrund')
(goToPreviousCardInStack	'gehe zur vorherigen Karte im Stapel'	'Geht zur vorherigen Karte im momentanen Hintergrund')
(goToRightOf:				'plaziere rechts von'		'Setzt das Objekt rechts neben ein anderes')
(goto:						'gehe zur Seite'		'Geht zur angegebenen Seite')
(hide					'verstecke Dich'					'Versteckt das Objekt')
(initiatePainting			'beginne neue Zeichnung'		'Beginnt eine neue Zeichnung')
(insertCard				'erzeuge Karte'				'Erzeugt eine neue Karte und f�gt sie ein.')
(lastPage				'gehe zur letzten Seite'			'Geht zur letzten Seite')
(liftAllPens				'nimm alle Stifte hoch'			'Nimmt alle Stifte hoch, die sich im Inneren befinden')
(loadSineWave			'lade Sinuswelle'			'L�dt eine Sinuswelle als momentanen Graph')
(loadSound:				'lade Ger�usch'					'L�dt das angegebene Ger�usch als momentanen Graph')
(lowerAllPens			'setze alle Stifte ab'			'Setzt alle Stifte ab, die sich im Inneren befinden')
(makeNewDrawingIn:	'beginne neue Zeichnung in'	'Beginnt eine neue Zeichnung im angegebenen Objekt')
(moveToward:			'gehe in Richtung '				'Bewegt das Objekt in Richtung eines anderen Objektes')
(nextPage				'gehe zur n�chsten Seite'		'Geht zur n�chsten Seite')
(pauseScript:			'stoppe Skript'					'H�lt ein Skript an')
(play					'spiele Gerausch ab'				'Spielt den momentanen Graphen als Ger�usch ab')
(previousPage			'gehe zur vorherigen Seite'	'Geht zur vorherigen Seite')
(removeAll				'entferne alles'				'Entfernt und l�scht alle Elemente')
(reverse				'umdrehen'						'Dreht den Inhalt des Graphen um')
(roundUpStrays			'hole zur�ck'					'Holt alle Objekte zur�ck, falls sie sich irgendwo verstecken')
(seesColor:				'sieht die Farbe'					'�berpr�ft, ob das Objekt die angegebene Farbe sieht')
(show					'zeige Dich'						'Zeigt das Objekt')
(shuffleContents			'mische Inhalt'					'Mischt alle Objekte zuf�llig')
(stampAndErase			'stanze und l�sche Dich'		'F�gt das Abbild des Objektes den Stiftspurent hinzu und l�scht es anschlie�end.')
(startScript:				'starte Skript'					'Beginnt die wiederholte Ausf�hrung eines Skriptes')
(stopScript:				'stoppe Script'				'Beendet die wiederholte Ausf�hrung eines Skriptes')
(tellAllSiblings:			'sage den Geschwistern'		'Sendet eine Nachricht zu allen Geschwistern des Objektes')
(touchesA:				'ber�hrt'					'�berpr�ft, ob ein Objekt des angegebenen Typs ber�hrt wird')
(turn:					'drehe Dich um'				'�ndert die Richtung des Objektes um den angegebenen Winkel')
(unhideHiddenObjects	'zeige versteckte Objekte' 		'Zeigt alle versteckten Objekte an')
(wearCostumeOf: 		'trage Kost�m von'				'Tr�gt das Kost�m eines anderen Objektes')
(wrap					'wickel Dich rum'				'Wickelt das Objekt um den Rand seines Beh�lters')

(getActWhen 			'Ausf�hrungsstatus'	'Bestimmt, wann das Skript ausgef�hrt wird')
(getAllButFirstCharacter	'Alle Buchstaben au�er dem Ersten' 'Enth�lt alle Buchstaben au�er dem Ersten')
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
(getDistance				'Distanz'				'Die Distanz zum Ursprung des Beh�lters')
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
(getHeight				'H�he'					'Die H�he des Objektes')
(getHolder				'Beh�lter'				'Der Beh�lter dieses Objekt')
(getIndexInOwner		'Eignerindex' 			'Der Index des Objektes in seinem Eigner')
"@@@: Folgendes sollte vermutlich die Hand und nicht die Maus referenzieren :@@@"
(getIsUnderMouse		'ist Maus dar�ber'		'�berpr�ft, ob die Maus �ber dem Objekt ist')
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
(getObtrudes				'ragt hinaus'			'�berpr�ft, ob das Objekt aus seinem Eigner herausragt')
(getPenColor				'Stiftfarbe'				'Die Farbe des Stiftes')
(getPenDown			'Stift unten'				'Der Status des Stiftes')
(getPenSize				'Stiftgr��e'			'Der Durchmesser des Stiftes')
(getRight				'rechte Kante'			'Die rechte Kante des Objektes')
(getRoundedCorners		'runde Ecken'			'Bestimmt, ob Ecken abgerundet werden')
(getSampleAtCursor		'Stichprobe'				'Eine Stichprobe des Wertes an der momentanen Zeigerposition')
(getSaturationUnder		'S�ttigung darunter'	'Die S�ttigung der Farbe unter dem Objekt')
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
(#'book navigation'		#'Buchnavigation'		'Navigation in B�chern')
(button					Schalter					'Das Objekt als Schalter')
(collections				Beh�lter				'Das Objekt als Beh�lter')
(fog					Nebel					'3D Nebel Eigenschaften')
(geometry				Geometrie				'Zur Geometrie des Objektes')
(#'color & border'		#'Farbe & Rand'			'Zum Thema Farben und R�nder')
(graphics				Graphik					'Graphische Eigenschaften')
(variables				Variablen				'Variablen des Objektes')
(joystick				Joystick					'Das Objekt als Joystick')
(miscellaneous			Verschiedenes			'Alles was woanders nicht hinpa�t')
(motion					Bewegung				'Bewegungseigenschaften')
(paintbox				Malpalette				'Die Zeichenpalette')
(#'pen trails'			Stiftspuren				'Alles zum Thema Spuren hinterlassen')
(#'pen use'				Stifte					'Verwendung von Stiften')
(playfield				Spielfeld				'ja ich wei� auch nicht...')
(sampling				Messen					'Messungen von Werten')
(scripts					Skripte					'Alle Deine Skripte')
(slider					Regler					'Das Objekt als Regler')
(speaker				Lautsprecher			'Das Objekt als Lautsprecher')
(#'stack navigation'		Stapelnavigation		'Navigation in Stapeln')
(storyboard				Storyboard				'Sp�ter mal...')
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
(clearOwnersPenTrails 'rens alla pennestr�k' 'Rens bort alle pennestr�k i pennens lekeplass')
(clearTurtleTrails 'rens skilpaddestr�k' 'Rens bort alle skilpaddestr�k p� innsiden')
(color:sees: 'farge syns' 'Om valgte farge ser angitt farge')
(deleteCard 'Ta bort kort' 'Ta bort dette kortet')
(doMenuItem: 'utf�r menyvalg' 'utf�r menyvalget')
(doScript: 'kj�r' 'kj�r skriptet en gang, ved nesta tick')
(emptyScript 'tomt skript' 'et tomt skript')
(fire 'starta' 'starter alle knapphendelser for dette objektet')
(firstPage 'f�rsta siden' 'g� till f�rsta siden')
(followPath 'f�lg vei' 'f�lg denne veien')
(forward: 'framover med' 'Flytter objektet framover i objektets n�v�rende rettning')
(getActWhen 'kj�r n�r' 'N�r skriptet skal kj�res')
(getAllButFirstCharacter 'alle uten om f�rsta' 'Alle mine tegn uten om det f�rste')
(getAmount 'st�rrelse' 'Forflyttningens st�rrelse')
(getAngle 'vinkel' 'Vinkelforflyttningens st�rrelse')
(getBorderColor 'kantfarge' 'Fargen p� objektets kant')
(getBorderWidth 'kantbredde' 'Bredden p� objektets kant')
(getBottom 'bunn' 'Den nederste kanten')
(getBrightnessUnder 'kontrast' 'Kontrasten under objektets mittpunkt')
(getCharacters 'tegn' 'Tegnet i mitt innehold')
(getColor 'farge' 'Objektets farge')
(getColorUnder 'farge under' 'Fargen under objektets mittpunkt')
(getConePosition 'h�ytalerposition' 'h�ytalerens position')
(getCursor 'mark�r' 'Mark�rens n�v�rende position, ombytt til f�rste om det er mulig')
(getDescending 'fallende' 'Sier om den minste verdien er �verst / til venstre (fallende = false) eller nederst / til h�yre (fallande = true)')
(getDistance 'avstand' 'Lengden p� vektoren mellom utgangspunktet og objektets posisjon')
(getFirstCharacter 'f�rste tegnet' 'Det f�rste tegnet i mitt innehold')
(getFirstElement 'f�rsta elementet' 'Det f�rsta objektet i mitt innehold')
(getFogColor 't�kens farge' 'Fargen p� t�ken som benyttes')
(getFogDensity 't�kens tetthet' 'Tettheten p� t�ken som benyttes')
(getFogRangeEnd 't�kens intervalslutt' 'Intervalets slutt p� t�ken som benyttes')
(getFogRangeStart 't�kens intervalstart' 'Intervalets start p� t�ken som benyttes')
(getFogType 't�kens typ' 'Typen av t�ke som benyttes')
(getGraphic 'bilde' 'Bildet som b�res av objektet')
(getGraphicAtCursor 'bilde ved mark�r' 'Bildet som b�res av objektet ved mark�ren')
(getHeading 'rettning' 'I vilken rettning objektet peker. 0 er rett opp')
(getHeight 'h�yde' 'H�yden')
(getHolder 'beholder' 'objektets beholder')
(getIndexInOwner 'elementnummer' 'mitt indeksnummer i min beholdere')
(getIsUnderMouse 'er under muspekeren' 'om objektet befinner seg under muspekeren')
(getKnobColor 'h�ndtakets farge' 'H�ndtakets farge')
(getLabel 'etikett' 'Teksten p� knappen')
(getLastValue 'seneste verdi' 'Seneste beregnede verdi')
(getLeft 'venstre' 'Den venstre kanten')
(getLeftRight 'venstre/h�yre' 'Horisontel forflyttning')
(getLuminanceUnder 'lysstyrke under' 'Lysstyrken under objektets mittpunkt')
(getMaxVal 'maxverdi' 'Tallet som representerer n�r h�ndtaket er lengst til h�yre eller lengst ned, den st�rste verdien som h�ndtaket gir.')
(getMinVal 'minverdi' 'Tallet som representerer n�r h�ndtaket er lengst til venstre eller h�yest opp, den minste verdien som h�ndtaket gir.')
(getMouseX 'mus x' 'Muspekerens x-koordinat')
(getMouseY 'mus y' 'Muspekerens y-koordinat')
(getNewClone 'kopia' 'returnerer en kopi av det her objektet')
(getNumberAtCursor 'tall ved mark�r' 'tallet ved mark�ren')
(getNumericValue 'numeriskt verdi' 'Et tall som representerer den aktuelle posisjonen av h�ndtaket.')
(getObtrudes 'stikker ut' 'om objektet stikker ut over beholderens kant')
(getPenColor 'pennefarge' 'fargen p� blekket i pennen')
(getPenDown 'pen nedtrykt' 'om pennen er nedtrykt n�')
(getPenSize 'pennens bredde' 'pennens bredde')
(getRight 'h�yre' 'Den h�yre kanten')
(getRoundedCorners 'rundede hj�rner' 'om hj�rnene skal v�re runde')
(getSampleAtCursor 'verdi ved mark�r' 'N�v�rende verdi ved mark�rens posisjon')
(getSaturationUnder 'mettning under' 'Fargemettning under objektets mittpunkt')
(getScaleFactor 'skala' 'Objektets skala')
(getTheta 'theta' 'Vinkelen mellom den positive x-axelen og vektoren mellom utgangspunktet og objektets posisjon')
(getTop 'toppen' 'Den �verste kanten')
(getTruncate 'heltall' 'Om bare heltall anvendes som verdi, om br�ktal ikke er tillatt.')
(getUpDown 'opp/ner' 'Vertikal forflyttning')
(getValueAtCursor 'spiller ved mark�r' 'objektet ved mark�ren')
(getViewingByIcon 'normalt synssett' 'Synsettet p� inneholdet er normal')
(getWidth 'bredd' 'Bredden')
(getX 'x' 'X-koordinaten')
(getY 'y' 'Y-koordinaten')
(goToFirstCardInBackground 'g� til f�rste i bakgrunnen' 'G� til det f�rste kortet i den n�v�rende bakgrunnen')
(goToFirstCardOfStack 'g� til f�rste kortet i stacken' 'G� til f�rste korten i hele stacken')
(goToLastCardInBackground 'g� til siste kortet i bakgrunnen' 'G� til det siste kortet i den n�v�rende bakgrunnen')
(goToLastCardOfStack 'g� til siste kortet i stacken' 'G� til det siste kortet i hele stacken')
(goToNextCardInStack 'g� till neste kort' 'G� til neste kort i stacken')
(goToPreviousCardInStack 'g� til forrige kort' 'G� til det forrige kortet i stacken')
(goToRightOf: 'plassere etter' 'plassere dette objekt til h�yre om et annet')
(goto: 'g� til' 'g� til angitt side')
(hide 'gjem' 'gj�r objektet usynligt')
(initiatePainting 'begynn malning' 'Begynn malning av et nytt objekt i den vanlige lekeplatsen.')
(insertCard 'legg inn kort' 'Skap et nytt kort')
(lastPage 'siste siden' 'g� til siste siden')
(liftAllPens 'l�ft alle penner' 'L�ft pennene p� mitt inneholds alle objekt.')
(loadSineWave '�pne sinusb�lge' '�pne en sinusb�lge som n�v�rende graf')
(loadSound: '�pne lyd' '�pne angivet lyd som n�v�rende lyd')
(lowerAllPens 'senk alle penner' 'Senk pennene p� mitt inneholds alle objekt.')
(makeNewDrawingIn: 'bgynn malning i' 'lag en ny malning i angiven lekeplats')
(moveToward: 'flytte mot' 'flytte mot angivet objekt')
(nextPage 'nesta side' 'g� til neste side')
(pauseAll: 'pause alle' 'pause skriptet i objektet och alle dets slekninger')
(pauseScript: 'pause skript' 'pause skriptet')
(play 'spela' 'Spill n�v�rende graf som en lyd')
(previousPage 'foreg�ende side' 'g� til foreg�ende side')
(removeAll 'ta bort alle' 'Ta bort alle element fra lekeplatsen')
(reverse 'reversere' 'Reversere grafen')
(roundUpStrays 'samle inn bortsprungne' 'Samle inn alle deler utenfor beholderen s� at de blir synlige igen.')
(seesColor: 'er over farge' 'om noen del av objektet er over den angitte fargen')
(show 'vise' 'gj�r objektet synlig')
(shuffleContents 'blande innehold' 'Blande lekeplatsens innehold')
(stampAndErase 'stemple og forsvinn' 'legg til mitt bilde som tegnestrek och forsvinn')
(startAll: 'start alle' 'start skriptet tickende i objektet och alla dets slekninger.')
(startScript: 'start skript' 'start skriptet tickende')
(stopAll: 'stopp alle' 'gj�r skriptet "normalt" i objektet och alla dets slektninger')
(stopScript: 'stopp skript' 'gj�r skriptet "normalt" i objektet')
(tellAllSiblings: 'si til slekninger' 'send et meddelende til alla slekninger')
(touchesA: 'r�rer' 'Om jeg r�rer noe som ser ut som...')
(turn: 'sving med' 'Endre objektets retning med angitt mengde')
(unhideHiddenObjects 'vise alle gjemte objekt' 'Gj�r alle gjemte objekt synlige.')
(wearCostumeOf: 'se ut som' 'b�r samme drakt som...')
(wrap 'fold over kant' 'fold over kanten om det er passende')) language: #Norsk.

	self translateCategories: #(
(basic					'element�re'				'et antall viktige saker')
(#'book navigation'		'boknavigering'			'saker som har med b�ker och stackar � gj�re')
(button					'knapp'					'objektet betraktet som en knapp man kan trykke p�')
(collections				'samlinger'				'objektet betraktet som en samling av andre objekt')
(fog					't�ke'					'3-dimensionell t�ke')
(geometry				'geometri' 				'verdier og koordinater')
(#'color & border'		'farger & kanter'		'saker som har med farger og kanter p� objekt � gj�re')
(graphics				'grafikk'					'objektet betraktat som et bilde')
(variables				'instansvariabler'		'instansvariabler tilh�rende dette objektet')
(joystick				'joystick	'				'objektet som en joystick')
(miscellaneous			'diverse' 				'diverse kommander')
(motion					'r�relse' 				'saker som har med forflyttning och vridning � gj�re')
(paintbox				'maleskrin'				'malepaletten')
(#'pen trails'			'pennestr�k'				'saker som har med str�k som pennen tegner � gj�re)
(#'pen use'				'pen bruk' 		'bruk av et objekts pen')
(playfield				'lekeplats'					'objektet som beholdere for andre synlige objekt')
(sampling				'pr�vetagning'			'pr�vetagning')
(scripts					'skript'					'metoder lagt til dette objektet')
(slider					'h�ndtak'			'nyttige funksjoner for h�ndtak')
(speaker				'h�ytaler'				'objektet sett som en h�ytaler')
(#'stack navigation'		'stacknavigering'		'navigering innen en stack')
(storyboard				'storyboard'				'storyboard')
(tests					'tester'					'for � lage ja/nei tester i script')
(text					'tekst'					'Objektet som tekst')
(viewing				'seer'					'Saker som har med olika seeren � gj�re')
(vector					'vektor'					'Objektet som en vektor')
 ) language: #Norsk.

	self addToTranslationTableFrom: #(
(:						'_'						'gi verdi')
(Incr:					'�ke med'			'�ka verdi med')
(Decr:					'minske med'			'minske verdi med')
(Mult:					'multiplicera med'			'multiplisere verdi med'')) language: #Norsk! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addSpanishVocabulary

   "'Espa�ol' translation by Diego G�mez Deck and Germ�n Morales"
   " � � � � � � � "

   self translateMethodInterfaceWordings: #(
(append:                   'agregar al final'                    'Agregar el objeto a mi contenido, poni�ndolo despu�s de todos los otros objetos actualmente contenidos por mi')
(beep:                     'sonar'                               'Reproducir el sonido especificado')
(bounce:                   'rebotar'                             'Rebotar si toca el borde')
(cameraPoint               'punto de la c�mara'                  'El punto de la c�mara')
(clear                     'limpiar'                             'Limpia el gr�fico del contenido actual')
(clearOwnersPenTrails      'limpiar todos los rastros del l�piz'       'Limpiar todos los rastros del l�piz en mi contenedor')
(clearTurtleTrails         'limpiar los rastros del l�piz'       'Limpiar los rastros del l�piz en el interior')
(color:sees:               'color ve'                            'Si el color dado ve el otro color')
(deleteCard                'borrar tarjeta'                      'Borrar la tarjeta actual')
(doMenuItem:               'realizar la opci�n del men�'         'Realizar la opci�n del men�')
(doScript:                 'realizar'                            'Realizar el gui�n dado una vez, en el pr�ximo instante')
(emptyScript               'gui�n vac�o'                         'Un gui�n vac�o')
(fire                      'activar'                             'Activar todas y cada una de las acciones del bot�n de este objeto')
(firstPage                 'primera p�gina'                      'Ir a la primera p�gina')
(followPath                'seguir el camino'                    'Seguir el camino')
(forward:                  'avanzar'                             'Mover el objeto hacia adelante en la direcci�n del objeto')
(getActWhen                'activar cuando'                      'Cuando se debe activar el gui�n')
(getAllButFirstCharacter   'todos excepto el primero'            'Todos mis caracteres excepto el primero')
(getAmount                 'desplazamiento'                      'La cantidad de desplazamiento')
(getAngle                  '�ngulo'                              'El desplazamiento angular')
(getBorderColor            'color del borde'                     'El color del borde del objeto')
(getBorderWidth            'ancho del borde'                     'El ancho del borde del objeto')
(getBottom                 'abajo'                               'El borde de abajo')
(getBrightnessUnder        'brillo debajo'                       'El brillo debajo del centro del objeto')
(getCharacters             'caracteres'                          'Los caracteres en mi contenido')
(getColor                  'color'                               'El color del objeto')
(getColorUnder             'color debajo'                        'El color debajo del centro del objeto')
(getConePosition           'posici�n del cono'                   'La posici�n del cono del altavoz')
(getCursor                 'cursor'                              'La posici�n actual del cursor, trasladado al principio si es apropiado')
(getDescending             'descendiente'                        'Verdadero si el menor valor est� arriba o a la izquierda, falso si est� abajo o a la derecha')
(getDistance               'distancia'                           'El largo del vector que conecta el origen a la posici�n del objeto')
(getFirstCharacter         'primer caracter'                     'El primer caracter de mi contenido')
(getFirstElement           'primer elemento'                     'El primer objeto de mi contenido')
(getFogColor               'color de la niebla'                  'El color de la niebla que se est� aplicando')
(getFogDensity             'intensidad de la niebla'             'La intensidad de la niebla que se est� aplicando')
(getFogRangeEnd            'niebla hasta'                        'Hasta donde se aplica la niebla')
(getFogRangeStart          'niebla desde'                        'Desde donde se aplica la niebla')
(getFogType                'tipo de niebla'                      'El tipo de la niebla que se est� aplicando')
(getGraphic                'gr�fico'                             'El gr�fico usado actualmente')
(getGraphicAtCursor        'gr�fico en el cursor'                'El gr�fico usado por el objeto en el cursor')
(getHeading                'direcci�n'                           'En que direcci�n est� mirando el objeto. Cero significa hacia arriba')
(getHeight                 'altura'                              'La altura del objeto')
(getHolder                 'contenedor'                          'El contenedor del objeto')
(getIndexInOwner           'n�mero de elemento'                     'Mi n�mero dentro de mi contenedor')
(getIsUnderMouse           'debajo del rat�n'                    'Si el objeto est� debajo de la posici�n actual del rat�n')
(getKnobColor              'color de la perilla'                 'El color del deslizador')
(getLabel                  'etiqueta'                            'La etiqueta del bot�n')
(getLastValue              '�ltimo valor'                        'El �ltimo valor obtenido')
(getLeft                   'izquierda'                           'El borde de la izquierda')
(getLeftRight              'desplazamiento izquierda - derecha'  'El desplazamiento horizontal')
(getLuminanceUnder         'luminiscencia debajo'                'La luminiscencia debajo del centro del objeto')
(getMaxVal                 'm�ximo valor'                        'El n�mero representado cuando la perilla est� a la derecha o abajo del deslizador. Este es el m�ximo valor devuelto por el deslizador')
(getMinVal                 'm�nimo valor'                        'El n�mero representado cuando la perilla est� a la izquierda o arriba  del deslizador. Este es el m�nimo valor devuelto por el deslizador')
(getMouseX                 'x del rat�n'                         'La coordenada X del puntero del rat�n')
(getMouseY                 'y del rat�n'                         'La coordenada Y del puntero del rat�n')
(getNewClone               'copiar'                              'Devuelve una copia de este objeto')
(getNumberAtCursor         'n�mero en el cursor'                 'El n�mero en el cursor')
(getNumericValue           'valor num�rico'                      'Un n�mero representando la posici�n actual de la perilla')
(getObtrudes               'sobresale'                         'Si el objeto sobresale de los bordes de su contenedor')
(getPenColor               'color del l�piz'                     'El color del l�piz')
(getPenDown                'l�piz bajo'                          'Si el l�piz est� bajo')
(getPenSize                'tama�o del l�piz'                    'El ancho del l�piz')
(getRight                  'derecha'                             'El borde derecho')
(getRoundedCorners         'esquinas redondeadas'                'Si las esquinas deber�an ser redondeadas')
(getSampleAtCursor         'muestra en el cursor'                'El valor de la muestra en la posici�n actual del cursor')
(getSaturationUnder        'saturaci�n debajo'                   'La saturaci�n debajo del centro del objeto')
(getScaleFactor            'factor de escala'                    'El factor de escala por el cual el objeto es magnificado')
(getTheta                  'theta'                               'El �ngulo entre el eje X positivo y el vector que conecta el origen a la posici�n del objeto')
(getTop                    'arriba'                              'El borde de arriba')
(getTruncate               'truncado'                            'Si es verdadero, solo n�meros son usados como valores; si es falso, valores fraccionarios son permitidos')
(getUpDown                 'desplazamiento arriba - abajo'       'El desplazamiento vertical')
(getValueAtCursor          'objeto en el cursor'                 'El objeto actualmente en el cursor')
(getViewingByIcon          'vista normal'                        'Si los contenidos son vistos normalmente')
(getWidth                  'ancho'                               'El ancho')
(getX                      'x'                                   'La coordenada X')
(getY                      'y'                                   'La coordenada Y')
(goToFirstCardInBackground 'ir a la primera tarjeta del fondo'   'Ir a la primera tarjeta del fondo actual')
(goToFirstCardOfStack      'ir a la primera tarjeta de la pila'  'Ir a la primera tarjeta de la pila completa')
(goToLastCardInBackground  'ir a la �ltima tarjeta del fondo'    'Ir a la �ltima tarjeta del fondo actual')
(goToLastCardOfStack       'ir a la �ltima tarjeta de la pila'   'Ir a la �ltima tarjeta de la pila completa')
(goToNextCardInStack       'ir a la pr�xima tarjeta de la pila'  'Ir a la pr�xima tarjeta')
(goToPreviousCardInStack   'ir a la anterior tarjeta de la pila' 'Ir a la anterior tarjeta')
(goToRightOf:              'alinear despu�s de'                     'Ubicar este objeto a la derecha de otro')
(goto:                     'ir a'                                'Ir a la p�gina dada')
(hide                      'ocultar'                             'Hacer invisible al objeto')
(include:                  'incluir'                             'Agregar el objeto a mi contenido')
(initiatePainting          'iniciar pintado'                     'Iniciar el pintado de nuevos objetos en el campo de juegos estandar')
(insertCard                'insertar nueva tarjeta'              'Insertar una nueva tarjeta')
(lastPage                  '�ltima p�gina'                       'Ir a la �ltima p�gina')
(liftAllPens               'levantar todos los l�pices'          'Levantar los l�pices de todos los objetos en mi interior')
(loadSineWave              'cargar forma de onda senoidal'       'Cargar una forma de onda senoidal como el gr�fico actual')
(loadSound:                'cargar sonido'                       'Cargar el sonido especificado dentro del gr�fico actual')
(lowerAllPens              'bajar todos los l�pices'             'Bajar los l�pices de todos los objetos en mi interior')
(makeNewDrawingIn:         'empezar pintado en'                  'Crear un nuevo dibujo en el campo de juegos especificado')
(moveToward:               'mover hacia'                         'Mover hacia el objeto dado')
(nextPage                  'pr�xima p�gina'                      'Ir a la pr�xima p�gina')
(pauseAll:                 'pausar todo'                         'Hacer que todos los guiones se pausen en el objeto y en todos sus hermanos')
(pauseScript:              'pausar gui�n'                        'Hacer que se pause el gui�n dado')
(play                      'reproducir'                          'Reproducir el gr�fico actual como un sonido')
(prepend:                  'agregar adelante'                    'Agregar el objeto a mi contenido, poni�ndolo antes que todos los otros objetos actualmente contenidos por mi')
(previousPage              'p�gina anterior'                     'Ir a la p�gina anterior')
(removeAll                 'remover todo'                        'Remover todos los elementos del campo de juegos')
(reverse                   'invertir'                            'Invertir el gr�fico')
(roundUpStrays             'reunir perdidos'                       'Traer todas las partes fuera del contenedor nuevamente a la vista')
(seesColor:                'sobre color'                         'Si alguna parte del objeto esta sobre el color dado')
(show                      'mostrar'                             'Hacer visible el objeto')
(shuffleContents           'mezclar el contenido'                'Mezclar el contenido del campo de juegos')
(stampAndErase             'estampar y borrar'                   'Estampar mi imagen al rastro del l�piz y salir')
(startAll:                 'comenzar todo'                       'Comenzar el latido del gui�n en el objeto y en todos sus hermanos')
(startScript:              'comenzar gui�n'                      'Comenzar el latido del gui�n dado')
(stopAll:                  'parar todos'                         'Hacer que el estado del gui�n dado sea "normal" en el objeto y en todos sus hermanos')
(stopScript:               'detener gui�n'                         'Hacer que el estado del gui�n dado sea "normal"')
(tellAllSiblings:          'decir a todos los hermanos'          'Enviar un mensaje a todos los hermanos')
(touchesA:                 'toca un'                             'Si toco algo que parece como...')
(turn:                     'girar'                               'Cambiar la direcci�n del objeto en la cantidad especificada')
(unhideHiddenObjects       'mostrar objetos ocultos'             'Mostrar todos los objetos ocultos')
(wearCostumeOf:            'lucir como'                          'Usar el traje de...')

(wrap                      'ajusta'                                'traslada al otro borde si es apropiado')
(penArrowheads             'puntas de flecha en el l�piz'        'Si muestra puntas de flecha al final de cada trazo del l�piz')
(dropShadow                'sombra'                              'Si muestra la sombra')
(shadowColor               'color de la sombra'                  'Color de la sombra')
(clipSubmorphs             'cortar submorphs'                    'Si cortar o no mis submorphs')

) language: #'Espa�ol'.

   self translateCategories: #(
(basic                 'b�sico'                 'Unas pocas cosas importantes')
(#'book navigation'    'navegacion del libro'   'Relativo a libros, pilas, etc.')
(button                'bot�n'                  'Para pensar este objeto como un bot�n')
(collections           'colecciones'            'Para pensar este objeto como una colecci�n')
(fog                   'niebla'                 'Niebla 3D')
(geometry              'geometr�a'              'Medidas y coordenadas')
(#'color & border'     'color y borde'          'Asuntos relacionados con colores y bordes de objetos')
(graphics              'gr�ficos'               'Para pensar este objeto como una imagen')
(variables			 'variables de instancia' 'Variables de instancia agregadas por este objeto')
(joystick              'joystick'               'El objeto como un Joystick')
(miscellaneous         'miscel�neo'             'Comandos varios')
(scripting             'guiones'                'Comandos para  comenzar y parar guiones, etc.')
(motion                'movimiento'             'Asuntos relacionados con el movimiento y el giro')
(paintbox              'paleta de pintor'       'La paleta del pintor')
(#'pen trails'         'rastros del l�piz'      'Relacionados a los rastros dejados por los l�pices')
(#'pen use'            'uso del l�piz'          'Uso del l�piz del objeto')
(playfield             'campo de juegos'        'El objeto como un contenedor para otros objetos visibles')
(sampling              'muestreo'               'Muestreo')
(scripts               'guiones'                'Guiones agregados por este objeto')
(slider                'deslizador'             'Funciones �tiles para deslizadores')
(speaker               'altavoz'                'El objeto como un altavoz de audio')
(#'stack navigation'   'navegaci�n de pila'     'Navegaci�n dentro de una pila')
(storyboard            'pizarra de historia'    'Pizarra de historia')
(tests                 'pruebas'                'Pruebas por verdadero/falso, para usar en cuadros de "Prueba" de guiones')
(text                  'texto'                  'El objeto como texto')
(viewing               'visualizaci�n'          'Asuntos relacionados a visualizaci�n')
(vector                'vector'                 'El objeto como un vector')
(layout                'disposici�n'            'Disposici�n de los objetos contenidos')
(#'drag & drop'        'arrastrar y soltar'     'Arrastrar y soltar')
(observation           'observaci�n'            'Observaci�n')

 ) language: #'Espa�ol'.

        self addToTranslationTableFrom: #(
(:                                                '_'                                                'asignar el valor')
(Incr: 'incrementar por' 'incrementar el valor por')
(Decr: 'reducir por' 'reducir el valor por')
(Mult: 'multiplicar por' 'multiplicar el valor por')) language: #'Espa�ol'
! !

!EToyVocabulary methodsFor: 'language translations' stamp: 'sw 4/15/2003 23:46'!
addSwedishVocabulary
	"Add a Swedish vocabulary. Well, ok, perhaps better translations could be made...
	I might have simplified a few word choices to be better suited for kids like 'sudda' instead of 'rensa' (two words for 'clear'). Hmm, this was hard... :-)"

	self translateMethodInterfaceWordings: #(
(append: 'l�gg till' 'L�gg till objektet i mitt inneh�ll')
(beep: 'spela ljud' 'Spela upp angivet ljud')
(bounce: 'studsa' 'studsa bort fr�n kanten ifall den tr�ffas')
(cameraPoint 'kameraposition' 'kamerans position')
(clear 'sudda' 'Sudda bildens inneh�ll')
(clearOwnersPenTrails 'sudda alla pennstreck' 'Sudda alla pennstreck i pennans lekplats')
(clearTurtleTrails 'sudda pennstreck' 'Sudda alla pennstreck p� insidan')
(color:sees: 'f�rg syns' 'ifall vald f�rg ser angiven f�rg')
(deleteCard 'Ta bort kort' 'Ta bort det nuvarande kortet')
(doMenuItem: 'utf�r menyval' 'utf�r menyvalet')
(doScript: 'k�r' 'k�r skriptet en g�ng, vid n�sta tick')
(emptyScript 'tomt skript' 'ett tomt skript')
(fire 'starta' 'startar alla knapph�ndelser f�r detta objekt')
(firstPage 'f�rsta sidan' 'g� till f�rsta sidan')
(followPath 'f�lj v�g' 'f�lj den utsatta v�gen')
(forward: 'fram�t med' 'Flyttar objektet fram�t i objektets nuvarande riktning')
(getActWhen 'k�r n�r' 'N�r skriptet skall k�ras')
(getAllButFirstCharacter 'alla utom f�rsta' 'Alla mina tecken utom det f�rsta')
(getAmount 'storlek' 'F�rflyttningens storlek')
(getAngle 'vinkel' 'Vinkelf�rflyttningens storlek')
(getBorderColor 'kantf�rg' 'F�rgen p� objektets kant')
(getBorderWidth 'kantbredd' 'Bredden p� objektets kant')
(getBottom 'botten' 'Den nedersta kanten')
(getBrightnessUnder 'kontrast' 'Kontrasten under objektets mittpunkt')
(getCharacters 'tecken' 'Tecknen i mitt inneh�ll')
(getColor 'f�rg' 'Objektets f�rg')
(getColorUnder 'f�rg under' 'F�rgen under objektets mittpunkt')
(getConePosition 'h�gtalarposition' 'h�gtalarens position')
(getCursor 'mark�r' 'Mark�rens nuvarande position, omv�xlad till b�rjan ifall det �r l�mpligt')
(getDescending 'fallande' 'S�ger ifall det minsta v�rdet �r �verst/till v�nster (fallande = false) eller nederst/till h�ger (fallande = true)')
(getDistance 'avst�nd' 'L�ngden p� vektorn mellan ursprungspunkten och objektets position')
(getFirstCharacter 'f�rsta tecknet' 'Det f�rsta tecknet i mitt inneh�ll')
(getFirstElement 'f�rsta elementet' 'Det f�rsta objektet i mitt inneh�ll')
(getFogColor 'dimmans f�rg' 'F�rgen p� dimman som appliceras')
(getFogDensity 'dimmans densitet' 'Densiteten p� dimman som appliceras')
(getFogRangeEnd 'dimmans intervallslut' 'Intervallets slut p� dimman som appliceras')
(getFogRangeStart 'dimmans intervallstart' 'Intervallets start p� dimman som appliceras')
(getFogType 'dimmans typ' 'Typen av dimma som appliceras')
(getGraphic 'bild' 'Bilden som b�rs av objektet')
(getGraphicAtCursor 'bild vid mark�r' 'Bilden som b�rs av objektet vid mark�ren')
(getHeading 'riktning' '�t vilket h�ll objektet pekar. 0 �r rakt upp')
(getHeight 'h�jd' 'H�jden')
(getHolder 'beh�llare' 'objektets beh�llare')
(getIndexInOwner 'elementnummer' 'mitt index i min beh�llare')
(getIsUnderMouse '�r under muspekaren' 'ifall objektet befinner sig under muspekaren')
(getKnobColor 'draghandtagets f�rg' 'Draghandtagets f�rg')
(getLabel 'etikett' 'Texten p� knappen')
(getLastValue 'senaste v�rde' 'Senast ber�knade v�rde')
(getLeft 'v�nster' 'Den v�nstra kanten')
(getLeftRight 'v�nster/h�ger' 'Horisontell f�rflyttning')
(getLuminanceUnder 'ljusstyrka under' 'Ljusstyrkan under objektets mittpunkt')
(getMaxVal 'maxv�rde' 'Talet som representerar n�r draghandtaget �r l�ngst till h�ger eller l�ngst ner, det st�rsta v�rdet som draghandtaget ger.')
(getMinVal 'minv�rde' 'Talet som representerar n�r draghandtaget �r l�ngst till v�nster eller h�gst upp, det minsta v�rdet som draghandtaget ger.')
(getMouseX 'mus x' 'Muspekarens x-koordinat')
(getMouseY 'mus y' 'Muspekarens y-koordinat')
(getNewClone 'kopia' 'returnerar en kopia det h�r objektet')
(getNumberAtCursor 'tal vid mark�r' 'talet vid mark�ren')
(getNumericValue 'numeriskt v�rde' 'Ett tal som representerar den aktuella positionen av draghandtaget.')
(getObtrudes 'sticker ut' 'ifall objektet sticker ut �ver beh�llarens kant')
(getPenColor 'pennf�rg' 'f�rgen p� bl�cket i pennan')
(getPenDown 'penna nedtryckt' 'ifall pennan �r nedtryckt just nu')
(getPenSize 'pennans bredd' 'pennans bredd')
(getRight 'h�ger' 'Den h�gra kanten')
(getRoundedCorners 'rundade h�rn' 'Ifall h�rnen skall vara runda')
(getSampleAtCursor 'v�rde vid mark�r' 'Nuvarande v�rde vid mark�rens position')
(getSaturationUnder 'm�ttnad under' 'M�ttnaden under objektets mittpunkt')
(getScaleFactor 'skalfaktor' 'Objektets f�rstoringsfaktor')
(getTheta 'theta' 'Vinkeln mellan den positiva x-axeln och vektorn mellan ursprungspunkten och objektets position')
(getTop 'toppen' 'Den �versta kanten')
(getTruncate 'trunkera' 'Ifall endast heltal anv�nds som v�rden, om inte �r br�ktal ocks� till�tna.')
(getUpDown 'upp/ner' 'Vertikal f�rflyttning')
(getValueAtCursor 'spelare vid mark�r' 'objektet vid mark�ren')
(getViewingByIcon 'normalvy' 'vyn p� inneh�llet �r normal')
(getWidth 'bredd' 'Bredden')
(getX 'x' 'X-koordinaten')
(getY 'y' 'Y-koordinaten')
(goToFirstCardInBackground 'g� till f�rsta i bakgrunden' 'G� till det f�rsta kortet i den nuvarande bakgrunden')
(goToFirstCardOfStack 'g� till f�rsta kortet i stacken' 'G� till f�rsta korten i hela stacken')
(goToLastCardInBackground 'g� till sista kortet i bakgrunden' 'G� till det sista kortet i den nuvarande bakgrunden')
(goToLastCardOfStack 'g� till sista kortet i stacken' 'G� till det sista kortet i hela stacken')
(goToNextCardInStack 'g� till n�sta kort' 'G� till n�sta kort i stacken')
(goToPreviousCardInStack 'g� till f�rra kortet' 'G� till det f�reg�ende kortet i stacken')
(goToRightOf: 'align after' 'placera detta objekt till h�ger om ett annat')
(goto: 'g� till' 'g� till angiven sida')
(hide 'g�m' 'g�r objektet osynligt')
(initiatePainting 'b�rja m�lning' 'P�b�rja m�lning av ett nytt objekt i den vanliga lekplatsen.')
(insertCard 'stoppa in kort' 'Skapa ett nytt kort')
(lastPage 'sista sidan' 'g� till sista sidan')
(liftAllPens 'lyft alla pennor' 'Lyft pennorna p� mitt inneh�lls alla objekt.')
(loadSineWave 'ladda sinusv�g' 'Ladda en sinusv�g som nuvarande graf')
(loadSound: 'ladda ljud' 'Ladda angivet ljud som nuvarnde graf')
(lowerAllPens 's�nk alla pennor' 'S�nk pennorna p� mitt inneh�lls alla objekt.')
(makeNewDrawingIn: 'p�b�rja m�lning i' 'g�r en ny m�lning i angiven lekplats')
(moveToward: 'flytta mot' 'flytta mot angivet objekt')
(nextPage 'n�sta sida' 'g� till n�sta sida')
(pauseAll: 'pausa alla' 'pausa skriptet i objektet och alla dess kusiner')
(pauseScript: 'pausa skript' 'pausa skriptet')
(play 'spela' 'Spela nuvarande graf som ett ljud')
(previousPage 'f�reg�ende sida' 'g� till f�reg�ende sida')
(removeAll 'ta bort alla' 'Ta bort alla element fr�n lekplatsen')
(reverse 'reversera' 'Reversera grafen')
(roundUpStrays 'samla in bortsprungna' 'Samla in alla delar utanf�r beh�llaren s� att de blir synliga igen.')
(seesColor: '�r �ver f�rg' 'ifall n�gon del av objektet �r �ver den givna f�rgen')
(show 'visa' 'g�r objektet synligt')
(shuffleContents 'blanda inneh�ll' 'Blanda lekplatsens inneh�ll')
(stampAndErase 'st�mpla och f�rsvinn' 'l�gg till min bild som ritstreck och f�rsvinn')
(startAll: 'starta alla' 'starta skriptet tickande i objektet och alla dess kusiner.')
(startScript: 'starta skript' 'starta skriptet tickande')
(stopAll: 'stoppa alla' 'g�r skriptet "normalt" i objektet och alla dess kusiner')
(stopScript: 'stoppa skript' 'g�r skriptet "normalt" i objektet')
(tellAllSiblings: 's�g till kusiner' 'skicka ett meddelande till alla kusiner')
(touchesA: 'vidr�r' 'Ifall jag vidr�r n�got som ser ut som...')
(turn: 'sv�ng med' '�ndra objektets riktning med angiven m�ngd')
(unhideHiddenObjects 'visa alla g�mda objekt' 'G�r alla g�mda objekt synliga.')
(wearCostumeOf: 'se ut som' 'b�r samma dr�kt som...')
(wrap 'v�xla �ver kant' 'v�xla �ver kanten ifall det �r l�mpligt')) language: #Svenska.

	self translateCategories: #(
(basic					'grunder'				'ett antal viktiga saker')
(#'book navigation'		'boknavigering'			'saker som har att g�ra med b�cker och stackar med mera')
(button					'knapp'					'objektet betraktat som en knapp man kan trycka p�')
(collections				'samlingar'				'objektet betraktat som en samling av andra objekt')
(fog					'dimma'					'3-dimensionell dimma')
(geometry				'geometri' 				'm�tv�rden och koordinater')
(#'color & border'		'f�rger & kanter'		'saker som har att g�ra med f�rger och kanter p� objekt')
(graphics				'grafik'					'objektet betraktat som en bild')
(variables				'instansvariabler'		'instansvariabler tillagda i detta objekt')
(joystick				'joystick	'				'objektet som en joystick')
(miscellaneous			'diverse' 				'diverse kommandon')
(motion					'r�relse' 				'saker som har att g�ra med f�rflyttning och vridning')
(paintbox				'm�larl�da'				'm�larpaletten')
(#'pen trails'			'pennstreck'				'saker som har att g�ra med streck som pennan ritar')
(#'pen use'				'pennanv�ndning' 		'anv�ndning av ett objekts penna')
(playfield				'lekplats'					'objektet som beh�llare f�r andra synliga objekt')
(sampling				'provtagning'			'provtagning')
(scripts					'skript'					'metoder tillagda i detta objekt')
(slider					'draghandtag'			'anv�ndbara funktioner f�r draghandtag')
(speaker				'h�gtalare'				'objektet betraktat som en h�gtalare')
(#'stack navigation'		'stacknavigering'		'navigering inom en stack')
(storyboard				'storyboard'				'storyboard')
(tests					'tester'					'ja/nej tester, f�r anv�ndning skriptens testytor')
(text					'text'					'Objektet som text')
(viewing				'vyer'					'Saker som har att g�ra med olika vyer')
(vector					'vektor'					'Objektet som en vektor')
 ) language: #Svenska.

	self addToTranslationTableFrom: #(
(:						'_'						'tilldela v�rde')
(Incr:					'�ka med'			'�ka v�rde med')
(Decr:					'minska med'			'minska v�rde med')
(Mult:					'multiplicera med'			'multiplicera v�rde med')) language: #Svenska
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

