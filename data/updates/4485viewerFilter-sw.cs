'From Squeak3.2alpha of 2 October 2001 [latest update: #4482] on 13 November 2001 at 7:51:22 pm'!"Change Set:		viewerFilter-swDate:			13 November 2001Author:			Scott WallaceAllows a filter to be applied to item-lists in a Viewer, to suit pedagogic preference and various types of users.The existing preference 'eToyFriendly', which by default is true in the plug-in image but false in the standalone 3.2a image, now governs whether a filter customized to classroom use at UES and the Open School in school year 2001-2 is to be used.Also fixes a minor unrelated bug."!!Object methodsFor: 'viewer' stamp: 'sw 11/13/2001 09:37'!tilePhrasesForCategory: aCategorySymbol inViewer: aViewer	"Return a collection of phrases for the category.  If using classic tiles, only include phrases that have fewer than two arguments, because all that they can handle."	| interfaces itsSelector toSuppress |	interfaces _ self methodInterfacesForCategory: aCategorySymbol inVocabulary: aViewer currentVocabulary limitClass: aViewer limitClass.	interfaces _ self methodInterfacesInPresentationOrderFrom: interfaces forCategory: aCategorySymbol.	toSuppress _ aViewer currentVocabulary phraseSymbolsToSuppress.	interfaces _ interfaces select: [:int | (toSuppress includes: int selector) not].	Preferences universalTiles ifFalse:		[interfaces _ interfaces select:			[:int |				itsSelector _ int selector.				itsSelector numArgs < 2 or:					"The lone two-arg loophole in classic tiles"					[#(color:sees:) includes: itsSelector]]].	^ interfaces collect:		[:aMethodInterface |			aMethodInterface wantsReadoutInViewer				ifTrue:					[aViewer phraseForVariableFrom: aMethodInterface]				ifFalse:					[aViewer phraseForCommandFrom: aMethodInterface]]! !!PasteUpMorph class methodsFor: 'scripting' stamp: 'sw 11/13/2001 10:28'!additionsToViewerCategories	"Answer a list of (<categoryName> <list of category specs>) pairs that characterize the phrases this kind of morph wishes to add to various Viewer categories."	^ # ((playfield ((command initiatePainting 'Initiate painting of a new object in the standard playfield.')(slot mouseX 'The x coordinate of the mouse pointer' Number readWrite Player getMouseX  unused unused)(slot mouseY 'The y coordinate of the mouse pointer' Number readWrite Player getMouseY  unused unused)(command roundUpStrays 'Bring all out-of-container subparts back into view.')(slot numberAtCursor 'the Number at the cursor' Number readWrite Player getNumberAtCursor Player setNumberAtCursor: )(slot playerAtCursor 'the object currently at the cursor' Player readWrite Player getValueAtCursor  unused unused)(slot graphicAtCursor 'the graphic worn by the object at the cursor' Graphic readOnly Player getGraphicAtCursor  unused unused)(command unhideHiddenObjects 'Unhide all hidden objects.')))(collections ((slot cursor 'The index of the chosen element' Number readWrite Player getCursor Player setCursorWrapped:)(slot playerAtCursor 'the object currently at the cursor' Player readWrite Player getValueAtCursor  unused unused)(slot firstElement  'The first object in my contents' Player  readWrite Player getFirstElement  Player  setFirstElement:)(slot NumberAtCursor 'the number at the cursor' Number readWrite Player getNumberAtCursor Player setNumberAtCursor: )(slot graphicAtCursor 'the graphic worn by the object at the cursor' Graphic readOnly Player getGraphicAtCursor  unused unused)(command removeAll 'Remove all elements from the playfield')(command shuffleContents 'Shuffle the contents of the playfield')(command append: 'Add the object to my content' Player)))(#'stack navigation' ((command goToNextCardInStack 'Go to the next card')(command goToPreviousCardInStack  'Go to the previous card')(command goToFirstCardInBackground 'Go to the first card of the current background')(command goToFirstCardOfStack 'Go to the first card of the entire stack')(command goToLastCardInBackground 'Go to the last card of the current background')(command goToLastCardOfStack 'Go to the last card of the entire stack')(command deleteCard 'Delete the current card')(command insertCard 'Create a new card')))(viewing ((slot viewingNormally 'whether contents are viewed normally' Boolean readWrite Player getViewingByIcon Player setViewingByIcon: )))(#'pen trails' ((command liftAllPens 'Lift the pens on all the objects in my interior.')(command lowerAllPens  'Lower the pens on all the objects in my interior.')(command arrowheadsOnAllPens  'Put arrowheads on the ends of strokes of pens on all objects.')(command noArrowheadsOnAllPens  'Stop putting arrowheads on the ends of strokes of pens on all objects.')(command clearTurtleTrails 'Clear all the pen trails in the interior.'))))! !!Vocabulary methodsFor: 'method list' stamp: 'sw 11/13/2001 09:34'!phraseSymbolsToSuppress	"Answer a dictatorially-imposed list of phrase-symbols that are to be suppressed from viewers, even if they otherwise show up.  Note that EToyVocabulary reimplements"	^ #()! !!EToyVocabulary methodsFor: 'method list' stamp: 'sw 11/13/2001 10:31'!phraseSymbolsToSuppress	"Answer a dictatorially-imposed list of phrase-symbols that are to be suppressed from viewers when the eToyFriendly preference is set to true.  This list at the moment corresponds to the wishes of Alan and Kim and the LA teachers using Squeak in school-year 2001-2"	^ Preferences eToyFriendly		ifTrue:			[#(moveToward: followPath goToRightOf:				getViewingByIcon initiatePainting)]		ifFalse:			[#()]! !