'From Squeak2.8 of 13 June 2000 [latest update: #2345] on 3 July 2000 at 10:54:53 pm'!"Change Set:		ENH-ObjectExplorerMenuDate:			3 July 2000Author:			Chris NortonI was trying to figure out what code was responsible for some options on a popup menu, so I decided to explore the morph.  Exploring the morph, I quickly located the action selectors I was looking for, but I was unable to get at the code for the selectors without typing.  Well, I admit to some laziness, so this situation just wouldn't do.  I dropped everything and wrote this little enhancement to make my life a bit easier.  :-)This change set adds two menu options to ObjectExplorer's genericMenu: if the insideObject (the object selected in the list) is of class Symbol.  I figure that most of the time, the symbol will probably be a selector.  So this enhancement adds 'senders' and 'implementors' to the list of availble menu options for Symbols.Hope you other lazy people can find a use for this little hack.  :-)---==> Chris"!!ObjectExplorer methodsFor: 'as yet unclassified' stamp: 'ccn 7/3/2000 22:35'!genericMenu: aMenu	| insideObject menu |	currentSelection ifNil:		[menu _ aMenu.		menu add: '*nothing selected*' target: self selector: #yourself]	 ifNotNil:		[menu _ DumberMenuMorph new defaultTarget: self.		insideObject _ currentSelection withoutListWrapper.		menu 			add: 'explore' target: insideObject  selector: #explore;			add: 'inspect' target: insideObject  selector: #inspect;			addLine;			add: 'objects pointing to this value' target: Smalltalk  selector:  #browseAllObjectReferencesTo:except:ifNone:  argumentList: (Array with: insideObject with: #() with: nil);			addLine;			add: 'browse full' target: Browser  selector: #fullOnClass:  argument: insideObject class;			add: 'browse class' target: insideObject class  selector: #browse;			add: 'browse hierarchy' target: Utilities  selector:  #spawnHierarchyForClass:selector: argumentList: (Array with: insideObject class with: nil).		insideObject class == Symbol ifTrue: [			menu 				addLine;				add: ('senders of ', insideObject printString) target: Smalltalk  selector: #browseAllCallsOn:  argument: insideObject;				add: ('implementors of ', insideObject printString) target: Smalltalk  selector: #browseAllImplementorsOf:  argument: insideObject]].	^ menu! !