'From Squeak2.9alpha of 17 July 2000 [latest update: #3231] on 7 January 2001 at 12:48:44 pm'!"Change Set:		listGoofDate:			7 January 2001Author:			Bob ArningFixes a slight goof in an previous change set (lessDelay)"!!PointerFinder methodsFor: 'morphic ui' stamp: 'RAA 1/7/2001 12:47'!open	| window list |	window _ (SystemWindow labelled: 'Pointer Finder')		model: self.	list _ PluggableListMorph new		doubleClickSelector: #inspectObject;		on: self		list: #pointerList		selected: #pointerListIndex		changeSelected: #pointerListIndex:		menu: #menu:shifted:		keystroke: #arrowKey:from:.	window addMorph: list frame: (0@0 extent: 1@1).	list color: Color lightMagenta.	window openInWorld! !