'From Squeak2.8alpha of 4 February 2000 [latest update: #2158] on 20 May 2000 at 6:41:04 pm'!"Change Set:		077BadBadBrowserDnD-mirDate:			19 May 2000Author:			Michael RuegerMark Mayfield wrote:> If you have edited some text in the browser and click in the class> list or method list you will be prompted if is OK to cancel those> changes. If you choose NO...too late...the changes are canceled and> you left holding the text of the item you clicked on in the list box.Oops, my messing around with Stephans stuff caused this.The attached change set should fix this."!!PluggableListMorph methodsFor: 'events' stamp: 'mir 5/19/2000 19:04'!mouseDown: event onItem: aMorph	| now delta |	event yellowButtonPressed ifTrue: [^ self yellowButtonActivity: event shiftPressed].	now _ Time millisecondClockValue.	delta _ ((lastClickTime ifNil: [0]) - now) abs.	lastClickTime _ now.	self setProperty: #okToChange toValue: true.	(aMorph == selectedMorph and: [doubleClickSelector notNil and: [delta < DoubleClickTime]])		ifTrue:			[model perform: doubleClickSelector]		ifFalse:			[model okToChange ifFalse: [				self setProperty: #okToChange toValue: false.				^ self].  "No change if model is locked"			((autoDeselect == nil or: [autoDeselect]) and: [aMorph == selectedMorph])				ifTrue: [self setSelectedMorph: nil]				ifFalse: [self setSelectedMorph: aMorph]]! !!PluggableListMorph methodsFor: 'events' stamp: 'mir 5/19/2000 19:05'!mouseMove: evt	(evt yellowButtonPressed  not "First check for option (menu) click"		and: [(self valueOfProperty: #okToChange) == true])		ifTrue: [self setSelectedMorph: nil.]	! !!PluggableListMorph methodsFor: 'events' stamp: 'mir 5/19/2000 19:03'!mouseStillDown: evt onItem: item 	| dist |	(self valueOfProperty: #okToChange) ~~ true		ifTrue: [^self].	evt redButtonPressed		ifTrue: 			[dist _ (evt targetPoint - item position) abs.			(dist x max: dist y)				> 5				ifTrue: 					[self drag: evt itemMorph: item]]! !