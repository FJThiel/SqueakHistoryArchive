'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 10 March 2004 at 4:02:12 pm'!"Change Set:		BlankMenuIconsPref-nk (v3)Date:			10 March 2004Author:			Ned KonzI couldn't avoid the recent discussion of menu icons, so I am contributing this little ENH.This will line up all the text in menus containing icons by using blank icons for those items without icons.I believe that this leads to an easier to read menu.v2: handled cases where submenus have icons but parent menus don't.v3: removed preference. Made checkboxes line up too."Preferences removePreference: #useBlankMenuIcons!!MenuItemMorph commentStamp: '<historical>' prior: 0!I represent an item in a menu.Instance variables:	isEnabled 	<Boolean>	True if the menu item can be executed.	subMenu 	<MenuMorph | nil>	The submenu to activate automatically when the user mouses over the item.	isSelected 	<Boolean>	True if the item is currently selected.	target 		<Object>		The target of the associated action.	selector 		<Symbol>	The associated action.	arguments 	<Array>		The arguments for the associated action.	icon		<Form | nil>	An optional icon form to be displayed to my left.If I have a dynamic marker, created by strings like <yes> or <no> in my contents, it will be installed as a submorph.!!MenuIcons class methodsFor: 'accessing - icons' stamp: 'nk 3/9/2004 11:27'!blankIcon	^ Icons		at: #blankIcon		ifAbsentPut: [ Form				extent: 16 @ 16				depth: 8 ] ! !!MenuIcons class methodsFor: 'decoration' stamp: 'nk 3/10/2004 16:00'!decorateMenu: aMenu 	"decorate aMenu with icons"	| numberAdded |	Preferences menuWithIcons		ifFalse: [^ self].	numberAdded := 0.	self itemsIcons		do: [:each | 			| wordings icon | 			wordings := each key.			icon := each value.			""			wordings				do: [:eachWording | (self							putIcon: icon							onItemWithWording: eachWording							in: aMenu)						ifTrue: [numberAdded := numberAdded + 1]]].	numberAdded isZero ifTrue: [^ self].	aMenu addBlankIconsIfNecessary: self blankIcon! !!MenuIcons class methodsFor: 'decoration' stamp: 'nk 3/9/2004 11:33'!putIcon: aForm onItemWithWording: aString in: aMenu 	"Search for an item with wording aString in aMenu and use  	aForm as the icon.	Return true if an icon was added."	| item |	item := aMenu itemWithWording: aString translated.	item ifNotNil: [ item icon: aForm ].	^item notNil! !!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:55'!contents: aString withMarkers: aBool inverse: inverse 	"Set the menu item entry. If aBool is true, parse aString for embedded markers."	| markerIndex marker |	self contentString: nil.	"get rid of old"	aBool ifFalse: [^super contents: aString].	self removeAllMorphs.	"get rid of old markers if updating"	self hasIcon ifTrue: [ self icon: nil ].	(aString notEmpty and: [aString first = $<]) 		ifFalse: [^super contents: aString].	markerIndex := aString indexOf: $>.	markerIndex = 0 ifTrue: [^super contents: aString].	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 		ifFalse: [^super contents: aString].	self contentString: aString.	"remember actual string"	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 				ifTrue: [self onImage]				ifFalse: [self offImage].	super contents:  (aString copyFrom: markerIndex + 1 to: aString size).	"And set the marker"	marker := ImageMorph new image: marker.	marker position: self left @ (self top + 2).	self addMorphFront: marker! !!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:19'!hasIcon	"Answer whether the receiver has an icon."	^ icon notNil! !!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:19'!hasIconOrMarker	"Answer whether the receiver has an icon or a marker."	^ self hasIcon or: [ submorphs isEmpty not ]! !!MenuItemMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:25'!hasMarker	"Answer whether the receiver has a marker morph."	^ submorphs isEmpty not! !!MenuItemMorph methodsFor: 'accessing' stamp: 'dgd 3/22/2003 14:45'!icon: aForm 	"change the the receiver's icon"	icon := aForm! !!MenuItemMorph methodsFor: 'drawing' stamp: 'nk 3/10/2004 15:46'!drawOn: aCanvas 	| stringColor stringBounds leftEdge |	isSelected & isEnabled		ifTrue: [			aCanvas fillRectangle: self bounds fillStyle: self selectionFillStyle.			stringColor := color negated]		ifFalse: [stringColor := color].	leftEdge := 0.	self hasIcon		ifTrue: [| iconForm | 			iconForm := isEnabled ifTrue:[self icon] ifFalse:[self icon asGrayScale].			aCanvas paintImage: iconForm at: self left @ (self top + (self height - iconForm height // 2)).			leftEdge := iconForm width + 2].	self hasMarker		ifTrue: [ leftEdge := leftEdge + self submorphBounds width + 8 ].	stringBounds := bounds left: bounds left + leftEdge.	aCanvas		drawString: contents		in: stringBounds		font: self fontToUse		color: stringColor.	subMenu		ifNotNil: [aCanvas paintImage: SubMenuMarker at: self right - 8 @ (self top + self bottom - SubMenuMarker height // 2)]! !!MenuMorph methodsFor: 'accessing' stamp: 'nk 3/10/2004 15:20'!addBlankIconsIfNecessary: anIcon	"If any of my items have an icon, ensure that all do by using anIcon for those that don't"	| withIcons withoutIcons |	withIcons _ Set new.	withoutIcons _ Set new.	self items do: [ :item |		item hasIconOrMarker			ifTrue: [ withIcons add: item ]			ifFalse: [ withoutIcons add: item ].		item hasSubMenu ifTrue: [ item subMenu addBlankIconsIfNecessary: anIcon ]].	(withIcons isEmpty or: [ withoutIcons isEmpty ]) ifTrue: [ ^self ].	withoutIcons do: [ :item | item icon: anIcon ].! !