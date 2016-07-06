'From Squeak 2.4c of May 10, 1999 on 1 July 1999 at 4:31:41 pm'!"Change Set:		MenuColorDate:			28 June 1999Author:			Dan IngallsIntroduces a new preference, #menuColorFromWorld.  When ennabled, this preference causes menus to appear raised, and with a color that is lighter or darker than the World color, depending on whether it is dark or light."!!MenuLineMorph methodsFor: 'drawing' stamp: 'di 6/28/1999 17:05'!drawOn: aCanvas	aCanvas		fillRectangle: (bounds topLeft corner: bounds rightCenter)		color: owner color darker.	aCanvas		fillRectangle: (bounds leftCenter corner: bounds bottomRight)		color: owner color lighter! !!MenuMorph methodsFor: 'initialization' stamp: 'di 6/28/1999 17:08'!setDefaultParameters	(Preferences menuColorFromWorld and: [Display depth > 4])		ifTrue: [self setColor: (World color luminance > 0.7							ifTrue: [World color mixed: 0.8 with: Color black]							ifFalse: [World color mixed: 0.4 with: Color white])					borderWidth: Preferences menuBorderWidth					borderColor: #raised]		ifFalse: [self setColor: Preferences menuColor					borderWidth: Preferences menuBorderWidth					borderColor: Preferences menuBorderColor].	inset _ 3! !"Postscript:Enable the preference..."Preferences disable: #menuColorFromWorld.!