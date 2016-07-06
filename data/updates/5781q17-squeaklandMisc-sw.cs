'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:54 pm'!
"Change Set:		q17-squeaklandMisc-sw
Date:			4 March 2004
Author:			Scott Wallace

A merger of several small and mostly unrelated Squeakland updates.

From 0158redUnderEdit-sw (20 April 2003)
¥ Provides under-edit feedback for StringMorphs-- turns the characers red when under edit.

From 0161grabPlayerFix-sw (24 April 2003)
¥ Fixes the bug that could result in the loss the viewer for an off-screen object if, after an object had been placed in the hand by a 'grab me' request from a viewer, the object were released in a no-drop-zone.

From:  0168retargetFix-sw (20 May 2003)
¥ Fixes bug in code for retargeting a tile such that it references a different player.
¥ Revert another piece of Modules residue
(required integration with 3.7a babel changes)

From:  0172copyPlayerFix-sw (22 May 2003)
Fixes the bug that had made the #copy function appear as a command rather than a slot in a Viewer.

From 0173joystickStepTime-laza, by Alexander Lazarevic: (8 June 2003)
Fixes bug that gave wrong step-time to JoystickMorph when tracking real joystick

From 0180mouseXYFixes-sw (9 January 2003)
Fixes mouseX and mouseY so that they give the correct values when originAtCenter is in effect for a playfield.

From 0189hideDot-sw (20 January 2004)
Makes the Dot spend its life just off the top-left-edge of the screen.
"!


!Object methodsFor: 'viewer' stamp: 'sw 5/22/2003 14:06'!
tilePhrasesForCategory: aCategorySymbol inViewer: aViewer
	"Return a collection of phrases for the category.  If using classic tiles, only include phrases that have fewer than two arguments, because all that they can handle."

	| interfaces itsSelector toSuppress resultType |
	interfaces _ self methodInterfacesForCategory: aCategorySymbol inVocabulary: aViewer currentVocabulary limitClass: aViewer limitClass.
	interfaces _ self methodInterfacesInPresentationOrderFrom: interfaces forCategory: aCategorySymbol.
	toSuppress _ aViewer currentVocabulary phraseSymbolsToSuppress.
	interfaces _ interfaces select: [:int | (toSuppress includes: int selector) not].
	Preferences universalTiles ifFalse:
		[interfaces _ interfaces select:
			[:int |
				itsSelector _ int selector.
				itsSelector numArgs < 2 or:
					"The lone two-arg loophole in classic tiles"
					[#(color:sees:) includes: itsSelector]]].
	^ interfaces collect:
		[:aMethodInterface |
			((resultType _ aMethodInterface resultType) notNil and: [resultType ~~ #unknown]) 
			"aMethodInterface wantsReadoutInViewer"
				ifTrue:
					[aViewer phraseForVariableFrom: aMethodInterface]
				ifFalse:
					[aViewer phraseForCommandFrom: aMethodInterface]]! !


!JoystickMorph methodsFor: 'stepping and presenter' stamp: 'laza 6/8/2003 11:53'!
stepTime
	"Provide for as-fast-as-possible stepping in the case of a real joystick"

	^ realJoystickIndex
		ifNotNil:
			[0]  "fast as we can to track actual joystick"
		ifNil:
			[super stepTime]! !


!Object class methodsFor: 'instance creation' stamp: 'sw 1/23/2003 09:45'!
categoryForUniclasses
	"Answer the default system category into which to place unique-class instances"

	^ 'UserObjects'! !


!PasteUpMorph methodsFor: 'misc' stamp: 'sw 1/9/2003 23:22'!
mouseX
	"Answer the x-coordinate of the mouse, in my coordinate system"

	^ self isInWorld
		ifTrue:
			[(self cursorPoint x) - self cartesianOrigin x]
		ifFalse:
			[0]! !

!PasteUpMorph methodsFor: 'misc' stamp: 'sw 1/9/2003 23:23'!
mouseY
	"Answer the y-coordinate of the mouse, in my coordinate system"

	^ self isInWorld
		ifTrue:
			[self cartesianOrigin y - (self cursorPoint y)]
		ifFalse:
			[0]! !


!Player methodsFor: 'misc' stamp: 'sw 4/24/2003 23:57'!
grabPlayerIn: aWorld
	"Invoked from a Viewer: rip my morph out of its container, wherever that may be, and place it in the hand, being careful to set things up so that if the subsequent drop is rejected, the morph will end up in a visible location on the screen"

	| aMorph newPosition |
	ActiveHand releaseMouseFocus.
	(aMorph _ self costume) visible: true.
	newPosition _ ActiveHand position - (aMorph extent // 2).
	aMorph isInWorld
		ifTrue:
			[aMorph goHome.
			aMorph formerPosition: aMorph positionInWorld]
		ifFalse:
			[aMorph formerPosition: aWorld center].
	aMorph formerOwner: ActiveWorld.
	aMorph position: newPosition.

	ActiveHand targetOffset: aMorph position - ActiveHand position.
	ActiveHand addMorphBack: aMorph.! !


!Presenter methodsFor: 'standardPlayer etc' stamp: 'sw 1/20/2004 20:08'!
positionStandardPlayer
	"Put the standard player slightly off-screen"

	standardPlayer ifNotNil:
		[standardPlayer costume position: (associatedMorph topLeft - (13@0))]! !


!Project methodsFor: 'menu messages' stamp: 'sw 1/20/2004 19:34'!
finalEnterActions
	"Perform the final actions necessary as the receiver project is entered"

	| navigator armsLengthCmd navType thingsToUnhibernate |

	self projectParameters 
		at: #projectsToBeDeleted 
		ifPresent: [ :projectsToBeDeleted |
			self removeParameter: #projectsToBeDeleted.
			projectsToBeDeleted do: [ :each | 
				Project deletingProject: each.
				each removeChangeSetIfPossible]].

	thingsToUnhibernate _ world valueOfProperty: #thingsToUnhibernate ifAbsent: [#()].
	thingsToUnhibernate do: [:each | each unhibernate].
	world removeProperty: #thingsToUnhibernate.

	navType _ ProjectNavigationMorph preferredNavigator.
	armsLengthCmd _ self parameterAt: #armsLengthCmd ifAbsent: [nil].
	navigator _ world findA: navType.
	(Preferences classicNavigatorEnabled and: [Preferences showProjectNavigator and: [navigator isNil]]) ifTrue:
		[(navigator _ navType new)
			bottomLeft: world bottomLeft;
			openInWorld: world].
	navigator notNil & armsLengthCmd notNil ifTrue:
		[navigator color: Color lightBlue].
	armsLengthCmd notNil ifTrue:
		[Preferences showFlapsWhenPublishing
			ifFalse:
				[self flapsSuppressed: true.
				navigator ifNotNil:	[navigator visible: false]].
		armsLengthCmd openInWorld: world].
	Smalltalk isMorphic ifTrue:
		[world reformulateUpdatingMenus.
		world presenter positionStandardPlayer].

	WorldState addDeferredUIMessage: [self startResourceLoading].! !


!StringMorphEditor methodsFor: 'display' stamp: 'sw 4/20/2003 15:46'!
initialize
	"Initialize the receiver.  Give it a white background"

	super initialize.
	self backgroundColor: Color white.
	self color: Color red! !


!TileMorph methodsFor: 'initialization' stamp: 'sw 3/3/2004 16:28'!
retargetFrom: oldPlayer to: newPlayer
	"Change the receiver so that if formerly it referred to oldPlayer, it refers to newPlayer instead"

	| newLabel |
	(type == #objRef  and: [actualObject == oldPlayer]) ifTrue:
		[actualObject _ newPlayer.
		newLabel _ actualObject externalName.
		self isPossessive ifTrue:
			[newLabel _ newLabel, '''s' translated].
		self line1: newLabel]! !

