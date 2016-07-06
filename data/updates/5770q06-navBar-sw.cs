'From Squeak3.7alpha of 11 September 2003 [latest update: #5764] on 4 March 2004 at 3:17:53 pm'!
"Change Set:		q06-navBar-sw
Date:				3 March 2004
Author:				Scott Wallace, Michael Rueger

Merges code for the ProjectNavigationMorph from several Squeakland updates:

0156navigatorHelp-sw  of 18 April 2003
0157quitButton-sw of 21 April 2003
0185noTabsInNavBar-sw of 19 December 2003
0195navLangButton (by Michael Rueger) of 29 Feb 2004.

Changes wording of balloon help for a couple of navigator buttons, as per Kim.
Eliminates the show-tabs/hide-tabs button from the nav-bar.
Adds a button to the navigator bar for switching the etoy tile language."!


!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'mir 1/2/2004 13:54'!
chooseLanguage

	Project current chooseNaturalLanguage! !

!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'sw 3/3/2004 14:19'!
quitSqueak
	"Obtain a confirmation from the user, and if the answer is true, quite Squeak summarily"

	(self confirm: 'Are you sure you want to Quit Squeak?' translated) ifFalse: [^ self].
	
	SmalltalkImage current snapshot: false andQuit: true
! !

!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'sw 4/12/2003 20:44'!
buttonFind
	"Answer a button for finding/loading projects"

	^ self makeButton: 'FIND' balloonText: 'Click here to find a project.  Hold down this button to reveal additional options.' for: #findAProjectSimple
! !

!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'mir 1/2/2004 13:56'!
buttonLanguage
	"Answer a button for finding/loading projects"

	^ self makeButton: Project current naturalLanguage balloonText: 'Click here to choose your language.' for: #chooseLanguage
! !

!ProjectNavigationMorph methodsFor: 'the buttons' stamp: 'sw 4/12/2003 20:44'!
buttonPublish
	"Answer a button for publishing the project"

	^ self makeButton: 'PUBLISH IT!!' balloonText: 'Click here to save a project.  Hold down this button to reveal additional publishing options' for: #publishProject! !

!ProjectNavigationMorph methodsFor: 'buttons' stamp: 'mir 2/29/2004 14:53'!
makeTheButtons

	^{
		self buttonNewProject.
		self buttonShare.
		self buttonPrev.
		self buttonNext.
		self buttonPublish.
		self buttonNewer.
		self buttonTell.
		self buttonFind.
		self buttonFullScreen.
		"self buttonFlaps."
		self buttonPaint.
	},
	(
		Preferences includeSoundControlInNavigator ifTrue: [{self buttonSound}] ifFalse: [#()]
	),
	{
		self buttonLanguage.
		self buttonUndo.
		self buttonQuit.
	}
! !

"Postscript:"
Flaps replaceGlobalFlapwithID: 'Navigator'.
!

