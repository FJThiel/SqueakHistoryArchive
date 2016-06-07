"This is adapted from LPF.st to just install Installer and Monticello
1.5, and not any of the other stuff that LPF does"
| version wc |

"First we obtain a reasonable version of Installer"
Transcript show: 'Installer bootstrap - Loading http://installer.pbwiki.com/f/Installer.st'; cr.

wc := Smalltalk at: #MCWorkingCopy ifAbsent: [ nil ].

wc ifNotNil: [ 
	(wc allManagers select:  [:each | each packageName = 'Installer-Core' ]) 
		do: [ :ea | ea unregister ] 
].

SystemOrganization removeSystemCategory: #'Installer-Core'.
(HTTPSocket httpGet: 'installer.pbwiki.com/f/Installer.st') readStream fileIn!

Transcript show: 'Installer bootstrap - Loaded '.

Transcript show: 'Begin "Operation Level Playing Field"...'.

Installer install: 'LevelPlayingField-Preamble'.
Installer install: 'LevelPlayingField'. 

