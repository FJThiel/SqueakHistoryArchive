'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5247] on 9 June 2003 at 6:01:28 pm'!"Change Set:		SMLoader-dvfDate:			9 June 2003Author:			Daniel VainsencherThe purpose of this SqueakMap (SM) Package Loader is to be a minimal practical tool to see what SqueakMap packages are available and relevant, and install them easily.(This includes a postscript which registers this version of SMLoader (0.98) as installed. -dew)"!SystemWindow subclass: #SMLoader	instanceVariableNames: 'squeakMap packagesList packagesListIndex downloadDirectory filter '	classVariableNames: ''	poolDictionaries: ''	category: 'SM-Loader'!!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 14:52'!addFiltersToMenu: aMenu
	| filterSymbol help |
	self filterSpecs do: [:filterArray | 
		filterSymbol _ filterArray second.
		help _ filterArray third.
		aMenu addUpdating: #showFilterString: target: self selector: #toggleFilterState: argumentList: (Array with: filterSymbol).
		aMenu balloonTextForLastItem: help]

	! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 16:04'!generalOptions
	^#(#('help' #help) 
		#('update map from the net' loadUpdates)
		#('upgrade all installed packages' upgradeInstalledPackages)
		#- 
		#('set download directory' #setDownloadDirectory 'Set the directory where downloaded packages will be placed. If not set, it is the Squeak default directory') 
		#- )

! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 14:46'!packageList
	^(self packages) select: [:e | self filter allSatisfy: [:currFilter | 
		(self perform: currFilter) value: e]]! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/2/2002 23:21'!packageNameList
	^self packageList collect: [:e | e name, ' (', e versionLabel , ')']! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/2/2002 22:48'!packageSpecificOptions
	| choices package |
	package _ self selectedPackage.
	choices _ OrderedCollection new.
		package isDownloadable ifTrue: [
			choices add: #('download' downloadPackage 'Simply download the current release of the selected package.').
			package isInstallable ifTrue:[
				choices add: #('install'	installPackage	'Download and install package')	].
			choices add: #-.].
	^choices! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 11:24'!packagesMenu: aMenu 
	"Answer the packages-list menu"

	| choices |
	choices := OrderedCollection new.
	self selectedPackage 
		ifNotNil: [choices addAll: self packageSpecificOptions].
	choices addAll: self generalOptions.
	aMenu addList: choices.
	self addFiltersToMenu: aMenu.
	^aMenu! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 9/20/2002 21:33'!addPackagesTo: window at: fractions plus: verticalOffset
	"Add the list for packages, and answer the verticalOffset plus the height added"

	| divider listMorph |
	listMorph _ self buildMorphicPackagesList.
	listMorph borderWidth: 0.
	divider _ BorderedSubpaneDividerMorph forBottomEdge.
	Preferences alternativeWindowLook ifTrue:[
		divider extent: 4@4; color: Color transparent; borderColor: #raised; borderWidth: 2.
	].
	window 
		addMorph: listMorph
		
	! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 10/2/2002 22:39'!buildMorphicPackagesList
	| list |
	(list _ PluggableListMorph new) 
			on: self list: #packageNameList
			selected: #packagesListIndex changeSelected: #packagesListIndex:
			menu: #packagesMenu: keystroke: #packagesListKey:from:.
	^list! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 10/25/2002 15:55'!buildPackagePane

	| ptm |
	ptm _ PluggableTextMorph 
		on: self 
		text: #contents
		accept: nil
		readSelection: nil "#packageSelection "
		menu: nil.
	ptm lock.
	^ptm! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 11/20/2002 23:37'!createWindow
	self addMorph: (self buildMorphicPackagesList borderWidth: 0)
		frame: (0 @ 0 corner: 0.4 @ 1).
	self addMorph: (self buildPackagePane borderWidth: 0)
		frame: (0.4 @ 0 corner: 1.0 @ 1.0).
	self 
		on: #mouseEnter
		send: #paneTransition:
		to: self.
	self 
		on: #mouseLeave
		send: #paneTransition:
		to: self.
	self setUpdatablePanesFrom: #(#packageNameList #selectedPackagesList).
	self setLabel: 'SM Package Loader'! !!SMLoader methodsFor: 'gui building' stamp: 'dew 10/28/2002 12:43'!help
	(self confirm: 'Welcome to the SqueakMap package loader. 
The names of packages are followed by (installed version -> latest version). 
If there is no arrow, your installed version of the package is the latest.
The checkbox menu items at the bottom let you modify which packages 
you''ll see. Take a look at them - only some packages are shown initially. 
The options available for a package depend on how it was packaged. 
If you like a package or have comments on it, please contact
the author or the squeak mailing list.
 
Would you like to view more detailed help on the SqueakMap swiki page?') 
	ifTrue: [Scamper openOnUrl: 'http://minnow.cc.gatech.edu/squeak/2726' asUrl]! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 10/31/2002 11:31'!paneColor
	^ Color yellow muchLighter duller! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 9/20/2002 23:18'!perform: selector orSendTo: otherTarget 
	"Selector was just chosen from a menu by a user. If can respond, then  
	perform it on myself. If not, send it to otherTarget, presumably the  
	editPane from which the menu was invoked."
	(self respondsTo: selector)
		ifTrue: [^ self perform: selector]
		ifFalse: [^ super perform: selector orSendTo: otherTarget]! !!SMLoader methodsFor: 'model' stamp: 'dvf 9/20/2002 21:17'!contents
	| package |
	package _ self selectedPackage.
	^package ifNil: ['<No package selected>'] ifNotNil: [package fullDescription]
! !!SMLoader methodsFor: 'model' stamp: 'gh 10/22/2002 17:27'!downloadPackage
	"Download package."

	| installer |
	Cursor wait showWhile: [
		installer _ SMInstaller forPackage: self selectedPackage directory: downloadDirectory.
		(installer download)
			ifFalse:[self inform: 'Download of package ', self selectedPackage name, ' failed.']]! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/24/2002 22:15'!installPackage
	"Install package."

	| installer |
	installer _ SMInstaller forPackage: self selectedPackage directory: downloadDirectory.
	(installer install)
		ifFalse:[self inform: 'Install of package ', self selectedPackage name, ' failed.'].
	self noteChanged! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/25/2002 00:36'!loadUpdates
	squeakMap loadUpdates.
	self noteChanged ! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/25/2002 14:47'!on: aSqueakMap 
	squeakMap := aSqueakMap.
	self loadUpdates.
	downloadDirectory := FileDirectory default.
	filter := OrderedCollection new.
	"Initial configuration -"
	self
		filterAdd: #filterAutoInstall;
		filterAdd: #filterNotUptoDate;
		filterAdd: #filterVersion! !!SMLoader methodsFor: 'model' stamp: 'gh 10/22/2002 11:13'!packages
	"We request the cards as sorted by name by default."

	^squeakMap cardsByName asArray
! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/19/2002 00:58'!selectedPackage
	^(self packagesListIndex = 0) | (self packageList size < self packagesListIndex) ifFalse: [self packageList at: self packagesListIndex] ifTrue: [nil]! !!SMLoader methodsFor: 'model' stamp: 'dvf 9/20/2002 23:26'!setDownloadDirectory
	"Set the directory for downloads."

	downloadDirectory _ FileList2 modalFolderSelector: (downloadDirectory ifNil: [FileDirectory default])! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/31/2002 21:09'!upgradeInstalledPackages
	squeakMap upgradeOldPackagesDirectory: downloadDirectory.
	self noteChanged! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 9/20/2002 23:16'!filter
	^filter! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 10/18/2002 17:02'!filter: anObject 
	"update my selection"

	| oldPackage index |
	oldPackage := self selectedPackage.
	filter := anObject.
	self packagesListIndex: ((index := self packageList indexOf: oldPackage) 
				ifNil: [0]
				ifNotNil: [index]).
	self noteChanged! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 11/20/2002 23:26'!noteChanged
	self changed: #packageNameList.
	self changed: #packagesListIndex.	"update my selection"
	self contentsChanged.! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 9/20/2002 21:13'!packagesListIndex
	^packagesListIndex ifNil: [1]! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 11/20/2002 23:26'!packagesListIndex: anObject

	packagesListIndex := anObject.
	self changed: #packagesListIndex.	"update my selection"
	self contentsChanged! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 9/20/2002 23:44'!filterAdd: anObject

	self filter: (self filter copyWith: anObject)
! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:46'!filterRemove: anObject

	self filter: (self filter copyWithout: anObject)
! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 15:52'!filterSpecs
	^#(#('display only packages for current version' #filterVersion 'display only packages designed to work with your Squeak version') 
	#('display only auto-installable packages' #filterAutoInstall 'display only packages that can install automatically')
	#('display new or newer packages' #filterNotUptoDate 'display packages that are not installed or that have been updated.')
	#('display installed packages' #filterInstalled 'display packages that are installed.'))! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 11:27'!labelForFilter: aFilterSymbol 
	^(self filterSpecs detect: [:fs | fs second = aFilterSymbol]) first! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:48'!showFilterString: aFilterSymbol 
	^(self stateForFilter: aFilterSymbol), (self labelForFilter: aFilterSymbol)! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:45'!stateForFilter: aFilterSymbol 
	^(self filter includes: aFilterSymbol) ifTrue: ['<yes>'] ifFalse: ['<no>']

! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:45'!toggleFilterState: aFilterSymbol 

	^(self filter includes: (aFilterSymbol)) 
		ifTrue: [self filterRemove: aFilterSymbol]
		ifFalse: [self filterAdd: aFilterSymbol]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 11:29'!filterAutoInstall
	^[:package | package isInstallable]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:08'!filterInstalled
	^[:package | package isInstalled]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:08'!filterNotInstalledYet
	^[:package | package isInstalled not]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:07'!filterNotUptoDate
	
	^[:package | package isAvailable]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 11/20/2002 23:44'!filterVersion
	"Ignore spaces in the version string, they're sometimes spurious."
	^[:package | package categories anySatisfy:  
		[:cat | (cat name, '*') match: (Smalltalk version copyWithout: $ ) ]]! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:54'!initialize
	
	 (TheWorldMenu respondsTo: #registerOpenCommand:)
         ifTrue: [TheWorldMenu registerOpenCommand: {'Package Loader'. {self. #open}}].
! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:55'!removeFromSystem

		(TheWorldMenu respondsTo: #registerOpenCommand:) ifTrue: 
			[TheWorldMenu unregisterOpenCommand: 'Package Loader'].
		super removeFromSystem! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:55'!unload
	(TheWorldMenu respondsTo: #registerOpenCommand:) ifTrue: 
		[TheWorldMenu unregisterOpenCommand: 'Package Loader'].! !!SMLoader class methodsFor: 'as yet unclassified' stamp: 'dvf 9/20/2002 20:52'!new
	"Create a browser on the default map."

	^self newOn: SMSqueakMap default! !!SMLoader class methodsFor: 'as yet unclassified' stamp: 'dvf 9/20/2002 20:52'!newOn: aMap
	"Create a browser on <aMap>."

	^super new on: aMap; yourself! !!SMLoader class methodsFor: 'as yet unclassified' stamp: 'dvf 9/20/2002 20:53'!open
	"Create and open a SqueakMap Loader."
	"self open"

	(self new) createWindow; openInWorld! !!SMLoader class methodsFor: 'as yet unclassified' stamp: 'dvf 9/20/2002 20:53'!openOn: aSqueakMap
	"Create and open a SqueakMap Loader on a given map."

	"self openOn: SqueakMap default"

	(self newOn: aSqueakMap) createWindow; openInWorld! !SMLoader initialize!"Postscript:""Register package 'SM Package Loader' 0.98 as installed"SMSqueakMap default noteInstalledPackage:  '047a3b12-7e52-4c5d-be8b-d06635fc4f1c' version: '0.98'.!