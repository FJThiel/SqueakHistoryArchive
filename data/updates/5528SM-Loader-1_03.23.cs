SystemOrganization addCategory: #'SM-Loader'!SystemWindow subclass: #SMLoader	instanceVariableNames: 'squeakMap packagesList packagesListIndex filter '	classVariableNames: ''	poolDictionaries: ''	category: 'SM-Loader'!SMLoader class	instanceVariableNames: ''!!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 14:52'!addFiltersToMenu: aMenu
	| filterSymbol help |
	self filterSpecs do: [:filterArray | 
		filterSymbol _ filterArray second.
		help _ filterArray third.
		aMenu addUpdating: #showFilterString: target: self selector: #toggleFilterState: argumentList: (Array with: filterSymbol).
		aMenu balloonTextForLastItem: help]

	! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 9/20/2002 21:33'!addPackagesTo: window at: fractions plus: verticalOffset
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
	^ptm! !!SMLoader methodsFor: 'gui building' stamp: 'ar 6/15/2003 21:12'!buildSearchPane	| typeInView |	typeInView _ PluggableTextMorph on: self 		text: nil accept: #findPackage:notifying:		readSelection: nil menu: nil.	typeInView setBalloonText:'To find a package type in a fragment of its name and hit return'.	typeInView acceptOnCR: true.	typeInView hideScrollBarIndefinitely.	^typeInView! !!SMLoader methodsFor: 'model' stamp: 'dvf 9/20/2002 21:17'!contents
	| package |
	package _ self selectedPackage.
	^package ifNil: ['<No package selected>'] ifNotNil: [package fullDescription]
! !!SMLoader methodsFor: 'gui building' stamp: 'gk 10/1/2003 10:47'!createWindow
	self addMorph: (self buildSearchPane borderWidth: 0)
		frame: (0@0 corner: 0.4@0.07).
	self addMorph: (self buildMorphicPackagesList borderWidth: 0)
		frame: (0 @ 0.07 corner: 0.4 @ 1).
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
	self setUpdatablePanesFrom: #(#packageNameList).
	self setLabel: 'SM Package Loader'! !!SMLoader class methodsFor: 'preferences' stamp: 'nk 6/24/2003 16:24'!downloadDirectory	"Answer my default download directory. "	^FileDirectory default directoryNamed: self downloadDirectoryName! !!SMLoader class methodsFor: 'preferences' stamp: 'nk 6/24/2003 16:25'!downloadDirectoryName	"Answer the name of my default download directory.	This can be a relative name.	Default to the current directory for compatibility with old behavior"	^Preferences parameterAt: #SMLoaderDownloadDirectoryName		ifAbsentPut: [ FileDirectory default fullName ].! !!SMLoader class methodsFor: 'preferences' stamp: 'nk 6/24/2003 16:25'!downloadDirectoryName: dir	"Set the name of my default download directory to dir, which can be relative."	^Preferences setParameter: #SMLoaderDownloadDirectoryName		to: (FileDirectory default fullNameFor: dir)! !!SMLoader methodsFor: 'model' stamp: 'gk 9/30/2003 16:41'!downloadPackage
	"Download package."

	[Cursor wait showWhile: [
		(SMInstaller forPackage: self selectedPackage
					directory: self class downloadDirectory) download]
	] on: Error do: [:ex |
		self inform: ('Error occurred during download:\', ex messageText) withCRs]! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 9/20/2002 23:16'!filter
	^filter! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 10/18/2002 17:02'!filter: anObject 
	"update my selection"

	| oldPackage index |
	oldPackage := self selectedPackage.
	filter := anObject.
	self packagesListIndex: ((index := self packageList indexOf: oldPackage) 
				ifNil: [0]
				ifNotNil: [index]).
	self noteChanged! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 9/20/2002 23:44'!filterAdd: anObject

	self filter: (self filter copyWith: anObject)
! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 11:29'!filterAutoInstall
	^[:package | package isInstallable]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:08'!filterInstalled
	^[:package | package isInstalled]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:08'!filterNotInstalledYet
	^[:package | package isInstalled not]! !!SMLoader methodsFor: 'filters' stamp: 'dvf 10/25/2002 17:07'!filterNotUptoDate
	
	^[:package | package isAvailable]! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:46'!filterRemove: anObject

	self filter: (self filter copyWithout: anObject)
! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 15:52'!filterSpecs
	^#(#('display only packages for current version' #filterVersion 'display only packages designed to work with your Squeak version') 
	#('display only auto-installable packages' #filterAutoInstall 'display only packages that can install automatically')
	#('display new or newer packages' #filterNotUptoDate 'display packages that are not installed or that have been updated.')
	#('display installed packages' #filterInstalled 'display packages that are installed.'))! !!SMLoader methodsFor: 'filters' stamp: 'dvf 11/20/2002 23:44'!filterVersion
	"Ignore spaces in the version string, they're sometimes spurious."
	^[:package | package categories anySatisfy:  
		[:cat | (cat name, '*') match: (Smalltalk version copyWithout: $ ) ]]! !!SMLoader methodsFor: 'model' stamp: 'nk 6/24/2003 06:27'!findPackage: aString notifying: aView	"search and select a package with the given (sub) string"	| index list match |	match := aString asString asLowercase.	index := self packagesListIndex.	list := self packageNameList.	list isEmpty ifTrue: [ ^self ].	index+1 to: list size do:[:i|		((list at: i) asLowercase includesSubString: match) 			ifTrue:[^self packagesListIndex: i].	].	"wrap around"	1 to: index do:[:i|		((list at: i) asLowercase includesSubString: match) 			ifTrue:[^self packagesListIndex: i].	].	self inform: 'No package matching ', aString asString.! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 16:04'!generalOptions
	^#(#('help' #help) 
		#('update map from the net' loadUpdates)
		#('upgrade all installed packages' upgradeInstalledPackages)
		#- 
		#('set download directory' #setDownloadDirectory 'Set the directory where downloaded packages will be placed. If not set, it is the Squeak default directory') 
		#- )

! !!SMLoader methodsFor: 'gui building' stamp: 'nk 6/27/2003 05:42'!help
	"Present help text. If there is a web server available, offer to open it.
	Use the WebBrowser registry if possible, or Scamper if available."
	| message browserClass |
	message _ 'Welcome to the SqueakMap package loader. 

The names of packages are followed by (installed version -> latest version). 

If there is no arrow, your installed version of the package is the latest.

The checkbox menu items at the bottom let you modify which packages 

you''ll see. Take a look at them - only some packages are shown initially. 

The options available for a package depend on how it was packaged. 

If you like a package or have comments on it, please contact

the author or the squeak mailing list.'.


	browserClass _ Smalltalk at: #WebBrowser ifPresent: [ :registry | registry default ].
	browserClass _ browserClass ifNil: [ Smalltalk at: #Scamper ifAbsent: [ ^self inform: message ]].

	(self confirm: message, '

Would you like to view more detailed help on the SqueakMap swiki page?') 

	ifTrue: [ browserClass openOnUrl: 'http://minnow.cc.gatech.edu/squeak/2726' asUrl]! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:54'!initialize
	
	 (TheWorldMenu respondsTo: #registerOpenCommand:)
         ifTrue: [TheWorldMenu registerOpenCommand: {'Package Loader'. {self. #open}}].
! !!SMLoader methodsFor: 'model' stamp: 'gk 9/30/2003 16:27'!installPackage
	"Install package."

	[Cursor wait showWhile: [
		(SMInstaller forPackage: self selectedPackage
					directory: self class downloadDirectory) install.
		self noteChanged]
	] on: Error do: [:ex |
		self inform: ('Error occurred during install:\', ex messageText) withCRs].
	! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 11:27'!labelForFilter: aFilterSymbol 
	^(self filterSpecs detect: [:fs | fs second = aFilterSymbol]) first! !!SMLoader methodsFor: 'model' stamp: 'gk 10/1/2003 10:24'!loadUpdates
	[Cursor wait showWhile: [
		squeakMap loadUpdates.
		self noteChanged ]
	] on: Error do: [:ex |
		self inform: ('Error occurred when updating map:\', ex messageText) withCRs]
	

! !!SMLoader class methodsFor: 'instance creation' stamp: 'dvf 9/20/2002 20:52'!new
	"Create a browser on the default map."

	^self newOn: SMSqueakMap default! !!SMLoader class methodsFor: 'instance creation' stamp: 'dvf 9/20/2002 20:52'!newOn: aMap
	"Create a browser on <aMap>."

	^super new on: aMap; yourself! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 5/24/2003 15:39'!noteChanged	packagesList _ nil.
	self changed: #packageNameList.
	self changed: #packagesListIndex.	"update my selection"
	self contentsChanged.! !!SMLoader methodsFor: 'model' stamp: 'nk 6/24/2003 16:27'!on: aSqueakMap 	squeakMap := aSqueakMap.	self loadUpdates.	filter := OrderedCollection new.	"Initial configuration -"	self		filterAdd: #filterAutoInstall;		filterAdd: #filterNotUptoDate! !!SMLoader class methodsFor: 'interface opening' stamp: 'dvf 9/20/2002 20:53'!open
	"Create and open a SqueakMap Loader."
	"self open"

	(self new) createWindow; openInWorld! !!SMLoader class methodsFor: 'interface opening' stamp: 'dvf 9/20/2002 20:53'!openOn: aSqueakMap
	"Create and open a SqueakMap Loader on a given map."

	"self openOn: SqueakMap default"

	(self newOn: aSqueakMap) createWindow; openInWorld! !!SMLoader methodsFor: 'lists' stamp: 'dvf 5/24/2003 15:41'!packageList
	^packagesList ifNil: [packagesList _ (self packages) select: [:e | 		self filter allSatisfy: [:currFilter | (self perform: currFilter) value: e]]]! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/2/2002 23:21'!packageNameList
	^self packageList collect: [:e | e name, ' (', e versionLabel , ')']! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/2/2002 22:48'!packageSpecificOptions
	| choices package |
	package _ self selectedPackage.
	choices _ OrderedCollection new.
		package isDownloadable ifTrue: [
			choices add: #('download' downloadPackage 'Simply download the current release of the selected package.').
			package isInstallable ifTrue:[
				choices add: #('install'	installPackage	'Download and install package')	].
			choices add: #-.].
	^choices! !!SMLoader methodsFor: 'model' stamp: 'gh 10/22/2002 11:13'!packages
	"We request the cards as sorted by name by default."

	^squeakMap cardsByName asArray
! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 9/20/2002 21:13'!packagesListIndex
	^packagesListIndex ifNil: [1]! !!SMLoader methodsFor: 'accessing' stamp: 'dvf 11/20/2002 23:26'!packagesListIndex: anObject

	packagesListIndex := anObject.
	self changed: #packagesListIndex.	"update my selection"
	self contentsChanged! !!SMLoader methodsFor: 'lists' stamp: 'dvf 10/25/2002 11:24'!packagesMenu: aMenu 
	"Answer the packages-list menu"

	| choices |
	choices := OrderedCollection new.
	self selectedPackage 
		ifNotNil: [choices addAll: self packageSpecificOptions].
	choices addAll: self generalOptions.
	aMenu addList: choices.
	self addFiltersToMenu: aMenu.
	^aMenu! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 10/31/2002 11:31'!paneColor
	^ Color yellow muchLighter duller! !!SMLoader methodsFor: 'gui building' stamp: 'dvf 9/20/2002 23:18'!perform: selector orSendTo: otherTarget 
	"Selector was just chosen from a menu by a user. If can respond, then  
	perform it on myself. If not, send it to otherTarget, presumably the  
	editPane from which the menu was invoked."
	(self respondsTo: selector)
		ifTrue: [^ self perform: selector]
		ifFalse: [^ super perform: selector orSendTo: otherTarget]! !!SMLoader class methodsFor: 'private-publishing' stamp: 'dvf 10/9/2003 18:30'!publish	| pi versionNo packagedFileName packageFile sd initialPackagedFileName compressedPackagedFileName |	pi _ PackageInfo named: 'SM-Loader'.	versionNo := FillInTheBlank request: 'Version number for this file?'.	pi fileOut.	initialPackagedFileName := pi externalName,'.st'.	packagedFileName _ pi externalName, '.', versionNo asString, '.cs'.	FileDirectory default rename: initialPackagedFileName toBe: packagedFileName.	GZipWriteStream compressFile: packagedFileName.	compressedPackagedFileName _ packagedFileName,'.gz'.	packageFile := FileDirectory default readOnlyFileNamed: compressedPackagedFileName.	sd := ServerDirectory new.	sd		user: 'dvf';		server: 'modules.squeakfoundation.org';		password: (FillInTheBlank request: 'password?');		directory: 'Packages/';		putFile: packageFile named: compressedPackagedFileName! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:55'!removeFromSystem

		(TheWorldMenu respondsTo: #registerOpenCommand:) ifTrue: 
			[TheWorldMenu unregisterOpenCommand: 'Package Loader'].
		super removeFromSystem! !!SMLoader methodsFor: 'model' stamp: 'dvf 10/19/2002 00:58'!selectedPackage
	^(self packagesListIndex = 0) | (self packageList size < self packagesListIndex) ifFalse: [self packageList at: self packagesListIndex] ifTrue: [nil]! !!SMLoader methodsFor: 'model' stamp: 'nk 6/24/2003 16:27'!setDownloadDirectory	"Set the directory for downloads."	self class setDownloadDirectory! !!SMLoader class methodsFor: 'preferences' stamp: 'gk 10/3/2003 12:48'!setDownloadDirectory
	"SMLoader setDownloadDirectory"
	"Set the directory for downloads."

	| dir newDir |
	dir _ self downloadDirectoryName.
	newDir _ FileList2 modalFolderSelector: (FileDirectory default directoryNamed: dir).
	newDir ifNil: [^self]. "Dialog was cancelled" 
	^self downloadDirectoryName: newDir fullName.! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:48'!showFilterString: aFilterSymbol 
	^(self stateForFilter: aFilterSymbol), (self labelForFilter: aFilterSymbol)! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:45'!stateForFilter: aFilterSymbol 
	^(self filter includes: aFilterSymbol) ifTrue: ['<yes>'] ifFalse: ['<no>']

! !!SMLoader methodsFor: 'filter utilities' stamp: 'dvf 10/25/2002 14:45'!toggleFilterState: aFilterSymbol 

	^(self filter includes: (aFilterSymbol)) 
		ifTrue: [self filterRemove: aFilterSymbol]
		ifFalse: [self filterAdd: aFilterSymbol]! !!SMLoader class methodsFor: 'menu registration' stamp: 'dvf 10/31/2002 20:55'!unload
	(TheWorldMenu respondsTo: #registerOpenCommand:) ifTrue: 
		[TheWorldMenu unregisterOpenCommand: 'Package Loader'].! !!SMLoader methodsFor: 'model' stamp: 'gk 10/1/2003 10:25'!upgradeInstalledPackages
	[Cursor wait showWhile: [
		squeakMap upgradeOldPackagesDirectory: self class downloadDirectory.
		self noteChanged]
	] on: Error do: [:ex |
		self inform: ('Error occurred when upgrading old packages:\', ex messageText) withCRs]
	! !SMLoader initialize!"Postscript:""Register package 'SM Package Loader' 1.03 as installed"SMSqueakMap default noteInstalledPackage:  '047a3b12-7e52-4c5d-be8b-d06635fc4f1c' version: '1.03'.!