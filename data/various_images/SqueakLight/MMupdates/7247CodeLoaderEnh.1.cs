'From MinimalMorphic of 8 December 2006 [latest update: #7246] on 9 December 2009 at 7:31:08 am'!!CodeLoader commentStamp: '<historical>' prior: 0!CodeLoader provides a simple facility for loading code from the network.Examples:	| loader |	loader _ CodeLoader new.	loader baseURL:'http://isgwww.cs.uni-magdeburg.de/~raab/test/'.	loader localCache: #('.cache' 'source').	"Sources and segments can be loaded in parallel"	loader loadSourceFiles: #('file1.st' 'file2.st.gz').	loader localCache: #('.cache' 'segments').	loader loadSegments: #('is1.extseg' 'is2.extseg.gz').	"Install sources first - will wait until the files are actually loaded"	loader installSourceFiles.	"And then the segments"	loader installSegments.!!CodeLoader methodsFor: 'installing' stamp: 'edc 9/17/2009 18:24'!installMonticelloFor: aList 	"Install the previously loaded source files"	aList		do: [:packName | self lookMonticelloVersion: packName].	sourceFiles := nil! !!CodeLoader methodsFor: 'installing' stamp: 'edc 9/17/2009 18:24'!lookMonticelloVersion: packageName 	| mcw montiNames package version |	mcw := MCWorkingCopyBrowser new				repository: (MCHttpRepository						location: baseURL						user: 'squeak'						password: 'squeak').	mcw repository		ifNotNilDo: [:repos | montiNames := repos readableFileNames].	package := montiNames				detect: [:any | any = packageName]				ifNone: [].	package		ifNotNil: [Utilities				informUser: 'Installing ' , packageName printString				during: [version := mcw repository loadVersionFromFileNamed: package.					version load]].	MCPackageManager		managersForCategory: packageName		do: [:wc | wc repositoryGroup				addRepository: (MCHttpRepository new location: baseURL)]! !!CodeLoader class methodsFor: 'utilities' stamp: 'ads 7/31/2003 14:00'!signFilesFrom: sourceNames to: destNames key: privateKey
	"Sign all the given files using the private key.
	This will add an 's' to the extension of the file."
	"| fd oldNames newNames |
	fd _ FileDirectory default directoryNamed:'unsigned'.
	oldNames _ fd fileNames.
	newNames _ oldNames collect:[:name| 'signed', FileDirectory slash, name].
	oldNames _ oldNames collect:[:name| 'unsigned', FileDirectory slash, name].
	CodeLoader
		signFilesFrom: oldNames
		to: newNames
		key: DOLPrivateKey."
	| dsa |
	dsa _ DigitalSignatureAlgorithm new.
	dsa initRandomNonInteractively.
	'Signing files...' displayProgressAt: Sensor cursorPoint
		from: 1 to: sourceNames size during:[:bar|
			1 to: sourceNames size do:[:i|
				bar value: i.
				self signFile: (sourceNames at: i) renameAs: (destNames at: i) key: privateKey dsa: dsa]].
! !