'From SqueakLight|II of 31 May 2008 [latest update: #7215] on 12 January 2009 at 7:24:50 am'!!CodeLoader methodsFor: 'installing' stamp: 'edc 12/24/2008 08:15'!installLastMonticelloFor: aList	"Install the previously loaded source files"	aList		do: [:packName | 								self lookLastVersion: packName].	sourceFiles := nil! !!CodeLoader methodsFor: 'installing' stamp: 'edc 12/24/2008 10:26'!lookLastVersion: packageName 	| mcw montiNames package version |	mcw := MCWorkingCopyBrowser new				repository: (MCHttpRepository						location: baseURL						user: 'squeak'						password: 'squeak').	mcw repository		ifNotNilDo: [:repos | montiNames := repos readableFileNames].	package := montiNames				detect: [:any | any beginsWith: packageName]				ifNone: [].	package		ifNotNil: [Utilities				informUser: 'Installing ' , packageName printString				during: [version := mcw repository loadVersionFromFileNamed: package.					version load]].	MCPackageManager		managersForCategory: packageName		do: [:wc | wc repositoryGroup				addRepository: (MCHttpRepository new location: baseURL)]! !