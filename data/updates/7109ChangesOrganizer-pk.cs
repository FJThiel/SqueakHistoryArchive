'From Squeak3.10alpha of 30 March 2007 [latest update: #7108] on 24 June 2007 at 10:16:11 am'!!MCRepositoryGroup methodsFor: 'as yet unclassified' stamp: 'edc 6/24/2007 09:50'!versionWithInfo: aVersionInfo	^self versionWithInfo: aVersionInfo ifNone: [ repositories add: ReleaseBuilderFor3dot10 new repository ]! !ReleaseBuilderFor3dot10 new loadTogether: #( 'Compression-edc.9.mcz''ScriptLoader-edc.325.mcz''SystemChangeNotification-Tests-edc.8.mcz''EToys-bf.24.mcz') merge: false.ReleaseBuilderFor3dot10 new updatePackages: 'Files-edc.21(20).mcdMonticello-edc.312(311).mcdSMBase-edc.87(86).mcdSystem-edc.105(104).mcdTests-edc.27(26).mcdTools-edc.80(79).mcdUniverses-edc.22(ls.21).mcd'!