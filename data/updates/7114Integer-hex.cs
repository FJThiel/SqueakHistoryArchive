'From Squeak3.10alpha of 30 March 2007 [latest update: #7113] on 25 June2007 at 8:44:56 am'!"Reporter: johnpfSummary: 0006441: [BUG] SmallInteger hex not understoodDescription: message hex is understood by Character and Float.  The wikipage http://wiki.squeak.org/squeak/2137 [^] includes the code i hexwhere i is a SmallInteger.$a hex.is not understood though, as SmallInteger doesn't understand it.A lazy fix is to addSmallInteger>>hex^self asFloat hex.but this seems wrong, as any Integer should understand it, and possiblyany Number, especially as if a Float understands it!!Tested in 3.9 and 3.10a (build 7081), fails in both."!ReleaseBuilderFor3dot10 new updatePackages: 'Kernel-edc.158(157).mcd'!