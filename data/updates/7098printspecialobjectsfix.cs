'From Squeak3.10alpha of 30 March 2007 [latest update: #7096] on 8 May 2007 at 6:37:35 pm'!"Reporter: loewisSummary: 0006340: Smalltalk specialObjectsArray fails to printDescription: Smalltalk specialObjectsArray fails to print and inspect, because object 36 is a MethodContext with a nil method. This change detects that case and processes it appropriately"!"To follow this seehttp://bugs.squeak.org/view.php?id= 6340"ReleaseBuilderFor3dot10 new updatePackages: 'Kernel-edc.155(154).mcd'!