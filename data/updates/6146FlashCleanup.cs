'From Squeak3.6beta of ''4 July 2003'' [latest update: #5395] on 11 August 2003 at 10:02:43 pm'!"Change Set:		FlashCleanupDate:			31 July 2003Author:			Adam Spitzmd: updated to take the FlashProgressBar changes in accountRecategorizes some stuff in order to turn the Flash code into a single PackageInfo package with no external dependents."CurrentProjectRefactoring class organization classifyAll: { #updateProjectFillsIn:. } under: '*balloon-MMFlash'.Smalltalk organization renameCategory: 'Balloon-MMFlash Morphs' toBe: 'Balloon-MMFlash-Morphs'.Smalltalk organization renameCategory: 'Balloon-MMFlash Import' toBe: 'Balloon-MMFlash-Import'.Smalltalk organization renameCategory: 'Balloon-MMFlash Support' toBe: 'Balloon-MMFlash-Support'.!