'From Squeak3.8alpha of 8 September 2004 [latest update: #5987] on 11 September 2004 at 12:20:50 pm'!"Change Set:		seeUnstableUpdates-mdDate:			11 Sept 2004Author:			md File this in and your image will subsequently be able to update from the unstable update stream.   When confronted with the prompt for which update server to use, choose the one named 'updates-unstable' "!!Utilities class methodsFor: 'fetching updates' stamp: 'md 9/11/2004 12:06'!assureAbsenceOfUnstableUpdateStream	"Check to see if the unstable Updates stream is in the list; if it is, *remove* it.  This is the *opposite* of #assureAvailabilityOfUnstableUpdateStream"	UpdateUrlLists ifNil: [UpdateUrlLists _ OrderedCollection new].	UpdateUrlLists _ UpdateUrlLists select:		[:pair | pair first ~= 'Unstable Updates*']"Utilities assureAbsenceOfUnstableUpdateStream"! !!Utilities class methodsFor: 'fetching updates' stamp: 'md 9/11/2004 12:07'!assureAvailabilityOfUnstableUpdateStream	"Check to see if the unstable Updates stream is in the list; if not, add it"	UpdateUrlLists ifNil: [UpdateUrlLists _ OrderedCollection new].	UpdateUrlLists do:		[:pair | (pair first =  'Unstable Updates*') ifTrue: [^ self]].	UpdateUrlLists addFirst: #('Unstable Updates*' #('squeak.cs.uiuc.edu/Squeak2.0/' 'update.squeakfoundation.org/external/'))"Utilities assureAvailabilityOfUnstableUpdateStream"! !"Postscript:"Utilities assureAvailabilityOfUnstableUpdateStream.!