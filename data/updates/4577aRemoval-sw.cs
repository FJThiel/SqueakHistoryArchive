'From Squeak3.2alpha of 3 October 2001 [latest update: #4576] on 3 December 2001 at 9:19:28 pm'!"Change Set:		aRemoval-swDate:			3 December 2001Author:			Scott WallaceRemoves a method in Preferences that was no longer in use but which had attracted undeserved attention, and the class variable that it was the only user of"!Preferences class removeSelector: #initializeSyntaxColorsAndStyles!Object subclass: #Preferences	instanceVariableNames: ''	classVariableNames: 'DesktopColor DictionaryOfPreferences Parameters '	poolDictionaries: ''	category: 'System-Support'!