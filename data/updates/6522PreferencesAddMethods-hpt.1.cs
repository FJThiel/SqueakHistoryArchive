'From Squeak3.9alpha of ''2 November 2004'' [latest update: #6485] on 5 December 2004 at 1:32:55 pm'!"Change Set:		PreferencesAddMethods-hptDate:			5 December 2004Author:			Hernan TylimIt just adds two methods that I forgot to put when I made the original changeset. These methods are for adding boolean preference specifying a changeInformee object.Note that this changeset is required for old packages to install correctly on 3.9, an example of such package is Shout. "!!Preferences class methodsFor: 'add preferences' stamp: 'hpt 12/5/2004 13:28'!addBooleanPreference: prefSymbol categories: categoryList default: aValue balloonHelp: helpString projectLocal: localBoolean changeInformee: informeeSymbol  changeSelector: aChangeSelector	"Add an item repreesenting the given preference symbol to the system. Default view for this preference is boolean"	self addPreference: prefSymbol  categories: categoryList default:  aValue balloonHelp: helpString  projectLocal: localBoolean  changeInformee: informeeSymbol changeSelector: aChangeSelector viewRegistry: PreferenceViewRegistry ofBooleanPreferences ! !!Preferences class methodsFor: 'add preferences' stamp: 'hpt 12/5/2004 13:29'!addPreference: prefSymbol categories: categoryList default: aValue balloonHelp: helpString projectLocal: localBoolean changeInformee: informeeSymbol  changeSelector: aChangeSelector	"Add an item representing the given preference symbol to the system. Default view for this preference is boolean to keep backward compatibility"	self addBooleanPreference: prefSymbol categories: categoryList default: aValue balloonHelp: helpString projectLocal: localBoolean changeInformee: informeeSymbol  changeSelector: aChangeSelector! !