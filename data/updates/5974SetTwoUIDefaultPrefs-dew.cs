'From Squeak3.7beta of ''1 April 2004'' [latest update: #5972] on 17 July 2004 at 12:15:12 am'!"Change Set:		SetTwoUIDefaultPrefs-dewDate:			17 July 2004Author:			Doug WayA couple of DoIts which set the default value for preferences #browseWithDragNDrop and #scrollBarsNarrow to true.  See the UI Defaults vote on SqueakPeople site.  (#scrollBarsNarrow has been set to true in the 3.7alpha/beta image for a while now.)(These defaults don't appear to be defined in source code anywhere... they're only in the Preferences dictionary.)"(Preferences preferenceAt: #browseWithDragNDrop) defaultValue: true.(Preferences preferenceAt: #scrollBarsNarrow) defaultValue: true.!