'From Squeak3.6alpha of ''17 March 2003'' [latest update: #5305] on 2 July 2003 at 10:36:47 am'!"Change Set:		RemoveSqCInternalUpdates-nkDate:			2 July 2003Author:			Ned KonzThis removes the SqC internal updates stream from the list of update URLs.This CS is otherwise empty."Utilities updateUrlLists removeAllSuchThat: [ :ea | ea first = 'SqC Internal Updates*' ]!