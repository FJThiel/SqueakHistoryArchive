'From Squeak3.6 of ''6 October 2003'' [latest update: #5424] on 9 November 2003 at 2:10:08 pm'!"Change Set:		MenuIconsCommentDate:			9 November 2003Author:			stephane ducasseClass comment for MenuIcons."!!MenuIcons commentStamp: 'sd 11/9/2003 14:09' prior: 0!I represent a registry for icons.  You can see the icons I contain using the following script:| dict methods |dict := Dictionary new. methods := MenuIcons class selectors select: [:each | '*Icon' match: each asString].methods do: [:each | dict at: each put: (MenuIcons perform: each)].GraphicalDictionaryMenu openOn: dict withLabel: 'MenuIcons'!