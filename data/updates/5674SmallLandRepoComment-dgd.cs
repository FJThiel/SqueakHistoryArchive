'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5657] on 26 January 2004 at 12:41:40 pm'!"Change Set:		SmallLandRepository-commentDate:			26 January 2004Author:			Marcus DenkerThis just adds a comment to PRServerDirectoryto be filed in *after* SmallLandRepository-dgd"!!PRServerDirectory commentStamp: 'md 1/26/2004 12:40' prior: 0!Add support to publish or download projects from Small-Land ProjectRepository (SLPR).The SLPR has virtual folders where the projects appears.  The SLPR canbe acceded from the FileList or from the web interface athttp://repository.small-land.org:8080Basically it's a type of superswiki (but better ;)).The features in SMPR not present in SuperSwiki are:- Both the web interface and the squeak-side interface are fulltranslatable.   The server has translations for English and Spanish justnow, but it's almost trivial to include other translations... Stef?Marcus? ;)- The projects are categorized in "virtual" folder.  These folders (ByCategory, By Author, By Language, Alphabetical, etc) give us goodsearching behaviour just using the FileList and mouse clicks.- The web interface (also full translatable) has a search a la google.- All the urls to query the web interface are "clean enough" so googlecan make a good job indexing our content in .pr files.It's planned to add "editing" features to the web interface tore-categorize, remove, etc projects.Enjoy it,-- Diego Gomez Deckhttp://www.small-land.org!