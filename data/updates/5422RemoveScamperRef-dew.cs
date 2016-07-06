'From Squeak3.6gamma of ''11 September 2003'' [latest update: #5420] on 23 September 2003 at 1:01:03 am'!"Change Set:		RemoveScamperRef-dewDate:			23 September 2003Author:			Doug WayRemove Undeclared reference to Scamper from SMLoader>>help.  (Future versions of SMLoader will need to implement this or a similar fix.)"!!SMLoader methodsFor: 'gui building' stamp: 'dew 9/23/2003 00:50'!help
	(self confirm: 'Welcome to the SqueakMap package loader. 
The names of packages are followed by (installed version -> latest version). 
If there is no arrow, your installed version of the package is the latest.
The checkbox menu items at the bottom let you modify which packages 
you''ll see. Take a look at them - only some packages are shown initially. 
The options available for a package depend on how it was packaged. 
If you like a package or have comments on it, please contact
the author or the squeak mailing list.
 
Would you like to view more detailed help on the SqueakMap swiki page?') ifTrue:	[WebBrowser default		ifNil: [self inform: 'Couldn''t find a web browser.  See http://minnow.cc.gatech.edu/squeak/2726']		ifNotNil:			[WebBrowser default openOnUrl: 'http://minnow.cc.gatech.edu/squeak/2726' asUrl]]! !