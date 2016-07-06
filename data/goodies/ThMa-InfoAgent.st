"      NAME	InfoAgent
       AUTHOR	thomas.mahler@home.ins.de (Thomas Mahler)
       URL	(none)
       FUNCTION	computer auded information retrieval
       KEYWORDS	autonomous agent information retrieval web
       ST-VERSIONS	Squeak
       PREREQUISITES	Squeak 2.5
       CONFLICTS	(none known)
       DISTRIBUTION	world
       VERSION	0.2
       DATE	11-Oct-99

SUMMARY

Computer aided information retrieval including:- tracking of Web-site changes- Browsing of changes in Web-Pagesplanned:- Meta-searchengine- Autonomous agent that searches the web for relevant information

				Thomas Mahler
"!
"      NAME     InfoAgent
       AUTHOR   thomas.mahler@home.ins.de
       URL      http://www.techno.net/pcl/tm/squeak/ThMa-InfoAgent.st
       FUNCTION Computer aided information retrieval
       ST-VERSIONS      Squeak
       PREREQUISITES    Squeak 2.5
       CONFLICTS        (none known)
       DISTRIBUTION     world
       VERSION  0.2
       DATE     11-Oct-99

SUMMARY

Computer aided information retrieval including:
- tracking of Web-site changes
- Browsing of changes in Web-Pages

planned:
- Meta-searchengine
- Autonomous agent that searches the web for relevant information


                                Thomas Mahler
"!

Object subclass: #WebDocument
	instanceVariableNames: 'title url category abstract themes ActContent CachedContent CachedTimestamp VersionHistory '
	classVariableNames: 'CacheDir '
	poolDictionaries: ''
	category: 'ThMa-InfoAgent'!
!WebDocument commentStamp: '<historical>' prior: 0!
The WebWatcher manages a pool of WebDocuments.
A WebDocument may be any file identified by a valid Url.
In Addition to the bare Url a WebDocument contains additional information for classifying the document.
The last version of the doc is cached for further analysis and  keeping track of changes of the document!
]style[(4 10 298)f1,f1LWebWatcher Comment;,f1!


!WebDocument methodsFor: 'retrieval' stamp: 'ThMa 10/8/1999 19:15'!
cache
	"Store retrieved doc in cache"
	CachedContent _ ActContent.
	CachedTimestamp _ Time dateAndTimeNow.
! !

!WebDocument methodsFor: 'retrieval' stamp: 'ThMa 10/8/1999 18:46'!
lookup
	"lookup doc on the web"
	ActContent _ (url asUrl) retrieveContents.
! !

!WebDocument methodsFor: 'retrieval' stamp: 'ThMa 9/25/1999 15:44'!
lookup: aUrl
	"lookup and remember url"
	self url: aUrl;
          lookup.! !


!WebDocument methodsFor: 'accessing' stamp: 'ThMa 10/2/1999 18:30'!
ActContent
	^ ActContent.! !

!WebDocument methodsFor: 'accessing' stamp: 'ThMa 10/8/1999 19:15'!
ActContent: aMIMEDocument
	"Set Content"
	ActContent _ aMIMEDocument.! !

!WebDocument methodsFor: 'accessing' stamp: 'tm 8/19/1999 11:38'!
CachedContent
	^ CachedContent.! !

!WebDocument methodsFor: 'accessing' stamp: 'ThMa 10/8/1999 19:14'!
CachedContent: DocContent
	"store a document in cache"
	CachedContent := DocContent.

         ! !

!WebDocument methodsFor: 'accessing' stamp: 'tm 8/18/1999 21:23'!
CachedTimeStamp: aTimestamp
	CachedTimestamp := aTimestamp.! !

!WebDocument methodsFor: 'accessing' stamp: 'ThMa 10/2/1999 18:42'!
url
	^ url.! !

!WebDocument methodsFor: 'accessing' stamp: 'ThMa 10/8/1999 19:11'!
url: aUrl
	"set url"
	url _ aUrl.! !


!WebDocument methodsFor: 'presentation' stamp: 'ThMa 10/8/1999 18:39'!
show
	"open document in scamper web browser"
     | aScamper |
     aScamper := Scamper new.
     aScamper jumpToUrl: url.
     aScamper openAsMorph setLabel: 'WebWatcher detected change for: ' , url.
	
    
    
     ! !

!WebDocument methodsFor: 'presentation' stamp: 'ThMa 10/8/1999 19:09'!
showDiffs
	"Display difference between cached and current version of a webdocument"
	| cachedStream actStream cachedDoc actDoc |
	cachedStream _ ReadStream on: (CachedContent isNil ifTrue: [''] ifFalse: [CachedContent content]).
	actStream _ ReadStream on: ActContent content.
	cachedDoc _ HtmlParser parse: cachedStream.
	actDoc _ HtmlParser parse: actStream.

	(StringHolder new textContents:
			(TextDiffBuilder buildDisplayPatchFrom: cachedDoc asHtml to: actDoc asHtml))
			openLabel: 'Changes in ', url! !


!WebDocument methodsFor: 'testing' stamp: 'ThMa 10/8/1999 23:19'!
hasChanged
	"check whether doc has changed since last caching"
     | res |
     self lookup.
     CachedContent isNil 
          ifTrue: [res _ true.]  "New docs are reported as changed on their first check"
          ifFalse: [res _ (CachedContent content = ActContent content ) not]. 
     ^ res.

	
	! !


!WebDocument methodsFor: 'FilePersistence' stamp: 'ThMa 10/8/1999 21:00'!
loadFromFile
	"retrieve document from file cache"

	 ^ self loadFromFile: (WebWatcher instance at: #cacheDir) , (Url hash asString) , '.html'! !

!WebDocument methodsFor: 'FilePersistence' stamp: 'ThMa 10/2/1999 23:21'!
loadFromFile: aFileName
	"comment stating purpose of message"

	| file |
     CachedContent isNil ifTrue: [self cache].
     file _ FileStream fileNamed: aFileName.
	file ifNil: [ self error: 'could not open file' ].
     "CachedContent on: file contentsOfEntireFile."
	CachedContent _ MIMEDocument 	contentType: (MIMEDocument guessTypeFromName: url) 
									content: ( file contentsOfEntireFile)
									url: url.
     file close.! !

!WebDocument methodsFor: 'FilePersistence' stamp: 'ThMa 10/8/1999 22:31'!
saveToFile
	"make content persistent in filesystem"

     ^ self saveToFile: (WebWatcher instance at: #cacheDir) , (Url hash asString) , '.html'	! !

!WebDocument methodsFor: 'FilePersistence' stamp: 'ThMa 10/8/1999 22:30'!
saveToFile: aFileName
	"comment stating purpose of message"

	| file |
	FileDirectory default deleteFileNamed: aFileName.
	"file _ FileStream fileNamed: (FileDirectory checkName:  aFileName fixErrors: true)."
     file _ FileStream fileNamed: aFileName.
	file ifNil: [ self error: 'could not save file' ].
	file nextPutAll: ActContent content.
	file close.
    ^ aFileName.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WebDocument class
	instanceVariableNames: ''!

!WebDocument class methodsFor: 'instance creation' stamp: 'ThMa 10/2/1999 23:34'!
new: aUrl
	"construct document from URL"
	| wd |
	wd _ WebDocument new.
	wd url: aUrl;
	    lookup.
	^ wd.
	

	! !



Object subclass: #WebWatcher
	instanceVariableNames: 'WatchedDocuments WatchingProcess Configuration '
	classVariableNames: 'Instance '
	poolDictionaries: ''
	category: 'ThMa-InfoAgent'!
!WebWatcher commentStamp: '<historical>' prior: 0!
The WebWatcher(TM) is a tool for tracking changes in WWW pages.
It maintains a list of WebDocument objects which are checked periodically for changes.
If the WebWatcher detects any changes, it will dislay the respective page in a Scamper window. 
Try WebWatcher run to start up WebWatchers main menu.
Most actions are logged to the Transcript, if you want to see what's going on try: Transcript open

If you want to browse the class just do: Browser fullOnClass: WebWatcher

Contact me if you have any comments or wishes regarding WebWatcher:
mailto:thomas.mahler@home.ins.de 
http://www.techno.net/pcl/tm/squeak!
]style[(18 1 68 11 132 7 14 14 119 15 43 31 70 32 2 35)bf3cblack;,f1cblack;,f1,f1LWebDocument Comment;,f1,f1LScamper Comment;,f1,f1dWebWatcher run;;,f1,f1dTranscript open;;,f1,f1dBrowser fullOnClass: WebWatcher;;,f1,f1Rmailto:thomas.mahler@home.ins.de;,f1,f1Rhttp://www.techno.net/pcl/tm/squeak;!


!WebWatcher methodsFor: 'DocManagement' stamp: 'ThMa 10/8/1999 18:59'!
add: aUrl
	"add aUrl to collection of watched documents"
     | doc |
     WatchedDocuments isNil ifTrue: [WatchedDocuments := Set new].
     doc := WebDocument new: aUrl.
	WatchedDocuments add: doc.! !

!WebWatcher methodsFor: 'DocManagement' stamp: 'ThMa 9/22/1999 19:57'!
docs
	"return set of all watches documents"

	^ WatchedDocuments.! !

!WebWatcher methodsFor: 'DocManagement' stamp: 'ThMa 9/25/1999 17:07'!
remove: aUrl
	"remove documents with the given URL"
	WatchedDocuments removeAllSuchThat: [:doc | (doc url) = aUrl].

	! !


!WebWatcher methodsFor: 'Watching' stamp: 'ThMa 10/8/1999 20:02'!
checkAllDocuments
	"check all watched documents for changes"
	self log: 'Checking for changes at ', Time now asString. 
     WatchedDocuments isNil ifTrue: [^ 'no documents to check...'].
	WatchedDocuments do: [:i | self checkDocument: i].
     ^ 'done.'! !

!WebWatcher methodsFor: 'Watching' stamp: 'ThMa 10/8/1999 23:03'!
checkDocument: aWebDocument
	"check whether doc has changed, display message and document"
	| checkNew |
	"docs that have been newly added may be reported as being changed or not
      according to the setting of the flag #displayNewDocuments
	"
	checkNew _ (aWebDocument CachedContent notNil) | (self at: #displayNewDocuments).
	checkNew ifFalse: [aWebDocument cache.].

     (aWebDocument hasChanged & checkNew)  ifTrue: [self detectAction: aWebDocument].
 ! !

!WebWatcher methodsFor: 'Watching' stamp: 'ThMa 10/8/1999 23:05'!
detectAction: aWebDocument
	"Actions to be performend when a changed document has been detected"
   
     self log:  ('detected document change for: ' , aWebDocument url ).
	"
	(self at: #openChangedDocInDiffView) ifTrue: [aWebDocument showDiffs].
	(self at: #openChangedDocInScamper) ifTrue: [aWebDocument show].
	"
	(self at: #changeActions) do: [:act | aWebDocument perform: act].
	aWebDocument cache.
	
     ! !

!WebWatcher methodsFor: 'Watching' stamp: 'ThMa 10/8/1999 20:55'!
startWatching
	"start watching process"
     self stopWatching.
     WatchingProcess := [
         [true] whileTrue: [
         self checkAllDocuments.
         (Delay forSeconds: (self at: #checkInterval)) wait.]
         ] newProcess.
     WatchingProcess priority: Processor systemBackgroundPriority.
     WatchingProcess resume.
     self log: 'Watching started...'.
	! !

!WebWatcher methodsFor: 'Watching' stamp: 'ThMa 10/8/1999 20:03'!
stopWatching
	"stop watching process"
     WatchingProcess ifNotNil: [
         WatchingProcess terminate.
         WatchingProcess _ nil.
         self log: 'Watching stopped...'. 
         ].
! !


!WebWatcher methodsFor: 'user interface' stamp: 'thma 10/6/1999 08:39'!
invokeMainMenu
	| myHand |
	myHand _ HandMorph new.
	myHand initialize.
	myHand openInWorld.
	self mainMenu popUpAt: (100 @ 100) forHand: myHand.! !


!WebWatcher methodsFor: 'menus' stamp: 'ThMa 10/8/1999 20:52'!
mainMenu
	| menu |
	menu _ MenuMorph new.
	menu   
		addTitle: 'WebWatcher (tm)';
	 	addStayUpItem;
		add: 'Manage Urls...' target: self action: #invokeDocumentsMenu;
		add: 'Add new Url...' target: self action: #invokeFillinNewUrl;
		add: 'Restart'		 target: self action: #startWatching;
		add: 'Stop'			 target: self action: #stopWatching;
		addLine;
		add: 'Configure...'	 target: self action: #invokeConfiguration;
		addLine;
		add: 'Help...'			 target: self action: #invokeHelp;
		add: 'About...'		 target: self action: #invokeAbout;
     	toggleStayUp: true.
	^ menu! !


!WebWatcher methodsFor: 'menu actions' stamp: 'ThMa 9/28/1999 19:43'!
durableMainMenu 
	Utilities windowFromMenu: self mainMenu target: self title: 'WebWatcher'! !

!WebWatcher methodsFor: 'menu actions' stamp: 'thma 10/6/1999 09:22'!
invokeAbout
	|menu |
	menu _ CustomMenu new.
	menu initialize.
     menu add: '(c) 1999 by Thomas Mahler' action: nil;
		   startUpWithCaption: 'WebWatcher 1.0'.

  ! !

!WebWatcher methodsFor: 'menu actions' stamp: 'ThMa 10/8/1999 22:52'!
invokeConfiguration
	Configuration edit.! !

!WebWatcher methodsFor: 'menu actions' stamp: 'ThMa 9/27/1999 21:15'!
invokeDeleteMenu: aUrl
	"Confirm deletion of URL from WatchedDocuments list"
	| menu answer|
	menu _ CustomMenu new.
	menu 
		initialize;
		addLine;
		add: ('remove ', aUrl) action: true;
		add: 'Cancel' action: false.
	answer _ menu startUpWithCaption: 'Really remove ?'.
	^ answer ifTrue: [self remove: aUrl].
! !

!WebWatcher methodsFor: 'menu actions' stamp: 'ThMa 10/8/1999 19:06'!
invokeDocumentsMenu
	"display list of tracked documents"
	|menu result|
	menu _ CustomMenu new.
	menu initialize.
	WatchedDocuments ifNotNil: [
		WatchedDocuments do: [:doc | menu add: (doc url) action: (doc url)].
		]. 
	result _ menu startUpWithCaption: 'List of tracked Urls'.
	^ result ifNotNil: [self invokeDeleteMenu: result].
    ! !

!WebWatcher methodsFor: 'menu actions' stamp: 'ThMa 9/27/1999 20:07'!
invokeFillinNewUrl
	"comment stating purpose of message"

	| newUrl |
	newUrl _ FillInTheBlank request: 'Watch this new URL:' initialAnswer: 'http://'.
	newUrl = '' ifFalse: [self add: newUrl].
	! !

!WebWatcher methodsFor: 'menu actions' stamp: 'thma 10/6/1999 09:11'!
invokeHelp
	"bring up Help window"
	| w |
	w _ Workspace new.
	w contents: WebWatcher comment;
	   openLabel: 'WebWatcher Help'! !


!WebWatcher methodsFor: 'logging' stamp: 'ThMa 10/8/1999 20:48'!
log: aString
	"if logging is enabled display Message in Transcript"
	(self at: #doLogging) ifTrue: [
		Transcript cr; show: aString.
	]
! !


!WebWatcher methodsFor: 'Configuration' stamp: 'ThMa 10/8/1999 21:04'!
at: Key
	^ (self configuration) at: Key.! !

!WebWatcher methodsFor: 'Configuration' stamp: 'ThMa 10/8/1999 22:38'!
configuration
 	Configuration ifNil: [Configuration _ WebWatcherConfiguration default].
	^ Configuration.! !

!WebWatcher methodsFor: 'Configuration' stamp: 'ThMa 10/8/1999 20:37'!
configuration: aCfg
	Configuration _ aCfg.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WebWatcher class
	instanceVariableNames: ''!

!WebWatcher class methodsFor: 'instance creation' stamp: 'ThMa 10/8/1999 20:45'!
instance
	"return singleton instance"
     Instance ifNil: [Instance _ super new].
    ^ Instance.
	! !

!WebWatcher class methodsFor: 'instance creation' stamp: 'ThMa 10/8/1999 19:26'!
new
	"As we want to use a singleton instance of WebWatcher, 
	 instance creation is not allowed for clients.
	 The singleton instance is returned by WebWatcher instance
	"
 	self shouldNotImplement.!
]style[(5 144 19 3 27)f1b,f1cblack;,f1cred;,f1cblack;,f1! !

!WebWatcher class methodsFor: 'instance creation' stamp: 'ThMa 9/24/1999 18:39'!
reset
	"deleting instance"
     Instance ifNotNil: [
		Instance stopWatching.
		Instance _ nil.
	]! !

!WebWatcher class methodsFor: 'instance creation' stamp: 'ThMa 10/5/1999 20:38'!
run
	"display WebWatchers control panel"
	(self instance) startWatching;
				   invokeMainMenu.! !


!WebWatcher class methodsFor: 'examples' stamp: 'ThMa 10/8/1999 19:50'!
demo
	"perform a simple demonstration of WebWatcher's functionality with WebWatcher demo
      Run this demo from within a morphic world.

	 If you don't have an active internet connection you may try to use local urls
	 like http://localhost or file:Readme.txt etc.

	 Don't forget to set your Squeak Proxy Settings by either stopping the proxy by 
	 HTTPSocket stopUsingproxyServer 
	 or by anouncing it by HTTPSocket useProxyServerNamed: 'Proxy.MyProvider.com' port: '8080'
     "

	self	instance add: 'http://www.squeak.org'.
  	self	run.!
]style[(4 37 10 22 15 264 31 159)f1b,f1,f1LWebWatcher Comment;,f1,f1dWebWatcher demo;;,f1,f1dHTTPSocket stopUsingproxyServer;;,f1! !


Dictionary subclass: #WebWatcherConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ThMa-InfoAgent'!
!WebWatcherConfiguration commentStamp: 'ThMa 10/8/1999 22:56' prior: 0!
Configuration of WebWatcher is kept in a separate class to keep it pluggable.

It is a Dictionary which keeps values for all kind of WebWatcher settings.
A default configuration is provided and can be edited by: WebWatcherConfiguration default edit!
]style[(17 10 185 36)f1,f1LWebWatcher Comment;,f1,f1dWebWatcherConfiguration default edit;;!


!WebWatcherConfiguration methodsFor: 'as yet unclassified' stamp: 'ThMa 10/8/1999 22:55'!
edit
	"Until now editing means: change it in an inspector..."
	self inspect.
	! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

WebWatcherConfiguration class
	instanceVariableNames: ''!

!WebWatcherConfiguration class methodsFor: 'instance creation' stamp: 'ThMa 10/8/1999 22:59'!
default
	"build default Settings dictionary for WebWatcher"
	| cfg |
	cfg _ self new.
	cfg add: (Association key: #doLogging value: true);
		add: (Association key: #checkInterval value: 60);
		add: (Association key: #cacheDir value: '');
		add: (Association key: #displayNewDocuments value: true);
		add: (Association key: #openChangedDocInScamper value: true);
		add: (Association key: #openChangedDocInDiffView value: false);
		add: (Association key: #changeActions value: #(#show #showDiffs)).
	^ cfg.
	! !

