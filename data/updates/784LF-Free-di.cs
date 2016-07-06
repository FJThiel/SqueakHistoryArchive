'From Squeak 2.3 of January 14, 1999 on 4 April 1999 at 11:11:02 am'!"Change Set:		LF-FreeDate:			4 April 1999Author:			Dan IngallsThis changeSet removes one lineFeed embedded in a string pattern, and defines a method for removing any other LFs in source code that is part of a CR-LF pair.The postscript invokes the general method scan whichmay take several minutes.Note that the scan may halt if it encounters any LFs other than in CR-LF pairs."!!HTTPSocket class methodsFor: 'get the page' stamp: 'di 4/4/1999 09:57'!httpGetNoError: url args: args accept: mimeType	"Return the exact contents of a web file.  Do better error checking.  Asks for the given MIME type.  To fetch raw data, you can use the MIMI type 'application/octet-stream'.  If mimeType is nil, use 'text/html'.  The parsed header is saved. Use a proxy server if one has been registered.""Edited to remove a lineFeed from the source 4/4/99 - di"	| document data |	document _ self httpGetDocument: url  args: args  accept: mimeType.	(document isKindOf: String) ifTrue: [		"strings indicate errors"		^ document ].	data _ document content.	(data beginsWith: '<HTML><HEAD>' , (String with: Character linefeed) , '<TITLE>4')		ifTrue: ["an error message  404 File not found"				^ data copyFrom: 21 to: data size-16].		^ (RWBinaryOrTextStream with: data) reset! !!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 4/4/1999 11:08'!removeAllLineFeeds    "Smalltalk removeAllLineFeeds"	"Scan all methods for source code with lineFeeds.	Replaces all occurrences of <CR><LF> by <CR> silently.	Halts with a message if any other LFs are found."	 | oldCodeString n crlf cr newCodeString oldStamp oldCategory m |	crlf _ String with: Character cr with: Character lf.	cr _ String with: Character cr.	Smalltalk forgetDoIts.'Scanning sources for LineFeeds.This will take a few minutes...'displayProgressAt: Sensor cursorPointfrom: 0 to: CompiledMethod instanceCountduring: [:bar | n _ 0. m _ 0.	Smalltalk allBehaviorsDo:		[:cls | 		cls selectors do:			[:selector | (n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].			oldCodeString _ (cls sourceCodeAt: selector) asString.			(oldCodeString indexOf: Character lf startingAt: 1) > 0 ifTrue:				[newCodeString _ oldCodeString copyReplaceAll: crlf with: cr asTokens: false.				(newCodeString indexOf: Character lf startingAt: 1) > 0					ifTrue: [(self confirm: cls name , ' ' , (selector contractTo: 30) , 'has an isolated LineFeed (not part of CRLF).Shall I replace it?') ifFalse: [self halt]].				oldStamp _ Utilities timeStampForMethod: (cls compiledMethodAt: selector).				oldCategory _ cls whichCategoryIncludesSelector: selector.				cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.				m _ m + 1]]].].	Transcript cr; show: m printString , ' methods stripped of LFs.'.! !