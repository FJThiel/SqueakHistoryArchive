'From Squeak2.8alpha of 4 February 2000 [latest update: #1873] on 3 March 2000 at 6:56:47 pm'!"Change Set:		31SourceFileCheck-tprDate:			27 February 2000Author:			Tim Rowledge, tim@sumeru.stanford.eduCheck that the first few characters of the fected source code matches the first part of the selector. There is a good chance of a mismatch implying that the source file was incorrectly ftp'd or otherwise CR/LF mangled.I like the idea but I changed and error: into a notifier and rewrote the error text in a more user friendly way. --sma"!!Browser methodsFor: 'message list' stamp: 'sma 3/3/2000 13:52'!selectedMessage	"Answer a copy of the source code for the selected message selector."	| class selector method tempNames |	contents == nil ifFalse: [^ contents copy].	class _ self selectedClassOrMetaClass.	selector _ self selectedMessageName.	method _ class compiledMethodAt: selector ifAbsent: [		^ ''].	"method deleted while in another project"	currentCompiledMethod _ method.	(Sensor controlKeyPressed		or: [method fileIndex > 0 and: [(SourceFiles at: method fileIndex) == nil]])		ifTrue:		["Emergency or no source file -- decompile without temp names"		contents _ (class decompilerClass new decompile: selector in: class method: method)			decompileString.		contents _ contents asText makeSelectorBoldIn: class.		^ contents copy].	Sensor leftShiftDown ifTrue:		["Special request to decompile -- get temps from source file"		tempNames _ (class compilerClass new						parse: method getSourceFromFile asString in: class notifying: nil)						tempNames.		contents _ ((class decompilerClass new withTempNames: tempNames)				decompile: selector in: class method: method) decompileString.		contents _ contents asText makeSelectorBoldIn: class.		^ contents copy].	contents _ class sourceCodeAt: selector.	(contents asString findString: selector keywords first ) = 1		ifFalse:			[PopUpMenu notify: 'Possible problem with source file!!The method source should start with the methodselector but this is not the case!! This can happen ifyou download the "SqueakV2.sources" file as TEXT.It must be transfered in BINARY mode even if it looks like a text file to preserve the CR line ends.You may proceed with caution but it is recommendedto download a new source file.'].	Preferences browseWithPrettyPrint ifTrue:		[contents _ class compilerClass new			format: contents in: class notifying: nil decorated: Preferences colorWhenPrettyPrinting].	self showDiffs ifTrue:		[contents _ self diffFromPriorSourceFor: contents].	contents _ contents asText makeSelectorBoldIn: class.	^ contents copy! !