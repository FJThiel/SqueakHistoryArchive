'From Squeak3.1alpha of 28 February 2001 [latest update: #4061] on 23 May 2001 at 2:56:42 pm'!"Change Set:		msgFix-swDate:			23 May 2001Author:			Scott WallaceA small fix for the case of a change sorter with no selected class or method"!!CodeHolder methodsFor: 'message list' stamp: 'sw 5/23/2001 14:51'!selectedMessage	"Answer a copy of the source code for the selected message."	| class selector method tempNames |	contents == nil ifFalse: [^ contents copy].	self showingDecompile ifTrue:		[^ self decompiledSourceIntoContents].	class _ self selectedClassOrMetaClass.	(class isNil or: [(selector _ self selectedMessageName) isNil]) ifTrue: [^ ''].	method _ class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"	currentCompiledMethod _ method.	(Sensor controlKeyPressed		or: [method fileIndex > 0 and: [(SourceFiles at: method fileIndex) == nil]])		ifTrue:		["Emergency or no source file -- decompile without temp names"		contents _ (class decompilerClass new decompile: selector in: class method: method)			decompileString.		contents _ contents asText makeSelectorBoldIn: class.		^ contents copy].	Sensor leftShiftDown ifTrue:		["Special request to decompile -- get temps from source file"		tempNames _ (class compilerClass new						parse: method getSourceFromFile asString in: class notifying: nil)						tempNames.		contents _ ((class decompilerClass new withTempNames: tempNames)				decompile: selector in: class method: method) decompileString.		contents _ contents asText makeSelectorBoldIn: class.		^ contents copy].	self showComment		ifFalse:			[contents _ self sourceStringPrettifiedAndDiffed]		ifTrue:			[contents _ self commentContents].	^ contents copy! !