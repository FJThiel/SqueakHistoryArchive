'From Squeakland 3.2.4913 of 10 July 2002 [latest update: #136] on 12 September 2002 at 11:01:08 am'!"Published as 4974knownRenames-mlr.cs to 3.3a"!!SmartRefStream methodsFor: 'read write' stamp: 'mir 9/12/2002 10:59'!initKnownRenames	renamed		at: #FlasherMorph put: #Flasher;		yourself! !!SmartRefStream methodsFor: 'read write' stamp: 'mir 9/12/2002 10:59'!initShapeDicts	"Initialize me. "	self flag: #bobconv.		"These must stay constant.  When structures read in, then things can change."	steady _ {Array. Dictionary. Association. String. SmallInteger} asSet.	renamed ifNil: [		renamed _ Dictionary new.  "(old class name symbol -> new class name)"		renamedConv _ Dictionary new "(oldClassNameSymbol -> conversionSelectorInNewClass)"	].	self initKnownRenames! !