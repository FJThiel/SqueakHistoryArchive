'From Squeak3.10beta of 22 July 2007 [latest update: #7130] on 5 October 2007 at 6:17:47 pm'!"Change Set:		DelayStartupDate:			5 October 2007Author:			Andreas RaabChange startup/shutdown sequence to have Delay be first/last thing being processed since some of the other services rely on Delay being active."!!SystemDictionary class methodsFor: 'initialization' stamp: 'ar 10/5/2007 18:16'!initialize	"SystemDictionary initialize"	| oldList |	oldList _ StartUpList.	StartUpList _ OrderedCollection new.	"These get processed from the top down..."	#(		Delay		DisplayScreen		Cursor		InputSensor		ProcessorScheduler  "Starts low space watcher and bkground."		FileDirectory  "Enables file stack dump and opens sources."		ShortIntegerArray		ShortRunArray		CrLfFileStream	) do:[:clsName|		Smalltalk at: clsName ifPresent:[:cls| Smalltalk addToStartUpList: cls].	].	oldList ifNotNil: [oldList do: [:className | Smalltalk at: className						ifPresent: [:theClass | Smalltalk addToStartUpList: theClass]]].	#(		ImageSegment		PasteUpMorph		ControlManager	) do:[:clsName|		Smalltalk at: clsName ifPresent:[:cls| Smalltalk addToStartUpList: cls].	].			oldList _ ShutDownList.	ShutDownList _ OrderedCollection new.	"These get processed from the bottom up..."	#(		Delay		DisplayScreen		InputSensor		Form		ControlManager		PasteUpMorph		StrikeFont		Color		FileDirectory		SoundPlayer		HttpUrl		Password		PWS		MailDB		ImageSegment	) do:[:clsName|		Smalltalk at: clsName ifPresent:[:cls| Smalltalk addToShutDownList: cls].	].	oldList ifNotNil: [oldList reverseDo: [:className | Smalltalk at: className						ifPresent: [:theClass | Smalltalk addToShutDownList: theClass]]].! !SystemDictionary initialize!