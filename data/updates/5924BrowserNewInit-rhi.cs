'From Squeak3.7beta of ''1 April 2004'' [latest update: #5905] on 12 May 2004 at 3:03:55 pm'!"Change Set:		BrowserNewInitializeFixDate:			12 May 2004Author:			Robert HirschfeldRemoves an explicit and unnecessary send of initialize already performed by the new-initialize pattern introduced in 3.7."!!Browser methodsFor: 'initialize-release' stamp: 'rhi 5/12/2004 15:00'!systemOrganizer: aSystemOrganizer	"Initialize the receiver as a perspective on the system organizer, 	aSystemOrganizer. Typically there is only one--the system variable 	SystemOrganization."	contents _ nil.	systemOrganizer _ aSystemOrganizer.	systemCategoryListIndex _ 0.	classListIndex _ 0.	messageCategoryListIndex _ 0.	messageListIndex _ 0.	metaClassIndicated _ false.	self setClassOrganizer.	self editSelection: #none.! !