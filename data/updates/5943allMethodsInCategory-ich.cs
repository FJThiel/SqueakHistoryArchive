'From Squeak3.7beta of ''1 April 2004'' [latest update: #5905] on 16 May 2004 at 1:08:19 am'!!SystemNavigation methodsFor: 'browse' stamp: 'ich. 5/16/2004 01:05'!allMethodsInCategory: category 	| aCollection |	aCollection _ SortedCollection new.	Cursor wait showWhile:		[self allBehaviorsDo:			[:x | (x allMethodsInCategory: category) do:				[:sel | aCollection add: x name , ' ' , sel]]].	^aCollection.	! !