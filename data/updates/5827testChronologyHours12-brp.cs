'From Squeak3.7alpha of 11 September 2003 [latest update: #5816] on 12 March 2004 at 3:23:24 pm'!"Change Set:		testChronologyHours12-brpDate:			12 March 2004Author:			Brent PinkneyFix test cases to match the implementation of DateAndTime>>#hour12.The original implementation has been fixed by Avi. This just makes the tests compliant"!!DateAndTimeEpochTest methodsFor: 'testing' stamp: 'brp 3/12/2004 15:21'!testHour12	self assert: aDateAndTime hour12  = DateAndTime new hour12.	self assert: aDateAndTime hour12  = 12! !!DateAndTimeLeapTest methodsFor: 'testing' stamp: 'brp 3/12/2004 15:19'!testHour12	self assert: aDateAndTime hour12  =   1.! !