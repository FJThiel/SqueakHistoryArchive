'From Squeak3.8alpha of ''17 July 2004'' [latest update: #5976] on 15 August 2004 at 3:34:48 pm'!"Change Set:		SystemWindowCloseBoxFixDate:			15 August 2004Author:			Karl RambergStephane Ducasse reported : There is a small bug. To reproduce it:	- take a window	- select in the menu make unclosable.	- then select make closable-> the icon of the closable is not the same as before.Fix attached"!!SystemWindow methodsFor: 'menu' stamp: 'kfr 8/15/2004 15:27'!makeClosable	| opaqueColor |	mustNotClose := false.	closeBox		ifNil: [self addCloseBox.			self isActive				ifTrue: [opaqueColor := self paneColor]				ifFalse: [opaqueColor := self paneColor muchDarker].			self				updateBox: closeBox				color: (opaqueColor alphaMixed: 0.5 with: Color red).			self extent: self extent]! !