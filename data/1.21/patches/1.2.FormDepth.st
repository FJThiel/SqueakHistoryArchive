Date:	97 Aug 11 9:59:51 pm
From:	Ward Cunningham <ward@c2.com>
To:		squeak@create.ucsb.edu
Subject:	Form>>storeString for depth other than one.

This is a multi-part message in MIME format.

--------------493E5B6E232B
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit

Squeak 1.20 Forms failed to report their depth when asked for a
storeString. Enclosed please find a ChangeSet that adds "depth:" to the
storeOn: method and adds instance creation protocol to notice it. --
Ward

-- 
Ward Cunningham
v 503-245-5633   mailto:ward@c2.com  
f 503-246-5587   http://c2.com/

--------------493E5B6E232B
Content-Type: text/plain; charset=us-ascii; name="Form-storeString.cs"
Content-Transfer-Encoding: 7bit
Content-Disposition: inline; filename="Form-storeString.cs"

'From Squeak 1.2 of June 29, 1997 on 1 August 1997 at 4:25:16 pm'!"
Change Set:		Form-storeString
Date:			1 August 1997
Author:			Ward Cunningham

Corrects the Form>>storeString representation of 
Forms with depth > 1.

"!


!Form methodsFor: 'fileIn/Out' stamp: 'ward 8/1/97 10:20'!
storeOn: aStream base: anInteger 
	"Store the receiver out in the form: Form newExtent:fromArray:#()offset:"

	aStream nextPut: $(.
	aStream nextPutAll: self species name.
	aStream crtab: 1.
	aStream nextPutAll: 'extent: '.
	self extent printOn: aStream.
	aStream crtab: 1.
	aStream nextPutAll: 'depth: '.
	self depth printOn: aStream.
	aStream crtab: 1.
	aStream nextPutAll: 'fromArray: #('.
	1 to: bits size do: 
		[:index | 
		anInteger = 10
			ifTrue: [aStream space]
			ifFalse: [aStream crtab: 2].
		(self bits at: index) printOn: aStream base: anInteger].
	aStream nextPut: $).
	aStream crtab: 1.
	aStream nextPutAll: 'offset: '.
	self offset printOn: aStream.
	aStream nextPut: $)! !

!Form methodsFor: 'private' stamp: 'ward 8/1/97 16:22'!
setExtent: extent depth: anInteger offset: aPoint
	"Create a virtual bit map with the givcen extent and offset."

	^ (self setExtent: extent depth: anInteger) offset: aPoint! !


!Form class methodsFor: 'instance creation' stamp: 'ward 8/1/97 07:06'!
extent: extentPoint depth: anInteger fromArray: anArray offset: offsetPoint 
	"Answer an instance of me with bitmap initialized from anArray."

	^ (self basicNew setExtent: extentPoint depth: anInteger offset: offsetPoint)
		initFromArray: anArray! !



--------------493E5B6E232B--


