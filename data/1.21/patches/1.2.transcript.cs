Date:	97 Aug 30 11:08:22 am
From:	Dan Ingalls <DanI@wdi.disney.com>
To:		Squeak@create.ucsb.edu
Subject:	Better fix for occluded transcript

--============_-1339152484==_============
Content-Type: text/plain; charset="us-ascii"

Folks -

Attached is a fix for transcript display when occluded that does not have the problem that Tim Olson pointed out in the earlier one.

Also, as a side-effect of enabling windows to refresh without valid cached bits, Squeak's "window dont save bits (compact)" option, no longer leaves the screen with ugly holes after moving, closing or collapsing windows.

	- Dan


PS regarding the transcript in recent releases:

It seems that, for quite a while, the transcript in the Squeak release has=
 had an extra view in its dependents list.  It is usually invisible, except=
 that it makes a hole in the screen when you close the transcript.  This can=
 be fixed, and the new transcript code will work better if you...

	Close the transcript, and then execute
		Transcript breakDependents.

We'll fix this in the next release.


--============_-1339152484==_============
Content-Type: text/plain; name="OccludedTranscript-di.cs"; charset="us-ascii"
Content-Disposition: attachment; filename="OccludedTranscript-di.cs"

'From Squeak 1.21c of Aug 4, 1997 on 30 August 1997 at 11:34:50 am'!
"Change Set:		OccludedTranscript
Date:			30 August 1997
Author:			Dan Ingalls

Allows the transcript to display when occluded by other windows.  This is a simple solution, and not especially efficient, but it sure runs faster and looks better than the old approach of bringing the transcript to the top for every change.

As a side-effect of enabling windows to refresh without valid cached bits, Squeak's "window dont save bits (compact)" option, no longer leaves the screen with ugly holes after moving, closing or collapsing windows.
"!


!Paragraph methodsFor: 'private' stamp: 'di 8/30/97 11:14'!
withClippingRectangle: clipRect do: aBlock
	| saveClip |
	saveClip _ clippingRectangle.
	clippingRectangle _ clipRect.
		aBlock value.
	clippingRectangle _ saveClip! !


!StandardSystemView methodsFor: 'displaying' stamp: 'di 8/29/97 18:57'!
displayOn: aPort
	bitsValid ifFalse:
		[^ Display clippingTo: aPort clipRect do: [super display]].
	windowBits displayOnPort: aPort at: self windowOrigin! !

!StandardSystemView methodsFor: 'displaying' stamp: 'di 8/30/97 11:07'!
erase
	"Clear the display box of the receiver to be gray, as the screen background."
	| oldValid |
	CacheBits
		ifTrue:
			[oldValid _ bitsValid.
			bitsValid _ false.
			ScheduledControllers restore: self windowBox without: self.
			bitsValid _ oldValid]
		ifFalse:
			[ScheduledControllers restore: self windowBox without: self]! !


!TextCollectorController methodsFor: 'entry control' stamp: 'di 8/29/97 19:20'!
appendEntry
	"Append the text in the model's writeStream to the editable text. "
	
	view topView isCollapsed
		ifTrue: [paragraph text
				replaceFrom: 1
				to: paragraph text size
				with: model contents asText]
		ifFalse: 
			[self deselect.
			paragraph text size > model characterLimit ifTrue: 
				[paragraph removeFirstChars: paragraph text size - (model characterLimit // 2)].
			self selectWithoutComp: paragraph text size + 1.
			self replaceSelectionWith: model nextEntry asText.
			self selectWithoutComp: paragraph text size + 1.
			model contents: paragraph text]! !

!TextCollectorController methodsFor: 'private' stamp: 'di 8/30/97 11:21'!
doOccluded: actionBlock
	| paneRect rectSet bottomStrip |
	view topView isCollapsed ifTrue: [^ actionBlock value].
	paneRect _ paragraph clippingRectangle.
	rectSet _ self visibleAreas.
	paragraph withClippingRectangle: (paneRect withHeight: 0)
		do: [actionBlock value.
			self scrollIn: paneRect].
	bottomStrip _ paneRect withTop: paragraph compositionRectangle bottom + 1.
	rectSet do:
		[:rect |
		(bottomStrip intersects: rect) ifTrue:
			["The subsequent displayOn should clear this strip but it doesnt"
			Display fill: (bottomStrip intersect: rect)
					fillColor: paragraph backgroundColor].
		paragraph withClippingRectangle: rect
				do: [paragraph displayOn: Display]]! !

!TextCollectorController methodsFor: 'private' stamp: 'di 8/30/97 11:34'!
scrollIn: scrollRect
	"Altered from selectAndScroll so can use with null clipRect"
	"Scroll until the selection is in the view and then highlight it."
	| deltaY |
	deltaY _ stopBlock top - scrollRect top.
	deltaY >= 0 
		ifTrue: [deltaY _ stopBlock bottom - scrollRect bottom max: 0].
						"check if stopIndex below bottom of scrollRect"
	deltaY ~= 0 
		ifTrue: [self scrollBy: (deltaY abs + paragraph lineGrid - 1) * deltaY sign]! !

!TextCollectorController methodsFor: 'private' stamp: 'di 8/27/97 22:34'!
visibleAreas
	"Transcript dependents last controller visibleAreas"
	| visibleAreas rect remnants myTopController |
	myTopController _ self view topView controller.
	visibleAreas _ Array with: view insetDisplayBox.
	myTopController view uncacheBits.
	ScheduledControllers scheduledControllers do:
		[:c | c == myTopController ifTrue: [^ visibleAreas].
		rect _ c view windowBox.
		remnants _ OrderedCollection new.
		visibleAreas do: [:a | remnants addAll: (a areasOutside: rect)].
		visibleAreas _ remnants].
	^ visibleAreas! !


!TextCollectorView methodsFor: 'updating' stamp: 'di 8/29/97 18:26'!
update: aParameter
	"Transcript cr; show: 'qwre'.    Transcript clear."
	aParameter == #appendEntry ifTrue:
		[^ controller doOccluded: [controller appendEntry]].
	aParameter == #update ifTrue:
		[^ controller doOccluded:
				[controller changeText: model contents asText]].
	^ super update: aParameter! !


ControlManager removeSelector: #bring:nextToTopFor:!

--============_-1339152484==_============--


