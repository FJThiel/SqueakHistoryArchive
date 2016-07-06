'From Squeak3.1alpha of 28 February 2001 [latest update: #4310] on 5 September 2001 at 4:54:23 pm'!"Change Set:		RememberColor-arDate:			5 September 2001Author:			Andreas RaabMake the paint box remember colors from prior paints."!ImageMorph subclass: #PaintBoxMorph	instanceVariableNames: 'action tool currentCursor thumbnail currentColor currentBrush colorMemory colorPatch stampHolder rotationTabForm scaleTabForm colorMemoryThin brushes focusMorph weakDependents recentColors '	classVariableNames: 'AllOffImage AllOnImage AllPressedImage ColorChart OriginalBounds Prototype RecentColors '	poolDictionaries: ''	category: 'Morphic-Support'!!PaintBoxMorph methodsFor: 'recent colors' stamp: 'ar 9/5/2001 14:20'!fixUpRecentColors	| inner outer border box form newImage canvas morph |	self fixUpColorPicker.	recentColors _ WriteStream on: Array new.	form _ image.	newImage _ Form extent: form extent + (0@41) depth: form depth.	form displayOn: newImage.	newImage copy: ((0@(form height-10)) extent: form width @ (newImage height - form height + 10)) 		from: 0 @ (form height - (newImage height - form height + 10))		in: form rule: Form over.	canvas _ newImage getCanvas.	canvas line: 12@(form height-10) to: 92@(form height-10) width: 1 color: Color black.	canvas _ canvas copyOffset: 12@(form height-9).	inner _ (Color r: 0.677 g: 0.71 b: 0.968).	outer _ inner darker darker.	border _  (Color r: 0.194 g: 0.258 b: 0.194).	0 to: 1 do:[:y|		0 to: 3 do:[:x|			box _ (x*20) @ (y*20) extent: 20@20.			morph _ BorderedMorph new bounds: ((box insetBy: 1) translateBy: canvas origin + bounds origin).			morph borderWidth: 1; borderColor: border.			morph color: Color white.			morph on: #mouseDown send: #mouseDownRecent:with: to: self.			morph on: #mouseMove send: #mouseStillDownRecent:with: to: self.			morph on: #mouseUp send: #mouseUpRecent:with: to: self.			self addMorphFront: morph.			recentColors nextPut: morph.			canvas fillRectangle: box color: Color white.			canvas frameRectangle: (box insetBy: 1) color: border.			canvas frameRectangle: (box) color: inner.			box _ box insetBy: 1.			canvas line: box topRight to: box bottomRight width: 1 color: outer.			canvas line: box bottomLeft to: box bottomRight width: 1 color: outer.	]].	recentColors _ recentColors contents.	(RecentColors == nil or:[RecentColors size ~= recentColors size]) ifTrue:[		RecentColors _ recentColors collect:[:each| each color].	] ifFalse:[		RecentColors keysAndValuesDo:[:idx :aColor| (recentColors at: idx) color: aColor].	].	self image: newImage.	self toggleStamps.	self toggleStamps.! !!PaintBoxMorph methodsFor: 'recent colors' stamp: 'ar 9/5/2001 14:19'!recentColor: aColor	"Remember the color as one of our recent colors"	(recentColors anySatisfy:[:any| any color = aColor]) ifTrue:[^self]. "already remembered"	recentColors size to: 2 by: -1 do:[:i|		(recentColors at: i) color: (recentColors at: i-1) color.		RecentColors at: i put: (RecentColors at: i-1).	].	(recentColors at: 1) color: aColor.	RecentColors at: 1 put: aColor.! !