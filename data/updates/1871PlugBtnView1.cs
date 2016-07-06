'From Squeak2.8alpha of 14 January 2000 [latest update: #1851] on 23 February 2000 at 12:29:34 am'!"Change Set:		PluggableButtonViewDate:			23 February 2000Author:			Andrew C. GreenbergFixes a bug that caused labels of PluggableButtonView to be improperly centered in some cases.  Previously, PluggableButtonView labels were centered as though installed within the window parent View, but without a border.  This appeared harmless when the PluggableButtonView is relatively centered with respect to the enclosing View and the borderWidth is symmetric (as is the case with existing system windows), PluggableButtonViews will otherwise display with an out-of-center label.Additionally, this change set replaces a magic number in the previous code which catered to leading of the default font with the actual leading of the font in which the leading was displayed."!!View methodsFor: 'display box access' stamp: 'acg 2/23/2000 00:08'!insetDisplayBox	"Answer the receiver's inset display box. The inset display box is the 	intersection of the receiver's window, tranformed to display coordinates, 	and the inset display box of the superView, inset by the border width. 	The inset display box represents the region of the display screen in 	which the inside of the receiver (all except the border) is displayed. If 	the receiver is totally clipped by the display screen and its superView, 	the resulting Rectangle will be invalid."	insetDisplayBox ifNil: [insetDisplayBox _ self computeInsetDisplayBox].	^insetDisplayBox! !!PluggableButtonView methodsFor: 'displaying' stamp: 'acg 2/23/2000 00:18'!displayView	"Displays this switch and its label, if any."	self clearInside.	label ifNotNil: [		(label isKindOf: Paragraph) ifTrue: [			label foregroundColor: self foregroundColor				 backgroundColor: self backgroundColor].		label displayOn: Display				at: label boundingBox topLeft				clippingBox: self insetDisplayBox].	complemented _ false.! !!PluggableButtonView methodsFor: 'private' stamp: 'acg 2/23/2000 00:16'!centerAlignLabelWith: aPoint	"Align the center of the label with aPoint."	| alignPt |	alignPt _ label boundingBox center.	(label isKindOf: Paragraph) ifTrue: 		[alignPt _ alignPt + (0@(label textStyle leading))]. 	label align: alignPt with: aPoint! !!PluggableButtonView methodsFor: 'private' stamp: 'acg 2/23/2000 00:10'!centerLabel	"If there is a label, align its center with the center of the insetDisplayBox"	label ifNotNil: 		[self centerAlignLabelWith: self insetDisplayBox center].! !!PluggableButtonView methodsFor: 'private' stamp: 'acg 2/23/2000 00:09'!insetDisplayBox	"Answer the receiver's inset display box. The inset display box is the 	intersection of the receiver's window, tranformed to display coordinates, 	and the inset display box of the superView, inset by the border width. 	The inset display box represents the region of the display screen in 	which the inside of the receiver (all except the border) is displayed. If 	the receiver is totally clipped by the display screen and its superView, 	the resulting Rectangle will be invalid."	insetDisplayBox ifNil: 		[insetDisplayBox _ self computeInsetDisplayBox.		 self centerLabel].	^insetDisplayBox! !