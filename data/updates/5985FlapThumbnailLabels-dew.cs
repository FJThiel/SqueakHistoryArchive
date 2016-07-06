'From Squeak3.7gamma of ''17 July 2004'' [latest update: #5983] on 5 August 2004 at 12:00:09 am'!"Change Set:		FlapThumbnailLabels-dewDate:			4 August 2004Author:			Doug WayFix the thumbnails labels in the flaps to be readable.  (Using a non-TT font for the labels.)  Also regenerates flaps in the postscript so the change takes effect."!!IconicButton methodsFor: 'initialization' stamp: 'dew 8/4/2004 23:53'!initializeWithThumbnail: aThumbnail withLabel: aLabel andSend: aSelector to: aReceiver 		"Initialize the receiver to show aThumbnail on its face, giving it the label supplied and arranging for it, when the button goes down on it, to obtain a new morph by sending the supplied selector to the supplied receiver"	| labeledItem |	labeledItem _ AlignmentMorph newColumn beTransparent.	labeledItem borderWidth: 0.	labeledItem addMorph: aThumbnail.	labeledItem addMorphBack: (Morph new extent: (4@4)) beTransparent.	labeledItem addMorphBack: (BorderedStringMorph contents: aLabel font: (StrikeFont familyName: Preferences standardBalloonHelpFont familyName size: 13)).  "Use a non-TT font. TT fonts look bad in thumbnail form."	self		beTransparent;		labelGraphic: (labeledItem imageForm asFormOfDepth: 16);		borderWidth: 0;		target: aReceiver;		actionSelector: #launchPartVia:label:;		arguments: {aSelector. aLabel};		actWhen: #buttonDown.	self stationarySetup.! !"Postscript:"Language recreateFlaps.!