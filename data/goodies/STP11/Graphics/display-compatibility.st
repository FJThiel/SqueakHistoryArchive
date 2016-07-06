"display-compatibility.st
These are for backward compatibility.

    AUTHOR          Stephen T. Pope (stp@create.ucsb.edu)
    VERSION         1.0
    PREREQUISITES   none
    DISTRIBUTION    world
    DATE    December 5, 1996"!


!DisplayObject methodsFor: 'displaying-generic'!
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger mask: aForm
	"This is here for backward compatibility."

	^self displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm! !

!Form methodsFor: 'displaying'!
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger mask: aForm

	aDisplayMedium copyBits: self boundingBox
		from: self
		at: aDisplayPoint + self offset
		clippingBox: clipRectangle
		rule: ruleInteger
		fillColor: aForm! !

!Form class methodsFor: 'instance creation'!
extent: extentPoint fromBytes: anArray offset: offsetPoint 
	"Answer an instance of me with bitmap initialized from anArray."

	| bits word |
	bits := Array new: anArray size / 4.
	0 to: (bits size - 1) do:
		[ :index |
		word := (anArray at: index * 4 + 1) << 24.
		word := word + ((anArray at: index * 4 + 2) << 16).
		word := word + ((anArray at: index * 4 + 3) << 8).
		word := word + (anArray at: index * 4 + 4).
		bits at: index + 1 put: word].
	^(self basicNew setExtent: extentPoint offset: offsetPoint)
		initFromArray: bits! !
