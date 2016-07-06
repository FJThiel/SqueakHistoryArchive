"Resistor"

"make sure the superclass exists"
#TwoLeadedObject lookupClass!

TwoLeadedObject subclass: #Resistor
	instanceVariableNames: 'resistance text '
	classVariableNames: 'Pictures '
	poolDictionaries: ''
	category: 'Prototypes'!

Resistor prototype primitives: 'resistance'; parts: 'text'.
Resistor prototype lead1 node primitiveSet.location: 40@10.
Resistor prototype lead2 node primitiveSet.location: 40@110.
Resistor prototype field: #resistance replaceWith: 1000.0 .
Resistor prototype field: #text replaceWith: #TextThing clonePrototype.
Resistor prototype text primitiveSet.text: '1000.0' asText.
Resistor prototype text primitiveSet.frame: (55@50 corner: 100@70)!

Constraint owner: Resistor prototype
	rule: '(lead1 node voltage-lead2 node voltage) = (lead1 current*resistance)'
	methods: 
		#('lead1 node primitiveSet.voltage: lead2 node voltage + (lead1 current*resistance)'
		'lead2 node primitiveSet.voltage: lead1 node voltage - (lead1 current*resistance)'
		'lead1 primitiveSet.current: (lead1 node voltage-lead2 node voltage)/resistance'
		'self primitiveSet.resistance: (lead1 node voltage-lead2 node voltage)/lead1 current')!

Constraint owner: Resistor prototype
	rule: 'text text = resistance printString asText'
	methods: #('self primitiveSet.resistance: text text asNumber'
		'text text reference')!

Constraint owner: Resistor prototype
	rule: 'text frame origin = (lead1 node location + lead2 node location /2 + (15@-10))'
	methods: #('text primitiveSet.frame: ((lead1 node location + lead2 node location /2 + (15@-10))
			extent: (45@20))'
		'lead1 node location reference'
		'lead2 node location reference')!


Resistor compile: 
'initializePicturesUp: upPicture slant: slantPicture
	| rotatedPicture | 
	"upPicture is a vertically alligned resistor and slantPicture is at a 45 degree angle pointed up and to the right"
	Pictures _ Array new: 4.
	rotatedPicture _ slantPicture rotateBy: 1.
	rotatedPicture offset: slantPicture offset transposed.
	Pictures at: 1 put: rotatedPicture.
	rotatedPicture _ upPicture rotateBy: 1.
	rotatedPicture offset: upPicture offset transposed.
	Pictures at: 2 put: rotatedPicture.
	Pictures at: 3 put: slantPicture.
	Pictures at: 4 put: upPicture'
classified: 'showing'!


Resistor compile: 
'pictureFor: pictureIndex
	| linearIndex |
	linearIndex _ ((pictureIndex x + 1) * 3) + pictureIndex y + 2.
	linearIndex>5 ifTrue: [linearIndex _ 10 - linearIndex].
	linearIndex <= 5 ifTrue: [^Form extent: 10@10].
	^Pictures at: linearIndex'
classified: 'showing'!

Resistor compile: 
'showPicture: medium
	| center orientation delta |
	super showPicture: medium.
	center_lead1 node location+lead2 node location//2.
	orientation _ (lead2 node location-lead1 node location) unitVector rounded.
	(self pictureFor: orientation) displayOn: medium at: center rule: Form paint.
	orientation x * orientation y = 0 ifTrue: [delta _ 35] ifFalse: [delta _ 25].
	medium drawLine: (Form dotOfSize: 1) from: center-(delta*orientation) to: lead1 node location
		rule: Form over fillColor: nil.
	medium drawLine: (Form dotOfSize: 1) from: center+(delta-1*orientation) to: lead2 node location
		rule: Form over fillColor: nil.'
classified: 'showing'!
	Resistor prototype
		initializePicturesUp:
			  (Form extent: 21@69
					fromArray: #( 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 96 2047 896 2047 7168 2047 24576 2047 32768 2047 24576 2047 7168 2047 896 2047 112 2047 14 2047 1 51199 0 14335 0 4095 0 14335 1 51199 14 2047 112 2047 896 2047 7168 2047 24576 2047 32768 2047 24576 2047 7168 2047 896 2047 112 2047 14 2047 1 51199 0 14335 0 4095 0 14335 1 51199 14 2047 112 2047 896 2047 7168 2047 24576 2047 32768 2047 24576 2047 7168 2047 896 2047 112 2047 14 2047 1 51199 0 14335 0 4095 0 14335 1 51199 14 2047 48 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047 32 2047)
				   	offset: -10@-35)
		slant: (Form extent: 49@49
					fromArray: #( 0 0 0 32768 0 0 1 0 0 0 2 0 0 0 4 0 0 0 8 0 0 0 16 0 0 0 32 0 0 0 64 0 0 0 128 0 0 0 256 0 0 96 128 0 0 88 128 0 0 38 64 0 0 33 32832 0 0 16 24608 0 0 16 6176 0 0 8 1552 0 0 8 400 0 0 4 104 0 0 4 24 0 1 32770 0 0 1 24578 0 0 0 38913 0 0 0 34305 0 0 0 16768 32768 0 0 16480 32768 0 0 8216 16384 0 0 8198 16384 0 0 4097 40960 0 0 4096 24576 0 1536 2048 0 0 1408 2048 0 0 608 1024 0 0 536 1024 0 0 262 512 0 0 257 33280 0 0 128 24832 0 0 128 6400 0 0 64 1664 0 0 64 384 0 0 128 0 0 0 256 0 0 0 512 0 0 0 1024 0 0 0 2048 0 0 0 4096 0 0 0 8192 0 0 0 16384 0 0 0 32768 0 0 0)
					offset: -25@-25)!

