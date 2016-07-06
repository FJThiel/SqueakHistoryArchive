"AnalogVoltmeter"

"make sure the superclass exists"
#TwoLeadedObject lookupClass!

TwoLeadedObject subclass: #AnalogVoltmeter
	instanceVariableNames: 'setting'
	classVariableNames: 'PictureForm'
	poolDictionaries: ''
	category: 'Prototypes'!

AnalogVoltmeter prototype parts: 'setting'.!
AnalogVoltmeter prototype lead1 node primitiveSet.location: 50@60.!
AnalogVoltmeter prototype lead2 node primitiveSet.location: 200@60.!
AnalogVoltmeter prototype field: #setting replaceWith:  165@41.!

Constraint owner: AnalogVoltmeter prototype
	rule: 'setting = (self nearestSettingTo: setting) '
	methods: #('self primitiveSet.setting: (self nearestSettingTo: setting) '
				'lead1 node location reference'
				'lead2 node location reference')!

Constraint owner: AnalogVoltmeter prototype
	rule: 'lead1 current=0'
	methods: #('lead1 primitiveSet.current: 0.0')!

AnalogVoltmeter compile: 
'nearestSettingTo: trial
	| minSetting deltaY |
	minSetting_lead1 node location + lead2 node location // 2 + (40@-19).
	deltaY _ (((trial y - minSetting y)//6 max: 0) min: 6).
	^minSetting+(0@(deltaY*6))'
classified: 'setting'!

AnalogVoltmeter prototype picture: 
	(Form
	extent: 96@50
	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 16383 65535 65535 65535 65535 65532 8192 0 0 0 0 4 8874 43690 43690 43648 119 8196 8874 43690 43690 43648 85 24580 8874 43690 43690 43648 85 8196 8874 43690 43690 43648 85 8196 8874 43690 43690 43648 375 8196 8704 32800 2050 128 0 4 8704 32800 2050 128 7 8196 8704 32 2 0 5 24580 8704 32 2 0 5 8196 8192 0 0 0 5 8196 9984 32 7 0 23 8196 9472 96 1 0 0 4 9472 32 7 0 0 8196 9472 32 4 0 0 24580 9984 32 7 0 0 8196 8192 0 0 0 0 8196 8192 0 0 0 1 8196 8192 0 0 0 0 4 8192 0 0 0 0 8196 8192 0 0 0 0 24580 8192 0 0 0 0 8196 8192 0 0 0 0 8196 8192 0 0 0 0 8196 8192 0 0 0 0 4 8192 0 0 0 2 28676 8192 0 0 0 6 20484 8192 0 0 0 2 20484 8192 0 0 0 2 20484 8192 0 0 0 2 28676 8192 0 0 0 0 4 8192 0 0 0 39 28676 8192 0 0 0 101 20484 8192 0 0 0 37 20484 8192 0 0 0 37 20484 8260 58492 57344 0 39 28676 8261 5137 0 0 0 4 8261 5137 0 0 375 28676 8301 5136 49152 0 853 20484 8233 5136 8192 0 341 20484 8249 5136 8192 0 341 20484 8208 59281 49152 0 375 28676 8192 0 0 0 0 4 8192 0 0 0 0 4 16383 65535 65535 65535 65535 65532 0 0 0 0 0 0 0 0 0 0 0 0)
	offset: -48@-25)
	origin: 'lead1 node location' asPath!

AnalogVoltmeter compile: 
'showPicture: medium
	| center minY deltaY maxRead needleStart needleDisp startDisp |
	super showPicture: medium.
	center_lead1 node location+lead2 node location//2.
	PictureForm displayOn: medium at: center rule: Form paint.
	medium drawLine: (Form dotOfSize: 1) from: center-(48@0) to: lead1 node location
		rule: Form over fillColor: nil.
	medium drawLine: (Form dotOfSize: 1) from: center+(48@0) to: lead2 node location
		rule: Form over fillColor: nil.
	minY _ lead1 node location y + lead2 node location y // 2 -19.
	deltaY _ (((setting y - minY)//6 max: 0) min: 6).
	maxRead _ 2.5 * (10 raisedToInteger: deltaY-3).
	needleStart _ center + (-15@20).
	needleDisp _ (lead1 node voltage-lead2 node voltage*50/maxRead min: 50)-28@-30.
	startDisp _ -10*(needleDisp x/needleDisp y@1).
	medium drawLine: (Form dotOfSize: 1)
			from: needleStart+startDisp
			to: needleStart+needleDisp
			rule: Form over fillColor: nil.
	'
classified: 'showing'!


