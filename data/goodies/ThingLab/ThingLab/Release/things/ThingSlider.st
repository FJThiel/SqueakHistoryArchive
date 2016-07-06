ThingLabObject subclass: #ThingSlider
	instanceVariableNames: 'mercury node line'
	classVariableNames: 'SliderPicture'
	poolDictionaries: ''
	category: 'Prototypes'!

ThingSlider prototype parts: 'mercury node line'.
ThingSlider prototype field: #mercury replaceWith: (30@100 line: 30@150).
ThingSlider prototype field: #node replaceWith: #Node clonePrototype.
ThingSlider prototype node primitiveSet.value: 400.0 .
ThingSlider prototype node primitiveSet.location: 100@200.
ThingSlider prototype field: #line replaceWith: (30@150 line: 100@200)!

ThingSlider prototype merge: #('mercury point2' 'line point1').
ThingSlider prototype merge: #('line point2' 'node location').

ThingSlider prototype inserters: #('mercury point2' 'node')!

Constraint owner: ThingSlider prototype
	rule: '10.0*(mercury point2 y-mercury point1 y-10) = node value'
	methods: #(
		'mercury point1 y: mercury point2 y -10 - (node value*0.1)'
		'node primitiveSet.value: 10.0*(mercury point2 y-mercury point1 y - 10)'
		'mercury point2 y reference')!

Constraint owner: ThingSlider prototype
	rule: 'mercury point1 x = mercury point2 x'
	methods: #('mercury point1 x: mercury point2 x'
		'mercury point2 x reference')!

ThingSlider classPool at: #SliderPicture put: (Form
	extent: 60@124
	fromArray: #( 0 0 0 0 0 0 0 0 16383 64512 0 0 16383 64512 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 134 6240 12288 3072 393 9360 12288 3072 137 9360 12288 3072 137 9360 12288 3072 137 9360 12288 3135 63625 9360 12288 3072 137 9360 12288 3072 454 6240 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 6 6240 12288 3072 9 9360 12288 3072 9 9360 12288 3072 6 9360 12288 3072 9 9360 12288 3135 63497 9360 12288 3072 9 9360 12288 3072 6 6240 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 3 6240 12288 3072 4 9360 12288 3072 8 9360 12288 3072 14 9360 12288 3072 9 9360 12288 3135 63497 9360 12288 3072 9 9360 12288 3072 6 6240 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 3 6240 12288 3072 5 9360 12288 3072 5 9360 12288 3072 9 9360 12288 3072 9 9360 12288 3135 63503 42128 12288 3072 1 9360 12288 3072 1 6240 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 6 6240 12288 3072 9 9360 12288 3072 1 9360 12288 3072 1 9360 12288 3072 2 9360 12288 3135 63492 9360 12288 3072 8 42128 12288 3072 15 6240 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 96 12288 3072 0 144 12288 3072 0 144 12288 3072 0 144 12288 3072 0 144 12288 3135 63488 144 12288 3072 0 144 12288 3072 0 96 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 12288 3072 0 0 16383 64512 0 0 16383 64512 0 0 0 0 0 0 0 0 0 0)
	offset: 0@0)!

ThingSlider compile:
'showPicture: medium | top r |
	line showPicture: medium.  node showPicture: medium.
	SliderPicture displayOn: medium at: mercury point2-(11@122) rule: Form paint.
	top _ mercury point1 max: mercury point2-(0@120).
	r _ top-(3@0) corner: mercury point2+(5@-2).
	medium fill: r rule: Form over fillColor: Color gray'
classified: 'showing'.

"Old message - for efficent display of changing thermometers.  Needs to
do something with foreground selectors ..."
ThingSlider compile:
'showNewMercury: medium | top r1 r2 |
	r1_mercury point2-(4@120) corner: mercury point2+(4@0).
	medium white: r1.
	top _ mercury point1 max: mercury point2-(0@120).
	r2 _ top-(4@0) corner: mercury point2+(4@0).
	medium fill: r2 rule: Form over fillColor: Color gray'
classified: 'showing'.


"The ThingSlider icon was made as follows:

	| pictureForm rect pen degrees para textLoc  |
pictureForm _ Form extent: 60@124.
rect _ 2@2 extent: 20@120.
pictureForm white.
pictureForm black: rect.
pictureForm white: (rect insetBy: 2).
pen _ Pen newOnForm: pictureForm.
pen rule: Form paint.
degrees_1000.
10 to: 110 by: 20 do:
	[:y |
	pen place: rect origin+(24@y); north; turn: 90; go: 10.
	para_degrees printString asParagraph.  para rightFlush.
	textLoc _ (61@(y-7)) - (para compositionRectangle width@0).
	para displayOn: pictureForm at: textLoc rule: Form under.
	degrees_degrees-200].
pictureForm writeOn: 'ThingSlider.form'.
pictureForm displayOn: Display at: 500@600.
"

