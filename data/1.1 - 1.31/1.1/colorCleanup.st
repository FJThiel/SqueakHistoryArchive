'Correction of color examples. Also changed them to be class methods. Some obsolete private instance methods are removed.' !

!Color class methodsFor: 'examples'!
showPalette
	"Show the 12x12x12 palette used in fromUser."
	"Color showPalette"

	 | rect |
	"RGB display gives 12x12x12 cube to choose from"

	rect _ 0@0 extent: 5@5.	"modified in loop below"
	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |
				rect moveTo: (r*60) + (b*5) @ (g*5).
				Display fill: rect fillColor: (Color red: r green: g blue: b range: 11).
	]]]! !
!Color class methodsFor: 'examples'!
showHuesInteractively
	"Shows a palette of hues at (saturation, brightness) point determined by the mouse position. Click mouse button to exit and return the selected saturation and brightness."
	"Color showHuesInteractively"

	| baseP p |
	baseP _ Sensor cursorPoint.
	[Sensor anyButtonPressed] whileFalse: [
		p _ (Sensor cursorPoint - baseP + 80) / 100.0.
		self showHuesAtSaturation: p x brightness: p y.
	].
	^ (p x min: 1.0) @ (p y min: 1.0)! !
!Color class methodsFor: 'examples'!
hsvExample
	"Shows a palette of hues, varying the saturation and brightness for each one."
	"Color hsvExample."

	| d rect |
	d _ Display depth.
	rect _ 0@0 extent: 5@5.	"modified in loop below"
	0 to: 179 by: 15 do: [:h |
		0 to: 10 do: [:s |
			0 to: 10 do: [:v |
				rect moveTo: (h*4) + (s*5) @ (v*5).
				Display fill: rect fillColor:
					(Color hue: h saturation: s / 10 brightness: v / 10)

				rect moveBy: 0 @ 80.
				Display fill: rect fillColor:
					(Color hue: h + 180 saturation: s / 10 brightness: v / 10)
	]]]! !
!Color class methodsFor: 'examples'!
showHuesAtSaturation: s brightness: v
	"Shows a palette of hues at the given (saturation, brightness) point."
	"Color showHuesAtSaturation: 0.9 brightness: 0.9"

	| rect |
	rect _ 0@0 extent: 35@35. "modified in loop below"
	0 to: 179 by: 10 do: [:h |
		rect moveTo: (h*4) + 5 @ 5.
		Display fill: rect fillColor: (Color hue: h saturation: s brightness: v).
		rect moveTo: (h*4) + 5 @ 45.
		Display fill: rect fillColor: (Color hue: h + 180 saturation: s brightness: v).
	].
! !

Color removeSelector: #hsvExample!
Color removeSelector: #showHuesInteractively!
Color removeSelector: #showPalette!
Color removeSelector: #showHuesAtSaturation:brightness:!

Color removeSelector: #closestColor1!
Color removeSelector: #closestColor2!
Color removeSelector: #closestColor4!
