'From Squeak 2.3 of January 14, 1999 on 29 March 1999 at 9:29:39 am'!!HTTPSocket class methodsFor: 'get the page' stamp: 'tk 3/29/1999 09:25'!httpShowJpeg: url	"Display the picture retrieved from the given URL, which is assumed to be a JPEG file. See examples in httpGif:."	| nameTokens image |	nameTokens _ url findTokens: '/'.	image _ self httpJpeg: url.	World ifNil: [FormView open: image named: nameTokens last]		ifNotNil: [World addMorph: (SketchMorph new form: image)].! !