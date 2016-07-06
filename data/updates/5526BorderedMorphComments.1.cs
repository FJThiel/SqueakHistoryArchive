'From Squeak3.7alpha of ''11 September 2003'' [latest update: #5497] on 27 October 2003 at 11:18:49 am'!"Change Set:		BorderedMorphCommentsDate:			27 October 2003Author:			Karl RambergComments for BorderedMorph and subclasses and AlignmentMorph"!!BorderStyle commentStamp: 'kfr 10/27/2003 10:19' prior: 0!See BorderedMorphBorderedMorh new borderStyle: (BorderStyle inset width: 2); openInWorld.!!BorderedMorph commentStamp: 'kfr 10/27/2003 11:17' prior: 0!BorderedMorph introduce borders to morph. Borders have the instanceVariables borderWidth and borderColor. BorderedMorph new borderColor: Color red; borderWidth: 10; openInWorld.BorderedMorph also have a varaity of border styles: simple, inset, raised, complexAltFramed, complexAltInset, complexAltRaised, complexFramed, complexInset, complexRaised.These styles are set using the classes BorderStyle, SimpleBorder, RaisedBorder, InsetBorder and ComplexBorder.BorderedMorph new borderStyle: (SimpleBorder width: 1 color: Color white); openInWorld.BorderedMorph new borderStyle: (BorderStyle inset width: 2); openInWorld.!!EllipseMorph commentStamp: 'kfr 10/27/2003 10:32' prior: 0!A round BorderedMorph. Supports borderWidth and borderColor. Only simple borderStyle is implemented.EllipseMorph new borderWidth:10; borderColor: Color green; openInWorld.EllipseMorph new borderStyle:(SimpleBorder width: 5 color: Color blue); openInWorld.!!RectangleMorph commentStamp: 'kfr 10/27/2003 11:12' prior: 0!A subclass of BorderedMorph that supports different fillStyles.RectangleMorph diagonalPrototype openInWorld.RectangleMorph gradientPrototype openInWorld.!!AlignmentMorph commentStamp: 'kfr 10/27/2003 10:25' prior: 0!Used for layout.Since all morphs now support layoutPolicy the main use of this class is no longer needed.Kept around for compability. Supports a few methods not found elsewhere that can be convenient, eg. newRow!!SimpleBorder commentStamp: 'kfr 10/27/2003 10:17' prior: 0!see BorderedMorph!!ComplexBorder commentStamp: 'kfr 10/27/2003 10:18' prior: 0!see BorderedMorph.poly _ polygon250 baseColor _ Color blue twiceLighter.border _ (ComplexBorder framed: 10) baseColor: poly color.border frameRectangle: ((100@100 extent: 200@200) insetBy: -5) on: Display getCanvas.baseColor _ Color red twiceLighter.border _ (ComplexBorder framed: 10) baseColor: baseColor.border drawPolygon: {100@100. 300@100. 300@300. 100@300} on: Display getCanvas.border drawPolyPatchFrom: 100@200 via: 100@100 via: 200@100 to: 200@200 on: Display getCanvas.border drawPolyPatchFrom: 100@100 via: 200@100 via: 200@200 to: 100@200 on: Display getCanvas.border drawPolyPatchFrom: 200@100 via: 200@200 via: 100@200 to: 100@100 on: Display getCanvas.border drawPolyPatchFrom: 200@200 via: 100@200 via: 100@100 to: 200@100 on: Display getCanvas.border _ (ComplexBorder raised: 10) baseColor: poly color.border drawPolygon: poly getVertices on: Display getCanvas360 / 16.0 22.5points _ (0 to: 15) collect:[:i| (Point r: 100 degrees: i*22.5) + 200].Display getCanvas fillOval: (100@100 extent: 200@200) color: baseColor.border drawPolygon: points on: Display getCanvas.-1 to: points size + 1 do:[:i|	border drawPolyPatchFrom: (points atWrap: i) via: (points atWrap: i+1) via: (points atWrap: i+2) to: (points atWrap: i+3) on: Display getCanvas.].Display getCanvas fillOval: (100@100 extent: 200@200) color: baseColor.0 to: 36 do:[:i|	border drawLineFrom: (Point r: 100 degrees: i*10) + 200 to: (Point r: 100 degrees: i+1*10) + 200		on: Display getCanvas.].drawPolygon:Point r: 1.0 degrees: 10MessageTally spyOn:[Display deferUpdates: true.t1 _ [1 to: 1000 do:[:i|	border drawLineFrom: (100@100) to: (300@100) on: Display getCanvas.	border drawLineFrom: (300@100) to: (300@300) on: Display getCanvas.	border drawLineFrom: (300@300) to: (100@300) on: Display getCanvas.	border drawLineFrom: (100@300) to: (100@100) on: Display getCanvas]] timeToRun.Display deferUpdates: false.].MessageTally spyOn:[Display deferUpdates: true.t2 _ [1 to: 1000 do:[:i|	border drawLine2From: (100@100) to: (300@100) on: Display getCanvas.	border drawLine2From: (300@100) to: (300@300) on: Display getCanvas.	border drawLine2From: (300@300) to: (100@300) on: Display getCanvas.	border drawLine2From: (100@300) to: (100@100) on: Display getCanvas]] timeToRun.Display deferUpdates: false.].!!InsetBorder commentStamp: 'kfr 10/27/2003 09:32' prior: 0!see BorderedMorph!!RaisedBorder commentStamp: 'kfr 10/27/2003 09:32' prior: 0!see BorderedMorph!