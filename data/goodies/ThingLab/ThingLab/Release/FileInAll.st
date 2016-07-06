| root things |
root := 
FileDirectory default.
( root readOnlyFileNamed: 'ThingLab-Predefs.cs' ) fileIn.
( 
root readOnlyFileNamed: 'ThingLab.cs' ) fileIn.
( 
root readOnlyFileNamed: 'Point-unitVector.st' ) fileIn.
( 
root readOnlyFileNamed: 'BitBlt-copyBits.st' ) fileIn.
things := root directoryNamed: 'things'.
( things readOnlyFileNamed: 'Expressions.st' ) fileIn. 
( things readOnlyFileNamed: 'Constant.st' ) fileIn. 
( things readOnlyFileNamed: 'Node.st' ) fileIn. 
( things readOnlyFileNamed: 'Meter.st' ) fileIn. 
( things readOnlyFileNamed: 'Ground.st' ) fileIn. 
( things readOnlyFileNamed: 'Load.st' ) fileIn. 
( things readOnlyFileNamed: 'AnalogAmmeter.st' ) fileIn. 
( things readOnlyFileNamed: 'AnalogVoltmeter.st' ) fileIn. 
( things readOnlyFileNamed: 'DigitalAmmeter.st' ) fileIn. 
( things readOnlyFileNamed: 'DigitalVoltmeter.st' ) fileIn. 
( things readOnlyFileNamed: 'Anchor.st' ) fileIn. 
( things readOnlyFileNamed: 'BarChart.st' ) fileIn. 
( things readOnlyFileNamed: 'BarGraph.st' ) fileIn. 
( things readOnlyFileNamed: 'Battery.st' ) fileIn. 
( things readOnlyFileNamed: 'Beam.st' ) fileIn. 
( things readOnlyFileNamed: 'BlockThing.st' ) fileIn. 
( things readOnlyFileNamed: 'ColonBargraph.st' ) fileIn. 
( things readOnlyFileNamed: 'BoxBargraph.st' ) fileIn. 
( things readOnlyFileNamed: 'CFConverter.st' ) fileIn. 
( things readOnlyFileNamed: 'Conditional.st' ) fileIn. 
( things readOnlyFileNamed: 'ConstantLengthLine.st' ) fileIn. 
( things readOnlyFileNamed: 'CursorThing.st' ) fileIn. 
( things readOnlyFileNamed: 'Divide.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalObject.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalLead.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalLeadLabel.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalNode.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalNodeLabel.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalSwitch.st' ) fileIn. 
( things readOnlyFileNamed: 'ElectricalWire.st' ) fileIn. 
( things readOnlyFileNamed: 'EqualityTest.st' ) fileIn. 
( things readOnlyFileNamed: 'FixedBridge.st' ) fileIn. 
( things readOnlyFileNamed: 'HorizontalLine.st' ) fileIn. 
( things readOnlyFileNamed: 'InserterPoint.st' ) fileIn. 
( things readOnlyFileNamed: 'InstanceNumber.st' ) fileIn. 
( things readOnlyFileNamed: 'InstanceSet.st' ) fileIn. 
( things readOnlyFileNamed: 'LeftRight.st' ) fileIn. 
( things readOnlyFileNamed: 'ListConnector.st' ) fileIn. 
( things readOnlyFileNamed: 'ListViewThing.st' ) fileIn. 
( things readOnlyFileNamed: 'MethodDeclaration.st' ) fileIn. 
( things readOnlyFileNamed: 'MethodPrototype.st' ) fileIn. 
( things readOnlyFileNamed: 'PointFlexLabel.st' ) fileIn. 
( things readOnlyFileNamed: 'PointLabel.st' ) fileIn. 
( things readOnlyFileNamed: 'PointLink.st' ) fileIn. 
( things readOnlyFileNamed: 'PointOnLine.st' ) fileIn. 
( things readOnlyFileNamed: 'MidPointLine-Network.st' ) fileIn. 
( things readOnlyFileNamed: 'MidPointLine.st' ) fileIn. 
( things readOnlyFileNamed: 'Minus.st' ) fileIn. 
( things readOnlyFileNamed: 'MyMid.st' ) fileIn. 
( things readOnlyFileNamed: 'MyVert.st' ) fileIn. 
( things readOnlyFileNamed: 'Not.st' ) fileIn. 
( things readOnlyFileNamed: 'Plus.st' ) fileIn. 
( things readOnlyFileNamed: 'Printer.st' ) fileIn. 
( things readOnlyFileNamed: 'Resistor.st' ) fileIn. 
( things readOnlyFileNamed: 'SmallFixedBridge.st' ) fileIn. 
( things readOnlyFileNamed: 'TextConnector.st' ) fileIn. 
( things readOnlyFileNamed: 'TextDemo.st' ) fileIn. 
( things readOnlyFileNamed: 'TextLabel.st' ) fileIn. 
( things readOnlyFileNamed: 'Thermometer.st' ) fileIn. 
( things readOnlyFileNamed: 'ThingSlider.st' ) fileIn. 
( things readOnlyFileNamed: 'Times.st' ) fileIn. 
( things readOnlyFileNamed: 'TwoLeadedObject.st' ) fileIn. 
( things readOnlyFileNamed: 'TwoMeters.st' ) fileIn. 
( things readOnlyFileNamed: 'UnaryFunction.st' ) fileIn. 
( things readOnlyFileNamed: 'UpDown.st' ) fileIn. 
( things readOnlyFileNamed: 'Variable.st' ) fileIn. 
( things readOnlyFileNamed: 'VariableHeightText.st' ) fileIn. 
( things readOnlyFileNamed: 'VerticalLine-Network.st' ) fileIn. 
( things readOnlyFileNamed: 'VerticalLine.st' ) fileIn. 
( things readOnlyFileNamed: 'VoltageDivider.st' ) fileIn. 
( things readOnlyFileNamed: 'WheatstoneBridge.st' ) fileIn. 
( things readOnlyFileNamed: 'Wire.st' ) fileIn. 
( things readOnlyFileNamed: 'Bar-Network.st' ) fileIn. 
"
The following whisper could be accepted, but leads to errors like
'PictureForm is still used...'. Apparently sequence is important.

dir fileNames do: [ : each | ( dir readOnlyFileNamed: each ) fileIn ].
"
( Smalltalk at: #
ThingLabBrowser ) open.
