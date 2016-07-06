'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 3 October 2004 at 9:22:13 am'!"Change Set:		MoreIntervalTestsDate:			3 October 2004Author:			Jeff SparkesThe previous IntervalTest only tested success cases.  These changes include testing that invalid inputs create empty intervals and also test floating point and fractional intervals."!!IntervalTest methodsFor: 'testing'!testInvalid	"empty, impossible ranges"	self assert: (1 to: 0) = #().	self assert: (1 to: -1) = #().	self assert: (-1 to: -2) = #().	self assert: (1 to: 5 by: -1) = #().		"always contains only start value."	self assert: (1 to: 1) = #(1).	self assert: (1 to: 5 by: 10) = #(1).	self assert: (1 to: 0 by: -2) = #(1).! !!IntervalTest methodsFor: 'testing'!testNumericTypes	(3 asNumber) to: 5 = #(3 4 5).		3.0 to: 5.0 = #(3.0 4.0 5.0).	3.0 to: 5.0 by: 0.5 = #(3.0 3.5 4.0 4.5 5.0).		3/1 to: 5/1 = #(3 4 5).	1/2 to: 5/2 by: 1/2 = #(1/2 1 3/2 2 5/2).! !