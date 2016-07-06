'From Squeak3.8gamma of ''24 November 2004'' [latest update: #6599] on 2 June 2005 at 1:47:06 pm'!"Change Set:		LocalePluginAddinsDate:			2 June 2005Author:			tim@rowledge.orgChanges to Squeak3.8 Locale class to make use of prims in LocalePlugin"!!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:42'!primCountry	"Returns string with country tag according to ISO 639"	<primitive: 'primitiveCountry' module: 'LocalePlugin'>	^'USA'! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:45'!primCurrencyNotation	"Returns boolean if symbol is pre- (true) or post-fix (false)"	<primitive: 'primitiveCurrencyNotation' module: 'LocalePlugin'>	^true! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:47'!primCurrencySymbol	"Returns string with currency symbol"	<primitive: 'primitiveCurrencySymbol' module:'LocalePlugin'>	^'$'! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:48'!primDST	"Returns boolean if DST  (daylight saving time) is active or not"	<primitive:'primitiveDaylightSavings' module: 'LocalePlugin'>	^false! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:42'!primDecimalSymbol	"Returns string with e.g. '.' or ','"	<primitive:'primitiveDecimalSymbol' module: 'LocalePlugin'>	^'.'! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:42'!primDigitGrouping	"Returns string with e.g. '.' or ',' (thousands etc)"	<primitive:'primitiveDigitGroupingSymbol' module: 'LocalePlugin'>	^','! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:43'!primLanguage	"returns string with language tag according to ISO 639"	<primitive:'primitiveLanguage' module: 'LocalePlugin'>	^'ENG'! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:51'!primLongDateFormat	"Returns the long date format	d day, m month, y year,	double symbol is null padded, single not padded (m=6, mm=06)	dddd weekday	mmmm month name"	<primitive:'primitiveLongDateFormat' module: 'LocalePlugin'>! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:43'!primMeasurement	"Returns boolean denoting metric(true) or imperial(false)."	<primitive:'primitiveMeasurementMetric' module: 'LocalePlugin'>	^true! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:53'!primShortDateFormat	"Returns the short date format	d day, m month, y year,	double symbol is null padded, single not padded (m=6, mm=06)	dddd weekday	mmmm month name"	<primitive:'primitiveShortDateFormat' module: 'LocalePlugin'>! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/1/2005 18:54'!primTimeFormat	"Returns string time format	Format is made up of 	h hour (h 12, H 24), m minute, s seconds, x (am/pm String)	double symbol is null padded, single not padded (h=6, hh=06)"	<primitive:'primitiveTimeFormat' module: 'LocalePlugin'>! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:43'!primTimezone	"The offset from UTC in minutes, with positive offsets being towards the east.	(San Francisco is in UTC -08*60 and Paris is in GMT +01*60 (daylight savings is not in effect)."	<primitive:'primitiveTimezoneOffset' module: 'LocalePlugin'>	^0! !!Locale methodsFor: 'system primitives' stamp: 'tpr 6/2/2005 13:44'!primVMOffsetToUTC	"Returns the offset in minutes between the VM and UTC.	If the VM does not support UTC times, this is 0.	Also gives us backward compatibility with old VMs as the primitive will fail and we then can return 0."	<primitive:'primitiveVMOffsetToUTC' module: 'LocalePlugin'>	^0! !Locale removeSelector: #primShortSateFormat!Locale removeSelector: #primTimeTormat!