'From Squeak3.6beta of ''4 July 2003'' [latest update: #5395] on 23 August 2003 at 12:30:29 pm'!"Change Set:		KCP-0099-SingletonSysNavDate:			23 August 2003Author:			Stef and DanielIntroduce a singleton for SystemNavigationChange Object>>systemNavigation to use it."!Object subclass: #SystemNavigation	instanceVariableNames: ''	classVariableNames: 'Default '	poolDictionaries: ''	category: 'System-Support'!!Object methodsFor: '*system-support' stamp: 'dvf 8/23/2003 12:27'!systemNavigation	^ SystemNavigation default! !!SystemNavigation class methodsFor: 'as yet unclassified' stamp: 'dvf 8/23/2003 12:25'!default	Default isNil ifTrue: [Default _ self new].	^Default! !