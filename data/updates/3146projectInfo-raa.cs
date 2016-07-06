'From Squeak2.9alpha of 17 July 2000 [latest update: #3204] on 24 December 2000 at 9:37:14 am'!"Change Set:		projectInfoDate:			24 December 2000Author:			Bob ArningMake getting/setting the project info (used by SuperSwiki) a little easier - a new option on the menu that appears when the Publish button is held down"!!EToyProjectDetailsMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 12/24/2000 09:34'!getFullInfoFor: aProject ifValid: aBlock	^self 		getFullInfoFor: aProject 		ifValid: aBlock 		expandedFormat: false! !!EToyProjectDetailsMorph class methodsFor: 'as yet unclassified' stamp: 'RAA 12/24/2000 09:33'!getFullInfoFor: aProject ifValid: aBlock expandedFormat: expandedFormat	| me |	(me _ self basicNew)		expandedFormat: expandedFormat;		project: aProject		actionBlock: [ :x | 			aProject world setProperty: #ProjectDetails toValue: x.			x at: 'projectname' ifPresent: [ :newName | 				newName = aProject name ifFalse: [aProject changeSet name: newName].			].			me delete.			aBlock value.		];		initialize;		openCenteredInWorld! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'RAA 12/24/2000 09:30'!doPublishButtonMenuEvent: evt	| menu selection |	menu _ CustomMenu new.	menu 		add: 'publish normally' action: [self publishProject];		add: 'publish to different server' action: [self publishDifferent];		add: 'edit project info' action: [self editProjectInfo].	selection _ menu build startUpCenteredWithCaption: 'Publish options'.	selection ifNil: [^self].	selection value.! !!ProjectNavigationMorph methodsFor: 'the actions' stamp: 'RAA 12/24/2000 09:33'!editProjectInfo	EToyProjectDetailsMorph 		getFullInfoFor: (self world ifNil: [^self]) project		ifValid: []		expandedFormat: true! !