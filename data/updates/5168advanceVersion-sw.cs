'From Squeak3.4beta of ''1 December 2002'' [latest update: #5156] on 7 January 2003 at 2:16:25 am'!"Change Set:		advanceVersion-swDate:			7 January 2003Author:			Scott WallaceThis is the last update for version 3.4beta.  In its postscript is an executable statement that will set the version forward either to 3.4gamma or to 3.5alpha, at user option"!"Postscript:Offer the chance to advance the version number."(self confirm: 'There are no further updates for Squeak 3.4beta.Do you wish to advance to version 3.5alpha?[Yes] Your system will be marked as 3.5alpha, and you willsubsequently receive ''test pilot'' updates for 3.5.[No] Your system will be marked as 3.4gamma, allowing youto receive only final fixes for the stable 3.4 release.[Neither] You may choose No, and immediately quit without saving,allowing you to make a backup copy before adopting this change.DO YOU WANT TO ADVANCE to Version 3.5alpha now?')	ifTrue: [SystemVersion newVersion: 'Squeak3.5alpha'.			SystemVersion current date: '7 January 2003'.			self inform: 'You may now save this Version 3.5alpha imageand retrieve updates again for 3.5alpha and beyond.']	ifFalse: [SystemVersion current version: 'Squeak3.4gamma'; date: '7 January 2003'.			self inform: 'You may now save this Version 3.4gamma imageand retrieve updates again for any final fixes to 3.4.- - - - -(If you quit without saving now, your image will revert to3.4beta without any of the updates you just loaded)'].!