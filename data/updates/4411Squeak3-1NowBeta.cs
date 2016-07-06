'From Squeak3.1alpha of 5 February 2001 [latest update: #4410] on 2 October 2001 at 8:12:42 pm'!"Change Set:		Squeak3-1NowBetaDate:			2 October 2001Author:			Dan IngallsThis is the last update for version 3.1 alpha.  In its postscript is an executable statement that will set the version forward either to 3.1beta (in order to receive final fixes for 3.1) or to 3.2alpha (in order to continue receiving interesting but risky enhancements."!"Postscript:Offer the chance to advance the version number."(self confirm: 'There are no further updates for Squeak 3.1alpha.Do you wish to advance to version 3.2alpha?[Yes] You may continue to receive ''test pilot'' updates for 3.2.[No] Your system will be marked as 3.1beta, allowing youto receive only fixes for the stable 3.1 release.[Neither] You may choose No, and immediately quit without saving,allowing you to make a backup copy before adopting this change.DO YOU WANT TO ADVANCE to Version 3.2alpha now?')	ifTrue: [SystemVersion current version: 'Squeak3.2alpha'; date: Date today.			PopUpMenu notify: 'You may now save this Version 3.2alpha imageand retrieve updates again for 3.2alpha and beyond.']	ifFalse: [SystemVersion current version: 'Squeak3.1beta'; date: Date today.			PopUpMenu notify: 'You may now save this Version 3.1beta imageand retrieve updates again for any final fixes to 3.1.- - - - -(If you quit without saving now, your image will revert to3.1alpha without any of the updates you just loaded)'].!