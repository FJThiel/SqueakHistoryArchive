'From Squeak3.3alpha of 11 January 2002 [latest update: #4905] on 30 July 2002 at 9:42:43 pm'!"Postscript:Advance the 3.2gamma images to Squeak3.2 version number."(self confirm: 'This is the last update for Squeak 3.2gamma.Your system is is about to be marked as Squeak3.2, andshould be code-equivalent to the final Squeak3.2 release.There may be further updates available now or in the futurefor the Squeak3.2 system, and you may retrieve these in theusual manner.[Choose no if for some reason you wish to abort this change.Choose yes to track any further updates to Squeak3.2]')	ifTrue: [SystemVersion current version: 'Squeak3.2'.			PopUpMenu notify: 'You may now request updates againto retrieve updates for Squeak3.2.']	ifFalse: [self halt: 'you may close this window and quit'].!