'From Squeak2.9alpha of 13 June 2000 [latest update: #2893] on 25 October 2000 at 7:40:58 pm'!"Change Set:		InnerMVCMenus-arDate:			25 October 2000Author:			Andreas RaabFixes MVC menus running in inner worlds. They would invoke a morphic cycle in an inner world which doesn't work since the event is coming in the reference frame of the outermost world and thus, event processing has to start there."!!MVCMenuMorph methodsFor: 'invoking' stamp: 'ar 10/25/2000 19:39'!invokeAt: aPoint in: aWorld	"Add this menu to the given world centered at the given point. Wait for the user to make a selection and answer it. The selection value returned is an integer in keeping with PopUpMenu, if the menu is converted from an MVC-style menu."	"Details: This is invoked synchronously from the caller. In order to keep processing inputs and updating the screen while waiting for the user to respond, this method has its own version of the World's event loop." 	| w |	self flag: #bob.		"is <aPoint> global or local?"	self flag: #arNote.	"<aPoint> is local to aWorld"	self popUpAt: aPoint forHand: aWorld primaryHand in: aWorld.	done _ false.	w _ aWorld outermostWorldMorph. "containing hand"	[self isInWorld & done not] whileTrue: [w doOneSubCycle].	self delete.	^ mvcSelection! !