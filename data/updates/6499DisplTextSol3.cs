'From Squeak3.7gamma of ''17 July 2004'' [latest update: #5987] on 20 November 2004 at 12:37:08 pm'!"Change Set:		DisplTextSol3Date:			20 November 2004Author:			Boris GaerterThis is one proposal to fix a problem when we measure DisplayText in the Morphic environment. To avoid the use of an invalid bounding box when we change the text style of a text, we enforce the immediate recreation of the paragraph when we assign a new text style. This proposal does that in TextMorph>>releaseCachedState "!!TextMorph methodsFor: 'caching' stamp: 'BG 11/20/2004 12:36'!releaseCachedState	super releaseCachedState.	self releaseParagraph; paragraph.! !