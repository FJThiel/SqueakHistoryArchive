'From Squeak2.8alpha of 4 February 2000 [latest update: #2310] on 12 June 2000 at 1:00:11 pm'!"Change Set:		138WeakTweak-mmDate:			10 June 2000Author:			MathMorphsIn 2277WeakValueAssocRef-mm -- MathMorphs -- 27 May 2000the class WeakValueAssociation is changed butWeakValueDictionary still references the obsolete versionin #at:put:.This preamble-only change set recompiles that method in orderto place the new class in it."WeakValueDictionary recompile: #at:put: from: WeakValueDictionary!