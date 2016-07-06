'From Squeak2.8 of 13 June 2000 [latest update: #2359] on 5 September 2000 at 1:57:49 pm'!"Change Set:		BetterPhoneDialerDate:			5 September 2000Author:			Duane MaxwellThis changeset adds a class method to AbstractSound that generates the Dual Tone Multiple Frequencies (DTMF) signals necessary to dial a US telephone.  Basically, you hold your handset up to your speaker and generate the tones by sending the phone number as a string to the method AbstractSound>>dial:.  It will ignore anything but dial characters (0..9,*,#), with the exception of the comma, which invokes a one second pause.Also included are methods to reproduce dial tones, busy signals, even that annoying sound when you leave your phone off hook.This version of the code builds SequentialSounds and returns them from the method after starting them up - that way the process isn't locked up.  It also makes the timing more uniform for dialing.example:AbstractSound dial: '867-5309'AbstractSound dialTone: 2AbstractSound busySignal: 3AbstractSound hangUpWarning: 10"!!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:50'!busySignal: count	"AbstractSound busySignal: 3"	| m s |	s _ SequentialSound new.	m _ MixedSound new.	m	add: (FMSound new setPitch: 480 dur: 0.5 loudness: 0.5);		add: (FMSound new setPitch: 620 dur: 0.5 loudness: 0.5).	s add: m.	s add: (FMSound new setPitch: 1 dur: 0.5 loudness: 0).	^ (RepeatingSound repeat: s count: count) play.! !!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:56'!dial: aString	| index lo hi m s |	"AbstractSound dial: '867-5309'" "ask for Jenny"	s _ SequentialSound new.	aString do: [ :c |		c = $,			ifTrue: [ s add: (FMSound new setPitch: 1 dur: 1 loudness: 0) ]			ifFalse: [				(index _ ('123A456B789C*0#D' indexOf: c)) > 0					ifTrue: [						lo _ #(697 770 852 941) at: (index - 1 // 4 + 1).						hi _ #(1209 1336 1477 1633) at: (index - 1 \\ 4 + 1).						m _ MixedSound new.						m add: (FMSound new setPitch: lo dur: 0.15 loudness: 0.5).						m add: (FMSound new setPitch: hi dur: 0.15 loudness: 0.5).						s add: m.						s add: (FMSound new setPitch: 1 dur: 0.05 loudness: 0)]]].	^ s play.! !!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:49'!dialTone: duration	"AbstractSound dialTone: 2"	| m |	m _ MixedSound new.	m add: (FMSound new setPitch: 350 dur: duration loudness: 0.5).	m add: (FMSound new setPitch: 440 dur: duration loudness: 0.5).	m play.	^ m! !!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:50'!hangUpWarning: count	"AbstractSound hangUpWarning: 20"	| m s |	s _ SequentialSound new.	m _ MixedSound new.	m	add: (FMSound new setPitch: 1400 dur: 0.1 loudness: 0.5);		add: (FMSound new setPitch: 2060 dur: 0.1 loudness: 0.5).	s add: m; add: (FMSound new setPitch: 1 dur: 0.1 loudness: 0).	^ (RepeatingSound repeat: s count: count) play! !