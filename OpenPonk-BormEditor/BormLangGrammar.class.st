"
I define and parse grammar of BORM Lang.

!! Synchronization
The inverse operation (taking model and producing DSL) is provided by BormLangExporter

!! Example

Also see BormDiagramExamples for some examples.
[[[
'Person ""Customer"" {
	initial state ""wants pizza""
		go to ""order"" if ""hungry""
	
	activity ""order""
		send ""pizza choice"" to ""Operator::accepts order"" and receive ""confirmation""
		go to ""wait""
	
	state ""wait for pizza"" as ""wait""
		go to ""pay""
	
	activity ""accepts pizza and pays"" as ""pay""
		go to ""final""
	
	final state ""purchase completed"" as ""final""
}
System ""Operator"" {
	activity ""accepts order""
}
' asBorm open
]]]
"
Class {
	#name : 'BormLangGrammar',
	#superclass : 'PPCompositeParser',
	#instVars : [
		'participants',
		'participant',
		'participantType',
		'string',
		'stringEnclosed',
		'newWord',
		'space',
		'stateType',
		'stateEnd',
		'state',
		'activity',
		'transition',
		'transitions',
		'newLine',
		'communication',
		'communications',
		'node',
		'node_',
		'nodes',
		'namePair',
		'namePairOpened',
		'name',
		'nameOpened',
		'constraint',
		'diagram',
		'activityStart'
	],
	#category : 'OpenPonk-BormEditor-Dsl',
	#package : 'OpenPonk-BormEditor',
	#tag : 'Dsl'
}

{ #category : 'accessing' }
BormLangGrammar >> activity [
	^ (activityStart , space , namePair) ==> [ :arr | arr last ]
]

{ #category : 'accessing' }
BormLangGrammar >> activityEnd [
	^ ')' asParser
	"activityStart
	^ 'activity' asParser"
]

{ #category : 'accessing' }
BormLangGrammar >> activityStart [
	^ '(' asParser
	"activityStart
	^ 'activity' asParser"
]

{ #category : 'accessing' }
BormLangGrammar >> communication [
	^ (('talk to ' asParser , name) ==> [ :arr | { arr second . nil . nil } ])
		/ (('send ' asParser , name , ' to ' asParser , name , (' and receive ' asParser , name) optional) ==> [ :arr | { arr fourth . arr second . arr fifth ifNotNil: [ arr fifth second ] } ])
]

{ #category : 'accessing' }
BormLangGrammar >> communications [
	^ communication trim star
]

{ #category : 'accessing' }
BormLangGrammar >> constraint [
	^ (space , 'if' asParser , space , name) optional ==> [ :arr | arr ifNotNil: [ arr fourth ] ]
]

{ #category : 'accessing' }
BormLangGrammar >> diagram [
	^ participants
	"diagram
	^ participants"
]

{ #category : 'accessing' }
BormLangGrammar >> name [
	"^  (namePair/((stringEnclosed/ newWord) ==> [:w | {w.w}]))"
	"	^ ($' asParser , string , $' asParser) ==> [ :arr | arr second ]"
	^ ($" asParser , string , $" asParser) ==> [ :arr | arr second ]
]

{ #category : 'accessing' }
BormLangGrammar >> nameOpened [
	"^  (namePairOpened /(string ==> [:w | {w.w}]))"
	"	^ ($' asParser , string , $' asParser) ==> [ :arr | arr second ]"
	^ ($" asParser , string , $" asParser) ==> [ :arr | arr second ]
]

{ #category : 'accessing' }
BormLangGrammar >> namePair [
	"^ ( ((  stringEnclosed /newWord ), space, $: asParser,space,(  stringEnclosed /newWord ) ))
		map: [ :id :sp1 :c :sp2 :nm | { id.nm }]"
			^ (name , ((space , 'as' asParser , space , name) ==> [ :arr | arr fourth ]) optional)
		==> [ :arr | arr first -> (arr second ifNil: [ arr first ]) ]
]

{ #category : 'accessing' }
BormLangGrammar >> namePairOpened [
	^  ((string/space), space, $: asParser,space,(string/space) )
		map: [ :id :sp1 :c :sp2 :nm | { id.nm }]
		"	^ (name , ((space , 'as' asParser , space , name) ==> [ :arr | arr fourth ]) optional)
		==> [ :arr | arr first -> (arr second ifNil: [ arr first ]) ]"
]

{ #category : 'accessing' }
BormLangGrammar >> newLine [
	^ Character cr asParser plus
]

{ #category : 'accessing' }
BormLangGrammar >> newWord [

	^ #word asParser  plus flatten
]

{ #category : 'accessing' }
BormLangGrammar >> node [
	^ activity / state , communications , transitions
]

{ #category : 'accessing' }
BormLangGrammar >> node_ [
	^ activity / state 
	"node
	^ activity / state , communications , transitions"
]

{ #category : 'accessing' }
BormLangGrammar >> nodes [
	^ node trim star
	"nodes
	^ node trim star"
]

{ #category : 'accessing' }
BormLangGrammar >> "participant [
	^ (participantType , space , namePair , space , ${ asParser , space , nodes
		, space , $} asParser) map: [ :type :sp :pair :sp2 :bra :sp3 :vertices :sp4 :ket
 | { type . pair . vertices }
			  ]"
		participant
	^ (participantType , space , namePair , space , ${ asParser , #newline asParser , nodes
		, #newline asParser optional , $} asParser) map: [ :type :sp :pair :sp2 :bra :nl :vertices :nl2 :ket
 | { type . pair . vertices }
			  ]


]

{ #category : 'accessing' }
BormLangGrammar >> participantType [
	^  'Person' asParser / 'Organization' asParser / 'System' asParser
	"participantType
	^ 'Person' asParser / 'Organization' asParser / 'System' asParser"
]

{ #category : 'accessing' }
BormLangGrammar >> participants [
	^ participant  trim star
	"participants
	^ participant trim star"
]

{ #category : 'accessing' }
BormLangGrammar >> space [
	"^#space asParser star"
	^ Character space asParser plus
]

{ #category : 'accessing' }
BormLangGrammar >> start [
	^ diagram end
]

{ #category : 'accessing' }
BormLangGrammar >> state [
	^ (stateType , space) optional , 'state' asParser , space , namePair
		map: [ :type :smb :sp :pair | 
			{(type ifNotNil: [ type first ]).
			pair} ]
]

{ #category : 'accessing' }
BormLangGrammar >> stateEnd [
	^ (']]' asParser / ']' asParser ) flatten
	"^ ('final' asParser / 'initial and final' asParser / 'initial' asParser) flatten"
]

{ #category : 'accessing' }
BormLangGrammar >> stateType [
	"^ ('[[*' asParser / '[*' asParser / '[[' asParser/ '[' asParser) flatten"
	^ ('final' asParser / 'initial and final' asParser / 'initial' asParser) flatten
]

{ #category : 'accessing' }
BormLangGrammar >> string [
	"^ (space,newWord ,space ) plus flatten"
		^ (#word asParser / Character space asParser / '::' asParser) plus flatten
]

{ #category : 'accessing' }
BormLangGrammar >> stringEnclosed [
	^ ($" asParser, (string/space) ,$" asParser ) map: [ :u1 :st :u2 | st ]
	"	^ (#word asParser / Character space asParser / '::' asParser) plus flatten"
		
]

{ #category : 'accessing' }
BormLangGrammar >> transition [
	^ 'go to ' asParser , name , constraint
		map: [ :goTo :pair :condition | 
			{pair. condition} ]
]

{ #category : 'accessing' }
BormLangGrammar >> transitions [
	^ transition trim star
]
