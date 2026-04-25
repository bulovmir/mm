Class {
	#name : 'OPTextWindow',
	#superclass : 'Object',
	#category : 'OpenPonk-BormTextEditor',
	#package : 'OpenPonk-BormTextEditor'
}

{ #category : 'toolbar' }
OPTextWindow class >> toolbarMenu: aBuilder [
|diagram|
	<opEditorToolbarMenu: #OPBormPlugin>
	diagram := BormORModel named: 'Test Model'.
	diagram  addAll:{BormParticipant named:'User'}.
	(aBuilder item: 'Open Text Editor ')
		icon: OPBormEditorIcons oRModel16Icon;	
		action: [( OPProject new
		 name: 'Test';
		 addModel: ( diagram) ;
		 addModel: ( diagram) ;
		addModel: aBuilder model diagramController model  ;
		addModel: aBuilder model diagramController model  ;
		 open) showAllElementsInAllDiagrams ]
]
