Class {
	#name : #Targil1,
	#superclass : #Object,
	#classVars : [
		'globe'
	],
	#category : #Ex1
}

{ #category : #adding }
Targil1 >> addHack [
	"translates vm add command to hack "

	| cr r|
	cr:= Character cr.
	cr:= cr asString .
	r:= '// vm command: add', cr,'@SP', cr,'A=M-1', cr,'D=M',cr,'A=A-1', cr,'M=D+M', cr, '@SP',cr,'M=M-1', cr.
   ^r .
]

{ #category : #'as yet unclassified' }
Targil1 >> andHack [
	"translates vm and command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: and', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1', cr,'M=D&M', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil1 >> eqHack [
"returns a string which represents the eq vm command in asm"
	|cr result labelTrue labelFalse counter|
	cr:= Character cr.
	cr:=cr asString.
	counter :=globe getCounter. 
	labelTrue :='IF_TRUE',counter asString .
	labelFalse := 'IF_FALSE',counter asString.
	result :='//vm command eq',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'A=A-1',cr,'D=D-M',cr,'@',labelTrue,cr,'D;JEQ',cr,'D=0',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D',cr,'@',labelFalse,cr,'0;JMP',cr,'(',
	labelTrue,')',cr,'D=-1',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D',cr,'(',labelFalse,')',cr,'@SP',cr,'M=M-1',cr.
	^result.
]

{ #category : #'as yet unclassified' }
Targil1 >> gtHack [
"comment stating purpose of message"
	| result cr labelTrue labelFalse counter|
	cr:=Character cr.
	cr:=cr asString .
	counter:= globe getCounter.
	labelTrue :='IF_TRUE',counter asString .
	labelFalse :='IF_FALSE',counter asString.
	result:='//vm command gt',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'A=A-1',cr,'D=M-D',cr,'@',labelTrue,cr,'D;JGT',cr,'D=0',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D', cr,'@',labelFalse ,cr,'0;JMP',cr,'(',labelTrue ,')',cr,'D=-1',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D',cr,'(',labelFalse,')',cr,'@SP',cr,'M=M-1',cr.
	^result .
]

{ #category : #initialization }
Targil1 >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation" 
	globe := Global new.
]

{ #category : #'as yet unclassified' }
Targil1 >> ltHack [
	"comment stating purpose of message"
	|cr result labelTrue labelFalse counter|
	cr:=Character cr.
	cr:= cr asString.
	counter:=globe  getCounter .
	labelTrue :='IF_TRUE',counter asString.
	labelFalse:='IF_FALSE',counter asString.
	result:='//vm command lt',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'A=A-1',cr,'D=M-D',cr,'@',labelTrue,cr,'D;JLT',cr,'D=0',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D',cr,'@',labelFalse,cr,'0;JMP',cr,'(',labelTrue,')',cr,'D=-1',cr,'@SP',cr,'A=M-1',cr,'A=A-1',cr,'M=D',cr,'(',labelFalse ,')',cr,'@SP',cr,'M=M-1',cr.
	^result.
]

{ #category : #'as yet unclassified' }
Targil1 >> mainProgram: arg [
	"comment stating purpose of message"
	self pathFileIterator: arg.
]

{ #category : #'as yet unclassified' }
Targil1 >> negHack [
   "translates vn add command to hack "
	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: neg', cr,'@SP', cr,'A=M-1', cr,'D=M', cr,'@0', cr,'D=A-D',cr,'@SP',cr,'A=M-1',cr,'M=D',cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil1 >> notHack [
   "translates vm not command to hack "
	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: not', cr,'@SP', cr,'A=M-1', cr,'D=M', cr,'D=!D',cr,'@SP',cr,'A=M-1',cr,'M=D',cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil1 >> orHack [
	"translates vm or command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: or:', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1	', cr,'M=D|M', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil1 >> pathFileIterator: path [ 
	"gets a directory, finds vm files and creates asm files"
	| p vms fName streamWrite vmsLen streamRead  line splitLine writeString first |
	p := FileLocator home / path.
	vms := p allChildren
		select: [ :arg2 | arg2 basename endsWith: 'vm' ].
	fName:= p basename asString , '.asm'.
	"tmp5" streamWrite := (FileLocator home / path / fName ) writeStream.
	"tmp7" vmsLen := vms size.
	[ vmsLen = 0 ]
		whileFalse: [ |fileref| fileref := (vms at: vmsLen ) asFileReference .
			"tmp6" streamRead := fileref readStream  .
			vmsLen  := vmsLen  - 1.
			[ streamRead  atEnd ]
				whileFalse: [ "tmp8" line := streamRead  nextLine.
					"tmp9" splitLine := Character space split: line .
					"tmp11" first := splitLine  at: 1.
					writeString := ''. "tmp10 is going to be the hack command"
					first = 'add'
						ifTrue: [ writeString  := self addHack ].
					first = 'sub'
						ifTrue: [ writeString  := self subHack  ].
					first  = 'neg'
						ifTrue: [ writeString := self negHack ].
					first = 'eq'
						ifTrue: [ writeString := self eqHack ].
					first = 'gt'
						ifTrue: [ writeString := self gtHack ].
					first = 'lt'
						ifTrue: [ writeString := self ltHack ].
					first = 'and'
						ifTrue: [ writeString := self andHack  ].
					first = 'not'
						ifTrue: [ writeString := self notHack ].
					first = 'or'
						ifTrue: [ writeString := self orHack ].
					first = 'pop'
						ifTrue: [ writeString := self popHack: line and: (p basename )   ].
					first = 'push'
						ifTrue: [ writeString := self pushHack: line and: (p basename )   ].
					streamWrite  nextPutAll: writeString ].
			streamRead  close ].
	streamWrite  close. 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


	
	
	

]

{ #category : #'as yet unclassified' }
Targil1 >> popHack: command [
	"vm command: pop segment index"
	| parsed segment result index cr shift foo|
	parsed := command findTokens: $ .
	segment:= parsed at: 2.
	index := parsed at: 3.
	"foo := [ |b r| r:=''. b:= index asInteger . [b=0] whileFalse: [  r:=r,'A=A+1',cr.b:=b-1.]. ^r]."
	cr:= Character cr.
	cr:=cr asString .
	segment = 'argument' ifTrue: [ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A+1',cr.tmp:=tmp-1. ].
		result:='//vm command pop argument ',index asString ,cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@ARG',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment = 'local' ifTrue:[ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A+1',cr.tmp:=tmp-1. ].
		result:='//vm command pop local ',index asString,cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@LCL',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	"segment = 'static' ifTrue: []."
	segment ='this' ifTrue: [  |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A+1',cr.tmp:=tmp-1. ].
		result:='//vm command pop this ',index asString,cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@THIS',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment ='that' ifTrue: [ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A+1',cr.tmp:=tmp-1. ].
		result:='//vm command pop that ',index asString,cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@THAT',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment ='pointer' ifTrue: [ index:='0' ifTrue:[ result:='//vm command pop pointer 0',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@THIS',cr,'M=D',cr,'@SP',cr,'M=M-1',cr.].
		index := '1' ifFalse:[ result:='//vm command pop pointer 1',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@THAT',cr,'M=D',cr,'@SP',cr,'M=M-1',cr.].].
	segment ='temp' ifTrue: [shift := 5+ index asInteger.  result :='// vm command: pop temp', index,'@SP', cr,'A=M-1', cr,'D=M', cr,'@',shift asString, cr, 'M=D', cr, '@SP', cr,'M=M-1',cr.].
	^result .
	

]

{ #category : #'as yet unclassified' }
Targil1 >> popHack: command and: fileName [
	"vm command: pop segment index"
	| parsed segment result index cr shift |
	parsed := command findTokens: $ .
	segment:= parsed at: 2.
	index := parsed at: 3.
	cr:= Character cr.
	cr:=cr asString .
	segment = 'argument' ifTrue: [ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A=A+1',cr.tmp:=tmp-1. ].
		result:='//vm command pop argument',cr,'@SP',cr,'A=M-1',cr,'D=M',cr,'@2',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment = 'local' ifTrue:[ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A=A+1',cr.tmp:=tmp-1. ].
		result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@1',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment = 'static' ifTrue: [ result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@',fileName,'.',index,cr,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment ='this' ifTrue: [  |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A=A+1',cr.tmp:=tmp-1. ].
		result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@3',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment ='that' ifTrue: [ |tmp| shift:=''. tmp:=index asInteger .[tmp=0] whileFalse: [ shift:=shift,'A=A+1',cr.tmp:=tmp-1. ].
		result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@4',cr,'A=M',cr,shift,'M=D',cr,'@SP',cr,'M=M-1',cr.].
	segment ='pointer' ifTrue: [ index ='0' ifTrue:[ result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@3',cr,'M=D',cr,'@SP',cr,'M=M-1',cr.].
		index = '1' ifTrue:[ result:='@SP',cr,'A=M-1',cr,'D=M',cr,'@4',cr,'M=D',cr,'@SP',cr,'M=M-1',cr.].].
	segment ='temp' ifTrue: [shift := 5+ index asInteger.  result :='// vm command: pop temp', index,cr,'@SP', cr,'A=M-1', cr,'D=M', cr,'@',shift asString, cr, 'M=D', cr, '@SP', cr,'M=M-1',cr.].
	^result .
	

]

{ #category : #'as yet unclassified' }
Targil1 >> pushHack: command [
   "vm command: push segment index"
	| parsed segment result index cr shift|
	parsed := command findTokens: $ .
	segment:= parsed at: 2.
	index := parsed at: 3.
	cr:= Character cr.
	cr:=cr asString .
	segment = 'argument' ifTrue: [ result:='@',index,cr,'D=A',cr,'@ARG',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment = 'local' ifTrue:[ result:='@',index,cr,'D=A',cr,'@LCL',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='constant' ifTrue: [ result:='@',index,cr,'D=A',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='this' ifTrue: [result:='@',index,cr,'D=A',cr,'@ THIS',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr. ].
	segment ='that' ifTrue: [result:='@',index,cr,'D=A',cr,'@ THAT',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='temp' ifTrue: [ result:='@5',cr,'D=A',cr,'@',index,cr,'A=A+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='pointer' ifTrue: [ index ='0' ifTrue:[ result:='@THIS',cr,'A=M',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	index='1' ifTrue:[ result:= '@THAT',cr,'A=M',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].].
	^result .
	
]

{ #category : #'as yet unclassified' }
Targil1 >> pushHack: command and: fileName [
   "vm command: push segment index"
	| parsed segment result index cr shift|
	parsed := command findTokens: $ .
	segment:= parsed at: 2.
	index := parsed at: 3.
	cr:= Character cr.
	cr:=cr asString .
	segment = 'argument' ifTrue: [ result:='//vm command push argument ',index asString,cr,'@',index,cr,'D=A',cr,'@2',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment = 'local' ifTrue:[ result:='//vm command push local',index asString,cr,'@',index,cr,'D=A',cr,'@1',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment = 'static' ifTrue: [ result:='//vm command push static ',index asString,cr,'@',fileName asString,'.',index,cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='constant' ifTrue: [ result:='//vm command push constant ',index asString,cr,'@',index,cr,'D=A',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='this' ifTrue: [result:='//vm command push this ',index asString ,cr,'@',index,cr,'D=A',cr,
		'@3',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr. ].
	segment ='that' ifTrue: [result:='//vm command push that ',index asString,cr,'@',index,cr,'D=A',cr,
		'@4',cr,'A=M+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	segment ='pointer' ifTrue: [ index ='0' ifTrue:[ result:='//vm command push pointer 0',cr,'@3',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
		index='1' ifTrue:[ result:= '//vm commnad push pointer 1',cr,'@4',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].].
	segment ='temp' ifTrue: [ result:='//vm command push temp ',index asString ,cr,'@',index,cr,'D=A',cr,'@5',cr,'A=A+D',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr.].
	^result .
	
]

{ #category : #'as yet unclassified' }
Targil1 >> subHack [
	"translates vm sub command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: sub', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1	', cr,'M=M-D	', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]
