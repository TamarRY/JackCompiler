Class {
	#name : #Targil2,
	#superclass : #Object,
	#classVars : [
		'globe'
	],
	#category : #Ex1
}

{ #category : #adding }
Targil2 >> addHack [
	"translates vm add command to hack "

	| cr r|
	cr:= Character cr.
	cr:= cr asString .
	r:= '// vm command: add', cr,'@SP', cr,'A=M-1', cr,'D=M',cr,'A=A-1', cr,'M=D+M', cr, '@SP',cr,'M=M-1', cr.
   ^r .
]

{ #category : #'as yet unclassified' }
Targil2 >> andHack [
	"translates vm and command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: and', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1', cr,'M=D&M', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil2 >> callHack: funcName and: args [
		 	|cr result counter|
			cr := Character cr.
			cr := cr asString .
			counter :=globe getCounter. 
			result := '//push return address',cr,'@',funcName asString ,'.ReturnAddress',counter asString,cr,'D=A',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr,
			'//push LCL',cr,'@LCL',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr,
			'//push ARG',cr,'@ARG',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr,
			'//push THIS',cr,'@THIS',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr,
			'//push THAT',cr,'@THAT',cr,'D=M',cr,'@SP',cr,'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr,
			'//ARG=sp-n-5',cr,'@', args asString, cr, 'D=A', cr, '@', 5 asString , cr, 'D=D+A', cr, '@SP', cr, 'D=M-D', cr, '@ARG', cr, 'M=D', cr, 
			'//LCL=SP',cr,'@SP',cr,'D=M',cr,'@LCL',cr,'M=D',cr,'//go to the new func',cr,'@',funcName asString,cr,'0;JMP',cr,
			'//label for return address',cr,'(',funcName asString,'.ReturnAddress', counter asString ,')',cr.
			^result 
			
]

{ #category : #'as yet unclassified' }
Targil2 >> eqHack [
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
Targil2 >> functionHack: f and: k [
			|cr result counter|
			cr := Character cr.
			cr := cr asString .
			result := '//function ',f asString,cr,'(',f asString ,')',cr.
			counter := k asInteger .
			[ counter = 0 ] whileFalse: [ result := result,'@0',cr,'D=A',cr,'@SP',cr, 'A=M',cr,'M=D',cr,'@SP',cr,'M=M+1',cr. counter := counter-1. ].
		   ^result
]

{ #category : #'as yet unclassified' }
Targil2 >> gotoHack: fileName and: label [
		|cr result|
		cr := Character cr.
		cr := cr asString.
		result := '//goto',label asString ,cr,'@',fileName asString,'.',label asString ,cr,'0;JMP',cr.
		^result
]

{ #category : #'as yet unclassified' }
Targil2 >> gtHack [
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

{ #category : #'as yet unclassified' }
Targil2 >> ifGotoHack: fileName and: label [
	| cr result |
	cr := Character cr.
	cr := cr asString.
	result := '// vm command: if-goto ' , label , cr , '@SP' , cr
		, 'M=M-1' , cr , 'A=M //top stack element' , cr
		, 'D=M' , cr , '@' , fileName asString, '.' , label asString , cr
		, 'D;JNE' , cr.
	^ result
]

{ #category : #initialization }
Targil2 >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation" 
	globe := Global new.
]

{ #category : #'as yet unclassified' }
Targil2 >> labelHack: fileName and: c [
			|cr result|
			cr := Character cr.
			cr := cr asString.
			result := '// vm command: label' , cr , '(' , fileName asString , '.' , c asString , ')'
		, cr.
		^result

	
]

{ #category : #'as yet unclassified' }
Targil2 >> ltHack [
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
Targil2 >> mainProgram: arg [
	"comment stating purpose of message"
	self pathFileIterator: arg.
]

{ #category : #'as yet unclassified' }
Targil2 >> negHack [
   "translates vn add command to hack "
	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: neg', cr,'@SP', cr,'A=M-1', cr,'D=M', cr,'@0', cr,'D=A-D',cr,'@SP',cr,'A=M-1',cr,'M=D',cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil2 >> notHack [
   "translates vm not command to hack "
	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: not', cr,'@SP', cr,'A=M-1', cr,'D=M', cr,'D=!D',cr,'@SP',cr,'A=M-1',cr,'M=D',cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil2 >> orHack [
	"translates vm or command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: or:', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1	', cr,'M=D|M', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]

{ #category : #'as yet unclassified' }
Targil2 >> pathFileIterator: path [ 
	"gets a directory, finds vm files and creates asm files"
	| p vms fName streamWrite vmsLen streamRead  line splitLine writeString first |
	p := FileLocator home / path.
	vms := p allChildren
		select: [ :arg2 | arg2 basename endsWith: 'vm' ].
   fName:= p basename asString , '.asm'.
	 streamWrite := (FileLocator home / path / fName ) writeStream.
	 vmsLen := vms size.
	vmsLen >1 
	ifTrue: [ |write cr|cr:= Character cr. 	cr:= cr asString .write := '@256',cr,'D=A',cr,'@SP',cr,'M=D',cr. write:=write asString , (self callHack: 'Sys.init' and: 0) asString  . streamWrite nextPutAll: write.].
	[ vmsLen = 0 ]
		whileFalse: [ |fileref fileName| fileref := (vms at: vmsLen ) asFileReference .
			"tmp6" streamRead := fileref readStream  .
			fileName := fileref basename.
			fileName := $. split: fileName.
			fileName := fileName at: 1.
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
						ifTrue: [ |fNamein|fNamein:= fileName asString.writeString := self popHack: line and: (fNamein )   ].
					first = 'push'
						ifTrue: [ |fNamein|fNamein:= fileName asString.writeString := self pushHack: line and: (fNamein )   ].
					first = 'label'
						ifTrue: [ |fNamein| fNamein:= fileName asString.writeString := self labelHack:  fNamein  and: (splitLine at: 2) ].
					first = 'goto'
						ifTrue: [ |fNamein| fNamein:= fileName.writeString := self gotoHack: fNamein  and: (splitLine at: 2) ].
					first = 'if-goto'
						ifTrue: [ |fNamein| fNamein:= fileName.writeString := self ifGotoHack: fNamein  and: (splitLine at: 2) ].
					first = 'function'
						ifTrue: [|fNamein| fNamein:= splitLine at: 2. writeString := self functionHack: fNamein and: (splitLine at: 3) ].
					first = 'return'
						ifTrue: [ writeString := self returnHack  ].
					first = 'call'
						ifTrue: [ |fNamein| fNamein:= splitLine at: 2. writeString := self callHack: fNamein  and: (splitLine at: 3) ].
					streamWrite  nextPutAll: writeString ].
			streamRead  close ].
	streamWrite  close. 
]

{ #category : #'as yet unclassified' }
Targil2 >> popHack: command and: fileName [
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
Targil2 >> pushHack: command and: fileName [
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
Targil2 >> returnHack [
	| cr  result| "tmp1" "tmp2"
	cr := Character cr.
	cr := cr asString.
	result := '// vm command: return' , cr
		, '//FRAME=LCL: saves the address of the current func local var'
		, cr , '@LCL' , cr , 'D=M' , cr
		, '//RET= *(FRAME-5), RAM[13]=(LCL-5): temp for the return address'
		, cr , '@5' , cr , 'A=D-A' , cr , 'D=M' , cr , '@13' , cr
		, 'M=D' , cr
		, '//*ARG=POP(): puts the return value in the ARG segment of the calling func'
		, cr , '@SP' , cr , 'A=M-1' , cr , 'D=M' , cr , '@ARG'
		, cr , 'A=M' , cr , 'M=D' , cr
		, '//SP= ARG+1: sp points to afterARG' ,cr , '@ARG' , cr , 'D=M+1'
		, cr , '@SP' , cr , 'M=D' , cr , '//THAT= *(FRAME -1)' , cr
		, '@LCL' , cr , 'A=M-1' , cr
		, 'D=M // D holds the address of THAT segment of the calling func'
		, cr , '@THAT' , cr , 'M=D' , cr , '//THIS= *(FRAME-2)' , cr
		, '@LCL' , cr , 'A=M-1 //that' , cr , 'A=A-1//this' , cr
		, 'D=M // D holds the address of THIS segment of the calling func'
		, cr , '@THIS' , cr , 'M=D' , cr , '//ARG=*(FRAME-3)' , cr
		, '@LCL' , cr , 'A=M-1 //that' , cr , 'A=A-1//this',cr
		, 'A=A-1 //arg' , cr
		, 'D=M // D holds the address of ARG segment of the calling func'
		, cr , '@ARG' , cr , 'M=D' , cr , '//LCL=*(FRAME-4)' , cr
		, '@LCL' , cr , 'A=M-1 //that' , cr , 'A=A-1 //this',cr
		, 'A=A-1 //arg' , cr,'A=A-1 //lcl' , cr
		, 'D=M // D holds the address of LCL segment of the calling func'
		, cr , '@LCL' , cr , 'M=D' , cr , '//goto RET' , cr , '@13'
		, cr , 'A=M// A holds the return address' , cr , '0;JMP' , cr.
	^ result
]

{ #category : #'as yet unclassified' }
Targil2 >> subHack [
	"translates vm sub command to hack "

	| cr result|
	cr:= Character cr.
	cr:= cr asString .
	result:= '// vm command: sub', cr,'@SP', cr, 'A=M-1', cr,'D=M', cr,'A=A-1	', cr,'M=M-D	', cr,'@SP', cr,'M=M-1', cr.
   ^result .
]
