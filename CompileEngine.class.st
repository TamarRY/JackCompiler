Class {
	#name : #CompileEngine,
	#superclass : #Object,
	#instVars : [
		'counter',
		'ifIndex',
		'whileIndex',
		'className',
		'symbolTable',
		'vmWriter'
	],
	#category : #Ex1
}

{ #category : #'as yet unclassified' }
CompileEngine >> ParseStatement: read [
| str line cr result|
result := ''.
cr := Character cr.
cr := cr asString .
line := read at: counter .
str := self getXMLElement: line.
str = ' let ' ifTrue: [ result := result, (self compileLetStatement: read) asString . ].
str = ' if ' ifTrue: [ result := result, (self compileIfStatement: read) asString .  ].
str = ' while ' ifTrue: [ result := result,cr,(self compileWhileStatement: read) asString . ].
str = ' do ' ifTrue: [ result := result,cr,(self compileDoStatement: read) asString . ].
str = ' return ' ifTrue: [  result := result,cr,(self compileReturnKey: read) asString . ].
^result.
]

{ #category : #compiling }
CompileEngine >> ParseStatements: read [
| cr result line element|
cr := Character cr.
cr := cr asString.
result:= '<statements> '.
line := read at: counter .
element := self getXMLElement: line.
[ element = ' } ' ] whileFalse: [ result := result,(self ParseStatement: read) asString ,cr. line := read at: counter. element := self getXMLElement: line. ].
result := result,'</statements>',cr.
^result.
]

{ #category : #compiling }
CompileEngine >> compileClass: read [ 
	"comment stating purpose of message"
	| cr result line word id tmp |
	symbolTable := SymbolTable new.
	vmWriter:= VMWriter new.
	ifIndex := 0. whileIndex := 0.
	cr:=Character cr.
	cr:= cr asString.
	counter := 2. 
	result := ''. "<identifier> class </identifier>"
	counter :=counter +1. "<identifier> className </identifier>"
	className:= self getXMLElementTrim: ((read at: counter) asString). "class name"
	counter :=counter +1."<symbol> { </symbol>"
	counter := counter + 1. 
	tmp:= counter .
	line:= read at: (counter )   . " static | field| constructor| function |method  }"
	word:= self getXMLElement: line.
	[(word =' static ') | (word= ' field ') ]
		whileTrue: [ self compileClassVarDec: read.  line := (read at: counter) asString ." static | field| int| char |boolean | className | }" word:= self getXMLElement: line. ] .
	[ (word =' constructor ')| (word=' function ')|(word=' method ') ]
		whileTrue: [  result := result, (self compileSubroutine: read) asString ,cr.  line:= (read at: counter) asString . "int| char |boolean | className | }"
			word:= self getXMLElement: line. ].
	^result.
]

{ #category : #compiling }
CompileEngine >> compileClassVarDec: read [
	"comment stating purpose of message"
   |  cr  line kind type name |
	cr:=Character cr.
	cr:= cr asString.
	line:= read at: counter . "static | field"  kind := self getXMLElementTrim: line.
	counter := counter +1. line:= read at: counter. "type" type := self getXMLElementTrim: line.
	counter := counter +1. line:= read at: counter. "varName" name := self getXMLElementTrim: line.
	symbolTable define: name and: type and: kind.
	counter := counter + 1.
	[ ( self getXMLElement: (read at: counter)) = ' , ' ]
		whileTrue: [ line:= read at: counter. "," 
			counter := counter +1. line:= read at: counter. "varName" name := self getXMLElementTrim: line. symbolTable define: name and: type and: kind.
			counter := counter + 1. ].
	line:= read at: counter. ";"
	counter := counter + 1.
]

{ #category : #compiling }
CompileEngine >> compileDoStatement: read [
	|cr result|
	cr := Character cr.
	cr := cr asString.
	result := ''.
	counter := counter + 1."do"
	result := result, (self compileSubRoutineCall: read) asString ,(vmWriter writePop: 'temp' and: 0 ),cr.
	counter := counter + 1. ";"
	^result . 
]

{ #category : #compiling }
CompileEngine >> compileExpression: read [
	| line cr result ops word op opsDict|
	ops := #(' + ' ' - ' ' * ' ' / ' ' &amp; ' ' | ' ' &gt; ' ' &lt; ' ' = ') asArray .
	opsDict :=  Dictionary new.
	opsDict at: '+' put: 'add'.opsDict at: '-' put: 'sub'.opsDict at: '=' put: 'eq'.opsDict at: '&gt;' put: 'gt'.opsDict at: '&lt;' put: 'lt'.opsDict at: '&amp;' put: 'and'.
	opsDict at: '|' put: 'or'.
	cr := Character cr.
	cr := cr asString.
	result := ( self compileTerm: read) asString .	
	[ line := read at: counter .word := self getXMLElement: line.ops includes: word ] 
			whileTrue: [ op:= self getXMLElementTrim: line. counter := counter+1. "op" result := result,(self compileTerm: read) asString.
							 (opsDict keys includes: op) ifTrue: [ result := result,(vmWriter writeArithmetic: (opsDict at: op) asString). ].
							 op = '*' ifTrue: [ result := result,((vmWriter writeCall: 'Math.multiply' and: 2) asString) .  ].
							 op = '/' ifTrue: [ result := result,((vmWriter writeCall: 'Math.divide' and: 2) asString) .  ].
						  ].
	^result.
	
]

{ #category : #compiling }
CompileEngine >> compileIfStatement: read [
| cr line result word ifInd|
cr := Character cr.
cr := cr asString .
result := ''. 
ifInd := ifIndex asString  .
ifIndex := ifIndex +1.
counter := counter + 1."("
counter := counter +1. "expression"
result := result,(self compileExpression: read) asString . " expression" 
counter := counter + 1. ")"
counter  := counter + 1."{"
result := result , (vmWriter writeIf: ('IF_TRUE', ifInd)),cr, (vmWriter writeGoto: ('IF_FALSE',ifInd )),cr,(vmWriter writeLabel: ('IF_TRUE', ifInd)),cr, (self compileStatements: read) asString, (vmWriter writeGoto: ('IF_END',ifInd )),cr.
counter := counter +1. "}"
result := result, (vmWriter writeLabel: ('IF_FALSE',ifInd )),cr.
line := read at: counter.
word := self getXMLElement: line.
word = ' else ' ifTrue: [ counter := counter + 1."else"
								 counter  := counter + 1"{". result := result,(self compileStatements: read). counter := counter + 1. "}"
								].
result := result, (vmWriter writeLabel: ('IF_END',ifInd )),cr.
^result.
]

{ #category : #compiling }
CompileEngine >> compileKeyword: read [
	|result keyword|
	keyword := read at: counter . keyword := self getXMLElementTrim: keyword .
	counter := counter + 1.
	keyword = 'this' ifTrue: [ result := vmWriter writePush: 'pointer' and: 0.  ]
						  ifFalse: [ result := vmWriter writePush: 'constant' and: 0. keyword = 'true' ifTrue: [ result := result , (vmWriter writeArithmetic: 'not'). ]  												].
	^result 

	
]

{ #category : #compiling }
CompileEngine >> compileLetStatement: read [
| result cr line str varName varKind varIndex|
result := ''.
cr := Character cr.
cr := cr asString.
line:=(read at: counter ). "let"
counter := counter +1.
line:=(read at: counter ). "identifier,varName"
varName := self getXMLElementTrim: line.
varKind := symbolTable kindOf: varName .
varIndex := symbolTable indexOf: varName .
counter := counter + 1.
line := read at: counter. " [ or ="
str := self getXMLElement: line.
"counter := counter +1."
str = ' [ ' ifTrue: [ counter := counter +1.result:=result,(self compileExpression: read) asString,cr. line:=(read at: counter ) "]",cr. counter := counter +1."=" 
	result :=result , (vmWriter writePush: varKind  and: varIndex) asString ,cr, (vmWriter writeArithmetic: 'add') asString, cr. counter := counter +1. result:=result,(self compileExpression: read) asString,cr. counter := counter +1. ";" result := result , (vmWriter writePop: 'temp' and:0 ) asString "expression result" ,cr, (vmWriter writePop: 'pointer' and:1 ) asString ,cr, (vmWriter writePush: 'temp' and: 0) ,(vmWriter writePop: 'that' and:0 ) asString ,cr.] 
	ifFalse: [  counter := counter +1. "=" result := result ,(self compileExpression: read) asString,cr. counter := counter +1. ";" result := result , (vmWriter writePop: varKind  and: varIndex  ).].
^result.
]

{ #category : #compiling }
CompileEngine >> compileParameterList: read [
	"comment stating purpose of message"
| cr  line word id type name|
	cr:=Character cr.
	cr:= cr asString.

	line := read at: counter.
	word := self getXMLElement: line.
	id := self getTokenType: line.
	( (word=' int ') | (word = ' char ') | (word = ' boolean ') | (id = 'identifier'))
	ifTrue: [ counter := counter + 1. type:= self getXMLElementTrim: line."type"
	line := read at: counter . counter := counter + 1. name:= self getXMLElementTrim: line."varName"
	symbolTable define: name and: type  and: 'argument'.
	[ line := read at: counter. word:= self getXMLElement: line. word = ' , ']
		whileTrue: [ ",".counter := counter + 1."type" .type:= self getXMLElementTrim:((read at: counter) asString ) .
			         counter := counter + 1."varName" name:= self getXMLElementTrim:((read at: counter) asString) .
			          symbolTable define: name and: type  and: 'argument'.counter:=counter + 1. ]. ].

	
]

{ #category : #compiling }
CompileEngine >> compileReturnKey: read [
	|cr result line word|
	cr := Character cr.
	cr := cr asString.
	result := ''.
	counter := counter + 1."return"
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ; ' ifFalse: [ result := result,(self compileExpression: read) asString .line := read at: counter.]
					 ifTrue: [ result := result , (vmWriter writePush: 'constant' and: 0 ) asString ,cr. ].
	result := result, (vmWriter writeReturn ) asString  ,cr.
	counter := counter + 1. ";"
	^result.
]

{ #category : #'as yet unclassified' }
CompileEngine >> compileStatement: read [
| str line cr result|
result := ''.
cr := Character cr.
cr := cr asString .
line := read at: counter .
str := self getXMLElement: line.
str = ' let ' ifTrue: [ result := result, (self compileLetStatement: read) asString . ].
str = ' if ' ifTrue: [ result := result, (self compileIfStatement: read) asString .  ].
str = ' while ' ifTrue: [ result := result,cr,(self compileWhileStatement: read) asString . ].
str = ' do ' ifTrue: [ result := result,cr,(self compileDoStatement: read) asString . ].
str = ' return ' ifTrue: [  result := result,cr,(self compileReturnKey: read) asString . ].
^result.
]

{ #category : #compiling }
CompileEngine >> compileStatements: read [
| cr result line element|
cr := Character cr.
cr := cr asString.
result:= ''.
line := read at: counter .
element := self getXMLElement: line.
[ element = ' } ' ] whileFalse: [ result := result,(self compileStatement: read) asString ,cr. line := read at: counter. element := self getXMLElement: line. ].
^result.
]

{ #category : #compiling }
CompileEngine >> compileString: read [
	| cr result str lenStr |
	cr:= Character cr.
	cr := cr asString .
	str := read at: counter. 
	counter := counter + 1.
	str := self getXMLElementTrim: str.
	result := (vmWriter writePush: 'constant' and: (str size)),(vmWriter writeCall: 'String.new' and: 1).
	 1 to: str size do: [ :i| result := result,(vmWriter writePush: 'constant' and: ((str at: i) asciiValue)),(vmWriter writeCall: 'String.appendChar' and: 2). ].
	^result 
]

{ #category : #compiling }
CompileEngine >> compileSubRoutineCall: read [
	|cr result line word funcName paramNum identifier type instanceKind instanceIndex|
	paramNum:=0.
	cr := Character cr.
	cr := cr asString.
	result:= ''.
	line := read at: counter. "identifier" 
	identifier := self getXMLElementTrim: line.
	funcName := identifier .
	counter := counter+1.
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ( ' ifTrue: [ "(" funcName := className  asString ,'.', identifier asString . paramNum := paramNum +1.result:= vmWriter writePush: 'pointer' and: 0.  ].
	word = ' . ' ifTrue: [ counter := counter + 1."." funcName := self getXMLElementTrim: (read at: counter ). type:= symbolTable typeOf: identifier .
			 "not static"		type ~='none' ifTrue: [ instanceKind := symbolTable kindOf: identifier .instanceIndex := symbolTable indexOf: identifier .
										                      result := result , (vmWriter writePush: instanceKind  and: instanceIndex). funcName := type asString, '.',                                                              funcName asString . paramNum := paramNum +1.]
			"static of any class"					ifFalse: [ funcName := identifier asString, '.', funcName . ].
									counter := counter+1. "("].
	counter := counter + 1. "beginning of expressions"
	line := read at: counter.
	word := self getXMLElement: line.
	word = ' ) ' ifFalse: [ paramNum := paramNum +1.result := result,(self compileExpression: read) asString .
								   line := read at: counter. word := self getXMLElement: line.
									[ word = ' , ' ] whileTrue: [ counter := counter + 1.result := result,(self compileExpression: read) asString. paramNum:= paramNum +1.
																			line := read at: counter. word := self getXMLElement: line.] ].
	counter := counter +1. ")"	
	result := result , (vmWriter writeCall: funcName  and: paramNum) .
	^result. 
]

{ #category : #compiling }
CompileEngine >> compileSubroutine: read [
	|  cr result line funcKind funcName  word  localCount fieldCount|
	symbolTable startSubroutine .
	cr:=Character cr.
	cr:= cr asString.
	result:= ''.
	line:= read at: counter . "constructor | function | method"
	funcKind := self getXMLElementTrim: line.
	counter := counter +1. line:= read at: counter . "void | type"
	counter := counter +1. line:= read at: counter . "subroutine name"
	funcName := self getXMLElementTrim: line.
	funcKind = 'method'
		ifTrue:[symbolTable define: 'this' and: className   and: 'argument' ].
	counter := counter +1. line:= read at: counter . "("
	counter := counter + 1.
	 self compileParameterList: read.
	line:= read at: counter ")".
	 counter := counter + 1. line:= read at: counter . "{"
	"result := result , (self compileSubroutineBody: read) asString,cr."
	"subroutine body"
	counter := counter + 1. "body"
	line := read at: counter. word :=(self getXMLElement: line).
	[  word = ' var ' ]
		whileTrue: [  self compileVarDec: read.line := read at: counter. word :=(self getXMLElement: line).].
	funcName := className ,'.' ,funcName .
	localCount := symbolTable varCount: 'local'.
	result := result, (vmWriter writeFunction: funcName  and: localCount) asString ,cr .
	funcKind ='constructor'
		ifTrue: [ fieldCount :=symbolTable varCount: 'this'. 
			result := result , (vmWriter writePush: 'constant'  and: fieldCount) asString , (vmWriter writeCall: 'Memory.alloc'  and: 1 ) asString , (vmWriter writePop: 'pointer' and: 0 ).  ].
	funcKind = 'method'
		ifTrue: [ result := result , (vmWriter writePush: 'argument'  and: 0) asString , (vmWriter writePop: 'pointer'  and: 0 ) asString . ].
	result:= result, (self compileStatements: read) asString,cr.
	line:= read at: counter . "}"
	counter := counter + 1.
	^result .
]

{ #category : #compiling }
CompileEngine >> compileSubroutineBody: read [
	| cr result1 line word |
	cr:=Character cr.
	cr:= cr asString.
	result1:= '<subroutineBody>',cr.
	line:= read at: counter . "{"
	counter := counter + 1.
	result1 := result1 , line, cr.
	line := read at: counter. word :=(self getXMLElement: line).
	[  word = ' var ' ]
		whileTrue: [ |tmp| tmp := self parseVarDec: read.result1 := result1 ,tmp, cr. line := read at: counter. word :=(self getXMLElement: line).].
	result1 := result1 , (self ParseStatements: read) asString,cr.
	line:= read at: counter . "}"
	counter := counter + 1.
	result1 := result1 , line, cr, '</subroutineBody>',cr.
	^result1 .
]

{ #category : #compiling }
CompileEngine >> compileSubroutineDec: read [
	|  cr result line funcKind funcName  word  localCount fieldCount|
	symbolTable startSubroutine .
	cr:=Character cr.
	cr:= cr asString.
	result:= ''.
	line:= read at: counter . "constructor | function | method"
	funcKind := self getXMLElementTrim: line.
	counter := counter +1. line:= read at: counter . "void | type"
	counter := counter +1. line:= read at: counter . "subroutine name"
	funcName := self getXMLElementTrim: line.
	funcKind = 'method'
		ifTrue:[symbolTable define: 'this' and: className   and: 'argument' ].
	counter := counter +1. line:= read at: counter . "("
	counter := counter + 1.
	 self compileParameterList: read.
	line:= read at: counter ")".
	 counter := counter + 1. line:= read at: counter . "{"
	"result := result , (self compileSubroutineBody: read) asString,cr."
	"subroutine body"
	counter := counter + 1.
	line := read at: counter. word :=(self getXMLElement: line).
	[  word = ' var ' ]
		whileTrue: [  self compileVarDec: read.line := read at: counter. word :=(self getXMLElement: line).].
	funcName := funcName ,'.' ,className .
	localCount :=symbolTable varCount: 'local'.
	result := result, (vmWriter writeFunction: funcName  and: localCount) asString ,cr .
	funcKind ='constractor'
		ifTrue: [ fieldCount :=symbolTable varCount: 'this'. 
			result := result , (vmWriter writePush: 'constant'  and: fieldCount) asString , (vmWriter writeCall: 'Memmory.alloc'  and: 1 ) asString .  ].
	funcKind = 'method'
		ifTrue: [ result := result , (vmWriter writePush: 'argument'  and: 0) asString , (vmWriter writePop: 'pointer'  and: 0 ) asString . ].
	result:= result, (self compileStatements: read) asString,cr.
	line:= read at: counter . "}"
	counter := counter + 1.
	^result .
]

{ #category : #compiling }
CompileEngine >> compileTerm: read [
	|cr result line type word nextLine unaryopsDict varName varKind varIndex|
	unaryopsDict := Dictionary new. unaryopsDict at: ' ~ ' put: 'not'.unaryopsDict at: ' - ' put: 'neg'.
	cr := Character cr.
	cr := cr asString.
	result := ''.
	line := read at: counter .
	type := self getTokenType: line.
	word := self getXMLElement: line.
	type = 'integerConstant' ifTrue: [ word := self getXMLElementTrim: line.
												 result := result,(vmWriter writePush: 'constant' and:word) asString ,cr.counter:=counter+1.^result. ].
	type = 'stringConstant' ifTrue: [ result := result,(self compileString: read). ^result ].
	type = 'keyword' ifTrue: [ result := result,(self compileKeyword: read).^result. ].
	((word = ' - ' )|( word = ' ~ ')) ifTrue: [ counter := counter + 1. 
															 result := result, (self compileTerm: read) asString ,(vmWriter writeArithmetic: (unaryopsDict at: word)),cr.^result ].
	word = ' ( ' ifTrue: [ counter:=counter+1"(".result:=result,(self compileExpression: read) asString.counter:=counter+1")".^result ].
	nextLine := read at: (counter+1).
	nextLine := self getXMLElement: nextLine .
	nextLine = ' [ ' ifTrue: [ |arrayName arrayIndex arrayKind| arrayName := self getXMLElementTrim: line.counter:=counter+1.counter:=counter+1"[".
											result:=result,(self compileExpression: read) asString .counter := counter+1"]".
											arrayKind := symbolTable kindOf: arrayName . arrayIndex := symbolTable indexOf: arrayName.
											result :=result,(vmWriter writePush: arrayKind  and: arrayIndex) ,(vmWriter writeArithmetic: 'add').
											result := result,(vmWriter writePop: 'pointer' and: 1),(vmWriter writePush: 'that' and: 0).  
											 ^result.].
	(nextLine = ' ( ' | nextLine = ' . ') ifTrue: [ result:=result,(self compileSubRoutineCall: read) asString .^result. ].
	varName := self getXMLElementTrim: (read at: counter ). varKind := symbolTable kindOf: varName . varIndex := symbolTable indexOf: varName .
	result := result,(vmWriter writePush: varKind   and: varIndex ). 
	counter:=counter+1.
	^result.
]

{ #category : #compiling }
CompileEngine >> compileVarDec:read [
	"comment stating purpose of message"
| cr  line  type name|
	cr:=Character cr.
	cr:= cr asString.
	line:= read at: counter . "var" 
	counter := counter +1. line:= read at: counter . "type" type:= self getXMLElementTrim: line. 
	counter := counter +1. line:= read at: counter . "var name" name:= self getXMLElementTrim: line.
	symbolTable define: name and: type  and: 'local'.
	counter := counter + 1.
	[ line := read at: counter .( self getXMLElement: line) asString =' , ' ]
		whileTrue: [   counter := counter +1. line:= read at: counter. "varName"name:= self getXMLElementTrim: line. 
			            symbolTable define: name and: type  and: 'local'. counter := counter +1.].
	counter := counter + 1.

	
]

{ #category : #parsing }
CompileEngine >> compileWhileStatement: read [
	|cr result line whileInd|
	whileInd := whileIndex asString .
	whileIndex := whileIndex + 1.
	cr := Character cr.
	cr := cr asString .
	result := (vmWriter writeLabel: ('WHILE',whileInd) ),cr.
	counter := counter + 1."while"
	counter := counter + 1."("
	result := result , (self compileExpression: read) asString .
	counter := counter + 1.")"
	counter := counter + 1."{"
	result := result,(vmWriter writeArithmetic: 'not').
	result := result, (vmWriter writeIf: ('WHILE_END',whileInd)),cr,(self compileStatements: read) asString , (vmWriter writeGoto: ('WHILE',whileInd)),cr,
	(vmWriter writeLabel: ('WHILE_END',whileInd)),cr.
	 counter := counter + 1."}"
	^result.
]

{ #category : #'as yet unclassified' }
CompileEngine >> compiler: path [
		|result   xmlt p s|
		p := FileLocator home / path.
      xmlt:= p allChildren select: [ :arg2 | arg2 basename endsWith: 'T.xml' ].
		s:= xmlt size .
		[ s=0 ]
			whileFalse:[
				|fName split streamWrite read n|
				fName:= (xmlt  at: s) basename .split:= $T split: fName. fName := (split at: 1), '.vm'.
				streamWrite :=(FileLocator home / path / fName ) writeStream.
				n:=path,'\' ,((xmlt  at: s) basename ).
				read:=  (StandardFileStream oldFileNamed: n ) contents.
				read:= Character cr split: read.
				result := self compileClass: read.
				streamWrite nextPutAll: result .
				s:=s-1.
				streamWrite close .			
				].
]

{ #category : #'as yet unclassified' }
CompileEngine >> getTokenType: line [
	"comment stating purpose of message"
	| result tmp|
"line:='<identifier> className </identifier>'."
tmp:= $> split: line.
tmp:= tmp at: 1.
tmp:= $< split: tmp.
result := tmp at: 2.
^result.
]

{ #category : #'as yet unclassified' }
CompileEngine >> getXMLElement: str [
| result split temp |
split := $> split: str.
temp := split at: 2.
split := $< split: temp.
result := split at: 1.
^result.
]

{ #category : #'as yet unclassified' }
CompileEngine >> getXMLElementTrim: str [
	| result |
	result := self getXMLElement: str.
	result := result trimRight .
	result := result trimLeft .
	^result.
]

{ #category : #accessing }
CompileEngine >> main: path [
	"comment stating purpose of message"

	| |
	Targil4 new createTokenXML: path.
	self compiler: path.
]

{ #category : #parsing }
CompileEngine >> parseWhileStatement: read [
	|cr result line whileInd|
	whileInd := whileIndex asString .
	whileIndex := whileIndex + 1.
	cr := Character cr.
	cr := cr asString .
	result := (vmWriter writeLabel: ('WHILE',whileInd) ),cr.
	counter := counter + 1."while"
	counter := counter + 1."("
	result := result , (self compileExpression: read) asString .
	counter := counter + 1.")"
	counter := counter + 1."{"
	result := result, (vmWriter writeIf: ('WHILE_END',whileInd)),cr,(self compileStatements: read) asString , (vmWriter writeGoto: ('WHILE',whileInd)),cr,
	(vmWriter writeLabel: ('WHILE_END',whileInd)),cr.
	 counter := counter + 1."}"
	^result.
]
