          y       parsing error: nom error
 --> <editor>:5:26
  | 
  | (let ((x true)) (== x 2)))
  |                          ^ nom error
         type error: mismatched types
 --> <editor>:5:21
  | 
  | (let ((x true)) (== x 
  |                     ^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:3:14
  | 
  | (print (== 2 true
  |              ^^^^ expected: i32, found: bool
   |       type error: mismatched types
 --> <editor>:3:14
  | 
  | (print (== 2 true
  |              ^^^^ expected: i32, found: bool
               �       parsing error: nom error
 --> <editor>:7:3
  | 
  |   (f)
  |   ^^^^^^ nom error

parsing error: expected ")"
 --> <editor>:7:3
  | 
  |   (f)
  |   ^^^^^^ expected ")"
   h       symbol error: invalid symbol
 --> <editor>:6:13
  | 
  |   (f (fn () x
  |             ^ invalid_symbol
           2: i32   �       parsing error: nom error
 --> <editor>:13:2
  | 
  |  
  |   nom error

parsing error: expected ")"
 --> <editor>:13:2
  | 
  |  
  |   expected ")"
          missing heap value name: x           label@f
2: i32   d       type error: mismatched types
 --> <editor>:4:7
  | 
  |  
  |       ^^^^ expected: i32, found: bool
   n       type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true)
  |       ^^^^ expected: i32, found: bool
         type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   r      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true 2)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   r      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true 2)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   r      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true 2)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   r      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true 2)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   �      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true hand)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:5:10
  | 
  | (== true hand)
  |          ^^^^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   �      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true hand)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:5:10
  | 
  | (== true hand)
  |          ^^^^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
   �      type error: mismatched types
 --> <editor>:5:5
  | 
  | (== true false)
  |     ^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:5:10
  | 
  | (== true false)
  |          ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:19
  | 
  | (== 2 true) (== 4 false)
  |                   ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:7
  | 
  | (== 2 true) (== 4 false)
  |       ^^^^ expected: i32, found: bool
@       (include "externs.w")


(== 2 true) (== 4 false)
(== true false)M      load rust@card
store @card
load rust@move
store @move
load rust@some
store @some
load rust@or_default
store @or_default
load rust@+
store @+
load rust@-
store @-
load rust@==
store @==
load x
store @x
load f
store @f
load-ref @f
unload @f
unload @x
call print
call-dynamic
call print
return
	'x:
load 2: i32
return
	'f:
call x
return                                                                                                                                                                           
           	                  3          4          2                                            
                                            2       3       4                  	                                                                