          |       type error: mismatched types
 --> <editor>:3:14
  | 
  | (print (== 2 true
  |              ^^^^ expected: i32, found: bool
   �       parsing error: nom error
 --> <editor>:1:2
  | 
  | (include "externs.w")
  |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ nom error
   �       parsing error: nom error
 --> <editor>:1:2
  | 
  | (include "externs.w")
  |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ nom error
   j       parsing error: nom error
 --> <editor>:3:2
  | 
  | (print (== 2 true))
  |  ^^^^^^^^^^^^^^^^^^ nom error
   �       parsing error: nom error
 --> <editor>:1:2
  | 
  | (include "externs.w")
  |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ nom error
   �       parsing error: nom error
 --> <editor>:1:2
  | 
  | (include "externs.w")
  |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ nom error
   �       parsing error: nom error
 --> <editor>:1:2
  | 
  | (include "externs.w")
  |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ nom error
   �       parsing error: nom error
 --> <editor>:5:6
  | 
  | (let x
  |      ^ nom error

parsing error: expected "("
 --> <editor>:5:6
  | 
  | (let x
  |      ^ expected "("
   �       parsing error: nom error
 --> <editor>:5:6
  | 
  | (let x
  |      ^ nom error

parsing error: expected "("
 --> <editor>:5:6
  | 
  | (let x
  |      ^ expected "("
   �       parsing error: nom error
 --> <editor>:5:6
  | 
  | (let x
  |      ^ nom error

parsing error: expected "("
 --> <editor>:5:6
  | 
  | (let x
  |      ^ expected "("
   �       parsing error: nom error
 --> <editor>:5:6
  | 
  | (let x
  |      ^ nom error

parsing error: expected "("
 --> <editor>:5:6
  | 
  | (let x
  |      ^ expected "("
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
   �       parsing error: nom error
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2))
  |                      ^^^ nom error

parsing error: expected ")"
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2))
  |                      ^^^ expected ")"
   �       parsing error: nom error
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)  )
  |                      ^^^^^ nom error

parsing error: expected ")"
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)  )
  |                      ^^^^^ expected ")"
   �       parsing error: nom error
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)
  |                      ^^ nom error

parsing error: expected ")"
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)
  |                      ^^ expected ")"
   �       parsing error: nom error
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)))
  |                      ^^^^ nom error

parsing error: expected ")"
 --> <editor>:5:22
  | 
  | (let ((x true) (== x 2)))
  |                      ^^^^ expected ")"
   y       parsing error: nom error
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
            0       (include "externs.w")


(let ((x 1)) (== x 2))
 �       load rust@card
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
load 1: i32
store @x
load 2: i32
load-ref @x
call ==
unload @x
return                                                                        
                                                                                                   	                             4          2           3                                           
                                            2       3       4                  	                                                                