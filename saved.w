          >       runtime error: invalid type
	found true: bool
	expected ZoneId           1: i32           1: i32           7: i32           6: i32           6: i32           6: i32   p       type error: mismatched types
 --> <editor>:11:10
  | 
  | (my_test 1 
  |          ^ expected: bool, found: i32
   p       type error: mismatched types
 --> <editor>:11:10
  | 
  | (my_test 1 
  |          ^ expected: bool, found: i32
           6: i32   p       type error: mismatched types
 --> <editor>:11:10
  | 
  | (my_test 1 
  |          ^ expected: bool, found: i32
   y       type error: mismatched types
 --> <editor>:11:12
  | 
  | (my_test 1 true
  |            ^^^^ expected: i32, found: bool
           6: i32   z       type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: i32, found: bool
   
      type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: zone, found: bool

type error: mismatched types
 --> <editor>:7:21
  | 
  |   (if (== x 0) 0 (+ x
  |                     ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:30
  | 
  |   (if (== x 0) 0 (+ x(tri (- x 
  |                              ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:11
  | 
  |   (if (== x 
  |           ^ expected: i32, found: zone
   
      type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: zone, found: bool

type error: mismatched types
 --> <editor>:7:21
  | 
  |   (if (== x 0) 0 (+ x
  |                     ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:30
  | 
  |   (if (== x 0) 0 (+ x(tri (- x 
  |                              ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:11
  | 
  |   (if (== x 
  |           ^ expected: i32, found: zone
         type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: zone, found: bool

type error: mismatched types
 --> <editor>:7:21
  | 
  |   (if (== x 0) 0 (+ x 
  |                     ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:31
  | 
  |   (if (== x 0) 0 (+ x (tri (- x 
  |                               ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:11
  | 
  |   (if (== x 
  |           ^ expected: i32, found: zone
         type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: zone, found: bool

type error: mismatched types
 --> <editor>:7:21
  | 
  |   (if (== x 0) 0 (+ x 
  |                     ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:31
  | 
  |   (if (== x 0) 0 (+ x (tri (- x 
  |                               ^ expected: i32, found: zone

type error: mismatched types
 --> <editor>:7:11
  | 
  |   (if (== x 
  |           ^ expected: i32, found: zone
   z       type error: mismatched types
 --> <editor>:9:13
  | 
  | (print (tri true
  |             ^^^^ expected: i32, found: bool
           1: i32�       (include "externs.w")
(include "defines.w")

(define my_test (fn (x: MyT y: MyT) -> MyU 2))

(define tri (fn (x)
  (if (== x 0) 0 (+ x (tri (- x 1))))))

(print (tri 1))

(my_test 1 1)�      	load rust@card
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
	load my_test
	store @my_test
	load tri
	store @tri
	load 1: i32
	call tri
	call print
	load 1: i32
	load 1: i32
	call my_test
	unload @my_test
	unload @tri
	return
'my_test:
	store @x
	store @y
	load 2: i32
	unload @x
	unload @y
	return
'tri:
	store @x
	load 0: i32
	load-ref @x
	call ==
	jump-if-true 'if-true-#0
	load 1: i32
	load-ref @x
	call -
	call tri
	load-ref @x
	call +
	jump 'end-if-#0
'if-true-#0:
	load 0: i32
'end-if-#0:
	unload @x
	return                                                                        	                                            
                                                                                   4          3          2                                                                  	                      2       3       4                             
                                                 