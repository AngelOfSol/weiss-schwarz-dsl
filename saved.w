                  1: i32           1: i32           1: i32           1: i32           1: i32           1: i32           1: i32           1: i32   t       type error: mismatched types
 --> <editor>:4:10
  | 
  | (if true true
  |          ^^^^ expected: i32, found: bool
   t       type error: mismatched types
 --> <editor>:4:10
  | 
  | (if true true
  |          ^^^^ expected: i32, found: bool
   x       type error: mismatched types
 --> <editor>:4:15
  | 
  | (if true true 1
  |               ^ expected: bool, found: i32
   �       type error: mismatched types
 --> <editor>:4:15
  | 
  | (if true true move
  |               ^^^^ expected: fn(card, zone) -> zone, found: bool
   �       type error: mismatched types
 --> <editor>:4:22
  | 
  | (if true true (fn () 1
  |                      ^ expected: bool, found: fn() -> i32
   �       type error: mismatched types
 --> <editor>:4:22
  | 
  | (if true true (fn () 1
  |                      ^ expected: bool, found: fn() -> i32
   w       type error: mismatched types
 --> <editor>:4:14
  | 
  | (if (if true 1 
  |              ^ expected: bool, found: i32
   �       type error: mismatched types
 --> <editor>:4:16
  | 
  | (if (if true 1 2
  |                ^ expected: bool, found: i32

type error: mismatched types
 --> <editor>:4:14
  | 
  | (if (if true 1 
  |              ^ expected: bool, found: i32
   o       type error: mismatched types
 --> <editor>:4:10
  | 
  | (if true 1 
  |          ^ expected: bool, found: i32
   �       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 false
  |            ^^^^^ expected: i32, found: bool

type error: mismatched types
 --> <editor>:4:10
  | 
  | (if true 1 
  |          ^ expected: bool, found: i32
   z       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 false
  |            ^^^^^ expected: i32, found: bool
   �       type error: mismatched types
 --> <editor>:4:12
  | 
  | (if true 1 move
  |            ^^^^ expected: i32, found: fn(card, zone) -> zone
=       (include "externs.w")
(include "defines.w")

(if true 1 move)        	load 1: i32
	call print
	return                                                                   	                                                                                                       
                             2           4          3                     
                      2       3       4                             	                                                                                             