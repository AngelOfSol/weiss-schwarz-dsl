                  some 2: card?   (       invalid fn: or_default  at line 6, col 8   B       error: invalid extern
code:5:1
	(extern or_default fn(?T, T) -> T)   ?       error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   '       invalid type, found 2: i32, expected ?T   6       runtime error: invalid type
	found 2: i32
	expected ?T           2: i32   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))   D       type error: ambiguous type
code:7:1
	@1
	(print (or_default none 2))           2: i32           2: i32           2: i32�       (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)

(print (or_default none 2)):       	load 2: i32
	load none: ?
	call or_default
	print
	return                                                                                                         	                                                      
                                        3          2           4                                2       3       4                                                                                               	           
                           