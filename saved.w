                 invalid fn name "draw2"   ?       type error: invalid type
code:8:48
	expected: i32
	found: card    ?       type error: invalid type
code:8:48
	expected: i32
	found: bool    @       type error: invalid type
code:8:42
	expected: card
	found: bool    @       type error: invalid type
code:12:15
	expected: bool
	found: i32           invalid fn name "draw2"   @       type error: invalid type
code:10:48
	expected: i32
	found: bool    A       type error: invalid type
code:9:20
	expected: ?bool
	found: bool     
       true: bool           none: ?   ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0    ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0    ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0    ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0    ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0    e       type error: invalid type
code:9:32
	expected: fn(?bool, bool) -> bool
	found: fn(?bool, ?@0) -> bool            some true: bool?   e       type error: invalid type
code:9:32
	expected: fn(?bool, bool) -> bool
	found: fn(?bool, ?@0) -> bool    ?       type error: invalid type
code:9:32
	expected: bool
	found: ?@0     
       true: bool�       (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern or_default fn(?T, T) -> T)



(print (or_default (some true) false))M       	load false: bool
	load true: bool
	call some
	call or_default
	print
	return                                                  
                      	                                                                                                                               4          2           3                                	                                            2       3       4                                                              
                           