          :       error: invalid type
code:3:27
	expected: i32
	found: bool    :       error: invalid type
code:3:34
	expected: i32
	found: bool    :       error: invalid type
code:3:29
	expected: i32
	found: bool    :       error: invalid type
code:4:13
	expected: i32
	found: bool            2: i32
2: card           2: i32
2: card           2: i32
2: card   "       invalid fn: card  at line 4, col 3   �       Parsing Error: Error { input: LocatedSpan { offset: 13, line: 1, fragment: "(fn(i32) -> card))\n(print \n\t(let \n\t\t((my (fn (x: i32) (print x)))) \n\t\t(card (my 2))))", extra: () }, code: Tag }           2: i32
2: card   "       invalid fn: move  at line 4, col 1                
       hand: zone    
       hand: zone    
       deck: zone   "       invalid fn: some  at line 5, col 8           some 2: i32?           some 2: i32?   &       error: invalid extern
code:5:1
	broken   ;       error: invalid extern
code:5:1
	(extern broken fn(T) -> ())�       (extern card fn(i32) -> card)
(extern print fn(T) -> T)
(extern move fn(card, zone) -> zone)
(extern some fn(T) -> ?T)
(extern broken fn(T) -> ())

(print (some 2))&       	load 2: i32
	call some
	print
	return                                                                                              	                                                                                       
                  3          4          2                                 2       3       4                                        
                                                       	                                      