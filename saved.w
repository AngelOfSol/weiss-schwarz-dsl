          O       error: invalid type
code:5:3
	expected: fn(@7, @6) -> @7
	found: fn(i32) -> @5            1: i32    
       true: bool   T       error: invalid type
code:5:3
	expected: fn(bool, @5) -> bool
	found: fn(bool) -> @5    T       error: invalid type
code:5:3
	expected: fn(bool, @3) -> bool
	found: fn(bool) -> @3     
       true: bool           some 2: i32?   �       Parsing Error: Error { input: LocatedSpan { offset: 7, line: 1, fragment: "(let ((my (fn (x: i32) -> T x) (my 2))", extra: () }, code: Tag }   �       Parsing Error: Error { input: LocatedSpan { offset: 7, line: 1, fragment: "(let ((my (fn (x: i32) -> T x) (my 2)))", extra: () }, code: Tag }           2: i32   :       error: invalid type
code:1:45
	expected: i32
	found: bool    9       error: invalid type
code:4:7
	expected: i32
	found: bool     
       true: bool   :       error: invalid type
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
2: cardB       (print 
	(let 
		((my (fn (x: i32) (print x)))) 
		(card (my 2))))o       	load 2: i32
	call my-0
	call card
	print
	return
'my-0:
	store @x-1
	load-ref @x-1
	print
	unload @x-1
	return                                                                                              	                                 
                                                                         4          3          2                      
                                                                             	                                 2       3       4                                  