                 missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0                           "       error: ambiguous type
code:4:6
	id   0       error: ambiguous type
code:4:6
	fn(@4) -> @4
	id                            
       true: bool           1: i32   O       error: invalid type
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
       true: bool           some 2: i32?       (print (some 2))&       	load 2: i32
	call some
	print
	return                                                             
                                                                                                                         	                  4          2           3                                           
                                 2       3       4                                                              	                           