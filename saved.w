          �       Parsing Error: Error { input: LocatedSpan { offset: 6, line: 2, fragment: "((id \n\t\t(fn (x) x)\n\t))\n\t(id id))", extra: () }, code: Tag }          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0          missing heap value idx: 0                           "       error: ambiguous type
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
	found: fn(bool) -> @5 4       (print
	(let
		((id 
			(fn (x y) x)))
		(id true)))�       	load 2: i32
	load true: bool
	call id-0
	print
	return
'id-0:
	store @x-1
	store @y-2
	load-ref @x-1
	unload @x-1
	unload @y-2
	return                                                                        
                                            	                                                                                    4          2           3                                           
                                 	                      2       3       4                                                                   