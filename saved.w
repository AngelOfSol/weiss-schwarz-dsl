          k       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())))", code: Tag }   -       Parsing Error: Error { input: "", code: Tag }   4       invalid type for my_f, expected i32?, found i32: "2"           2: i32   l       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ()))))", code: Tag }   <       invalid type for my_f, expected i32?, found (): "(some  ())"           2: i32   ^       invalid type: expected fn(bool, i32, i32) -> i32, found fn(i32, i32, i32) -> i32: "(if 2 2 2)"           2: i32   f       invalid type: expected fn(bool, i32, i32) -> i32, found fn(bool, i32, bool) -> i32: "(if true 2 true)"   j       invalid type: expected fn(bool, bool, bool) -> bool, found fn(bool, bool, i32) -> bool: "(if true true 2)"   l       invalid type: expected fn(bool, bool, bool) -> bool, found fn(bool, bool, i32) -> bool: "(if 
	true true 2)"                       some 2: i32?           some 2: card?          invalid fn: none           some none: ??           none: ?           some 1: i32?           none: ?        (print (if false (some 1) none))�       	load false: bool
	jump-if-true 'if-true-0
	load none: ?
	jump 'end-if-0
'if-true-0:
	load 1: i32
	call some
'end-if-0:
	print
	return                                                             
                                                                             	                                                              4          3          2                      	                                                       
                      2       3       4                                                                   