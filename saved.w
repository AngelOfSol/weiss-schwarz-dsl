                 invalid fn: my_f   >       invalid type for fn return type, expected i32, found i32?: "x"   4       invalid type for my_f, expected i32?, found i32: "2"   <       invalid type for my_f, expected i32?, found T0?: "(some  2)"           2: i32   -       Parsing Error: Error { input: "", code: Tag }   7       invalid type for my_f, expected i32?, found ()?: "none"           2: i32   k       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())))", code: Tag }   l       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ()))))", code: Tag }   m       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())))))", code: Tag }   k       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())))", code: Tag }   j       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ()))", code: Tag }   i       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())", code: Tag }   k       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ())))", code: Tag }   -       Parsing Error: Error { input: "", code: Tag }   4       invalid type for my_f, expected i32?, found i32: "2"           2: i32   l       Parsing Error: Error { input: "(let \n\t((my_f (fn (x: ?i32) -> i32 2)))\n\t(my_f (some ()))))", code: Tag }   <       invalid type for my_f, expected i32?, found (): "(some  ())"B       (print (let 
	((my_f (fn (x: ?i32) -> i32 2)))
	(my_f (some ()))))j       	load 2: i32
	call some
	call my_f-0
	print
	return
'my_f-0:
	store @x-1
	load 2: i32
	unload @x-1
	return                                                                                                                                          
                                 	                             2           3          4                                
                                            2       3       4                                                   	                                      