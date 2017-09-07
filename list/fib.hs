fib = 0:1 : [ x+y   | (x,y)<-zip fib (tail fib)]
