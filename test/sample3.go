func fun1(x int,y int){
	print(x);
	print(y)
}
func fun2(x int) int{
	return x
}
func fun3() int{
	print 300;
	return 300
}
func fun4(){
	print 400
}
{
	y:=5;
	x:=3;
	while y>0{
		fun1(3,5);
		print x;
		print fun2(x);
		fun3();
		print fun3();
		fun4();
		y=0
	};

	print ((x+1)/2)*3;
	print fun2(x)+x;
	if y>0{
		fun1(3,5);
		print fun2(x);
		
	}else{
		fun3();
		print fun3();
		fun4();
	}
}