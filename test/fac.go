func fac(x int) int {
	if(x>2){
		t:=fac(x-1);
		b:=fac(x-2);
		return t+b
	}else{
		return 1

	}
}
{
	print fac(10)
}