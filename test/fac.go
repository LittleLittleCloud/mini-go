func fac(x int) int {
	if(x>1){
		t:=fac(x-1);
		return t*x
	}else{
		return 1

	}
}
{
	print fac(10)
}
