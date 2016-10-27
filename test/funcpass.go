func f() {
  print 1;
  print 2
}

func fi() int {
	x := 1;
	return x
}

func fii (x int) int {
	y := x+1;
	return y*2
}

func fbib (b bool, i int) bool {
  if b && i == 0 {
  	return true
  } else {
  	return false
  }
}

{
  f();
  a := fi();
  b := fii(a);
  c := fbib(true, b);
  print c
}
