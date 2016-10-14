func fac(x int) int {
  if x > 1 {
    return x * fac(x-1)
  } else {
    return 1
  }
}

{
  x := fac(4);
  print x
}
