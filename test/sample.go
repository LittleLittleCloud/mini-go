{
  x := 1;
  y := true;
  ch := newChannel;

  go {  while y {
           ch <- 1
        }
     };

  while y {
    <-ch
  };
 x = x + 1
}
