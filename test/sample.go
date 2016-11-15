{
  x := 1;
  y := 10;
  ch := newChannel;

  go {  while y>0 {
           ch <- 1;
           y=y-1
        }
     };

  while y>0 { 
    print <-ch
  };
 x = x + 1
}