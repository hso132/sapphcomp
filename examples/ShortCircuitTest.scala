object hey {
  if(true || 5/0 < 1) {
    Std.printString("yay1")
  } else {
    ()
  };
  if(false && 5/0 < 1) {
    ()
  } else {
    Std.printString("yay1")
  }
}
