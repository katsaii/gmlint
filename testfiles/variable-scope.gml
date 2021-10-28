{
  var val = 1;
  {
    // multiple definitions of the same local variable
    var val = 4;
  }
}
// using variable outside of the scope its defined in
val = 2;
