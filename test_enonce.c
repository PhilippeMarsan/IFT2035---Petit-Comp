{
  y = 1;
  p:
  while (1) {
    y = y*2;
    if (y > 100) break;
    x = y;
    b:
    while (x > 0)
    {
      if (x == 5) {
        print(y);
        continue p;
      }
      x = x-3;
    }
  }
}
