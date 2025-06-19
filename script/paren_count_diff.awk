BEGIN { diff=0 }
{
  o = split($0, a, "(") - 1
  c = split($0, b, ")") - 1
  diff += o - c
  printf("%04d %s\n", diff, $0)
}
