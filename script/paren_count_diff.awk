#!/usr/bin/env awk -f

BEGIN { diff = 0 }

{
  o = gsub(/\(/, "")
  c = gsub(/\)/, "")
  diff += o - c

  if ($0 ~ /^[[:space:]]*$/ && diff != 0) {
    msg = (diff > 0 ? "Possible missing closing parenthesis" : "Possible extra closing parenthesis")
    printf("Line %d: diff=%4d  %s\n", NR, diff, msg)
  }
}
