BEGIN { FS = " "; OFS = "\t" }

$1 == "#define" {
  if ($2 !~ /_APS./) {
    printf("(define %-33s %s)\n", $2, $3)
  }
}

