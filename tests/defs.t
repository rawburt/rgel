  $ rgel -entry main -quickjs defs/basics.rgel
  3 == 3
  $ rgel -entry main -quickjs defs/notfround-error.rgel
  Fatal error: exception Sys_error("defs/notfround-error.rgel: No such file or directory")
  [2]
  $ rgel -entry main -quickjs defs/ret-error.rgel
  defs/ret-error.rgel:5:3-5:10: Type mismatch: expected string but got int
  [1]
