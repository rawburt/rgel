  $ rgel -entry main -quickjs defs/basics.rgel
  3 == 3
  $ rgel -entry main -quickjs defs/notfound-error.rgel
  defs/notfound-error.rgel:3:16-3:24: Identifier not found: not_real
  [1]
  $ rgel -entry main -quickjs defs/ret-error.rgel
  defs/ret-error.rgel:3:3-3:10: Type mismatch: expected string but got int
  [1]
