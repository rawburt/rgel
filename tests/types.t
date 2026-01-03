  $ rgel -entry main -quickjs types/mismatch-error.rgel
  types/mismatch-error.rgel:5:16-5:27: Type mismatch: expected string but got int
  types/mismatch-error.rgel:7:20-7:21: Identifier not found: d
  [1]
  $ rgel -entry main -quickjs types/rec2-error.rgel
  types/rec2-error.rgel:16:20-16:26: Type mismatch: expected A but got B
  [1]
