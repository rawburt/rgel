  $ rgel -entry main -quickjs types/mismatch-error.rgel
  types/mismatch-error.rgel:2:16-2:27: Type mismatch: expected string but got int
  types/mismatch-error.rgel:4:20-4:21: Identifier not found: d
  [1]
  $ rgel -entry main -quickjs types/rec2-error.rgel
  types/rec2-error.rgel:13:20-13:26: Type mismatch: expected A but got B
  [1]
