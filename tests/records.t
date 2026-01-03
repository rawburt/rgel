  $ rgel -entry main -quickjs records/fields1-error.rgel
  records/fields1-error.rgel:10:19-10:41: Record field mismatch: expected Person(age: int, name: string) but got Person(name: string)
  [1]
  $ rgel -entry main -quickjs records/methods1.rgel
  Vec(x = 33, y = 63)
  $ rgel -entry main -quickjs records/rec1.rgel
