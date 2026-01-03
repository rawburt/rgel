  $ rgel -entry main -quickjs ffi/poly.rgel
  TypeError: cannot read property 'deepEqual' of undefined
      at main (out.js:2:48)
      at <eval> (out.js:8:5)
  $ rgel -entry main -quickjs ffi/simple.rgel
  123
