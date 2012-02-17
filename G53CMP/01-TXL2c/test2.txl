! A somewhat less basic test.
(let x = 1 + 2
 in
     let y = x * x
     in
         let x = y + x + 42
         in x * x)
+ 2
