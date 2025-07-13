 let sqrt x = head . dropWhile ((>=0.1) . abs . (x-) . (^2)) $ iterate (\g -> (g + x/g)/2) 1
> sqrt 25