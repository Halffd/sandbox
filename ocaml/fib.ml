let rec fibonacci n =
  Printf.printf "Computing fibonacci(%d)\n" n;
  if n < 2 then n
  else fibonacci (n - 1) + fibonacci (n - 2)

let () = Printf.printf "Result: %d\n" (fibonacci 5)
