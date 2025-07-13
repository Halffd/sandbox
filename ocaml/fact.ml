(* Function definition *)
let f n =
  if n < 3 then n
  else
    let rec loop f1 f2 f3 count =
      let next_val = f1 + 2 * f2 + 3 * f3 in
      Printf.printf "f(%d) = %d + 2*%d + 3*%d = %d\n" 
                   count f1 f2 f3 next_val;
      if count = n then f1
      else loop next_val f1 f2 (count + 1) in
    loop 2 1 0 2
    
(* Helper to print a range of values *)
let print_f_values n_max =
  Printf.printf "Computing f(n) where f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) for n ≥ 3\n";
  Printf.printf "Base cases: f(0) = 0, f(1) = 1, f(2) = 2\n\n";
  for i = 0 to n_max do
    Printf.printf "f(%d) = %d\n" i (f i)
  done

(* Print values from 0 to 10 *)
let () = 
  print_f_values 10;
  
  (* Demonstrate step-by-step calculation for f(6) *)
  Printf.printf "\nDetailed calculation for f(6):\n";
  ignore(f 6)