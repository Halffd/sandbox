class a = object method foo = print_endline "A" end
class b = object inherit a method foo = print_endline "B" end
class c = object inherit a method foo = print_endline "C" end
class d = object inherit b inherit c (* Last parent (C) wins *) end

let () = (new d)#foo (* Output: C *)