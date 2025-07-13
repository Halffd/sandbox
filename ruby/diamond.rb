module A; def foo; puts "A" end end
module B; include A; def foo; puts "B" end end
module C; include A; def foo; puts "C" end end

class D
  include B
  include C # Last included module (C) overrides
end

D.new.foo # Output: C