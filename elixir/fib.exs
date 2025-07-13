defmodule Fib do
  def fibonacci(n) do
    IO.puts("Computing fibonacci(#{n})")
    cond do
      n < 2 -> n
      true -> fibonacci(n - 1) + fibonacci(n - 2)
    end
  end
end

IO.puts("Result: #{Fib.fibonacci(5)}")
