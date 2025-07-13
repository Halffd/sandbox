using System;

interface IA
{
    void Foo(); // Declaration only
}

interface IB : IA
{
    // No implementation here
}

interface IC : IA
{
    // No implementation here
}

class D : IB, IC
{
    public void Foo() => Console.WriteLine("D"); // Implementing the method
}

public class Diamond2
{
    public static void Run()
    {
        D d = new D();
        ((IA)d).Foo(); // Output: D
    }
}