using System;

interface IBase
{
    void DoWork();
}

interface IFirst : IBase
{
    void FirstWork();  // Remove the default implementation
}

interface ISecond : IBase  
{
    void SecondWork(); // Remove the default implementation
}

class Diamond : IFirst, ISecond
{
    // Explicit implementation of IBase.DoWork
    void IBase.DoWork()
    {
        Console.WriteLine("Common work in DiamondClass");
    }

    // Actually implement the methods properly
    public void FirstWork()
    {
        Console.WriteLine("First specific work");
    }
    
    public void SecondWork()
    {
        Console.WriteLine("Second specific work");
    }
    
    public void DoWork()
    {
        Console.WriteLine("=== Diamond Problem Demo ===\n");
        
        Console.WriteLine("Calling through IBase:");
        ((IBase)this).DoWork();
        
        Console.WriteLine("\nCalling interface-specific methods:");
        FirstWork();
        SecondWork();
    }
}