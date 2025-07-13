using System;
using System.Collections.Generic;
using System.Linq;

// Moved GetBooks inside Bookshelf class

// For the generic type system example:
public class BookCollection<T> where T : Book
{
    private List<T> books = new List<T>();
    
    public void AddBook(T book)
    {
        books.Add(book);
    }
    
    public T GetBook(int index)
    {
        return books[index];
    }
    
    public int Count => books.Count; // Use property instead of method
    
    public IEnumerable<T> Books => books; // Expose books for iteration
}

// For generics with constraints:
public class Repository<T> where T : class
{
    private readonly List<T> items = new();
    private readonly Func<T> _factory;
    
    // Default constructor for types with parameterless constructors
    public Repository()
    {
        var ctor = typeof(T).GetConstructor(Type.EmptyTypes);
        if (ctor == null)
            throw new InvalidOperationException($"Type {typeof(T).Name} does not have a parameterless constructor.");
        
        _factory = () => (T)ctor.Invoke(null);
    }
    
    // Constructor that accepts a factory method for types with required properties
    public Repository(Func<T> factory)
    {
        _factory = factory ?? throw new ArgumentNullException(nameof(factory));
    }
    
    public void Add(T item)
    {
        items.Add(item ?? throw new ArgumentNullException(nameof(item)));
    }
    
    public T CreateNew()
    {
        return _factory();
    }
    
    public T Get(int index)
    {
        if (index < 0 || index >= items.Count)
            throw new ArgumentOutOfRangeException(nameof(index));
            
        return items[index]!;
    }
    
    public int Count => items.Count;
    
    public IEnumerable<T> Items => items;
}

public class Book
{
    public required string Title { get; set; }
    public required string Author { get; set; }
    public int Pages { get; set; }
}

public class Bookshelf
{
    private static IEnumerable<Book> GetBooks(IEnumerable<Book> books)
    {
        int i = 0;
        foreach (Book book in books)
        {
            Console.WriteLine($"Book {i}: {book.Title}");
            yield return book;
            i++;
        }
    }
    private List<Book> books = new List<Book>(); // Make it a List, not IEnumerable
    
    public void AddBook(Book book)
    {
        books.Add(book);
    }
    
    public Book GetBook(int index)
    {
        return books[index];
    }
    
    public int Count => books.Count; // Use property instead of method
    
    public IEnumerable<Book> Books => books; // Expose books for iteration
    
    public static void Run()
    {
        Bookshelf bookshelf = new Bookshelf();
        bookshelf.AddBook(new Book { Title = "Book 1", Author = "Author 1", Pages = 100 });
        bookshelf.AddBook(new Book { Title = "Book 2", Author = "Author 2", Pages = 200 });
        bookshelf.AddBook(new Book { Title = "Book 3", Author = "Author 3", Pages = 300 });
        
        Console.WriteLine($"Bookshelf has {bookshelf.Count} books");
        Console.WriteLine($"Book 1: {bookshelf.GetBook(0).Title}");
        Console.WriteLine($"Book 2: {bookshelf.GetBook(1).Title}");
        Console.WriteLine($"Book 3: {bookshelf.GetBook(2).Title}");

        var booksFromShelf = GetBooks(bookshelf.Books);
        foreach (Book book in booksFromShelf)
        {
            Console.WriteLine($"Processed book: {book.Title}");
        }
        
        Repository<Book> repository = new Repository<Book>(() => new Book 
        { 
            Title = "Default Title", 
            Author = "Unknown Author", 
            Pages = 0 
        });
        
        // Now we can add books with all required properties
        repository.Add(new Book { Title = "Repo Book 1", Author = "Author 1", Pages = 100 });
        repository.Add(new Book { Title = "Repo Book 2", Author = "Author 2", Pages = 200 });
        repository.Add(new Book { Title = "Repo Book 3", Author = "Author 3", Pages = 300 });
        
        Console.WriteLine($"Repository has {repository.Count} books");
        Console.WriteLine($"Book 1: {repository.Get(0).Title}");
        Console.WriteLine($"Book 2: {repository.Get(1).Title}");
        Console.WriteLine($"Book 3: {repository.Get(2).Title}");
        
        var booksFromRepo = GetBooks(repository.Items);
        foreach (Book book in booksFromRepo)
        {
            Console.WriteLine($"Processed repo book: {book.Title}");
        }
        
        BookCollection<Book> bookCollection = new BookCollection<Book>();
        bookCollection.AddBook(new Book { Title = "Collection Book 1", Author = "Author 1", Pages = 100 });
        bookCollection.AddBook(new Book { Title = "Collection Book 2", Author = "Author 2", Pages = 200 });
        bookCollection.AddBook(new Book { Title = "Collection Book 3", Author = "Author 3", Pages = 300 });
        
        Console.WriteLine($"BookCollection has {bookCollection.Count} books");
        Console.WriteLine($"Book 1: {bookCollection.GetBook(0).Title}");
        Console.WriteLine($"Book 2: {bookCollection.GetBook(1).Title}");
        Console.WriteLine($"Book 3: {bookCollection.GetBook(2).Title}");
        
        var booksFromCollection = GetBooks(bookCollection.Books);
        foreach (Book book in booksFromCollection)
        {
            Console.WriteLine($"Processed collection book: {book.Title}");
        }
        
        // For nullable value types (C# 2 feature):
        int? nullableInt = null;
        if (nullableInt.HasValue)
        {
            Console.WriteLine($"Value: {nullableInt.Value}");
        }
        else
        {
            Console.WriteLine("Nullable int has no value");
        }
    }
}
