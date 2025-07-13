
data class Product(
    val name: String,
    val category: String,
    val price: Double,
)

fun main() {
    val products = listOf(
        Product("Laptop", "Electronics", 999.99),
        Product("Smartphone", "Electronics", 499.99),
        Product("T-Shirt", "Clothing", 19.99),
        Product("Jeans", "Clothing", 39.99),
        Product("Coffee Maker", "Home Appliances", 89.99),
    )

    val groupedProducts = products.groupBy { it.category }

    groupedProducts.forEach { group ->
        println("Category: ${group.key}")
        group.value.forEach { product ->
            println(" - ${product.name}: $${product.price}")
        }
    }
}