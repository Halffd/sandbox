import kotlin.concurrent.*
import java.util.concurrent.locks.ReentrantLock
import kotlinx.coroutines.*
import kotlinx.coroutines.sync.*

class Counter {
    public var count = 0
    public val lock = ReentrantLock()
    
    fun increment() {
        synchronized(lock) {  // Same as Java
            count++
        }   
    }
    
    // Or with extension function magic:
    fun incrementWithStyle() {
        lock.withLock {
            count++
        }
    }
}

class AsyncCounter {
    public var count = 0
    public val mutex = Mutex()
    
    suspend fun increment() {
        mutex.withLock {  // Suspending, not blocking!
            count++
        }
    }
}

fun main() {
    val counter = Counter()
    val threads = (1..1000).map {
        thread {
            counter.increment()
        }
    }
    threads.forEach { it.join() }
    println(counter.count)

    // Semaphores with coroutines
    val semaphore = Semaphore(3)

    suspend fun doWork() {
        semaphore.withPermit {
            // Critical section - suspends instead of blocking
            delay(1000) // This doesn't block the thread!
        }
    }
}