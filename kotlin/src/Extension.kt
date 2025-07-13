import java.util.UUID
import kotlin.system.exitProcess
import java.util.Date
data class Manifest (
    val name: String,
    val version: String,
    val author: String,
    val description: String,
    val dependencies: List<String>,
    val enabled: Boolean,
    val manifestVersion: Int,
    val permissions: List<String>,
    val path: String,
    val toString: () -> String = { "Manifest(name=$name, version=$version, author=$author, description=$description, dependencies=$dependencies, enabled=$enabled, manifestVersion=$manifestVersion, permissions=$permissions, path=$path)" }
)
class Extension {
    val id: UUID = UUID.randomUUID()
    var manifest: Manifest = Manifest("", "", "", "", emptyList(), true, 1, emptyList(), "")
    // Make these mutable so they can be set from manifest
    var name: String = ""
    var version: String = ""
    var debug: Boolean = false
    var enabled: Boolean = false
    // Custom logging functions instead of overriding globals
    protected fun log(message: String) {
        if (debug) {
            println("[$name] $message")
        } else {
            println(message)
        }
    }
    
    protected fun logDebug(message: String) {
        if (debug) {
            println("[$name] DEBUG: $message")
        }
    }
    
    protected fun safeExit(code: Int) {
        if (debug) {
            println("[$name] Would exit with code $code (debug mode)")
        } else {
            exitProcess(code)
        }
    }
    
    fun load() {
        log("Loading extension $name")
        logDebug("Debug mode enabled for extension $name")
        onLoad()
        run()
    }
    
    fun unload() {
        log("Unloading extension $name")
        logDebug("Debug mode enabled for extension $name")
        onUnload()
        stop()
    }
    
    fun reload() {
        unload()
        load()
    }
    
    fun enable() {
        enabled = true
        log("Extension $name enabled")
    }
    
    fun disable() {
        enabled = false
        log("Extension $name disabled")
    }
    
    fun isEnabled(): Boolean = enabled
    
    private fun run() {
        if (!enabled) {
            logDebug("Extension $name is disabled, skipping execution")
            return
        }
        
        log("Running extension $name")
        logDebug("Extension $name is enabled")
        
        try {
            onRun()
        } catch (e: Exception) {
            log("Error running extension $name: ${e.message}")
            if (debug) {
                e.printStackTrace()
            }
        }
    }
    
    private fun stop() {
        log("Stopping extension $name")
        logDebug("Debug mode enabled for extension $name")
        
        try {
            onStop()
        } catch (e: Exception) {
            log("Error stopping extension $name: ${e.message}")
            if (debug) {
                e.printStackTrace()
            }
        }
        
        enabled = false
    }
    
    // Abstract methods for subclasses to implement
    protected fun onLoad() {}
    protected fun onRun() {}
    protected fun onStop() {}
    protected open fun onUnload() {}
}
fun Extension.log(message: String, date: String? = null, level: String = "INFO") {
    var date = date
    if(date == null) {
        date = Date().toString()
    }
    if (debug) {
        println("[$name] [$date] [$level] DEBUG: $message")
    } else {
        println("[$name] [$date] [$level] $message")
    }
}
fun Extension.onLoad() {
    println("Extension $name loaded")
}
fun Extension.onRun() {
    println("Extension $name running")
}
fun Extension.onStop() {
    println("Extension $name stopped")
}
fun Extension.onUnload() {
    println("Extension $name unloaded")
}

fun Extension.safeExit(code: Int) {
    if (debug) {
        println("[$name] Would exit with code $code (debug mode)")
    } else {
        exitProcess(code)
    }
}
fun Extension.logDebug(message: String) {
    log(message, level = "DEBUG")
}
fun main() {
    fun Extension.bigLog(message: String) {
        val upperDate = Date().toString().uppercase()
        log(message.uppercase(), level = "BIG", date = upperDate)
    }
    val extension = Extension()
    val name = "Extension"
    extension.name = name
    fun demo() {
        extension.load()
        extension.unload()
        extension.reload()
        extension.enable()
        extension.isEnabled()
        println(extension.manifest.toString())
        extension.log("Extension loaded")
        extension.logDebug("Debug mode enabled for extension $name")
        extension.bigLog("Extension loaded")
    }
    val overrideExtension: (Extension) -> Unit = { extension ->
        extension.name = "OverrideExtension"
        extension.version = "1.1"
        extension.load()
        extension.unload()
        extension.reload()
        extension.enable()
        extension.isEnabled()
        val (manifestName, manifestVersion, manifestAuthor, manifestDescription, manifestDependencies, manifestEnabled, manifestManifestVersion, manifestPermissions, manifestPath) = extension.manifest
        println("Manifest name: $manifestName")
        println("Manifest version: $manifestVersion")
        println("Manifest author: $manifestAuthor")
        println("Manifest description: $manifestDescription")
        println("Manifest dependencies: $manifestDependencies")
        println("Manifest enabled: $manifestEnabled")
        println("Manifest manifest version: $manifestManifestVersion")
        println("Manifest permissions: $manifestPermissions")
        println("Manifest path: $manifestPath")
        extension.log("Extension loaded")
        extension.logDebug("Debug mode enabled for extension $name")
        extension.bigLog("Extension loaded")
    }   
    println(overrideExtension.hashCode())
    overrideExtension(extension)
    demo()
    fun quit() {
        extension.disable()
        extension.safeExit(0)
    }
    quit()
}