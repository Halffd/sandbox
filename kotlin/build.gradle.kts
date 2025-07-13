plugins {
    kotlin("jvm") version "1.9.10"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
}

application {
    mainClass.set("ThreadsKt")
}

kotlin {
    jvmToolchain(17) // or whatever Java version you're using
}