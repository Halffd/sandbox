import fs from 'fs/promises';

async function runWasm() {
  try {
    // Load the WASM file
    const wasmBuffer = await fs.readFile('./tree_traversal.wasm');
    
    // Increase memory size
    const memory = new WebAssembly.Memory({ initial: 10 }); // 10 pages = 640KB
    
    let outputBuffer = "";
    
    // Define imports for the WASM module
    const importObject = {
      wasi_snapshot_preview1: {
        proc_exit: (code) => console.log(`Process exited with code ${code}`),
        fd_write: (fd, iovs, iovs_len, nwritten) => {
          try {
            const view = new DataView(memory.buffer);
            let bytesWritten = 0;
            
            for (let i = 0; i < iovs_len; i++) {
              const iov = iovs + i * 8;
              const ptr = view.getUint32(iov, true);
              const len = view.getUint32(iov + 4, true);
              
              if (ptr + len > memory.buffer.byteLength) {
                console.error(`Memory access error: Trying to access ${ptr + len} bytes but buffer is ${memory.buffer.byteLength} bytes`);
                continue;
              }
              
              const buf = new Uint8Array(memory.buffer, ptr, len);
              const str = new TextDecoder().decode(buf);
              outputBuffer += str;
              bytesWritten += len;
            }
            
            view.setUint32(nwritten, bytesWritten, true);
            return 0;
          } catch (error) {
            console.error("Error in fd_write:", error);
            return 1;
          }
        },
        fd_close: () => 0,
        fd_seek: () => 0
      },
      env: {
        memory,
        // Add any other required imports
        emscripten_notify_memory_growth: () => {},
        emscripten_memcpy_big: (dest, src, num) => {
          const heap = new Uint8Array(memory.buffer);
          heap.copyWithin(dest, src, src + num);
        }
      }
    };
    
    // Instantiate the WebAssembly module
    const result = await WebAssembly.instantiate(wasmBuffer, importObject);
    
    console.log("WebAssembly module loaded!");
    console.log("Available exports:", Object.keys(result.instance.exports));
    
    // Instead of calling our traversal function directly, let's call _start
    // which is the main entry point that should initialize everything
    console.log("Calling _start to run the program...");
    
    try {
      // Call the _start function which should be the entry point
      result.instance.exports._start();
      
      // Print any output we captured
      console.log("\nProgram output:");
      console.log(outputBuffer);
    } catch (error) {
      console.error("Error running WASM module:", error);
    }
  } catch (error) {
    console.error("Error:", error);
  }
}

// Run the main function
runWasm();