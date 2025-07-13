import torch
import multiprocessing
import os
import time

def cpu_stressor(duration=30):
    """
    Performs intensive tensor operations on CPU until duration is reached
    """
    start_time = time.time()
    
    # Use all available threads for this process
    torch.set_num_threads(os.cpu_count())
    
    try:
        while time.time() - start_time < duration:
            # Create large tensors with computationally intensive operations
            a = torch.randn(1000, 1000, dtype=torch.float32)
            b = torch.randn(1000, 1000, dtype=torch.float32)
            
            # Chain multiple matrix operations
            c = torch.matmul(a, b)
            d = torch.matmul(c, a.T)
            e = torch.matmul(d, b.T)
            
            # Perform element-wise operations
            f = torch.sigmoid(e)
            g = torch.relu(f)
            h = torch.log_softmax(g, dim=1)
            
            # Compute norm and sum to force computation
            _ = torch.norm(h)
            _ = torch.sum(h)
            
    except KeyboardInterrupt:
        pass

if __name__ == "__main__":
    print(f"Starting CPU stress test on {os.cpu_count()} cores...")
    
    # Create a pool of workers equal to the number of cores
    pool = multiprocessing.Pool(processes=os.cpu_count())
    
    try:
        # Run the stressor function on all cores
        pool.map(cpu_stressor, [30]*os.cpu_count())  # Run for 30 seconds
    except KeyboardInterrupt:
        print("\nStopping stress test...")
        pool.terminate()
        pool.join()
    finally:
        print("Stress test completed")