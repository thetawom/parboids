import sys
import numpy as np

def create_boids(n):    
    x = np.random.uniform(-5, 5, n)
    y = np.random.uniform(-5, 5, n)
    u = np.random.uniform(-1, 1, n)
    v = np.random.uniform(-1, 1, n)
    sizes = np.random.uniform(0.8, 1.2, n)
    np.savetxt(f"flock.txt", np.column_stack((x, y, u, v, sizes)), delimiter=" ", fmt="%1.3f")

if __name__ == "__main__":
    num_boids = int(sys.argv[1])
    create_boids(num_boids)