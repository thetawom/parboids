import sys
import numpy as np

def create_boids(n, bounds=10, vbounds=5):

    c2 = (-5, 5)
    c1 = (5, -5)
    scale = 1

    x1 = np.random.normal(c1[0], scale, size=int(n // 4))
    y1 = np.random.normal(c1[1], scale, size=int(n // 4))
    x2 = np.random.normal(c2[0], scale, size=int(n // 4))
    y2 = np.random.normal(c2[1], scale, size=int(n // 4))
    x3 = np.random.uniform(-bounds, bounds, n - len(x1) - len(x2))
    y3 = np.random.uniform(-bounds, bounds, n - len(y1) - len(y2))
    x = np.concatenate((x1, x2, x3))
    y = np.concatenate((y1, y2, y3))

    u = np.random.uniform(-vbounds, vbounds, len(x))
    v = np.random.uniform(-vbounds, vbounds, len(y))
    sizes = np.random.uniform(0.8, 1.2, len(x))
    
    np.savetxt(f"flock.txt", np.column_stack((x, y, u, v, sizes)), delimiter=" ", fmt="%1.3f")

if __name__ == "__main__":
    num_boids = int(sys.argv[1])
    create_boids(num_boids)