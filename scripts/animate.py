import csv
import matplotlib.pyplot as plt
from matplotlib import animation
import numpy as np
import os


def plot(filename):
    x, y, u, v, sizes = [], [], [], [], []
    with open(filename) as file:
        reader = csv.reader(file)
        for row in reader:
            px, py, vx, vy, m = row[0].split(" ")
            x.append(float(px))
            y.append(float(py))
            u.append(float(vx))
            v.append(float(vy))
            sizes.append(float(m))
            
    _, ax = plt.subplots()
    ax.quiver(x, y, u, v, linewidths=sizes)
    plt.show()


def get_vals(filename):
    x, y, u, v, sizes = [], [], [], [], []
    with open(filename) as file:
        reader = csv.reader(file)
        i = 0
        for row in reader:
            i += 1
            px, py, vx, vy, m = row[0].split(" ")
            x.append(float(px))
            y.append(float(py))
            u.append(float(vx))
            v.append(float(vy))
            sizes.append(float(m))
    return x, y, u, v, sizes


def create_animation():
    txt_files = {}

    filenames = os.listdir("output")
    for filename in filenames:
        name, ext = filename.split(".")
        if ext == "txt":
            txt_files[int(name)] = filename
            
    time_steps = len(txt_files)

    positions = []
    velocities = []
    sizes = []

    for i in range(time_steps):
        file = os.path.join("output", txt_files[i])
        x, y, u, v, m = get_vals(file)
        positions.append(np.array([x, y]))
        velocities.append(np.array([u, v]))
        sizes.append(m)
        
    fig = plt.figure()
    ax = fig.add_subplot(111, aspect='equal', autoscale_on=False, xlim=(0, 1), ylim=(0, 1))
    s = ax.scatter(positions[0][0], positions[0][1], s=0.5)

    def animate(i):
        bound = 20
        ax.clear()
        ax.set_xlim(-bound, bound)
        ax.set_ylim(-bound, bound)
        ax.text(0.01, 0.95, f"Time Step: {i}", transform=ax.transAxes)
        s = ax.scatter(positions[i][0], positions[i][1], s=0.5)

    anim = animation.FuncAnimation(fig, animate, interval=10, frames=range(time_steps))

    anim.save("animation.gif", writer="pillow")

if __name__ == "__main__":
    create_animation()