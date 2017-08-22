from time import time
import kmeans
import matplotlib.pyplot as plt
import numpy as np


SAVE_PATH = '../images/numeric_python_intro/{}.png'


def make_blobs(n_points):
    """Convenience function to create data in two clusters, one around
    (0.5, 0,5) and the other (-0.5, -0.5).

    Parameters
    ----------
    n_points : int, total number of data points in both clusters

    Returns
    -------
    ndarray, 2D
    """
    points = np.array([[0.5, 0.5], [-0.5, -0.5]])
    noise = np.random.normal(0, .25, size=(int(n_points/2), 2))
    noised_points = points[:, None] + noise
    return noised_points.reshape(-1, 2)


def remove_ticks(ax):
    """Helper function to remove the ticks from both axes.

    Parameters
    ----------
    ax : matplotlib Axis object
    """
    ax.set_xticklabels([])
    ax.set_yticklabels([])
    ax.set_xticks([])
    ax.set_yticks([])


def plot_setup(centroids, assignments, ax, title=None, legend=False):
    """Convenience function to plot clusters and centroids.

    Parameters
    ----------
    centroids : list-like 2D
    assignments : list of list-like
    ax : matplotlib axis object
    title : str, to put on ax
    legend : bool, plot legend
    """
    for centroid, assigns, color in zip(centroids, assignments, 'br'):
        size_alpha = 1 / (np.log2(len(assignments)) + 2)
        ax.scatter(*zip(*assigns), c=color, alpha=size_alpha, s=30, label='blob')
        ax.scatter(*centroid, c='k', marker='*', alpha=0.7, s=300, label='center')
    remove_ticks(ax)
    if title:
        ax.set_title(title, fontsize=20)
    if legend:
        leg = plt.legend(loc=2, fontsize=15)
        leg.get_frame().set_alpha(0)


def plot_single(X, km, title, file_name, legend=False):
    """Cluster and plot results for single algorithm.

    Parameters
    ----------
    X : list-like 2D
    km : function, kmeans clustering function
    title : str, for plot
    file_name : str, name to file to save to, not includeing rest of path
                     or file extension
    legend : bool, plot legend
    """
    fig, ax = plt.subplots(figsize=(10, 10))
    centroids, assignments = km(X, k=2)
    plot_setup(centroids, assignments, ax, legend=legend)
    fig.suptitle(title, fontsize=25)
    remove_ticks(ax)
    plt.savefig(SAVE_PATH.format(file_name), transparent=True)


def plot_cluster_comp(X, file_name):
    """Plots the results of both clustering implementations side-by-side
    with the timing for the implementations above the clustering results.
    
    Parameters
    ----------
    X : list-like 2D
    file_name : str, name to file to save to, not includeing rest of path
                     or file extension
    """
    fig, ax_lst = plt.subplots(1, 2, figsize=(20, 10))
    params = zip(ax_lst, (X.tolist(), X), ('Base Python', 'NumPy'),
                                (kmeans.base_python, kmeans.numpy))
    for ax, data, algo, km in params:
        start_time = time()
        centroids, assignments = km(data, k=2)
        total_time = time() - start_time
        timed_title = '{}: {:.2f} seconds'.format(algo, total_time)
        plot_setup(centroids, assignments, ax, timed_title)
        remove_ticks(ax)
    fig.suptitle('Timing - {} Data Points'.format(X.shape[0]), fontsize=25)
    plt.savefig(SAVE_PATH.format(file_name), transparent=True)


def plot_timing_comp(file_name, data_sizes=(10, 100, 1000, 2500, 5000)):
    """Plots the time taken to cluster on given data over some data sizes.
    
    Parameters
    ----------
    file_name : str, name to file to save to, not includeing rest of path
                     or file extension
    data_sizes : tuple, ints
    """
    fig, ax = plt.subplots(figsize=(10, 10))
    times = ([], [])
    kms = (kmeans.base_python, kmeans.numpy)
    for num_points in data_sizes:
        X = make_blobs(num_points)
        for data, km, km_time in zip((X.tolist(), X), kms, times):
            start_time = time()
            km(data, k=2)
            km_time.append(time() - start_time)

    ax.plot(data_sizes, times[0], c='r', label='Base')
    ax.plot(data_sizes, times[1], c='b', label='NumPy')
    ax.set_xlabel('Number of Points to Cluster', fontsize=20)
    ax.set_ylabel('Time to Cluster - Seconds', fontsize=20)
    leg = ax.legend(loc='best', fontsize=15)
    leg.get_frame().set_alpha(0)
    fig.suptitle('Time Scaling for 1000 Iterations', fontsize=25)
    plt.savefig(SAVE_PATH.format(file_name), transparent=True)


if __name__ == '__main__':
    X100 = make_blobs(100)
    X1000 = make_blobs(1000)
    X10000 = make_blobs(10000)
    plot_single(X100, kmeans.numpy, 'Example k-means',
                'example_clustering', legend=True)
    plot_cluster_comp(X100, 'comparison_100')
    plot_cluster_comp(X1000, 'comparison_1000')
    plot_cluster_comp(X10000, 'comparison_10000')
    plot_timing_comp('kmeans_time_comp')
