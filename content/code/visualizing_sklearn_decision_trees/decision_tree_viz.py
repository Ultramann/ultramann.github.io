from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.tree import DecisionTreeClassifier
import matplotlib.pyplot as plt
import numpy as np


def make_data(n_points, std=0.4, state=1):
    """Create data in two clusters from Gaussian noise, one around
    (0.5, 0,5) and the other (-0.5, -0.5).

    Parameters
    ----------
    n_points : int, total number of data points per clusters
    std : float, standard deviation of clusters
    state : int, random state to start numpy at

    Returns
    -------
    X : ndarray, 2D data points
    y : ndarray, 1D labels
    """
    points = np.array([[0.5, 0.5], [-0.5, -0.5]])
    rg = np.random.RandomState(state)
    noise = rg.normal(0, std, size=(n_points, 2, 2))
    noised_points = points + noise
    X = np.vstack([noised_points[:, 0, :], noised_points[:, 1, :]])
    y = np.append(np.zeros(n_points), np.ones(n_points))
    return X, y


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


def plot_2d_2class(X, y, ax, incorrect_label=None):
    """Plot the first 2 dimension of 2 class data coloring by class.

    Parameters
    ----------
    X : ndarray, 2D data points
    y : ndarray, 1D labels
    ax : matplotlib Axis object
    incorrect_label : ndarray, 1D bools - point labeled incorrect or not
    """
    for i, color in zip((0, 1), 'br'):
        X_class = X[y == i]
        if incorrect_label is None:
            size = None
        else:
            size = incorrect_label[y == i]
        ax.scatter(X_class[:, 0], X_class[:, 1], alpha=0.7, c=color, s=size)


def plot_decision_regions(classifier, ax, prob_gradient=False, h=.005):
    """Plot the decision boundary for a classifier on axis over it's boundaries.

    Parameters
    ----------
    classifier : model object, must implement predict method
    ax : matplotlib Axis object
    prob_gradient : bool, plot the gradient of the probabilities,
                          default: the prediction at the point
    h : granularity of mesh grid for predictions
    """
    x_min, x_max = ax.get_xlim()
    y_min, y_max = ax.get_ylim()
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h),
                         np.arange(y_min, y_max, h))

    if prob_gradient:
        cmap = plt.cm.bwr_r
        Z = classifier.predict_proba(np.c_[xx.ravel(), yy.ravel()])[:, 0].flatten()
    else:
        cmap = plt.cm.seismic
        Z = classifier.predict(np.c_[xx.ravel(), yy.ravel()])

    Z = Z.reshape(xx.shape)

    ax.contourf(xx, yy, Z, cmap=cmap, alpha=0.3)


def get_split_lines(ftbs, node_id, bounds, leaf,
                    features, thresholds, children_left, children_right):
    """Recursively plot the boundary lines for the nodes in an sklearn
    decision tree.

    Parameters
    ----------
    ftbs : list, tuples of feature, threshold, bounds for one of the splits in
                 the tree
    node_id : int, id of the node's whose decision boundary to plot this step
    bounds : ((float, float), (float, float)), ((x_min, x_max), (y_min, y_max))
                                               bounds of the region that the node
                                               made its decision in
    leaf : bool, has a leaf of the tree been reached, gets set to true for the next
                 recursive call if the child node id is the same as the current id
    features : ndarray, 1D of which feature was split on in each node
                        indexed by node id
    thresholds : ndarray, 1D of the threshold that was split on in each node
                          indexed by node id
    children_left : ndarray, 1D of the node id for the left child nodes
                             indexed by node id
    children_right : ndarray, 1D of the node id for the right child nodes
                              indexed by node id
    """
    if leaf: return

    x_bounds, y_bounds = bounds
    feature, threshold = features[node_id], thresholds[node_id]
    left_child, right_child = children_left[node_id], children_right[node_id]

    feature_bounds = bounds[feature]
    if feature_bounds[0] <= threshold <= feature_bounds[1]:
        ftbs.append((feature, threshold, bounds))
        if feature == 1:
            left_bounds = (x_bounds, (y_bounds[0], threshold))
            right_bounds = (x_bounds, (threshold, y_bounds[1]))
        else:
            left_bounds = ((x_bounds[0], threshold), y_bounds)
            right_bounds = ((threshold, x_bounds[1]), y_bounds)
    else:
        right_bounds = left_bounds = bounds

    get_split_lines(ftbs, left_child, left_bounds, left_child == node_id,
                       features, thresholds, children_left, children_right)
    get_split_lines(ftbs, right_child, right_bounds, right_child == node_id,
                       features, thresholds, children_left, children_right)


def get_split_lines_close(ftbs, node_id, bounds, leaf,
                          features, thresholds, children_left, children_right):
    """Same as get_split_lines but plots lines out of bounds. Included for
    producting plot for blog.
    """
    if leaf: return

    x_bounds, y_bounds = bounds
    feature, threshold = features[node_id], thresholds[node_id]
    left_child, right_child = children_left[node_id], children_right[node_id]

    ftbs.append((feature, threshold, bounds))
    if feature == 1:
        left_bounds = (x_bounds, (y_bounds[0], threshold))
        right_bounds = (x_bounds, (threshold, y_bounds[1]))
    else:
        left_bounds = ((x_bounds[0], threshold), y_bounds)
        right_bounds = ((threshold, x_bounds[1]), y_bounds)

    get_split_lines_close(ftbs, left_child, left_bounds, left_child == node_id,
                          features, thresholds, children_left, children_right)
    get_split_lines_close(ftbs, right_child, right_bounds, right_child == node_id,
                          features, thresholds, children_left, children_right)


def plot_split_lines(tree, ax, close=False):
    """Plots the split boundaries within the regions of a tree object.
    Starts the recursive call to plot_boundary_lines passing node id 0,
    aka, initializing with root node.

    Parameters
    ----------
    tree : sklearn Tree object, attribute of trained DecisionTree object
    ax : matplotlib Axis object
    close : bool, plot the split lines with the close algorithm?
    """
    feats_threshs_bounds = []
    split_line_getter = get_split_lines_close if close else get_split_lines
    split_line_getter(feats_threshs_bounds, 0,
                      (ax.get_xlim(), ax.get_ylim()),
                      False, tree.feature, tree.threshold,
                      tree.children_left, tree.children_right)
    for feature, threshold, (x_bounds, y_bounds) in feats_threshs_bounds:
        if feature == 1:
            ax.plot(x_bounds, (threshold, threshold), 'k-', lw=0.5)
        else:
            ax.plot((threshold, threshold), y_bounds, 'k-', lw=0.5)
