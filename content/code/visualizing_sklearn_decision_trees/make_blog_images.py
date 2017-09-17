import matplotlib
matplotlib.use('Agg')

from sklearn.tree import DecisionTreeClassifier
import decision_tree_viz as dtv
import matplotlib.pyplot as plt


SAVE_PATH = '../images/decision_tree_viz/{}.png'
SAVE_SETTINGS = dict(bbox_inches='tight', pad_inches=0, transparent=True)
SUPPLOT_DIST = 0.9


def plot_decision_tree(X, y, title, file_name, plot_splits, md=2, close=False):
    """Utility function to create necessary plots for blog post.

    Parameters
    ----------
    X : ndarray, 2D data points
    y : ndarray, 1D labels
    title : str, title of plot
    file_name : str, name of file to save to, not including rest of path
                     or file extension
    plot_splits : bool, plot splits or not
    md : int : max depth to train the tree to
    close : bool, plot the split lines with the close algorithm?
    """
    fig, ax = plt.subplots(figsize=(10, 10))
    dtv.plot_2d_2class(X, y, ax)
    dt = DecisionTreeClassifier(max_depth=md, random_state=42).fit(X, y)
    dtv.plot_decision_regions(dt, ax)
    if plot_splits:
        dtv.plot_split_lines(dt.tree_, ax, close)
    dtv.remove_ticks(ax)

    fig.suptitle(title, fontsize=25)
    plt.subplots_adjust(top=SUPPLOT_DIST)
    plt.savefig(SAVE_PATH.format(file_name), **SAVE_SETTINGS)


if __name__ == '__main__':
    X50, y50 = dtv.make_data(50)
    X500, y500 = dtv.make_data(500)
    plot_decision_tree(X50, y50, 'Decision Tree Regions',
                       'dt_decision_regions', False)
    plot_decision_tree(X500, y500, 'Complicated Decision Tree Regions',
                       'big_dt_decision_regions', False, 50)
    plot_decision_tree(X50, y50, 'Decision Tree Regions with Split Lines',
                       'dt_decision_regions_splits', True)
    plot_decision_tree(X50, y50, 'Almost There',
                       'dt_decision_regions_splits_close', True, 50, close=True)
    plot_decision_tree(X500, y500, 'Complicated Tree Split Lines',
                       'big_dt_decision_regions_splits', True, 50)
