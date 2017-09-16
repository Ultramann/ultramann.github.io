---
title: Visualizing scikit-learn Decision Trees
published: August 12, 2017
---

Decision trees aren't an insanely difficult concept to grasp. But this doesn't mean a good visualization of a trained decision tree can't help develop a deeper understanding of the model.

In this post we'll take a look at small piece of code I wrote to draw the split lines that result from a scikit-learn decision tree being built and step through the process I went through to write it. All the code used for the visualizations in this post can be found [here](https://github.com/Ultramann/ultramann.github.io/blob/source/content/code/visualizing_sklearn_decision_trees/decision_tree_viz.py).

<!--more-->

# Design Decisions

When I was first thinking about writing code to visualize decision tree splits I considered writing my own decision tree class. This *may* have been easier than tearing into scikit-learn's decision trees which end up being implemented in Cython.

However, after thinking about what I'd like from the code at a larger scale I realized it would be better to write something for scikit-learn's decision trees as they are the basis of all scikit-learn's tree ensemble models. Aka, I would be able to visualize the trees in a random forest or gradient boosted tree ensemble easily if I had something that worked on a single decision tree. Thanks, scikit-learn, for writing well composed code!

# Starting Point

One thing that's actually pretty easy to do with classification models is plot their decision regions; note, I'll only be working with classifiers because of this quality. Let's take a look at what you get with some sample data, and roughly the strategy used in a scikit-learn tutorial found [here](http://scikit-learn.org/stable/auto_examples/tree/plot_iris.html). The function to generate the plot below, `plot_decision_regions`, for plotting the decision regions can be found in the source linked above. Note that we really can't do a great job plotting decision boundaries in greater than two dimensions. So this post will only be looking at data in two dimensions.

<div class="mpl"><img class="single" src="/images/decision_tree_viz/dt_decision_regions.png"></div>

`plot_decision_regions` works by creating a grid of points (using [`np.meshgrid`](https://docs.scipy.org/doc/numpy/reference/generated/numpy.meshgrid.html)), lots of points, and running each of those points through the fitted decision tree to get a prediction. Then all of those predictions are plotted (using [`plt.contourf`](https://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.contourf)) creating the illusion that a complete decision regions are known.

From the decision regions above you could pretty easily infer the splits that were made in the decision tree. I mean, there were only three splits. But consider what happens when the data and therefore the decision tree gets bigger.

<div class="mpl"><img class="single" src="/images/decision_tree_viz/big_dt_decision_regions.png"></div>

To be quite honest, it'd be fair to say that having the explicit split lines plotted on the image above wouldn't really make it "easier" to understand what splits were made, much less why. But, I definitely don't think they hurt. Either way, you'll be able to decide for yourself soon.

# Decision Trees in scikit-learn

You can find the documentation for scikit-learn's `DecisionTreeClassifier` [here](http://scikit-learn.org/stable/modules/generated/sklearn.tree.DecisionTreeClassifier.html). The documentation describes the parameters that can be set before training the tree. Nothing about splits in sight...

Looking further, we find reference to:

```
tree_: Tree object
      The underlying Tree object.
```

It seems that if information about splits is going to be anywhere it'll be here. Unfortunately this is about where our friend, the documentation, stops being useful. Off to the source!

## The Source Code

Looking at the [`.fit`](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L698) method we see a good doc string, and then, lo and behold!

```python
super(DecisionTreeClassifier, self).fit(...)
```

Nerts!

This makes a lot of sense since basically the same logic can be used to train a [regression tree](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L819), so abstracting out as much similar behavior as possible is certainly a good idea.

Alright, [looks like](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L508) we need to go look at the `BaseDecisionTree`. Its [`.fit`](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L698) method looks waaay more promising. Perusing through this method is like adventuring through a jungle of logic checking the validity of the different parameters that can be passed to govern training. And [then](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L334)!

```python
self.tree_ = Tree(self.n_features_, self.n_classes_, self.n_outputs_)
```

Less nerts!

Where do we find `Tree`? Well, if this code is worth its salt, Python conventions-wise, I can immediately say, it's a class, and it's either defined in this `tree.py` script or it's getting imported at the top. Taking advantage of this knowledge involves a quick global definition search for "Tree". And there on [line 43](https://github.com/scikit-learn/scikit-learn/blob/ab93d65/sklearn/tree/tree.py#L43).

```python
from ._tree import Tree
```

If you aren't familiar with relative imports, this is just saying that `Tree` is getting imported from the `_tree` script in this directory.

### Source Code Exploration Pointers

Side note, if you've been trying to follow along with the winding path through the source code up until this point there's a chance you've been staying in the browser scrolling up and down in github. This might work on a small scale. But quickly it will become a pain to keep track of where you are and where you're moving to within a larger script. This is only exacerbated if you need to loop at code in multiple scrips.

To this end, I generally suggest that source code exploration be done in your text editor of choice.

There's a decent chance that you'll have retrieved scikit-learn via `pip` or `conda`, in which case you probably don't know the **actual** location of the source files. If this is the case I have a gem for you!

You can find the path to a library is being imported from as it's defined as a dunder attribute on the reference when imported. Aka, if you import a library, say, `sklearn` you can find out where it's file is by accessing it's `__file__` attribute.

```python
>>> import sklearn
>>> sklearn.__file__
'/Users/Ultramann/anaconda3/lib/python3.5/site-packages/sklearn/__init__.py'
```

For the record, the advice I espoused here will not be useful in the next section the `_tree` script is actually written in Cython, and on your system it's going to be compiled. So we're basically relegated to looking through the source in github. :-/

### Cython Time

As mentioned above, when I went to look for the `_tree` source in my scikit-learn directory, I could not find any source code. So it was off to github.

Probably the most valuable thing one could do at this point is read the doc string for the [`Tree`](https://github.com/scikit-learn/scikit-learn/blob/ab93d657eb4268ac20c4db01c48065b5a1bfe80d/sklearn/tree/_tree.pyx#L490) class found in Cython source file [`_tree.pyx`](https://github.com/scikit-learn/scikit-learn/blob/ab93d657eb4268ac20c4db01c48065b5a1bfe80d/sklearn/tree/_tree.pyx). This is **not** what I did the first time through and found myself confusedly reading a bunch of well thought out Cython; read: lots of separation of concerns; read: lots of misdirection. I eventually found what I was looking for in some stackoverflow answer and when I went back to look at the source I found the same, but more thorough, answer in the doc string.

Now lots of the previously confusing code made way more sense and I realized that I was confused because I was expecting some recursive process to build the tree as I was taught decision trees are built recursively. But now I saw that the recursive building process was being emulated by a stack of regions to work on. And so the "recursion" was being governed by a while loop, instead of explicit recursion, that constantly pops off the next region to decide a split in. When it does split it records the split information and pushes the two sub-regions the split created onto the stack. This makes sense as the code is written in Cython and not a functional language. Understanding this also made the comment in the source, "Recursive partition (without actual recursion)", make a lot more sense. It's good to know these details because understanding them will make the process of finding split lines way easier.

The key attributes of the `Tree` class that are going to be necessary to understand are: `children_left`, `children_right`, `feature` and `threshold`. All of these are arrays with length number of nodes in the tree, including leaves. They are all indexed by the concept of a node id. Given a node id you can use it to index into any of these arrays and get back the node id of its right child, the node id of its left child, the feature (by index) that was used to split on in that node, and the threshold for that split, respectively.

Armed with this knowledge we can return the more familiar world of Python to figure out a way to draw the split lines.

# Drawing the Split Lines

One of the first things that I was thinking prior to all this digging into scikit-learn source was that I'd be writing the code to tear apart the tree recursively. In line with the discussion of the tree building process above, this doesn't directly match with either the most pythonic approach to the problem, nor iterative process used to build the tree. On this note I'm punting since recursion, while not particularly pythonic, is cool and definitely faster for my brain and fingers to code up. The only potential downfall to choosing the recursion path is that Python enforces a default recursion depth limit of somewhere around 1000. So I could potentially run into that if I tried to recursively break down a scikit-learn tree that is *HUGE*. Good to know the limitations upfront and I'm willing to accept those potential future consequences. Moving on.

For a reference to aim for, this is what we're trying to create.

<div class="mpl"><img class="single" src="/images/decision_tree_viz/dt_decision_regions_splits.png"></div>

## Strategy

Since I've already decided that I'm going to use a recursive approach to traverse down the tree I'm positioned quite well to take a [top-down](https://en.wikipedia.org/wiki/Top-down_and_bottom-up_design) strategy to implementing this function. Top-down programming is going to be really effective here because I already know what the structure of a recursive function looks like. And even though I don't know the parameters that this function should be taking yet, nor what it should be returning, if anything, the top-down process will help me discover these things naturally.

Most of my top-down programming looks a lot like wishful thinking. I'll call to functions that don't exist, having an idea about what I want them to do, but not necessarily knowing how to write them yet, and layout the structure of my solution at a high level. This helps me by forcing me to think about what intermediate information/calculations I need to have/perform to solve the larger problem and putting a name to those ideas. It also has the advantage that I should be able to convince myself that if I implemented all of the functions I wish for I should have a working solution.

Now, the functions I wish for might end up being super simple and get replaced with a single line of code, but the might also be quite involved in their own right. In this case I'll use the same top-down tactic to split up that particular part of the problem into yet smaller pieces. It's rather apropos that this recursive problem solving technique will be very effective in writing this recursive code; but know that it is widely applicable and is almost always how I start solving a problem when there isn't a clear launching off point.

## Recursively Cutting Down the Tree

At this point I know that I'm going want to a function that has a basic structure like this.

```python
def recursive_thing(stuff, i, dont, know, yet):
    feature, threshold = get_node_split_info(ummm, things)
    left, right = get_child_nodes(more, things)

    record_split_info(feature, threshold)

    recursive_thing(left, child, stuff)
    recursive_thing(right, child, stuff)
```

Wonderful! The code is basically writing itself! Jk.

To figure out how to move from the lovely pseudocode above to a function that actually, you know, works I need to ask myself a couple of questions.

1. How to I get information about the split?
2. How to record the information about the splits as I recurse?
3. How do I use the `feature` and `threshold` information to recurse correctly?
4. What is the base case that stops the recursion?
5. How does the recursion get started?

### Getting Split Information

As mentioned [above](#cython-time) all of the information we need about how a split was made in a node is in the arrays `children_left`, `children_right`, `feature` and `threshold`. Each of these arrays being indexed by the node id. This tells me the function needs to get passed two things: the node id and all of these arrays. Getting the split information is now as simple as indexing a few times. And the code evolves.

```python
def recursive_thing(node_id, stuff, i, dont, know, yet,
                    features, thresholds, children_left, children_right):
    feature, threshold = features[node_id], thresholds[node_id]
    left_child, right_child = children_left[node_id], children_right[node_id]

    record_split_info(feature, threshold)

    recursive_thing(left_child, stuff,
                    features, thresholds, children_left, children_right)
    recursive_thing(right_child, stuff,
                    features, thresholds, children_left, children_right)
```

### Recording the Split Information

I actually punted on answering the first question for awhile and opted to pass a `matplotlib Axis` object through the recursive calls and directly plot the split lines on it each time one was found.

It wasn't until I showed one of my colleagues the code and he mentioned that it wouldn't be easy to test that I reverted, agreeing that I should separate the split finding from split drawing, or separating computation from updating state. I realized that the simple answer to the first question was to pass along a list that would append to storing all the information about the splits as the function recursed down the tree. This might not seem particularly...elegant, but it works well given the following considerations.

First, Python is a call-by-object-reference, not sure if that's an official term, language. Practically this means that it's cheap to pass anything to a function as it's just a reference to the object that gets passed. This can be an issue if you start to wander into concurrency-ville, but I'm not concerned with that here, so we good.

Second, there's a world in which we'd care about the order that we traverse the tree and therefore the order that split information in returned from this function. The nature of this naive recursion approach is that you only get depth first traversal out, if you wanted a breath first traversal you'd have to separate getting the split information for a level in your tree from recursing down a level. Bleh, good thing I don't care. Though I might in the future if I wanted to be able to control how deep I go drawing splits, or something even cooler like drawing the splits with thinner lines the further down the tree the split was made. Project for future me.

Now my function takes a slightly more specific form.

```python
def recursive_thing(split_info_list, node_id, stuff, i, dont, know, yet,
                    features, thresholds, children_left, children_right):
    feature, threshold = features[node_id], thresholds[node_id]
    left_child, right_child = children_left[node_id], children_right[node_id]

    split_info_list.append((feature, threshold))

    recursive_thing(split_info_list, left_child, stuff,
                    features, thresholds, children_left, children_right)
    recursive_thing(split_info_list, right_child, stuff,
                    features, thresholds, children_left, children_right)
```

### Using the Split Information

The main reason that I chose to write this function recursively was to get around the issue of figuring out the bounds that a split line should be drawn in as they successively restrict as the split is made further down the tree. Given this I see that "using the split information" means figuring out the bounds for left and right children knowing what feature a split was made on and the threshold that it occurred at. It also means passing that information at each call to the function and saving it along with the feature and threshold in `split_info_list`. At this point, to cut down on characters a bit, I renamed `split_info_list` to `ftbs` for feature-threshold-bounds. I'm also going to give the function a better name, how about `get_split_lines`?

It turns out that the `features` array actually holds three unique values even when we only work with data with two features. Those unique values are: `-1, 0, 1`. While this is a little weird, I know that indexing with `-1` will get the last element in a list/array. So `0` and `-1` are actually synonymous. This will barely affect the logic I write to figure out the bounds for the children.

I decided that all of the boundary information should be contained in a nested tuple that looks like: `((x_min, x_max), (y_min, y_max))`, all floats. This way it's easy to pass all of the boundary information in a single variable. Getting to the component bounds can be done with, either, successive indexing or tuple unpacking. I end up using both.

Now, it's easy to see that what `get_split_lines` should do is get the split information, `feature` and `threshold`, and, given the bounds for the node it's concerned with, determine the bounds for it's children. This calculation will be slightly different depending on whether the first or second feature was used to split. But other than this consideration, the process is pretty straightforward.

We're getting really close.

```python
def get_split_lines(ftbs, node_id, bounds,
                    features, thresholds, children_left, children_right):
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

    get_split_lines(ftbs, left_child, left_bounds,
                    features, thresholds, children_left, children_right)
    get_split_lines(ftbs, right_child, right_bounds,
                    features, thresholds, children_left, children_right)
```

### Stopping the Recursion

A base case is always necessary when writing recursive code. If you ever find yourself writing something recursively make sure that you're thinking about what the base case is that stops the recursive calls. This conditional will almost always be at the top of the function.

In this function the base case occurs when we encounter a leaf node. How do we know we're in a leaf node considering that no such information with recorded in the scikit-learn tree? After a little experimentation I realized that this information is stored implicitly in the `children_left` and `children_right` arrays. The leaf nodes are ones that reference themselves as their own "child". In code, `if {left,right}_child == node_id`.

```python
def get_split_lines(ftbs, node_id, bounds, leaf,
                    features, thresholds, children_left, children_right):
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

    get_split_lines(ftbs, left_child, left_bounds, left_child == node_id,
                    features, thresholds, children_left, children_right)
    get_split_lines(ftbs, right_child, right_bounds, right_child == node_id,
                    features, thresholds, children_left, children_right)
```

### Starting the Recursion and Plotting

The last thing we need so that we can see our splits is to figure out how to start our recursion and then take the `ftbs` list and iterate over it to draw the split lines on our plot. So what we really want is the following function that accepts a scikit-learn `Tree` object and an `Axis` object to plot on.

```python
def plot_split_lines(tree, ax):
    feats_threshs_bounds = []
    get_split_lines(feats_threshs_bounds, 0,
                    (ax.get_xlim(), ax.get_ylim()),
                    False, tree.feature, tree.threshold,
                    tree.children_left, tree.children_right)
    for feature, threshold, (x_bounds, y_bounds) in feats_threshs_bounds:
        if feature == 1:
            ax.plot(x_bounds, (threshold, threshold), 'k-', lw=0.5)
        else:
            ax.plot((threshold, threshold), y_bounds, 'k-', lw=0.5)
```

This function is super simple, it makes an empty list that will hold all the features, thresholds and bounds. Then it makes the first call to `get_split_lines` passing it the empty list it just created, the node id for the root of the tree, `0`, the boundaries of the `Axis` it was passed, False for whether or not it's at a leaf, and all of the arrays holding the split information from the `Tree`. Then is simply iterates over the list that gets mutated in place during all the recursive calls, and depending on what feature was selected it plots a line on the appropriate axis. Let's see what it produces.

<div class="mpl"><img class="single" src="/images/decision_tree_viz/dt_decision_regions_splits_close.png"></div>

This is pretty close! But, what's with the line over on the left side??

It took me a couple of minutes to figure this out. But eventually I realized that scikit-learn must be "choosing" a split sometime to catch all of the points. No matter. I just added a conditional to make sure that the function only recurses if the split is within the boundaries of the feature that was chosen. And so, the final version of the function!

```python
def get_split_lines(ftbs, node_id, bounds, leaf,
                    features, thresholds, children_left, children_right):
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
```

And here's what it creates on the same large tree from above. Nice.

<div class="mpl"><img class="single" src="/images/decision_tree_viz/big_dt_decision_regions_splits.png"></div>

# Conclusion

Well there you have it. Some code that plots the split lines for an scikit-learn decision tree. Along the way we took a look at some scikit-learn source code, got to play around with recursion, and practice our top-down problem solving. I don't know what more I could ask for.
