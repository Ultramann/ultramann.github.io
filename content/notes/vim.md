---
title: Vim
published: August 20, 2017
---

Notes on the amazing `Vim` text editor.

<!--more-->

# Vim Notes

## Plugins

Tim Pope's [pathogen](https://github.com/tpope/vim-pathogen) plugin manager. To install a new plugin in my vim directory of my dotfiles I run:

```
git submodule add <plugin-github-url> bundle/<repo_name>
git submodule init && git submodule update
```

I first found these instructions on [this](http://mirnazim.org/writings/vim-plugins-i-use/) and have since tweaked the methodology to keep the submodules tracked the level of the dotfiles repository.

## Misc Buffer Modes

There are a special set of buffers, probably more than I'm listing, that allow you to browse and edit some of your history in a buffer. The special histories you can view and their commands to get to them from normal mode are:

* search history: `q/` and `q?` for search forward and backward, respectively.
* command history: `q:`. Note you might, as I have, found myself in this buffer unintentionally since it's command is so close to that for quitting.
