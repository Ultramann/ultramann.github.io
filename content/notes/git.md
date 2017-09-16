---
title: Git
published: September 2, 2017
---

Notes on the version control system `git`.

<!--more-->

Many of these are largely unnecessary since git gives you so much feedback at the command line anyways.

# Git Notes

## Changing the Index

The index is a file where git keeps track of ephemeral changes to the repo. It is from the index that commits are made. Ways you can alter the index:

* `git add <file(s)/directory>`: take any modifications of the added files and stage them in the index.
* `git reset <file(s)/directory>`: take the staged modifications for the files and remove them from the index.
* `git rm <file(s)/directory>`: same as shell `rm` except the deletion is added to the index immediately.

## Changing Files

* `git checkout -- <file(s)/directory>`: reverts all changes made to the files. Technically they revert to the `HEAD` state.

## Merging

* Squashing:

>>```
>>git checkout master
>>git merge --squash development
>>git commit
>>```

## Links

* [How to Undo Almost Anything with Git](https://github.com/blog/2019-how-to-undo-almost-anything-with-git)
