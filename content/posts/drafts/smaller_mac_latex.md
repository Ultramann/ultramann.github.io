---
title: Smaller LaTeX for Mac
published: September 19, 2017
---

Instructions for having a version of latex with a smaller memory footprint.

<!--more-->

# Smaller LaTeX for Mac

## Table of Contents

1. [Purpose](#purpose)
2. [Setup Instructions](#setup-instructions)
    1. [Uninstalling Old MacTex](#uninstalling-old-latex-version)
    2. [Installing BasicTex](#installing-basictex)
3. [Using LaTeX](#using-latex)

## Purpose

Using LaTeX on a Mac is somewhat simple, you download [MacTeX](http://www.tug.org/mactex/). Sometimes ths download doesn't work, they give you the checksum to verify though. But once you've downloaded the installer running it to  MacTeX is simple...and takes ~5 GB of disk space.

This issues is addressed by installing BasicTex instead of the full MacTex packages. BasicTex has a **much** smaller memory footprint. The downside of BasicTex is that it doesn't ship with all of the necessary packages for developing great slides. The `setup.sh` script solves this problem by installing the requisite packages. (Note: the list of packages that it currently installs will likely need to be expanded, if you find a package that you think is worth installing feel free to add the package to the list in the setup script and submit a pull request to update.) 

## Setup Instructions

### Uninstalling Old LaTeX Version

If you happen to already have MacTex installed and want to free up some disk space simply delete the following directories:

* `/usr/local/texlive`
* `/Library/TeX`

### Installing BasicTex

1. Download BasicTex by clicking [here](http://tug.org/cgi-bin/mactex-download/BasicTeX.pkg).
2. Go through the install procedure by opening the BasicTex `.pkg` that you just downloaded. All of the defaults are fine.
3. Run the setup script: `bash setup.sh`.

## Using LaTeX

Using LaTeX is just like using any other markup language. If you want to figure out how to typeset something in LaTeX, google is your friend, there is a huge Stack Overflow presence of LaTeX questions getting answered. For slides we use the [beamer](https://www.sharelatex.com/learn/Beamer) package.

As these instructions are for building a bare bones LaTeX environment there is no GUI as you might be used to working with to edit LaTeX. Instead you can simply use your text editor of choice to write a `.tex` document. To compile the file you run `pdflatex <file_name>.tex`.

One thing that you might miss is having a dynamically reloading view of the PDF that is generated, this unfortunately is something that Preview does poorly, it will reload the view of the PDF when you regenerate it, but it always returns you to the top of the document. Fortunately there is an open source PDF reader called Skim that auto-reloads PDFs when they get regenerated and returns you to the current location. You can download it [here](https://sourceforge.net/projects/skim-app/files/Skim/Skim-1.4.29/Skim-1.4.29.dmg/download). To make it easy to open PDFs from the command line with Skim you can add the following lines to bash profile. They will allow you to run `opentex <file_name>.pdf` from the command line to open a PDF with Skim.

>>```
>>openTex() {
>>  open -a Skim $1
>>  }
>>alias opentex=openTex
>>```
