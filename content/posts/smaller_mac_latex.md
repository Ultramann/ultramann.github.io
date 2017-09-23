---
title: Smaller LaTeX for Mac
published: September 23, 2017
---

Instructions for setting up a version of LaTeX on a Mac with a smaller memory footprint than out of the box MacTeX.

I mainly use LaTeX to produce high quality PDFs and PDF-based slide decks, and this set up works great for all my needs.

<!--more-->

# Smaller LaTeX for Mac

## Purpose

Using LaTeX on a Mac is somewhat simple, you download [MacTeX](http://www.tug.org/mactex/). Sometimes this download doesn't work, they give you the checksum to verify though. But once you've downloaded the installer running it to install MacTeX is simple...and takes ~5 GB of disk space.

What is taking up all of this memory? You ask. It's a combination of basically any LaTeX package you could ever want along with a ton of GUIs. Luckily I don't use GUIs and I have a pretty short list of packages I want installed.

With this information it now makes sense to use BasicTeX instead of the entirety of MacTeX. BasicTeX has a **much** smaller memory footprint than MacTeX. The downside of BasicTeX is that it doesn't ship with all of the necessary packages that I use to make great PDFs.

It turns out, though, that BasicTeX does ship with a command line package installer. So I wrote a [script](ihttps://github.com/Ultramann/ultramann.github.io/blob/source/content/code/smaller_mac_latex/setup.sh) to install the packages I require beyond what comes with BasicTeX. This is great because it's likely the list of packages the script currently installs will likely need to be expanded, but since this is controlled programmatically it's easy to update for future installs.

To get set up with LaTeX in this way you can follow the instructions below.

## Setup Instructions

### Uninstalling Old LaTeX Version

If you happen to already have MacTeX installed and want to free up some disk space simply delete the following directories:

* `/usr/local/texlive`
* `/Library/TeX`

### Installing BasicTeX

1. Download BasicTeX by clicking [here](http://tug.org/cgi-bin/mactex-download/BasicTeX.pkg).
2. Go through the install procedure by opening the BasicTeX `.pkg` that you just downloaded. All of the defaults are fine.
3. Run the setup [script](https://github.com/Ultramann/ultramann.github.io/blob/source/content/code/smaller_mac_latex/setup.sh): `bash setup.sh`.

## Using LaTeX

Using LaTeX is just like using any other markup language. If you want to figure out how to typeset something in LaTeX, google is your friend, there is a huge Stack Overflow presence of LaTeX questions getting answered. For slides I use the [beamer](https://www.sharelatex.com/learn/Beamer) package.

As these instructions are for building a bare bones LaTeX environment there is no GUI as you might be used to working with to edit LaTeX. Instead you can simply use your text editor of choice to write a `.tex` document. To compile the file you run `pdflatex -shell-escape <file_name>.tex` at the command line. As I use Vim as my text editor, I have a key binding to run this command on the file currently being edited to simplify this process.

One thing that you might miss is having a dynamically reloading view of the PDF that is generated, this unfortunately is something that Preview does poorly, it will reload the view of the PDF when you regenerate it, but it always returns you to the top of the document.

Fortunately there is an open source PDF reader called Skim that auto-reloads PDFs when they get regenerated and returns you to the current location. You can download it [here](https://sourceforge.net/projects/skim-app/files/Skim/Skim-1.4.29/Skim-1.4.29.dmg/download). To make it easy to open PDFs from the command line with Skim you can add the following lines to your bash profile. They will allow you to run `opentex <file_name>.pdf` from the command line to open a PDF with Skim.

>>```
>>openTex() {
>>  open -a Skim $1
>>  }
>>alias opentex=openTex
>>```

## Wrap Up

That's about it. With this set up I can easily write LaTeX in my preferred text editor, from my terminal, and still get good preview functionality all the while keeping LaTeX's memory footprint to ~0.5 GB.
