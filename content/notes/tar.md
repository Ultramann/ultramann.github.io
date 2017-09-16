---
title: Tar
published: August 12, 2017
---

Notes on the `tar` program.

<!--more-->

# Tar Notes

I think I basically only ever use `tar` to archive a directory, maybe compress it, and to extract a directory from an archive. Here's how you do that.

## Create Archive

```
tar -cvf(z) <name_of_archive.tar(.gz)> <directory_to_archive>
```

Flags:

* `c`: `c`reate archive.
* `f`: create archive in `f`ile name. This is a shortcut for doing: `tar -cv(z) <directory_to_archive> > <name_of_archive.tar(.gz)>`.
* `v`: `v`erbosely describe the archive that gets created.
* `z`: compress the archive that gets created with g`z`ip. Just uses default compression level `6`. There's a way to pass the compression level to gzip from a tar option, but I never use that. Instead I'd use: `tar -cv <directory_to_archive> | gzip -9 > <name_of_archive.tar.gz>`

## Extract from Archive

```
tar -xvf(z) <name_of_archive.tar(.gz)>
```

Flags:

* `x`: e`x`tract archive.
* `v`: `v`erbosely describe the archive that gets extracted.
* `f`: extract archive from `f`ile.
* `z`: decompress with g`z`ip then extract. 
