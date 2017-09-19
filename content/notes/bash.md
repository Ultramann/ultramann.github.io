---
title: Bash
published: July 28, 2017
---

Notes on the [bourne again shell](https://en.wikipedia.org/wiki/Bash_(Unix_shell)). Has both small programs and scripting.

<!--more-->

# Bash Notes

## Looping

### For Loop

* Match directly on glob: `for fd in *; do echo $fd; done`
* Iterate over output of command: `for fd in $(ls); do echo $fd; done`

## Awesome Programs

### xargs

* `ls | grep .py$ | xargs wc -l`: counts the number of lines of each `.py` file in the directory and sums them up.

### grep

* `grep -v`: inverse selection

### find

Searches for files in a directory hierarchy.

#### Useful Flags

* `-name <pattern>`
* `-type {(f)ile, (d)irectory}`

#### Examples

* `find . -name *.py`
* `find . -t d -name notes`

## Examples

Counting lines by last word occurrence without lines that have a quote.

```
cat data.csv | grep -v \" | grep COKE | cut -f 4 -d ',' | sort | uniq -c
```

Data

>>```
>>CTIERRY COKE,3.38,89.61,beverages
>>COKE,1.56,14.98,beverages
>>COKE,1.55,46.97,beverages
>>DIET COKE 2002,2.54,6.3,beverages
>>COKE,4.52,76.36,beverages
>>```

