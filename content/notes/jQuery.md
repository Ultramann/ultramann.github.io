---
title: jQuery
published: August 15, 2017
---

Notes on the javascript library jQuery.

<!--more-->

# jQuery Notes

## Selecting

Seems to happen in a javascript function. jQuery operations are specified with `$`, the bling operator, followed by parenthesis with a string specifying the tag/class/id (or any other selector magic) of the element(s) to select. e.g.

>>```javascript
>>$("div")
>>$(".main")
>>$("#important")
>>$(".left:nth-child(3)")
>>$(".fancy-table:even")
>>```

## Operations

It seems that the bling pattern returns some sort of reference to all of the elements that match the selector. You can directly run a method on the operation which will effect all of the elements that get selected. Here are some useful methods:

* `.addClass("<class>")`: `.addClass("row")`
* `.removeClass("<class>")`: `.removeClass("fancy")`
* `.css("<property>", "<setting>")`: `.css("background-color", "blue")`
* `.prop("<property>", "<setting>")`: `.prop("disabled", "true")`
* `.html("<html>")`: `.html("<b>Oooo, bold text.</b>")`
* `.appendTo("<other-container>")`: `.appendTo(".top-div")`
  * If element lives in a container already it will be removed from that container before being appended to the new one.
* `.clone()`: makes sense that `.appendTo()` would be chained after this as the element cloned disappears otherwise.
* `.remove()`: poof, gone.
* `.parent()`: get reference to the html element(s) that the selected element(s) inherits from. Can then be manipulated with any of the other methods listed here.
* `.children()`: get reference to the html element(s) that inherit from the selected element(s). Can then be manipulated with any of the other methods listed here.
