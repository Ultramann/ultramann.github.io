$(function() {
    $("tr.teasable").hover(
        function() { $(this).find("td:last-child span.hidable").css("display", "block") },
        function() { $(this).find("td:last-child span.hidable").css("display", "none") }
    );

    $('tr').click( 
        function() { window.location = $(this).find('a').attr('href') })
           .hover(
        function() { $(this).toggleClass('hover') });
});

