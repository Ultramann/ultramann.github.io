$(function() {
    $("tr.table-teasable").hover(
        function() { $(this).find("span.hidable").css("display", "block"); },
        function() { $(this).find("span.hidable").css("display", "none"); }
    );

    $("tr.floating-teasable").mouseover(
        function() {
            $('#bytes > tbody > tr').each(
                function() { $(this).css("background-color", "transparent"); }
            ); 
            $(this).css("background-color", "#c4c4c4");
            $("div.preview").html($(this).find("span.hidable").html());
        }
    );

    $("div.col-container").mouseleave(
        function() {
            $("div.preview").html("");
            $('#bytes > tbody > tr').each(
                function() { $(this).css("background-color", "transparent"); }
            ); 
        }
    );

    $("tr").click( 
        function() { window.location = $(this).find("a").attr("href"); }
    ).hover(
        function() { $(this).toggleClass("hover"); }
    );
});
