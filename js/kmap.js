function initKmap() {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById("kmap").innerHTML = xhttp.responseText;
            $('#kmap').css("background-color", "#fefbf7");
            $($('#kmap').children()[0]).css("width", "100%");
            $($('#kmap').children()[0]).css("height", "750px");
            svgPanZoom($('#kmap').children()[0], {
                zoomScaleSensitivity: 0.3,
                dblClickZoomEnabled: false
            });

            //Set classes for each svg link
            var $elements = $('a', 'svg');
            $elements.each(function(index, e1) {
                $(e1).children().each(function(index, e2) {
                    $(e1).attr("clickJs", $(e1).attr("xlink:href"));
                    $(e1).css( "cursor", "pointer" );
                    $(e2).addClass("kmap" + $(e1).attr("xlink:href"));
                    $(e1).click(
                        function(event) {
                            eval($(e1).attr("clickJs"));
                        });
                    $(e1).removeAttr("xlink:href");
                });
            });
        }
    };
    xhttp.open("GET", "img/kmap-design.svg", true);
    xhttp.send();
}
