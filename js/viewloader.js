function loadView(toLoad) {
    if(!firebase.auth().currentUser)
        toLoad = 'views/home.html';

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById('content').innerHTML = xhttp.responseText;
        }
    };
    xhttp.open("GET", toLoad, true);
    xhttp.send();
}

function loadDashboard() {
    if(!firebase.auth().currentUser)
    {
        loadView('views/home.html');
        return;
    }

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById('content').innerHTML = xhttp.responseText;
            initKmap();
        }
    };
    xhttp.open("GET", 'views/dashboard.html', true);
    xhttp.send();
}

function loadGDriveView(gDriveId) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById('content').innerHTML = xhttp.responseText;

            $.get("https://docs.google.com/document/d/" + gDriveId + "/pub?embedded=true", function(html) {
                $("#google-doc-iframe").attr("srcdoc", html);
                setTimeout(function() {
                    $("#google-doc-iframe").contents().find('a[href^="http://"]').attr("target", "_blank");
                    $("#google-doc-iframe").contents().find('a[href^="https://"]').attr("target", "_blank");
                    // set height to whatever it needs
                    document.getElementById('google-doc-iframe').height = document.getElementById('google-doc-iframe').contentWindow.document.body.scrollHeight;
                }, 1000);
            });
        }
    };
    xhttp.open("GET", 'views/lesson.html', true);
    xhttp.send();
}
