$(document).keydown(function(event) {
    if (event.keyCode == 13) {
        $("#loginButton").click();
    }
});

$('img').bind('contextmenu', function(e) {
    return false;
});

function loadView(path) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById("content").innerHTML = xhttp.responseText;
        }
    };
    xhttp.open("GET", path, true);
    xhttp.send();
}

function homePage() {
    loadView("views/homepage.html");
}

function birdtreePage() {
    loadView("views/birdtree.html");
}

function registerPage() {
    loadView("views/register.html");
}

function forgotPasswordPage() {
    loadView("views/forgotpassword.html");
}

function changeEmailPage() {
    loadView("views/changeemail.html");
}

function changePasswordPage() {
    loadView("views/changepassword.html");
}

function deleteUserPage() {
    loadView("views/deleteuser.html");
}

function spoof1Page() {
    loadView("views/spoof1.html");
}

function spoof2Page() {
    loadView("views/spoof2.html");
}

function spoof3Page() {
    loadView("views/spoof3.html");
}
