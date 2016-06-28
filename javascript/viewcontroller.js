$(document).keydown(function(event){
    if(event.keyCode == 13){
        $("#loginButton").click();
    }
});

$('img').bind('contextmenu', function(e) {
    return false;
});

function registerPage() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/register.html", true);
  xhttp.send();
}

function forgotPasswordPage() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/forgotpassword.html", true);
  xhttp.send();
}

function changeEmailPage() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/changeemail.html", true);
  xhttp.send();
}

function changePasswordPage() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/changepassword.html", true);
  xhttp.send();
}

function deleteUserPage() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/deleteuser.html", true);
  xhttp.send();
}

function spoof1Page() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/spoof1.html", true);
  xhttp.send();
}

function spoof2Page() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/spoof2.html", true);
  xhttp.send();
}

function spoof3Page() {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById("content").innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", "views/spoof3.html", true);
  xhttp.send();
}
