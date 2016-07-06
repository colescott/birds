function loadView(toLoad, location) {
  var xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function() {
    if (xhttp.readyState == 4 && xhttp.status == 200) {
      document.getElementById(location).innerHTML = xhttp.responseText;
    }
  };
  xhttp.open("GET", toLoad, true);
  xhttp.send();
}
