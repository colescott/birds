var config = {
  apiKey: "AIzaSyBgOayoEYFyJtSkrQDMw1Pz64PccVYx81c",
  authDomain: "birds-e48b7.firebaseapp.com",
  databaseURL: "https://birds-e48b7.firebaseio.com",
  storageBucket: "birds-e48b7.appspot.com",
};

firebase.initializeApp(config);

var database = firebase.database();

var user = firebase.auth().currentUser;
var name, email, photoUrl, uid;

firebase.auth().onAuthStateChanged(function(user) {
  if (user) {
    name = user.displayName;
    email = user.email;
    photoUrl = user.photoURL;
    uid = user.uid;
    loadView('views/dashboard.html','content');
  } else {
    loadView('views/home.html','content');
  }
});

function loginUser() {
  var email = document.getElementById('loginEmail').value;
  var password = document.getElementById('loginPassword').value;
  firebase.auth().signInWithEmailAndPassword(email, password).catch(function(error) {
  var errorCode = error.code;
  var errorMessage = error.message;
  document.getElementById('loginError').innerHTML = error.message;
  });
}

function logoutUser() {
  firebase.auth().signOut().then(function() {
    // Logged out
  }, function(error) {
    alert(error);
  });
}

function registerUser() {
  var email = document.getElementById('registerEmail').value;
  var password1 = document.getElementById('registerPassword1').value;
  var password2 = document.getElementById('registerPassword2').value;
  if (password1 == password2) {
    firebase.auth().createUserWithEmailAndPassword(email, password1).catch(function(error) {
    // Handle Errors here.
    var errorCode = error.code;
    var errorMessage = error.message;
    document.getElementById('registerError').innerHTML = errorMessage;
    });
  } else {
    document.getElementById('registerError').innerHTML = "Passwords do not match";
  }
}

function updateContent() {
  var user = firebase.auth().currentUser;
  var email = user.email;
  console.log(email);
}
