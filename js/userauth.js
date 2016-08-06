var config = {
    apiKey: "AIzaSyB_QM9xJqQONmX8ca4aCWCw0x8e_czLWDQ",
    authDomain: "cardinalbirds.firebaseapp.com",
    databaseURL: "https://cardinalbirds.firebaseio.com",
    storageBucket: "project-7535783528222319330.appspot.com",
};

firebase.initializeApp(config);

var database = firebase.database();

var user = firebase.auth().currentUser;
var name, email, photoUrl, uid;

firebase.auth().onAuthStateChanged(function(user) {
    updateContent();
    if (user) {
        if (user.emailVerified) {
            console.log('Email is verified');
        } else {
            console.log('Email is not verified');
            user.sendEmailVerification();
        }
        name = user.displayName;
        email = user.email;
        photoUrl = user.photoURL;
        uid = user.uid;
        loadDashboard();
    } else {
        loadView('views/home.html');
    }
});

function loginUser() {
    var email = document.getElementById('loginEmail').value;
    var password = document.getElementById('loginPassword').value;
    firebase.auth().signInWithEmailAndPassword(email, password).catch(function(error) {
        var errorCode = error.code;
        var errorMessage = error.message;
        document.getElementById('loginError').innerHTML = error.message;
        updateContent();
    });
}

function logoutUser() {
    firebase.auth().signOut().then(function() {
        loadView('view/home.html');
    }, function(error) {
        alert(error);
    });
    updateContent();
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

function resetPassword() {
    var email = document.getElementById('resetEmail').value;
    firebase.auth().sendPasswordResetEmail(email);
}

function updateContent() {
    user = firebase.auth().currentUser;
}
