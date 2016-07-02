// Register the callback to be fired every time auth state changes
var ref = new Firebase("https://cardinalbirds.firebaseio.com");
ref.onAuth(authDataCallback);

var loggedIn = false;

// Create a callback which logs the current auth state
function authDataCallback(authData) {
    if (authData) {
        console.log("User " + authData.uid + " is logged in with " + authData.provider);
        loggedIn = true;
        loadPage();
    } else {
        console.log("User is logged out");
        loggedIn = false;
        loadPage();
    }
}

// Callback to handle result of authentication
function authHandler(error, authData) {
    if (error) {
        document.getElementById('loginInfo').innerHTML = "Login Failed. " + error;
    } else {
        console.log("Authenticated successfully with payload:", authData);
    }
}

function loginUser() {
    ref.authWithPassword({
        email: document.getElementById('loginEmail').value,
        password: document.getElementById('loginPassword').value
    }, authHandler);
}

function logoutUser() {
    ref.unauth();
    loadPage();
}

function registerUser() {
    if (document.getElementById('registerPassword1').value == document.getElementById('registerPassword2').value) {
        ref.createUser({
            email: document.getElementById('registerEmail').value,
            password: document.getElementById('registerPassword1').value
        }, function(error, userData) {
            if (error) {
                document.getElementById('registerError').innerHTML = error;
            } else {
                console.log("Successfully created user account with uid:", userData.uid);
                alert("Successfully registered. You may now log in.");
                window.location = "";
            }
        });
    } else {
        document.getElementById('registerError').innerHTML = "Error. Passwords do not match."
    }
}

function emailPasswordReset() {
    ref.resetPassword({
        email: document.getElementById("userEmail").value
    }, function(error) {
        if (error === null) {
            alert("Password reset email sent successfully");
            loadPage();
        } else {
            document.getElementById("passwordResetError").innerHTML = error;
        }
    });
}

function changeEmail() {
    ref.changeEmail({
        oldEmail: document.getElementById('oldEmailChange').value,
        newEmail: document.getElementById('newEmailChange').value,
        password: document.getElementById('emailChangePassword').value
    }, function(error) {
        if (error === null) {
            alert("Email changed successfully");
            window.location.href = "/";
        } else {
            document.getElementById('changeEmailError').innerHTML = error;
        }
    });
}

function changePassword() {
    ref.changePassword({
        email: document.getElementById('passwordChangeEmail').value,
        oldPassword: document.getElementById('passwordChangePasswordOld').value,
        newPassword: document.getElementById('passwordChangePasswordNew').value
    }, function(error) {
        if (error === null) {
            alert("Password changed successfully");
            window.location.href = "/";
        } else {
            document.getElementById('changePasswordError').innerHTML = error;
        }
    });
}

function deleteUser() {
    ref.removeUser({
        email: document.getElementById('deleteEmail').value,
        password: document.getElementById('deletePassword').value
    }, function(error) {
        if (error === null) {
            alert("User removed successfully");
            loadPage();
        } else {
            document.getElementById('deleteError').innerHTML = error;
        }
    });
}

function loadPage() {
    if (!loggedIn) {
        homePage();
    } else {
        birdtreePage();
    }
}
