const mongoose = require('mongoose');
const Schema = mongoose.Schema;
const passportLocalMongoose = require('passport-local-mongoose');

const User = new Schema({
    email: String,
    firstname: String,
    lastname: String,
    teamnumber: Number,
    progress: [{
        id: String,
        state: String
    }]
});

User.plugin(passportLocalMongoose, {
    usernameField: "email",
    usernameLowerCase: true
});

module.exports = mongoose.model("User", User);
