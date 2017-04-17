const express = require("express");
const mongoose = require("mongoose");
const passport = require("passport");
const LocalStrategy = require("passport-local").Strategy;
const jwt = require("jsonwebtoken");
const router = express.Router();
const expressValidator = require("express-validator");

const auth = require("./auth.js");
const users = require("./users.js");
const teams = require("./teams.js");
const lessons = require("./lessons.js");
const util = require("./util.js");
const { authenticate, errorHandler, jwtSecret, validator } = require("./middleware.js");

const User = require("./models/user");

mongoose.Promise = global.Promise;

router.use(passport.initialize());
router.use(expressValidator())

passport.use(new LocalStrategy({
        usernameField: "email"
    }, (username, password, done) => {
    User.authenticate()(username, password, (err, user, passErr) => {
        if (err)
            return done(err);
        if (passErr)
            return done(null, false, passErr);
        if (user)
            done(null, user);
    });
}));

router.get("/ping", (req, res) => {
    return res.send("Pong v1!");
});

router.use("/auth", auth);
router.use("/teams", teams);
router.use("/lessons", lessons);
router.use("/users", users);

// Error catch if error was thrown
router.use(errorHandler);

module.exports = router;
