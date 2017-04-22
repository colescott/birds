const express = require("express");
const app = express();
const cors = require("cors");
const bodyParser = require("body-parser");
const mongoose = require("mongoose");
const expressValidator = require("express-validator");
const middleware = require("../api/v1/middleware");
const passport = require("passport");
const LocalStrategy = require("passport-local").Strategy;

module.exports = dbUrl => {
    app.use(cors());
    mongoose.Promise = Promise;
    mongoose.connect(dbUrl);
    app.use(bodyParser.json());
    app.use(expressValidator());
    app.use(passport.initialize());

    passport.use(
        new LocalStrategy(
            {
                usernameField: "email"
            },
            (username, password, done) => {
                User.authenticate()(
                    username,
                    password,
                    (err, user, passErr) => {
                        if (err) return done(err);
                        if (passErr) return done(null, false, passErr);
                        if (user) done(null, user);
                    }
                );
            }
        )
    );

    return app;
};
