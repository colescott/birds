const express = require("express");
const router = express.Router();

const morgan = require("morgan");
const bodyParser = require("body-parser");
const cors = require("cors");
const expressValidator = require("express-validator");
const passport = require("passport");

const forceSSL = (req, res, next) => {
    // This code checks the protocol from heroku proxy
    if (req.headers["x-forwarded-proto"] !== "https") {
        // Redirect with a 301
        return res.redirect(
            301,
            ["https://", req.get("Host"), req.url].join("")
        );
    }
    return next();
};

module.exports = (mode = "dev") => {
    if (mode != "TEST") {
        router.use(morgan("dev"));
    }

    router.use(bodyParser.json());
    router.use(cors());

    router.use(expressValidator());
    router.use(passport.initialize());

    if (mode == "production") {
        router.use(forceSSL);
    }
    return router;
};
