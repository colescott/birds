const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");
const morgan = require("morgan");

const bodyParser = require("body-parser");
app.use(bodyParser.json());

const mongoose = require("mongoose");
const dbConfig = require("./db.js");
mongoose.connect(dbConfig.url);

const api = require("./api");

const forceSSL = function(req, res, next) {
    // This code checks the protocol from heroku proxy
    if (req.headers[ "x-forwarded-proto" ] !== "https") {
        // Redirect with a 301
        return res.redirect(301, ["https://", req.get("Host"), req.url].join(""));
    }
    return next();
 };

if (process.env.NODE_ENV == "PROD") {
    app.use(forceSSL);
}

app.use(cors());

if (process.env.NODE_ENV !== "TEST") {
    app.use(morgan("dev"));
}

app.use("/", express.static(path.join(__dirname, "static")));

app.get("/ping", (req, res) => {
    res.send("Pong!");
});

app.use("/api", api);

// Returns main page on all others... used when calling "/login" or others
app.use("*", express.static(path.join(__dirname, "static")));

module.exports = app;
