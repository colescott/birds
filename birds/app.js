const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");
const morgan = require("morgan");

const { forceSSL } = require("./middleware.js");
const favicon = require("serve-favicon");
const bodyParser = require("body-parser");
const mongoose = require("mongoose");
const dbConfig = require("./db.js");
const api = require("./api");

app.use(favicon(path.join(__dirname, "favicon.ico")));
app.use(bodyParser.json());

/*
if (process.env.NODE_ENV == "PROD") {
    app.use(forceSSL);
}

app.use(cors());

if (process.env.NODE_ENV !== "TEST") {
    app.use(morgan("dev"));
}*/

app.use("/", express.static(path.join(__dirname, "static")));

app.get("/ping", (req, res) => {
    res.send("Pong!");
});

app.use("/api", api);

// Returns main page on all others... used when calling "/login" or others
app.use("*", express.static(path.join(__dirname, "static")));

module.exports = app;
module.exports.mongoose = mongoose;