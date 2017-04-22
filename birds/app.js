const express = require("express");
const app = express();
const path = require("path");

const middleware = require("./middleware.js");
const favicon = require("serve-favicon");
const api = require("./api");

app.use(middleware(process.env.NODE_ENV));

app.use(favicon(path.join(__dirname, "favicon.ico")));

app.use("/", express.static(path.join(__dirname, "static")));

app.use("/api", api);

// Returns main page on all others... used when calling "/login" or others
app.use("*", express.static(path.join(__dirname, "static")));

module.exports = app;
