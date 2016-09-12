const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");
const morgan = require("morgan");

const ejwt = require('express-jwt');
app.use(ejwt({secret: "correcthorsebatterystaple", userProperty: 'tokenPayload'}).unless({path: ['/api/v1/auth/login', '/api/v1/users/:id', '/api/v1/users']}));

const bodyParser = require('body-parser')
app.use( bodyParser.json() );

const mongoose = require('mongoose');
const dbConfig = require('./db.js');
mongoose.connect(dbConfig.url);

const passport = require('passport');
const expressSession = require('express-session');
app.use(expressSession({secret: 'correcthorsebatterystaple'}));
app.use(passport.initialize());
app.use(passport.session());


app.use(function(req, res, next) {
  if (req.tokenPayload) {
    req.user = null;//getUserById(req.tokenPayload.id);
  }
  if (req.user) {
    return next();
  } else {
    return res.status(401).json({ status: 'error', code: 'unauthorized' });
  }
});

// Error handler middleware
app.use(function(err, req, res, next) {
  console.error(err);
  return res.status(500).json({ status: 'error', code: 'unauthorized' });
});

const api = require("./api");

app.use(cors());
app.use(morgan("dev"));

app.use("/", express.static(path.join(__dirname, "static")));

app.get("/ping", (req, res) => {
    res.send("Pong!");
});

app.use("/api", api);

// This should be the last app.get() in this file. Elias learned this the hard way
app.get("*", (req, res) => {
     res.status(404).send("Error 404");
});

module.exports = app;
