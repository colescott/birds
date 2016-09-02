const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");
const morgan = require("morgan");
const firebase = require("firebase");

var config = {
    apiKey: "AIzaSyB_QM9xJqQONmX8ca4aCWCw0x8e_czLWDQ",
    authDomain: "cardinalbirds.firebaseapp.com",
    databaseURL: "https://cardinalbirds.firebaseio.com",
    storageBucket: "project-7535783528222319330.appspot.com",
};
firebase.initializeApp(config);

const api = require("./api");

app.use(cors());
app.use(morgan("dev"));

app.use("/", express.static(path.join(__dirname, "static")));

app.get("/ping", (req, res) => {
    res.send("Pong!");
});

app.get("*", (req, res) => {
     res.status(404).send("Error 404");
});

app.use("/api", api);

module.exports = app;
