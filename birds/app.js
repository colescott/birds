const express = require("express");
const app = express();
const path = require("path");
const cors = require("cors");

const api = require("./api");

app.use(cors());

app.use("/", express.static(path.join(__dirname, "static")));

app.get("/ping", (req, res) => {
    res.send("Pong!");
});

app.get("*", (req, res) => {
     res.status(404).send("Error 404");
});

app.use("/api", api);

module.exports = app;
