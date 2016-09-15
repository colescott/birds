if (process.env.NODE_ENV == "TEST")
{
    if (!process.env.JWT_SECRET)
        process.env.JWT_SECRET = "correcthorsebatterystaple";
    if (!process.env.MONGODB_URI)
        process.env.MONGODB_URI = "mongodb://localhost/passport";
}


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

app.use(cors());
if (process.env.NODE_ENV !== "TEST") {
    app.use(morgan("dev"));
}

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
