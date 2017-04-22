const express = require("express");
const mongoose = require("mongoose");
const router = express.Router();

const auth = require("./auth.js");
const users = require("./users.js");
const teams = require("./teams.js");
const lessons = require("./lessons.js");
const { errorHandler } = require("./middleware.js");

mongoose.Promise = global.Promise;

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
