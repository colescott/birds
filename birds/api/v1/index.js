const express = require("express");
const router = express.Router();

router.get("/ping", (req, res) => {
    res.send("Pong! v1");
});

router.get("/addUser", (req, res) => {
    firebase.auth().createUserWithEmailAndPassword(email, password);
});

module.exports = router;
