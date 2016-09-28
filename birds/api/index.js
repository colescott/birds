const express = require("express");
const router = express.Router();

const v1 = require("./v1");

router.use("/v1", v1);

router.get("*", (req, res) => {
     res.status(404).send("Error 404");
});

module.exports = router;
