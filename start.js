const app = require("./birds/app.js");

const listener = app.listen(process.env.PORT || 8000, () => {
    console.log("Server Started on Port:", listener.address().port);
});
