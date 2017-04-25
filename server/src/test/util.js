const User = require("../api/v1/models/user.js");

exports.randomInRange = (min, max) => {
    const nMin = Math.ceil(min);
    const nMax = Math.floor(max);
    return Math.floor(Math.random() * (nMax - nMin)) + nMin;
};

exports.clearDB = async db => {
    await db.dropDatabase();
};

exports.addUser = (user, password) =>
    new Promise((resolve, reject) => {
        User.register(user, "testPass", (err, model, passwordError) => {
            if (err || passwordError) return reject(err || passwordError);
            resolve(model);
        });
    });
