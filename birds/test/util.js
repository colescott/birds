const User = require("../api/v1/models/user.js");
const exports = module.exports = {};

exports.clone = (obj) => {
    if (null == obj || "object" != typeof obj) return obj;
    let copy = obj.constructor();
    for (let attr in obj) {
        if (obj.hasOwnProperty(attr)) copy[ attr ] = obj[ attr ];
    }
    return copy;
};

exports.equal = (obj1, obj2) => {
    return JSON.stringify(obj1) == JSON.stringify(obj2);
};

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
                 if (err || passwordError)
                    return reject(err || passwordError);
                resolve(model);
             });
        })
