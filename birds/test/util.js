var exports = module.exports = {};

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
}