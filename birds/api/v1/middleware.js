const expressJwt = require("express-jwt");

const { error } = require("./util.js");

module.exports.jwtSecret = process.env.JWT_SECRET;

const ejwt = expressJwt({ secret: module.exports.jwtSecret });

module.exports.authenticate = (req, res, next) => {
    ejwt(req, res, (err) => {
        if (err)
            return res.status(401).send(error(401, err.message));

        next();
    });
};

module.exports.errorWrapper = f => async (req, res, next) => {
    try {
        await f(req, res, next);
    } catch (e) {
        next(e);
    }
};

module.exports.errorHandler = (err, req, res, next) => {
    if (err)
        return res.status(500).send(error(500, err.message));
    return next();
};

module.exports.validator = (args) => async (req, res, next) => {
    args.forEach(arg => {
        req.checkBody(arg).notEmpty();
    });
    const result = await req.getValidationResult();
    if (!result.isEmpty()) {
        return res.status(400).send(error(400, "Missing parameters."));
    } else {
        next();
    }
};
