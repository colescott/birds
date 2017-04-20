const expressJwt = require("express-jwt");
const jwt = require("jsonwebtoken");

const User = require("./models/user.js");
const { error } = require("./util.js");

module.exports.authenticate = (req, res, next) => {
    const bearer = req.headers.authorization;
    if (!bearer) {
        return res.status(401).send({
            code: 401,
            error: "Unauthorized",
            message: "No authorization token was found"
        });
    }
    const token = bearer.match(/Bearer (.*)/)[1];
    const jwtSecret = req.app.get("JWT_SECRET");
    jwt.verify(token, jwtSecret, (err, decoded) => {
        if (err) {
            return res.status(401).send({
                code: 401,
                error: "Unauthorized",
                message: "Token is not valid"
            });
        } else {
            const userId = decoded._doc._id;
            User.findById(userId)
                .then(user => {
                    req.user = user;
                    next();
                });
       }
    });

};

module.exports.errorWrapper = f => async (req, res, next) => {
    try {
        await f(req, res, next);
    } catch (e) {
        console.error(e);
        next(e);
    }
};

module.exports.errorHandler = (err, req, res, next) => {
    if (err)
        return res.status(500).send(error(500, err.message));
    return next();
};

module.exports.validator = (args = []) => async (req, res, next) => {
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

module.exports.permissions = (args = []) => (req, res, next) => {
    if (!(req.user && req.user.permissions)) {
        return res.status(401).send({
            code: 401,
            error: "Unauthorized",
            message: "You do not have the required permissions"
        });
    }
    if (args.some(arg => req.user.permissions[arg] != true)) {
        return res.status(401).send({
            code: 401,
            error: "Unauthorized",
            message: "You do not have the required permissions"
        });
    }
    next();
}
