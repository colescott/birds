const expressJwt = require("express-jwt");

const User = require("./models/user.js");
const { error } = require("./util.js");

module.exports.jwtSecret = process.env.JWT_SECRET;

const ejwt = expressJwt({ secret: module.exports.jwtSecret });

module.exports.authenticate = (req, res, next) => {
    ejwt(req, res, (err) => {
        if (err)
            return res.status(401).send(error(401, err.message));
        try {
            const userId = req.user._doc._id;
            User.findById(userId)
                .then(user => {
                    req.user = user;
                    next();
                });
        } catch (e) {
            console.warn(e);
            console.warn(req.user);
            next(e);
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
