const express = require("express");
const mongoose = require("mongoose");
const passport = require("passport");
const LocalStrategy = require("passport-local").Strategy;
const jwt = require("jsonwebtoken");
const expressJwt = require("express-jwt");
const router = express.Router();

const users = require("./users.js");
const teams = require("./teams.js");
const util = require("./util.js");

const User = require("./models/user");

const jwtSecret = process.env.JWT_SECRET;

mongoose.Promise = global.Promise;

router.use(passport.initialize());

const ejwt = expressJwt({ secret: jwtSecret });

const authenticate = (req, res, next) => {
    ejwt(req, res, (err) => {
        if (err && err.code && err.code == "invalid_token")
            return util.invalidToken(res);
        if (err && err.code && err.code == "credentials_required")
            return util.noSession(res);
        next();
    });
};

passport.use(new LocalStrategy({
        usernameField: "email"
    }, (username, password, done) => {
    User.authenticate()(username, password, (err, user, passErr) => {
        if (err)
            return done(err);
        if (passErr)
            return done(null, false, passErr);
        if (user)
        {
            done(null, user);
        }
    });
}));

router.get("/ping", (req, res) => {
    return res.send("Pong v1!");
});

/**
 * @api {post} /users Register user
 * @apiName Register
 * @apiGroup User
 *
 * @apiParam {String} email Users email.
 * @apiParam {String} password Users password.
 * @apiParam {String} firstname Users first name.
 * @apiParam {String} lastname Users last name.
 * @apiParam {String} teamnumber Users team number.
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.user User object
 * @apiSuccess {String} data.user.id Users id
 * @apiSuccess {String} data.user.email Users email
 * @apiSuccess {String} data.user.firstname Users firstname
 * @apiSuccess {String} data.user.id Users lastname
 * @apiSuccess {Number} data.user.id Users teamnumber
 *
 * @apiSuccessExample Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "user": {
 *           "id": "ILUVULESSTHAN3",
 *           "email": "cardinalbirdsdev@gmail.com",
 *           "firstname": "CardinalBIRDS",
 *           "lastname": "Dev Team",
 *           "teamnumber": 4159
 *         }
 *       }
 *     }
 *
 */
router.post("/users", users.register);

router.get("/users", users.getUsers);

router.get("/users/:id", authenticate, users.getUserById);

router.put("/users/:id", authenticate, users.updateUserById);

router.put("/users/:id/:action", authenticate, users.performActionOnUser);

router.post("/teams", teams.postCreateTeam);

router.get("/teams", teams.getTeams);

router.post('/auth/login', function(req, res, next) {
    passport.authenticate('local', {
        session: false
    }, function(err, user) {
        if (err) return util.error(res, err);
        if (!user) {
            return util.unauthorized(res);
        } else {
            const response = {
                token: jwt.sign({ id: user.id }, jwtSecret, { expiresIn: 2 * 60 * 60 }),
                user: util.sterilizeUserWithProgress(user)
            };
            return util.data(res, response);
        }
    })(req, res, next);
});

router.post("/auth/logout", authenticate, (req, res) => {
    req.logout();
    return res.send(util.data({ message: "Logged out successfully" }));
});

module.exports = router;
