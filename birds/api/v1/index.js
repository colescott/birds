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
        if (err)
            return util.console.error(res, "Unknown server error when logging in");
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
            done(null, user);
    });
}));

router.get("/ping", (req, res) => {
    return res.send("Pong v1!");
});

// TODO: add errors to all apidocs

/**
 * @api {post} /users Register user
 * @apiName Register
 * @apiGroup Users
 *
 * @apiParam {String} email Users email.
 * @apiParam {String} password Users password.
 * @apiParam {String} firstname Users first name.
 * @apiParam {String} lastname Users last name.
 * @apiParam {Number} teamnumber Users team number.
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.user User object
 * @apiSuccess {String} data.user.id Users id
 * @apiSuccess {String} data.user.email Users email
 * @apiSuccess {String} data.user.firstname Users firstname
 * @apiSuccess {String} data.user.lastname Users lastname
 * @apiSuccess {Number} data.user.teamnumber Users teamnumber
 *
 * @apiSuccessExample {json} Success-Response:
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

/**
 * @api {get} /users Get list of users
 * @apiName Get users
 * @apiGroup Users
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object[]} data.users Array of users
 * @apiSuccess {String} data.users.id Users id
 * @apiSuccess {String} data.users.email Users email
 * @apiSuccess {String} data.users.firstname Users firstname
 * @apiSuccess {String} data.users.lastname Users lastname
 * @apiSuccess {Number} data.users.teamnumber Users teamnumber
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "users": [{
 *           "id": "ILUVULESSTHAN3",
 *           "email": "cardinalbirdsdev@gmail.com",
 *           "firstname": "CardinalBIRDS",
 *           "lastname": "Dev Team",
 *           "teamnumber": 4159
 *         },
 *         {
 *           "id": "THISISAFAKEID",
 *           "email": "admin@team4159.org",
 *           "firstname": "Admin",
 *           "lastname": "Account",
 *           "teamnumber": 4159
 *         }]
 *       }
 *     }
 *
 */
router.get("/users", users.getUsers);

/**
 * @api {get} /users/:id Get user by id
 * @apiName Get user
 * @apiGroup Users
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.user User object
 * @apiSuccess {String} data.user.id Users id
 * @apiSuccess {String} data.user.email Users email
 * @apiSuccess {String} data.user.firstname Users firstname
 * @apiSuccess {String} data.user.lastname Users lastname
 * @apiSuccess {Number} data.user.teamnumber Users teamnumber
 * @apiSuccess {Boolean} data.user.isAdmin If user is an admin of team NOTE: only returns this if logged in as user trying to get
 * @apiSuccess {Object[]} [data.user.progress] User progress NOTE: only returns this if logged in as user trying to get
 * @apiSuccess {String} data.user.progress.id Id of lesson
 * @apiSuccess {String} data.user.progress.state Progress of lesson
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "user": {
 *           "id": "ILUVULESSTHAN3",
 *           "email": "cardinalbirdsdev@gmail.com",
 *           "firstname": "CardinalBIRDS",
 *           "lastname": "Dev Team",
 *           "teamnumber": 4159,
 *           "isAdmin": true,
 *           "progress": [
 *              {
 *                "id": "thisisalessonid",
 *                "state": "complete"
 *              }
 *            ]
 *         }
 *       }
 *     }
 *
 */
router.get("/users/:id", authenticate, users.getUserById);

/**
 * @api {put} /users/:id Set user values
 * @apiName Set user values
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} [email] Users new email.
 * @apiParam {String} [password] Users new password.
 * @apiParam {String} [firstname] Users new first name.
 * @apiParam {String} [lastname] Users new last name.
 * @apiParam {Number} [teamnumber] Users new team number.
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.user User object
 * @apiSuccess {String} data.user.id Users id
 * @apiSuccess {String} data.user.email Users email
 * @apiSuccess {String} data.user.firstname Users firstname
 * @apiSuccess {String} data.user.lastname Users lastname
 * @apiSuccess {Number} data.user.teamnumber Users teamnumber
 * @apiSuccess {Object[]} data.user.progress User progress
 * @apiSuccess {String} data.user.progress.id Id of lesson
 * @apiSuccess {String} data.user.progress.state Progress of lesson
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "user": {
 *           "id": "ILUVULESSTHAN3",
 *           "email": "cardinalbirdsdev@gmail.com",
 *           "firstname": "CardinalBIRDS",
 *           "lastname": "Dev Team",
 *           "teamnumber": 4159,
 *           "progress": [
 *              {
 *                "id": "thisisalessonid",
 *                "state": "complete"
 *              }
 *            ]
 *         }
 *       }
 *     }
 *
 */
router.put("/users/:id", authenticate, users.updateUserById);

/**
 * @api {put} /users/:id/:action Perform action on user
 * @apiName Perform action on user
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} [id] Id for lesson when :action = "setprogress"
 * @apiParam {String} [state] State for lesson when :action = "setprogress"
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully deleted user"
 *     }
 *
 */
router.put("/users/:id/:action", authenticate, users.performActionOnUser);

/**
 * @api {post} /teams Create new team
 * @apiName Create new team
 * @apiGroup Teams
 *
 * @apiParam {String} name Teams name.
 * @apiParam {Number} teamnumber Teams number.
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.team Team object
 * @apiSuccess {String} data.team.name Team name
 * @apiSuccess {Number} data.team.teamnumber Team number
 * @apiSuccess {String} data.team.password Team password, 6 char long
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "team": {
 *           "name": "CardinalBotics",
 *           "teamnumber": 4159,
 *           "password": "Iluvme"
 *         }
 *       }
 *     }
 *
 */
router.post("/teams", authenticate, teams.postCreateTeam);

/**
 * @api {get} /teams Get list of teams
 * @apiName Get teams
 * @apiGroup Teams
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object[]} data.teams Array of teams
 * @apiSuccess {String} data.teams.name Team name
 * @apiSuccess {Number} data.teams.teamnumber Team number
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "teams": [{
 *           "name": "CardinalBotics",
 *           "teamnumber": 4159
 *         },
 *         {
 *           "name": "FireHawk Robotics",
 *           "teamnumber": 6000
 *         }]
 *       }
 *     }
 *
 */
router.get("/teams", teams.getTeams);

/**
 * @api {get} /teams/:num Get team by number
 * @apiName Get team by number
 * @apiGroup Teams
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.team Array of teams
 * @apiSuccess {String} data.team.name Team name
 * @apiSuccess {Number} data.team.teamnumber Team number
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "team": {
 *           "name": "CardinalBotics",
 *           "teamnumber": 4159
 *         }
 *       }
 *     }
 *
 */
router.get("/teams/:num", teams.getTeam);

/**
 * @api {put} /teams/:num/:action Perform action on team
 * @apiName Perform action on team
 * @apiGroup Teams
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {Object} [user] User when :action = "addadmin" or "removeadmin"
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully removed admin"
 *     }
 *
 */
router.put("/teams/:num/:action", authenticate, teams.performActionOnTeam);

/**
 * @api {post} /auth/login Login
 * @apiName Login
 * @apiGroup Auth
 *
 * @apiParam {String} email Users email.
 * @apiParam {String} password Users password.
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.token Auth token to use on subsequent requests
 * @apiSuccess {Object} data.user User object
 * @apiSuccess {String} data.user.id Users id
 * @apiSuccess {String} data.user.email Users email
 * @apiSuccess {String} data.user.firstname Users firstname
 * @apiSuccess {String} data.user.lastname Users lastname
 * @apiSuccess {Number} data.user.teamnumber Users teamnumber
 * @apiSuccess {Boolean} data.user.isAdmin If users is an admin of team
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "token": "correcthorsebatterystaple",
 *         "user": {
 *           "id": "FAKEIDORISIT",
 *           "email": "cardinalbirdsdev@gmail.com",
 *           "firstname": "CardinalBIRDS",
 *           "lastname": "Dev Team",
 *           "teamnumber": 4159,
 *           "isAdmin": true
 *         }
 *       }
 *     }
 *
 */
router.post("/auth/login", function(req, res, next) {
    passport.authenticate("local", {
        session: false
    }, function(err, user) {
        if (err) return util.error(res, err);
        if (!user) {
            return util.unauthorized(res);
        } else {
            const response = {
                token: jwt.sign({ id: user.id }, jwtSecret, { expiresIn: 2 * 60 * 60 }),
                user: util.sterilizeUserAsUser(user)
            };
            return util.data(res, response);
        }
    })(req, res, next);
});

/**
 * @api {post} /auth/logout Logout
 * @apiName Logout
 * @apiGroup Auth
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {String} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Logged out successfully"
 *       }
 *     }
 *
 */
router.post("/auth/logout", authenticate, (req, res) => {
    req.logout();
    return util.message(res, "Logged out successfully");
});

module.exports = router;
