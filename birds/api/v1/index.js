const express = require("express");
const mongoose = require("mongoose");
const passport = require("passport");
const LocalStrategy = require("passport-local").Strategy;
const jwt = require("jsonwebtoken");
const router = express.Router();
const expressValidator = require("express-validator");

const users = require("./users.js");
const teams = require("./teams.js");
const lessons = require("./lessons.js");
const util = require("./util.js");
const { authenticate, errorHandler, jwtSecret } = require("./middleware.js");

const User = require("./models/user");

mongoose.Promise = global.Promise;

const validator = (args) => async (req, res, next) => {
    args.forEach(arg => {
        req.checkBody(arg).notEmpty();
    });
    const result = await req.getValidationResult();
    if (!result.isEmpty()) {
        return res.status(400).send({
            code: 400,
            error: "Bad Request"
        });
    } else {
        next();
    }
}

router.use(passport.initialize());
router.use(expressValidator())

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

router.use("/teams", teams);
router.use("/lessons", lessons);

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
router.post("/users", validator(["email", "password", "firstname", "lastname"]), users.register);

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

router.get("/test", (req, res) => res.send({ data: "test" }));

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
 * @api {put} /users/:id/delete Delete user
 * @apiName Delete user
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
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

/**
 * @api {put} /users/:id/setprogress Set lesson progress
 * @apiName Set lesson progress
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} id Id for lesson
 * @apiParam {String} state State for lesson
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully set progress"
 *     }
 *
 */

/**
 * @api {put} /users/:id/jointeam Join team
 * @apiName Join team
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {String} [teamnumber] Number of team
 * @apiParam {String} [password] Password for team
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully reset progress"
 *     }
 *
 */

/**
 * @api {put} /users/:id/resetprogress Reset all lesson progress
 * @apiName Reset all lesson progress
 * @apiGroup Users
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully joined team"
 *     }
 *
 */
router.put("/users/:id/:action", authenticate, users.performActionOnUser);

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
            return util.error(res, "Incorrect username or password.", 401);
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

// Error catch if error was thrown
router.use(errorHandler);

module.exports = router;
