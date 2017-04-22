const express = require("express");
const router = express.Router();

const util = require("./util.js");
const { error } = require("./util.js");
const { authenticate, errorWrapper, validator } = require("./middleware.js");
const User = require("./models/user");
const Team = require("./models/team");

const _ = require("lodash");

//Options for mongoose
const options = {};
options.new = true;

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
router.post(
    "/",
    validator(["email", "password", "firstname", "lastname"]),
    errorWrapper(async (req, res) => {
        const userQuery = await User.findOne({ email: req.body.email });

        if (userQuery)
            return res
                .status(400)
                .send(error(400, "A user with that email already exists"));

        const user = new User({
            email: req.body.email,
            firstname: req.body.firstname,
            lastname: req.body.lastname,
            progress: []
        });

        User.register(user, req.body.password, (err, model, passwordErr) => {
            if (err) throw err;
            if (passwordErr) throw passwordErr;

            return res.status(200).send({ user: util.sterilizeUser(model) });
        });
    })
);

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
router.get(
    "/",
    errorWrapper(async (req, res) => {
        const userList = await User.find({});
        const users = userList.map(user => util.sterilizeUser(user));
        return res.status(200).send({ users: users });
    })
);

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
router.get(
    "/:id",
    authenticate,
    errorWrapper(async (req, res) => {
        let user;
        try {
            user = await User.findById(req.params.id);
        } catch (e) {
            return res
                .status(400)
                .send(error(400, "Unable to find a user with that id"));
        }
        if (!user)
            return res
                .status(400)
                .send(error(400, "Unable to find a user with that id"));

        const response = {
            user: req.params.id == req.user.id
                ? util.sterilizeUserAsUser(user)
                : util.sterilizeUser(user)
        };
        return res.status(200).send(response);
    })
);

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
router.put(
    "/:id",
    authenticate,
    errorWrapper(async (req, res) => {
        if (req.user.id != req.params.id)
            return res
                .status(401)
                .send(
                    error(401, "You can only perform this action as the user")
                );

        const changes = _.pick(req.body, [
            "email",
            "firstname",
            "lastname",
            "teamnumber"
        ]);

        if (req.body.password) {
            User.findById(req.params.id, (err, user) => {
                if (err) throw err;
                user.setPassword(
                    req.body.password,
                    (err, thisModel, passwordErr) => {
                        if (err) throw err;
                        if (passwordErr) throw passwordErr;
                    }
                );
            });
        }

        let user;

        if (changes != {})
            user = await User.findByIdAndUpdate(
                req.params.id,
                changes,
                options
            );
        else user = await User.findById(req.params.id);

        const response = { user: util.sterilizeUserAsUser(user) };
        return res.status(200).send(response);
    })
);

/**
 * @api {delete} /users/:id Delete user
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
router.delete(
    "/:id",
    authenticate,
    errorWrapper(async (req, res) => {
        if (req.user.id != req.params.id)
            return res
                .status(401)
                .send(
                    error(401, "You can only perform this action as the user")
                );

        await User.findByIdAndRemove(req.params.id);
        return res
            .status(200)
            .send({ message: { text: "Successfully deleted user." } });
    })
);

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
router.put(
    "/:id/setprogress",
    authenticate,
    errorWrapper(async (req, res) => {
        if (req.user.id != req.params.id)
            return res
                .status(401)
                .send(
                    error(401, "You can only perform this action as the user")
                );

        // TODO: move to middleware
        if (!req.body.id)
            return res.status(400).send(error(400, "Id not set!"));
        if (!req.body.state)
            return res.status(400).send(error(400, "State not set!"));

        const user = await User.findByIdAndUpdate(
            req.user.id,
            { $push: { progress: { id: req.body.id, state: req.body.state } } },
            options
        );
        return res.status(200).send({ user: user });
    })
);

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
router.put(
    "/:id/jointeam",
    authenticate,
    errorWrapper(async (req, res) => {
        if (req.user.id != req.params.id)
            return res
                .status(401)
                .send(
                    error(401, "You can only perform this action as the user")
                );

        const data = await Team.findOne().byNumber(req.body.teamnumber).exec();

        if (data.length <= 0)
            return res
                .status(400)
                .send(error(400, "That team does not exist."));

        if (req.body.password != data[0].password)
            return res.status(401).send(error(401, "Incorrect password"));

        const user = await User.findById(req.params.id);

        await Team.addUser(req.body.teamnumber, user);

        return res.status(200).send({ team: data });
    })
);

module.exports = router;
