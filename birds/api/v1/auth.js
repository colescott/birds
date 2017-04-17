const express = require("express");
const router = express.Router();

const util = require("./util.js");
const { authenticate, errorWrapper } = require("./middleware.js");

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
router.post("/login", errorWrapper((req, res, next) => {
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
}));

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
router.post("/logout", authenticate, errorWrapper((req, res) => {
    req.logout();
    return util.message(res, "Logged out successfully");
}));

module.exports = router;
