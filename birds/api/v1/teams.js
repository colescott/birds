const express = require("express");
const router = express.Router();

const randomstring = require("randomstring");

const util = require("./util.js");
const { error } = require("./util.js");
const { authenticate, errorWrapper, validator } = require("./middleware.js");
const Team = require("./models/team");
const User = require("./models/user");

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

    
router.post("/", authenticate, validator(["name", "teamnumber"]), errorWrapper(async (req, res) => {
    const user = req.user;
    
    if (await Team.exists(req.body.teamnumber))
        return res.status(400).send(error(400, "A team with that number already exists!"));

    const team = new Team({
        name: req.body.name,
        teamnumber: req.body.teamnumber,
        password: randomstring.generate(6),
        users: []
    });

    await team.save();
    await Team.addUser(team.teamnumber, user);
    await Team.setAdmin(team.teamnumber, user, true);
    await User.findByIdAndUpdate(user.id, { teamnumber: team.teamnumber, isAdmin: true });

    const tem = util.sterilizeTeam(team);
    tem.password = team.password;

    return res.status(200).send({ team: tem });
}));


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
router.get("/", errorWrapper(async (req, res) => {
    const teams = await Team.find({});

    return res.status(200).send({ teams: teams.map(team => (util.sterilizeTeam(team))) });
}));

/**
 * @api {get} /teams/:num Get team by number
 * @apiName Get team by number
 * @apiGroup Teams
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.team Array of teams
 * @apiSuccess {String} data.team.name Team name
 * @apiSuccess {Number} data.team.teamnumber Team number
 * @apiSuccess {String} [data.team.password] Team password (Only if user is admin)
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
router.get("/:num", authenticate, errorWrapper(async (req, res) => {
    const teams = await Team.find({ teamnumber: req.params.num });

    if (teams.length < 1)
        return res.status(400).send(error(400, "That team does not exist."));

    const team = util.sterilizeTeam(teams[ 0 ]);

    const userIsAdmin = await Team.userIsAdmin(req.params.num, req.user);

    if (userIsAdmin)
        team.password = teams[ 0 ].password;

    return res.status(200).send({ team: team });
}));

/**
 * @api {delete} /teams/:id Delete team
 * @apiName Delete team
 * @apiGroup Teams
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
 *         "message": "Successfully deleted team."
 *     }
 *
 */
router.delete("/:num", authenticate, errorWrapper(async (req, res) => {
    const teams = await Team.find({ teamnumber: req.params.num });

    if (teams.length < 1)
        return res.status(400).send(error(400, "That team does not exist."));

    const userIsAdmin = await Team.userIsAdmin(req.params.num, req.user);

    if (!userIsAdmin)
        return res.status(401).send(error(401, "You are not an admin of this team."));

    await Team.findOneAndRemove({ teamnumber: req.params.num });
    return res.status(200).send({ message: { text: "Successfully deleted team." } });
}));

/**
 * @api {put} /teams/:num/addadmin Add admin
 * @apiName Add admin
 * @apiGroup Teams
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *teams
 * @apiParam {Object} user User to add as admin
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully added admin."
 *     }
 *
 */
router.put("/:num/addadmin", authenticate, validator(["user"]), errorWrapper(async (req, res) => {
    const teams = await Team.find({ teamnumber: req.params.num });

    if (teams.length < 1)
        return res.status(400).send(error(400, "That team does not exist."));

    const userIsAdmin = await Team.userIsAdmin(req.params.num, req.user);

    if (!userIsAdmin)
        return res.status(401).send(error(401, "You are not an admin of this team."));

    await Team.setAdmin(req.params.num, req.body.user, true);
    return res.status(200).send({ message: { text: "Successfully added admin." } });
}));

/**
 * @api {put} /teams/:num/removeadmin Remove admin
 * @apiName Remove admin
 * @apiGroup Teams
 *
 * @apiHeader {String} authorization Authorization token with format "Bearer {token}"
 *
 * @apiParam {Object} user User to remove as admin
 *
 * @apiSuccess {Object} data Data object containing info
 * @apiSuccess {Object} data.message Message
 *
 * @apiSuccessExample {json} Success-Response:
 *     HTTP/1.1 200 OK
 *     {
 *       "data": {
 *         "message": "Successfully removed admin."
 *     }
 *
 */
router.put("/:num/removeadmin", authenticate, validator(["user"]), errorWrapper(async (req, res) => {
    const teams = await Team.find({ teamnumber: req.params.num });

    if (teams.length < 1)
        return res.status(400).send(error(400, "That team does not exist."));

    const userIsAdmin = await Team.userIsAdmin(req.params.num, req.user);

    if (!userIsAdmin)
        return res.status(401).send(error(401, "You are not an admin of this team."));

    const adminNum = Team.numberOfAdmins(req.params.num);
    if (adminNum <= 1)
        return res.status(400).send(error(400, "You cannot remove the only admin."));

    await Team.setAdmin(req.params.num, req.body.user, false);
    return res.status(200).send({ message: { text: "Successfully removed admin." } });
}));

module.exports = router;
