var exports = module.exports = {};

const util = require("./util.js");
const Team = require("./models/team");
const User = require("./models/user");

const createTeam = (name, teamnumber, adminUser, cb) => {
    var team = new Team({
        name: name,
        teamnumber: teamnumber,
        users: [{
            id: adminUser.id,
            isAdmin: true,
            isModerator: false,
            moderates: []
        }]
    });
    team.save((err) => {
        return cb(err);
    });
};

const getTeams = (req, res) => {
    Team.find({}, function(err, teams) {
        const usrs = teams.map(team => (util.sterilizeTeam(team)));
        const val = { teams: usrs };
        return util.data(res, val);
    });
};

const postCreateTeam = (req, res) => {
    Team.exists(req.body.teamnumber, (err, exists) => {
        if (err)
            return util.error(res, err);
        if (exists)
            return util.error(res, "A team with that number already exists!", 400);
        const usr = new User({
            email: req.body.adminUser.email,
            firstname: req.body.adminUser.firstname,
            lastname: req.body.adminUser.lastname,
            teamnumber: req.body.adminUser.teamnumber,
            progress: []
        });
        User.register(usr, req.body.adminUser.password, (err, thisModel, passwordErr) => {
            if (err)
                return util.error(res, err);
            if (passwordErr)
                return util.error(res, passwordErr);
            User.findById(thisModel._id, (err, user) => {
                if (err)
                    return util.error(res, err);
                createTeam(req.body.name, req.body.teamnumber, user, (err) => {
                    if (err)
                        return util.error(res, err);
                    Team.findOne().byNumber(req.body.teamnumber).exec((err, data) => {
                        if (err)
                            return util.error(res, err);
                        const response = {
                            user: util.sterilizeUser(user),
                            team: util.sterilizeTeam(data[ 0 ])
                        };

                        util.data(res, response);
                    });
                });
            });
        });
    });
};

exports.createTeam = createTeam;
exports.getTeams = getTeams;
exports.postCreateTeam = postCreateTeam;
