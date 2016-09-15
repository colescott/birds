var exports = module.exports = {};

const util = require("./util.js");
const Team = require("./models/team");
const User = require("./models/user");

const createTeam = (name, teamnumber, adminUser) => {
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
        return err;
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
        var usr = new User({
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
            User.findById(thisModel._id, (err, user) =>
            {
                if (err)
                    return util.error(res, err);
                const errr = createTeam(req.body.name, req.body.teamnumber, user);
                if (errr)
                    return util.error(res, errr);
                return util.message(res, "Successfully created team.");
            });
        });
    });
};

exports.createTeam = createTeam;
exports.getTeams = getTeams;
exports.postCreateTeam = postCreateTeam;
