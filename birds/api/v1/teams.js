var exports = module.exports = {};

const randomstring = require("randomstring");

const util = require("./util.js");
const Team = require("./models/team");
const User = require("./models/user");

const createTeam = (name, teamnumber, adminUser, cb) => {

    var team = new Team({
        name: name,
        teamnumber: teamnumber,
        password: randomstring.generate(6),
        users: []
    });
    team.save((err) => {
        if (err)
            return cb(err);
        Team.addUser(teamnumber, adminUser, true, (err) => {
            if (err)
                return cb(err);
            User.findByIdAndUpdate(adminUser.id, { teamnumber: teamnumber, isAdmin: true }, (err) => {
                if (err)
                    return cb(err);

                return cb(null, team.password);
            });
        });
    });
};

const getTeams = (req, res) => {
    Team.find({}, function(err, teams) {
        if (err)
            return util.error(res, err);
            
        const tems = teams.map(team => (util.sterilizeTeam(team)));
        const val = { teams: tems };
        return util.data(res, val);
    });
};

const getTeam = (req, res) => {
    Team.find({ teamnumber: req.params.num }, function(err, teams) {
        if (err)
            return util.error(res, err);

        if (teams.length < 1)
            return util.error(res, "That team does not exist.", 400);

        const team = util.sterilizeTeam(teams[ 0 ]);
        Team.userIsAdmin(req.params.num, req.user, (err, isAdmin) => {
            if (err)
                return util.error(res, err);
            if (isAdmin)
                team.password = teams[ 0 ].password;
            const val = { team: team };
            return util.data(res, val);
        });
    });
};

const postCreateTeam = (req, res) => {
    Team.exists(req.body.teamnumber, (err, exists) => {
        if (err)
            return util.error(res, err);
        if (exists)
            return util.error(res, "A team with that number already exists!", 400);
        User.findById(req.user.id, (err, user) => {
            if (err)
                return util.error(res, err);
            createTeam(req.body.name, req.body.teamnumber, user, (err, password) => {
                if (err)
                    return util.error(res, err);
                Team.findOne().byNumber(req.body.teamnumber).exec((err, data) => {
                    if (err)
                        return util.error(res, err);
                    let team = util.sterilizeTeam(data[ 0 ]);
                    team.password = password;
                    const response = {
                        team: team
                    };

                    util.data(res, response);
                });
            });
        });
    });
};

const performActionOnTeam = (req, res) => {
    // TODO: Break out
    Team.userIsAdmin(req.params.num, req.user, (err, isAdmin) => {
        if (err)
            return util.error(res, err);
        if (!isAdmin)
            return util.error(res, "You are not an admin of this team.", 401);

        switch (req.params.action) {
        case "delete":
            Team.findOneAndRemove({ teamnumber: req.params.num }, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully deleted team.");
            });
            break;
        case "addadmin":
            Team.setAdmin(req.params.num, req.body.user, true, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully added admin.");
            });
            break;
        case "removeadmin":
            Team.numberOfAdmins(req.params.num, (err, num) => {
                if (err)
                    return util.error(res, err);
                if (num <= 1)
                    return util.error(res, "You cannot remove the only admin.", 400);

                Team.setAdmin(req.params.num, req.body.user, false, (err) => {
                    if (err)
                        return util.error(res, err);
                    return util.message(res, "Successfully removed admin.");
                });
            });
            break;
        default:
            return util.error(res, "Action not found", 400);
        }
    });
};

exports.createTeam = createTeam;
exports.getTeams = getTeams;
exports.getTeam = getTeam;
exports.postCreateTeam = postCreateTeam;
exports.performActionOnTeam = performActionOnTeam;
