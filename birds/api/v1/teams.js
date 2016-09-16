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

const getTeam = (req, res) => {
    Team.find({ teamnumber: req.body.teamnumber }, function(err, teams) {
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

const performActionOnTeam = (req, res) => {
    Team.userIsAdmin(req.params.num, req.user, (err, isAdmin) => {
        if (err)
            return util.error(res, err);
        if (!isAdmin)
            return util.error(res, "You are not an admin of this team.", 401);
        switch (req.params.action) {
        case "delete":
            Team.findAndRemove({ teamnumber: req.params.num }, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully deleted team.");
            });
            break;
        case "addadmin":
            Team.setAdmin(req.params.num, req.body.user, true, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully removed admin.");
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
