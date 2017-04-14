var exports = module.exports = {};

const _ = require("lodash");

const util = require("./util.js");
const User = require("./models/user");
const Team = require("./models/team");

//Options for mongoose
const options = {};
options.new = true;

exports.register = (req, res) => {
    User.find({ email: req.body.email }, (err, users) => {
        if (err)
            return util.error(res, err);
        if (users.length > 0)
            return util.error(res, "A user with that email already exists!", 400);
    });
    const usr = new User({
        email: req.body.email,
        firstname: req.body.firstname,
        lastname: req.body.lastname,
        progress: []
    });
    User.register(usr, req.body.password, (err, thisModel, passwordErr) => {
        if (err)
            return util.error(res, err);
        if (passwordErr)
            return util.error(res, passwordErr, 400);
        User.findById(thisModel._id, (err, user) => {
            if (err)
                return util.error(res, err);

            const response = {
                user: util.sterilizeUser(user)
            };
            return util.data(res, response);
        });
    });
};

exports.getUsers = (req, res) => {
    User.find({}, (err, users) => {
        const usrs = users.map(user => (util.sterilizeUser(user)));
        const val = { users: usrs };
        return util.data(res, val);
    });
};

exports.getUserById = (req, res) => {
    User.findById(req.params.id, (err, user) => {
        if (err)
            return util.error(res, err);
        const response = { user: req.params.id == req.user.id ? util.sterilizeUserAsUser(user) : util.sterilizeUser(user) };
        return util.data(res, response);
    });
};

exports.updateUserById = (req, res) => {

    if (req.user.id != req.params.id)
        return util.unauthorized(res);

    const changes = _.pick(req.body, ["email", "firstname", "lastname", "teamnumber"]);

    // TODO: just update based on logged in users id
    if (req.body.password) {
        User.findById(req.params.id, (err, user) => {
            if (err)
                return util.error(res, err);
            user.setPassword(req.body.password, (err, thisModel, passwordErr) => {
                if (err)
                    return util.error(res, err);
                if (passwordErr)
                    return util.error(res, passwordErr);
                // TODO: finish password setting
            });
        });
    }

    User.findById(req.params.id, (err, user) => {
        if (err)
            return util.error(res, err);
        const response = { user: util.sterilizeUserAsUser(user) };
        return util.data(res, response);
    });
};

exports.performActionOnUser = (req, res) => {
    // TODO: This should change like hella
    if (req.user.id != req.params.id)
        return util.unauthorized(res);

    User.findById(req.params.id, (err, user) => {
        if (err)
            return util.error(res, err);

        switch (req.params.action) {
        case "delete":
            // TODO: Should be delete request
            User.findByIdAndRemove(req.params.id, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "successfully deleted user.");
            });
            break;
        case "setprogress":
            if (!req.body.id)
                return util.error(res, "Id not set!", 400);
            if (!req.body.state)
                return util.error(res, "State not set!", 400);

            {
                let found = false;

                user.progress.forEach((obj) => {
                    if (found)
                        return;
                    if (obj.id == req.body.id) {
                        found = true;

                        User.findById(req.params.id).update({ "progress.id": req.body.id }, { "$set": {
                            "progress.$.state": req.body.state
                        } }, options, (err) => {
                            if (err)
                                return util.error(res, err);
                            return util.message(res, "Successfully set progress");
                        });
                    }
                });
                if (found)
                    return;
            }

            User.findByIdAndUpdate(req.user.id, { $push: { "progress": { id: req.body.id, state: req.body.state } } }, options, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully set progress");
            });
            break;
        case "resetprogress":
            User.findByIdAndUpdate(req.user.id, { progress: [] }, options, (err) => {
                if (err)
                    return util.error(res, err);
                return util.message(res, "Successfully reset progress");
            });
            break;
        case "jointeam":
            Team.findOne().byNumber(req.body.teamnumber).exec((err, data) => {
                if (err)
                    return util.error(res, err);
                if (data.length <= 0)
                    return util.error(res, "That team does not exist.", 400);

                if (req.body.password != data[ 0 ].password)
                    return util.error(res, "Incorrect password.", 401);

                Team.addUser(req.body.teamnumber, user, false, (err) => {
                    if (err)
                        return util.error(res, err);
                    Team.userIsAdmin(req.body.teamnumber, user, (err, isAdmin) => {
                        User.findByIdAndUpdate(req.user.id, { teamnumber: req.body.teamnumber, isAdmin: isAdmin }, options, (err) => {
                            if (err)
                                return util.error(res, err);
                        });

                        return util.message(res, "Successfully joined team");
                    });
                });
            });
            break;
        default:
            return util.error(res, "Action not found", 400);
        }
    });
};
