var exports = module.exports = {};

const util = require("./util.js");
const User = require("./models/user");
const Team = require("./models/team");

//Options for mongoose
const options = {};
options.new = true;

exports.register = (req, res) => {
    //TODO: make all args required
    if (!req.body.email)
        return util.error(res, "Email value required.", 400);
    Team.exists(req.body.teamnumber, (err, value) => {
        if (err || !value)
            return util.error(res, "Team does not exist!", 400);
        const usr = new User({
            email: req.body.email,
            firstname: req.body.firstname,
            lastname: req.body.lastname,
            teamnumber: req.body.teamnumber,
            progress: []
        });
        User.register(usr, req.body.password, (err, thisModel, passwordErr) => {
            if (err)
                return util.error(res, err);
            if (passwordErr)
                return util.error(res, passwordErr);
            User.findById(thisModel._id, (err, user) => {
                if (err)
                    return util.error(res, err);
                const response = {
                    user: util.sterilizeUser(user)
                };
                return util.data(res, response);
            });
        });
    });
};

exports.getUsers = (req, res) => {
    User.find({}, function(err, users) {
        const usrs = users.map(user => (util.sterilizeUser(user)));
        const val = { users: usrs };
        return util.data(res, val);
    });
};

exports.getUserById = (req, res) => {
    User.findById(req.params.id, (err, user) => {
        if (err)
            return util.error(res, err);
        const response = { user: req.params.id == req.user.id ? util.sterilizeUserWithProgress(user) : util.sterilizeUser(user) };
        return util.data(res, response);
    });
};

exports.updateUserById = (req, res) => {
    if (req.user.id != req.params.id)
        return util.unauthorized(res);

    const changes = {};
    if (req.body.email)
        changes.email = req.body.email;
    if (req.body.firstname)
        changes.firstname = req.body.firstname;
    if (req.body.lastname)
        changes.lastname = req.body.lastname;
    if (req.body.teamnumber)
        changes.teamnumber = req.body.teamnumber;

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

    if (changes != {})
        User.findByIdAndUpdate(req.params.id, changes, options, (err, user) => {
            if (err)
                return util.error(res, err);
            const response = { user: util.sterilizeUserWithProgress(user) };
            return util.data(res, response);
        });
    else
        User.findById(req.params.id, (err, user) => {
            if (err)
                return util.error(res, err);
            const response = { user: util.sterilizeUserWithProgress(user) };
            return util.data(res, response);
        });
};

exports.performActionOnUser = (req, res) => {
    if (req.user.id != req.params.id)
        return util.unauthorized(res);

    User.findById(req.params.id, (err, user) => {
        if (err)
            return util.error(res, err);

        switch (req.params.action) {
        case "delete":
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
                "use strict";
                
                let found = false;

                user.progress.forEach((obj) => {
                    if (found)
                        return;
                    if (obj.id == req.body.id) {
                        found = true;

                        User.update({ "progress.id": req.body.id }, { "$set": {
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
        default:
            return util.error(res, "Action not found", 400);
        }
    });
};
