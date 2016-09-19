const mongoose = require("mongoose");
const Schema = mongoose.Schema;

const Team = new Schema({
    name: String,
    teamnumber: Number,
    password: String,
    users: [{
        id: String,
        isAdmin: Boolean,
        isModerator: Boolean,
        moderates: [{
            branch: String
        }]
    }]
});

Team.query.byNumber = function(teamnumber) {
    return this.find({ teamnumber: teamnumber });
};

 // cb is (err, bool)
Team.statics.containsUser = function(teamnumber, user, cb) {
    this.findOne().byNumber(teamnumber).exec((err, data) => {
        if (err)
            return cb(err);
        if (data.length <= 0)
            return cb("That team does not exist.");
        let found = false;
        if (!data.users)
            return cb(null, false);
        data.users.forEach((usr) => {
            if (found)
                return;
            if (usr.id == user.id)
                found = true;
        });
        return cb(null, found);
    });
};

Team.statics.userIsAdmin = function(teamnumber, user, cb) {
    this.findOne().byNumber(teamnumber).exec((err, data) => {
        if (err)
            return cb(err);
        if (data.length <= 0)
            return cb("That team does not exist.");
        let done = false;
        data[ 0 ].users.forEach((usr) => {
            if (usr.id == user.id) {
                done = true;
                cb(null, usr.isAdmin);
            }
        });
        if (!done)
            return cb(null, false);
    });
};

Team.statics.numberOfAdmins = function(teamnumber, cb) {
    this.findOne().byNumber(teamnumber).exec((err, data) => {
        if (err)
            return cb(err);
        if (data.length <= 0)
            return cb("That team does not exist.");
        let adminNum = 0;
        data.users.forEach((usr) => {
            if (usr.isAdmin)
                adminNum++;
        });
        return cb(null, adminNum);
    });
};

// cb is (err)
Team.statics.setAdmin = function(teamnumber, user, isAdmin, cb) {
    this.findOne().byNumber(teamnumber).update({ "users.id": user.id }, { "$set": {
        "users.$.isAdmin": isAdmin
    } }, (err) => {
        return cb(err);
    });
};

 // cb is (err, bool)
Team.statics.exists = function(teamnumber, cb) {
    this.findOne().byNumber(teamnumber).exec((err, data) => {
        if (err)
            return cb(err);
        if (data.length <= 0)
            return cb(null, false);
        return cb(null, true);
    });
};

Team.statics.addUser = function(teamnumber, user, isAdmin, cb) {
    this.containsUser(teamnumber, user, (err, exists) => {
        if (err)
            return cb(err);
        if (exists)
            return cb(null);
        this.findOne({ teamnumber: teamnumber }).update({ $push: { "users": { id: user.id, isAdmin: isAdmin } } }, (err) => {
            return cb(err);
        });
    });
};

module.exports = mongoose.model("Team", Team);
