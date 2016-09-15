const mongoose = require("mongoose");
const Schema = mongoose.Schema;

const Team = new Schema({
    name: String,
    teamnumber: Number,
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
            return cb(null, false);
        let found = false;
        data.users.forEach((usr) => {
            if (found)
                return;
            if (usr.id == user.id)
                found = true;
        });
        return cb(found);
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

module.exports = mongoose.model("Team", Team);
