var exports = module.exports = {};

const noSession = (res) => {
    return error(res, "You need to be logged in to do this.", 401);
};

const invalidToken = (res) => {
    return error(res, "Invalid token.", 401);
};

const unauthorized = (res) => {
    return error(res, "Unauthorized.", 401);
};

const message = (res, message) => {
    return data(res, {message: message});
};

const error = (res, message, status) => {
    return res.status(status ? status : 500).send({error: {message: message ? message : "Unknown error."}});
};

const data = (res, data) => {
    return res.status(200).send({data: data});
};

const sterilizeUser = (user) => {
    return {
        id: user.id,
        email: user.email,
        firstname: user.firstname,
        lastname: user.lastname,
        teamnumber: user.teamnumber
    };
};

const sterilizeUserWithProgress = (user) => {
    const usr = sterilizeUser(user);
    usr.progress = user.progress;
    return usr;
}

exports.noSession = noSession;
exports.invalidToken = invalidToken;
exports.unauthorized = unauthorized;
exports.message = message;
exports.error = error;
exports.data = data;
exports.sterilizeUser = sterilizeUser;
exports.sterilizeUserWithProgress = sterilizeUserWithProgress;
