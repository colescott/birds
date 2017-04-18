const HTTPStatus = require("http-status");

module.exports.error = (status, message) => {
    return {
        code: status || 500,
        error: HTTPStatus[ status || 500 ],
        message: message || "Unknown error."
    };
};

module.exports.sterilizeUser = (user) => {
    return {
        id: user.id,
        email: user.email,
        firstname: user.firstname,
        lastname: user.lastname,
        teamnumber: user.teamnumber
    };
};

module.exports.sterilizeUserAsUser = (user) => {
    const usr = module.exports.sterilizeUser(user);
    usr.progress = user.progress;
    usr.isAdmin = user.isAdmin;
    return usr;
};

module.exports.sterilizeTeam = (team) => {
    return {
        name: team.name,
        teamnumber: team.teamnumber
    };
};

module.exports.sterilizeLesson = (lesson) => {
    return {
        id: lesson.id,
        title: lesson.title,
        branch: lesson.branch,
        prerequisites: lesson.prerequisites.map((prereq) => module.exports.sterilizePrereq(prereq))
    };
};

module.exports.sterilizePrereq = (prereq) => {
    return {
        id: prereq.id
    };
};

module.exports.sterilizeLessonWithData = (lesson, data) => {
    let sterilized = module.exports.sterilizeLesson(lesson);
    sterilized.data = data;
    return sterilized;
};

module.exports.validId = (id) => {
    return id.match(/^[0-9a-fA-F]{24}$/);
};
