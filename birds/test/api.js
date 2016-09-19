const randomstring = require("randomstring");
const request = require("supertest");
const app = require("../app.js");

const util = require("./util.js");

const testTeamReqest = { name: "CardinalBotics", teamnumber: 4159 };
const testUser = { email: "test@team4159.org", password: "password", firstname: "Test", lastname: "Account", progress: [] };
const testUserNoPass = { email: testUser.email, firstname: testUser.firstname, lastname: testUser.lastname };

const testUser2 = { email: "test2@team4159.org", password: "password", firstname: "Test", lastname: "Account #2", progress: [] };
var testUser2WithId;
var teamPassword;

var testUserWithId;
var testUserWithIdNoProgress;
var loginToken;

var loginToken2;

describe("Base Server", () => {
    it("should return pong on ping", (done) => {
        request(app)
            .get("/ping")
            .set("Accept", "text")
            .send()
            .expect("Content-Type", "text/html; charset=utf-8")
            .expect(200)
            .expect("Pong!", done);
    });
    it("should return 404 on random path", (done) => {
        request(app)
            .get("/kafbsjkfbjsfkdjsflhsdfk")
            .set("Accept", "text")
            .expect("Content-Type", "text/html; charset=utf-8")
            .expect(404, done);
    });
});

describe("APIv1", () => {
    it("should return pong on ping", (done) => {
        request(app)
            .get("/api/v1/ping")
            .set("Accept", "text")
            .send()
            .expect("Content-Type", "text/html; charset=utf-8")
            .expect(200)
            .expect("Pong v1!", done);
    });
    describe("POST /api/v1/users", () => {
        it("created and returned new user", (done) => {
            request(app)
            .post("/api/v1/users")
            .set("Accept", "application/json")
            .send(testUser)
            .expect("Content-Type", /json/)
            .expect(function(res) {
                if (res.body.name)
                    throw new Error("User already exists in database");
                delete res.body.data.user.id;
            })
            .expect(200, {
                data: {
                    user: testUserNoPass
                }
            }, done);
        });
        it("responds error on empty user", (done) => {
            request(app)
            .post("/api/v1/users")
            .set("Accept", "application/json")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "Email value required."))
                    throw new Error("Server not sending error with \"Email value required.\"");
            })
            .end(done);
        });
    });

    describe("GET /api/v1/users", () => {
        it("respond with json", (done) => {
            request(app)
            .get("/api/v1/users")
            .set("Accept", "application/json")
            .expect("Content-Type", /json/)
            .expect(200, done);
        });
        it("conatins test user", (done) => {
            request(app)
            .get("/api/v1/users")
            .set("Accept", "application/json")
            .expect(function(res) {
                var found = false;
                res.body.data.users.forEach((user) => {
                    if (user.email == testUser.email)
                    {
                        testUserWithIdNoProgress = util.clone(user);
                        found = true;
                    }
                });
                if (!found)
                    throw new Error("user does not exist!");
            })
            .end(done);
        });
    });

    describe("POST /api/v1/auth/login", () => {
        it("respond with json", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send({ email: "test@team4159.org", password: "password" })
            .expect("Content-Type", /json/)
            .expect(200, done);
        });
        it("responds with user", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send(testUser)
            .expect(function(res) {
                testUserWithId = util.clone(res.body.data.user);
                delete res.body.data.user.progress;
                if (!util.equal(res.body.data.user, testUserWithIdNoProgress))
                    throw new Error("user not in response");
            })
            .end(done);
        });
        it("responds with token", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send(testUser)
            .expect(function(res) {
                if (!res.body.data.token)
                    throw new Error("Login token not set");
                loginToken = res.body.data.token;
            })
            .end(done);
        });
        it("responds error on no email", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send({ password: testUser.password })
            .expect(function(res) {
                if (!(res.body.error.message == "Unauthorized."))
                    throw new Error("Server not sending error with \"Unauthorized.\"");
            })
            .end(done);
        });
        it("responds error on no password", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send({ email: testUser.email })
            .expect(function(res) {
                if (!(res.body.error.message == "Unauthorized."))
                    throw new Error("Server not sending error with \"Unauthorized.\"");
            })
            .end(done);
        });
        it("responds error on bad password", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send({ email: testUser.email, password: "notthepassword" })
            .expect(function(res) {
                if (!(res.body.error.message == "Unauthorized."))
                    throw new Error("Server not sending error with \"Unauthorized.\"");
            })
            .end(done);
        });
    });

    describe("GET /users/:id", () => {
        it("respond with json", (done) => {
            request(app)
            .get("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .expect("Content-Type", /json/)
            .expect(200, done);
        });
        it("responds with user", (done) => {
            request(app)
            .get("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .expect(function(res) {
                delete res.body.data.user.progress;
                if (!util.equal(res.body.data.user, testUserWithIdNoProgress))
                    throw new Error("User not equal to test user");
            })
            .end(done);
        });
        it("responds with 401 when not logged in", (done) => {
            request(app)
            .get("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .expect(401, done);
        });
    });

    describe("PUT /users/:id", () => {
        it("respond with json", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .send({})
            .expect("Content-Type", /json/)
            .expect(200, done);
        });
        it("responds with changed user", (done) => {
            const newUser = {
                firstname: randomstring.generate(),
                lastname: randomstring.generate(),
                teamnumber: util.randomInRange(1, 6237)
            };
            request(app)
            .put("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .send(newUser)
            .expect(function(res) {
                if (util.equal(res.body.data.user, testUserWithId))
                    throw new Error("User was not changed, equal to old user");
                if (res.body.data.user.firstname != newUser.firstname)
                    throw new Error("First name was not changed");
                if (res.body.data.user.lastname != newUser.lastname)
                    throw new Error("Last name was not changed");
                if (res.body.data.user.teamnumber != newUser.teamnumber)
                    throw new Error("Team number was not changed");
            })
            .end(done);
        });
        it("responds error on no token", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "You need to be logged in to do this."))
                    throw new Error("Server not sending error with \"You need to be logged in to do this.\"");
            })
            .end(done);
        });
        it("responds error on bad token", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer correcthorsebatterystaple")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "Invalid token."))
                    throw new Error("Server not sending error with \"Invalid token.\"");
            })
            .end(done);
        });
    });

    describe("POST /api/v1/teams", () => {
        it("create new team", (done) => {
            request(app)
            .post("/api/v1/teams")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .send(testTeamReqest)
            .expect("Content-Type", /json/)
            .expect(function(res) {
                teamPassword = res.body.data.team.password;
            })
            .end(done);
        });
    });

    describe("GET /api/v1/teams", () => {
        it("respond with json", (done) => {
            request(app)
            .get("/api/v1/teams")
            .set("Accept", "application/json")
            .expect("Content-Type", /json/)
            .expect(200, done);
        });
        it("conatins test team", (done) => {
            request(app)
            .get("/api/v1/teams")
            .set("Accept", "application/json")
            .expect(function(res) {
                var found = false;
                res.body.data.teams.forEach((team) => {
                    if (team.teamnumber == testTeamReqest.teamnumber) {
                        found = true;
                    }
                });
                if (!found)
                    throw new Error("team does not exist!");
            })
            .end(done);
        });
    });

    describe("PUT /users/:id/jointeam", () => {
        it("created test user 2", (done) => {
            request(app)
            .post("/api/v1/users")
            .set("Accept", "application/json")
            .send(testUser2)
            .expect("Content-Type", /json/)
            .expect(function(res) {
                testUser2WithId = res.body.data.user;
            })
            .end(done);
        });
        it("login test user 2", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send(testUser2)
            .expect(function(res) {
                loginToken2 = res.body.data.token;
            })
            .end(done);
        });
        it("join team", (done) => {
            request(app)
            .put("/api/v1/users/" + testUser2WithId.id + "/jointeam")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken2)
            .send({ teamnumber: 4159, password: teamPassword })
            .expect(200, done);
        });
        it("user has teamnumber=4159", (done) => {
            request(app)
            .get("/api/v1/users/" + testUser2WithId.id)
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken2)
            .expect(function(res) {
                if (res.body.data.user.teamnumber != 4159)
                    throw new Error("user.teamnumber is not 4159 :(");
            })
            .end(done);
        });
    });

    describe("PUT /teams/:num/delete", () => {
        it("responds team deleted", (done) => {
            request(app)
            .put("/api/v1/teams/" + testTeamReqest.teamnumber + "/delete")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .expect(200, done);
        });
        it("test team deleted from database", (done) => {
            request(app)
            .get("/api/v1/teams")
            .set("Accept", "application/json")
            .expect(function(res) {
                var found = false;
                res.body.data.teams.forEach((team) => {
                    if (team.teamnumber == testTeamReqest.teamnumber) {
                        found = true;
                    }
                });
                if (found)
                    throw new Error("team is still in the database!");
            })
            .end(done);
        });
        it("responds error on no token", (done) => {
            request(app)
            .put("/api/v1/teams/" + testTeamReqest.teamnumber + "/delete")
            .set("Accept", "application/json")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "You need to be logged in to do this."))
                    throw new Error("Server not sending error with \"You need to be logged in to do this.\"");
            })
            .end(done);
        });
        it("responds error on bad token", (done) => {
            request(app)
            .put("/api/v1/teams/" + testTeamReqest.teamnumber + "/delete")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer correcthorsebatterystaple")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "Invalid token."))
                    throw new Error("Server not sending error with \"Invalid token.\"");
            })
            .end(done);
        });
    });

    describe("POST /auth/logout", () => {
        it("logs out with 200", (done) => {
            request(app)
            .post("/api/v1/auth/logout")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .expect(200, done);
        });
        it("logs in with token", (done) => {
            request(app)
            .post("/api/v1/auth/login")
            .set("Accept", "application/json")
            .send(testUser)
            .expect(function(res) {
                if (!res.body.data.token)
                    throw new Error("Login token not set");
                loginToken = res.body.data.token;
            })
            .end(done);
        });
    });

    describe("PUT /users/:id/delete", () => {
        it("responds user deleted", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id + "/delete")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken)
            .expect({
                data: {
                    message: "successfully deleted user."
                }
            }, done);
        });
        it("responds user 2 deleted", (done) => {
            request(app)
            .put("/api/v1/users/" + testUser2WithId.id + "/delete")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer " + loginToken2)
            .expect({
                data: {
                    message: "successfully deleted user."
                }
            }, done);
        });
        it("user is deleted from database", (done) => {
            request(app)
            .get("/api/v1/users/")
            .set("Accept", "application/json")
            .expect(function(res) {
                var found = false;
                res.body.data.users.forEach((user) => {
                    if (user.email == testUser.email)
                    {
                        testUserWithId = util.clone(user);
                        found = true;
                    }
                });
                if (found)
                    throw new Error("The user still exists");
            })
            .end(done);
        });
        it("responds error on no token", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id + "/delete")
            .set("Accept", "application/json")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "You need to be logged in to do this."))
                    throw new Error("Server not sending error with \"You need to be logged in to do this.\"");
            })
            .end(done);
        });
        it("responds error on bad token", (done) => {
            request(app)
            .put("/api/v1/users/" + testUserWithId.id + "/delete")
            .set("Accept", "application/json")
            .set("Authorization", "Bearer correcthorsebatterystaple")
            .send({})
            .expect(function(res) {
                if (!(res.body.error.message == "Invalid token."))
                    throw new Error("Server not sending error with \"Invalid token.\"");
            })
            .end(done);
        });
    });
});
