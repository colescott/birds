const request = require("supertest");
const mongodb = require("mongodb");
const testUtil = require("./util.js");
const mongoose = require("mongoose");
const _ = require("lodash");
const jwt = require("jsonwebtoken");
const express = require("express");
const User = require("../api/v1/models/user");
const app = require("../app");

describe("Teams", () => {
    let db;
    let token;
    let user;

    let bond;
    let bondToken;

    const testUser = {
        email: "email",
        password: "pass",
        firstname: "first",
        lastname: "last"
    };

    const testTeam = {
        teamnumber: 4159,
        name: "CardinalBotics"
    };
    const JamesBond = {
        email: "james@nota.spy",
        password: "I<3James",
        firstname: "James",
        lastname: "Bond"
    };

    beforeAll(async () => {
        app.set("JWT_SECRET", "TEST");
        mongoose.Promise = Promise;
        await mongoose.connect("mongodb://localhost/test-lessons");
        db = mongoose.connection.db;
    });
    beforeEach(async () => {
        // Generate a new uer
        const newUser = new User(testUser);
        user = await testUtil.addUser(newUser, "testpass");

        const bondUser = new User(JamesBond);
        bond = await testUtil.addUser(bondUser, JamesBond.password);

        // generate a token
        const signedUser = jwt.sign(user, "TEST");
        token = `Bearer ${signedUser}`;

        // generate a token
        const signedBond = jwt.sign(bond, "TEST");
        bondToken = `Bearer ${signedBond}`;
    });
    afterEach(async () => {
        // nuke the db
        await testUtil.clearDB(db);
    });
    afterAll(async () => {
        /*
         * Without this line mongoose will crash after 35
         * seconds of inactivity, such as when running a test
         * harness in watch mode.
         * This makes me feel :(
         */
        await mongoose.disconnect();
    });

    describe("Create team", () => {
        it("Should allow create team", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeDefined();
                });
        }, 15000);
        it("Should require name", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send({ teamnumber: 4159 })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                });
        });
        it("Should require teamnumber", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send({ name: testTeam.name })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                });
        });
        it("Should require auth", async () => {
            await request(app)
                .post("/api/v1/teams")
                .send({ testUser })
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should not allow create duplicate team", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "A team with that number already exists!"
                    });
                });
        }, 10000);
    });

    describe("Get teams", () => {
        it("Should list no teams", async () => {
            await request(app)
                .get("/api/v1/teams")
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        teams: []
                    });
                });
        });
        it("Should list all teams", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam);
            await request(app)
                .get("/api/v1/teams")
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        teams: [testTeam]
                    });
                });
        });
    });

    describe("Get team by number", () => {
        it("Should return 401 on no auth", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .get("/api/v1/teams/4159")
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should return 400 on unknown team", async () => {
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "That team does not exist."
                    });
                });
        });
        it("Should return team from admin", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeDefined();
                });
        });
        it("Should return team with external user", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeUndefined();
                });
        });
    });

    describe("Delete team", () => {
        it("Should return 401 on no auth", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .delete("/api/v1/teams/4159")
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should return 400 on unknown team", async () => {
            await request(app)
                .delete("/api/v1/teams/4159")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "That team does not exist."
                    });
                });
        });
        it("Should return 400 on external user", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .delete("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "You are not an admin of this team."
                    });
                });
        });
        it("Should delete team", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .get("/api/v1/teams")
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        teams: [testTeam]
                    });
                });
            await request(app)
                .delete("/api/v1/teams/4159")
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        message: {
                            text: "Successfully deleted team."
                        }
                    });
                });
            await request(app)
                .get("/api/v1/teams")
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        teams: []
                    });
                });
        });
    });

    describe("Add admin to team", () => {
        it("Should return 401 on no auth", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put("/api/v1/teams/4159/addadmin")
                .send({ user: bond })
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should return 400 on unknown team", async () => {
            await request(app)
                .put("/api/v1/teams/4159/addadmin")
                .set("authorization", token)
                .send({ user: bond })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "That team does not exist."
                    });
                });
        });
        it("Should require user", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put("/api/v1/teams/4159/addadmin")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                });
        });
        it("Should add admin", async () => {
            const team = await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put(`/api/v1/users/${bond._id}/jointeam`)
                .set("authorization", bondToken)
                .send({ teamnumber: team.res.body.team.teamnumber, password: team.res.body.team.password })
                .expect(200);
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeUndefined();
                });
            await request(app)
                .put("/api/v1/teams/4159/addadmin")
                .set("authorization", token)
                .send({ user: { id: bond._id } })
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        message: {
                            text: "Successfully added admin."
                        }
                    });
                });
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeDefined();
                });
        });
    });

    describe("Remove admin from team", () => {
        it("Should return 401 on no auth", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put("/api/v1/teams/4159/removeadmin")
                .send({ user: bond })
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "No authorization token was found"
                    });
                });
        });
        it("Should return 400 on unknown team", async () => {
            await request(app)
                .put("/api/v1/teams/4159/removeadmin")
                .set("authorization", token)
                .send({ user: bond })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "That team does not exist."
                    });
                });
        });
        it("Should require user", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put("/api/v1/teams/4159/removeadmin")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                });
        });
        it("Should remove admin", async () => {
            const team = await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200);
            await request(app)
                .put(`/api/v1/users/${bond._id}/jointeam`)
                .set("authorization", bondToken)
                .send({ teamnumber: team.res.body.team.teamnumber, password: team.res.body.team.password })
                .expect(200);
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeUndefined();
                });
            await request(app)
                .put("/api/v1/teams/4159/addadmin")
                .set("authorization", token)
                .send({ user: { id: bond._id } })
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        message: {
                            text: "Successfully added admin."
                        }
                    });
                });
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeDefined();
                });
            await request(app)
                .put("/api/v1/teams/4159/removeadmin")
                .set("authorization", token)
                .send({ user: { id: bond._id } })
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        message: {
                            text: "Successfully removed admin."
                        }
                    });
                });
            await request(app)
                .get("/api/v1/teams/4159")
                .set("authorization", bondToken)
                .expect(200)
                .expect(res => {
                    expect(res.body.team.teamnumber).toEqual(testTeam.teamnumber);
                    expect(res.body.team.name).toEqual(testTeam.name);
                    expect(res.body.team.password).toBeUndefined();
                });
        });
    });
});