const request = require("supertest");
const mongodb = require("mongodb");
const testUtil = require("./util.js");
const mongoose = require("mongoose");
const _ = require("lodash");
const jwt = require("jsonwebtoken");
const express = require("express");
const User = require("../api/v1/models/user");

describe("Teams", () => {
    let app;
    let db;
    let token;
    let user;

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
    const BuggsBunny = {
        email: "buggs@rabbits.co.uk",
        password: "what's_up_doc?",
        firstname: "Buggs",
        lastname: "Bunny"
    };

    beforeAll(async () => {
        process.env.JWT_SECRET = "TEST";
        process.env.NODE_ENV = "TEST";
        process.env.MONGODB_URI = "mongodb://localhost/test-teams";
        app = require("../app");
        db = mongoose.connection.db;
    });
    beforeEach(async () => {
        // Generate a new uer
        const newUser = new User(testUser);
        user = await testUtil.addUser(newUser, "testpass");

        // generate a token
        const signedUser = jwt.sign(user, "TEST");
        token = `Bearer ${signedUser}`;
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

    describe("Get Teams", () => {
        it("Should list all teams", async () => {
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

    describe("Create team", () => {
        xit("Should allow create team", async () => {
            await request(app)
                .post("/api/v1/users")
                .send(testUser)
                .expect(200);
            await request(app)
                .post("/api/v1/teams")
                .set("authorization", token)
                .send(testTeam)
                .expect(200)
                .expect(res => {
                    expect(res.body).toEqual({
                        teams: []
                    });
                });
        });
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
                        "message": "Missing parameters."
                    });
                });
        });
        xit("Should require teamnumber", async () => {
            await request(app)
                .post("/api/v1/teams")
                .set("authorizatin", token)
                .send({ name: "CardinalBotics" })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        "message": "Missing parameters."
                    });
                });
        });
        xit("Should not allow duplicate account", async () => {
            await request(app)
                .post("/api/v1/users")
                .send(testUser)
                .expect(200)
                .expect(res => {
                    expect(res.body.user.id).toBeDefined();
                    res.body.user.id = "id";
                    expect(res.body).toEqual({
                        user: Object.assign({ id: "id" }, _.omit(testUser, ["password"]))
                    });
                });
            await request(app)
                .post("/api/v1/users")
                .send(testUser)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "A user with that email already exists"
                    });
                });
        });
    });
});