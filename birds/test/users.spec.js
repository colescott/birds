const request = require("supertest");
const testUtil = require("./util.js");
const mongoose = require("mongoose");
const _ = require("lodash");
const jwt = require("jsonwebtoken");
const User = require("../api/v1/models/user");

const express = require("express");
const app = express();

const middleware = require("../middleware");
const usersRouter = require("../api/v1/users");

describe("Users", () => {
    let db;
    let user;
    let token;

    const testUser = {
        email: "email",
        password: "pass",
        firstname: "first",
        lastname: "last",
        progress: [],
        password: "pass"
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
        mongoose.Promise = Promise;
        await mongoose.connect("mongodb://localhost/test-users");
        app.set("JWT_SECRET", "TEST");
        app.use(middleware("TEST"));
        app.use("/api/v1/users", usersRouter);
        db = mongoose.connection.db;
    });
    beforeEach(async () => {
        // add a new user
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

    describe("Get Users", () => {
        it("Should list all users", async () => {
            await request(app).get("/api/v1/users").expect(200).expect(res => {
                expect(res.body).toEqual({
                    users: [
                        _.omit(
                            Object.assign({}, testUser, {
                                id: res.body.users[0].id
                            }),
                            ["password", "progress"]
                        )
                    ]
                });
            });
        });
    });
    describe("Register", () => {
        it("Should allow registration", async () => {
            const newUser = {
                email: "mailme",
                password: "pass",
                firstname: "firstname",
                lastname: "lastname",
                password: "I_have_surpassed_you"
            };
            await request(app).post("/api/v1/users").send(newUser).expect(200).expect(res => {
                expect(res.body.user.id).toBeDefined();
                res.body.user.id = "id";
                expect(res.body).toEqual({
                    user: Object.assign({ id: "id" }, _.omit(newUser, ["password"]))
                });
            });
            await request(app).get("/api/v1/users").expect(200).expect(res => {
                expect(res.body.users[0].id).toBeDefined();
                delete res.body.users[0].id;
                delete res.body.users[1].id;
                expect(res.body).toEqual({
                    users: [
                        _.omit(testUser, ["password", "progress"]),
                        _.omit(newUser, ["password"])
                    ]
                });
            });
        });
        it("Should require paramaters", async () => {
            await request(app).post("/api/v1/users").expect(400).expect(res => {
                expect(res.body).toEqual({
                    code: 400,
                    error: "Bad Request",
                    message: "Missing parameters."
                });
            });
        });
        it("Should require all paramaters", async () => {
            await request(app)
                .post("/api/v1/users")
                .send({ email: "email", password: "pass", firstname: "first" })
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Missing parameters."
                    });
                });
        });
        it("Should not allow duplicate account", async () => {
            await request(app).post("/api/v1/users").send(testUser).expect(400).expect(res => {
                expect(res.body).toEqual({
                    code: 400,
                    error: "Bad Request",
                    message: "A user with that email already exists"
                });
            });
        });
    });

    describe("Get a User", () => {
        it("Should require being logged in", async () => {
            await request(app).get("/api/v1/users/cookie").expect(401).expect(res => {
                expect(res.body).toEqual({
                    code: 401,
                    error: "Unauthorized",
                    message: "No authorization token was found"
                });
            });
        });
        it("Should send 400 on an invalid id", async () => {
            await request(app)
                .get("/api/v1/users/cookie")
                .set("authorization", token)
                .expect(400)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 400,
                        error: "Bad Request",
                        message: "Unable to find a user with that id"
                    });
                });
        });
        it("Should return the correct user", async () => {
            const id = await request(app)
                .post("/api/v1/users")
                .send(_.omit(JamesBond, ["id"]))
                .expect(200)
                .then(res => {
                    return res.body.user.id;
                });
            await request(app)
                .get(`/api/v1/users/${id}`)
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body.user).toEqual(
                        Object.assign({ id }, _.omit(JamesBond, ["password"]))
                    );
                });
        });
        it("Should return the correct user", async () => {
            // Create the first user
            await request(app).post("/api/v1/users").send(_.omit(JamesBond, ["id"])).expect(200);

            // Create the second user
            const id = await request(app)
                .post("/api/v1/users")
                .send(_.omit(BuggsBunny, ["id"]))
                .expect(200)
                .then(res => res.body.user.id);

            await request(app)
                .get(`/api/v1/users/${id}`)
                .set("authorization", token)
                .expect(200)
                .expect(res => {
                    expect(res.body.user).toEqual(
                        Object.assign({ id }, _.omit(BuggsBunny, ["password"]))
                    );
                });
        });
    });

    describe("Update User", () => {
        it("Should update the user", async () => {
            await request(app)
                .put(`/api/v1/users/${user.id}`)
                .set("authorization", token)
                .send({ email: "new email" })
                .expect(200)
                .expect(res => {
                    expect(_.omit(res.body.user, "id")).toEqual(
                        _.omit(Object.assign({}, testUser, { email: "new email" }), "password")
                    );
                });
        });
        it("Should not allow updating another user", async () => {
            // Create the user
            const firstId = await request(app)
                .post("/api/v1/users")
                .send(_.omit(JamesBond, ["id"]))
                .expect(200)
                .then(res => res.body.user.id);

            // Try to update JamesBond
            await request(app)
                .put(`/api/v1/users/${firstId}`)
                .set("authorization", token)
                .send({ email: "iam@a.spy" })
                .expect(401)
                .expect(res => {
                    expect(res.body).toEqual({
                        code: 401,
                        error: "Unauthorized",
                        message: "You can only perform this action as the user"
                    });
                });
        });
    });

    describe("User Actions", () => {
        it("set user progress");
        it("reset user progress");
        it("delete user");
        it("join team");
    });
});
