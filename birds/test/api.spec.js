const request = require("supertest");
const testUtil = require("./util.js");
const mongoose = require("mongoose");

describe("Users", () => {
    let app;
    let db;

    beforeAll(async () => {
        // Setup env vars
        process.env.NODE_ENV = "TEST";
        process.env.JWT_SECRET = "TEST";
        process.env.MONGODB_URI = "mongodb://localhost/test";

        // TODO: Make this not awful
        app = require("../app.js");
        db = app.mongoose.connection.db;
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

    describe("Ping api v1 should return pong", () => {
        it("Should return pong v1", async () => {
            await request(app)
                .get("/api/v1/ping")
                .expect(200, "Pong v1!");
        });
    });
    describe("Ping api vX should return 404", () => {
        it("Should return 404", async () => {
            await request(app)
                .get("/api/vX/ping")
                .expect(404);
        });
    });
});
