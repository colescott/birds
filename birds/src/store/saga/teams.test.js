import { assert } from "chai";

import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

import teams, { joinTeam, createTeam } from "./teams.js";
import { getUser } from "./auth.js";

describe("TEAM SAGA", () => {
    const number = 1337;
    const pass = "pass";
    const token = "mayo";
    const id = 101;
    const name = "James Bond";
    const user = { id, token, pass, number };
    it("should watch for relevent actions", () => {
        const saga = teams();
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            take([
                c.TEAM_JOIN,
                c.TEAM_CREATE
            ])
        );
    });
    it("should select auth", () => {
        const saga = teams();
        saga.next();
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            select(s.getUser)
        );
    });
    it("should throw on error", () => {
        const saga = teams();
        saga.next();
        try {
            saga.next(new Error("error"));
        } catch (e) {
            assert.deepEqual(
                e,
                new Error("error")
            );
        }
    });
    describe("JOIN TEAM", () => {
        it("should call join team", () => {
            const saga = teams();
            saga.next();
            saga.next(a.joinTeam(number, pass));
            const { value: out } = saga.next({ token, id });
            assert.deepEqual(
                out,
                call(joinTeam, number, pass, id, token)
            );
        });
        it("should get the user", () => {
            const saga = teams();
            saga.next();
            saga.next(a.joinTeam(number, pass));
            saga.next({ token, id });
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                call(getUser, id, token)
            );
        });
        it("shoud update the user", () => {
            const saga = teams();
            saga.next();
            saga.next(a.joinTeam(number, pass));
            saga.next({ token, id });
            saga.next();
            const { value: out } = saga.next(user);
            assert.deepEqual(
                out,
                put(a.setUser(user))
            );
        });
        it("should redirect back to the home page", () => {
            const saga = teams();
            saga.next();
            saga.next(a.joinTeam(number, pass));
            saga.next({ token, id });
            saga.next();
            saga.next(user);
            const { value: out } = saga.next(user);
            assert.deepEqual(
                out,
                put(push("/"))
            );
        });
    });
    describe("CREATE TEAM", () => {
        it("should call api create team", () => {
            const saga = teams();
            saga.next();
            saga.next(a.createTeam(name, number));
            const { value: out } = saga.next({ token, id });
            assert.deepEqual(
                out,
                call(createTeam, name, number, token)
            );
        });
        it("should update the team password", () => {
            const saga = teams();
            saga.next();
            saga.next(a.createTeam(name, number));
            saga.next({ token, id });
            const { value: out } = saga.next({ password: pass });
            assert.deepEqual(
                out,
                put(a.setUser({ teamPass: pass }))
            );
        });
        it("should refetch the user", () => {
            const saga = teams();
            saga.next();
            saga.next(a.createTeam(name, number));
            saga.next({ token, id });
            saga.next({ password: pass });
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                call(getUser, id, token)
            );
        });
        it("should update the user data", () => {
            const saga = teams();
            saga.next();
            saga.next(a.createTeam(name, number));
            saga.next({ token, id });
            saga.next({ password: pass });
            saga.next();
            const { value: out } = saga.next(user);
            assert.deepEqual(
                out,
                put(a.setUser(user))
            );
        });
        it("should redirect back to the home page", () => {
            const saga = teams();
            saga.next();
            saga.next(a.createTeam(name, number));
            saga.next({ token, id });
            saga.next({ password: pass });
            saga.next();
            saga.next();
            const { value: out } = saga.next();
            assert.deepEqual(
                out,
                put(push("/"))
            );
        });
    });
});

describe("JOIN TEAM SAGA", () => {
    const number = 1337;
    const pass = "pass";
    const uid = 101;
    const token = "mayo";
    it("it should call api join team", () => {
        const saga = joinTeam(number, pass, uid, token);
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            call(api.teams.join, number, pass, uid, token)
        );
    });
    it("should throw on an error", () => {
        const saga = joinTeam(number, pass, uid, token);
        try {
            saga.next(new Error("error"));
        } catch (e) {
            assert.deepEqual(
                e,
                new Error("errror")
            );
        }
    });
});

describe("CREATE TEAM SAGA", () => {
    const name = "James Bond";
    const number = 1337;
    const token = "mayo";
    it("should call api teams create", () => {
        const saga = createTeam(name, number, token);
        const { value: out } = saga.next();
        assert.deepEqual(
            out,
            call(api.teams.create, name, number, token)
        );
    });
    it("should return the created team", () => {
        const saga = createTeam(name, number, token);
        saga.next();
        const { value: out } = saga.next({
            data: {
                team: { name, number, token }
            }
        });
        assert.deepEqual(
            out,
            { name, number, token }
        );
    });
    it("should throw on an error", () => {
        const saga = createTeam(name, number, token);
        saga.next();
        try {
            saga.next(new Error("error"));
        } catch (e) {
            assert.deepEqual(
                e,
                new Error("error")
            );
        }
    });
});
