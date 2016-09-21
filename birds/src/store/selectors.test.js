import { assert } from "chai";

import { createSelector } from "reselect";
import { getStore } from "./reducers/kvs.js";
import { createStore } from "redux";

import reducer from "./reducers";

import * as s from "./selectors";
import * as a from "./actions";

describe("Selecotor Tests", () => {
    const store = createStore(reducer);
    store.dispatch(a.setRegisterForm({ reducer: "registerForm" }));
    store.dispatch(a.setUser({ reducer: "user" }));
    store.dispatch(a.setLoginForm({ reducer: "loginForm" }));
    store.dispatch(a.setTeamForm({ reducer: "teamForm" }));
    const state = store.getState();
    it("should return the register form", () => {
        const data = s.getRegisterForm(state);
        assert.deepEqual(
            data,
            { reducer: "registerForm" }
        );
    });
    it("should return the auth", () => {
        const data = s.getUser(state);
        assert.deepEqual(
            data,
            { reducer: "user" }
        );
    });
    it("should return the login form", () => {
        const data = s.getLoginForm(state);
        assert.deepEqual(
            data,
            { reducer: "loginForm" }
        );
    });
    it("should return the team form", () => {
        const data = s.getTeamForm(state);
        assert.deepEqual(
            data,
            { reducer: "teamForm" }
        );
    });
});
