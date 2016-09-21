import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

import { getUser } from "./auth.js";

function* teams() {
    for (;;) {
        try {
            // Wait for team related actions
            const action = yield take([
                c.TEAM_JOIN,
                c.TEAM_CREATE
            ]);

            // Read the token
            const { token, id: uid } = yield select(s.getUser);
            switch (action.type) {
                case c.TEAM_JOIN: {
                    // Get Needed Params
                    const { number, pass } = action.payload;

                    // Join the Team
                    yield call(joinTeam, number, pass, uid, token);

                    // Refetch and Update the User
                    const user = yield call(getUser, uid, token);
                    yield put(a.setUser(user));

                    // Redirect to the Home Page
                    yield put(push("/"));
                    break;
                }
                case c.TEAM_CREATE: {
                    // Get Needed Params
                    const { name, number } = action.payload;

                    // Create The Team
                    const team = yield call(createTeam, name, number, token);
                    yield put(a.setUser({ teamPass: team.password }));

                    // Refetch the User
                    const user = yield call(getUser, uid, token);
                    yield put(a.setUser(user));

                    // Redirect to the Home Page
                    yield put(push("/"));
                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            // If there is an error, dispatch an error action
            yield put(a.setUser(e));
        }
    }
}

export function* joinTeam(number, pass, uid, token) {
    const res = yield call(api.teams.join, number, pass, uid, token);
    if (res.error) throw res.error.message;
}

export function* createTeam(name, number, token) {
    const res = yield call(api.teams.create, name, number, token);
    if (res.error) throw new Error(res.error.message);
    const { data } = res;
    return data.team;
}

export default teams;
