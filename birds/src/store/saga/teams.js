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
                c.JOIN_TEAM,
                c.CREATE_TEAM
            ]);

            // Read the token
            const { token } = yield select(s.getAuth);
            switch (action.type) {
                case c.JOIN_TEAM: {
                    // Get Needed Params
                    const { number, pass } = action.payload;
                    const { id: uid } = yield select(s.getAuth);

                    // Join the Team
                    yield joinTeam(number, pass, uid, token);

                    // Refetch and Update the User
                    const user = yield call(getUser, uid, token);
                    yield put(a.setAuth(user));

                    // Redirect to the Home Page
                    yield put(push("/"));
                    break;
                }
                case c.CREATE_TEAM: {
                    // Get Needed Params
                    const { name, number } = action.payload;
                    const { id: uid } = yield select(s.getAuth);

                    // Create The Team
                    const team = yield call(createTeam, name, number, token);
                    yield put(a.setAuth({ teamPass: team.password }));

                    // Refetch the User
                    const user = yield call(getUser, uid, token);
                    yield put(a.setAuth(user));

                    // Redirect to the Home Page
                    yield put(push("/"));
                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            // Log errors
            console.error(e);
        }
    }
}

export function* joinTeam(number, pass, uid, token) {
    yield call(api.teams.join, number, pass, uid, token);
}

export function* createTeam(name, number, token) {
    const res = yield call(api.teams.create, name, number, token);
    if (res.error) throw new Error(res.error.message);
    const { data } = res;
    return data.team;
}

export default teams;
