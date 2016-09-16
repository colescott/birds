import { take } from "redux-saga/effects";

function* error() {
    while (true) {
        const action = yield take("*");
        if (action.error) {
            console.error(action.payload);
        }
    }
}

export default error;
