import compose from "lodash/fp/compose";
import reduce from "lodash/fp/reduce";
import map from "lodash/fp/map";

export const checker = (extra, ...args) => (value) =>
    compose(
        reduce((a, b) => a || b)(null),
        map(f => f(value, extra))
    )(args);

export const isValidEmail = (text = "") =>
    text.includes("@")
    ? null
    : "Email must include @";
