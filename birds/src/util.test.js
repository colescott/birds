import { assert } from "chai";

import { checker } from "./util";

const testLargeNumber = (n) => n > 5 ? null : "The number is not big enough";
const testEven = (n) => n % 2 === 0 ? null : "The number is not even";

describe("UTIL", () => {
    describe("checker", () => {
        it("should export a function", () => {
            assert.typeOf(checker, "Function");
        });
        it("shoud return the result of the first function which does not return null", () => {
            assert.deepEqual(
                checker(
                    null,
                    testLargeNumber,
                    testEven
                )(9),
                "The number is not even"
            );
        });
        it("should return null if all functions pass", () => {
            assert.equal(
                checker(
                    null,
                    testLargeNumber,
                    testEven
                )(8),
                null
            );
        });
    });
});
