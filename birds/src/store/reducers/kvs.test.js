import { createStore } from "redux";
import { assert } from "chai";

import kvs, { getStore } from "./kvs";
const reducer = kvs("KVS");

describe("KVS REDUCER", () => {
    it("should export a function", () => {
        assert.typeOf(kvs, "Function");
    });
    it("should take one paramater", () => {
        assert.lengthOf(kvs, 1);
    });
    it("should export the getStore selector", () => {
        assert.typeOf(getStore, "Function");
    });
    it("should default to an empty object", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "" });
        const state = store.getState();
        assert.deepEqual(state, {});
    });
    it("should allow update on dispatching an object", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "KVS_SET", payload: { test: 1 } });
        const state = store.getState();
        assert.deepEqual(state, { test: 1 });
    });
    it("should select the store with getStore", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "KVS_SET", payload: { a: 1 } });
        const state = store.getState();
        assert.deepEqual({ a: 1 }, getStore(state));
    });
    it("should allow setting multiple properties", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "KVS_SET", payload: { a: 1, b: 2 } });
        const state = store.getState();
        assert.deepEqual(getStore(state), { a: 1, b: 2 });
    });
    it("should ignore dispathes to another prefix", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "OTHER_SET", payload: { a: 1 } });
        const state = store.getState();
        assert.deepEqual(getStore(state), {});
    });
    it("should allow resets", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "KVS_SET", payload: { a: 1 } });
        store.dispatch({ type: "KVS_SET", payload: { b: 2 } });
        store.dispatch({ type: "KVS_RESET" });
        const state = store.getState();
        assert.deepEqual(getStore(state), {});
    });
    it("should ignore actions with errors", () => {
        const store = createStore(reducer);
        store.dispatch({ type: "KVS_SET", payload: { a: 1 } });
        store.dispatch({ type: "KVS_SET", payload: { b: 2 }, error: true });
        const state = store.getState();
        assert.deepEqual(getStore(state), { a: 1 });
    });
});
