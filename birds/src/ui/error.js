import React from "react";
import { connect } from "react-redux";

import * as s from "../store/selectors";

const Error = ({ error }) => (
    <div>
        <p
            style={{
                color: "red"
            }}
        >
            { error }
        </p>
    </div>
);

const mapStateToProps = (state) => ({
    error: s.getAuthStatus(state).error
});

export default connect(mapStateToProps)(Error);
