import React from "react";
import { connect } from "react-redux";

import { Card, CardText } from "material-ui/Card";

import * as s from "../../store/selectors.js";

const Error404 = () => {
    <Card>
        <CardText>
            <h1> Uh oh... That's an error </h1>
            <br/>
            <p>Looks like you took a wrong turn somewhere.
            <br/>
            Would you like to go <Link to="/" key="Home/">home</Link>?
            </p>
        </CardText>
    </Card>
};

const mapStateToProps = (state) => ({
    user: s.getUser(state)
});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Error404);
