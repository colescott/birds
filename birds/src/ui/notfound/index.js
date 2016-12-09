import React from "react";
import { Link } from "react-router";

import { Card, CardText } from "material-ui/Card";

const NotFound = () => {
    return (
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
    );
};

export default NotFound;
