import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";

import { Card, CardActions, CardHeader } from "material-ui/Card";
import FlatButton from "material-ui/FlatButton";

const Logout = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            <Card>
                <CardHeader
                    title="Logout"
                />
                <CardActions
                    style={{
                        display: "flux",
                        alignItems: "space-around"
                    }}
                >
                     <FlatButton label="Logout" onClick={props.logout()}/>
                </CardActions>
            </Card>
        </div>
    );
};

const mapStateToProps = () => ({});

const mapDispatchToProps = (dispatch) => ({
    logout: () => () => dispatch(a.logoutAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Logout);
