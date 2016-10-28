import React from "react";
import { connect } from "react-redux";

import { Card, CardText } from "material-ui/Card";

import * as s from "../../store/selectors.js";

const adminPage = (user) => {
    return (
        <div>
            <p> { `You are an admin of team ${user.teamnumber}`} </p>
            {
                user.teamPass
                ? <p> { `Your team's passsword is ${user.teamPass}`} </p>
                : null
            }
        </div>
    );
};

const memberPage = (user) => {
    return (
        <div>
            <p> {`You are a member of team ${user.teamnumber}`} </p>
        </div>
    );
};

const Home = (props) => {
    if (props.user.id) {
        return (
            <Card>
                <CardText>
                    <h1> {`Welcome, ${props.user.firstname}`} </h1>
                    <div>
                        {
                            props.user.isAdmin
                            ? adminPage(props.user)
                            : memberPage(props.user)
                        }
                    </div>
                    <br/>
                    <p>Welcome to birds!</p>
                    <p>The content is currently under construction. Check back in the next few days to get started!</p>
                    <p>I know, I can't wait either</p>
                </CardText>
            </Card>
        );
    } else {
        return (
            <Card>
                <CardText>
                <h1> Welcome to Birds </h1>
                <p>Birds is the Beginning and Intermediate Role Development System devloped by Team 4159 CardinalBotics.</p>
                <p>This website will guide you through learning about everything robotics. Register to start!</p>
                </CardText>
            </Card>
        );
    }
};

const mapStateToProps = (state) => ({
    user: s.getUser(state)
});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Home);
