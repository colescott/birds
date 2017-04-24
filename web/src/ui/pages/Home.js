import React, { Component } from "react";
import { connect } from "react-redux";

import * as s from "../../store/selectors";

class Home extends Component {
    props: {
        name: string
    };
    render() {
        return <h1> Welcome {this.props.name} </h1>;
    }
}

const mapStateToProps = (state: State) => {
    const data = s.getAuthData(state);
    if (data.user) {
        return {
            name: `${data.user.firstname} ${data.user.lastname}`
        };
    } else {
        return {
            name: "Anonymous"
        };
    }
};

export default connect(mapStateToProps)(Home);
