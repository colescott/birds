import React, { Component } from "react";
import { Link } from "react-router";

class Main extends Component {
    render() {
      return (
        <div>
          <h1> Birds </h1>
          <ul>
            <li><Link to="/"> Home </Link></li>
            <li><Link to="/register"> Register </Link></li>
          </ul>
          {
            this.props.children
          }
        </div>
      )
    }
}

export default Main;
