import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";
import { Toolbar, ToolbarGroup, ToolbarTitle } from "material-ui/Toolbar";

import * as a from "../store/actions.js";

const NavBar = (props) => {
    return (
        <Toolbar
            style={{
                display: "flex",
                alignItems: "space-between"
            }}
        >
            <ToolbarGroup>
                <ToolbarTitle text="Birds" />
            </ToolbarGroup>
            <ToolbarGroup>
                <ToolbarTitle text={props.status} />
                {
                    props.links.map((v, i) =>
                        <Link to={v.to == "/logout" ? "#" : v.to} key={i}>
                            <ToolbarTitle text={v.text} onClick={v.to == "/logout" ? props.logout() : null}/>
                        </Link>
                    )
                }
                </ToolbarGroup>
        </Toolbar>
    );
};

const mapStateToProps = () => ({});

const mapDispatchToProps = (dispatch) => ({
    logout: () => () => dispatch(a.logoutAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(NavBar);

/*
<Link to={v.to} key={i} style={{ textDecoration: "none" }}>
    <MenuItem primaryText={v.name}/>
</Link>

<IconMenu
  iconButtonElement={
    <IconButton touch={true}>
      <NavigationExpandMoreIcon />
    </IconButton>
  }
>
  <MenuItem primaryText="Download" />
  <MenuItem primaryText="More Info" />
</IconMenu>
*/
