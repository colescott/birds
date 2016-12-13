import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    flex: {
        flex: 1,
    }
};

const listLessonsInBranch = (props, branch) => {
    return branch.map(function(lesson, i) {
        return <div key={i}>
            <Link to={`/lesson/${lesson.id}`}>
                <CardText>
                    {lesson.title}
                </CardText>
            </Link>
        </div>;
    });
};

const displayBranch = (props, branch) => {
    return (<div style={styles.div}>
        <Card style={styles.flex} >
            <CardHeader
                title={branch.name}
                subtitle={branch.length}
                actAsExpander={true}
                showExpandableButton={true}
            />
            <CardText
            expandable={true} >
            {listLessonsInBranch(props, branch)}
            </CardText>
        </Card>
    </div>);
};

const LessonList = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            {displayBranch(props, props.designBranch)}
            {displayBranch(props, props.manufacturingBranch)}
            {displayBranch(props, props.programmingBranch)}
            {displayBranch(props, props.socialBranch)}
        </div>
    );
};

const mapStateToProps = (state) => ({
    get list() {
        var lessonList = s.getLessonList(state);
        return Object.keys(lessonList).map((key) => {
            return lessonList[ key ];
        });
    },
    get designBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "design");
        branch.name = "Design";
        return branch.sort();
    },
    get manufacturingBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "manufacturing");
        branch.name = "Manufacturing";
        return branch.sort();
    },
    get programmingBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "programming");
        branch.name = "Programming";
        return branch.sort();
    },
    get socialBranch() {
        var branch = this.list.filter((lesson) => lesson.branch == "social");
        branch.name = "Social";
        return branch.sort();
    },
});

const mapDispatchToProps = (dispatch) => ({
  loadLessons: () => () => dispatch(a.getLessonList()),
});

export default connect(mapStateToProps, mapDispatchToProps)(LessonList);
